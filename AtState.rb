require_relative 'lib.rb'

$WORD = /[A-Za-z]\w*/
$ABSTRACT = /_\d*/
$NUMBER = /[0-9]+/
$REFERENCE = /\$#$WORD/
$BRACKET_OPEN = /\[/
$BRACKET_CLOSE = /\]/
$PAREN_OPEN = /\(/
$PAREN_CLOSE = /\)/
$COMMA = /,/
$STRING = /"(?:[^"]|"")*"/
$FUNC_START = /\{/
$FUNC_END = /\}/
$WHITESPACE = /\s+/
$UNKNOWN = /./
$PRECEDENCE = {
    "&"     => [26, :left],
    "&:"    => [26, :left],
    "~"     => [25, :left],#temporary precedence
    "@"     => [24, :left],
    "@@"    => [24, :left],
    "=>"    => [20, :right],
    "\\"    => [20, :right],
    "#"     => [20, :left],
    
    "^"     => [15, :right],
    "!"     => [15, :left],
    "*"     => [13, :left],
    "/"     => [13, :left],
    "|"     => [12, :left],
    "+"     => [11, :left],
    "-"     => [11, :left],
    
    "="     => [7, :left],
    "<"     => [7, :left],
    ">"     => [7, :left],
    "<="    => [7, :left],
    ">="    => [7, :left],
    ".."    => [5, :left],
    "..."   => [5, :left],
    "and"   => [4, :left],
    "or"    => [3, :left],
}
$operators = $PRECEDENCE.keys.sort { |x, y| y.size <=> x.size }
$OPERATOR = Regexp.new($operators.map { |e| Regexp.escape e }.join "|")
$OP_QUOTE = /`#$OPERATOR/
$TYPES = {
    $OPERATOR           => :operator,
    $WORD               => :word,
    $COMMA              => :comma,
    $STRING             => :string,
    $NUMBER             => :number,
    $ABSTRACT           => :abstract,
    $OP_QUOTE           => :op_quote,
    $FUNC_END           => :func_end,
    $REFERENCE          => :reference,
    $FUNC_START         => :func_start,
    $WHITESPACE         => :whitespace,
    $PAREN_OPEN         => :paren_open,
    $PAREN_CLOSE        => :paren_close,
    $BRACKET_OPEN       => :bracket_open,
    $BRACKET_CLOSE      => :bracket_close,
    $UNKNOWN            => :unknown,
}
$DATA = [
    :word,
    :number,
    :reference,
    :string,
    :op_quote,
    :call_func,
    :function,
    :abstract,
]
$TOKENIZER = Regexp.new($TYPES.keys.join "|")

def tokenize(code)
    Enumerator.new { |enum|
        code.scan($TOKENIZER) { |part|
            $TYPES.each { |k, v|
                if /^#{k}$/ === part
                    enum.yield [part, v]
                    break
                end
            }
        }
    }
end

def parse(code)
    # group expression
    stack = []
    out = []
    arities = []
    last_token = nil
    tokenize(code).each { |ent|
        # puts "#{out.map{|e|e[0]}} | #{stack.map{|e|e[0]}}"
        raw, type = ent
        # puts "RAW = #{raw.inspect}; TYPE = #{type.}"
        if $DATA.include? type
            out.push ent
        
        elsif type == :func_start
            stack.push ent
            out.push ent
        
        elsif type == :func_end
            out.push stack.pop while stack.last && [:operator, :unary_operator].include?(stack.last[1])
            collect = []
            collect.unshift out.pop until out.empty? || out.last[1] == :func_start
            stack.pop
            out.pop
            out.push [collect, :make_lambda]
        
        elsif type == :operator
            if last_token.nil? || !($DATA + [:bracket_close, :paren_close, :func_end]).include?(last_token[1])
                ent[1] = :unary_operator
            else
                cur_prec, cur_assoc = $PRECEDENCE[raw]
                # NOTE: precedence determining
                loop {
                    break if stack.empty?
                    top_raw, top_type = stack.last
                    break if top_type != :operator && top_type != :unary_operator
                    top_prec, top_assoc = $PRECEDENCE[top_raw]
                    break if top_assoc == :right ? top_prec <= cur_prec : top_prec < cur_prec
                    out.push stack.pop
                }
            end
            stack.push ent
            
            # puts "  STACK: #{stack}\n  OUT: #{out}"
        elsif type == :bracket_open
            # top = out.pop
            # top[1] = :function
            stack.push ent
            # stack.push top
            arities.push 1
        elsif type == :comma
            arities[-1] += 1
            out.push stack.pop while stack.last && [:operator, :unary_operator].include?(stack.last[1])
        elsif type == :bracket_close
            arities[-1] = 0 if last_token[1] == :bracket_open
            out.push stack.pop while stack.last[1] != :bracket_open
            out.push [arities.pop, :call_func]
            stack.pop
        
        elsif type == :paren_open
            stack.push ent
        elsif type == :paren_close
            # puts type
            # puts "  STACK: #{stack}\n  OUT: #{out}"
            out.push stack.pop while stack.last[1] != :paren_open
            stack.pop
            # puts "  STACK: #{stack}\n  OUT: #{out}"
        elsif type == :whitespace
            # do nothing
        else
            STDERR.puts "Unknown type #{type.inspect} (#{raw.inspect}) during shunting"
            raise
        end
        last_token = ent if type != :whitespace
    }
    out.push stack.pop until stack.empty?
    out
end

$ALPHA_UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
$ALPHA_LOWER = "abcdefghijklmnopqrstuvwxyz"

def get_abstract_number(abstract)
    /\d+/ === abstract
    $& ? $&.to_i - 1 : 0
end

def vectorize_monad(&fn)
    res = lambda { |inst, x|
        if x.is_a? Array
            x.map { |e| res[inst, e] }
        else
            fn[inst, x]
        end
    }
end

LEFT  = 0b01
RIGHT = 0b10

def vectorize_dyad(lr = LEFT | RIGHT, &fn)
    left  = lr & LEFT  != 0
    right = lr & RIGHT != 0
    res = lambda { |inst, *args|
        x, y = args
        use_left = x.is_a?(Array) && left
        use_right = y.is_a?(Array) && right
        if args.size == 1
            if use_left
                x.map { |e| res[inst, e] }
            else
                res[inst, x]
            end
        elsif use_left
            if use_right
                x.map.with_index { |e, i| res[inst, e, y[i]] }
            else
                x.map { |e| res[inst, e, y] }
            end
        elsif use_right
            y.map { |e| res[inst, x, e] }
        else
            fn[inst, x, y]
        end
    }
end

def vectorize(&fn)
    monadic = vectorize_monad { |*a| fn[*a] }
    dyadic  = vectorize_dyad  { |*a| fn[*a] }
    lambda { |inst, *args|
        if args.size == 1
            monadic[inst, *args]
        else
            dyadic[inst, *args]
        end
    }
end

class AtState
    def AtState.truthy?(ent)
        ent && ent != 0 && (ent.size != 0 rescue true)
    end
    
    def AtState.falsey?(ent)
        !AtState.truthy? ent
    end
    
    # All builtins
    @@functions = {
        ###########################
        #### GENERIC FUNCTIONS ####
        ###########################
        ## Contains:
        ## - control flow
        ## - memory manipulation
        ## - i/o
        ## - functions that are necessary
        "Define" => lambda { |inst, *args|
            inst.define *args
        },
        "Print" => lambda { |inst, *args|
            puts args.map(&:to_s).join(" ")
        },
        "Stdin" => lambda { |inst| STDIN.read },
        "Stdout" => lambda { |inst, *args| print args.flatten.join },
        "V" => lambda { |inst, *args| args },
        
        #############################
        #### UNIVERSAL FUNCTIONS ####
        #############################
        "Id" => lambda { |inst, a| a },
        "Palindromic" => lambda { |inst, ent| reverse(ent) == ent },
        "Reverse" => lambda { |inst, ent| reverse ent },
        
        ##########################
        #### HYBRID FUNCTIONS ####
        ##########################
        "Series" => lambda { |inst, f, max, start=0|
            i = start
            collect = []
            loop {
                value = f[inst, i]
                unless value.nil?
                    break if value >= max
                    collect.push value
                end
                i += 1
            }
            collect
        },
        
        ###########################
        #### NUMERIC FUNCTIONS ####
        ###########################
        "Abs" => vectorize_monad { |inst, n| n.abs },
        "Add" => lambda { |inst, *args|
            args.sum
        },
        "Collatz" => vectorize_monad { |inst, n| collatz n },
        "CollatzSize" => vectorize_monad { |inst, n| collatz(n).size },
        "Double" => vectorize_monad { |inst, n| @@operators["*"][inst, n, 2] },
        "Even" => vectorize_monad { |inst, n| n.even? },
        "Fibonacci" => lambda { |inst, n| nth_fibonacci(n) },
        "GCD" => lambda { |inst, *args|
            gcd args.flatten
        },
        "Halve" => vectorize_monad { |inst, n| @@operators["/"][inst, n, 2] },
        "LCM" => lambda { |inst, *args|
            lcm args.flatten
        },
        "N" => lambda { |inst, n| force_number n },
        "Odd" => vectorize_monad { |inst, n| n.odd? },
        "Polygonal" => lambda { |inst, n, order=3|
            gonal n, order
        },
        "Pythagorean" => vectorize_monad { |inst, n|
            pythagorean n
        },
        "Sign" => vectorize_monad { |inst, n| sign n },
        "Sqrt" => vectorize_monad { |inst, n| Math.sqrt n },
        "Square" => vectorize_monad { |inst, n| n * n },
        "Triangular" => lambda { |inst, n| gonal n, 3 },
        
        ##-----------------##
        ## Prime Functions ##
        ##-----------------##
        "IsPrime" => vectorize_monad { |inst, n|
            Prime.prime? n
        },
        "Prime" => vectorize_monad { |inst, n|
            nth_prime n
        },
        "PrimeDivision" => vectorize_monad { |inst, n|
            Prime.prime_division n
        },
        "PrimeFactors" => vectorize_monad { |inst, n|
            prime_factors n
        },
        "Primes" => vectorize_monad { |inst, n|
            Prime.first n
        },
        
        
        ##############################
        #### FUNCTIONAL FUNCTIONS ####
        ##############################
        "Agenda" => lambda { |inst, flist, cond|
            lambda { |inst, *args|
                ind = from_numlike cond[inst, *args]
                flist[ind][inst, *args]
            }
        },
        "Bond" => lambda { |inst, func, larg|
            lambda { |inst, *args| func[inst, larg, *args] }
        },
        "C" => lambda { |inst, arg| lambda { |inst, *discard| arg } },
        "Call" => vectorize_dyad(LEFT) { |inst, f, *args|
            f[inst, *args]
        },
        "Fixpoint" => lambda { |inst, f, n|
            fixpoint f.bind(inst), n
        },
        "Fork" => lambda { |inst, f, g, h|
            lambda { |inst, *args| g[inst, f[inst, *args], h[inst, *args]] }
        },
        "Nest" => lambda { |inst, f, e, n|
            from_numlike(n).times {
                e = f[inst, e]
            }
            e
        },
        "PeriodicSteps" => lambda { |inst, f|
            lambda { |inst, x| periodicloop f.bind(inst), x }
        },
        "Periodic" => lambda { |inst, f|
            lambda { |inst, x| periodicloop(f.bind(inst), x).last }
        },
        "RBond" => lambda { |inst, func, rarg|
            lambda { |inst, *args| func[inst, *args, rarg] }
        },
        
        
        #########################
        #### LOGIC FUNCTIONS ####
        #########################
        "All" => lambda { |inst, f, list=nil|
            if list.nil?
                f.all? { |e| AtState.truthy? e }
            else
                list.all? { |e| AtState.truthy?(f[inst, e]) }
            end
        },
        "If" => lambda { |inst, cond, t, f|
            if AtState.truthy? cond
                t
            else
                f
            end
        },
        
        
        ########################
        #### LIST FUNCTIONS ####
        ########################
        "Accumulate" => lambda { |inst, list|
            prefixes(list)[1..-1].map { |e| sum e }
        },
        "Average" => lambda { |inst, list| list.average },
        "Count" => lambda { |inst, list, f|
            if f.is_a? Proc
                list.count { |e| f[inst, e] }
            else
                list.count f
            end
        },
        "Delta" => lambda { |inst, list| list.delta },
        "First" => lambda { |inst, list| list.first },
        "Flat" => lambda { |inst, list, n=nil| list.flatten(n) },
        "Get" => vectorize_dyad(RIGHT) { |inst, list, inds|
            list[inds]
        },
        "Index" => vectorize_dyad(RIGHT) { |inst, list, ind|
            list.index ind
        },
        "Last" => lambda { |inst, list| list.last },
        "Max" => lambda { |inst, *args| args.flatten.max },
        "Median" => lambda { |inst, list| list.median },
        "Min" => lambda { |inst, *args| args.flatten.min },
        "Outers" => vectorize_dyad(RIGHT) { |inst, arr, n=1|
            arr[0...n] + arr[-n..-1]
        },
        "Prefixes" => lambda { |inst, list|
            prefixes list
        },
        "Powerset" => lambda { |inst, list| list.powerset },
        "Prod" => lambda { |inst, list| list.prod },
        "Range" => lambda { |inst, min, max=nil|
            if max.nil?
                (0..min).to_a
            else
                (min..max).to_a
            end
        },
        "Resize" => lambda { |inst, list, size| resize [*list], size },
        "Same" => lambda { |inst, *args|
            list = args.flatten
            list.all? { |e| e == list[0] }
        },
        "Size" => lambda { |inst, list| list.size },
        "Slices" => vectorize_dyad(RIGHT) { |inst, list, skew|
            slices list, skew
        },
        "Sort" => lambda { |inst, list| list.sort },
        "StdDev" => lambda { |inst, list| list.stddev },
        "Sum" => lambda { |inst, list| list.sum },
        "Unique" => lambda { |inst, a| a.uniq },
        "Variance" => lambda { |inst, list| list.variance },
        
        ##---------------------------##
        ## List Functional Functions ##
        ##---------------------------##
        "Map" => lambda { |inst, f, list|
            list.map { |e| f[inst, e] }
        },
        "MaxBy" => lambda { |inst, f, list|
            list.max { |e| f[inst, e] }
        },
        "MinBy" => lambda { |inst, f, list|
            list.min { |e| f[inst, e] }
        },
        "Outer" => lambda { |inst, f, a, b|
            a.product(b).map { |e| f[inst, *e] }
        },
        "Select" => lambda { |inst, f, list|
            # p [f,list]
            list.select { |e| f[inst, e] }
        },
        
        
        ##########################
        #### STRING FUNCTIONS ####
        ##########################
        "Chars" => vectorize_monad { |inst, n| n.chars },
        "Format" => lambda { |inst, str, *args| str % args },
        "Rot" => lambda { |inst, str, amount=13|
            rotN(str, amount)
        },
        "Join" => vectorize_dyad(RIGHT) { |inst, list, joiner=""| list.join joiner },
        
        
        ##################
        #### UNSORTED ####
        ##################
        #* none *#
    }
    
    # operators with two arguments
    @@operators = {
        "*" => vectorize_dyad { |inst, a, b| a * b },
        "/" => vectorize_dyad { |inst, a, b| simplify_number a * 1.0 / b },
        "-" => vectorize_dyad { |inst, a, b| a - b },
        "+" => vectorize_dyad { |inst, a, b| a + b },
        "^" => vectorize_dyad { |inst, a, b| a ** b },
        "|" => vectorize_dyad { |inst, a, b| b % a == 0 },
        "=" => vectorize_dyad { |inst, x, y| x == y },
        ">" => vectorize_dyad { |inst, x, y| x > y },
        "<" => vectorize_dyad { |inst, x, y| x < y },
        ">=" => vectorize_dyad { |inst, x, y| x >= y },
        "<=" => vectorize_dyad { |inst, x, y| x <= y },
        ".." => lambda { |inst, x, y| (x..y).to_a },
        "..." => lambda { |inst, x, y| (x...y).to_a },
        "or" => lambda { |inst, a, b|
            AtState.truthy?(a) ? a : b
        },
        "and" => lambda { |inst, a, b|
            AtState.falsey?(b) ? b : a
        },
        
        ## -- functional -- #
        "@" => lambda { |inst, f, g|
            lambda { |inst, *args| f[inst, g[inst, *args]] }
        },
        "@@" => lambda { |inst, f, g|
            lambda { |inst, *args| f[inst, *g[inst, *args]] }
        },
        "#" => lambda { |inst, x, y|
            Train.new *x, *y
        },
        "&" => lambda { |inst, a, b|
            if a.is_a? Proc
                lambda { |inst, *args| a[inst, *args, b] }
            elsif b.is_a? Proc
                lambda { |inst, *args| b[inst, a, *args] }
            else
                STDERR.puts "idk"
                raise
            end
        },
        "&:" => lambda { |inst, a, b|
            if a.is_a? Proc
                lambda { |inst, *args| a[inst, b, *args] }
            elsif b.is_a? Proc
                lambda { |inst, *args| b[inst, *args, a] }
            else
                STDERR.puts "idk#2"
                raise
            end
        },
        "=>" => @@functions["Map"],
        "\\" => @@functions["Select"],
    }
    
    @@unary_operators = {
        "-" => vectorize_monad { |inst, n| -n },
        "#" => lambda { |inst, n| n.size },
        "@" => lambda { |inst, f|
            if f.is_a? Proc
                vectorize { |inst, *args|
                    f[inst, *args]
                }
            else
                inst.get_value f
            end
        },
        "&" => lambda { |inst, f|
            if f.is_a? Proc
                lambda { |inst, *args|
                    f[inst, *args.flatten]
                }
            else
                f.to_s
            end
        },
        "~" => lambda { |inst, f|
            if f.is_a? Proc
                lambda { |inst, *args|
                    f[inst, *args.reverse]
                }
            else
                ~f
            end
        },
        "!" => vectorize_monad { |inst, n| factorial n },
    }
    
    def initialize(program)
        @tokens = parse(program)
        @variables = {
            "true" => true,
            "false" => false,
        }
        @stack = []
        @last_values = []
        @i = 0
    end
    attr_reader :stack, :last_values
    
    def get_value(obj)
        # p obj
        raw, type = obj
        if type == :reference
            raw[1..-1]
        elsif type == :save_last_value
            @last_values.push @stack.pop
        elsif type == :abstract
            number = @last_values.size - get_abstract_number(raw) - 1
            if number < 0
                raise "no value to obtain"
            end
            p @last_values
            @last_values[number]
        elsif type == :string
            raw[1..-2].gsub(/""/, '"')
        elsif @variables.has_key? raw
            @variables[raw]
        elsif @@functions.has_key? raw
            @@functions[raw]
        elsif type == :op_quote
            ref = raw[1..-1]
            lambda { |inst, *args|
                source = args.size == 1 ? @@unary_operators : @@operators
                source[ref][inst, *args]
            }
        else
            eval(obj[0].to_s)
        end
    end
    
    def define(name, value)
        @variables[name] = value
    end
    
    # modifies stack
    def exec_op(tok)
        raw, type = tok
        # puts "tok = #{tok}"
        @stack << case type
            when :call_func
                args = stack.pop(raw)
                func = @stack.pop
                func[self, *args]
                
            when :make_lambda
                lambda { |inst, *args|
                    raw.each { |arg|
                        arg_raw, arg_type = arg
                        if arg_type == :abstract
                            number = get_abstract_number(arg_raw)
                            inst.exec_op [args[number], :normal]
                        else
                            # p inst, arg
                            inst.exec_op arg
                        end
                    }
                    inst.stack.pop
                }
            when :operator
                ref = @@operators[raw]
                if ref.nil?
                    STDERR.puts "Invalid operator #{raw.inspect}"
                    raise
                end
                args = @stack.pop(2)
                ref[self, *args]
            when :unary_operator
                ref = @@unary_operators[raw]
                arg = @stack.pop
                if ref.nil?
                    STDERR.puts "Invalid unary operator #{raw.inspect}"
                    raise
                end
                ref[self, arg]
            when *$DATA
                # p @stack
                get_value(tok)
            when :normal
                raw
            else
                STDERR.puts "Unknown type #{type.inspect}"
                raise
        end
    end
    
    def step
        tok = @tokens[@i]
        exec_op tok
        @i += 1
    end
    
    def run
        step while @i < @tokens.size
    end
end