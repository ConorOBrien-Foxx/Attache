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
    "->"    => [1, :left],
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
    :make_lambda
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
                fn[inst, x]
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

class Node
    DISP_WIDTH = 4
    NODE_PREFIX = "\\- "
    LEAF_PREFIX = "-- "
    def initialize(head, children=[])
        @head = head
        @children = children
    end
    
    attr_reader :head, :children
    
    def add_child(children)
        @children.concat children
    end
    
    def to_ary
        [@head.clone, @children.clone]
    end
    
    def to_s(depth = 0)
        res = ""
        res += " " * DISP_WIDTH * depth
        res += NODE_PREFIX + @head.inspect
        res += "\n"
        depth += 1
        @children.each { |child|
            if child.is_a? Node
                res += child.to_s(depth)
            else
                res += " " * DISP_WIDTH * depth + LEAF_PREFIX + child.inspect
            end
            res += "\n"
        }
        depth -= 1
        depth == 0 ? res.chomp : res
    end
end

#idk
class AtLambda
    def initialize(inner_ast)
        @tokens = inner_ast[0]
    end
    
    def [](inst, *args)
        inst.evaluate_node(@tokens, args)
    end
end

class ConfigureValue
    def initialize(key, value)
        @key = key.to_sym
        @value = value
    end
    
    def to_a
        [@key, @value]
    end
    alias_method :to_ary, :to_a
    
    attr_accessor :key, :value
end

def ast(program)
    shunted = parse program rescue program
    roots = []
    stack = []
    build = nil
    shunted.each { |ent|
        raw, type = ent
        if type == :call_func
            args = stack.pop(raw)
            func = stack.pop
            cur = Node.new func, args
            stack.push cur
        elsif type == :operator
            args = stack.pop(2)
            cur = Node.new ent, args
            stack.push cur
            
        elsif $DATA.include? type
            stack.push ent
        else
            p ent
            raise
        end
    }
    stack
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
        "Define" => lambda { |inst, *args, **opts|
            inst.define *args
        },
        "Print" => lambda { |inst, *args, **opts|
            print args.map(&:to_s).join(" ")
            print opts[:end] || "\n"
        },
        "Stdin" => lambda { |inst, **opts|
            STDIN.read
        },
        "Stdout" => lambda { |inst, *args, **opts|
            print args.flatten.join
        },
        "V" => lambda { |inst, *args, **opts|
            args
        },
        
        #############################
        #### UNIVERSAL FUNCTIONS ####
        #############################
        "Id" => lambda { |inst, a, **opts|
            a
        },
        "Palindromic" => lambda { |inst, ent, **opts|
            reverse(ent) == ent
        },
        "Reverse" => lambda { |inst, ent, **opts|
            reverse ent
        },
        
        ##########################
        #### HYBRID FUNCTIONS ####
        ##########################
        "Series" => lambda { |inst, f, max, start=0, **opts|
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
        "Abs" => vectorize_monad { |inst, n, **opts|
            n.abs
        },
        "Add" => lambda { |inst, *args, **opts|
            args.sum
        },
        "Collatz" => vectorize_monad { |inst, n, **opts|
            collatz n
        },
        "CollatzSize" => vectorize_monad { |inst, n, **opts|
            collatz(n).size - 1
        },
        "Double" => vectorize_monad { |inst, n, **opts|
            @@operators["*"][inst, n, 2]
        },
        "Even" => vectorize_monad { |inst, n, **opts|
            n.even?
        },
        "Fibonacci" => lambda { |inst, n, **opts|
            nth_fibonacci(n)
        },
        "GCD" => lambda { |inst, *args, **opts|
            gcd args.flatten
        },
        "Halve" => vectorize_monad { |inst, n, **opts|
            @@operators["/"][inst, n, 2]
        },
        "LCM" => lambda { |inst, *args, **opts|
            lcm args.flatten
        },
        "N" => lambda { |inst, n, **opts|
            force_number n
        },
        "Odd" => vectorize_monad { |inst, n, **opts|
            n.odd?
        },
        "Polygonal" => lambda { |inst, n, order=3, **opts|
            gonal n, order
        },
        "Pythagorean" => vectorize_monad { |inst, n, **opts|
            pythagorean n
        },
        "Sign" => vectorize_monad { |inst, n, **opts|
            sign n
        },
        "Sqrt" => vectorize_monad { |inst, n, **opts|
            Math.sqrt n
        },
        "Square" => vectorize_monad { |inst, n, **opts|
            n * n
        },
        "Triangular" => lambda { |inst, n, **opts|
            gonal n, 3
        },
        
        ##-----------------##
        ## Prime Functions ##
        ##-----------------##
        "IsPrime" => vectorize_monad { |inst, n, **opts|
            Prime.prime? n
        },
        "Prime" => vectorize_monad { |inst, n, **opts|
            nth_prime n
        },
        "PrimeDivision" => vectorize_monad { |inst, n, **opts|
            Prime.prime_division n
        },
        "PrimeFactors" => vectorize_monad { |inst, n, **opts|
            prime_factors n
        },
        "Primes" => vectorize_monad { |inst, n, **opts|
            Prime.first n
        },
        
        
        ##############################
        #### FUNCTIONAL FUNCTIONS ####
        ##############################
        "Agenda" => lambda { |inst, flist, cond, **opts|
            lambda { |inst, *args|
                ind = from_numlike cond[inst, *args]
                flist[ind][inst, *args]
            }
        },
        "Bond" => lambda { |inst, func, larg, **opts|
            lambda { |inst, *args|
                func[inst, larg, *args]
            }
        },
        "C" => lambda { |inst, arg, **opts|
            lambda { |inst, *discard|
                arg
            }
        },
        "Call" => lambda { |inst, f, *args, **opts|
            f[inst, *args, **opts]
        },
        "Fixpoint" => lambda { |inst, f, n, **opts|
            fixpoint f.bind(inst), n
        },
        "Fork" => lambda { |inst, f, g, h, **opts|
            lambda { |inst, *args|
                g[inst, f[inst, *args], h[inst, *args]]
            }
        },
        "Nest" => lambda { |inst, f, e, n, **opts|
            from_numlike(n).times {
                e = f[inst, e]
            }
            e
        },
        "PeriodicSteps" => lambda { |inst, f, **opts|
            lambda { |inst, x, **opts|
                periodicloop f.bind(inst), x
            }
        },
        "Periodic" => lambda { |inst, f, **opts|
            lambda { |inst, x, **opts|
                periodicloop(f.bind(inst), x).last
            }
        },
        "RBond" => lambda { |inst, func, rarg, **opts|
            lambda { |inst, *args|
                func[inst, *args, rarg]
            }
        },
        
        
        #########################
        #### LOGIC FUNCTIONS ####
        #########################
        "All" => lambda { |inst, f, list=nil, **opts|
            if list.nil?
                f.all? { |e| AtState.truthy? e }
            else
                list.all? { |e| AtState.truthy?(f[inst, e]) }
            end
        },
        "If" => lambda { |inst, cond, t, f, **opts|
            if AtState.truthy? cond
                t
            else
                f
            end
        },
        
        
        ########################
        #### LIST FUNCTIONS ####
        ########################
        "Accumulate" => lambda { |inst, list, **opts|
            prefixes(list)[1..-1].map { |e| sum e }
        },
        "Average" => lambda { |inst, list, **opts|
            list.average
        },
        "Count" => lambda { |inst, list, f, **opts|
            if f.is_a? Proc
                list.count { |e| f[inst, e] }
            else
                list.count f
            end
        },
        "Delta" => lambda { |inst, list, **opts|
            list.delta
        },
        "First" => lambda { |inst, list, **opts|
            list.first
        },
        "Flat" => lambda { |inst, list, n=nil, **opts|
            list.flatten(n)
        },
        "Get" => vectorize_dyad(RIGHT) { |inst, list, inds, **opts|
            list[inds]
        },
        "Index" => vectorize_dyad(RIGHT) { |inst, list, ind, **opts|
            list.index ind
        },
        "Iota" => vectorize_monad { |inst, min, **opts|
            ((0...min) rescue (0...min.size)).to_a
        },
        "Last" => lambda { |inst, list, **opts|
            list.last
        },
        "Max" => lambda { |inst, *args, **opts|
            args.flatten.max
        },
        "Median" => lambda { |inst, list, **opts|
            list.median
        },
        "Min" => lambda { |inst, *args, **opts|
            args.flatten.min
        },
        "Outers" => vectorize_dyad(RIGHT) { |inst, arr, n=1, **opts|
            arr[0...n] + arr[-n..-1]
        },
        "Prefixes" => lambda { |inst, list, **opts|
            prefixes list
        },
        "Powerset" => lambda { |inst, list, **opts|
            list.powerset
        },
        "Prod" => lambda { |inst, list, **opts|
            list.prod
        },
        "Range" => vectorize_dyad { |inst, min, max=nil, **opts|
            if max.nil?
                (0..min).to_a
            else
                (min..max).to_a
            end
        },
        "Resize" => lambda { |inst, list, size, **opts|
            resize [*list], size
        },
        "Same" => lambda { |inst, *args, **opts|
            list = args.flatten
            list.all? { |e| e == list[0] }
        },
        "Sample" => vectorize_dyad(RIGHT) { |inst, list, n=nil, **opts|
            sample list, n },
        "Size" => lambda { |inst, list, **opts|
            list.size
        },
        "Slices" => vectorize_dyad(RIGHT) { |inst, list, skew, **opts|
            slices list, skew
        },
        "Sort" => lambda { |inst, list, **opts|
            list.sort
        },
        "StdDev" => lambda { |inst, list, **opts|
            list.stddev
        },
        "Sum" => lambda { |inst, list, **opts|
            list.sum
        },
        "Unique" => lambda { |inst, a, **opts|
            a.uniq
        },
        "Variance" => lambda { |inst, list, **opts|
            list.variance
        },
        
        ##---------------------------##
        ## List Functional Functions ##
        ##---------------------------##
        "Map" => lambda { |inst, f, list|
            list.map { |e| f[inst, e] }
        },
        "MaxBy" => lambda { |inst, f, list, **opts|
            list.max { |e| f[inst, e] }
        },
        "MinBy" => lambda { |inst, f, list, **opts|
            list.min { |e| f[inst, e] }
        },
        "Outer" => lambda { |inst, f, a, b, **opts|
            a.product(b).map { |e| f[inst, *e] }
        },
        "Select" => lambda { |inst, f, list, **opts|
            # p [f,list]
            list.select { |e| f[inst, e] }
        },
        
        
        ##########################
        #### STRING FUNCTIONS ####
        ##########################
        "Chars" => vectorize_monad { |inst, n, **opts|
            n.chars
        },
        "Format" => lambda { |inst, str, *args, **opts|
            str % args
        },
        "Rot" => lambda { |inst, str, amount=13|
            rotN(str, amount)
        },
        "Join" => vectorize_dyad(RIGHT) { |inst, list, joiner="", **opts|
            list.join joiner
        },
        "Split" => vectorize_dyad { |inst, str, sep, **opts|
            str.split sep
        },
        "Replace" => lambda { |inst, str, search, replace, **opts|
            replace str, search, replace
        },
        "Upcase" => vectorize_monad { |inst, str, **opts|
            str.upcase
        },
        "Downcase" => vectorize_monad { |inst, str, **opts|
            str.downcase
        },
        "IsUpcase" => vectorize_monad { |inst, str, **opts|
            str.upcase == str
        },
        "IsDowncase" => vectorize_monad { |inst, str, **opts|
            str.downcase == str
        },
        
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
                lambda { |inst, *args, **opts|
                    a[inst, *args, b, **opts]
                }
            elsif b.is_a? Proc
                lambda { |inst, *args, **opts|
                    b[inst, a, *args, **opts]
                }
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
        "->" => lambda { |inst, key, value|
            ConfigureValue.new key[0], value
        }
    }
    
    @@unary_operators = {
        "-" => vectorize_monad { |inst, n| -n },
        "#" => lambda { |inst, n| n.size },
        "/" => lambda { |inst, r| make_regex r },
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
        @trees = ast(program)
        @variables = {
            "true" => true,
            "false" => false,
            "lf" => "\n",
            "cr" => "\r",
            "nul" => "\0",
            "es" => "",
        }
        @stack = []
        @i = 0
    end
    attr_reader :stack
    
    def get_value(obj)
        # p obj
        raw, type = obj
        if type == :reference
            raw[1..-1]
            
        # elsif type == :abstract
        
        elsif type == :string
            raw[1..-2].gsub(/""/, '"')
        
        elsif @variables.has_key? raw
            @variables[raw]
        
        elsif @@functions.has_key? raw
            @@functions[raw]
        
        elsif type == :operator
            @@operators[raw] || @@unary_operators[raw]
        
        elsif type == :op_quote
            ref = raw[1..-1]
            lambda { |inst, *args|
                source = args.size == 1 ? @@unary_operators : @@operators
                source[ref][inst, *args]
            }
        
        elsif type == :number
            eval raw
        
        elsif type == :make_lambda
            AtLambda.new(ast raw)
        
        else
            puts "Unidentified get_value thing #{type.inspect}"
            p obj
            raise
        end
    end
    
    def define(name, value)
        @variables[name] = value
    end
    
    def evaluate_node(node, blank_args = [])
        return nil unless node.is_a? Node
        head, children = node
        args = []
        # special cases
        if "->" == head[0]
            args << children.shift
        end
        children.map! { |child|
            raw, type = child
            if child.is_a? Node
                evaluate_node child, blank_args
            elsif type == :abstract
                n = get_abstract_number(raw)
                blank_args[n]
            else
                get_value child
            end
        }
        args.concat children

        # filter ConfigureValue
        split = args.group_by { |e| e.is_a? ConfigureValue }
        config = split[true].to_h
        args = split[false]
        
        
        func = get_value head
        # todo: fix
        if func.arity < 0
            func[self, *args, **config]
        else
            func[self, *args]
        end
    end
    
    def run
        @trees.each { |tree|
            evaluate_node tree
        }
    end
    
    # # modifies stack
    # def exec_op(tok)
        # raw, type = tok
        # # puts "tok = #{tok}"
        # @stack << case type
            # when :call_func
                # args = stack.pop(raw)
                # func = @stack.pop
                # func[self, *args]
                
            # when :make_lambda
                # lambda { |inst, *args|
                    # raw.each { |arg|
                        # arg_raw, arg_type = arg
                        # if arg_type == :abstract
                            # number = get_abstract_number(arg_raw)
                            # inst.exec_op [args[number], :normal]
                        # else
                            # # p inst, arg
                            # inst.exec_op arg
                        # end
                    # }
                    # inst.stack.pop
                # }
            # when :operator
                # ref = @@operators[raw]
                # if ref.nil?
                    # STDERR.puts "Invalid operator #{raw.inspect}"
                    # raise
                # end
                # args = @stack.pop(2)
                # ref[self, *args]
            # when :unary_operator
                # ref = @@unary_operators[raw]
                # arg = @stack.pop
                # if ref.nil?
                    # STDERR.puts "Invalid unary operator #{raw.inspect}"
                    # raise
                # end
                # ref[self, arg]
            # when *$DATA
                # # p @stack
                # get_value(tok)
            # when :normal
                # raw
            # else
                # STDERR.puts "Unknown type #{type.inspect}"
                # raise
        # end
    # end
    
    # def step
        # tok = @tokens[@i]
        # exec_op tok
        # @i += 1
    # end
    
    # def run
        # step while @i < @tokens.size
    # end
end