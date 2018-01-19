require_relative 'lib.rb'

$WORD = /[A-Za-z]\w*/
$ABSTRACT = /_\d*/
$NUMBER = /(?:[0-9]*\.[0-9]+)|(?:[0-9]+)/
$REFERENCE = /\$#$WORD/
$BRACKET_OPEN = /\[|do/
$BRACKET_CLOSE = /\]|end/
$PAREN_OPEN = /\(/
$PAREN_CLOSE = /\)/
$COMMA = /,/
$STRING = /"(?:[^"]|"")*"/
$FUNC_START = /\{/
$FUNC_END = /\}/
$WHITESPACE = /\s+/
$UNKNOWN = /./
$PRECEDENCE = {
    ":"     => [30, :left],
    
    "&"     => [26, :left],
    "&:"    => [26, :left],
    "~"     => [25, :left],
    "@"     => [24, :left],
    "@@"    => [24, :left],
    "=>"    => [20, :right],
    "\\"    => [20, :right],
    "#"     => [20, :left],
    
    "^"     => [15, :right],
    "!"     => [15, :left],
    "*"     => [13, :left],
    "/"     => [13, :left],
    "%"     => [13, :left],
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
    "not"   => [4, :left],
    "or"    => [3, :left],
    "->"    => [1, :left],
}
$operators = $PRECEDENCE.keys.sort { |x, y| y.size <=> x.size }
$OPERATOR = Regexp.new($operators.map { |e| Regexp.escape e }.join "|")
$OP_QUOTE = /`#$OPERATOR/
$TYPES = {
    $BRACKET_OPEN       => :bracket_open,
    $BRACKET_CLOSE      => :bracket_close,
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
        i = 0
        code.scan($TOKENIZER) { |part|
            $TYPES.each { |k, v|
                if /^#{k}$/ === part
                    enum.yield Token.new part, v, i
                    i += part.size
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
        raw, type, start = ent
        
        if $DATA.include? type
            out.push ent
        
        elsif type == :func_start
            stack.push ent
            out.push ent
        
        elsif type == :func_end
            while stack.last && [:operator, :unary_operator].include?(stack.last.type)
                out.push stack.pop
            end
            
            collect = []
            until out.empty? || out.last.type == :func_start
                collect.unshift out.pop
            end
            
            next_start = stack.pop.start
            out.pop
            
            out.push Token.new collect, :make_lambda, next_start
        
        elsif type == :operator
            if last_token.nil? || !($DATA + [:bracket_close, :paren_close, :func_end]).include?(last_token.type)
                ent.type = :unary_operator
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
            
        elsif type == :bracket_open
            stack.push ent
            arities.push 1
        elsif type == :comma
            arities[-1] += 1
            out.push stack.pop while stack.last && [:operator, :unary_operator].include?(stack.last.type)
        elsif type == :bracket_close
            if last_token.type == :bracket_open
                arities[-1] = 0
            end
            
            while stack.last.type != :bracket_open
                out.push stack.pop
            end
            
            out.push Token.new arities.pop, :call_func, nil
            
            stack.pop
        
        elsif type == :paren_open
            stack.push ent
        elsif type == :paren_close
            out.push stack.pop while stack.last.type != :paren_open
            stack.pop
            
        elsif type == :whitespace
            # do nothing
            
        else
            STDERR.puts "Unknown type #{type.inspect} (#{raw.inspect}) during shunting"
            raise
        end
        last_token = ent if type != :whitespace
    }
    out.push stack.pop until stack.empty?
    
    offender = out.find { |raw, type| type == :bracket_open }
    if offender
        STDERR.puts "Syntax Error: unmatched \"[\" (at token #{offender})"
        nil
    else
        out
    end
end

def get_abstract_number(abstract)
    /\d+/ === abstract
    $& ? $&.to_i - 1 : 0
end

def vectorize_monad(&fn)
    res = lambda { |inst, *args|
        x , * = args
        if x.is_a? Array
            x.map { |e| res[inst, e] }
        else
            fn[inst, *args]
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
        rest = args[2..-1]
        use_left = x.is_a?(Array) && left
        use_right = y.is_a?(Array) && right
        if args.size == 1
            if use_left
                x.map { |e| res[inst, e, *rest] }
            else
                fn[inst, x, *rest]
            end
        elsif use_left
            if use_right
                x.map.with_index { |e, i| res[inst, e, y[i], *rest] }
            else
                x.map { |e| res[inst, e, y, *rest] }
            end
        elsif use_right
            y.map { |e| res[inst, x, e, *rest] }
        else
            fn[inst, *args]
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

class Token
    def initialize(raw, type, start)
        @raw = raw
        @type = type
        @start = start
    end
    
    attr_accessor :raw, :type, :start
    @@words = %w(raw type start)
    
    def [](n)
        raise "Indexing is deprecated. `Use inst.#{@@words[n]}` instead."
    end
    
    def []=(n, v)
        raise "Indexing is deprecated. `Use inst.#{@@words[n]} = #{v.inspect}` instead."
    end
    
    def to_ary
        [@raw, @type, @start]
    end
    
    def to_s
        "#{@type} #{@raw.inspect} @ #{@start}"
    end
    
    def inspect
        "#\x1b[33mToken\x1b[0m<" + to_ary.map(&:inspect).join(", ") + ">"
    end
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
    
    # def raw;@head;end
    
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
        @children.each_with_index { |child, i|
            if child.is_a? Node
                res += child.to_s(depth)
            else
                res += " " * DISP_WIDTH * depth + LEAF_PREFIX + child.inspect
            end
            res += "\n"
        }
        depth -= 1
        res.chomp
    end
    
    def inspect
        "#Node<#{@head}>{#{@children.inspect}}"
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
    shunted = if program.is_a? Array
        program
    else
        parse program
    end
    
    roots = []
    stack = []
    build = nil
    shunted.each { |ent|
        raw, type, start = ent
        if type == :call_func
            args = stack.pop(raw)
            func = stack.pop
            cur = Node.new func, args
            stack.push cur
        
        elsif type == :operator
            args = stack.pop(2)
            cur = Node.new ent, args
            stack.push cur
        
        elsif type == :unary_operator
            arg = stack.pop
            cur = Node.new ent, [arg]
            stack.push cur
        
        elsif $DATA.include? type
            stack.push ent
        
        else
            STDERR.puts "Unhandled shunt type #{type}"
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
    
    def AtState.execute(code)
        AtState.new(code).run
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
        "Print" => lambda { |inst, *args, **opts|
            print args.map(&:to_s).join(" ")
            print opts[:after] || "\n"
            args
        },
        "ReadLine" => lambda { |inst|
            STDIN.gets
        },
        "ReadChar" => lambda { |inst|
            STDIN.getc
        },
        "ReadInt" => lambda { |inst|
            STDIN.gets.chomp.to_i
        },
        "Save" => lambda { |inst, *args|
            inst.saved = args
        },
        "Stdin" => lambda { |inst|
            STDIN.read
        },
        "Stdout" => lambda { |inst, *args|
            print args.flatten.join
        },
        "V" => lambda { |inst, *args|
            args
        },
        
        #############################
        #### UNIVERSAL FUNCTIONS ####
        #############################
        "Id" => lambda { |inst, a|
            a
        },
        "Palindromic" => lambda { |inst, ent|
            reverse(ent) == ent
        },
        "Reverse" => lambda { |inst, ent|
            reverse ent
        },
        
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
        "Abs" => vectorize_monad { |inst, n|
            n.abs
        },
        "Add" => lambda { |inst, *args|
            args.sum
        },
        "Char" => lambda { |inst, arg|
            if arg.is_a? Array
                arg.map(&:chr).join
            else
                arg.chr
            end
        },
        "Collatz" => vectorize_monad { |inst, n|
            collatz n
        },
        "CollatzSize" => vectorize_monad { |inst, n|
            collatz(n).size - 1
        },
        "Double" => vectorize_monad { |inst, n|
            @@operators["*"][inst, n, 2]
        },
        "Digits" => vectorize_monad { |list, n| n.digits.reverse },
        "Even" => vectorize_monad { |inst, n|
            n.even?
        },
        "Fibonacci" => lambda { |inst, n|
            nth_fibonacci(n)
        },
        "GCD" => lambda { |inst, *args|
            gcd args.flatten
        },
        "Halve" => vectorize_monad { |inst, n|
            @@operators["/"][inst, n, 2]
        },
        "LCM" => lambda { |inst, *args|
            lcm args.flatten
        },
        "N" => lambda { |inst, n|
            force_number n
        },
        "Odd" => vectorize_monad { |inst, n|
            n.odd?
        },
        "Polygonal" => lambda { |inst, n, order=3|
            gonal n, order
        },
        "Pythagorean" => vectorize_monad { |inst, n|
            pythagorean n
        },
        "Random" => vectorize_monad { |inst, n=nil, m=nil|
            random(n, m)
        },
        "Sign" => vectorize_monad { |inst, n|
            sign n
        },
        "Sqrt" => vectorize_monad { |inst, n|
            Math.sqrt n
        },
        "Square" => vectorize_monad { |inst, n|
            n * n
        },
        "Triangular" => lambda { |inst, n|
            gonal n, 3
        },
        
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
        "Apply" => lambda { |inst, func, arg_arr|
            func[inst, *arg_arr]
        },
        "Applier" => lambda { |inst, func|
            lambda { |inst, args|
                func[inst, *args]
            }
        },
        "Bond" => lambda { |inst, func, larg|
            lambda { |inst, *args|
                func[inst, larg, *args]
            }
        },
        "C" => lambda { |inst, arg|
            lambda { |inst, *discard|
                arg
            }
        },
        "Call" => lambda { |inst, f, *args|
            f[inst, *args]
        },
        "Fixpoint" => lambda { |inst, f, n|
            fixpoint f.bind(inst), n
        },
        "Fork" => lambda { |inst, f, g, h|
            lambda { |inst, *args|
                g[inst, f[inst, *args], h[inst, *args]]
            }
        },
        "Nest" => lambda { |inst, f, e, n|
            from_numlike(n).times {
                e = f[inst, e]
            }
            e
        },
        "PeriodicSteps" => lambda { |inst, f|
            lambda { |inst, x|
                periodicloop f.bind(inst), x
            }
        },
        "Periodic" => lambda { |inst, f|
            # p "PERIODOC #{f}"
            lambda { |inst, x|
                periodicloop(f.bind(inst), x).last
            }
        },
        "RBond" => lambda { |inst, func, rarg|
            lambda { |inst, *args|
                func[inst, *args, rarg]
            }
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
        "Falsey" => lambda { |inst, arg|
            AtState.falsey? arg
        },
        "If" => lambda { |inst, cond, t, f|
            res = if AtState.truthy? cond
                t
            else
                f
            end
            inst.evaluate_node res
        },
        "Truthy" => lambda { |inst, arg|
            AtState.truthy? arg
        },
        "While" => lambda { |inst, cond, body|
            res = nil
            loop {
                c = inst.evaluate_node cond
                unless AtState.truthy? c
                    break
                end
                res = inst.evaluate_node body
            }
            res
        },
        "DoWhile" => lambda { |inst, cond, body|
            res = nil
            loop {
                res = inst.evaluate_node body
                c = inst.evaluate_node cond
                unless AtState.truthy? c
                    break
                end
            }
            res
        },
        
        
        ########################
        #### LIST FUNCTIONS ####
        ########################
        "Accumulate" => lambda { |inst, list|
            list.prefixes[1..-1].map { |e| sum e }
        },
        "Average" => lambda { |inst, list|
            list.average
        },
        "Count" => lambda { |inst, list, f|
            if f.is_a?(Proc) || f.is_a?(AtLambda)
                list.count { |e| f[inst, e] }
            else
                list.count f
            end
        },
        "Delta" => lambda { |inst, list|
            list.delta
        },
        "First" => lambda { |inst, list|
            list[0]
        },
        "Flat" => lambda { |inst, list, n=nil|
            list.flatten(n)
        },
        "Get" => vectorize_dyad(RIGHT) { |inst, list, inds|
            list[inds]
        },
        "Index" => vectorize_dyad(RIGHT) { |inst, list, ind|
            list.index ind
        },
        "Iota" => vectorize_monad { |inst, min|
            ((0...min) rescue (0...min.size)).to_a
        },
        "Larger" => vectorize_dyad { |inst, *args|
            args.max
        },
        "Last" => lambda { |inst, list|
            list[-1]
        },
        "Max" => lambda { |inst, *args|
            args.flatten.max
        },
        "Median" => lambda { |inst, list|
            list.median
        },
        "Min" => lambda { |inst, *args|
            args.flatten.min
        },
        "Outers" => vectorize_dyad(RIGHT) { |inst, arr, n=1|
            arr[0...n] + arr[-n..-1]
        },
        "Prefixes" => lambda { |inst, list|
            # p list
            force_list(list).prefixes
        },
        "Powerset" => lambda { |inst, list|
            list.powerset
        },
        "Prod" => lambda { |inst, list|
            list.prod
        },
        "Range" => vectorize_dyad { |inst, min, max=nil|
            if max.nil?
                (0..min).to_a
            else
                (min..max).to_a
            end
        },
        "Resize" => lambda { |inst, list, size|
            resize [*list], size
        },
        "Same" => lambda { |inst, *args|
            list = args.flatten
            list.all? { |e| e == list[0] }
        },
        "Sample" => vectorize_dyad(RIGHT) { |inst, list, n=nil|
            sample list, n },
        "Size" => lambda { |inst, list|
            list.size
        },
        "Slices" => vectorize_dyad(RIGHT) { |inst, list, skew|
            slices list, skew
        },
        "Smaller" => vectorize_dyad { |inst, *args|
            args.min
        },
        "Sort" => lambda { |inst, list|
            list.sort
        },
        "StdDev" => lambda { |inst, list|
            list.stddev
        },
        "Sum" => lambda { |inst, list|
            list.sum
        },
        "Unique" => lambda { |inst, a|
            a.uniq
        },
        "Variance" => lambda { |inst, list|
            list.variance
        },
        
        ##------------------------##
        ## Combinatoric Functions ##
        ##------------------------##
        "Combinations" => lambda { |inst, list, count=nil|
            if count.nil?
                count = (0..list.size).to_a
            end
            if count.is_a? Array
                count.map { |e| list.combination(e).to_a }.flatten(1)
            end
        },
        "Permutations" => vectorize_dyad(RIGHT) { |inst, list, count=list.size|
            list.permutation(count).to_a
        },
        
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
        "Chars" => vectorize_monad { |inst, n|
            n.chars
        },
        "Format" => lambda { |inst, str, *args|
            str % args
        },
        "Join" => vectorize_dyad(RIGHT) { |inst, list, joiner=""|
            list.join joiner
        },
        "Ord" => vectorize_monad { |inst, ent|
            if ent.is_a? String
                ent.chars.map(&:ord)
            else
                ent.ord
            end
        },
        "Split" => vectorize_dyad { |inst, str, sep|
            str.split sep
        },
        "Replace" => lambda { |inst, str, search, replace|
            replace str, search, replace
        },
        "Rot" => lambda { |inst, str, amount=13|
            rotN(str, amount)
        },
        "Upcase" => vectorize_monad { |inst, str|
            str.upcase
        },
        "Downcase" => vectorize_monad { |inst, str|
            str.downcase
        },
        "IsUpcase" => vectorize_monad { |inst, str|
            str.upcase == str
        },
        "IsDowncase" => vectorize_monad { |inst, str|
            str.downcase == str
        },
        
        ########################
        #### DATE FUNCTIONS ####
        ########################
        "Date" => lambda { |inst, *args|
            Time.new *args
        },
        "DateFormat" => lambda { |inst, fmt="%B %-d, %Y", date=Time.now|
            date.strftime fmt
        },
        "DayOfYear" => vectorize_dyad { |inst, n, date=Time.now|
            if date.is_a? Numeric
                date = Time.new date
            end
            date.day_of_year n
        },
        "YearDays" => lambda { |inst, date=Time.now|
            res = []
            dates = date.is_a?(Array) ? date : [date]
            dates.each { |date|
                res.concat yearlike(date).year_days
            }
            res
        },
        "DayOfWeek" => vectorize_monad { |inst, date|
            date.week_day
        },
        "Weekday" => vectorize_monad { |inst, date|
            date.wday
        },
        "Day" => vectorize_monad { |inst, date=Time.now|
            date.day
        }
        
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
        "%" => vectorize_dyad { |inst, a, b| a % b },
        "|" => vectorize_dyad { |inst, a, b| b % a == 0 },
        "=" => vectorize_dyad { |inst, x, y| x == y },
        ">" => vectorize_dyad { |inst, x, y| x > y },
        "<" => vectorize_dyad { |inst, x, y| x < y },
        ">=" => vectorize_dyad { |inst, x, y| x >= y },
        "<=" => vectorize_dyad { |inst, x, y| x <= y },
        ":" => lambda { |inst, x, y| (x..y).to_a },
        ".." => lambda { |inst, x, y| (x..y).to_a },
        "..." => lambda { |inst, x, y| (x...y).to_a },
        "or" => lambda { |inst, a, b|
            AtState.truthy?(a) ? a : b
        },
        "and" => lambda { |inst, a, b|
            AtState.falsey?(b) ? b : a
        },
        "not" => lambda { |inst, a, b|
            # A && !B
            AtState.truthy?(b) ? b : a
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
                lambda { |inst, *args|
                    a[inst, *args, b]
                }
            elsif b.is_a? Proc
                lambda { |inst, *args|
                    b[inst, a, *args]
                }
            else
                resize([*b], a)
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
        "~" => @@functions["Count"],
        "->" => lambda { |inst, key, value|
            ConfigureValue.new key[0], value
        },
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
        "not" => lambda { |inst, arg| AtState.falsey? arg },
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
            "sp" => " ",
            "inf" => Infinity,
            # perhaps temporary
            "alpha" => $ALPHA_LOWER,
            "ALPHA" => $ALPHA_UPPER,
        }
        @saved = []
    end
    attr_reader :stack
    attr_accessor :saved
    
    def error(message)
        STDERR.puts message
        exit
    end
    
    def get_value(obj)
        return obj unless obj.is_a? Token
        
        raw, type = obj
        
        if type == :reference
            raw[1..-1]
        
        elsif type == :string
            raw[1..-2].gsub(/""/, '"')
        
        elsif @variables.has_key? raw
            @variables[raw]
        
        elsif @@functions.has_key? raw
            @@functions[raw]
        
        elsif type == :operator
            @@operators[raw]
        
        elsif type == :unary_operator
            @@unary_operators[raw]
        
        elsif type == :op_quote
            ref = raw[1..-1]
            lambda { |inst, *args|
                source = args.size == 1 ? @@unary_operators : @@operators
                source[ref][inst, *args]
            }
        
        elsif type == :number
            # todo: fix this hack
            eval raw.gsub(/^\./, "0.")
        
        elsif type == :make_lambda
            AtLambda.new(ast raw)
        
        elsif type == :word
            error "Reference Error: Undefined variable #{raw.inspect}"
        
        else
            puts "Unidentified get_value thing #{type.inspect}"
            p obj
            raise
        end
    end
    
    def define(name, value)
        @variables[name] = value
    end
    
    def get_blank(blank, blank_args)
        n = get_abstract_number(blank)
        n < blank_args.size ? blank_args[n] : @saved[n]
    end
    
    @@configurable = ["Print"]
    @@held_arguments = {
        "->" => [true, false],
        "If" => [false, true, true],
        "While" => [true, true],
        "DoWhile" => [true, true],
    }
    def evaluate_node(node, blank_args = [])
        unless node.is_a? Node
            raise "#{node.inspect} is not a token" unless node.is_a? Token
            
            res = if node.type == :abstract
                get_blank node.raw, blank_args
            else
                get_value node
            end
            return res
        end
        
        head, children = node
        
        # special cases
        args = []

        if head.is_a? Token
            held = @@held_arguments[head.raw] || []
        else
            held = []
        end
        
        children.map!.with_index { |child, i|
            raw, type = child
            
            if held[i]
                child
            else
                if child.is_a? Node
                    evaluate_node child, blank_args
                elsif type == :abstract
                    get_blank raw, blank_args
                else
                    get_value child
                end
            end
        }
        args.concat children

        # filter ConfigureValue
        split = args.group_by { |e| e.is_a? ConfigureValue }
        config = split[true].to_h
        args = split[false]
        
        func = get_value head

        if func.is_a? Node
            func = evaluate_node func, blank_args
        end
        if func.nil?
            STDERR.puts "Error in retrieving value for #{head.inspect}"
            exit -3
        end
        
        
        if head.is_a?(Token) && @@configurable.include?(head.raw)
            func[self, *args, **config]
        else
            func[self, *args]
        end
    end
    
    def run
        @trees.map { |tree|
            evaluate_node tree
        }
    end
end