require_relative 'lib.rb'

$WORD = /[A-Za-z]\w*/
$ABSTRACT = /_+\d*/
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
$COMMENT = /;.*(?:\n|$)/
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
    $COMMENT            => :comment,
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
$DATA_SIGNIFIER = $DATA + [
    :bracket_close,
    :paren_close,
    :func_end
]
$SEPARATOR = $DATA_SIGNIFIER
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

def flush(out, stack, fin=[])
    out.push stack.pop until stack.empty? || fin.include?(stack.last.type)
end

def parse(code)
    # group expression
    stack = []
    out = []
    arities = []
    last_token = Token.new nil, nil, nil
    tokenize(code).each { |ent|
        raw, type, start = ent
        
        next if type == :comment
        
        if $DATA.include? type
            if $DATA.include? last_token.type
                # flush
                flush(out, stack, [:func_start])
            end
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
            if last_token.nil? || !$DATA_SIGNIFIER.include?(last_token.type)
                ent.type = :unary_operator
            else
                cur_prec, cur_assoc = $PRECEDENCE[raw]
                # NOTE: precedence determining
                loop {
                    break if stack.empty?
                    top_raw, top_type = stack.last
                    break if top_type != :operator && top_type != :unary_operator
                    top_prec, top_assoc = $PRECEDENCE[top_raw]
                    # fix unary operator precedence, temporary
                    # todo: unary operator precedence
                    if top_type == :unary_operator
                        top_prec = Infinity
                    end
                    
                    break if top_assoc == :right ? top_prec <= cur_prec : top_prec < cur_prec
                    out.push stack.pop
                }
            end
            stack.push ent
            
        elsif type == :bracket_open
            # determine if a function call
            # p last_token
            # p last_token.type
            unless $SEPARATOR.include? last_token.type
                # the "V" function creates an array
                # p "making array"
                out.push Token.new "V", :word, nil
            end
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
    
    flush out, stack
    
    offender = out.find { |raw, type| type == :bracket_open }
    if offender
        STDERR.puts "Syntax Error: unmatched \"[\" (at token #{offender})"
        nil
    else
        out
    end
end

def get_abstract_number(abstract, default=0)
    /\d+/ === abstract
    $& ? $&.to_i - 1 : default
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
        @tokens = inner_ast
    end
    
    def [](inst, *args)
        inst.local_descend
        res = @tokens.map { |token|
            inst.evaluate_node(token, args)
        }.last
        inst.local_ascend
        res
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

def display(entity)
    case entity
        when Matrix
            puts matrix.readable
        when Array
            begin
                mat = Matrix[*entity]
                puts mat.readable
            rescue
                p entity
            end
        else
            p entity
    end
end

class AtState
    def AtState.truthy?(ent)
        ent && ent != 0 && (ent.size != 0 rescue true)
    end
    
    def AtState.falsey?(ent)
        !AtState.truthy? ent
    end
    
    def AtState.func_like?(ent)
        AtLambda === ent || Proc === ent || Train === ent
    end
    
    def AtState.execute(*args)
        AtState.new(*args).run
    end
    
    def initialize(program, input=STDIN, output=STDOUT)
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
        @locals = [{}]
        @saved = []
        @in = input
        @out = output
    end
    
    attr_reader :stack
    attr_accessor :saved, :in, :out
    
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
        
        elsif @locals.last.has_key? raw
            @locals.last[raw]
        
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
    
    def clear(name)
        @variables.delete(name)
    end
    
    def define_local(name, value)
        @locals.last[name] = value
    end
    
    def clear_local(name)
        @locals.last.delete(name)
    end
    
    def local_descend
        @locals.push deep_copy @locals.last
    end
    
    def local_ascend
        @locals.pop
    end
    
    def get_blank(blank, blank_args)
        type = blank.match(/_+/)[0].size
        n = get_abstract_number(blank)
        # p "abstract type #{type}"
        case type
            when 1
                n < blank_args.size ? blank_args[n] : @saved[n]
            when 2
                blank_args[n..-1]
            else
                STDERR.puts "Blank too long: #{type} of #{blank}"
        end
    end
    
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
    
    # functions which can receive key things
    @@configurable = ["Print"]
    # functions whose arguments are not evaluated at once
    # (true = not evaluated, false = evaluated (normal))
    @@held_arguments = {
        "->" => [true, false],
        "If" => [false, true, true],
        "While" => [true, true],
        "DoWhile" => [true, true],
    }
    
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
        "AllInput" => lambda { |inst|
            inst.in.read
        },
        "Arg" => lambda { |inst, n=0|
            ARGV[n + 1]
        },
        "Clear" => lambda { |inst, *args|
            inst.clear *args
        },
        "ClearLocal" => lambda { |inst, *args|
            inst.clear_local *args
        },
        "Define" => lambda { |inst, *args|
            inst.define *args
        },
        "Display" => lambda { |inst, ent|
            display ent
        },
        "Local" => lambda { |inst, *args|
            inst.define_local *args
        },
        "Print" => lambda { |inst, *args, **opts|
            inst.out.print args.map(&:to_s).join(" ")
            inst.out.print opts[:after] || "\n"
            args
        },
        "ReadLine" => lambda { |inst|
            inst.in.gets
        },
        "ReadChar" => lambda { |inst|
            inst.in.getc
        },
        "ReadInt" => lambda { |inst|
            inst.in.gets.chomp.to_i
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
            @@functions["Sum"][inst, args]
        },
        "Bin" => lambda { |inst, n|
            @@functions["ToBase"][inst, n, 2]
        },
        "Ceiling" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.ceil
            else
                n.ceil(r)
            end
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
        "Divide" => lambda { |inst, *args|
            args.inject(1.0, :/)
        },
        "Even" => vectorize_monad { |inst, n|
            n.even?
        },
        "Fibonacci" => lambda { |inst, n|
            nth_fibonacci(n)
        },
        "Floor" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.floor
            else
                n.floor(r)
            end
        },
        "FromBase" => vectorize_dyad(RIGHT) { |inst, num, base|
            from_base num, base
        },
        "GCD" => lambda { |inst, *args|
            gcd args.flatten
        },
        "Hex" => lambda { |inst, n|
            @@functions["ToBase"][inst, n, 16]
        },
        "Halve" => vectorize_monad { |inst, n|
            @@operators["/"][inst, n, 2]
        },
        "LCM" => lambda { |inst, *args|
            lcm args.flatten
        },
        "Multiply" => lambda { |inst, *args|
            @@functions["Prod"][inst, args]
        },
        "N" => lambda { |inst, n|
            force_number n
        },
        "Oct" => lambda { |inst, n|
            @@functions["ToBase"][inst, n, 8]
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
        "Round" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.round
            else
                n.round(r)
            end
        },
        "Sign" => vectorize_monad { |inst, n|
            sign n
        },
        "Sqrt" => vectorize_monad { |inst, n|
            Math.sqrt n
        },
        "Square" => vectorize_monad { |inst, n|
            @@operators["*"][inst, n, n]
        },
        "Subtract" => lambda { |inst, *args|
            args.inject(0, :-)
        },
        "ToBase" => vectorize_dyad { |inst, num, base|
            to_base num, base
        },
        "Triangular" => lambda { |inst, n|
            gonal n, 3
        },
        "UnBin" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 2]
        },
        "UnHex" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 16]
        },
        "UnOct" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 8]
        },
        
        ##-------------------------##
        ## Trigonometric Functions ##
        ##-------------------------##
        "ArcCos" => vectorize_monad { |inst, n|
            Math::acos n
        },
        "ArcSin" => vectorize_monad { |inst, n|
            Math::asin n
        },
        "ArcTan" => vectorize_monad { |inst, n|
            Math::atan n
        },
        "ArcTan2" => vectorize_dyad { |inst, n, m|
            Math::atan2 n, m
        },
        "Cos" => vectorize_monad { |inst, n|
            Math::cos n
        },
        "Sin" => vectorize_monad { |inst, n|
            Math::sin n
        },
        "Tan" => vectorize_monad { |inst, n|
            Math::tan n
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
        "Mask" => lambda { |inst, mask, res|
            res.select.with_index { |e, i|
                AtState.truthy? mask[i]
            }
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
            list.prefixes.map { |e| e.sum }
        },
        "Average" => lambda { |inst, list|
            list.average
        },
        "Concat" => lambda { |inst, *args|
            args.flatten(1)
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
        "Indices" => vectorize_dyad(RIGHT) { |inst, list, ind|
            list.indices ind
        },
        "Index" => vectorize_dyad(RIGHT) { |inst, list, ind|
            list.index ind
        },
        "Iota" => lambda { |inst, min|
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
            force_list(list).prefixes
        },
        "Positions" => lambda { |inst, arr, els=arr|
            positions(arr, els)
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
        "Slices" => lambda { |inst, list, skew=(1..list.size).to_a|
            if skew.is_a? Array
                skew.flat_map { |e|
                    slices list, e
                }
            else
                slices list, skew
            end
        },
        "Smaller" => vectorize_dyad { |inst, *args|
            args.min
        },
        "Sort" => lambda { |inst, list, func=nil|
            if func.nil?
                list.sort
            else
                list.sort { |x, y|
                    func[inst, x, y]
                }
            end
        },
        "SortBy" => lambda { |inst, list, func|
            list.sort_by { |e| func[inst, e] }
        },
        "StdDev" => lambda { |inst, list|
            list.stddev
        },
        "Subsets" => lambda { |inst, list, n=list.size, exclude=[]|
            # p list, n, exclude
            if n < 0
                n = (list.size + n) % list.size
            end
            res = [[]]
            exclude = [*exclude]
            res.concat @@functions["Slices"][inst, list, (1..n).to_a]
            res.delete_if { |e| exclude.include? e.size }
        },
        "Sum" => lambda { |inst, list|
            list.inject(0) { |a, e|
                @@operators["+"][inst, a, e]
            }
        },
        "Unique" => lambda { |inst, a, arg=nil|
            if arg.nil?
                a.uniq
            else
                a.uniq { |e| arg[inst, e] }
            end
        },
        "Variance" => lambda { |inst, list|
            list.variance
        },
        
        ##------------------##
        ## Matrix Functions ##
        ##------------------##
        "Diagonal" => vectorize_dyad(RIGHT) { |inst, mat, diag=0|
            diagonal mat, diag
        },
        "LowerTriangle" => lambda { |inst, mat, strict=false|
            upperTriangle mat, AtState.truthy?(strict)
        },
        "MatrixRotate" => vectorize_dyad(RIGHT) { |inst, mat, n=1|
            n.times {
                mat = mat.transpose.map(&:reverse)
            }
            mat
        },
        "Transpose" => lambda { |inst, list|
            list.transpose
        },
        "UpperTriangle" => lambda { |inst, mat, strict=false|
            upperTriangle mat, AtState.truthy?(strict)
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
            else
                list.combination(count).to_a
            end
        },
        "Permutations" => vectorize_dyad(RIGHT) { |inst, list, count=list.size|
            if list.is_a? String
                force_list(list).permutation(count).map(&:join).to_a
            else
                list.permutation(count).to_a
            end
        },
        "Zip" => lambda { |inst, a, *b|
            a.zip(*b)
        },
        "ZipWith" => lambda { |inst, fn, a=nil, b=nil|
            l = lambda { |inst, a, b|
                zipwith(a, b) { |x, y| fn[inst, x, y] }
            }
            if a.nil?
                l
            else
                l[inst, a, b]
            end
        },
        
        ##---------------------------##
        ## List Functional Functions ##
        ##---------------------------##
        "Fold" => lambda { |inst, f, list=nil, start=nil|
            if list.nil?
                lambda { |inst, list, start=nil|
                    @@functions["Fold"][inst, f, list, start]
                }
            elsif AtState.func_like? list
                g = list
                lambda { |inst, list, start=nil|
                    @@functions["Fold"][inst, f, g[inst, list], start]
                }
            else
                if start.nil?
                    start = list[0]
                end
                list.inject { |a, c| f[inst, a, c] }
            end
        },
        "Map" => lambda { |inst, f, list|
            if AtState.func_like? list
                g = list
                lambda { |inst, list|
                    g[inst, list].map { |e|
                        f[inst, e]
                    }
                }
            else
                list.map { |e| f[inst, e] }
            end
        },
        "MapArgs" => lambda { |inst, f, list, *args|
            if AtState.func_like? list
                g = list
                n = args[0] || 1
                lambda { |inst, list, *args|
                    g[inst, list].map { |e|
                        f[inst, e, *resize(args + [list, e], n)]
                    }
                }
            else
                list.map { |e| f[inst, e, *args] }
            end
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
        "Repr" => lambda { |inst, ent|
            ent.inspect
        },
        "Rot" => lambda { |inst, str, amount=13|
            rotN(str, amount)
        },
        "String" => lambda { |inst, ent|
            #todo:standardize
            ent.to_s
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
        ":" => vectorize_dyad { |inst, x, y| (x..y).to_a },
        ".." => vectorize_dyad { |inst, x, y| (x..y).to_a },
        "..." => vectorize_dyad { |inst, x, y| (x...y).to_a },
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
            if AtState.func_like? a
                lambda { |inst, *args|
                    a[inst, *args, b]
                }
            elsif AtState.func_like? b
                lambda { |inst, *args|
                    b[inst, a, *args]
                }
            else
                resize([*b], a)
            end
        },
        "&:" => lambda { |inst, a, b|
            if AtState.func_like? a
                lambda { |inst, *args| a[inst, b, *args] }
            elsif AtState.func_like? b
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
            ConfigureValue.new key.raw, value
        },
    }
    
    @@unary_operators = {
        "-" => vectorize_monad { |inst, n| -n },
        "#" => lambda { |inst, n|
            if n.is_a? Train
                n.freeze
            else
                n.size
            end
        },
        "/" => lambda { |inst, r|
            if r.is_a? String
                make_regex r
            elsif AtState.func_like? r
                lambda { |inst, *args, last|
                    r[inst, last]
                }
            else
                raise "unimplemented"
            end
        },
        "\\" => lambda { |inst, f|
            if AtState.func_like? f
                lambda { |inst, first, *args|
                    f[inst, last]
                }
            else
                raise "unimplemented"
            end
        },
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
end