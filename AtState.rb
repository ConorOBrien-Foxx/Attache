require_relative 'lib.rb'

FOLDER_LOCATION = File.dirname(__FILE__)

$WORD = /[A-Za-z]\w*/
$ABSTRACT = /_+\d*/
$NUMBER = /(?:[0-9]*\.[0-9]+)|(?:[0-9]+)/
$REFERENCE = /\$#$WORD/
$ABSTRACT_REFERENCE = /\$+/
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
$COMMENT = /\?\?.*(?:\n|$)/
$COMMENT_OPEN = /\(\*/
$COMMENT_CLOSE = /\*\)/
$CURRY_OPEN = /<~/
$CURRY_CLOSE = /~>/

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
    "''"    => [20, :left],
    "'"     => [20, :left],
    "##"    => [19, :left],
    "|>"    => [15, :left],
    "<|"    => [15, :right],
    
    
    "^"     => [15, :right],
    "!"     => [15, :right],
    "?"     => [15, :left],
    "*"     => [13, :left],
    "/"     => [13, :left],
    "%"     => [13, :left],
    "|"     => [12, :left],
    "+"     => [11, :left],
    "-"     => [11, :left],
    
    "="     => [7, :left],
    "=/="   => [7, :left],
    "/="    => [7, :left],
    "=="    => [7, :left],
    "<"     => [7, :left],
    ">"     => [7, :left],
    "<="    => [7, :left],
    ">="    => [7, :left],
    "in"    => [6, :left],
    ".."    => [5, :left],
    "..."   => [5, :left],
    "and"   => [4, :left],
    "nor"   => [4, :left],
    "not"   => [4, :left],
    "xor"   => [3, :left],
    "or"    => [3, :left],
    "nand"  => [3, :left],
    "->"    => [2, :left],
    ":="    => [1, :left],
    ".="    => [1, :left],
    ";"     => [0, :left],
}
$operators = $PRECEDENCE.keys.sort { |x, y| y.size <=> x.size }
$OPERATOR = Regexp.new($operators.map { |e| Regexp.escape e }.join "|")
$OP_QUOTE = /`#$OPERATOR/
$TYPES = {
    $COMMENT            => :comment,
    $COMMENT_OPEN       => :comment_open,
    $COMMENT_CLOSE      => :comment_close,
    $CURRY_OPEN         => :curry_open,
    $CURRY_CLOSE        => :curry_close,
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
    $ABSTRACT_REFERENCE => :abstract_reference,
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
    :abstract_reference,
    :string,
    :op_quote,
    :call_func,
    :curry_func,
    :function,
    :abstract,
    :make_lambda
]
$DATA_SIGNIFIER = $DATA + [
    :bracket_close,
    :curry_close,
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

def get_starting(sym)
    sym.to_s.sub("_close", "_open").to_sym
end

def parse(code)
    # group expression
    stack = []
    out = []
    arities = []
    # keep track of curries promoted to calls
    curry_mask = []
    last_token = Token.new nil, nil, nil
    depth = nil
    tokenize(code).each { |ent|
        raw, type, start = ent
        
        next if type == :comment
        
        unless depth.nil?
            
            depth += 1 if type == :comment_open
            depth -= 1 if type == :comment_close
            
            if depth.zero?
                depth = nil
            end
            
            next
        end
        
        if type == :comment_open
            depth = 1
            next
        end
        
        if $DATA.include? type
            # two adjacent datatypes mark a statement
            if $DATA_SIGNIFIER.include? last_token.type
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
            
            # p collect
            
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
            unless $SEPARATOR.include? last_token.type
                # the "V" function creates an array
                out.push Token.new "V", :word, nil
            end
            stack.push ent
            arities.push 1
            
        elsif type == :curry_open
            # determine if a curry call
            unless $SEPARATOR.include? last_token.type
            
                # the "Hash" function creates a hash
                out.push Token.new "Hash", :word, nil
                stack.push Token.new "[", :bracket_open, nil
                curry_mask << true
            else
                stack.push ent
                curry_mask << false
            end
            arities.push 1
        
        elsif type == :comma
            arities[-1] += 1
            out.push stack.pop while stack.last && [:operator, :unary_operator].include?(stack.last.type)
        
        elsif type == :bracket_close || (type == :curry_close && curry_mask.pop)
            if last_token.type == :bracket_open
                arities[-1] = 0
            end
            
            while stack.last.type != :bracket_open
                out.push stack.pop
            end
            
            out.push Token.new arities.pop, :call_func, nil
            
            stack.pop
        
        elsif type == :curry_close
            if last_token.type == :curry_open
                arities[-1] = 0
            end
            
            while stack.last.type != :curry_open
                out.push stack.pop
            end
            
            out.push Token.new arities.pop, :curry_func, nil
            
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
    ARG_CONST = "ARGUMENTS"
    def initialize(inner_ast, params=[])
        @tokens = [*inner_ast]
        @params = params
        @scope = {}
    end
    
    attr_accessor :params, :scope
    
    def [](inst, *args)
        inst.local_descend(@scope)
        # define locals
        inst.define_local ARG_CONST, args
        inst.abstract_references << self
        @params.each_with_index { |name, i|
            inst.define_local name, args[i]
        }
        res = @tokens.map { |token|
            inner = inst.evaluate_node(token, args)
            if inner.kind_of? AtLambda
                # p "inner=",inner
                inner.scope.merge! @scope
            end
            inner
        }.last
        inst.abstract_references.pop
        @scope = inst.local_ascend
        res
    end
end

class ConfigureValue
    def initialize(key, value)
        @key = key
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
        
        elsif type == :curry_func
            args = stack.pop(raw)
            func = stack.pop
            stack.push Token.new lambda { |inst, *others|
                inst.evaluate_node Node.new func, args + others
            }, :function, start
        
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
            if matrix_like? entity
                puts entity.readable
            else
                p entity
            end
        when Hash
            p entity
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
        AtLambda === ent || Proc === ent || Train === ent || Tie === ent
    end
    
    def AtState.execute(*args)
        AtState.new(*args).run
    end
    
    @@default_variables = {
        "true" => true,
        "false" => false,
        "nil" => nil,
        "lf" => "\n",
        "cr" => "\r",
        "nul" => "\0",
        "es" => "",
        "sp" => " ",
        "tab" => "\t",
        "inf" => Infinity,
        # perhaps temporary
        "alpha" => $ALPHA_LOWER,
        "ALPHA" => $ALPHA_UPPER,
    }
    @@extended_variables = {}
    def initialize(program, input=STDIN, output=STDOUT)
        @trees = ast(program)
        @variables = @@default_variables.dup
        @abstract_references = []
        @locals = [{}]
        @saved = []
        @in = input
        @out = output
    end
    
    def load_lib(name)
        loc = Dir[File.join(FOLDER_LOCATION, "libs", name + ".*")]
        loc = loc.any? ? loc.first : nil
        if loc
            ext = File.extname loc
            case ext
                when ".@"
                    ast(File.read(loc)).each { |tree|
                        evaluate_node tree
                    }
                when ".rb"
                    require loc
            end
        else
            STDERR.puts "No such library #{name}"
        end
    end
    
    attr_reader :stack
    attr_accessor :variables, :locals, :saved, :in, :out, :abstract_references
    
    def error(message)
        STDERR.puts message
        # exit
        raise
    end
    
    def get_variable(name)
        if @locals.last.has_key? name
            @locals.last[name]
        
        elsif @variables.has_key? name
            @variables[name]
        
        elsif @@functions.has_key? name
            @@functions[name]
        
        else
            raise "no such variable #{name}"
        
        end
    end
    
    def get_value(obj)
        return obj unless obj.is_a? Token
        
        raw, type = obj
        
        if type == :reference
            raw[1..-1]
            
        elsif type == :function
            raw
        
        elsif type == :string
            raw[1..-2].gsub(/""/, '"').gsub(/\\./) { |e| eval '"' + e + '"' }
        
        elsif @@extended_variables.has_key? raw
            @@extended_variables[raw]
        
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
        
        elsif type == :abstract_reference
            @abstract_references[-raw.size]
        
        elsif type == :abstract
            get_blank(raw)
        
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
    
    def local_descend(adopt)
        # p @locals
        res = deep_copy @locals.last
        adopt ||= {}
        adopt.merge! res
        @locals.push adopt
        adopt
    end
    
    def local_ascend
        @locals.pop
    end
    
    def get_blank(blank, blank_args = nil)
        if blank_args.nil?
            blank_args = @locals.last[AtLambda::ARG_CONST] || []
        end
        
        type = blank.match(/_+/)[0].size
        n = get_abstract_number(blank)
        
        # p "abstract type #{type}, #{blank}, #{blank_args}"
        case type
            when 1
                n < blank_args.size ? blank_args[n] : @saved[n]
            when 2
                blank_args[n..-1]
            else
                STDERR.puts "Blank too long: #{type} of #{blank}"
        end
    end
    
    def evaluate_node(node, blank_args = nil)
        unless node.is_a? Node
            raise "#{node.inspect} is not a token" unless node.is_a? Token
            
            res = nil
            if node.type == :abstract
                res = get_blank node.raw, blank_args
            else
                res = get_value node
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

        configurable = @@configurable.include?(head.raw) rescue false
        # filter ConfigureValue
        if configurable
            split = args.group_by { |e| e.is_a? ConfigureValue }
            split[true] ||= []
            config = split[true].map { |a, b| [a.to_sym, b] }.to_h
            args = split[false]
        end
        
        func = get_value head

        if func.is_a? Node
            func = evaluate_node func, blank_args
        end
        
        if func.nil?
            STDERR.puts "[in function execution] Error in retrieving value for #{head.inspect}"
            exit -3
        end
        
        res = if head.is_a?(Token) && configurable
            func[self, *args, **config]
        else
            # special call function overloading
            case func
                when Array, Hash, String
                    # func[*args]
                    @@functions["Get"][self, func, *args]
                else
                    begin
                        # p func
                        func[self, *args]
                    rescue ArgumentError => e
                        STDERR.puts "Argument error: #{head}"
                        raise e
                    end
            end
        end
        
        res
    end
    
    def run
        @trees.map { |tree|
            evaluate_node tree
        }
    end
    
    def AtState.function(name, aliases: [], configurable: false, hold: nil, &body)
        @@functions[name] = convert_to_lambda(&body)
        if configurable
            @@configurable << name
        end
        
        aliases.each { |ali|
            @@functions[ali] = ali
        }
        
        unless hold.nil?
            @@held_arguments[name] = hold
        end
    end
    
    def AtState.variable(name, value)
        @@extended_variables[name] = value
    end
    
    # functions which can receive key things
    @@configurable = ["Print", "Option"]#, "Hash"]
    # functions whose arguments are not evaluated at once
    # (true = not evaluated, false = evaluated (normal))
    @@held_arguments = {
        "->" => [true, false],
        "If" => [false, true, true],
        "While" => [true, true],
        "DoWhile" => [true, true],
        "ForEach" => [false, true],
        "Modify" => [false, true],
        ":=" => [true, true],
        ".=" => [true, true],
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
        "Eval" => lambda { |inst, str|
            AtState.new(str).run.last
        },
        "EvalHere" => lambda { |inst, str|
            ast(str).map { |tree|
                inst.evaluate_node tree
            }.last
        },
        "Exit" => lambda { |inst, code=0|
            exit(code)
        },
        "Hash" => lambda { |inst, *opts|
            res = {}
            # p opts
            opts.each { |k, v|
                res[k] = v
            }
            res
        },
        "Local" => lambda { |inst, *args|
            inst.define_local *args
        },
        "Modify" => lambda { |inst, head, body|
            init = inst.get_variable head
            result = inst.evaluate_node body, [init]
            inst.define head, result
        },
        "Needs" => lambda { |inst, *libs|
            libs.each { |lib|
                inst.load_lib lib
            }
        },
        "Option" => lambda { |inst, prompt, **opts|
            read_option prompt, opts
        },
        "Print" => lambda { |inst, *args, **opts|
            inst.out.print args.map(&:to_s).join(" ")
            inst.out.print opts[:after] || "\n"
            args
        },
        "Prompt" => lambda { |inst, prompt=nil|
            prompt_input prompt, inst.in
        },
        "ReadLine" => lambda { |inst|
            inst.in.gets
        },
        "ReadLineLoop" => lambda { |inst, *args, func|
            lines = []
            loop {
                line = @@functions["Prompt"][inst, *args]
                break if line.nil?
                lines << func[inst, line]
            }
            lines
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
        "SeriesIf" => lambda { |inst, f, cond, max, start=0|
            i = start
            collect = []
            loop {
                value = f[inst, i]
                unless value.nil?
                    break if value >= max
                    collect.push value if cond[inst, value]
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
        "Log" => lambda { |inst, n|
            Math::log10 n
        },
        "Ln" => lambda { |inst, n|
            Math::log n
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
        
        ##------------------------##
        ## Number Logic Functions ##
        ##------------------------##
        "Even" => vectorize_monad { |inst, n|
            n.even?
        },
        "Negative" => vectorize_monad { |inst, n|
            n.negative?
        },
        "Odd" => vectorize_monad { |inst, n|
            n.odd?
        },
        "Positive" => vectorize_monad { |inst, n|
            n.positive?
        },
        "Zero" => vectorize_monad { |inst, n|
            n.zero?
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
        # "Group" => lambda { |inst, arr|
            
        # },
        "Tie" => lambda { |inst, *funcs|
            if funcs.any? { |e| !AtState.func_like? e }
                funcs.inject([]) { |acc, e| [*acc, *e] }
            else
                Tie.new funcs
            end
        },
        "TieArray" => lambda { |inst, *funcs|
            Tie.new funcs, true
        },
        "Nest" => lambda { |inst, f, e, n|
            from_numlike(n).times {
                e = f[inst, e]
            }
            e
        },
        "NestWhile" => lambda { |inst, f, init, cond|
            iter = init
            while cond[inst, iter]
                iter = f[inst, iter]
            end
            iter
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
        "Any" => lambda { |inst, f, list=nil|
            if list.nil?
                f.any? { |e| AtState.truthy? e }
            else
                list.any? { |e| AtState.truthy?(f[inst, e]) }
            end
        },
        "Falsey" => lambda { |inst, arg|
            AtState.falsey? arg
        },
        "If" => lambda { |inst, cond, t, f=nil|
            res = if AtState.truthy? cond
                t
            else
                f
            end
            if res.is_a?(Token) || res.is_a?(Node)
                inst.evaluate_node res
            else
                res
            end
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
        "ForEach" => lambda { |inst, ent, body|
            arr = force_list(ent)
            
            arr.each { |x|
                inst.evaluate_node body, [x]
            }
            
            nil
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
        "Has" => lambda { |inst, list, member|
            list.include? member
        },
        "FlatGet" => lambda { |inst, list, inds|
            [*inds].each { |i|
                list = list[i]
            }
            list
        },
        "Indices" => vectorize_dyad(RIGHT) { |inst, list, ind|
            list.indices ind
        },
        "Index" => vectorize_dyad(RIGHT) { |inst, list, ind|
            list.index ind
        },
        "Intersection" => lambda { |inst, *lists|
            lists.inject(&:&)
        },
        "Intersperse" => lambda { |inst, lists, joiner|
            res = []
            lists.each_with_index { |e, i|
                res << e
                res << joiner if i != lists.size - 1
            }
            res
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
        "Overlap" => lambda { |inst, list, arr|
            overlap list, arr
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
        "Remove" => lambda { |inst, list, ent|
            list = list.clone
            list.delete ent
            list
        },
        "RemoveFirst" => lambda { |inst, list, ent|
            list = list.clone
            list.delete_at list.index ent
            list
        },
        "RemoveAll" => lambda { |inst, list, ents|
            ents = [*ents]
            list.reject { |e| ents.include? e }
        },
        "Rotate" => lambda { |inst, list, amount=1|
            if list.is_a? String
                @@functions["Rotate"][inst, force_list(list), amount].join
            else
                rotate list, amount
            end
        },
        "Same" => lambda { |inst, *args|
            list = args.flatten(1)
            list.all? { |e| e == list[0] }
        },
        "Sample" => vectorize_dyad(RIGHT) { |inst, list, n=nil|
            sample list, n
        },
        "Set" => lambda { |inst, ent, key, val|
            if String === ent
                scope = inst.locals.last
                scope = inst.variables unless scope.has_key? ent
                scope[ent][key] = val
            else
                ent[key] = val
            end
        },
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
        "SplitAt" => vectorize_dyad(RIGHT) { |inst, str, inds=[1]|
            split_at force_list(str), inds
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
        "Union" => lambda { |inst, *lists|
            lists.inject(&:|)
        },
        "Unique" => lambda { |inst, a, arg=nil|
            a = force_list a
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
        "MatrixIota" => lambda { |inst, mat|
            matrix_iota mat
        },
        "Tr" => lambda { |inst, list|
            list.transpose
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
            force_list(a).zip(*b.map { |e| force_list e })
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
                    start = list.shift
                end
                
                list.fold(inst, f, start)
            end
        },
        "Map" => vectorize_dyad(LEFT) { |inst, f, list=nil|
            if AtState.func_like? list
                g = list
                lambda { |inst, *args|
                    g[inst, *args].map { |e|
                        f[inst, e]
                    }
                }
            elsif list.nil?
                lambda { |inst, list|
                    list.map { |e| f[inst, e] }
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
        "Outer" => lambda { |inst, f, a, *bs|
            a.product(*bs).map { |e| f[inst, *e] }
        },
        "Select" => lambda { |inst, f, list|
            if AtState.func_like? list
                g = list
                lambda { |inst, list|
                    @@functions["Select"][inst, f, g[inst, list]]
                }
            else
                list.select { |e|
                    AtState.truthy? f[inst, e]
                }
            end
        },
        "Reject" => lambda { |inst, f, list|
            if AtState.func_like? list
                g = list
                lambda { |inst, list|
                    @@functions["Reject"][inst, f, g[inst, list]]
                }
            else
                list.reject { |e| AtState.truthy? f[inst, e] }
            end
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
        "Replace" => lambda { |inst, str, search, replace=""|
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
        ":=" => lambda { |inst, var, val|
            if Node === var
                # p var, val
                # todo: pattern matching
                args = var.children.map(&:raw)
                res = AtLambda.new [val], args
                inst.define var.head.raw, res
            else
                name = var.raw
                inst.define name, inst.evaluate_node(val)
            end
        },
        ".=" => lambda { |inst, var, val|
            #todo: expand like :=
            name = var.raw
            inst.define_local name, inst.evaluate_node(val)
        },
        "*" => vectorize_dyad { |inst, a, b| a * b },
        "/" => vectorize_dyad { |inst, a, b| simplify_number a * 1.0 / b },
        "-" => vectorize_dyad { |inst, a, b| a - b },
        "+" => vectorize_dyad { |inst, a, b| a + b },
        "^" => vectorize_dyad { |inst, a, b| a ** b },
        "%" => vectorize_dyad { |inst, a, b| a % b },
        "|" => vectorize_dyad { |inst, a, b|
            if AtState.func_like? b
                b[inst, a]
            else
                b % a == 0
            end
        },
        "|>" => lambda { |inst, x, y|
            y[inst, x]
        },
        "<|" => lambda { |inst, x, y|
            x[inst, y]
        },
        "=" => vectorize_dyad { |inst, x, y| x == y },
        "/=" => vectorize_dyad { |inst, x, y| x != y },
        "==" => lambda { |inst, x, y| x == y },
        "=/=" => lambda { |inst, x, y| x != y },
        ">" => vectorize_dyad { |inst, x, y| x > y },
        "<" => vectorize_dyad { |inst, x, y| x < y },
        ">=" => vectorize_dyad { |inst, x, y| x >= y },
        "<=" => vectorize_dyad { |inst, x, y| x <= y },
        ":" => vectorize_dyad { |inst, x, y| (x..y).to_a },
        ".." => vectorize_dyad { |inst, x, y| (x..y).to_a },
        "..." => vectorize_dyad { |inst, x, y| (x...y).to_a },
        "in" => lambda { |inst, x, y| @@functions["Has"][inst, y, x] },
        "or" => lambda { |inst, a, b|
            AtState.truthy?(a) ? a : b
        },
        "xor" => lambda { |inst, a, b|
            AtState.truthy?(a) ^ AtState.truthy?(b)
        },
        "nand" => lambda { |inst, a, b|
            AtState.falsey?(a) || AtState.falsey?(b) ? true : false
        },
        "and" => lambda { |inst, a, b|
            AtState.falsey?(b) ? b : a
        },
        "nor" => lambda { |inst, a, b|
            AtState.truthy?(a) || AtState.truthy?(b) ? false : true
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
            if AtState.func_like? g
                lambda { |inst, *args| f[inst, *g[inst, *args]] }
            else
                f[inst, *g]
            end
        },
        "##" => lambda { |inst, f, g|
            @@operators["@"][inst, f, g]
        },
        "#" => lambda { |inst, x, y|
            Train.new *x, *y
        },
        "'" => @@functions["Tie"],
        "''" => @@functions["TieArray"],
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
            if key.is_a?(Node) && key.head == "V"
                # p key
                params = key.children.map(&:raw)
                value.params = params
                value
            elsif key.is_a?(Token) && key.type == :word
                ConfigureValue.new key.raw, value
            elsif key.is_a?(Node)
                keyval = inst.evaluate_node key
                ConfigureValue.new keyval, value
            else
                keyval = inst.get_value key
                ConfigureValue.new keyval, value
            end
        },
        ";" => lambda { |inst, x, y| y },
        "!" => lambda { |inst, a, b|
            if AtState.func_like? a
                a[inst, b]
            else
                raise "Unimplemented: ncr/npr, idk"
            end
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
        # matrix size
        "##" => lambda { |inst, n|
            dim n
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
        # vectorize
        "@" => lambda { |inst, f|
            if AtState.func_like? f
                vectorize { |inst, *args|
                    f[inst, *args]
                }
            else
                inst.get_value f
            end
        },
        # equiv. f[*x]
        "&" => lambda { |inst, f|
            if AtState.func_like? f
                lambda { |inst, *args|
                    f[inst, *args.flatten]
                }
            else
                f.to_s
            end
        },
        # reverses arguments
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
        "?" => vectorize_monad { |inst, n| AtState.truthy? n },
        "not" => lambda { |inst, arg| AtState.falsey? arg },
    }
end