require_relative 'lib.rb'
require_relative 'AtClass.rb'

FOLDER_LOCATION = File.dirname(__FILE__)

$WORD = /[[:alpha:]][[[:alpha:]]\w]*/
$ABSTRACT = /_+\d*/
$NUMBER = /(?:(?:[0-9]*\.[0-9]+)|(?:[0-9]+))i?/
$REFERENCE = /\$#$WORD/
$ABSTRACT_REFERENCE = /\$+/
$BRACKET_OPEN = /\[|do\b/
$BRACKET_CLOSE = /\]|end\b/
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
$CURRY_OPEN = /<~|«/
$CURRY_CLOSE = /~>|»/
$STATEMENT_SEP = /;/

$PRECEDENCE = {
    "."       => [99, :left],

    ":"       => [30, :left],

    "&"       => [26, :left],
    "&:"      => [26, :left],
    "~"       => [25, :left],
    "@"       => [24, :left],
    "@@"      => [24, :left],
    "@%"      => [24, :left],
    "=>"      => [20, :right],
    "⇒"      => [20, :right], # => alias
    "\\"      => [20, :right],
    "#"       => [20, :left],
    "''"      => [20, :left],
    "'"       => [20, :left],
    "##"      => [19, :left],



    "∩"       => [18, :left], # Intersection alias
    "∪"       => [17, :left], # Union alias
    "∆"       => [17, :left], # symmetric difference
    "Ø"       => [17, :left], # setwise difference

    "^"       => [15, :right],
    "!"       => [15, :right],
    "?"       => [15, :left],
    "*"       => [13, :left],
    "/"       => [13, :left],
    "//"      => [13, :left],
    "⁄"       => [13, :left], # // alias (fraction slash)
    "%"       => [13, :left],
    "|"       => [12, :left],
    "+"       => [11, :left],
    "-"       => [11, :left],
    "±"       => [11, :left],

    "="       => [9, :left],
    "=/="     => [9, :left],
    "/="      => [9, :left],
    "≠"       => [9, :left], # /= alias
    "=="      => [9, :left],
    "<"       => [9, :left],
    ">"       => [9, :left],
    "<="      => [9, :left],
    "≤"       => [9, :left], # <= alias
    ">="      => [9, :left],
    "≥"       => [9, :left], # >= alias
    "in"      => [8, :left],
    ".."      => [7, :left],
    "‥"       => [7, :left], # .. alias
    "..."     => [7, :left],
    "…"       => [7, :left], # ... alias
    "|>"      => [6, :left],
    "▷"      => [6, :left], # |> alias
    "<|"      => [6, :right],
    "◁"      => [6, :left], # <| alias
    "and"     => [6, :left],
    "∧"       => [6, :left], # and alias
    "nor"     => [6, :left],
    "⊽"       => [6, :left], # nor alias
    "not"     => [6, :left],
    "xor"     => [5, :left],
    "⊻"       => [5, :left], # xor alias
    "or"      => [5, :left],
    "∨"       => [5, :left], # or alias
    "nand"    => [5, :left],
    "⊼"       => [5, :left], # nand alias
    "->"      => [4, :left],
    "→"       => [4, :left], # -> alias
    "else"    => [3, :left],
    ":>"      => [3, :left],
    ":="      => [2, :right],
    "≔"      => [2, :right],
    ".="      => [2, :right],
    ";;"      => [1, :left],
}
$PRECEDENCE_UNARY = Hash.new(Infinity)
$PRECEDENCE_UNARY["..."] = 0
$PRECEDENCE_UNARY["…"] = 0

$operators = $PRECEDENCE.keys.sort { |x, y| y.size <=> x.size }
$OPERATOR = Regexp.new($operators.map { |e|
    if /^\w+$/ === e
        "#{e}\\b"
    else
        Regexp.escape e
    end
}.join "|")
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
    $STATEMENT_SEP      => :statement_sep,
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
    :func_end,
]
$SEPARATOR = $DATA_SIGNIFIER
$TOKENIZER = Regexp.new($TYPES.keys.join("|"), "u")

def tokenize(code)
    Enumerator.new { |enum|
        i = 0
        depth = nil
        build = nil
        code = code.encode("UTF-8")
        code.scan($TOKENIZER) { |part|
            $TYPES.each { |reg, type|
                next unless /^#{reg}$/ === part

                if depth.nil?
                    if type == :comment_open
                        depth = 1
                        build = part
                    else
                        enum.yield Token.new part, type, i
                        i += part.size
                    end
                else
                    build += part
                    depth += 1 if type == :comment_open
                    depth -= 1 if type == :comment_close
                    if depth.zero?
                        depth = nil
                        enum.yield Token.new build, :comment, i
                    end
                    i += part.size
                end

                break
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
    # keep track of curries promoted to calls
    curry_mask = []
    last_token = Token.new nil, nil, nil
    parens = []

    tokenize(code).each { |ent|
        raw, type, start = ent

        next if type == :comment

        is_data = $DATA.include?(type) || type == :func_start# || type == :curry_open
        last_was_data = $DATA_SIGNIFIER.include? last_token.type

        # two adjacent datatypes mark a statement
        if is_data && last_was_data || type == :statement_sep
            flush(out, stack, [:func_start])
        end

        if type == :statement_sep
            last_token = ent
            next
        end

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

                    if top_type == :unary_operator
                        top_prec = $PRECEDENCE_UNARY[top_raw]
                    end

                    break if top_assoc == :right ? top_prec <= cur_prec : top_prec < cur_prec
                    out.push stack.pop
                }
            end
            stack.push ent

        elsif type == :bracket_open
            if !stack.empty? && stack.last.raw == "."
                out.push stack.pop
            end

            # determine if a function call
            unless $SEPARATOR.include? last_token.type
                # the "V" function creates an array
                out.push Token.new "V", :word, nil
            end
            stack.push ent
            arities.push 1

        elsif type == :curry_open
            if !stack.empty? && stack.last.raw == "."
                out.push stack.pop
            end

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
            unless arities.last.nil?
                arities[-1] += 1
            end

            out.push stack.pop while stack.last && [:operator, :unary_operator].include?(stack.last.type)

            if arities.last.nil?
                parens[-1] = true
                out.push Token.new "discard", :discard, nil
            end

        elsif type == :bracket_close || (type == :curry_close && curry_mask.pop)
            if last_token.type == :bracket_open || last_token.type == :curry_open
                arities[-1] = 0
            end

            loop {
                if stack.empty?
                    STDERR.puts "Syntax Error: unmatched closing brace: #{ent}"
                    return nil
                end
                break if stack.last.type == :bracket_open
                out.push stack.pop
            }

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
            # dh "stack", stack
            # dh "out", out
            arities.push nil
            parens.push nil

        elsif type == :paren_close
            arities.pop
            temp = []
            temp.push stack.pop while stack.last.type != :paren_open
            stack.pop

            # if parens.pop
                # # out.unshift Token.new "Last", :word, nil
                # # stack.unshift Token.new "[", :bracket_open, nil
                # # dh "stack", stack
                # # dh "out", out
            # else
                # # dh "stack", stack
                # # stack.pop
            # end


            # temp.unshift Token.new("Last",:word,nil) if parens.pop

            out.concat temp

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
            if args.size != fn.arity - 1 && !fn.arity.negative?
                raise ArgumentError.new
            else
                fn[inst, *args]
            end
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

class AtFunction
    def initialize(fn, held: held=[], config: config=false, arity: 0)
        @held = held
        @config = config
        @fn = fn
        # p "#{fn}"
        # p "arity: #{arity.inspect}"
        @arity = arity
    end

    def AtFunction.from(**opts, &fn)
        AtFunction.new(fn, **opts)
    end

    def size
        @arity
    end

    def [](inst, *args)
        @fn[inst, *args]
    end

    attr_accessor :held, :config, :fn, :arity
end

def held(*held, &fn)
    AtFunction.new(fn, held: held)
end
def configurable(&fn)
    AtFunction.new(fn, config: true)
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
        # if @head.type == :operator
            # @children.map(&:raw).join @head.raw
        # else
            # @head.raw + @children.inspect
        # end
    end
end

class AtLambda
    ARG_CONST = "ARGUMENTS"
    def initialize(inner_ast, params=[])
        @tokens = [*inner_ast]
        @params = params
        @scope = {}
        @ascend = true
        @descend = true
        @ignore_other = false
    end

    def bind(inst)
        lambda { |*args|
            self[inst, *args]
        }
    end

    def size
        @params.size
    end

    alias :arity :size

    attr_accessor :params, :scope, :ascend, :descend, :ignore_other

    def [](inst, *args)
        inst.local_descend(@scope) if @descend

        # define locals
        inst.define_local ARG_CONST, args
        inst.abstract_references << self
        @params.each_with_index { |name, i|
            inst.define_local name, args[i]
        }

        temp_scope = @scope.dup

        res = @tokens.map.with_index { |token, i|

            inner = inst.evaluate_node(token, args, @scope)

            if @ascend && @descend
                temp = inst.locals.last.dup
                @params.each { |param|
                    temp.delete param
                }
                temp_scope.merge! temp
            end

            AtState.traverse(inner) { |atom|
                if atom.kind_of? AtLambda
                    atom.scope.merge! temp_scope
                end
            }

            inner
        }.last

        inst.abstract_references.pop
        temp_scope = inst.local_ascend if @ascend

        @scope = temp_scope unless @ignore_other

        res
    end
end

class AtError
    def initialize(name, message, origin="(null)")
        @name = name
        @message = message
        @origin = origin
    end

    def to_s
        "#{@origin}: #{@name}: #{@message}"
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

class Applicator
    def initialize(value = [])
        @value = [*value]
    end

    attr_accessor :value
end

def make_curry(args, func)
    lambda { |inst, *others|
        abstracts = []
        to_remove = []

        args.each { |el|
            if Token === el && el.type == :abstract
                n = get_abstract_number(el.raw)
                abstracts[n] = others[n]
                to_remove.push n
            end
        }

        not_provided = to_remove - (0...others.size).to_a

        if not_provided.empty?
            others.reject!.with_index { |e, i| to_remove.include? i }

            inst.evaluate_node Node.new(func, args + others), abstracts
        else
            next_args = args.map { |el|
                if Token === el && el.type == :abstract
                    n = get_abstract_number(el.raw)
                    ind = not_provided.index n
                    if ind
                        dest = n - others.size + 1
                        Token.new "_#{dest}", :abstract, nil
                    else
                        abstracts[n]
                    end
                else
                    el
                end
            }

            make_curry(next_args, func)
        end
    }
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
    return nil if shunted.nil?
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
            stack.push Token.new make_curry(args, func), :function, start

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

        # elsif type == :discard
            # stack.push Node.new ent, [stack.pop]

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
    entity
end

# used for property overloading
def class_has?(klass, prop)
    return false unless AtClassInstance === klass
    klass.methods.has_key? "$#{prop}"
end

class AtState
    NOT_PROVIDED = :not_provided

    def AtState.truthy?(ent)
        res = ent && ent != 0 && (ent.size != 0 rescue true)
    end

    def set_op_quote(token, res)
        op = token.raw[1..-1]
        hash = {
            1 => @@unary_operators,
            2 => @@operators,
        }
        hash[res.arity][op] = res
    end

    def AtState.falsey?(ent)
        !AtState.truthy?(ent)
    end

    def AtState.func_like?(ent)
        AtLambda === ent || Proc === ent || Train === ent || Tie === ent
    end

    def AtState.execute(*args)
        AtState.new(*args).run
    end

    def AtState.traverse(container, &fn)
        rec = lambda { |arg|
            if Hash === arg
                arg.map { |k, v|
                   [k, rec[v]]
                }.to_h
            elsif Array === arg
                arg.map { |e| rec[e] }
            else
                fn[arg]
            end
        }
        rec[container]
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
        "∞" => Infinity,
        "E" => Math::E,
        "PI" => Math::PI,
        "TAU" => Math::PI * 2,
        "PAU" => Math::PI * 1.5,
        "argv" => ARGV[1..-1],
        "¼" => Rational(1, 4),
        "½" => Rational(1, 2),
        "¾" => Rational(3, 4),
        "⅓" => Rational(1, 3),
        "⅔" => Rational(2, 3),
        "⅕" => Rational(1, 5),
        "⅖" => Rational(2, 5),
        "⅗" => Rational(3, 5),
        "⅘" => Rational(4, 5),
        "⅙" => Rational(1, 6),
        "⅚" => Rational(5, 6),
        "⅛" => Rational(1, 8),
        "⅜" => Rational(3, 8),
        "⅝" => Rational(5, 8),
        "⅞" => Rational(7, 8),
        # perhaps temporary
        "alpha" => $ALPHA_LOWER,
        "ALPHA" => $ALPHA_UPPER,
        "ascii" => (32..126).map(&:chr).join,
    }
    @@extended_variables = {}

    def initialize(program, input=STDIN, output=STDOUT)
        @trees = ast(program)
        if @trees.nil?
            exit
        end
        @variables = @@default_variables.dup
        @abstract_references = []
        @locals = [{}]
        @saved = []
        @in = input
        @out = output
        load_lib "std"
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
        exit
        # raise
    end

    def get_variable(name)
        if @@extended_variables.has_key? name
            @@extended_variables[name]

        elsif @locals.last.has_key? name
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
            raw[1..-2].gsub(/""/, '"').gsub(/\\x.{2}|\\./) { |e|
                eval '"' + e + '"' rescue e
            }

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

    def local_descend(arg={})
        res = @locals.last.dup
        adopt = arg.dup
        adopt.merge! res.dup
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

    def evaluate_node(node, blank_args = nil, merge_with = nil, check_error: true)
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

        func = get_value head
        if AtFunction === func
            held = func.held
            configurable = func.config
            func = func.fn
        elsif head.is_a? Token
            held = @@held_arguments[head.raw] || []
            configurable = @@configurable.include?(head.raw) rescue false
        else
            held = []
            configurable = false
        end


        children.map!.with_index { |child, i|
            raw, type = child

            if held[i]
                child
            else
                if child.is_a? Node
                    evaluate_node child, blank_args, merge_with, check_error: check_error
                elsif type == :abstract
                    get_blank raw, blank_args
                else
                    get_value child
                end
            end
        }
        args.concat children

        # check if error occured
        if check_error
            args.each { |arg|
                if AtError === arg
                    return arg
                end
            }
        end

        # filter ConfigureValue
        if configurable
            split = args.group_by { |e| e.is_a? ConfigureValue }
            split[true] ||= []
            config = split[true].map { |a, b| [a.to_sym, b] }.to_h
            args = split[false]
        end

        args = args.flat_map { |e|
            Applicator === e ? e.value : [e]
        }

        if func.is_a? Node
            func = evaluate_node func, blank_args, merge_with, check_error: check_error
        end

        if func.nil?
            STDERR.puts "[in function execution] Error in retrieving value for #{head.inspect}"
            exit -3
        end


        if func.kind_of?(AtLambda) && !merge_with.nil?
            # di "func infusion"
            # dh "func", func.inspect
            # dhash "merge_with", merge_with
            func.scope.merge! merge_with
            # dd "func infused"
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

        if res.kind_of?(AtLambda) && !merge_with.nil?
            # di "res infusion"
            # dh "func", func.inspect
            # dhash "merge_with", merge_with
            res.scope.merge! merge_with
            # dd "res infused"
        end

        res
    end

    def evaluate_node_safe(node_maybe)
        if Node === node_maybe || Token === node_maybe
            evaluate_node node_maybe
        else
            node_maybe
        end
    end

    def run
        @trees.map { |tree|
            res = evaluate_node tree#, [], @locals.last.dup
            if AtError === res
                puts res.to_s
                exit -2
            end
            res
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

    def cast_string(value)
        if AtClassInstance === value && value.methods["$string"]
            value.methods["$string"][self]
        else
            value.to_s rescue "#{value}"
        end
    end

    # functions which can receive key things
    @@configurable = [
        "Bisect",
        "Chop",
        "Configure",
        "Print",
        "NestList",
        "Option",
        "Safely",
        "Series",
        "SeriesIf",
    ]
    # functions whose arguments are not evaluated at once
    # (true = not evaluated, false = evaluated (normal))
    HOLD_ALL = Hash.new(true)
    @@held_arguments = {
        "->" => [true, false],
        "→" => [true, false],
        "If" => [false, true, true],
        "While" => [true, true],
        "DoWhile" => [true, true],
        "ForEach" => [false, true],
        "Modify" => [false, true],
        ":=" => [true, true],
        ".=" => [true, true],
        "." => [false, true],
        "DoSafe" => [true],
        "TryCatch" => [true, true],

        "and" => [true, true],
        "nand" => [true, true],
        "else" => [true, true],
        "or" => [true, true],
        "nor" => [true, true],
        "not" => [true, true],
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

        #<<
        # Reads all input from the instance's source input.
        # @return string
        # @genre IO
        #>>
        "AllInput" => lambda { |inst|
            inst.in.read
        },

        #<<
        # Returns the <code>n</code>th argument given to the program.
        # @return string
        # @genre IO
        # @type n number
        #>>
        "Arg" => lambda { |inst, n=0|
            ARGV[n + 1]
        },
        #<<
        # Undefines <code>name</code> from the global scope. Returns that variable's value, or <code>nil</code> if the variable was undefined.
        # @return (*)
        # @genre scope
        # @type name string
        #>>
        "Clear" => lambda { |inst, name|
            inst.clear name
        },
        #<<
        # Undefines <code>name</code> from the local scope. Returns that variable's value, or <code>nil</code> if the variable was undefined.
        # @return (*)
        # @genre scope
        # @type name string
        #>>
        "ClearLocal" => lambda { |inst, name|
            inst.clear_local name
        },
        #<<
        # Returns a function which calls <code>func</code> configured with <code>opts</code>.
        # @type func fn
        # @return fn
        # @example print_weird := Configure[
        # @example     Print,
        # @example     joiner->", ",
        # @example     before->"{",
        # @example     after->"}\n"
        # @example ]
        # @example
        # @example print_weird[3, 4, 5]
        # @example ?? {3, 4, 5}
        # @genre functional
        #>>
        "Configure" => lambda { |inst, func, **opts|
            lambda { |inst, *args|
                func[inst, *args, **opts]
            }
        },
        #<<
        # Defines <code>name</code> in the global scope as <code>value</code>. Returns <code>value</code>.
        # @return (*)
        # @genre scope
        # @type name string
        # @type value (*)
        # @example Define[$a, "This is variable a!"]
        # @example Print[a]
        # @example ?? This is variable a!
        #>>
        "Define" => lambda { |inst, name, value|
            inst.define name, value
        },
        #<<
        # Returns the value of the variable whose name is <code>name</code>.
        # @return (*)
        # @type name string
        # @genre scope
        # @example a := 323
        # @example Print[Retrieve[$a]]
        # @example ?? 323
        #>>
        "Retrieve" => lambda { |inst, name|
            inst.get_variable name
        },
        #<<
        # Displays <code>ent</code> in a human-reversible format. Primarily, prints the elements of arrays (whose dimension is at least 2) on separate lines.
        # @return (*)
        # @type ent (*)
        # @genre IO
        # @example Display[ [1:3, 4:6, 9:11] ]
        # @example ??  1  2  3
        # @example ??  4  5  6
        # @example ??  9 10 11
        # @example Display[ "Hello, World!" ]
        # @example ?? "Hello, World!"
        #>>
        "Display" => lambda { |inst, ent|
            display ent
        },
        #<<
        # Evaluates <code>str</code> in a new scope. Returns the last expression evaluated.
        # @return (*)
        # @type str string
        # @genre meta
        #>>
        "Eval" => lambda { |inst, str|
            AtState.new(str).run.last
        },
        #<<
        # Evaluates <code>str</code> in a the current scope. Returns the last expression evaluated.
        # @return (*)
        # @type str string
        # @genre meta
        #>>
        "EvalHere" => lambda { |inst, str|
            ast(str).map { |tree|
                inst.evaluate_node tree
            }.last
        },
        #<<
        # Terminates the program with exit code <code>0</code>. Flushes STDOUT.
        # @optional code
        # @param code the code to exit with.
        # @type code number
        # @return nil
        # @genre IO
        #>>
        "Exit" => lambda { |inst, code=0|
            STDOUT.flush
            exit! code
        },
        #<<
        # Creates a hash.
        # @return Hash[string -> (*)]
        # @type opts ConfigureValue
        # @param opts A series of value pairs <code>a -> v</code>.
        # @genre data
        #>>
        "Hash" => lambda { |inst, *opts|
            res = {}
            # p opts
            opts.each { |k, v|
                res[k] = v
            }
            res
        },
        "Hold" => lambda { |inst, fn|
            
        },
        #<<
        # Defines <code>name</code> in the local scope as <code>value</code>. Returns <code>value</code>.
        # @return (*)
        # @genre scope
        # @type name string
        # @type value (*)
        #>>
        "Local" => lambda { |inst, name, value|
            inst.define_local name, value
        },
        #<<
        # Modifies <code>head</code> according to <code>body</code>.
        # @type head string
        # @param body an expression using the first abstract variable <code>_</code> to represent the original value.
        # @type body expression
        # @return (*)
        # @genre scope
        # @example a := 3
        # @example Modify[$a, _ * 2 + 1]
        # @example Print[a]
        # @example ?? 7
        #>>
        "Modify" => lambda { |inst, head, body|
            init = inst.get_variable head
            result = inst.evaluate_node body, [init]
            inst.define head, result
        },
        #<<
        # Imports libraries. Returns list of libaries' names included.
        # @type libs string
        # @param libs list of strings corresponding to library names.
        # @genre meta
        # @return [string]
        #>>
        "Needs" => lambda { |inst, *libs|
            libs.each { |lib|
                inst.load_lib lib
            }
        },
        #<<
        # Prints <code>prompt</code> and waits for character input found in <code>opts</code>.
        # @type prompt string
        # @return string
        # @genre IO
        # @example input := Option["Press something!",
        # @example     s -> "stop this program",
        # @example     i -> "print info",
        # @example     h -> "print hello world"
        # @example ]
        # @example ?? outputs "Press something!"
        # @example ?? user presses the `i` key
        # @example Print["Received:", Repr[input]]
        # @example ?? Received: "i"
        #>>
        "Option" => lambda { |inst, prompt, **opts|
            read_option prompt, opts
        },
        #<<
        # Prints each argument of <code>args</code>, separated by spaces.
        # @type args (*)
        # @return [(*)]
        # @option after String printed after everything else. Default: <code>"\n"</code>.
        # @option before String printed before everything else. Default: <code>""</code>.
        # @option joiner The string which joins <code>args</code>. Default: <code>" "</code>.
        # @genre IO
        #>>
        "Print" => lambda { |inst, *args, **opts|
            joiner = opts[:joiner] || " "
            inst.out.print opts[:before] || ""
            inst.out.print args.map { |e|
                inst.cast_string e
            }.join(joiner)
            inst.out.print opts[:after] || "\n"
            args
        },
        #<<
        # Prompts for a line of input. Returns string without trailing newline.
        # @return string
        # @type prompt string
        # @param prompt String to display before input text. Nothing if omitted.
        # @optional prompt
        # @genre IO
        #>>
        "Prompt" => lambda { |inst, prompt=nil|
            prompt_input prompt, inst.in
        },
        #<<
        # Reads a line of input, to include any trailing newline.
        # @return string
        # @genre IO
        #>>
        "ReadLine" => lambda { |inst|
            inst.in.gets
        },
        #<<
        # Iterates <code>func</code> over lines of stdin received by <code>Prompt[...args]</code>.
        # Returns a list of these lines modified by <code>func</code>.
        # @return [string]
        # @type args (*)
        # @type func fn[string -> string]
        # @param func A function that receives lines of input and returns a string.
        # @genre IO
        #>>
        "ReadLineLoop" => lambda { |inst, *args, func|
            lines = []
            loop {
                line = @@functions["Prompt"][inst, *args]
                break if line.nil?
                lines << func[inst, line]
            }
            lines
        },
        #<<
        # Reads a character from the input.
        # @return string
        # @genre IO
        #>>
        "ReadChar" => lambda { |inst|
            inst.in.getc
        },
        #<<
        # Reads an integer from the input.
        # @return number
        # @genre IO
        #>>
        "ReadInt" => lambda { |inst|
            inst.in.gets.chomp.to_i
        },
        #<<
        # Reads a float from the input.
        # @return number
        # @genre IO
        #>>
        "ReadFloat" => lambda { |inst|
            inst.in.gets.chomp.to_f
        },
        #<<
        # Reads a number from the input.
        # @return number
        # @genre IO
        #>>
        "ReadNumber" => lambda { |inst|
            force_number inst.in.gets.chomp
        },
        #<<
        # Updates values of global abstracts, respective to <code>args</code>. <code>_1</code> would become <code>args[0]</code>, and <code>_<em>n</em></code> would become <code>args[n-1]</code>.
        # @type args (*)
        # @return [(*)]
        # @genre meta
        #>>
        "Save" => lambda { |inst, *args|
            inst.saved = args
        },
        #<<
        # Reads all of STDIN.
        # @return string
        # @genre IO
        #>>
        "Stdin" => lambda { |inst|
            STDIN.read
        },
        #<<
        # Writes <code>args</code>, joined together, to STDOUT.
        # @type args (*)
        # @genre IO
        # @return nil
        #>>
        "Stdout" => lambda { |inst, *args|
            print args.flatten.join
        },
        #<<
        # Returns an array of the arguments.
        # @type args (*)
        # @return [(*)]
        # @genre data
        #>>
        "V" => lambda { |inst, *args|
            args
        },

        ##---------##
        ## File IO ##
        ##---------##
        #<<
        # Returns the contents of file <code>name</code>, or <code>nil</code> if the file does not exist.
        # @type name string
        # @return string
        # @genre IO/files
        # @option encoding The encoding that the target file is in.
        #>>
        "FileRead" => lambda { |inst, name, **opts|
            opts[:encoding] ||= "UTF-8"
            File.read(name.strip, encoding: opts[:encoding]) rescue nil
        },
        #<<
        # Writes <code>content</code> to file <code>name</code>. Returns the number of bytes written.
        # @type name string
        # @type content string
        # @return number
        # @genre IO/files
        # @option encoding The encoding that the target file is in.
        #>>
        "FileWrite" => lambda { |inst, name, content, **opts|
            opts[:encoding] ||= "UTF-8"
            File.write(name.strip, content, encoding: opts[:encoding])
        },
        #<<
        # Returns <code>true</code> if <code>name</code> represents a valid file, <code>false</code> otherwise.
        # @type name string
        # @return bool
        # @genre IO/files
        #>>
        "FileExists" => lambda { |inst, name|
            File.exists? name.strip
        },


        #################
        #### CLASSES ####
        #################
        #<<
        # Creates an anonymous class.
        # @type body fn[nil -> (*)]
        # @param body Any local definition made within constitutes a method or instance variable decleration.
        # @return class
        # @genre class
        #>>
        "Class" => lambda { |inst, body|
            AtClass.new inst, body
        },
        #<<
        # Instantiates a class with parameters <code>args</code>.
        # @type ac class
        # @type args (*)
        # @return class[]
        # @genre class
        #>>
        "New" => lambda { |inst, ac, *args|
            ac.create(*args)
        },

        #############################
        #### UNIVERSAL FUNCTIONS ####
        #############################
        #<<
        # Identity function; returns its argument.
        # @return (*)
        # @type a (*)
        # @genre functional
        #>>
        "Id" => lambda { |inst, a|
            a
        },
        #<<
        # Determines if <code>ent</code> is palindromic, that is, if it is itself reversed.
        # @type ent [(*)]|string
        # @return bool
        # @genre list/logic
        #>>
        "Palindromic" => lambda { |inst, ent|
            reverse(ent) == ent
        },
        #<<
        # Reverses the elements of <code>ent</code>.
        # @return (*)
        # @type ent (*)
        # @genre list
        #>>
        "Reverse" => lambda { |inst, ent|
            reverse ent
        },

        ##########################
        #### HYBRID FUNCTIONS ####
        ##########################
        #<<
        # Obtains all non-negative values under <code>max</code> by repeating <code>f</code>.
        # @return [number]
        # @type f fn[number]
        # @type max number
        # @type start number
        # @optional start
        # @genre functional
        # @option include determines whether or not to include <code>max</code> as an upperbound. Default: <code>false</code>
        # @example Print[Series[Prime, 13]]
        # @example ?? [2, 3, 5, 7, 11]
        # @example Print[Series[Prime, 13, include->true]]
        # @example ?? [2, 3, 5, 7, 11, 13]
        #>>
        "Series" => lambda { |inst, f, max, start=0, **config|
            i = start
            collect = []
            loop {
                value = f[inst, i]
                unless value.nil?
                    break if config[:include] ? value > max : value >= max
                    collect.push value
                end
                i += 1
            }
            collect
        },
        #<<
        # Obtains all non-negative values under <code>max</code> which satisfy <code>cond</code> by repeating <code>f</code>.
        # @return [number]
        # @type f fn[n]
        # @type cond fn[n->b]
        # @type max number
        # @type start number
        # @optional start
        # @genre functional
        # @option include determines whether or not to include <code>max</code> as an upperbound. Default: <code>false</code>
        # @example Print[SeriesIf[Prime, Odd, 13]]
        # @example ?? [3, 5, 7, 11]
        # @example Print[SeriesIf[Prime, Odd, 13, include->true]]
        # @example ?? [3, 5, 7, 11, 13]
        #>>
        "SeriesIf" => lambda { |inst, f, cond, max, start=0, **config|
            i = start
            collect = []
            loop {
                value = f[inst, i]
                unless value.nil?
                    break if config[:include] ? value > max : value >= max
                    collect.push value if cond[inst, value]
                end
                i += 1
            }
            collect
        },
        #<<
        # Returns elements from <code>list</code> until before the first element which does not satisfy <code>cond</code>.
        # @type cond fn
        # @type list [(*)]
        # @return list
        # @reforms
        # @genre functional
        # @example Display[TakeWhile[{_ /= sp}, "Hello, World!"]]
        # @example ?? "Hello,"
        # @example Print[TakeWhile[Even, [2, 4, 6, 1, 2, 3]]]
        # @example ?? [2, 4, 6]
        # @example Print[TakeWhile[IsPrime, 357923]]
        # @example ?? 357
        #>>
        "TakeWhile" => lambda { |inst, cond, list|
            collect = []

            force_list(list).each { |el|
                break unless cond[inst, el]
                collect << el
            }

            reform_list collect, list
        },
        #<<
        # Starting at <code>n = start</code>, increments <code>n</code> until
        # <code>cond[f[n]]</code> is truthy. Returns <code>f[n]</code>. Note: this ignores <code>nil</code> values returned.
        # @type f fn
        # @type cond fn
        # @type start number
        # @optional start
        # @param f A function that maps a number to a domain, such as <code>Square</code> or <code>Prime</code>.
        # @param cond A function which, given a possible result of <code>f</code>, returns a truthy or falsey value.
        # @param start The starting point for testing numbers.
        # @return (*)
        # @genre functional
        #>>
        "GenerateFirst" => lambda { |inst, f, cond, start=0|
            res = nil
            n = start
            until !res.nil? && AtState.truthy?(cond[inst, res = f[inst, n]])
                n += 1
            end
            res
        },
        #<<
        # Similar to <a href="#GenerateFirst"><code>GenerateFirst</code></a>, returns the first <code>size</code> values of <code>f</code> satisfying <code>cond</code>
        # @type size number
        # @type f fn
        # @type cond fn
        # @type start number
        # @optional start
        # @param f A function that maps a number to a domain, such as <code>Square</code> or <code>Prime</code>.
        # @param cond A function which, given a possible result of <code>f</code>, returns a truthy or falsey value.
        # @param start The starting point for testing numbers.
        # @return (*)
        # @genre functional
        #>>
        "GenerateN" => lambda { |inst, f, cond, size, start=0|
            res = nil
            collect = []
            n = start
            until collect.size >= size
                res = f[inst, n]
                if !res.nil? && AtState.truthy?(cond[inst, res])
                    collect << res
                end
                n += 1
            end
            collect
        },

        ###########################
        #### NUMERIC FUNCTIONS ####
        ###########################
        #<<
        # Calculates the absolute value of <code>n</code>.
        # @return number
        # @type n number
        # @genre numeric
        #>>
        "Abs" => vectorize_monad { |inst, n|
            n.abs
        },
        #<<
        # Adds each of <code>args</code> together.
        # @return number
        # @type args number
        # @genre numeric
        #>>
        "Add" => lambda { |inst, *args|
            @@functions["Sum"][inst, args]
        },
        #<<
        # Converts <code>n</code> to base <code>2</code>.
        # @type n number
        # @return number
        # @genre numeric/bases
        #>>
        "Bin" => vectorize_monad { |inst, n|
            @@functions["ToBase"][inst, n, 2]
        },
        #<<
        # Returns <code>n</code> rounded up to the nearest integer.
        # @return number
        # @type n number
        # @param r the precision to round. No precision if omitted.
        # @type r number
        # @optional r
        # @genre numeric
        #>>
        "Ceiling" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.ceil
            else
                n.ceil(r)
            end
        },
        #<<
        # Converts <code>arg</code> to characters.
        # @type arg number|[number]
        # @return string
        # @paramtype number arg Converts <code>arg</code> to a character.
        # @paramtype [number] arg Converts <code>arg</code> to a string of char codes.
        # @genre conversion
        #>>
        "Char" => lambda { |inst, arg|
            if arg.is_a? Array
                arg.map(&:chr).join
            else
                arg.chr
            end
        },
        #<<
        # Produces the Collatz sequence of <code>n</code>.
        # @type n number
        # @return [number]
        # @genre numeric/series
        #>>
        "Collatz" => vectorize_monad { |inst, n|
            collatz n
        },
        #<<
        # Returns the number of steps it takes for <code>n</code> to reach <code>1</code> according to the Collatz transformation.
        # @type n number
        # @return number
        # @genre numeric/series
        #>>
        "CollatzSize" => vectorize_monad { |inst, n|
            collatz(n).size - 1
        },
        #<<
        # Doubles <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Double" => vectorize_monad { |inst, n|
            @@operators["*"][inst, n, 2]
        },
        #<<
        # Produces the digits of <code>n</code>.
        # @type n number
        # @return [number]
        # @genre numeric
        #>>
        "Digits" => vectorize_monad { |list, n|
            n.digits.reverse
        },
        #<<
        # Divides each number in <code>args</code> by the next. That is, folding division over <code>args</code>
        # @type args number
        # @return number
        # @genre numeric
        #>>
        "Divide" => lambda { |inst, *args|
            args[0] = args[0].to_f
            args.inject(:/)
        },
        #<<
        # Returns the <code>n</code>th number in the Fibonacci sequence, starting with <code>f<sub>0</sub> = 0</code> and <code>f<sub>1</sub> = 1</code>.
        # @type n number
        # @return number
        # @genre numeric/series
        #>>
        "Fibonacci" => lambda { |inst, n|
            nth_fibonacci(n)
        },
        #<<
        # Returns <code>n</code> rounded down to the nearest integer.
        # @return number
        # @type n number
        # @param r the precision to round. No precision if omitted.
        # @type r number
        # @optional r
        # @genre numeric
        #>>
        "Floor" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.floor
            else
                n.floor(r)
            end
        },
        #<<
        # Converts <code>num</code> from base <code>base</code>
        # to base <code>10</code>.
        # @return number
        # @type num [number]
        # @param num an array of digits representing the number in base <code>base</code>.
        # @type base number
        # @param base a number greater than <code>0</code>, representing the source base of the numeric array.
        # @genre numeric/bases
        #>>
        "FromBase" => vectorize_dyad(RIGHT) { |inst, num, base|
            from_base num, base
        },
        #<<
        # Takes the Greatest Common Divisor of the atoms of <code>args</code>.
        # @type args number
        # @param args List of numbers, which can have nested elements.
        # @return number
        # @genre numeric
        #>>
        "GCD" => lambda { |inst, *args|
            gcd args.flatten
        },
        #<<
        # Converts <code>n</code> to base <code>16</code>.
        # @type n number
        # @return number
        # @genre numeric/bases
        #>>
        "Hex" => vectorize_monad { |inst, n|
            @@functions["ToBase"][inst, n, 16]
        },
        #<<
        # Takes half of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Halve" => vectorize_monad { |inst, n|
            @@operators["/"][inst, n, 2]
        },
        #<<
        # Takes the Least Common Multiple of the atoms of <code>args</code>.
        # @type args number
        # @param args List of numbers, which can have nested elements.
        # @return number
        # @genre numeric
        #>>
        "LCM" => lambda { |inst, *args|
            lcm args.flatten
        },
        #<<
        # Takes the base-<code>10</code> logarithm of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Log" => lambda { |inst, n|
            Math::log10 n
        },
        #<<
        # Takes the natural logarithm of <code>n</code>. Note that <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>ln</mi><mo>(</mo><mi>n</mi><mo>)</mo><mo>=</mo><msub><mi>log</mi><mi>e</mi></msub><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @return number
        # @type n number
        # @genre numeric
        #>>
        "Ln" => lambda { |inst, n|
            Math::log n
        },
        #<<
        # Takes the product of the elements of <code>args</code>.
        # @return number
        # @type args number
        # @genre numeric
        #>>
        "Multiply" => lambda { |inst, *args|
            @@functions["Prod"][inst, args]
        },
        #<<
        # Converts <code>ent</code> to an integer.
        # @type ent [number]|string|number|(*)
        # @return number
        # @genre conversion
        # @paramtype [number] ent Converts <code>ent</code> to base <code>10</code>.
        # @paramtype string ent Parses <code>ent</code> as a base <code>10</code> integer.
        # @paramtype number ent Converts <code>ent</code> to an integer if it represents one.
        # @paramtype (*) ent Attempts to cast <code>ent</code> to an integer.
        #>>
        "N" => lambda { |inst, ent|
            force_number ent
        },
        #<<
        # Converts <code>n</code> to base <code>8</code>.
        # @type n number
        # @return number
        # @genre numeric/bases
        #>>
        "Oct" => vectorize_monad { |inst, n|
            @@functions["ToBase"][inst, n, 8]
        },
        #<<
        # Returns <code>[a - b, a + b]</code>.
        # @return [number]
        # @type a number
        # @type b number
        # @genre numeric
        #>>
        "PlusMinus" => vectorize_dyad { |inst, a, b|
            [@@operators["+"][inst, a, b], @@operators["-"][inst, a, b]]
        },
        #<<
        # Obtains the <code>n</code>th <code>order</code>-agonal number.
        # @return number
        # @type n number
        # @type order number
        # @genre numeric/series
        # @example (* The triangular numbers (default) *)
        # @example Print[Polygonal[0:5, 3]]
        # @example ?? [0, 1, 3, 6, 10, 15]
        # @example Print[Polygonal[0:5]]
        # @example ?? [0, 1, 3, 6, 10, 15]
        # @example
        # @example (* The square numbers *)
        # @example Print[Polygonal[0:5, 4]]
        # @example ?? [0, 1, 4, 9, 16, 25]
        #>>
        "Polygonal" => vectorize_dyad { |inst, n, order=3|
            gonal n, order
        },
        #<<
        # Returns the <code>n</code>th enumeration of the Pythagorean triples.
        # @return [number]
        # @type n number
        # @genre numeric/series
        # @example Print => Pythagorean[0:10]
        # @example ?? [3, 4, 5]
        # @example ?? [8, 6, 10]
        # @example ?? [5, 12, 13]
        # @example ?? [15, 8, 17]
        # @example ?? [12, 16, 20]
        # @example ?? [7, 24, 25]
        # @example ?? [24, 10, 26]
        # @example ?? [21, 20, 29]
        # @example ?? [16, 30, 34]
        # @example ?? [9, 40, 41]
        # @example ?? [35, 12, 37]
        #>>
        "Pythagorean" => vectorize_monad { |inst, n|
            pythagorean n
        },
        #<<
        # Returns a pseudo-random number. By default, returns a random float between <code>0</code> (inclusive) and <code>1</code> (exclusive).
        # @param n If <code>m</code> is omitted, returns a random integer between <code>0</code> (inclusive) and <code>n</code> (exclusive).
        # @param m When provided, returns a random integer between <code>n</code> and <code>m</code>, inclusive.
        # @optional n
        # @optional m
        # @type n number
        # @type m number
        # @return number
        # @genre numeric
        #>>
        "Random" => vectorize_dyad { |inst, n=nil, m=nil|
            random(n, m)
        },
        #<<
        # Returns a fraction representing <code>a / b</code>.
        # @return rational
        # @type a number
        # @type b number
        # @genre numeric
        #>>
        "Rational" => vectorize_dyad { |inst, a, b|
            Rational(a, b)
        },
        #<<
        # Returns <code>n</code> rounded half-up to the nearest integer.
        # @return number
        # @type n number
        # @param r the precision to round. No precision if omitted.
        # @type r number
        # @optional r
        # @genre numeric
        #>>
        "Round" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.round
            else
                n.round(r)
            end
        },
        #<<
        # Returns the sign of <code>n</code>; Returns <code>-1</code> if <code>n</code> is negative, <code>1</code> if it is positive, and <code>0</code> otherwise.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Sign" => vectorize_monad { |inst, n|
            sign n
        },
        #<<
        # Returns the numerator of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric/rational
        #>>
        "Numerator" => vectorize_monad { |inst, n|
            Rational(n).numerator
        },
        #<<
        # Returns the numerator of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric/rational
        #>>
        "Denominator" => vectorize_monad { |inst, n|
            Rational(n).denominator
        },
        #<<
        # Returns the square root of <code>n</code>. The result is imaginary if <code>n</code> is negative.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Sqrt" => vectorize_monad { |inst, n|
            CMath.sqrt n
        },
        #<<
        # Returns the square of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Square" => vectorize_monad { |inst, n|
            @@operators["*"][inst, n, n]
        },
        #<<
        # Subtracts each number in <code>args</code> by the next. That is, folding subtraction over <code>args</code>.
        # @type args number
        # @return number
        # @genre numeric
        #>>
        "Subtract" => lambda { |inst, *args|
            args.inject(:-)
        },
        #<<
        # Converts <code>num</code> to base <code>base</code>.
        # @type num number
        # @type base number
        # @return [number]
        # @genre numeric/bases
        #>>
        "ToBase" => vectorize_dyad { |inst, num, base|
            to_base num, base
        },
        #<<
        # Returns the <code>n</code>th Triangular number.
        # @type n number
        # @return number
        # @example Print[Triangular[0:5]]
        # @example Print[Polygonal[0:5, 3]]
        # @example
        # @example ?? [0, 1, 3, 6, 10, 15]
        # @genre numeric/series
        #>>
        "Triangular" => vectorize_monad { |inst, n|
            gonal n, 3
        },
        #<<
        # Converts from base <code>2</code> (binary) to base <code>10</code>.
        # @genre numeric/bases
        # @return number
        # @type n [number]
        #>>
        "UnBin" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 2]
        },
        #<<
        # Converts from base <code>16</code> (hexadecimal) to base <code>10</code>.
        # @genre numeric/bases
        # @return number
        # @type n [number]
        #>>
        "UnHex" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 16]
        },
        #<<
        # Converts from base <code>8</code> (octal) to base <code>10</code>.
        # @genre numeric/bases
        # @return number
        # @type n [number]
        #>>
        "UnOct" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 8]
        },

        ##-------------------------##
        ## Trigonometric Functions ##
        ##-------------------------##
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>cos</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcCos" => vectorize_monad { |inst, n|
            Math::acos n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>sin</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcSin" => vectorize_monad { |inst, n|
            Math::asin n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>tan</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcTan" => vectorize_monad { |inst, n|
            Math::atan n
        },
        #<<
        # Calculates the principal value of <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>tan</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>y</mi><mo>/</mo><mi>x</mi><mo>)</mo></math>, or <code>atan2(y, x)</code>.
        # @type y number
        # @type x number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcTan2" => vectorize_dyad { |inst, y, x|
            Math::atan2 y, x
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>cos</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Cos" => vectorize_monad { |inst, n|
            Math::cos n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>sin</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Sin" => vectorize_monad { |inst, n|
            Math::sin n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>tan</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Tan" => vectorize_monad { |inst, n|
            Math::tan n
        },

        ##-----------------##
        ## Prime Functions ##
        ##-----------------##
        #<<
        # Returns <code>true</code> if <code>n</code> is prime, and <code>false</code> otherwise.
        # @type n number
        # @return bool
        # @genre numeric/prime
        #>>
        "IsPrime" => vectorize_monad { |inst, n|
            Prime.prime? n
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is composite (not prime), and <code>false</code> otherwise.
        # @type n number
        # @return bool
        # @genre numeric/prime
        #>>
        "IsComposite" => vectorize_monad { |inst, n|
            !Prime.prime? n
        },
        #<<
        # Returns the next prime greater than <code>n</code>.
        # @return number
        # @type n number
        # @type rep number
        # @optional rep
        # @param rep When specified, returns the <code>rep</code>th prime after <code>n</code>.
        # @genre numeric/prime
        #>>
        "NextPrime" => vectorize_dyad { |inst, n, rep=1|
            rep.times {
                n += 1 + n % 2
                n += 2 until Prime.prime? n
            }
            n
        },
        #<<
        # Returns the closest previous prime before <code>n</code>.
        # @return number
        # @type n number
        # @type rep number
        # @optional rep
        # @param rep When specified, returns the <code>rep</code>th prime before <code>n</code>.
        # @genre numeric/prime
        #>>
        "PreviousPrime" => vectorize_dyad { |inst, n, rep=1|
            rep.times {
                break n = nil if n <= 2
                if n == 3
                    n = 2
                else
                    n -= 1 + n % 2
                    n -= 2 until Prime.prime? n
                end
            }
            n
        },
        #<<
        # Returns the <code>n</code>th prime, starting at <Code>Prime[1] = 2</code>.
        # @type n number
        # @return number
        # @genre numeric/prime
        # @example Print[Prime[1:10]]
        #>>
        "Prime" => vectorize_monad { |inst, n|
            nth_prime n
        },
        #<<
        # Returns a list of the prime exponents of <code>n</code>. That is, a list of base-exponent pairs <code>[[p1, e1], ..., [pN, eN]]</code> such that <math xmlns="http://www.w3.org/1998/Math/MathML"><msub><mi>p</mi><mi>k</mi></msub><mo>&#x2208;</mo><mi mathvariant="normal">&#x2119;</mi></math> and <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>n</mi><mo>=</mo><munder><mo>&#x220F;</mo><mrow><mn>1</mn><mo>&#x2264;</mo><mi>k</mi><mo>&lt;</mo><mi>n</mi></mrow></munder><msubsup><mi>p</mi><mi>k</mi><msub><mi>e</mi><mi>k</mi></msub></msubsup></math>.
        # @return [[number, number]]
        # @type n number
        # @genre numeric/prime
        #>>
        "PrimeDivision" => vectorize_monad { |inst, n|
            Prime.prime_division n
        },
        #<<
        # Returns a list of the prime factors of <code>n</code> with duplicates.
        # @return [number]
        # @type n number
        # @genre numeric/prime
        #>>
        "PrimeFactors" => vectorize_monad { |inst, n|
            prime_factors n
        },
        #<<
        # Returns a list of the first <code>n</code> primes.
        # @return [number]
        # @type n number
        # @genre numeric/prime
        #>>
        "Primes" => vectorize_monad { |inst, n|
            Prime.first n
        },
        # "PrimePi" => vectorize_monad { |inst, n|

        # },
        #<<
        # Returns the number of unique prime factors of <code>n</code>.
        # @return number
        # @type n number
        # @genre numeric/prime
        #>>
        "PrimeNu" => vectorize_monad { |inst, n|
            prime_factors(n).uniq.size
        },
        #<<
        # Returns the number of prime factors of <code>n</code>.
        # @return number
        # @type n number
        # @genre numeric/prime
        #>>
        "PrimeOmega" => vectorize_monad { |inst, n|
            prime_factors(n).size
        },

        ##------------------------##
        ## Number Logic Functions ##
        ##------------------------##
        #<<
        # Returns <code>true</code> if <code>n</code> is even (a multiple of <code>2</code>), otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Even" => vectorize_monad { |inst, n|
            n.even?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> = <code>a+bi</code> for <code>b /= 0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Imaginary" => vectorize_monad { |inst, n|
            unless n.real?
                n.imaginary != 0
            else
                false
            end
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is less than <code>0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Negative" => vectorize_monad { |inst, n|
            n.negative?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is odd (not a multiple of <code>2</code>), otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Odd" => vectorize_monad { |inst, n|
            n.odd?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is greater than <code>0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Positive" => vectorize_monad { |inst, n|
            n.positive?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> has no imaginary part, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Real" => vectorize_monad { |inst, n|
            unless n.real?
                n.imaginary == 0
            else
                true
            end
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is <code>0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Zero" => vectorize_monad { |inst, n|
            n.zero?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> represents an integer, otherwise <code>false</code>.
        # @type n number
        # @return bool
        # @genre numeric/logic
        #>>
        "Integral" => vectorize_monad { |inst, n|
            n == n.floor
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is a numeric value, otherwise <code>false</code>.
        # @type n number
        # @return bool
        # @genre numeric/logic
        #>>
        "Numeric" => lambda { |inst, n|
            Numeric === n
        },

        ##############################
        #### FUNCTIONAL FUNCTIONS ####
        ##############################
        #<<
        # Returns a function <code>fn[...args]</code> such that the <code>cond[...args]</code>th element of <code>flist</code> is called upon <code>args</code>.
        # @return fn[(*) -> (*)]
        # @type flist fn
        # @param flist A list of functions.
        # @type cond fn[(*) -> number]
        # @param cond A function which produces an index by which to obtain a function from <code>flist</code>.
        # @example f := Agenda[ [Halve, {3 * _ + 1}], Odd]
        # @example Print[Map[f, [1, 2, 3, 4]]]
        # @example ?? [4, 1, 10, 2]
        # @genre functional
        #>>
        "Agenda" => lambda { |inst, flist, cond|
            lambda { |inst, *args|
                ind = from_numlike cond[inst, *args]
                flist[ind][inst, *args]
            }
        },
        #<<
        # Calls <code>func</code> with <code>arg_arr</code>.
        # @type func fn
        # @type arg_arr [(*)]
        # @return (*)
        # @example args := [ Reverse, [ "hello", "world" ] ]
        # @example Print[ Apply[Map, args] ]
        # @example ?? ["olleh", "dlrow"]
        # @genre functional
        #>>
        "Apply" => lambda { |inst, func, arg_arr|
            func[inst, *arg_arr]
        },
        #<<
        # Returns a function <code>fn[...args]</code> which applies <code>func</code> to <code>args</code>.
        # @type func fn
        # @return fn[[(*)]]
        # @genre functional
        # @example f := Applier[Print]
        # @example f[1:5]
        # @example Print[1, 2, 3, 4, 5]
        # @example ?? 1 2 3 4 5
        #>>
        "Applier" => lambda { |inst, func|
            lambda { |inst, args|
                func[inst, *args]
            }
        },
        #<<
        # Bonds <code>larg</code> to the left side of <code>func</code>. That is, <code>Bond[func, larg][...args]</code> is the same as <code>func[larg, ...args]</code>.
        # @type func fn
        # @type larg (*)
        # @return fn
        # @genre functional
        #>>
        "Bond" => lambda { |inst, func, larg|
            lambda { |inst, *args|
                func[inst, larg, *args]
            }
        },
        #<<
        # Returns a function that returns <code>arg</code>.
        # @type arg (*)
        # @return fn
        # @genre functional
        #>>
        "C" => lambda { |inst, arg|
            lambda { |inst, *discard|
                arg
            }
        },
        #<<
        # Calls <code>f</code> over <code>args</code>.
        # @type args (*)
        # @type f fn
        # @return (*)
        # @genre functional
        # @example Call[Print, "Hello", "World"]
        # @example ?? Hello World
        #>>
        "Call" => lambda { |inst, f, *args|
            f[inst, *args]
        },
        #<<
        # Returns <code>fn</code>, fully curried according to <code>arity</code>.
        # @type f fn
        # @type arity number
        # @optional arity
        # @return fn
        # @param arity Default: <code>#f</code>.
        # @genre functional
        # @example add_dy := [x, y] -> { x + y }
        # @example adder := Curry[add_dy]
        # @example add3 := adder[3]
        # @example Print[add3[5]]
        # @example ?? 8
        # @example Print[adder[2][5]]
        # @example ?? 7
        # @example
        # @example add_tri := Curry[Add/3]
        # @example adder1 := add_tri[1]
        # @example add6 := adder1[5]
        # @example Print[add6[10]]
        # @example ?? 16
        #>>
        "Curry" => lambda { |inst, f, arity=f.arity|
            if arity.negative?
                arity = ~arity
            end

            arity += 1 if AtLambda === f || AtFunction === f

            rec = lambda { |fn, inst, *args|
                if args.size >= arity - 1
                    fn[inst, *args]
                else
                    lambda { |inst, *more|
                        rec[fn, inst, *args, *more]
                    }
                end
            }
            rec[f, inst]
        },
        #<<
        # Applies <code>f</code> to <code>n</code> until <code>f[n]</code> converges.
        # @type f fn
        # @type n (*)
        # @return (*)
        # @genre functional
        #>>
        "Fixpoint" => lambda { |inst, f, n|
            fixpoint f.bind(inst), n
        },
        #<<
        # Composes the functions <code>f</code>, <code>g</code>, and <code>h</code> into a fork. When called with arguments <code>args</code>, this is equivalent to calling <code>g[f[...args], h[...args]]</code>.
        # @type f fn
        # @type g fn
        # @type h fn
        # @return fn
        # @genre functional
        # @example avg := Fork[Sum, Divide, Size]
        # @example Print[avg[1:5]]
        # @example ?? 3.0
        #>>
        "Fork" => lambda { |inst, f, g, h|
            lambda { |inst, *args|
                g[inst, f[inst, *args], h[inst, *args]]
            }
        },
        # "Group" => lambda { |inst, arr|

        # },
        #<<
        # Composes the functions <code>f</code> and <code>g</code> into a hook. When called with arguments <code>args</code> this is equivalent to calling <code>f[First[args], g[...args]]</code>.
        # @type f fn
        # @type g fn
        # @return fn
        # @genre functional
        #>>
        "Hook" => lambda { |inst, f, g|
            lambda { |inst, *args|
                f[inst, args.first, g[inst, *args]]
            }
        },
        #<<
        # Given a function <code>func</code>, returns a function which calls <code>func</code> with an additional
        # argument representing the number of times <code>func</code> has been invoked.
        # @type func fn
        # @return fn
        # @genre functional
        #>>
        "InvocationIndex" => lambda { |inst, func|
            i = -1
            lambda { |inst, *args|
                func[inst, *args, i += 1]
            }
        },
        #<<
        # Ties <code>args</code> together.
        # @type args (*)|fn
        # @return (*)|fn
        # @paramtype (*) args Concatenates all <code>args</code> together.
        # @paramtype fn args Creates a tie between all of <code>args</code>.
        # @genre functional
        # @example Print[Tie[1:3, 5:7]]
        # @example ?? [1, 2, 3, 5, 6, 7]
        # @example f := Tie[Double, Halve]
        # @example Print[f[1, 2, 3, 4, 5, 6]]
        # @example ?? [2, 1, 6, 2, 10, 3]
        #>>
        "Tie" => lambda { |inst, *args|
            if args.any? { |e| !AtState.func_like? e }
                args.inject([]) { |acc, e| [*acc, *e] }
            else
                Tie.new args
            end
        },
        #<<
        # <code>Tie</code>, but applied to the first argument instead of all arguments.
        # @type funcs fn
        # @genre functional
        # @return fn
        # @example f := TieArray[Double, Halve]
        # @example Print[f[1:6]]
        # @example ?? [2, 1, 6, 2, 10, 3]
        #>>
        "TieArray" => lambda { |inst, *funcs|
            Tie.new funcs, true
        },
        #<<
        # Applies <code>f</code> to <code>e</code> <code>n</code> times.
        # @type f fn
        # @type e (*)
        # @type n number
        # @return (*)
        # @genre functional
        # @example Print[Nest[Double, 1, 3]]
        # @example ?? 8 (= 2 ^ 3)
        #>>
        "Nest" => lambda { |inst, f, e, n|
            from_numlike(n).times {
                e = f[inst, e]
            }
            e
        },
        #<<
        # Applies <code>f</code> to <code>e</code> <code>n</code> times, keeping the intermediate results.
        # @type f fn
        # @type e (*)
        # @type n number
        # @return (*)
        # @genre functional
        # @example Print[NestList[Double, 1, 3]]
        # @example ?? [1, 2, 4, 8]
        # @example Print[NestList[Double, 1, 3, first->false]]
        # @example ?? [2, 4, 8]
        # @option first Specifies whether or not to include the first element. Default: <code>true</code>.
        #>>
        "NestList" => configurable { |inst, f, e, n, **opts|
            opts[:first] = opts.has_key?(:first) ? opts[:first] : true
            list = opts[:first] ? [e] : []
            from_numlike(n).times {
                e = f[inst, e]
                list << e
            }
            list
        },
        #<<
        # Applies <code>f</code> to <code>init</code> until <code>cond[init]</code> is truthy.
        # @type f fn
        # @type cond fn
        # @type init (*)
        # @return (*)
        # @genre functional
        # @example Print[NestWhile[Halve, 100, Even]]
        # @example ?? 25
        #>>
        "NestWhile" => lambda { |inst, f, init, cond|
            iter = init
            while cond[inst, iter]
                iter = f[inst, iter]
            end
            iter
        },
        #<<
        # Returns a function which, given <code>(*) x</code>, applies <code>f</code> to <code>x</code> until a result occurs twice, then returns the list of intermediate steps.
        # @type f fn
        # @return fn
        # @genre functional
        # @example dig_root_steps := PeriodicSteps[Sum@Digits]
        # @example Print[dig_root_steps[1853]]
        # @example ?? [1853, 17, 8, 8]
        #>>
        "PeriodicSteps" => lambda { |inst, f|
            lambda { |inst, x|
                periodicloop f.bind(inst), x
            }
        },
        #<<
        # Returns a function which, given <code>(*) x</code>, applies <code>f</code> to <code>x</code> until a result occurs twice, then returns the final result.
        # @type f fn
        # @return fn
        # @genre functional
        # @example dig_root := Periodic[Sum@Digits]
        # @example Print[dig_root[1853]]
        # @example ?? 8
        #>>
        "Periodic" => lambda { |inst, f|
            # p "PERIODOC #{f}"
            lambda { |inst, x|
                periodicloop(f.bind(inst), x).last
            }
        },
        #<<
        # Bonds <code>larg</code> to the right side of <code>func</code>. That is, <code>Bond[func, rarg][...args]</code> is the same as <code>func[...args, rarg]</code>.
        # @type func fn
        # @type rarg (*)
        # @return fn
        # @genre functional
        #>>
        "RBond" => lambda { |inst, func, rarg|
            lambda { |inst, *args|
                func[inst, *args, rarg]
            }
        },


        #########################
        #### LOGIC FUNCTIONS ####
        #########################
        #<<
        # Returns <code>true</code> if all members of <code>Map[f, list]</code> are truthy.
        # @optional list
        # @type list [(*)]
        # @type f fn|[(*)]
        # @param list When omitted, returns <code>true</code> if all members of <code>f</code> are truthy. Otherwise, returns <code>false</code>. When specified, <code>f</code> <em>must</em> be a function.
        # @return bool
        # @genre logic
        #>>
        "All" => lambda { |inst, f, list=nil|
            if list.nil?
                f.all? { |e| AtState.truthy? e }
            else
                list.all? { |e| AtState.truthy?(f[inst, e]) }
            end
        },
        #<<
        # Returns <code>true</code> if any member of <code>Map[f, list]</code> is truthy.
        # @optional list
        # @type list [(*)]
        # @type f fn|[(*)]
        # @param list When omitted, returns <code>true</code> if any member of <code>f</code> is truthy. Otherwise, returns <code>false</code>. When specified, <code>f</code> <em>must</em> be a function.
        # @return bool
        # @genre logic
        #>>
        "Any" => lambda { |inst, f, list=nil|
            if list.nil?
                f.any? { |e| AtState.truthy? e }
            else
                list.any? { |e| AtState.truthy?(f[inst, e]) }
            end
        },
        #<<
        # Returns <code>Not[Any[...]]</code>.
        # @optional list
        # @type list [(*)]
        # @type f fn|[(*)]
        # @param list When omitted, returns <code>true</code> if any member of <code>f</code> is truthy. Otherwise, returns <code>false</code>. When specified, <code>f</code> <em>must</em> be a function.
        # @return bool
        # @genre logic
        #>>
        "None" => lambda { |inst, f, list=nil|
            !@@functions["Any"][inst, f, list]
        },
        #<<
        # Returns <code>true</code> if <code>arg</code> is falsey, <code>false</code> otherwise. (See also: <a href="#Falsey"><code>Falsey</code></a>.)
        # @return bool
        # @type arg (*)
        # @genre logic
        #>>
        "Not" => lambda { |inst, arg|
            AtState.falsey? arg
        },
        #<<
        # Returns <code>true</code> if <code>arg</code> is falsey, <code>false</code> otherwise. (See also: <a href="#Not"><code>Not</code></a>.)
        # @return bool
        # @type arg (*)
        # @genre logic
        #>>
        "Falsey" => lambda { |inst, arg|
            AtState.falsey? arg
        },
        #<<
        # If <code>cond</code> is truthy, evaluates <code>t</code>. Otherwise, if <code>f</code> is specified, evaluates <code>f</code>.
        # @return (*)
        # @type cond (*)
        # @type t expression
        # @type f expression
        # @optional f
        # @genre logic
        # @example If[3 = 4,
        # @example     Print["Logic does not hold!"],
        # @example     Print["Everything is situation normal."]
        # @example ]
        #>>
        "If" => lambda { |inst, cond, t, f=nil|
            res = if AtState.truthy? cond
                t
            else
                f
            end
            inst.evaluate_node_safe res
        },
        #<<
        # Determines whether or not <code>arg = func[arg]</code>
        # @return bool
        # @genre logic
        # @type arg (*)
        # @type func fn
        # @optional arg
        # @param arg when omitted, returns a function <code>f[x] = Invariant[func, x]</code>.
        #>>
        "Invariant" => lambda { |inst, func, arg=nil|
            if arg.nil?
                lambda { |inst, arg|
                    func[inst, arg] == arg
                }
            else
                func[inst, arg] == arg
            end
        },
        #<<
        # Selects all elements <code>el</code> in <code>list</code> whose respective member in <code>mask</code> is truthy.
        # @type mask [(*)]
        # @type list [(*)]
        # @return [(*)]
        # @example Print[Mask[ [true, false, true, true, false], 1:5]]
        # @example ?? [1, 3, 4]
        # @genre logic
        # @reforms
        #>>
        "Mask" => lambda { |inst, mask, list|
            masked = force_list(list).select.with_index { |e, i|
                AtState.truthy? mask[i]
            }
            reform_list masked, list
        },
        #<<
        # Returns <code>true</code> if <code>arg</code> is truthy, <code>false</code> otherwise.
        # @return bool
        # @type arg (*)
        # @genre logic
        #>>
        "Truthy" => lambda { |inst, arg|
            AtState.truthy? arg
        },
        #<<
        # While <code>cond</code> evaluates as truthy, evaluates <code>body</code>. Returns <code>nil</code> if <code>cond</code> was false before executing <code>body</code>.
        # @return (*)
        # @type cond expression
        # @type body expression
        # @genre logic
        # @example i := 0
        # @example While[i < 5, Print[i]; i := i + 1]
        # @example ?? 0
        # @example ?? 1
        # @example ?? 2
        # @example ?? 3
        # @example ?? 4
        # @example While[false, Print["Hello!"]]
        # @example ?? nothing is printed
        #>>
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
        #<<
        # Evaluates <code>body</code>, then stops only if <code>cond</code> evaluates as falsey.
        # @return (*)
        # @type cond expression
        # @type body expression
        # @genre logic
        # @example i := 0
        # @example DoWhile[i < 5, Print[i]; i := i + 1]
        # @example ?? 0
        # @example ?? 1
        # @example ?? 2
        # @example ?? 3
        # @example ?? 4
        # @example DoWhile[false, Print["Hello!"]]
        # @example ?? Hello!
        #>>
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
        #<<
        # For every value <code>el</code> in <code>ent</code>, evaluates <code>body</code>, setting the first abstract value to <code>el</code>, and the second to its index.
        # @genre logic
        # @type ent [(*)]
        # @return nil
        #>>
        "ForEach" => lambda { |inst, ent, body|
            arr = force_list(ent)

            arr.each_with_index { |x, i|
                inst.evaluate_node body, [x, i]
            }

            nil
        },


        ########################
        #### LIST FUNCTIONS ####
        ########################
        #<<
        # Generates the cumulative sums of <code>list</code>.
        # @type list [number]
        # @return [number]
        # @genre list
        #>>
        "Accumulate" => lambda { |inst, list|
            list.prefixes.map { |e| e.sum }
        },
        #<<
        # Flattens the matrices held in the matrix-like <code>list</code>.
        # @type list [[list]]
        # @return [list]
        # @genre list
        # @example m1 := [[1, 2], [3, 4]]
        # @example m2 := [[0, 0], [7, 7]]
        # @example Display[ArrayFlatten[ [[m1, m2, m1], [m2, m1, m2]] ]]
        # @example ??  1 2 0 0 1 2
        # @example ??  3 4 7 7 3 4
        # @example ??  0 0 1 2 0 0
        # @example ??  7 7 3 4 7 7
        #>>
        "ArrayFlatten" => lambda { |inst, list|
            inner = list.flatten(1).first { |e| Array === e }

            size = [*dim(inner)]
            if size.size < 2
                list
            else
                list.map { |row|
                    row.inject { |acc, mat|
                        unless Array === mat
                            mat = mat_from(*size, mat)
                        end
                        stitch acc, mat
                    }
                }.flatten(1)
            end
        },
        #<<
        # Returns the average of <code>list</code>, that is, the sum of the elements divided by the length.
        # @type list [number]
        # @return number
        # @genre list
        #>>
        "Average" => lambda { |inst, list|
            list.average
        },
        #<<
        # Splits <code>list</code> in half.
        # @type list [(*)]
        # @return [[(*)], [(*)]]
        # @genre list
        # @option bias what should be done with the center element, in the case of odd lists. <code>"none"</code>: drop it (default). <code>"left"</code>: append center to the left half. <code>"right"</code>: append center to the right half.
        # @example Print[Bisect[1:4]]
        # @example ?? [[1, 2], [3, 4]]
        # @example Print[Bisect[1:5]]
        # @example ?? [[1, 2], [4, 5]]
        # @example Print[Bisect[1:5, bias->$none]]
        # @example ?? [[1, 2], [4, 5]]
        # @example Print[Bisect[1:5, bias->$left]]
        # @example ?? [[1, 2, 3], [4, 5]]
        # @example Print[Bisect[1:5, bias->$right]]
        # @example ?? [[1, 2], [3, 4, 5]]
        #>>
        "Bisect" => lambda { |inst, list, **opts|
            opts[:bias] ||= "none"
            right_upper = list.size / 2.0
            left_lower = list.size / 2.0

            case opts[:bias]
                when "none"
                    right_upper = right_upper.floor
                    left_lower = left_lower.ceil

                when "left"
                    right_upper = right_upper.ceil
                    left_lower = left_lower.ceil

                when "right"
                    right_upper = right_upper.floor
                    left_lower = left_lower.floor

                else
                    STDERR.puts "Invalid option to `Bisect`: #{opts[:bias].inspect}"
            end

            [list[0...right_upper], list[left_lower..-1]]
        },
        #<<
        # "Bounces" a list. That is, returns the list concatenated with itself reversed, where the last element of the original array is not duplicated in the concatenation. Inspiration: <a href="https://github.com/DennisMitchell/jellylanguage/wiki/Atoms#%C5%92-atoms-other-monads">Jelly's <code>ŒB</code> monadic atom</code></a>.
        # @type list [(*)]
        # @return [(*)]
        # @reforms
        # @genre list
        # @example Print[Bounce[1:3]]
        # @example ?? [1, 2, 3, 2, 1]
        # @example Print[Bounce["Hello"]]
        # @example ?? HellolleH
        # @example Print[Bounce[942]]
        # @example ?? 94249
        #>>
        "Bounce" => lambda { |inst, list|
            listified = force_list(list)
            bounced = listified[0..-2] + listified.reverse
            reform_list bounced, list
        },
        #<<
        # Chops <code>list</code> into groups of length <code>size</code>.
        # @type list [(*)]
        # @return [[(*)]]
        # @option extra Boolean: keeps elements which don't add up to <code>size</code> if <code>true</code>. Default: <code>true</code>.
        # @genre list
        # @example Print[Chop[1:8, 3]]
        # @example ?? [[1, 2, 3], [4, 5, 6], [7, 8]]
        # @example Print[Chop[1:8, 3], extra->false]
        # @example ?? [[1, 2, 3], [4, 5, 6]]
        # @example Print[Chop["Hello, World!", 3]]
        # @example ?? ["Hel", "lo,", " Wo", "rld", "!"]
        # @example Print[Chop[1:12, [1, 2, 3]]]
        # @example ?? [[1], [2, 3], [4, 5, 6], [7], [8, 9], [10, 11, 12]]
        # @reforms elements
        #>>
        "Chop" => lambda { |inst, list, size, **opts|
            extra = opts.has_key?(:extra) ? opts[:extra] : true
            res = chop force_list(list), size, extra
            res.map { |e|
                reform_list e, list
            }
        },
        #<<
        # Chunks <code>list</code> into runs of consecutive values. Returns an array of members which look like <code>[el, els]</code>, where <code>el</code> is the principal run value, and <code>els</code> are the values in that run.
        # @type list [(*)]
        # @optional f
        # @type f fn
        # @param f When specified, maps each value <code>el</code> in <code>list</code> over <code>f</code>, then uses <code>f[el]</code> as the principal run value.
        # @return [[(*), [(*)]]]
        # @genre list
        # @example Print[Chunk[ [1, 2, 3, 2, 2, 3] ]]
        # @example ?? [[1, [1]], [2, [2]], [3, [3]], [2, [2, 2]], [3, [3]]]
        # @example Display[Chunk[ [1, -1, 1, 1, 2, 3, -3, 5, -2 ], Square ]]
        # @example ??   1 [1, -1, 1, 1]
        # @example ??   4           [2]
        # @example ??   9       [3, -3]
        # @example ??  25           [5]
        # @example ??   4          [-2]
        #>>
        "Chunk" => lambda { |inst, list, f=nil|
            list = force_list list
            if f.nil?
                list.chunk { |e| e }.to_a
            else
                list.chunk { |e| f[inst, e] }.to_a
            end
        },
        #<<
        # Returns all elements of <code>parent</code> not included in any of the lists in <code>args</code>.
        # @type parent [(*)]
        # @type args [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Complement" => lambda { |inst, parent, *args|
            any = args.map { |e|
                force_list e
            }.flatten(1)
            force_list(parent).reject { |e| any.include? e }
        },
        #<<
        # Returns the concatentation of each list in <code>args</code>.
        # @type args [(*)]
        # @return [(*)]
        # @genre list
        # @example Print[Concat[ 1:5, 2:3, [7], [[8, 9]] ]]
        # @example ?? [1, 2, 3, 4, 5, 2, 3, 7, [8, 9]]
        #>>
        "Concat" => lambda { |inst, *args|
            args.flatten(1)
        },
        #<<
        # Counts the number of members satisfying <code>f</code> in <code>list</code>; or, if <code>f</code> is not a function, the number of times <code>f</code> appears in <code>list</code>.
        # @type list [(*)]
        # @type f fn|(*)
        # @return number
        # @genre list
        #>>
        "Count" => lambda { |inst, list, f|
            if f.is_a?(Proc) || f.is_a?(AtLambda)
                list.count { |e| f[inst, e] }
            else
                list.count f
            end
        },
        #<<
        # Returns <code>true</code> if every element <code>a</code> which is followed by an element <code>b</code> is strictly greater than <code>b</code>.
        # @type list [(*)]
        # @return bool
        # @genre list/logic
        # @example Print[Decreasing[ -(1:5) ]]
        # @example ?? true
        # @example Print[Decreasing[ [2, 1, 0] ]]
        # @example ?? true
        # @example Print[Decreasing[ [] ]]
        # @example ?? true
        # @example Print[Decreasing[ [2, 2, 1, 0] ]]
        # @example ?? false
        # @example Print[Decreasing[ 1:5 ]]
        # @example ?? false
        #>>
        "Decreasing" => lambda { |inst, list|
            list.delta.all?(&:negative?) rescue false
        },
        #<<
        # Returns the differences between each member of <code>list</code>.
        # @type list [number]
        # @return [number]
        # @genre list
        # @example Print[Delta[ 1:4 ]]
        # @example ?? [1, 1, 1]
        # @example Print[Delta[ [] ]]
        # @example ?? []
        # @example Print[Delta[ Square[0:4] ]]
        # @example ?? [1, 3, 5, 7]
        #>>
        "Delta" => lambda { |inst, list|
            list.delta
        },
        #<<
        # Returns all elements exclusive to <code>a</code> or <code>b</code>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Difference" => lambda { |inst, a, b|
            halve1 = @@functions["Complement"][inst, a, b]
            halve2 = @@functions["Complement"][inst, b, a]
            halve1 + halve2
        },
        #<<
        # Returns the first element of <code>list</code>.
        # @type list [(*)]
        # @return (*)
        # @genre list
        # @example Print[First[ [3, 9, 2, 0] ]]
        # @example ?? 3
        # @example Print[First[ "Hello!" ]]
        # @example ?? H
        #>>
        "First" => lambda { |inst, list|
            list[0]
        },
        #<<
        # Returns the first element <code>e</code> in <code>list</code> which satisfies <code>f</code>. Returns <code>nil</code> otherwise.
        # @type f fn|(*)
        # @type list [(*)]
        # @return (*)
        # @paramtype fn f applied to each successive member of <code>list</code> until a truthy value is obtained.
        # @paramtype (*) f returns the first element in <code>list</code> equal to <code>f</code>.
        # @genre list
        #>>
        "Find" => lambda { |inst, f, list|
            if AtState.func_like? f
                list.find { |e| f[inst, e] }
            else
                list.find { |e| e == f }
            end
        },
        #<<
        # Flattens <code>list</code>.
        # @type list [(*)]
        # @optional n
        # @type n number
        # @return [(*)]
        # @genre list
        # @param n when specified, flattens <code>list</code> to 1 level <code>n</code> times.
        #>>
        "Flat" => lambda { |inst, list, n=nil|
            list.flatten(n)
        },
        #<<
        # Gets all members at indices <code>inds</code> from list.
        # @type list [(*)]
        # @type ind number|ConfigureValue
        # @return [(*)]|string
        # @paramtype ConfigureValue ind Returns the members between the key and the value. If the value is negative, starts at the respective index from the right. Returns a string if given a string, instead of an array of characters.
        # @genre list
        # @example Print[Get[1:5, 2]]
        # @example ?? 3
        # @example Print[Get["Hello, World!", 0:4]]
        # @example ?? ["H", "e", "l", "l", "o"]
        # @example Print[Get["Hello, World!", 4 -> -4]]
        #>>
        "Get" => vectorize_dyad(RIGHT) { |inst, list, ind|
            if ConfigureValue === ind
                list[ind.key..ind.value]
            else
                list[ind]
            end
        },
        #<<
        # Returns <code>true</code> if <code>list</code> contains <code>member</code>, <code>false</code> otherwise.
        # @type list [(*)]|string
        # @type member (*)
        # @return bool
        # @genre list
        # @paramtype string list returns if <code>member</code> is contained within <code>list</code>.
        #>>
        "Has" => lambda { |inst, list, member|
            if String === list
                !!list.index(member)
            else
                list.include? member
            end
        },
        #<<
        # Returns <code>list[i0][i1]...[iN]</code> for each <code>i</code> in <code>inds</code>.
        # @type list [(*)]
        # @type inds [(*)]|(*)
        # @return (*)
        # @genre list
        # @example id := Identity[3]
        # @example Display[id]
        # @example ??  1 0 0
        # @example ??  0 1 0
        # @example ??  0 0 1
        # @example Print[FlatGet[id, 0]]
        # @example ?? [1, 0, 0]
        # @example Print[FlatGet[id, [0]]]
        # @example ?? [1, 0, 0]
        # @example Print[FlatGet[id, [0, 0]]]
        # @example ?? 1
        #>>
        "FlatGet" => lambda { |inst, list, inds|
            [*inds].each { |i|
                list = list[i]
            }
            list
        },
        #<<
        # Returns <code>true</code> if each member <code>el</code> is strictly greater than the previous element, otherwise <code>false</code>.
        # @type list [(*)]
        # @return bool
        # @genre list/logic
        #>>
        "Increasing" => lambda { |inst, list|
            list.delta.all?(&:positive?) rescue false
        },
        #<<
        # Returns all indices at which <code>ind</code> occurs in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return [number]
        # @genre list
        # @example Print[Indices[ [1, 1, 2, 3, 1, 3, 1], [1, 3] ]]
        # @example ?? [[0, 1, 4, 6], [3, 5]]
        #>>
        "Indices" => vectorize_dyad(RIGHT) { |inst, list, ind|
            force_list(list).indices ind
        },
        #<<
        # Returns the index of the first occurrence of <code>ind</code> in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return number
        # @genre list
        #>>
        "Index" => vectorize_dyad(RIGHT) { |inst, list, ind|
            force_list(list).index ind
        },
        #<<
        # Returns all indices at which <code>ind</code> occurs in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return [number]
        # @genre list
        # @example Print[IndicesFlat[ [1, 1, 2, 3, 1, 3, 1], [1, 3] ]]
        # @example ?? []
        #>>
        "IndicesFlat" => lambda { |inst, list, ind|
            force_list(list).indices ind
        },
        #<<
        # Returns the index of the first occurrence of <code>ind</code> in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return number
        # @genre list
        #>>
        "IndexFlat" => lambda { |inst, list, ind|
            force_list(list).index ind
        },
        #<<
        # Returns the intersection of the arguments.
        # @type lists [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Intersection" => lambda { |inst, *lists|
            lists.inject(&:&)
        },
        #<<
        # Returns a list where <code>joiner</code> is placed in between each element of <code>list</code>.
        # @type list [(*)]
        # @type joiner (*)
        # @return [(*)]
        # @genre list
        # @example Print[Intersperse[1:3, 0]]
        # @example ?? [1, 0, 2, 0, 3]
        #>>
        "Intersperse" => lambda { |inst, list, joiner|
            res = []
            force_list(list).each_with_index { |e, i|
                res << e
                res << joiner if i != list.size - 1
            }
            res
        },
        #<<
        # Returns an range from <code>0</code> (inclusive) to <code>min</code> or the length of <code>min</code> as applicable (exclusive).
        # @type min [(*)]|number
        # @return [(*)]
        # @genre list
        # @example Print[Iota[5]]
        # @example ?? [0, 1, 2, 3, 4]
        # @example Print[Iota["Hi!"]]
        # @example ?? [0, 1, 2]
        #>>
        "Iota" => lambda { |inst, min|
            ((0...min) rescue (0...min.size)).to_a
        },
        #<<
        # Returns the larger of <code>a</code> and <code>b</code>. Similar to <a href="#Max"><code>Max</code></a>, except <code>Larger</code> vectorizes.
        # @type a [(*)]
        # @type b [(*)]
        # @return (*)
        # @genre list
        # @example Print[Larger[1, 5]]
        # @example ?? 5
        # @example Print[Larger[[1, 2, 3], [3, 2, 1]]]
        # @example ?? [3, 2, 3]
        #>>
        "Larger" => vectorize_dyad { |inst, a, b|
            [a, b].max
        },
        #<<
        # Returns the last member of <code>list</code>.
        # @type list [(*)]|string
        # @return (*)
        # @genre list
        # @example Print[Last["hiya"]]
        # @example ?? a
        # @example Print[Last[1:5]]
        # @example ?? 5
        #>>
        "Last" => lambda { |inst, list|
            list[-1]
        },
        #<<
        # Forces <code>ent</code> into a list.
        # @type ent (*)
        # @return [(*)]
        # @genre list
        # @paramtype number ent Returns the digits of <code>ent</code>.
        # @paramtype string ent Returns the characters of <code>ent</code>.
        # @paramtype hash ent Returns an array of key-value pairs in <code>ent</code>.
        #>>
        "List" => lambda { |inst, ent|
            force_list ent
        },
        #<<
        # Returns the largest element contained in any atom of <code>args</code>.
        # @type args (*)
        # @return (*)
        # @genre list
        #>>
        "Max" => lambda { |inst, *args|
            args.flatten.max
        },
        #<<
        # Returns the median of <code>list</code>.
        # @type list [number]
        # @return number
        # @genre list
        # @example Print[Median[[1, 2, 3]]]
        # @example ?? 2
        # @example Print[Median[[1, 2, 3, 4]]]
        # @example ?? 2.5
        #>>
        "Median" => lambda { |inst, list|
            force_number list.median
        },
        #<<
        # Returns the smallest element contained in any atom of <code>args</code>.
        # @type args (*)
        # @return (*)
        # @genre list
        #>>
        "Min" => lambda { |inst, *args|
            args.flatten.min
        },
        #<<
        # Returns the outermost <code>n</code> elements from the left and right sides of <code>list</code>.
        # @type list [(*)]
        # @type n number
        # @optional n
        # @return [(*)]
        # @param n Default: <code>1</code>
        # @genre list
        # @example Print[Outers[1:5]]
        # @example ?? [1, 5]
        # @example Print[Outers[1:5, 2]]
        # @example ?? [1, 2, 4, 5]
        #>>
        "Outers" => vectorize_dyad(RIGHT) { |inst, list, n=1|
            list[0...n] + list[-n..-1]
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
        #<<
        # Returns an array representing the run-length encoded version of <code>list</code>.
        # @type list (*)
        # @return [[(*),  number]]
        # @genre list
        #>>
        "RLE" => lambda { |inst, list|
            force_list(list)
                .chunk { |e| e }
                .map { |k, v| [k, v.size] }
        },
        "Rotate" => vectorize_dyad(RIGHT) { |inst, list, amount=1|
            if list.is_a? String
                @@functions["Rotate"][inst, force_list(list), amount].join
            else
                rotate list, amount
            end
        },
        "Rotations" => lambda { |inst, list|
            list = force_list list
            (0...list.size).map { |rot|
                rotate list, rot
            }
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
            if Numeric === list
                list.abs.to_s.size
            elsif class_has? list, "size"
                list["$size"][inst]
            else
                list.size
            end
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
            if String === list
                return @@functions["Sort"][inst, list.chars].join
            end
            if func.nil?
                list.sort
            else
                list.sort { |x, y|
                    func[inst, x, y]
                }
            end
        },
        "Shuffle" => lambda { |inst, list|
            shuffled = force_list(list).shuffle
            reform_list shuffled, list
        },
        "SortBy" => lambda { |inst, list, func|
            if String === list
                return @@functions["SortBy"][inst, list.chars].join
            end
            list.sort_by { |e|
                res = func[inst, e]
                if res == !!res
                    res = force_number res
                end
                res
            }
        },
        "SplitAt" => vectorize_dyad(RIGHT) { |inst, str, inds=[1]|
            split_at force_list(str), inds
        },
        "StdDev" => lambda { |inst, list|
            list.stddev
        },
        "Stitch" => lambda { |inst, left, right|
            stitch left, right
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
        #<<
        # Returns an array of lists representing all pairs <code>(x, y)</code> such that <code>a <= y <= b</code> and <code>a <= x < y</code>.
        # @genre list
        # @type a number
        # @type b number
        # @return [[number, number]]
        #>>
        "TriangleRange" => lambda { |inst, a, b|
            pairs = []

            (a..b).each { |y|
                (a...y).each { |x|
                    pairs << [x, y]
                }
            }

            pairs
        },
        #<<
        # Returns an array of lists representing all pairs <code>(x, y)</code> satisfying <code>cond</code> such that <code>a <= y <= b</code> and <code>a <= x < y</code>.
        # @genre list
        # @type a number
        # @type b number
        # @type cond fn
        # @return [[number, number]]
        #>>
        "TriangleSelect" => lambda { |inst, cond, a, b|
            pairs = []

            (a..b).each { |y|
                (a...y).each { |x|
                    pairs << [x, y] if cond[inst, x, y]
                }
            }

            pairs
        },
        #<<
        # Returns an array representing the union of all lists in <code>lists</code>.
        # @type lists [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Union" => lambda { |inst, *lists|
            lists.inject(&:|)
        },
        #<<
        # Returns the non-duplicated elements in <code>list</code>.
        # @type list [(*)]
        # @type func fn
        # @optional func
        # @return [(*)]
        # @genre list
        # @reforms
        #>>
        "Unique" => lambda { |inst, list, func=nil|
            fl = force_list list
            unique = if func.nil?
                fl.uniq
            else
                fl.uniq { |e| arg[func, e] }
            end
            reform_list unique, list
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
        "Identity" => vectorize_monad { |inst, size|
            Matrix.identity(size).to_a
        },
        "LowerTriangle" => lambda { |inst, mat, strict=false|
            lower_triangle mat, AtState.truthy?(strict)
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
        "Translate" => lambda { |inst, str, source, repl|
            str.tr source, repl
        },
        "UpperTriangle" => lambda { |inst, mat, strict=false|
            upper_triangle mat, AtState.truthy?(strict)
        },

        ##------------------------##
        ## Combinatoric Functions ##
        ##------------------------##
        "Combinations" => lambda { |inst, list, count=nil|
            inner = force_list list
            if count.nil?
                count = (0..inner.size).to_a
            end
            if count.is_a? Array
                count.map { |e|
                    reform_list inner.combination(e).to_a, list
                }.flatten(1)
            else
                reform_list inner.combination(count).to_a, list
            end
        },
        "Permutations" => vectorize_dyad(RIGHT) { |inst, list, count=list.size|
            if list.is_a? String
                force_list(list).permutation(count).map(&:join).to_a
            else
                reform_list force_list(list).permutation(count).to_a, list
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
            list.max_by { |e| f[inst, e] }
        },
        "MinBy" => lambda { |inst, f, list|
            list.min_by { |e| f[inst, e] }
        },
        "Outer" => lambda { |inst, f, *lists|
            if lists.empty?
                lambda { |inst, *lists|
                    combine(*lists) { |*e|
                        f[inst, *e]
                    }
                }
            else
                combine(*lists) { |*e|
                    f[inst, *e]
                }
            end
        },
        "Select" => lambda { |inst, f, list=nil|
            if AtState.func_like?(list) || list.nil?
                g = list || lambda { |inst, a| a }
                lambda { |inst, *args|
                    @@functions["Select"][inst, f, g[inst, *args]]
                }
            else
                list.select { |e|
                    AtState.truthy? f[inst, e]
                }
            end
        },
        "Table" => lambda { |inst, f, as, bs=as|
            as.map { |a|
                bs.map { |b|
                    f[inst, a, b]
                }
            }
        },
        "Reject" => lambda { |inst, f, list|
            if AtState.func_like? list
                g = list
                lambda { |inst, *args|
                    @@functions["Reject"][inst, f, g[inst, *args]]
                }
            else
                list.reject { |e| AtState.truthy? f[inst, e] }
            end
        },


        ##########################
        #### STRING FUNCTIONS ####
        ##########################
        #<<
        # Returns the characters of <code>str</code>.
        # @return [string]
        # @type str string
        # @genre string
        #>>
        "Chars" => vectorize_monad { |inst, str|
            str.chars
        },
        #<<
        # Selects the first key (or value) in <code>opts</code> which start with <code>val</code>.
        # @genre string
        # @return string
        # @type opts [string]|{string->(*)}
        # @type val string
        # @example options := <~
        # @example   stop -> 0,
        # @example   continue -> 1,
        # @example   redo -> 2
        # @example ~>
        # @example choice := "s"
        # @example Print[FindHead[options, choice]]
        # @example ?? stop
        #>>
        "FindHead" => lambda { |inst, opts, val|
            if Hash === opts
                opts = opts.keys
            end

            opts.sort.select { |e|
                e.start_with? val
            }.first
        },
        #<<
        # Formats a string. Currently, uses Ruby's <a href="https://ruby-doc.org/core-2.0.0/String.html#method-i-25"><code>%</code></a> method on strings.
        # @type str string
        # @type args (*)
        # @return string
        # @genre string
        # @example Print[Format["%s is a %s", "Java", "Joke"]]
        # @example ?? Java is a Joke
        #>>
        "Format" => lambda { |inst, str, *args|
            str % args
        },
        #<<
        # Returns the characters of each line of <code>str</code>.
        # @type str string
        # @type inner string
        # @return string
        # @optional inner
        # @param inner Character to pad the ends of each line with. Default: <code>" "</code>.
        # @genre string
        # @example Map[Print, Grid["Hello,\nWorld\nof mine!"]]
        # @example ?? ["H", "e", "l", "l", "o", ",", " ", " "]
        # @example ?? ["W", "o", "r", "l", "d", " ", " ", " "]
        # @example ?? ["o", "f", " ", "m", "i", "n", "e", "!"]
        #>>
        "Grid" => lambda { |inst, str, inner=" "|
            str = str.lines rescue str
            grid = str.map(&:chomp).map(&:chars)
            pad_grid grid, inner
        },
        "Join" => vectorize_dyad(RIGHT) { |inst, list, joiner=""|
            list.join joiner
        },
        "Match" => vectorize_dyad { |inst, source, match|
            match = make_regex match
            source.match(match).to_a
        },
        "MatchAll" => vectorize_dyad { |inst, source, match|
            match = make_regex match
            source.scan(match)
        },
        "Ord" => vectorize_monad { |inst, ent|
            ent.ord
        },
        "Ords" => vectorize_monad { |inst, ent|
            if ent.is_a? String
                ent.chars.map(&:ord)
            else
                ent.ord
            end
        },
        #<<
        # Pads <code>ent</code> to be no less than length <code>amt</code>, padding with fill elements <code>fill</code> on the left.
        # @type ent (*)
        # @type amt number
        # @type fill (*)
        # @return (*)
        # @genre string
        # @optional fill
        # @paramtype string ent appends space characters to the left of <code>ent</code>.
        # @paramtype [(*)] ent appends <code>0</code>s to the left of <code>ent</code>.
        # @example Display[PadLeft["Charles", 20]]
        # @example ?? "             Charles"
        # @example Print[PadLeft[1:3, 6]]
        # @example ?? [0, 0, 0, 1, 2, 3]
        #>>
        "PadLeft" => lambda { |inst, ent, amt, fill=NOT_PROVIDED|
            case ent
                when String
                    fill = fill == NOT_PROVIDED ? " " : fill
                    ent.rjust(amt, fill)

                else
                    fill = fill == NOT_PROVIDED ? 0 : fill
                    ent = force_list ent
                    until ent.size >= amt
                        ent = [fill, *ent]
                    end
                    ent
            end
        },
        #<<
        # Pads <code>ent</code> to be no less than length <code>amt</code>, padding with fill elements <code>fill</code> on the right.
        # @type ent (*)
        # @type amt number
        # @type fill (*)
        # @return (*)
        # @genre string
        # @optional fill
        # @paramtype string ent appends space characters to the right of <code>ent</code>.
        # @paramtype [(*)] ent appends <code>0</code>s to the right of <code>ent</code>.
        # @example Display[PadRight["Charles", 20]]
        # @example ?? "Charles             "
        # @example Print[PadRight[1:3, 6]]
        # @example ?? [1, 2, 3, 0, 0, 0]
        #>>
        "PadRight" => lambda { |inst, ent, amt, fill=NOT_PROVIDED|
            case ent
                when String
                    fill = fill == NOT_PROVIDED ? " " : fill
                    ent.ljust(amt, fill)

                else
                    fill = fill == NOT_PROVIDED ? 0 : fill
                    ent = force_list ent
                    until ent.size >= amt
                        ent = [*ent, fill]
                    end
                    ent
            end
        },
        "Split" => vectorize_dyad { |inst, str, sep=/\s+/|
            str.split sep
        },
        "Lines" => vectorize_monad { |inst, str|
            str.split(/\r?\n/)
        },
        "Replace" => lambda { |inst, str, search, replace=""|
            replace str, search, replace
        },
        "ReplaceMultiple" => lambda { |inst, str, *args|
            str = str.dup
            args.map(&:to_a).each { |k, v|
                str.gsub!(Regexp.new(k), v)
            }
            str
        },
        "ReplaceF" => lambda { |inst, str, search, func|
            str.gsub(search) { |e|
                func[inst, e]
            }
        },
        "Repr" => lambda { |inst, ent|
            ent.inspect
        },
        "Rot" => vectorize_dyad(RIGHT) { |inst, str, amount=13|
            rotN(str, amount)
        },
        "String" => lambda { |inst, ent|
            inst.cast_string ent
        },
        "SwapCase" => vectorize_monad { |inst, str|
            str.chars.map { |e|
                if e != e.upcase
                    e.upcase
                elsif e != e.downcase
                    e.downcase
                else
                    e
                end
            }.join
        },
        "Upcase" => vectorize_monad { |inst, str|
            str.upcase
        },
        "UnGrid" => lambda { |inst, str|
            str.map(&:join).join "\n"
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
        },

        ##################
        #### UNSORTED ####
        ##################
        "Error" => lambda { |inst, name, msg="An error has occured."|
            AtError.new name, msg
        },
        "HTMLEscape" => lambda { |inst, str|
            str.gsub("&", "&amp;").gsub("<", "&lt;").gsub(">", "&gt;").gsub("\"", "&quot;")
        },
        "DoSafe" => lambda { |inst, body|
            begin
                inst.evaluate_node body
                nil
            rescue Exception => e
                e
            end
        },
        "TryCatch" => lambda { |inst, body, catch|
            res = inst.evaluate_node body
            if AtError === res
                inst.evaluate_node catch, [res], check_error: false
                nil
            else
                res
            end
        },
        "Safely" => lambda { |inst, func, catch=nil, **opts|
            rec = lambda { |inst, *args|
                begin
                    func[inst, *args]
                rescue Exception => e
                    if catch.nil?
                        e
                    elsif AtState.func_like? catch
                        catch[inst, e]
                    else
                        catch
                    end
                    if opts[:redo]
                        rec[inst, *args]
                    end
                end
            }
        },
    }

    # operators with two arguments
    @@operators = {
        #<<
        # Obtains the member in <code>obj</code> with <code>prop</code> as a key.
        # </p><p><em>Note:</em> this feature is not completely finished, and may have unintended behaviours, such as <code>[1, 2, 3].size</code>.
        # @type prop raw
        # @type obj (*)
        # @return (*)
        # @genre operator
        # @example ages := <~
        # @example   john -> 32,
        # @example   bob -> 14,
        # @example   dude -> 92
        # @example ~>
        # @example Print[ages.john]
        # @example ?? 32
        # @example
        # @example Person := Class! {
        # @example     name .= _1
        # @example     age .= _2
        # @example }
        # @example john := New[Person, "John Smith", 43]
        # @example Print[john.name, john.age, joiner->", "]
        # @examlpe ?? John Smith, 43
        #>>
        "." => lambda { |inst, obj, prop|
            if AtClassInstance === obj || Hash === obj
                obj[prop.raw]
            elsif obj.respond_to? prop.raw.to_sym
                obj.send prop.raw.to_sym
            else
                raise 'idk'
            end
        },
        ":=" => lambda { |inst, var, val|
            if Node === var
                #todo: pattern matching++

                args = var.children.map { |e|
                    if Node === e
                        e.children[0].raw
                        STDERR.puts "Note: undefined behaviour raised: unimplemented argument matching"
                    else
                        e.raw
                    end
                }
                if var.head.raw == "'"
                    val = inst.evaluate_node val
                    args.each_with_index { |arg, i|
                        inst.define arg, val[i]
                    }
                else
                    res = AtLambda.new [val], args
                    if var.head.type == :op_quote
                        inst.set_op_quote var.head, res
                    else
                        inst.define var.head.raw, res
                    end
                end
            else
                res = inst.evaluate_node(val)
                if var.type == :op_quote
                    unless AtState.func_like? res
                        res = lambda { |*discard| res }
                    end
                    inst.set_op_quote var, res
                else
                    name = var.raw
                end
                inst.define name, res
            end
        },
        "≔" => held(true, true) { |inst, var, val|
            @@operators[":="][inst, var, val]
        },
        ".=" => lambda { |inst, var, val|
            #todo: expand like :=
            #todo: abstract `.=` and `:=` logic
            if Node === var
                args = var.children.map(&:raw)
                if var.head.raw == "'"
                    args.each_with_index { |arg, i|
                        inst.define_local arg, val[i]
                    }
                else
                    res = AtLambda.new [val], args
                    inst.define_local var.head.raw, res
                end
            else
                name = var.raw
                inst.define_local name, inst.evaluate_node(val)
            end
        },
        #<<
        # Multiplication.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "*" => vectorize_dyad { |inst, a, b|
            if class_has? a, "mul"
                a["$mul"][inst, b]
            elsif class_has? b, "rmul"
                b["$rmul"][inst, a]
            elsif String === b
                b * a
            else
                a * b
            end
        },
        #<<
        # Divides <code>a</code> by <code>b</code>. If <code>a / b</code> represents an integer, the argument becomes an integer. E.g., <code>4 / 2</code> is <code>2</code>, not <code>2.0</code>.
        # @type a number
        # @type b number
        # @return number
        # @genre operator
        #>>
        "/" => vectorize_dyad { |inst, a, b|
            # p 'div',a,b
            if class_has? a, "div"
                a["$div"][inst, b]
            elsif class_has? b, "rdiv"
                b["$rdiv"][inst, a]
            elsif AtState.func_like? a
                AtFunction.from(arity: b) { |inst, *args|
                    a[inst, *args]
                }
            else
                simplify_number a * 1.0 / b
            end
        },
        #<<
        # Creates a fraction <code>a / b</code>. See also: <code><a href="#Rational">Rational</a></code>
        # @type a number
        # @type b number
        # @return rational
        # @genre operator
        #>>
        "//" => vectorize_dyad { |inst, a, b|
            Rational(a, b)
        },
        #<<
        # Creates a fraction <code>a / b</code>. See also: <code><a href="#Rational">Rational</a></code> and <code><a href="#//">//</a></code>.
        # @type a number
        # @type b number
        # @return rational
        # @genre operator
        #>>
        "⁄" => vectorize_dyad { |inst, a, b|
            Rational(a, b)
        },
        #<<
        # Subtraction.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "-" => vectorize_dyad { |inst, a, b|
            if class_has? a, "sub"
                a["$sub"][inst, b]
            elsif class_has? b, "rsub"
                b["$rsub"][inst, a]
            else
                a - b
            end
        },
        #<<
        # Addition.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "+" => vectorize_dyad { |inst, a, b|
            if class_has? a, "add"
                a["$add"][inst, b]
            elsif class_has? b, "radd"
                b["$radd"][inst, a]
            else
                a + b
            end
        },
        #<<
        # Returns <code><a href="#PlusMinus">PlusMinus</a>[a, b]</a></code>.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "±" => vectorize_dyad { |inst, a, b|
            @@functions["PlusMinus"][inst, a, b]
        },
        #<<
        # Exponentiation.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "^" => vectorize_dyad { |inst, a, b|
            if class_has? a, "pow"
                a["$pow"][inst, b]
            elsif class_has? b, "rpow"
                b["$rpow"][inst, a]
            else
                a + b
            end
        },
        #<<
        # Modulo.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "%" => vectorize_dyad { |inst, a, b|
            if class_has? a, "mod"
                a["$mod"][inst, b]
            elsif class_has? b, "rmod"
                b["$rmod"][inst, a]
            else
                a % b
            end
        },
        #<<
        # Returns <code>true</code> if <code>a</code> divides evenly into <code>b</code>.
        # @type a (*)
        # @type b fn|(*)
        # @return (*)
        # @genre operator/logic
        # @paramtype fn b When <code>b</code> a function, calls <code>b[a]</code>.
        #>>
        "|" => vectorize_dyad { |inst, a, b|
            if AtState.func_like? b
                b[inst, a]
            else
                @@operators["%"][inst, b, a] == 0
            end
        },
        #<<
        # Calls <code>y</code> with single parameter <code>x</code>.
        # @type x (*)
        # @type y fn
        # @return (*)
        # @genre operator
        #>>
        "|>" => lambda { |inst, x, y|
            y[inst, x]
        },
        #<<
        # Calls <code>y</code> with single parameter <code>x</code>. See also: <a href="#|>"><code>|&gt;</code></a>.
        # @type x (*)
        # @type y fn
        # @return (*)
        # @genre operator
        #>>
        "▷" => lambda { |inst, x, y|
            @@operators["|>"][inst, x, y]
        },
        #<<
        # Calls <code>x</code> with single parameter <code>y</code>.
        # @type x fn
        # @type y (*)
        # @return (*)
        # @genre operator
        #>>
        "<|" => lambda { |inst, x, y|
            x[inst, y]
        },
        #<<
        # Calls <code>x</code> with single parameter <code>y</code>. See also: <a href="#<|"><code>&lt;|</code></a>.
        # @type x fn
        # @type y (*)
        # @return (*)
        # @genre operator
        #>>
        "◁" => lambda { |inst, x, y|
            @@operators["<|"][inst, x, y]
        },
        #<<
        # Returns <code>true</code> if <code>x</code> equals <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "=" => vectorize_dyad { |inst, x, y|
            x == y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> does <em>not</em> equal <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "/=" => vectorize_dyad { |inst, x, y|
            x != y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> equals <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "==" => lambda { |inst, x, y|
            x == y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> does <em>not</em> equal <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "=/=" => lambda { |inst, x, y|
            x != y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is greater than <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        ">" => vectorize_dyad { |inst, x, y|
            x > y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is less than <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "<" => vectorize_dyad { |inst, x, y|
            x < y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is greater than or equal to <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        ">=" => vectorize_dyad { |inst, x, y|
            x >= y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is greater than or equal to <code>y</code>, <code>false</code> otherwise. See also: <a href="#>="><code>&gt;=</code></a>.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "≥" => vectorize_dyad { |inst, x, y|
            x >= y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is less than or equal <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "<=" => vectorize_dyad { |inst, x, y|
            x <= y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is less than or equal <code>y</code>, <code>false</code> otherwise. See also: <a href="#<="><code>&lt;=</code></a>.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "≤" => vectorize_dyad { |inst, x, y|
            x <= y
        },
        #<<
        # Returns a range from <code>x</code> up to <code>y</code>. If <code>x</code> and <code>y</code> are both functions, returns a function <code>f[...args]</code> which calls <code>x[...Map[y, args]]</code>.
        # @type x number|fn
        # @type y number|fn
        # @return [number]|fn
        # @genre operator
        # @example Print[3:6]
        # @example ?? [3, 4, 5, 6]
        # @example add_rev := Add:Reverse
        # @example Print[add_rev[23, 45]]
        # @example ?? 86
        #>>
        ":" => vectorize_dyad { |inst, x, y|
            if AtState.func_like?(x) && AtState.func_like?(y)
                lambda { |inst, *args|
                    x[inst, *args.map { |e|
                        y[inst, e]
                    }]
                }
            else
                (x..y).to_a
            end
        },
        #<<
        # Returns a range from <code>x</code> to <code>y</code>, inclusive.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        #>>
        ".." => vectorize_dyad { |inst, x, y|
            (x..y).to_a
        },
        #<<
        # Returns a range from <code>x</code> to <code>y</code>, inclusive. See also: <a href="#.."><code>..</code></a>.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        #>>
        "‥" => vectorize_dyad { |inst, x, y|
            @@operators[".."][inst, x, y]
        },
        #<<
        # Returns a range from <code>x</code> to <code>y</code>, excluding <code>y</code>.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        #>>
        "..." => vectorize_dyad { |inst, x, y|
            (x...y).to_a
        },
        #<<
        # Returns a range from <code>x</code> to <code>y</code>, excluding <code>y</code>. See also: <a href="#..."><code>...</code></a>.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        #>>
        "…" => vectorize_dyad { |inst, x, y|
            @@operators["..."][inst, x, y]
        },
        #<<
        # Returns <code>true</code> if <code>y</code> contains <code>x</code>, otherwise <code>false</code>. See also: <code><a href="#Has">Has</a></code>.
        # @return bool
        # @type x (*)
        # @type y [(*)]
        # @genre operator/logic
        # @example Print[3 in 1:5]
        # @example ?? true
        # @example Print[30 in 1:5]
        # @example ?? false
        #>>
        "in" => lambda { |inst, x, y|
            @@functions["Has"][inst, y, x]
        },
        #<<
        # Returns <code>a</code> if <code>a</code> is truthy, <code>b</code> otherwise. Short-circuits.
        # @type a expr
        # @type b expr
        # @return (*)
        # @genre operator/logic
        #>>
        "or" => lambda { |inst, a, b|
            lres = inst.evaluate_node_safe a
            if AtState.truthy? lres
                lres
            else
                inst.evaluate_node_safe b
            end
        },
        #<<
        # Returns <code>a</code> if <code>a</code> is truthy, <code>b</code> otherwise. Short-circuits. See also: <a href="#or"><code>or</code></a>.
        # @type a expr
        # @type b expr
        # @return (*)
        # @genre operator/logic
        #>>
        "∨" => held(true, true) { |inst, a, b|
            @@operators["or"][inst, a, b]
        },
        #<<
        # Returns <code>true</code> if exactly one of <code>a</code> and <code>b</code> is truthy, <code>false</code> otherwise.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "xor" => lambda { |inst, a, b|
            # no short circuiting, since both values must be compared
            AtState.truthy?(a) ^ AtState.truthy?(b)
        },
        #<<
        # Returns <code>true</code> if exactly one of <code>a</code> and <code>b</code> is truthy, <code>false</code> otherwise. See also: <a href="#xor"><code>xor</code></a>.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "⊻" => lambda { |inst, a, b|
            @@operators["xor"][inst, a, b]
        },
        #<<
        # Returns <code>true</code> if one of <code>a</code> and <code>b</code> are falsey, <code>false</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "nand" => lambda { |inst, a, b|
            if AtState.falsey? inst.evaluate_node_safe a
                true
            elsif AtState.falsey? inst.evaluate_node_safe b
                true
            else
                false
            end
        },
        #<<
        # Returns <code>true</code> if one of <code>a</code> and <code>b</code> are falsey, <code>false</code> otherwise. Short-circuits. See also: <a href="#nand"><code>nand</code></a>.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "⊼" => held(true, true) { |inst, a, b|
            @@operators["nand"][inst, a, b]
        },
        #<<
        # Returns <code>true</code> if both of <code>a</code> and <code>b</code> are truthy, <code>false</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "and" => lambda { |inst, a, b|
            lres = inst.evaluate_node_safe a
            if AtState.falsey? lres
                lres
            else
                inst.evaluate_node_safe b
            end
        },
        #<<
        # Returns <code>true</code> if both of <code>a</code> and <code>b</code> are truthy, <code>false</code> otherwise. Short-circuits. See also: <a href="#and"><code>and</code></a>.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "∧" => held(true, true) { |inst, a, b|
            @@operators["and"][inst, a, b]
        },
        #<<
        # Returns <code>a</code> if <code>a</code> is truthy, <code>b</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "else" => lambda { |inst, a, b|
            lres = inst.evaluate_node_safe a
            if AtState.truthy? lres
                lres
            else
                inst.evaluate_node_safe b
            end
        },
        #<<
        # Returns <code>true</code> if both of <code>a</code> and <code>b</code> are falsey, <code>false</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "nor" => lambda { |inst, a, b|
            if AtState.truthy? inst.evaluate_node_safe a
                false
            elsif AtState.truthy? inst.evaluate_node_safe b
                false
            else
                true
            end
        },
        #<<
        # Returns <code>true</code> if both of <code>a</code> and <code>b</code> are falsey, <code>false</code> otherwise. Short-circuits. See also: <a href="#nor"><code>nor</code></a>.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "⊽" => held(true, true) { |inst, a, b|
            @@operators["nor"][inst, a, b]
        },
        #<<
        # Returns <code>false</code> if <code>b</code> is truthy, <code>a</code> otherwise.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        # @example even_nonpos := { Even[_] not Positive[_] }
        # @example Print[Select[even_nonpos, [-4, -3, -2, 0, 2, 3, 4]]]
        # @example ?? [-4, -2, 0]
        #>>
        "not" => lambda { |inst, a, b|
            # A && !B
            rres = inst.evaluate_node_safe b
            if AtState.truthy? rres
                false
            else
                inst.evaluate_node_safe a
            end
        },
        #<<
        # Returns the intersection of <code>a</code> and <code>b</code>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre operator
        #>>
        "∩" => lambda { |inst, a, b|
            a & b
        },
        #<<
        # Returns the union of <code>a</code> and <code>b</code>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre operator
        #>>
        "∪" => lambda { |inst, a, b|
            a | b
        },
        #<<
        # Returns the symmetric difference between <code>a</code> and <code>b</code>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre operator
        #>>
        "∆" => lambda { |inst, a, b|
            (a | b) - (a & b)
        },
        #<<
        # Returns the <code>a</code> without all elements of <code>b</code>. See also: <a href="#Complement"><code>Complement</code></a>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre operator
        #>>
        "Ø" => lambda { |inst, a, b|
            a - b
        },


        ## -- functional -- #
        "@" => lambda { |inst, f, g|
            if AtState.func_like? g
                lambda { |inst, *args|
                    f[inst, g[inst, *args]]
                }
            else
                f[g]
            end
        },
        "@@" => lambda { |inst, f, g|
            if AtState.func_like? g
                lambda { |inst, *args| f[inst, *g[inst, *args]] }
            else
                f[inst, *g]
            end
        },
        "@%" => lambda { |inst, f, g|
            lambda { |inst, *args|
                g[inst, *args]
                f[inst]
            }
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
        "⇒" => @@functions["Map"],
        ":>" => lambda { |inst, source, func|
            source.map { |x| func[inst, x] }
        },
        "\\" => @@functions["Select"],
        "~" => @@functions["Count"],
        "->" => lambda { |inst, key, value|
            if key.is_a?(Node) && key.head.raw == "V"
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
        "→" => lambda { |inst, key, value|
            @@operators["->"][inst, key, value]
        },
        ";;" => lambda { |inst, x, y| y },
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
                @@functions["Size"][inst, n]
            end
        },
        #<<
        # Forces <code>func</code> to use parent scope.
        # @type func fn
        # @return fn
        # @operator
        # @genre unary operator
        # @example a .= 5
        # @example Call[{ a .= 3 }]
        # @example Print[a]
        # @example ?? 5
        # @example Call['{ a .= 93 }]
        # @example Print[a]
        # @example ?? 93
        #>>
        "'" => lambda { |inst, func|
            func.descend = func.ascend = false
            func
        },
        # matrix size
        "##" => lambda { |inst, n|
            dim n
        },
        "±" => vectorize_monad { |inst, a|
            [a, @@unary_operators["-"][inst, a]]
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
                    # p args
                    # (lambda{|*b|p b})[*args]
                    f[inst, *args.flatten(1)]
                }
            else
                f.to_s
            end
        },
        # reverses arguments
        "~" => lambda { |inst, f|
            if AtState.func_like? f
                lambda { |inst, *args|
                    f[inst, *args.reverse]
                }
            else
                ~f
            end
        },
        "!" => vectorize_monad { |inst, n|
            factorial n
        },
        "?" => vectorize_monad { |inst, n|
            AtState.truthy? n
        },
        #<<
        # Returns <code>false</code> if <code>b</code> is truthy, <code>true</code> otherwise.
        # @type arg (*)
        # @return bool
        # @genre unary operator
        #>>
        "not" => lambda { |inst, arg|
            AtState.falsey? inst.evaluate_node arg
        },
        "..." => lambda { |inst, arg|
            Applicator.new arg
        },
        "…" => lambda { |inst, arg|
            @@unary_operators["..."][inst, arg]
        },
    }
end
