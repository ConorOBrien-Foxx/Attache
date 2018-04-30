require_relative 'lib.rb'
# require_relative 'AtClass.rb'
# later in file

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

    "!"       => [16, :right],
    "^"       => [15, :right],
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
    "<:"      => [10, :left],
    "↞"      => [10, :left],

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
    "is_a"    => [8, :left],

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
    "↠"      => [3, :left], # :> alias
    "typeof"  => [3, :left],

    ":="      => [2, :right],
    "::="     => [2, :right],
    "≔"      => [2, :right],
    ".="      => [2, :right],
    "..="     => [2, :right],

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

def vectorizes?(ent)
    Array === ent || class_has?(ent, "$map")
end

def map_vector(inst, vec, with_index: false, &fn)
    if fn.nil?
        res = []
        map_vector(inst, vec, with_index: with_index) { |inst, e, *|
            res << e
        }
        return res
    end

    if with_index
        i = 0
        old_fn = fn
        fn = lambda { |inst, e|
            res = old_fn[inst, e, i]
            i += 1
            res
        }
    end
    if class_has? vec, "$map"
        vec["$map"][inst, fn, vec]
    else
        vec.map { |e| fn[inst, e] }
    end
end

def vectorize_monad(&fn)
    res = lambda { |inst, *args|
        x , * = args
        if vectorizes? x
            map_vector(inst, x) { |inst, e| res[inst, e] }
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
        use_left = vectorizes?(x) && left
        use_right = vectorizes?(y) && right
        if args.size == 1
            if use_left
                map_vector(inst, x) { |inst, e| res[inst, e, *rest] }
            else
                fn[inst, x, *rest]
            end
        elsif use_left
            if use_right
                ys = y
                if class_has? y, "$map"
                    # because `map` doesn't have to return an array, this
                    # step is necessary to procure an array.
                    # TODO: array cast overload?
                    ys = map_vector(inst, y)
                end
                map_vector(inst, x, with_index: true) { |inst, e, i|
                    res[inst, e, ys[i], *rest]
                }
            else
                map_vector(inst, x) { |inst, e| res[inst, e, y, *rest] }
            end
        elsif use_right
            map_vector(inst, y) { |inst, e|
                res[inst, x, e, *rest]
            }
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
    def initialize(fn, held: held=[], config: config=false, arity: nil)
        @held = held
        @config = config
        @fn = fn
        @arity = arity || fn.arity rescue fn.size rescue 0
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

def token_join(toks)
    return toks if String === toks
    toks.map { |e|
        res = e.type == :call_func ? "Call<#{e.raw}>" :  e.raw
        Array === res ? "[" + token_join(res) + "]" : res
    }.join(" ")
end

class AtLambda
    ARG_CONST = "ARGUMENTS"
    def initialize(inner_ast, params=[], raw: [])
        @tokens = [*inner_ast]
        @params = params
        @scope = {}
        @ascend = true
        @descend = true
        @ignore_other = false
        @raw = token_join raw
    end

    def bind(inst)
        lambda { |*args|
            self[inst, *args]
        }
    end

    def size
        @params.size
    end

    def inspect
        "AtLambda(#{@raw})"
    end

    alias :arity :size

    attr_accessor :params, :scope, :ascend, :descend, :ignore_other, :tokens, :raw

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

        @scope.merge! temp_scope

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

def curry(arity=nil, &fn)
    arity ||= fn.arity - 1
    rec = lambda { |inst, *args|
        if args.size < arity
            lambda { |inst, *more|
                rec[inst, *args, *more]
            }
        else
            fn[inst, *args]
        end
    }
    rec
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
    klass.methods.has_key? "#{prop}"
end


require_relative 'AtFunctions.rb'
class AtState
    def AtState.truthy?(ent)
        return true if AtState.func_like? ent
        res = ent && ent != 0 && (ent.size != 0 rescue true)
    end

    def set_op_quote(token, res)
        op = token.raw[1..-1]
        hash = {
            1 => @@unary_operators,
            2 => @@operators,
        }
        hash[res.arity.negative? ? ~res.arity : res.arity][op] = res
    end

    def AtState.falsey?(ent)
        !AtState.truthy?(ent)
    end

    def AtState.func_like?(ent)
        AtLambda === ent || AtFunction === ent || AtClassMethod === ent ||
        Proc === ent || Train === ent || Tie === ent
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
            AtFunction.new(lambda { |inst, *args|
                source = args.size == 1 ? @@unary_operators : @@operators
                source[ref][inst, *args]
            }, arity: 2)

        elsif type == :number
            # todo: fix this hack
            eval raw.gsub(/^\./, "0.")

        elsif type == :make_lambda
            AtLambda.new(ast(raw), raw: raw)

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

        args = (args || []).flat_map { |e|
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
            if Array === func || Hash === func || String === func || class_has?(func, "$get")
                @@functions["Get"][self, func, *args]
            else
                begin
                    if class_has? func, "$call"
                        func["$call"][self, *args]
                    else
                        func[self, *args]
                    end
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

    def cast_list(value)
        if class_has? value, "$map"
            map_vector(self, value)
        else
            force_list value
        end
    end

    def enlist(value)
        if class_has? value, "$map"
            map_vector(self, value)
        elsif value.nil?
            [value]
        else
            [*value]
        end
    end

    include AtFunctionCatalog
end

# aha
require_relative 'AtClass.rb'
