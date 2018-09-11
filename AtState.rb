require_relative 'lib.rb'
require_relative 'tokenize.rb'
# require_relative 'AtClass.rb'
# later in file

def atstate_init(argv)
    $ARGV = argv
end

FOLDER_LOCATION = File.dirname(__FILE__)

def flush(out, stack, fin=[])
    out.push stack.pop until stack.empty? || fin.include?(stack.last.type)
end

def open_func?(type)
    [:func_start, :named_func_start].include? type
end

FormatStringInformation = Struct.new(:token, :count) {
    def inspect
        "FSI(#{token}, #{count})"
    end
}
def parse(code)
    # group expression
    stack = []
    out = []
    arities = []
    # keep track of curries promoted to calls
    curry_mask = []
    last_token = Token.new nil, nil, nil
    parens = []

    format_string_stack = []

    # di "tokenizer"
    tokenize(code).each { |ent|
        raw, type, start = ent

        next if type == :comment

        # cls
        # dh "current", ent
        # darr "stack", stack
        # darr "out", out
        # darr "format", format_string_stack
        # STDIN.gets

        if type == :format_string_begin
            format_string_stack.push FormatStringInformation.new(ent, 1)
            stack.push Token.new(nil, :format_indicator, nil)
            next

        elsif type == :format_string_continue
            flush(out, stack, [:format_indicator])
            format_string_stack.last.count += 1
            format_string_stack.last.token.raw += raw
            next

        elsif type == :format_string_end
            format_string_stack.last.token.raw += raw
            info = format_string_stack.pop
            token = info.token
            token.type = :format_string
            flush(out, stack, [:format_indicator])
            stack.pop
            out << Token.new(info.count, :format_string_count, nil)
            out << token
            next

        end

        is_data = $DATA.include?(type) || open_func?(type) # || type == :curry_open
        last_was_data = $DATA_SIGNIFIER.include? last_token.type

        # two adjacent datatypes mark a statement
        if (is_data && last_was_data || type == :statement_sep) && format_string_stack.empty?
            flush(out, stack, [:func_start, :named_func_start])
        end

        if type == :statement_sep
            last_token = ent
            next
        end

        if $DATA.include? type
            out.push ent

        elsif open_func? type
            stack.push ent
            out.push ent

        elsif type == :func_end
            while stack.last && [:operator, :unary_operator].include?(stack.last.type)
                out.push stack.pop
            end

            collect = []
            until out.empty? || open_func?(out.last.type)
                collect.unshift out.pop
            end

            if out.last.type == :named_func_start
                #Token<"x", :word, 3>, #Token<"_", :abstract, 8>, #Token<".=", :operator, 5>
                prefix = []
                ["x", "y", "z"].each_with_index { |var, i|
                    prefix.push Token.new var, :word, nil
                    prefix.push Token.new "_#{i + 1}", :abstract, nil
                    prefix.push Token.new ".=", :operator, nil
                }
                collect = prefix.concat collect
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
                    raise AttacheSyntaxError.new("Unmatched closing brace: #{ent.raw}", ent.position)
                    # STDERR.puts "Syntax Error: unmatched closing brace: #{ent}"
                    # return nil
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
            loop {
                if stack.empty?
                    raise AttacheSyntaxError.new("Expected an open parenthesis to match #{raw.inspect}", ent.position)
                end
                break if stack.last.type == :paren_open
                temp.push stack.pop
            }
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
        raise AttacheSyntaxError.new("Unmatched closing brace: #{offender.raw}", offender.position)
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

HOLD_ALL = Hash.new(true)
def held(*held, &fn)
    if Hash === held[0]
        held = held[0]
    end
    AtFunction.new(fn, held: held)
end
def configurable(arity: nil, &fn)
    AtFunction.new(fn, config: true, arity: arity)
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

    # def inspect
    #     "AtLambda(#{@raw})"
    # end

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
    def initialize(key, value, raw: true)
        @key = key
        @value = value
        @raw = !!raw
    end

    def raw?
        @raw
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
    format_string_info = []
    build = nil
    return nil if shunted.nil?
    shunted.each { |ent|
        raw, type, start, line, column = ent
        if type == :call_func
            args = stack.pop(raw)
            func = stack.pop
            cur = Node.new func, args
            stack.push cur

        elsif type == :curry_func
            args = stack.pop(raw)
            func = stack.pop
            stack.push Token.new make_curry(args, func), :function, start, line, column

        elsif type == :operator
            args = stack.pop(2)
            cur = Node.new ent, args
            stack.push cur

        elsif type == :unary_operator
            arg = stack.pop
            cur = Node.new ent, [arg]
            stack.push cur

        elsif type == :format_string
            count = format_string_info.pop
            args = stack.pop(count)
            stack.push Node.new ent, args

        elsif type == :format_string_count
            format_string_info.push ent.raw

        elsif $DATA.include? type
            stack.push ent

        # elsif type == :discard
            # stack.push Node.new ent, [stack.pop]

        elsif type == :paren_open
            raise AttacheSyntaxError.new("Unmatched parenthesis: #{raw.inspect}", ent.position)

        else
            raise AttacheUnimplementedError.new("Unhandled type during shunting: #{type}", ent.position)
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

class Type
    private_class_method :new
    def initialize(name)
        @value = name
    end

    STRING = new(:string)
    ARRAY = new(:array)
    NUMBER = new(:number)
    FUNCTION = new(:function)
    def inspect
        "Type(#{@value.to_s})"
    end
    def to_s
        @value.to_s
    end

    def Type.of(el)
        case el
            when String
                STRING
            when Array
                ARRAY
            when Numeric
                NUMBER
            when AtClassInstance
                new el.parent.name
            else
                if AtState.func_like? el
                    FUNCTION
                else
                    raise "No type defined for #{el}, of class #{el.class}"
                end
        end
    end
end

class AttacheError < Exception
    def initialize(message, line=nil)
        @message = message
        @line = line
    end

    def AttacheError.descendants
        ObjectSpace.each_object(Class).select { |klass| klass < AttacheError }
    end

    def readable
        "#{self.class.name}: #{@line ? "(#{@line})" : ""} #{@message}"
    end
end

# when an operator is used incorrectly
class AttacheOperatorError < AttacheError; end
# for behaviour not yet implemented
class AttacheUnimplementedError < AttacheError; end
# a syntax error...
class AttacheSyntaxError < AttacheError; end
# when improper data is given to a function
class AttacheValueError < AttacheError; end


require_relative 'AtClass.rb'
require_relative 'AtFunctions.rb'

class AtState
    def AtState.truthy?(ent)
        return true if AtState.func_like? ent
        res = ent && ent != 0 && (ent.size != 0 rescue true)
    end

    def AtState.typeof(ent)
        Type.of(ent)
    end

    def set_op_quote(token, res, arity=nil)
        op = token.raw[1..-1]
        hash = {
            1 => @@unary_operators,
            2 => @@operators,
        }
        if arity.nil?
            index = res.arity.negative? ? ~res.arity : res.arity
        else
            index = arity
        end
        index = 2 if index > 2

        if index.zero?
            hash[1][op] = hash[2][op] = res
        else
            hash[index][op] = res
        end
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
        "NOT_PROVIDED" => AtFunctionCatalog::NOT_PROVIDED,
        "NP" => AtFunctionCatalog::NOT_PROVIDED,
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
        @position = nil
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
                    ast(File.read(loc, encoding: "utf-8")).each { |tree|
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
    attr_accessor :variables, :locals, :saved, :in, :out, :abstract_references, :position

    def error(message)
        STDERR.puts message
        exit
        # raise
    end

    def set_variable(var, val, dest=:define)
        if Node === var
            # set operator by arity
            is_op_set = Token === var.head && var.head.raw == "/"
            is_op_set &&= Token === var.children.first
            is_op_set &&= var.children.first.type == :op_quote

            if is_op_set
                op, arity = var.children
                arity = get_value arity
                val = evaluate_node val
                set_op_quote op, val, arity
            else
                #todo: pattern matching++
                args = var.children.map { |e|
                    if Node === e
                        e.children[0].raw
                        STDERR.puts "Note: undefined behaviour raised: unimplemented argument matching"
                    else
                        e.raw
                    end
                }
                if ["'", "V"].include? var.head.raw
                    val = evaluate_node val
                    args.each_with_index { |arg, i|
                        send dest, arg, val[i]
                    }
                else
                    res = AtLambda.new [val], args
                    if var.head.type == :op_quote
                        set_op_quote var.head, res
                    else
                        send dest, var.head.raw, res
                    end
                end
            end
        else
            res = evaluate_node(val)
            if var.type == :op_quote
                unless AtState.func_like? res
                    res = lambda { |*discard| res }
                end
                set_op_quote var, res
            else
                name = var.raw
            end
            send dest, name, res
        end
    end

    def set_global(var, val)
        set_variable var, val
    end

    def set_local(var, val)
        set_variable var, val, :define_local
    end

    def default_cell_type(type)
        case type
            when String
                " "
            when Numeric
                0
            when Array
                []
            else
                type.class.new
        end
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

    def format_string(str, args)
        str[2..-2].gsub(/\$\{\}/).with_index { |match, i|
            args[i]
        }
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

        elsif type == :raw_string || type == :format_string_bare
            raw[2..-2].gsub(/""/, '"')

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
                op = source[ref]
                op[inst, *args]
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

        elsif type == :counter_reference
            ARGV[raw[1] == "$" ? raw[1..-1].to_i + 1 : -raw[2..-1].to_i]

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

    def AtState.configurable?(func)
        case func
            when AtFunction
                func.config
            else
                false
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

        position_holder = head == Node ? head.raw : head
        @position = position_holder.position rescue nil

        is_format_string = head.type == :format_string rescue false
        # special cases
        args = []

        func = is_format_string ? nil : get_value(head)
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

        if is_format_string
            return format_string(head.raw, args)
        end

        if func.is_a? Node
            func = evaluate_node func, blank_args, merge_with, check_error: check_error
        end

        if func.nil?
            if head.type == :unary_operator
                raise AttacheOperatorError.new("Operator #{head.raw.inspect} has no unary case.", head.position)
            end
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
