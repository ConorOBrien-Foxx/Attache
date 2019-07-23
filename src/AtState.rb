require_relative 'lib.rb'
require_relative 'tokenize.rb'
require_relative 'AtClass.rb'
# require_relative 'AtFunctions.rb'
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

class AtShunter
    def initialize(code)
        @stack = []
        @out = []
        @arities = []
        @curry_mask = []
        @last_token = Token.new nil, nil, nil
        @parens = []
        @format_string_stack = []
        @code = code
        @source = tokenize @code
        @consume_queue = []
    end

    def read_token
        if @consume_queue.empty?
            @source.next
        else
            @consume_queue.shift
        end
    rescue StopIteration
        nil
    end

    def imp_call_start?(type)
        [:word, :bracket_close, :func_end, :paren_close].include? type
    end

    def step
        ent = read_token

        return :stop if ent.nil?

        raw, type, start = ent

        return if type == :comment

        # TODO: make this have better precedence
        # (remove hack with !! operator)
        if raw == "{" && imp_call_start?(@last_token.type) && @last_token.line == ent.line
            ## call func
            @consume_queue << Token.new("!!", :operator)
            @consume_queue << ent
            return
        end

        if type == :format_string_begin
            @format_string_stack.push FormatStringInformation.new(ent, 1)
            @stack.push Token.new(nil, :format_indicator, nil)
            return

        elsif type == :format_string_continue
            flush(@out, @stack, [:format_indicator])
            @format_string_stack.last.count += 1
            @format_string_stack.last.token.raw += raw
            return

        elsif type == :format_string_end
            @format_string_stack.last.token.raw += raw
            info = @format_string_stack.pop
            token = info.token
            token.type = :format_string
            flush(@out, @stack, [:format_indicator])
            @stack.pop
            @out << Token.new(info.count, :format_string_count, nil)
            @out << token
            return

        end

        is_data = $DATA.include?(type) || open_func?(type)
        last_was_data = $DATA_SIGNIFIER.include? @last_token.type
        both_are_data = is_data && last_was_data

        # two adjacent datatypes mark a statement
        flush_stack =
            (both_are_data || type == :statement_sep) &&
            @format_string_stack.empty?

        # p ent
        # p @out.map(&:raw)
        # p @stack.map(&:raw)
        # puts

        if flush_stack
            flush(@out, @stack, [:func_start, :named_func_start])
        end

        if type == :statement_sep
            @last_token = ent
            return
        end

        if $DATA.include? type
            @out.push ent

        elsif open_func? type
            @stack.push ent
            @out.push ent

        elsif type == :func_end
            while @stack.last && [:operator, :unary_operator].include?(@stack.last.type)
                @out.push @stack.pop
            end

            collect = []
            until @out.empty? || open_func?(@out.last.type)
                collect.unshift @out.pop
            end

            if @out.last.type == :named_func_start
                #Token<"x", :word, 3>, #Token<"_", :abstract, 8>, #Token<".=", :operator, 5>
                prefix = []
                ["x", "y", "z"].each_with_index { |var, i|
                    prefix.push Token.new var, :word, nil
                    prefix.push Token.new "_#{i + 1}", :abstract, nil
                    prefix.push Token.new ".=", :operator, nil
                }
                collect = prefix.concat collect
            end

            next_start = @stack.pop.start
            @out.pop

            @out.push Token.new collect, :make_lambda, next_start

        elsif type == :operator
            if @last_token.nil? || !$DATA_SIGNIFIER.include?(@last_token.type)
                ent.type = :unary_operator

            else
                cur_prec, cur_assoc = $PRECEDENCE[raw]
                # NOTE: precedence determining
                loop {
                    break if @stack.empty?
                    top_raw, top_type = @stack.last
                    break if top_type != :operator && top_type != :unary_operator
                    top_prec, top_assoc = $PRECEDENCE[top_raw]

                    if top_type == :unary_operator
                        top_prec = $PRECEDENCE_UNARY[top_raw]
                    end

                    break if top_assoc == :right ? top_prec <= cur_prec : top_prec < cur_prec
                    @out.push @stack.pop
                }
            end
            @stack.push ent

        elsif type == :bracket_open
            # parse expression a.b[c] as (a.b)[c]
            if !@stack.empty? && @stack.last.raw == "."
                # but parse a.[b] as (a).([b])
                if @last_token.raw != "."
                    @out.push @stack.pop
                end
            end

            # determine if a function call
            unless $SEPARATOR.include? @last_token.type
                # the "V" function creates an array
                @out.push Token.new "V", :word, nil
            end
            @stack.push ent
            @arities.push 1

        elsif type == :curry_open
            if !@stack.empty? && @stack.last.raw == "."
                @out.push @stack.pop
            end

            # determine if a curry call
            unless $SEPARATOR.include? @last_token.type
                # the "Hash" function creates a hash
                @out.push Token.new "Hash", :word, nil
                @stack.push Token.new "[", :bracket_open, nil
                @curry_mask << true
            else
                @stack.push ent
                @curry_mask << false
            end
            @arities.push 1

        elsif type == :comma
            unless @arities.last.nil?
                @arities[-1] += 1
            end

            @out.push @stack.pop while @stack.last && [:operator, :unary_operator].include?(@stack.last.type)

            if @arities.last.nil?
                @parens[-1] = true
                @out.push Token.new "discard", :discard, nil
            end

        elsif type == :bracket_close || (type == :curry_close && @curry_mask.pop)
            if @last_token.type == :bracket_open || @last_token.type == :curry_open
                @arities[-1] = 0
            end

            loop {
                if @stack.empty?
                    raise AttacheSyntaxError.new("Unmatched closing brace: #{ent.raw}", ent.position)
                end
                break if @stack.last.type == :bracket_open
                @out.push @stack.pop
            }

            @out.push Token.new @arities.pop, :call_func, nil

            @stack.pop

        elsif type == :curry_close
            if @last_token.type == :curry_open
                @arities[-1] = 0
            end

            while @stack.last.type != :curry_open
                @out.push @stack.pop
            end

            @out.push Token.new @arities.pop, :curry_func, nil

            @stack.pop

        elsif type == :paren_open
            @stack.push ent
            @arities.push nil
            @parens.push nil

        elsif type == :paren_close
            @arities.pop
            temp = []
            loop {
                if @stack.empty?
                    raise AttacheSyntaxError.new("Expected an open parenthesis to match #{raw.inspect}", ent.position)
                end
                break if @stack.last.type == :paren_open
                temp.push @stack.pop
            }
            @stack.pop

            @out.concat temp

        elsif type == :whitespace
            # do nothing

        else
            STDERR.puts "Unknown type #{type.inspect} (#{raw.inspect}) during shunting"
            raise
        end
        @last_token = ent if type != :whitespace
    end

    def shunt
        loop {
            running = step != :stop
            break unless running
        }

        flush @out, @stack

        offender = @out.find { |raw, type|
            type == :bracket_open
        }
        if offender
            raise AttacheSyntaxError.new(
                "Unmatched closing brace: #{offender.raw}",
                offender.position
            )
        else
            @out
        end
    end
end

def shunt(code)
    AtShunter.new(code).shunt
end

def get_abstract_number(abstract, default=0)
    /\d+/ === abstract
    $& ? $&.to_i - 1 : default
end

# used for property overloading
def class_has?(klass, prop)
    return false unless AtClassInstance === klass
    klass.methods.has_key? "#{prop}"
end

def vectorizes?(ent)
    Array === ent
    # Array === ent || class_has?(ent, "$map")
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
    vec.map { |e| fn[inst, e] }
    # if class_has? vec, "$map"
    #     vec["$map"][inst, fn, vec]
    # else
        # vec.map { |e| fn[inst, e] }
    # end
end

def vectorize_arity(args, depths, &fn)
    unless Array === depths
        depths = Hash.new(depths)
    end
    arity = args.size
    checks = args.map.with_index { |e, i|
        depths[i] && vectorizes?(e)
    }
    # assert same size
    if checks.none?
        fn[*args]
    else
        args_to_check = args.select.with_index { |e, i|
            checks[i]
        }
        unique_sizes = args_to_check.map(&:size).uniq
        raise "bad lengths" unless unique_sizes.size == 1
        size = unique_sizes[0]

        (0...size).map { |i|
            them = args.map.with_index { |e, j|
                if checks[j] && depths[j]
                    e[i]
                else
                    e
                end
            }

            vectorize_arity(them, depths, &fn)
        }
    end
end

class AtFunction
    def initialize(
        fn,
        configurable: false,
        arity: nil,
        operator: false,
        memoize: false,
        associative: false,
        held: [],
        vectorize: nil
    )
        @held = held
        @operator = operator
        @associative = associative
        @memoize = memoize
        @memo = {}
        if @held == true
            @held = HOLD_ALL
        end
        if @held.nil?
            raise "@held cannot be nil"
        end
        @configurable = configurable
        @fn = fn
        @arity = arity || fn.arity rescue fn.size rescue 0
        @vectorize = vectorize
    end

    def AtFunction.from(**opts, &fn)
        AtFunction.new(fn, **opts)
    end

    def AtFunction.vectorize(arity=nil, **opts, &fn)
        AtFunction.new(fn, **opts, vectorize: true, arity: arity)
    end

    def AtFunction.memoize(arity=nil, **opts, &fn)
        AtFunction.new(fn, **opts, memoize: true, arity: arity)
    end

    def AtFunction.held(*held, &fn)
        if Hash === held[0]
            held = held[0]
        elsif held.empty?
            held = true
        end
        AtFunction.new(fn, held: held)
    end

    def AtFunction.configurable(configurable=true, **opts, &fn)
        AtFunction.new(fn, configurable: configurable, **opts)
    end

    def AtFunction.constant(value)
        AtFunction.from { |inst, *discard|
            value
        }
    end

    def size
        @arity
    end

    # raised only by `operator_value`
    class NoOperatorValueFoundError < Exception; end
    def operator_value(inst, args)
        case args.size
            when 2
                a, b = args
                # specificly defined operator
                left = "$#@operator"
                right = "$r#@operator"
                if class_has? a, left
                    return a[left][inst, b]
                elsif class_has? b, right
                    return b[right][inst, a]
                elsif @associative && class_has?(a, right)
                    return a[right][inst, b]
                elsif @associative && class_has?(b, left)
                    return b[left][inst, a]
                end
                # bin_op_left/right defined
                if class_has? a, "$bin_op_left"
                    return a["$bin_op_left"][inst,
                        @operator,
                        self,
                        b
                    ]
                elsif class_has? b, "$bin_op_right"
                    return b["$bin_op_right"][inst,
                        @operator,
                        self,
                        a
                    ]
                elsif @associative && class_has?(a, "$bin_op_right")
                    return a["$bin_op_right"][inst,
                        @operator,
                        self,
                        b
                    ]
                elsif @associative && class_has?(b, "$bin_op_left")
                    return b["$bin_op_left"][inst,
                        @operator,
                        self,
                        a
                    ]
                end

                # bin_op defined (ambidirectional)
                if class_has? a, "$bin_op"
                    return a["$bin_op"][inst,
                        "left",
                        @operator,
                        self,
                        b
                    ]
                elsif class_has? b, "$bin_op"
                    return b["$bin_op"][inst,
                        "right",
                        @operator,
                        self,
                        a
                    ]
                end
            when 1
                raise AttacheUnimplementedError.new("Unary class operators have not been implemented yet")
            else
                raise "unknown operator arity #{args.size}"
        end
        raise NoOperatorValueFoundError.new
    end
    private :operator_value

    def call_operator(inst, *args)
        found = true
        value = begin
            operator_value(inst, args)
        rescue NoOperatorValueFoundError => e
            found = false
            nil
        end
        {
            found: found,
            value: value
        }
    end

    def call_normal(inst, *args)
        if @operator
            result = call_operator(inst, *args)
            if result[:found]
                return result[:value]
            end
        end
        if @memoize && @memo.has_key?(args)
            return @memo[args]
        end
        value = @fn[inst, *args]
        if @memoize
            @memo[args] = value
        end
        value
    end

    def [](inst, *args)
        if @vectorize.nil?
            call_normal inst, *args
        else
            vectorize_arity(args, @vectorize) { |*vector_args|
                call_normal inst, *vector_args
            }
        end
    end

    attr_accessor :held, :configurable, :fn, :arity
end

HOLD_ALL = Hash.new(true)
def held(*held, &fn)
    raise AttacheDeprecationError.new "`held` will be deprecated"
    if Hash === held[0]
        held = held[0]
    end
    AtFunction.new(fn, held: held)
end
def configurable(arity: nil, &fn)
    raise AttacheDeprecationError.new "`configurable` will be deprecated"
    AtFunction.new(fn, configurable: true, arity: arity)
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
        raise "to_ary is deprecated"
    end

    def to_s(depth = 0, color: true)
        res = ""
        res += " " * DISP_WIDTH * depth
        res += NODE_PREFIX + @head.inspect(color: color)
        res += "\n"
        depth += 1
        @children.each_with_index { |child, i|
            if child.is_a? Node
                res += child.to_s(depth, color: color)
            else
                res += " " * DISP_WIDTH * depth + LEAF_PREFIX + child.inspect(color: color)
            end
            res += "\n"
        }
        depth -= 1
        res.chomp
    end

    def is_arrow_pair?
        @head&.raw == "->" && !@children.empty?
    end

    def inspect(color: true)
        (color ? "#\x1b[33mNode\x1b[0m<" : "#Node<") + "#{@head}>{#{@children.inspect}}"
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
    OPTS_CONST = "OPTIONS"
    def initialize(
        inner_ast,
        params=[],
        splat_mask=[],
        default_hash: {},
        raw: [],
        name: "<anonymous>"
    )
        @tokens = [*inner_ast]
        @params = params
        @splat_mask = splat_mask
        @splat_count = splat_mask.count(true)
        @default_hash = default_hash
        @scope = {}
        @ascend = true
        @descend = true
        @ignore_other = false
        @name = name
        @raw = token_join raw
    end

    def bind(inst)
        AtFunction.new { |other_inst, *args|
            self[inst, *args]
        }
    end

    def size
        @params.size
    end

    alias :arity :size

    attr_accessor :params, :scope, :ascend, :descend, :ignore_other, :tokens, :raw

    def call(inst, args, opts=nil)
        inst.local_descend(@scope) if @descend

        # define locals
        inst.define_local ARG_CONST, args
        unless opts.nil?
            inst.define_local OPTS_CONST, opts.map { |k, v| [k.to_s, v] } .to_h
        end
        inst.abstract_references << self

        arity = args.size
        splat_domain = arity - @params.size + @splat_count
        splat_each = splat_domain.to_f / @splat_count

        # check if splats are valid
        unless @splat_count == 0 || splat_each.to_i == splat_each
            non_splat_count = @params.size - @splat_count
            raise AttacheArgumentError.new(
                "Given #{
                    arity
                } #{
                    "argument".pluralize(splat_domain)
                }; incompatiable with #{
                    @splat_count
                } #{
                    "splat".pluralize(@splat_count)
                } and #{
                    non_splat_count
                } #{
                    "regular argument".pluralize(non_splat_count)
                }.",
                @name
            )
        end
        splat_each = splat_each.to_i rescue nil

        arg_index = 0
        @params.zip(@splat_mask) { |name, is_splat|
            if is_splat
                value = args[arg_index...arg_index + splat_each]
                arg_index += splat_each
            else
                if arg_index >= args.size
                    if @default_hash.has_key? name
                        value = inst.evaluate_node @default_hash[name], @scope
                    else
                        raise AttacheArgumentError.new(
                            "No value passed for parameter #{
                                name.inspect
                            }.",
                            @name
                        )
                    end
                else
                    value = args[arg_index]
                end
                arg_index += 1
            end
            inst.define_local name, value
        }

        temp_scope = @scope.dup

        res = @tokens.map.with_index { |token, i|
            inst.save_blanks args
            inner = inst.evaluate_node(token, @scope)

            if @ascend && @descend
                temp = inst.locals.last.dup
                # NOTE: I have no idea why this was here, but commenting it out
                # fixes a bug so I'm just going to do that.
                # @params.each { |param|
                #     temp.delete param
                # }
                temp_scope.merge! temp
            end

            AtState.traverse(inner) { |atom|
                if atom.kind_of? AtLambda
                    atom.scope.merge! temp_scope
                end
            }

            inst.pop_blanks

            inner
        }.last

        inst.abstract_references.pop
        temp_scope = inst.local_ascend if @ascend

        @scope.merge! temp_scope

        res
    end

    def call_with_opts(inst, *args, **opts)
        call inst, args, opts
    end

    def [](inst, *args)
        call inst, args, nil
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
    return AtFunction.from { |inst, *others|
        abstracts = []
        to_remove = []

        fill_abstracts = -> args {
            args.each { |el|
                if Token === el && el.type == :abstract
                    n = get_abstract_number(el.raw)
                    abstracts[n] = others[n]
                    to_remove.push n
                elsif el.is_a? Node
                    fill_abstracts[el.children]
                end
            }
        }

        fill_abstracts[args]

        not_provided = to_remove - (0...others.size).to_a

        if not_provided.empty?
            others.reject!.with_index { |e, i| to_remove.include? i }

            inst.save_blanks abstracts
            new_args = args.map { |arg|
                inst.evaluate_node arg
            }
            new_args += others
            calling_function = inst.evaluate_node(func)
            result = calling_function[inst, *new_args]
            inst.pop_blanks
            result
        else
            update_abstracts = -> args {
                args.map { |el|
                    if Token === el && el.type == :abstract
                        n = get_abstract_number(el.raw)
                        ind = not_provided.index n
                        if ind
                            dest = n - others.size + 1
                            Token.new "_#{dest}", :abstract, nil
                        else
                            abstracts[n]
                        end
                    elsif el.is_a? Node
                        Node.new(el.head, update_abstracts[el.children])
                    else
                        el
                    end
                }
            }

            next_args = update_abstracts[args]

            make_curry(next_args, func)
        end
    }
end

def curry(arity=nil, &fn)
    arity ||= fn.arity - 1
    rec = AtFunction.from { |inst, *args|
        if args.size < arity
            AtFunction.from { |inst, *more|
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
        shunt program
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

    def ===(other)
        self == Type.of(other)
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
    def initialize(message, pos=nil)
        @message = message
        @pos = pos
    end

    def AttacheError.descendants
        ObjectSpace.each_object(Class).select { |klass| klass < AttacheError }
    end

    def readable
        "#{self.class.name}: #{@pos ? "(#{@pos}) " : ""}#{@message}"
    end
end

# when an operator is used incorrectly
class AttacheOperatorError < AttacheError; end
# when an error is raised during runtime, usually by the user
class AttacheRuntimeError < AttacheError; end
# when a value is undefined
class AttacheUndefinedError < AttacheError; end
# for behaviour not yet implemented
class AttacheUnimplementedError < AttacheError; end
# for invalid arguments (e.g. count)
class AttacheArgumentError < AttacheError; end
# a syntax error...
class AttacheSyntaxError < AttacheError; end
# when improper data is given to a function
class AttacheValueError < AttacheError; end
# for deprecating various aspsects of Attache, particularly internal functions
class AttacheDeprecationError < AttacheError; end


class AttacheValueError
    def self.assert_type(type, *values, source: nil)
        values.each { |value|
            unless type === value
                raise self.new("Expected value to be #{
                    type
                }, got #{
                    Type.of(value)
                }.", source)
            end
        }
    end
end

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
        "nan" => Float::NAN,
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
        "erng" => AtRNG.new(42),
        "NOT_PROVIDED" => AtFunctionCatalog::NOT_PROVIDED,
        "NP" => AtFunctionCatalog::NOT_PROVIDED,
        "alpha" => $ALPHA_LOWER,
        "ALPHA" => $ALPHA_UPPER,
        "NTS_dict_after" => "0123456789#$ALPHA_LOWER#$ALPHA_UPPER",
        "NTS_dict_head" => "#$ALPHA_LOWER#$ALPHA_UPPER",
        "ascii" => (32..126).map(&:chr).join,
    }
    @@extended_variables = {}

    def initialize(
            program,
            input=STDIN,
            output=STDOUT,
            exclude_std: false,
            auto_save_results: false
    )
        @trees = ast(program)
        if @trees.nil?
            exit
        end
        @variables = @@default_variables.dup
        @abstract_references = []
        @locals = [{}]
        @blanks = []
        @position = nil
        @in = input
        @out = output
        @auto_save_results = auto_save_results
        @saved = []
        load_lib "std" unless exclude_std
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
    end

    @@attribute_map = {
        "#" => :configurable,
        "@" => :vector,
        "&" => :curry,
        "!" => :memoize,
    }
    def set_variable(var, val, dest=:define)
        if Node === var
            # set operator by arity
            head_token = Token === var.head

            attributes = []
            if head_token
                loop {
                    index = @@attribute_map[var.head.raw]

                    break if index.nil?

                    attributes << index

                    var = var.children.first
                }
            end

            is_op_set = head_token && var.head.raw == "/"
            is_op_set &&= Token === var.children.first
            is_op_set &&= var.children.first.type == :op_quote

            if is_op_set
                op, arity = var.children
                arity = get_value arity
                val = evaluate_node val
                raise if is_vector #todo: fix
                set_op_quote op, val, arity
                return val

            # child updating
            elsif var.head.raw == "."
                parent, prop = var.children
                object = evaluate_node parent
                res = evaluate_node val
                if Node === prop
                    if prop.head.raw == "V"
                        prop.children.map.with_index { |child, i|
                            begin
                                value_at = res[i]
                            rescue
                                raise AttacheValueError.new(
                                    "Error obtaining element at index #{i} of `#{res.inspect}`",
                                    child.position
                                )
                            end
                            object[child.raw] = res[i]
                        }
                    else
                        raise AttacheUnimplementedError.new(
                            "Unknown node head type #{prop.head.raw.inspect}",
                            prop.position
                        )
                    end
                elsif Token === prop
                    object[prop.raw] = res
                else
                    raise AttacheUnimplementedError.new("Unhandled property type #{prop.class}")
                end

            else
                #todo: pattern matching++
                splat_mask = var.children.map { |arg|
                    Node === arg && arg.head.raw == "..."
                }
                default_hash = {}
                args = var.children.map.with_index { |e, i|
                    if Node === e
                        if e.head.raw == ".="
                            name = e.children[0].raw
                            raw_default = e.children[1]
                            default_hash[name] = raw_default
                            name
                        elsif e.head.raw = "..."
                            e.children[0].raw
                        else
                            raise AttacheUnimplementedError.new(
                                "Unidentified expression in parameter #{
                                    i + 1
                                } (symbol #{
                                    e.head.inspect
                                })",
                                e.head.position
                            )
                        end
                    else
                        e.raw
                    end
                }
                if ["'", "V"].include? var.head.raw
                    val = evaluate_node val
                    args.each_with_index { |arg, i|
                        send dest, arg, val[i]
                    }
                    return val
                else
                    res = AtLambda.new(
                        [val],
                        args,
                        splat_mask,
                        default_hash: default_hash,
                        name: var.head.readable
                    )

                    attributes.each { |attr|
                        old = res
                        res = case attr
                            when :curry
                                curry(args.size) { |inst, *args|
                                    old[inst, *args]
                                }
                            when :vector
                                AtFunction.vectorize { |inst, *args|
                                    old[inst, *args]
                                }
                            when :configurable
                                AtFunction.configurable { |inst, *args, **configurable|
                                    old[inst, *args, **configurable]
                                }
                            when :memoize
                                AtFunction.memoize { |inst, *args|
                                    old[inst, *args]
                                }
                            else
                                raise AttacheUnimplementedError.new("unknown attribute " + attr.to_s)
                        end
                    }
                    if var.head.type == :op_quote
                        set_op_quote var.head, res
                    else
                        send dest, var.head.raw, res
                    end
                    return res
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
            return res
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
        str[2..-2].gsub(/""/, '"').gsub(/\\x.{2}|\\./) { |e|
            eval '"' + e + '"' rescue e
        }.gsub(/\$\{\}/).with_index { |match, i|
            args[i]
        }
    end

    def parse_number(raw)
        raw = raw.dup
        modifiers = []
        while !raw.empty? && "xi".index(raw[-1])
            modifiers << raw[-1]
            raw.chop!
        end
        raw.gsub!(/^\./, "0.")
        if modifiers.include? "x"
            res = BigDecimal.new(raw)
        else
            res = eval raw
        end
        res *= 1i if modifiers.include? "i"
        res
    end

    def all_variables
        [@@functions, @variables, *@locals]
    end

    def closest_variables_to(name, limit=1)
        possibilities = all_variables.flat_map { |scope|
            scope.keys
        }.map { |var|
            [var, levenshtein_distance(var, name)]
        }.select { |var, dist|
            dist <= limit
        }.sort_by { |var, dist|
            dist
        }.map { |var, dist|
            var
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

        elsif type == :format_string_bare
            raw[2..-2].gsub(/""/, '"').gsub(/\\x.{2}|\\./) { |e|
                eval '"' + e + '"' rescue e
            }

        elsif type == :raw_string
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
            parse_number raw

        elsif type == :compressed_number
            @@functions["STN"][self, raw[2..-1]]

        elsif type == :make_lambda
            AtLambda.new(ast(raw), raw: raw)

        elsif type == :word
            closest = closest_variables_to raw
            message = "Undefined variable #{raw.inspect}.\n"
            message += "Did you mean?"
            closest.each { |var|
                message += "\n    #{var}"
            }
            raise AttacheUndefinedError.new(
                message,
                obj.position
            )

        elsif type == :abstract_reference
            @abstract_references[-raw.size]

        elsif type == :abstract
            get_blank(raw)

        elsif type == :counter_reference
            index = raw[1] == "$" ? -raw[2..-1].to_i : raw[1..-1].to_i + 1
            ARGV[index]

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

    def save_blanks(blanks)
        @blanks << blanks
    end

    def pop_blanks
        @blanks.pop
    end

    def get_blanks
        if @blanks.empty?
            @saved
        else
            @blanks.last
        end
    end

    def get_blank(blank)
        type = blank.match(/_+/)[0].size
        n = get_abstract_number(blank)

        range = case type
            when 1
                n
            when 2
                n..-1
            else
                STDERR.puts "Blank too long: #{type} of #{blank}"
        end

        get_blanks[range]
    end

    def AtState.configurable?(func)
        case func
            when AtFunction
                func.configurable
            else
                false
        end
    end

    def evaluate_leaf(token, merge_with, check_error: true)
        unless token.is_a? Token
            raise "#{node.inspect} is not a token"
        end
        res = nil

        if token.type == :abstract
            res = get_blank token.raw
        else
            res = get_value token
        end

        res
    end

    def evaluate_atfunction(fun, merge_with, check_error: true)

    end

    def evaluate_function(fun, merge_with, check_error: true)

    end

    def call_function(func, node, args, configurable: false)
        res = if configurable
            func[self, *args, **configurable]
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
                # TODO: use an actual error
                rescue ArgumentError => e
                    source = node.head rescue node.inspect
                    STDERR.puts "Argument error: #{source}"
                    raise e
                end
            end
        end
        res
    end

    def evaluate_node(node, merge_with = nil, check_error: true)
        unless node.is_a? Node
            value = evaluate_leaf node, merge_with, check_error: true
            return value
        end

        position_holder = node.head == Node ? node.head.raw : node.head
        @position = position_holder.position rescue nil

        is_format_string = node.head.type == :format_string rescue false
        # special cases
        args = []

        func = is_format_string ? nil : get_value(node.head)

        # 1st pass at characteristic checking
        if AtFunction === func
            held = func.held
            configurable = func.configurable
        elsif AtLambda === func
            held = []
            configurable = false
        else
            held = []
            configurable = false
        end

        # obtain value of head
        if func.is_a? Node
            func = evaluate_node func, merge_with, check_error: check_error
        end

        # evaluate children
        children = node.children.map.with_index { |child, i|
            # raw, type = child
            value = if held[i]
                child
            else
                if child.is_a? Node
                    evaluate_node child, merge_with, check_error: check_error
                elsif child.type == :abstract
                    get_blank child.raw
                else
                    get_value child
                end
            end
            value
        }
        args.concat children

        # second pass of held arguments
        if AtFunction === func
            configurable = func.configurable
            held = func.held
        end

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
            configurable = split[true].map { |a, b| [a.to_sym, b] }.to_h
            args = split[false]
        end

        # reduce applicators
        args = (args || []).flat_map { |e|
            Applicator === e ? e.value : [e]
        }

        # preemptively return format strings
        if is_format_string
            return format_string(node.head.raw, args)
        end

        # error checking -- function/operator arity does not exist
        if func.nil?
            if Token === node.head && node.head.type == :unary_operator
                raise AttacheOperatorError.new("Operator #{node.head.raw.inspect} has no unary case.", node.head.position)
            end
            STDERR.puts "[in function execution] Error in retrieving value for #{node.head.inspect}"
            exit -3
        end

        # infuse scopes
        if func.kind_of?(AtLambda) && !merge_with.nil?
            func.scope.merge! merge_with
        end

        # call the function
        result = call_function(func, node, args, configurable: configurable)

        # infuse scope
        if result.kind_of?(AtLambda) && !merge_with.nil?
            result.scope.merge! merge_with
        end

        result
    end

    def evaluate_node_safe(node_maybe)
        if Node === node_maybe || Token === node_maybe
            evaluate_node node_maybe
        else
            node_maybe
        end
    end

    def run
        last_results = []
        @trees.map { |tree|
            save_blanks last_results.dup if @auto_save_results
            res = evaluate_node tree
            pop_blanks if @auto_save_results
            if AtError === res
                puts res.to_s
                exit -2
            end
            last_results = [res] if @auto_save_results
            res
        }
    end

    def AtState.function(name, aliases: [], configurable: false, hold: [], &body)
        fn = AtFunction.new(
            convert_to_lambda(&body),
            configurable: configurable,
            held: hold
        )
        @@functions[name] = fn

        aliases.each { |ali|
            @@functions[ali] = fn
        }
    end

    def AtState.variable(name, value)
        @@extended_variables[name] = value
    end

    def cast_string(value, *modes)
        if AtClassInstance === value && value.methods["$string"]
            value.methods["$string"][self, *modes]
        else
            value.to_s(*modes) rescue "#{value}"
        end
    end

    def cast_list(value)
        if class_has? value, "$list"
            value["$list"][self]
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
