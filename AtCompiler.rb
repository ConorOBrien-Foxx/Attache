require_relative 'AtState.rb'

class AtCompiler
    def initialize(program)
        @program = program
        @tokens = parse program
        @preamble = []
        @output = []

        @stack = []

        @parsed = false
    end

    def to_s
        parse_all unless @parsed
        (@preamble + @output).join "\n"
    end

    def parse_all
        @tokens.each { |token|
            parse_token token
        }
        @parsed = true
    end

    def append(*lines)
        @output.concat lines
    end
    def append_preamble(*lines)
        @preamble.concat lines
    end

    def parse_token(token)
        case token.type
            when :reference, :string, :number
                @stack.push send :"parse_#{token.type}", token.raw
            when :word
                ensure_function_defined token.raw
                @stack.push token.raw
            when :call_func
                arity = token.raw
                args = @stack.pop arity
                fn = @stack.pop
                append call_function fn, args
            else
                p token, token.type
        end
    end

    def parse_number(n, flags="")
        raise "unimplemented `number`"
    end

    def ensure_function_defined(name)
        raise "unimplemented `ensured_function_defined`"
    end

    def call_function(name, args)
        raise "unimplemented `call_function`"
    end

    def parse_reference(to)
        raise "unimplemented `reference`"
    end

    def parse_string(bytes)
        raise "unimplemented `string`"
    end
end

class ToRuby < AtCompiler
    FUNCTION_TRANSLATIONS = {
        "Print" => [
            "def Print(*args)",
            "    puts args.map(&:inspect).join ' '",
            "end"
        ]
    }
    def initialize(*args)
        super(*args)
        @compiled_functions = {}
        # append_preamble ""
    end

    def ensure_function_defined(name)
        return unless FUNCTION_TRANSLATIONS[name]

        unless @compiled_functions[name]
            @compiled_functions[name] = true
            append_preamble *FUNCTION_TRANSLATIONS[name]
        end
    end

    def call_function(name, *args)
        "#{name}(#{args.join ", "})"
    end

    def parse_reference(to)
        parse_string to
    end

    def parse_string(bytes)
        bytes.inspect
    end

    def parse_number(n, flags="")
        "#{n}#{flags}"
    end

end
