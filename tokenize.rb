class Token
    def initialize(raw=nil, type=nil, start=nil, line=nil, column=nil)
        @raw = raw
        @type = type
        @start = start
        @line = line
        @column = column
    end

    def Token.parse(str)
        require 'json'
        type, start, line, col, raw = str.split(/\s+/, 5) + Array.new(5) { "" }
        raw = JSON::parse raw
        type = type.to_sym
        start, line, col = [start, line, col].map { |e|
            if e.empty?
                nil
            else
                e.to_i rescue nil
            end
        }
        Token.new(raw, type, start, line, col)
    end

    def to_parse
        "#{@type} #{@start} #{@line} #{@column} #{@raw.inspect}"
    end

    attr_accessor :raw, :type, :start, :line, :column
    @@words = %w(raw type start line column)

    def [](n)
        raise "Indexing is deprecated. `Use inst.#{@@words[n]}` instead."
    end

    def []=(n, v)
        raise "Indexing is deprecated. `Use inst.#{@@words[n]} = #{v.inspect}` instead."
    end

    def to_ary
        @@words.map { |word|
            instance_variable_get "@#{word}"
        }
    end

    def to_s
        "#{@type} #{@raw.inspect} @ #{@start}"
    end

    def position
        "#{line}:#{column}"
    end

    def inspect
        "#\x1b[33mToken\x1b[0m<" + to_ary.map(&:inspect).join(", ") + ">"
    end
end

$WORD = /[[:alpha:]][[[:alpha:]]\w]*/
$ABSTRACT = /_+\d*/
$NUMBER = /(?:(?:[0-9]*\.[0-9]+)|(?:[0-9]+))(?:i?x?|x?i?)/
$REFERENCE = /\$#$WORD/
$COUNTER_REFERENCE = /\$\$?[0-9]+/
$ABSTRACT_REFERENCE = /\$+/
$BRACKET_OPEN = /\[|do\b/
$BRACKET_CLOSE = /\]|end\b/
$PAREN_OPEN = /\(/
$PAREN_CLOSE = /\)/
$COMMA = /,/
$STRING = /"(?:[^"]|"")*"/
$RAW_STRING = /`#$STRING/
$FORMAT_STRING_BEGIN = /\$"/
$FORMAT_STRING_END = /"/
$FORMAT_STRING_CONTINUE = /\}/
$FORMAT_STRING_INTERRUPT = /\$\{/
# $FORMAT_STRING = /\$#$STRING/
$FUNC_START = /\{/
$NAMED_FUNC_START = /\$#$FUNC_START/
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
    "."        => [999, :left],

    ":"        => [30, :left],
    "::"       => [30, :left],

    "`"        => [28, :left],

    "&"        => [26, :left],
    "&:"       => [26, :left],
    "~"        => [25, :left],
    "@"        => [24, :left],
    "@@"       => [24, :left],
    "@%"       => [24, :left],
    "&>"       => [20, :right],
    "=>"       => [20, :right],
    "@>"       => [20, :right],
    "⇒"       => [20, :right], # => alias
    "\\"       => [20, :right],
    "#"        => [20, :left],
    "''"       => [20, :left],
    "'"        => [20, :left],
    "##"       => [19, :left],



    "∩"        => [18, :left], # Intersection alias
    "∪"        => [17, :left], # Union alias
    "∆"        => [17, :left], # symmetric difference
    "Ø"        => [17, :left], # setwise difference
    "^^"       => [17, :left], # similar to the above
    "⩓"        => [17, :left], # ^^ alias

    "!"        => [16, :right],
    "^"        => [15, :right],
    "?"        => [15, :left],
    "*"        => [13, :left],
    "/"        => [13, :left],
    "//"       => [13, :left],
    "⁄"        => [13, :left], # // alias (fraction slash)
    "%"        => [13, :left],
    "|"        => [12, :left],
    "+"        => [11, :left],
    "-"        => [11, :left],
    "±"        => [11, :left],
    "<:"       => [10, :left],
    "↞"       => [10, :left],

    "="        => [9, :left],
    "=/="      => [9, :left],
    "/="       => [9, :left],
    "≠"        => [9, :left], # /= alias
    "=="       => [9, :left],
    "is"       => [9, :left], # == alias
    "<"        => [9, :left],
    ">"        => [9, :left],
    "<="       => [9, :left],
    "≤"        => [9, :left], # <= alias
    ">="       => [9, :left],
    "≥"        => [9, :left], # >= alias

    "in"       => [8, :left],
    "!in"      => [8, :left],
    "is_a"     => [8, :left],
    "is_an"    => [8, :left],

    ".."       => [7, :left],
    "‥"        => [7, :left], # .. alias
    "..."      => [7, :left],
    "…"        => [7, :left], # ... alias

    "|>"       => [6, :left],
    "▷"       => [6, :left], # |> alias
    "<|"       => [6, :right],
    "◁"       => [6, :left], # <| alias

    "and"      => [6, :left],
    "∧"        => [6, :left], # and alias
    "nor"      => [6, :left],
    "⊽"        => [6, :left], # nor alias
    "not"      => [6, :left],
    "xor"      => [5, :left],
    "⊻"        => [5, :left], # xor alias
    "or"       => [5, :left],
    "∨"        => [5, :left], # or alias
    "nand"     => [5, :left],
    "⊼"        => [5, :left], # nand alias

    "->"       => [4, :left],
    "→"        => [4, :left], # -> alias

    "else"     => [3, :left],
    ":>"       => [3, :left],
    "↠"       => [3, :left], # :> alias
    "typeof"   => [3, :left],
    "parentof" => [3, :left],

    ":="       => [2, :right],
    "::="      => [2, :right],
    "≔"       => [2, :right],
    ".="       => [2, :right],
    "..="      => [2, :right],
    "@="       => [2, :right],

    ";;"       => [1, :left],
}
$PRECEDENCE_UNARY = Hash.new(99)
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
    $OP_QUOTE           => :op_quote,
    $OPERATOR           => :operator,
    $STATEMENT_SEP      => :statement_sep,
    $WORD               => :word,
    $COMMA              => :comma,
    $RAW_STRING         => :raw_string,
    $FORMAT_STRING_BEGIN => :format_string_begin,
    $STRING             => :string,
    $NUMBER             => :number,
    $ABSTRACT           => :abstract,
    $NAMED_FUNC_START   => :named_func_start,
    $FUNC_END           => :func_end,
    $REFERENCE          => :reference,
    $ABSTRACT_REFERENCE => :abstract_reference,
    $COUNTER_REFERENCE  => :counter_reference,
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
    :counter_reference,
    :string,
    :raw_string,
    :format_string,
    :format_string_bare,
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
# $TOKENIZER = Regexp.new($TYPES.keys.join("|"), "u")

class AtTokenizer
    def initialize(code)
        @code = code
        @i = 0
        @build = []
        @match = nil

        @line = 1
        @column = 1
    end

    def running?
        @i < @code.size
    end

    def advance(n = 1)
        n.times {
            if @code[@i] == "\n"
                @column = 0
                @line += 1
            end
            @column += 1
            @i += 1
        }
    end

    def has_ahead?(entity)
        if @code.index(entity, @i) == @i
            @match = @code.match(entity, @i).to_s
        else
            nil
        end
    end

    def read_until(entity, skipping=/^$/)
        build = ""
        loop {
            if has_ahead? skipping
                build += @match
                advance @match.size
            else
                break if has_ahead? entity
                build += @code[@i]
                advance 1
            end
        }
        build
    end

    def step(output)
        $TYPES.each { |re, type|
            next unless has_ahead? re
            token = Token.new(@match, type, @i, @line, @column)
            if type == :comment_open
                token.type = :comment

                depth = 1
                advance @match.size
                until depth.zero?
                    if has_ahead? $COMMENT_OPEN
                        depth += 1
                    elsif has_ahead? $COMMENT_CLOSE
                        depth -= 1
                    elsif !has_ahead?(/./m)
                        raise "no more characters while searching for end of comment"
                    end
                    token.raw += @match
                    advance @match.size
                end

            elsif type == :format_string_begin

                advance token.raw.size
                first_time = true
                loop {
                    # TODO: make an "empty" format string not "FORMAT_STRING_END"
                    token.raw += read_until(/#$FORMAT_STRING_INTERRUPT|#$FORMAT_STRING_END/, /""/)
                    token.raw += @match
                    advance @match.size
                    if $FORMAT_STRING_END === @match
                        token.type = first_time ? :format_string_bare : :format_string_end
                        break
                    end
                    inner = nil
                    collect = []
                    depth = 1
                    while running?
                        inner = step(collect)
                        depth += 1 if inner.raw == "{" || inner.raw == "${"
                        depth -= 1 if inner.raw == "}"
                        break if depth == 0
                    end
                    collect.pop
                    output << token
                    collect.each { |c| output << c }
                    token = inner
                    token.type = :format_string_continue
                    first_time = false
                }

            else
                advance @match.size
            end

            output << token unless output.nil?

            return token
        }
        raise "unhandled character at position #{@i}"
    end

    def run(output)
        step(output) while running?
    end
end

def tokenize(code)
    if Enumerable === code
        code.to_enum
    else
        tokenizer = AtTokenizer.new(code)
        Enumerator.new { |enum|
            tokenizer.run(enum)
        }
    end
end
