require_relative 'color_inspect.rb'

class Token
    def initialize(raw=nil, type=nil, start=nil, line=nil, column=nil)
        @raw = raw
        @type = type
        @start = start
        @line = line
        @column = column
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

    def to_s(color: true)
        "#{@type} #{color_inspect(@raw, color: color)} @ #{@start}"
    end

    def position
        "#{line}:#{column}"
    end

    def inspect(color: true)
        (color ? "#\x1b[33mToken\x1b[0m<" : "#Token<") + to_ary.map { |item| color_inspect(item, color: color) }.join(", ") + ">"
    end
end

$WORD = /\G[[:alpha:]][[[:alpha:]]\w]*/
$WORD_NO_G = /[[:alpha:]][[[:alpha:]]\w]*/
$ABSTRACT = /\G_+\d*/
$NUMBER = /\G(?:(?:[0-9]*\.[0-9]+)|\G(?:[0-9]+))(?:i?x?|x?i?)/
$REFERENCE = /\G\$#$WORD_NO_G/
$COUNTER_REFERENCE = /\G\$\$?[0-9]+/
$ABSTRACT_REFERENCE = /\G\$+/
$BRACKET_OPEN = /\G\[|\Gdo\b/
$BRACKET_CLOSE = /\G\]|\Gend\b/
$PAREN_OPEN = /\G\(/
$PAREN_CLOSE = /\G\)/
$COMMA = /\G,/
$STRING = /\G"(?:[^"]|"")*"/
$STRING_NO_G = /"(?:[^"]|"")*"/
$RAW_STRING = /\G`#$STRING_NO_G/
$FORMAT_STRING_BEGIN = /\G\$"/
$FORMAT_STRING_END = /\G"/
$FORMAT_STRING_CONTINUE = /\G\}/
$FORMAT_STRING_INTERRUPT = /\G\$\{/
# $FORMAT_STRING = /\G\$#$STRING/
$FUNC_START = /\G\{/
$FUNC_START_NO_G = /\{/
$NAMED_FUNC_START = /\G\$#$FUNC_START_NO_G/
$FUNC_END = /\G\}/
$WHITESPACE = /\G\s+/
$UNKNOWN = /\G./
$COMMENT = /\G\?\?.*(?:\n|$)/
$COMMENT_OPEN = /\G\(\*/
$COMMENT_CLOSE = /\G\*\)/
$CURRY_OPEN = /\G<~|\G«/
$CURRY_CLOSE = /\G~>|\G»/
$STATEMENT_SEP = /\G;/

$PRECEDENCE = {
    "."        => [99999, :left],

    ":"        => [300, :left],
    "::"       => [300, :left],

    "`"        => [280, :left],

    "&"        => [260, :left],
    "&:"       => [260, :left],
    "~"        => [250, :left],
    "@"        => [240, :left],
    "@@"       => [240, :left],
    "@%"       => [240, :left],
    "&>"       => [200, :right],
    "=>"       => [200, :right],
    "@>"       => [200, :right],
    "⇒"       => [200, :right], # => alias
    "\\"       => [200, :right],
    "#"        => [200, :left],
    "''"       => [200, :left],
    "'"        => [200, :left],
    "##"       => [190, :left],



    "∩"        => [180, :left], # Intersection alias
    "∪"        => [170, :left], # Union alias
    "∆"        => [170, :left], # symmetric difference
    "Ø"        => [170, :left], # setwise difference
    "^^"       => [170, :left], # similar to the above
    "⩓"        => [170, :left], # ^^ alias

    "!"        => [160, :right],
    "^"        => [150, :right],
    "?"        => [150, :left],
    "*"        => [130, :left],
    "/"        => [130, :left],
    "//"       => [130, :left],
    "⁄"        => [130, :left], # // alias (fraction slash)
    "%"        => [130, :left],
    "|"        => [120, :left],
    "+"        => [110, :left],
    "-"        => [110, :left],
    "±"        => [110, :left],
    "<:"       => [100, :left],
    "↞"       => [100, :left],

    "="        => [90, :left],
    "=/="      => [90, :left],
    "/="       => [90, :left],
    "≠"        => [90, :left], # /= alias
    "=="       => [90, :left],
    "is"       => [90, :left], # == alias
    "<"        => [90, :left],
    ">"        => [90, :left],
    "<="       => [90, :left],
    "≤"        => [90, :left], # <= alias
    ">="       => [90, :left],
    "≥"        => [90, :left], # >= alias

    "in"       => [80, :left],
    "!in"      => [80, :left],
    "is_a"     => [80, :left],
    "is_an"    => [80, :left],

    ".."       => [70, :left],
    "‥"        => [70, :left], # .. alias
    "..."      => [70, :left],
    "…"        => [70, :left], # ... alias

    "|>"       => [65, :left],
    "▷"       => [65, :left], # |> alias
    "<|"       => [65, :right],
    "◁"       => [65, :left], # <| alias

    "!!"       => [60, :left],

    "and"      => [55, :left],
    "∧"        => [55, :left], # and alias
    "nor"      => [55, :left],
    "⊽"        => [55, :left], # nor alias
    "not"      => [55, :left],
    "xor"      => [50, :left],
    "⊻"        => [50, :left], # xor alias
    "or"       => [50, :left],
    "∨"        => [50, :left], # or alias
    "nand"     => [50, :left],
    "⊼"        => [50, :left], # nand alias

    "->"       => [40, :left],
    "→"        => [40, :left], # -> alias

    "else"     => [30, :left],
    ":>"       => [30, :left],
    "↠"       => [30, :left], # :> alias
    "typeof"   => [30, :left],
    "parentof" => [30, :left],

    ":="       => [20, :right],
    "::="      => [20, :right],
    "≔"       => [20, :right],
    ".="       => [20, :right],
    "..="      => [20, :right],
    "@="       => [20, :right],

    ";;"       => [10, :left],
}
$PRECEDENCE_UNARY = Hash.new(999)
$PRECEDENCE_UNARY["..."] = 0
$PRECEDENCE_UNARY["…"] = 0

$operators = $PRECEDENCE.keys.sort { |x, y| y.size <=> x.size }
$OPERATOR = Regexp.new($operators.map { |e|
    if /^\w+$/ === e
        '\\G' + "#{e}\\b"
    else
        '\\G' + Regexp.escape(e)
    end
}.join "|")
$OPERATOR_NO_G = Regexp.new($operators.map { |e|
    if /^\w+$/ === e
        "#{e}\\b"
    else
        Regexp.escape e
    end
}.join "|")
$OP_QUOTE = /\G`#$OPERATOR_NO_G/
$TYPES = {
    $COMMENT            => :comment,
    $COMMENT_OPEN       => :comment_open,
    $COMMENT_CLOSE      => :comment_close,
    $CURRY_OPEN         => :curry_open,
    $CURRY_CLOSE        => :curry_close,
    $BRACKET_OPEN       => :bracket_open,
    $BRACKET_CLOSE      => :bracket_close,
    $OP_QUOTE           => :op_quote,
    $RAW_STRING         => :raw_string,
    $OPERATOR           => :operator,
    $STATEMENT_SEP      => :statement_sep,
    $WORD               => :word,
    $COMMA              => :comma,
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
        s = @code[@i, n]
        lines = s.count("\n")
        @line += lines
        if lines > 0
            @column = s.match(/^.*\Z/).size
        else
            @column += s.size
        end
        @i += n
    end

    def has_ahead?(entity)
        if entity.is_a? Regexp
            match = @code.match(entity, @i)
            if match
                @match = match.to_s
            else
                nil
            end
        else
            if @code[@i, entity.size] == entity
                @match = entity
            else
                nil
            end
        end
    end

    def read_until(entity, skipping=/\G^$/)
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
                    elsif !has_ahead?(/\G./m)
                        raise "no more characters while searching for end of comment"
                    end
                    token.raw += @match
                    advance @match.size
                end

            elsif type == :format_string_begin

                advance token.raw.size
                first_time = true
                loop {
                    # TODO: make an  format string not "FORMAT_STRING_END"
                    token.raw += read_until(/\G#$FORMAT_STRING_INTERRUPT|\G#$FORMAT_STRING_END/, /""/)
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
    tokenizer = AtTokenizer.new(code)
    Enumerator.new { |enum|
        tokenizer.run(enum)
    }
end
