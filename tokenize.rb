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
$RAW_STRING = /`#$STRING/
$FORMAT_STRING_BEGIN = /\$".*?\$\{/m
$FORMAT_STRING = /\$#$STRING/
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
    "."        => [99, :left],

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
    $OP_QUOTE           => :op_quote,
    $OPERATOR           => :operator,
    $STATEMENT_SEP      => :statement_sep,
    $WORD               => :word,
    $COMMA              => :comma,
    $RAW_STRING         => :raw_string,
    $FORMAT_STRING_BEGIN => :format_string_begin,
    $FORMAT_STRING      => :format_string,
    $STRING             => :string,
    $NUMBER             => :number,
    $ABSTRACT           => :abstract,
    $NAMED_FUNC_START   => :named_func_start,
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
    :raw_string,
    :format_string,
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
        build_type = nil
        code = code.encode("UTF-8")
        code.scan($TOKENIZER) { |part|
            $TYPES.each { |reg, type|
                next unless /^#{reg}$/ === part

                if depth.nil?
                    if type == :comment_open
                        depth = 1
                        build = part
                        build_type = type
                    else
                        enum.yield Token.new part, type, i
                        i += part.size
                    end
                elsif build_type == :comment_open
                    build += part
                    depth += 1 if type == :comment_open
                    depth -= 1 if type == :comment_close
                    if depth.zero?
                        depth = nil
                        enum.yield Token.new build, :comment, i
                    end
                    i += part.size
                else
                    raise "unimplemented"
                end

                break
            }
        }
    }
end
