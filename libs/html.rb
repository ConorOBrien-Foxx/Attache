# html parser

class Regexp
    def |(other)
        /#{self}|#{other}/
    end
end

class HTMLParser
    SPACE = /[\x20\x09\x0A\x0C\x0D]/
    WHITE_SPACE = /\s/
    UPPERCASE_ASCII_CHARACTER = /[A-Z]/
    LOWERCASE_ASCII_CHARACTER = /[a-z]/
    ASCII_DIGIT = /[0-9]/
    
    ALPHANUMERIC_ASCII = UPPERCASE_ASCII | LOWERCASE_ASCII | ASCII_DIGIT
    ASCII_HEX_DIGIT = /[0-9A-Fa-f]/
    UPPERCASE_HEX_DIGIT = /[0-9A-F]/
    LOWERCASE_HEX_DIGIT = /[0-9a-f]/
    
    def HTMLParser.strip_line_breaks(string)
        string.gsub(/\r\n/, "")
    end
    
    # strip_leading_trailing_whitespace
    def HTMLParser.strip_whitespace(string)
        string.gsub(/^#{SPACE}+/, "")
              .gsub(/#{SPACE}+$/, "")
    end
    
    def HTMLParser.strip_collapse_whitespace(string)
        strip_whitespace string.gsub(/#{SPACE}+/, "\x20")
    end
    
    def HTMLParser.strict_split(input, delimiter)
        position = 0
        tokens = []
        
        #ehhh
        while position < input.size
            member = collect_characters delimiter, invert: true
            tokens.push member
            position += 1
        end
    end
    
    def initialize(input)
        @input = input
        @position = 0
    end
    
    def has_characters_left?
        @position < @input.size
    end
    
    def current
        @input[@position]
    end
    
    def advance
        @position += 1
    end
    
    def collect_characters(characters, invert: false)
        result = ""
        
        while has_characters_left? && characters.include?(current) != invert
            result += current
        end
        
        result
    end
    
    def skip_whitespace
        collect_characters SPACE
    end
end

