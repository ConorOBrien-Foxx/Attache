# attache module

require_relative '../AtState.rb'

AtState.function("Tokenize") { |inst, text|
    tokenize(text).map { |tok|
        a, b, c = tok
        Token.new a, b.to_s, c
    }
}