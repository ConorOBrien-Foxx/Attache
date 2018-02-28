# attache module

require_relative '../AtState.rb'

AtState.function("Tokenize") { |inst, text|
    tokenize(text).to_a
}