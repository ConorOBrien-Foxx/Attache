require 'zlib'
require_relative 'lib.rb'

def zlib_deflate(str)
    deflated = Zlib::Deflate.deflate(str, 9)
    deflated[2..-5]
end

INFLATION = Zlib::Inflate.new(-8)
def zlib_inflate(str)
    INFLATION.inflate(str)
end

$words = nil
def load_words
    return if $words
    $words = File.read("information/dictionary.txt").lines.map(&:chomp)
    $words = $words.flat_map { |word|
        [word, word.upcase, word.downcase, word[0].upcase + word[1..-1].downcase].uniq
    }
    nil
end

