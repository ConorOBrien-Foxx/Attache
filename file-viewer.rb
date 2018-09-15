#!/usr/bin/ruby

# file =

require 'io/console'
def getchar(safe=true)
    res = STDIN.getch
    exit -42 if res == "\x03" if safe
    res
end

mod = 10

file = File.open(ARGV[0])

file.each_line.with_index { |line, i|
    print line
    getchar if i % mod == mod - 1
}
