#/usr/bin/env ruby

begin
    require 'ruby-prof'
rescue LoadError
    puts "Profiling requires ruby-prof to be installed"
    puts "Use `gem install ruby-prof` to install"
    exit
end
require_relative 'AtState.rb'

RubyProf.start
AtState.new ""
result = RubyProf.stop

# print a flat profile to text
printer = RubyProf::FlatPrinter.new(result)
printer.print(STDOUT)