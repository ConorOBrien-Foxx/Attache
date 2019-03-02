require 'ruby-prof'
require_relative 'AtState.rb'

RubyProf.start
AtState.new ""
result = RubyProf.stop

# print a flat profile to text
printer = RubyProf::FlatPrinter.new(result)
printer.print(STDOUT)