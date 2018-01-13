# perform tests of AtState.rb

require 'optparse'
require_relative 'AtState.rb'

options = {
    mode: []
}
filename = File.basename __FILE__
parser = OptionParser.new { |opts|
    opts.banner = "Usage: #{filename} [options]"
            
    opts.separator ""
    opts.separator "[options]"
    opts.separator "  test methods:"
    opts.on("-a", "--all", "Test all functions") { |v|
        options[:mode].push :library, :parser, :state
    }
    opts.on("-l", "--lib", "Test the library functions") { |v|
        options[:mode] << :library
    }
    opts.on("-p", "--parser", "Test the parser") { |v|
        options[:mode] << :parser
    }
    opts.on("-s", "--state", "Test the state") { |v|
        options[:mode] << :state
    }
    opts.separator "  other:"
    opts.on_tail("-h", "--help", "Prints this help message") { puts opts; exit }
}
parser.parse!

if ARGV.empty? && options[:mode].empty?
    puts parser
    exit
end

options[:mode].uniq.each { |mode|
    
}