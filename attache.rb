#!/usr/bin/ruby

require 'optparse'
require_relative 'AtState.rb'

class AttacheParser
    FILENAME = File.basename __FILE__
    
    def self.parse(args)
        options = {}
        # defaults
        #none
        
        parser = OptionParser.new { |opts|
            opts.program_name = FILENAME
            opts.banner = "Usage: #{FILENAME} [options]"
            
            opts.separator ""
            opts.separator "[options]"
            
            opts.on(
                "-d", "--debug",
                "Debug the program"
            ) do |v|
                options[:debug] = v
            end
            
            opts.on(
                "-t", "--tokenize",
                "Display the tokens of the input program"
            ) do |v|
                options[:tokenize] = v
            end
            
            opts.on(
                "-s", "--shunt",
                "Display the result of the shunted program"
            ) do |v|
                options[:shunt] = v
            end
            
            opts.on(
                "-p", "--program",
                "Display the program received by #{FILENAME}"
            ) do |v|
                options[:show_program] = v
            end
            
            opts.on(
                "-a", "--ast",
                "Use ast!"
            ) do |v|
                options[:ast] = v
            end
            
            opts.on(
                "-e", "--execute CODE",
                String,
                "Execute the following string as code",
            ) do |v|
                options[:program] = v
            end
            
            opts.on_tail(
                "-h", "--help",
                "Show this help message"
            ) do |v|
                puts opts
                exit
            end
        }
        parser.parse!(args)
        if options.empty? && args.empty?
            puts parser
            exit
        end
        options
    end
end

options = AttacheParser.parse(ARGV)

program = options[:program] || File.read(ARGV[0])

if options[:show_program]
    puts "[program]"
    puts program
end

if options[:ast]
    ast(program).each { |node| puts node.to_s }
end

if options[:tokenize] || options[:shunt]
    iter = {}
    iter[:tokenize] = :tokenize   if options[:tokenize]
    iter[:shunt]    = :parse      if options[:shunt]
    
    iter.each { |display, option|
        puts "[#{display}]"
        tokens = send option, program
        table = tokens.map.with_index { |(raw, type, index), i|
            [i.to_s, index.to_s, type.to_s, raw.inspect]
        }
        print_table table
    }
else
    inst = AtState.new program
    inst.run
end

# k.run