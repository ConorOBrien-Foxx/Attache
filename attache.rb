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
                "-i", "--STDIN [TYPE]",
                String,
                "Reads the program from STDIN, until [TYPE]"
            ) do |v=nil|
                options[:stdin] = v
            end

            opts.on(
                "-p", "--program",
                "Display the program received by #{FILENAME}"
            ) do |v|
                options[:show_program] = v
            end

            opts.on(
                "-a", "--ast",
                "Display the AST parsed from the program"
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

            opts.on("-r", "--repl",
                "Engages the Attache repl",
            ) do |v|
                options[:repl] = v
            end

            opts.on("-H", "--highlight",
                "Performs syntax highlighting on the code"
            ) do |v|
                options[:highlight] = v
            end

            opts.on("-T", "--templat",
                "Executes TemplAt code"
            ) do |v|
                options[:templat] = v
            end

            opts.on("-S", "--serve-templat",
                "Starts localhost:8000 with TemplAt code"
            ) do |v|
                options[:serve_templat] = v
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

def read_program(options)
    if options.has_key? :stdin
        STDIN.gets options[:stdin]
    else
        options[:program] || File.read(ARGV[0], encoding: "UTF-8") rescue ""
    end
end

program = read_program(options)

if options[:serve_templat]
    require_relative 'TemplAt.rb'
    inst = AtState.new "Needs[$socket]\nServeTemplat[get_code]"
    inst.variables['get_code'] = lambda { |inst|
        read_program options
    }
    begin
        inst.run
    rescue
        STDERR.puts "An error occured opening localhost."
        print templat(program).to_html
    end
elsif options[:templat]
    require_relative 'TemplAt.rb'
    print templat(program).to_html
    exit
end

if options[:repl]
    program += 'Needs["repl"]; REPL[]'
end

if options[:highlight]
    inst = AtState.new "Needs[$visuals]"
    inst.run
    print inst.variables["highlight"][inst, program]
    exit
end

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
