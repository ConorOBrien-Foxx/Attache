#!/usr/bin/env ruby

require 'optparse'
require_relative 'AtState.rb'

class AttacheParser
    FILENAME = File.basename __FILE__

    def self.parse(args)
        options = {}
        # defaults
        options[:needs] = []

        parser = OptionParser.new { |opts|
            opts.program_name = FILENAME
            opts.banner = "Usage: #{FILENAME} [options]"

            opts.separator ""
            opts.separator "[options]"

            opts.on(
                "-a", "--ast",
                "Display the AST parsed from the program"
            ) do |v|
                options[:ast] = v
            end

            opts.on(
                "-d", "--debug",
                "Debug the program"
            ) do |v|
                options[:debug] = v
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

            opts.on(
                "-i", "--STDIN [TYPE]",
                String,
                "Reads the program from STDIN, until [TYPE]"
            ) do |v=nil|
                options[:stdin] = v
            end

            opts.on("-f", "--fast",
                "Omits inclusion of std.@ for faster initialization"
            ) do |v|
                options[:fast] = v
            end

            opts.on("-m", "--time",
                "Times how long it takes to handle the program",
            ) do |v|
                options[:time] = v
            end

            opts.on("-n", "--needs LIB",
                String,
                "Equivalent to having `Needs[LIB]` at the beginning of the program."
            ) do |v|
                options[:needs] << v
            end

            opts.on(
                "-p", "--program",
                "Display the program received by #{FILENAME}"
            ) do |v|
                options[:show_program] = v
            end

            opts.on("-r", "--repl",
                "Engages the Attache repl",
            ) do |v|
                options[:repl] = v
            end

            opts.on(
                "-s", "--shunt",
                "Display the result of the shunted program"
            ) do |v|
                options[:shunt] = v
            end

            opts.on(
                "-t", "--tokenize",
                "Display the tokens of the input program"
            ) do |v|
                options[:tokenize] = v
            end

            opts.on("-H", "--highlight",
                "Performs syntax highlighting on the code"
            ) do |v|
                options[:highlight] = v
            end

            opts.on("-S", "--serve-templat",
                "Starts localhost:8000 with TemplAt code"
            ) do |v|
                options[:serve_templat] = v
            end

            opts.on("-T", "--templat",
                "Executes TemplAt code"
            ) do |v|
                options[:templat] = v
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

if options[:time]
    $start_time = Time.now
end

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

unless options[:needs].empty?
    program = options[:needs].map { |e| "Needs[#{e.inspect}];" }.join + program
end

if options[:highlight]
    inst = AtState.new "Needs[$visuals]"
    inst.run
    print inst.variables["Highlight"][inst, program]
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
        table = tokens.map.with_index { |token, i|
            [
                i.to_s,
                "#{token.line}:#{token.column} (#{token.start})",
                token.type.to_s,
                token.raw.to_s.inspect
            ]
        }
        print_table table
    }
else
    begin
        inst = AtState.new program, exclude_std: options[:fast]
        inst.run
    rescue AttacheError => e
        puts e.readable
    end
end

if options[:time]
    $end_time = Time.now
    puts "Time taken: #{$end_time - $start_time}"
end

# k.run
