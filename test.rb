# perform tests of AtState.rb

require 'optparse'
require 'json'
require_relative 'AtState.rb'

def indent(text, width = 2)
    text.gsub(/^/m, " " * width)
end

def pind(*a)
    puts indent *a
end

def tokenize_test(text)
    tokenize(text).to_a.map { |e| e.map(&:to_s) }
end

options = {
    mode: [],
    generate: [],
}
filename = File.basename __FILE__
parser = OptionParser.new { |opts|
    opts.banner = "Usage: #{filename} [options]"
            
    opts.separator ""
    opts.separator "[options]"
    opts.separator "  test methods:"
    opts.on("-a", "--all", "Test all functions") { |v|
        options[:mode].push :library, :tokenizer, :state
    }
    opts.on("-l", "--lib", "Test the library functions") { |v|
        options[:mode] << :library
    }
    opts.on("-t", "--tokenize", "Test the tokenizer") { |v|
        options[:mode] << :tokenizer
    }
    opts.on("-s", "--state", "Test the state") { |v|
        options[:mode] << :state
    }
    opts.separator "  meta:"
    opts.on("-g", "--generate case", "Generate test cases") { |v|
        options[:generate] << v
    }
    opts.on("-S", "--show-tests", "Show all current tests") { |v|
        options[:show_tests] = v
    }
    opts.on("-i", "--interactive", "Enables interactivity") { |v|
        options[:interactive] = v
    }
    opts.separator "  other:"
    opts.on_tail("-h", "--help", "Prints this help message") { puts opts; exit }
}
parser.parse!

if ARGV.empty? && options[:mode].empty? && options.size == 2
    puts parser
    exit
end

$config = JSON::parse File.read "tests.json"

if options[:show_tests]
    $config.each { |test_name, data|
        cases = data["cases"]
        unless options[:interactive]
            puts "Tests for #{test_name}:"
        end
        cases.each { |c|
            if options[:interactive]
                cls
                puts "Tests for #{test_name}:"
            end
            puts "Input:"
            pind c["input"]
            puts "Output:"
            pind c["output"].map(&:inspect).join("\n")
            STDIN.gets if options[:interactive]
        }
    }
    exit
end

unless options[:generate].empty?
    method = options[:mode].first.to_s
    # exit
    options[:generate].each { |input|
        hash = {
            input: input,
            output: tokenize_test(input)
        }
        puts "Writing:"
        pind JSON::pretty_generate(hash)
        $config[method]["cases"] << hash
    }
    puts "Press ENTER to confirm."
    STDIN.gets
    File.write "tests.json", JSON::generate($config)
    puts "tests.json updated."
    exit
end
# generate tests
# method = "tokenizer"
# exit


options[:mode].uniq.each { |mode|
    mode = mode.to_s
    data = $config[mode]
    method = data["method"]
    cases = data["cases"]
    passed = 0
    cases.each.with_index { |hash, i|
        input = hash["input"]
        output = hash["output"]
        result = send(method, input).to_a
        if output.nil?
            puts "Incomplete test: in #{mode.inspect}: Test #{i}:"
            p [input, output]
            p result
            exit -2
        end
        if result != output
            puts "Failed test in #{mode.inspect}: Test #{i}:"
            puts "Input:"
            pind input
            puts "Expected output:"
            pind output
            puts "Received:"
            pind result
        else
            passed += 1
        end
    }
    print "[#{mode}] "
    if passed == cases.size
        puts "All #{cases.size} tests passed."
    else
        puts "#{passed * 100 / cases.size}% of tests passed."
    end
}