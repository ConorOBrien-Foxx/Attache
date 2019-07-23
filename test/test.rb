# perform tests of AtState.rb

require 'optparse'
require 'json'
require_relative '../src/AtState.rb'
require_relative 'directory.rb'

JSON_LOCATION = File.join TEST_LOCATION, "tests.json"

def indent(text, width = 2)
    text.gsub(/^/m, " " * width)
end

def pind(*a)
    puts indent *a
end

def tokenize_test(text)
    tokenize(text).to_a.map { |e| e.to_ary.map(&:to_s) }
end

def show_tokens(toks)
    toks.map(&:inspect).join("\n")
end

def safe_run(*args, &fn)
    result = {"error"=>"","output"=>""}
    begin
        result["output"] = fn[*args]
    rescue Exception => e
        result["error"] = e.to_s
    end
    result
end

def eval_test(text)
    safe_run(text) { |e| eval e }
end

def environment_test(program)
    safe_run(program) { |e|
        AtState.execute(e).last
    }
end

# allows for {a:NaN} == {a:NaN}
# todo: make more "safe"?
def same_rough(a, b)
    a == b || Marshal.dump(a) == Marshal.dump(b)
end

options = {
    mode: [],
    generate: [],
}
$default_size = options.size
filename = File.basename __FILE__
parser = OptionParser.new { |opts|
    opts.banner = "Usage: #{filename} [options]"

    opts.separator ""
    opts.separator "[options]"
    opts.separator "  test methods:"
    opts.on("-a", "--all", "Test all functions") { |v|
        options[:mode].push :library, :tokenizer, :environment
    }
    opts.on("-l", "--lib", "Test the library functions") { |v|
        options[:mode] << :library
    }
    opts.on("-t", "--tokenize", "Test the tokenizer") { |v|
        options[:mode] << :tokenizer
    }
    opts.on("-e", "--environment", "Test the environment") { |v|
        options[:mode] << :environment
    }
    opts.separator "  meta:"
    opts.on("-g", "--generate [case]", "Generate test cases") { |v|
        unless v.nil?
            options[:generate] ||= []
            options[:generate] << v
        end
        options[:generate_seen] = true
    }
    opts.on("-S", "--show-tests", "Show all current tests") { |v|
        options[:show_tests] = v
    }
    opts.on("-i", "--interactive", "Enables interactivity") { |v|
        options[:interactive] = v
    }
    opts.on("-G", "--gen-interact", "Equivalent to -gi") { |v|
        options[:generate_seen] = true
    }
    opts.separator "  other:"
    opts.on_tail("-h", "--help", "Prints this help message") { puts opts; exit }
}
parser.parse!

if ARGV.empty? && options[:mode].empty? && options.size == $default_size
    puts parser
    exit
end

$config = JSON::parse File.read(JSON_LOCATION), allow_nan: true
# deep clone hash
$old_config = Marshal.load Marshal.dump $config.clone

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
            pind show_tokens c["output"]
            STDIN.gets if options[:interactive]
        }
    }
    exit
end

if options[:generate_seen]
    unless options[:generate].empty?
        method = options[:mode].first.to_s
        # exit
        options[:generate].each { |input|
            hash = {
                input: input,
                output: tokenize_test(input)
            }
            if options[:interactive]
                puts "Writing:"
                pind JSON::pretty_generate(hash, allow_nan: true)
            end
            $config[method]["cases"] << hash
        }
        if options[:interactive]
            puts "Press ENTER to confirm."
            STDIN.gets
        end
        File.write JSON_LOCATION, JSON::generate($config, allow_nan: true)
        puts "tests.json updated."
    else
        # interactive modification
        if options[:mode].empty?
            puts "Current aspects:"
            $config.keys.each { |key| pind key }
            method = prompt_input "Which aspect would you like to modify? "
        else
            method = options[:mode].first.to_s
        end

        if $config[method].nil?
            opt = read_option "#{method} does not currently exist. Would you like to create it?", $YES_NO
            exit -3 if opt == "n"

            $config[method] ||= {}

            used_method = prompt_input "Which method does #{method} use? "

            $config[method]["method"] = used_method
            $config[method]["cases"] = []
        end
        data = $config[method]
        cases = data["cases"]
        i = 0

        loop {
            cls
            record = cases[i]
            title = "Record #{i + 1} of #{cases.size}"
            puts "=" * (title.size + 4)
            puts "= #{title} ="
            puts "=" * (title.size + 4)
            if record.nil?
                puts "<no record>"
            else
                puts "[input]"
                pind record["input"]
                puts "[output]"
                pind show_tokens record["output"]
            end
            action = read_option "What would you like to do?", {
                "c" => "create new record",
                "p" => "previous record",
                "n" => "next record",
                "f" => "first record",
                "l" => "last record",
                "j" => "jump to record number",
                "d" => "delete current record",
                "m" => "modify current record",
                "r" => "refresh output of current record",
                "s" => "stop",
            }
            case action
                when "p"
                    i -= 1
                when "f"
                    i = 0
                when "l"
                    i = cases.size - 1
                when "n"
                    i += 1
                when "j"
                    puts "Please enter the index number:"
                    i = gets.to_i
                when "d"
                    cases.delete_at i
                when "m", "c", "r"
                    i = cases.size if action == "c"

                    cases[i] ||= {}
                    if action == "r"
                        input = cases[i]["input"]
                    else
                        input = prompt_input "What should the input case be? "
                        cases[i]["input"] = input
                    end

                    output = send data["method"], input
                    puts "Received output and error:"
                    pind show_tokens output
                    puts
                    confirm = read_option "Does this make sense?", $YES_NO
                    if confirm == "y"
                        cases[i]["output"] = output
                    else
                        cases.delete_at i
                        prompt_input "Cancelled. Press ENTER to continue."
                    end
                when "s"
                    break
            end
        }

        data["cases"] = cases
        $config[method] = data

        # only write if necessary
        unless same_rough $old_config, $config
            begin
                File.write JSON_LOCATION, JSON::generate($config, allow_nan: true)
            rescue
                puts "Error writing JSON file. Perhaps malformed JSON?"
                # p $config
                p $config[method]
            end
        end
    end
    exit
end


options[:mode].uniq.each { |mode|
    mode = mode.to_s
    data = $config[mode]
    if data.nil?
        puts "Incomplete/nonexistant test data #{mode.inspect}"
        exit -3
    end
    method = data["method"]
    cases = data["cases"]
    passed = 0
    cases.each.with_index { |hash, i|
        input = hash["input"]
        output = hash["output"]
        result = send(method, input)

        if result.is_a? Enumerator
            result = result.to_a
        end

        if output.nil?
            puts "Incomplete test: in #{mode.inspect}: Test #{i}:"
            p [input, output]
            p result
            exit -2
        end
        unless same_rough result, output
            puts "Failed test in #{mode.inspect}: Test #{i}:"
            puts "Input:"
            pind input
            puts "Expected output:"
            pind show_tokens output
            puts "Received:"
            pind show_tokens result
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
