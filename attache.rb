#!/usr/bin/ruby

require 'prime'

class Train
    def initialize(*a)
        @train = a
    end
    
    def append(*args)
        Train.new *@tran, *args
    end
    
    def <<(arg)
        @train.concat arg
    end
    
    def get_func
        lambda { |inst, *args|
            case @train.size
                #todo: expand
                when 3
                    f, g, h = @train
                    g[inst, f[inst, *args], h[inst, *args]]
                when 2
                    f, g = @train
                    f[inst, args.first, g[inst, *args]]
                else
                    STDERR.puts "Invalid train size #{@train.size}"
                    raise
            end
        }
    end
    
    def [](inst, *args)
        get_func[inst, *args]
    end
    
    def to_a
        @train
    end
end

$WORD = /[A-Za-z]\w*/
$ABSTRACT = /_\d*/
$NUMBER = /[0-9]+/
$REFERENCE = /\$#$WORD/
$BRACKET_OPEN = /\[/
$BRACKET_CLOSE = /\]/
$PAREN_OPEN = /\(/
$PAREN_CLOSE = /\)/
$COMMA = /,/
$STRING = /"(?:[^"]|"")*"/
$FUNC_START = /\{/
$FUNC_END = /\}/
$PRECEDENCE = {
    "&"  => [26, :left],
    "@"  => [24, :left],
    "@@" => [24, :left],
    "=>" => [20, :right],
    "\\" => [20, :right],
    "#"  => [20, :left],
    
    "^"  => [15, :right],
    "*"  => [13, :left],
    "/"  => [13, :left],
    "+"  => [12, :left],
    "-"  => [12, :left],
    
    "="  => [7, :left],
    "<"  => [7, :left],
    ">"  => [7, :left],
    "<=" => [7, :left],
    ">=" => [7, :left],
    ".." => [5, :left],
}
$operators = $PRECEDENCE.keys.sort { |x, y| y.size <=> x.size }
$OPERATOR = Regexp.new($operators.map { |e| Regexp.escape e }.join "|")
$OP_QUOTE = /`#$OPERATOR/
$TYPES = {
    $WORD               => :word,
    $COMMA              => :comma,
    $STRING             => :string,
    $NUMBER             => :number,
    $ABSTRACT           => :abstract,
    $OPERATOR           => :operator,
    $OP_QUOTE           => :op_quote,
    $FUNC_END           => :func_end,
    $REFERENCE          => :reference,
    $FUNC_START         => :func_start,
    $PAREN_OPEN         => :paren_open,
    $PAREN_CLOSE        => :paren_close,
    $BRACKET_OPEN       => :bracket_open,
    $BRACKET_CLOSE      => :bracket_close,
}
$DATA = [
    :word,
    :number,
    :reference,
    :string,
    :op_quote,
    :call_func,
    :function,
    :abstract,
]
$TOKENIZER = Regexp.new($TYPES.keys.join "|")

def tokens(code)
    Enumerator.new { |enum|
        code.scan($TOKENIZER) { |part|
            $TYPES.each { |k, v|
                if /^#{k}$/ === part
                    enum.yield [part, v]
                end
            }
        }
    }
end

def debug_obj(title,**obj)
    puts "[#{title}]"
    obj.each{|v,k|puts"  #{v} -> #{k}"}
end
def parse(code)
    # group expression
    stack = []
    out = []
    arities = []
    last_token = nil
    tokens(code).each { |ent|
        # puts "#{out.map{|e|e[0]}} | #{stack.map{|e|e[0]}}"
        raw, type = ent
        # puts "RAW = #{raw.inspect}; TYPE = #{type.}"
        if $DATA.include? type
            # puts type
            # puts "  STACK: #{stack}\n  OUT: #{out}"
            out.push ent
            # puts "  STACK: #{stack}\n  OUT: #{out}"
        elsif type == :func_start
            stack.push ent
            out.push ent
        elsif type == :func_end
            out.push stack.pop while stack.last && [:operator, :unary_operator].include?(stack.last[1])
            collect = []
            collect.unshift out.pop until out.empty? || out.last[1] == :func_start
            stack.pop
            out.pop
            out.push [collect, :make_lambda]
        
        elsif type == :operator
            # puts type
            # puts "  STACK: #{stack}\n  OUT: #{out}"
            if last_token.nil? || !($DATA + [:bracket_close, :paren_close]).include?(last_token[1])
                ent[1] = :unary_operator
            else
                cur_prec, cur_assoc = $PRECEDENCE[raw]
                # NOTE: precedence determining
                loop {
                    break if stack.empty?
                    top_raw, top_type = stack.last
                    break if top_type != :operator && top_type != :unary_operator
                    top_prec, top_assoc = $PRECEDENCE[top_raw]
                    break if top_assoc == :right ? top_prec <= cur_prec : top_prec < cur_prec
                    # break if top_prec <= cur_prec 
                    out.push stack.pop
                }
            end
            stack.push ent
            
            # puts "  STACK: #{stack}\n  OUT: #{out}"
        elsif type == :bracket_open
            # top = out.pop
            # top[1] = :function
            stack.push ent
            # stack.push top
            arities.push 1
        elsif type == :comma
            arities[-1] += 1
            out.push stack.pop while stack.last && [:operator, :unary_operator].include?(stack.last[1])
        elsif type == :bracket_close
            arities[-1] = 0 if last_token[1] == :bracket_open
            out.push stack.pop while stack.last[1] != :bracket_open
            out.push [arities.pop, :call_func]
            stack.pop
        
        elsif type == :paren_open
            stack.push ent
        elsif type == :paren_close
            # puts type
            # puts "  STACK: #{stack}\n  OUT: #{out}"
            out.push stack.pop while stack.last[1] != :paren_open
            stack.pop
            # puts "  STACK: #{stack}\n  OUT: #{out}"
        else
            STDERR.puts "Unknown type #{type.inspect} during parsing"
            raise
        end
        last_token = ent
    }
    out.push stack.pop until stack.empty?
    out
end

$ALPHA_UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
$ALPHA_LOWER = "abcdefghijklmnopqrstuvwxyz"

def rotN(str, n)
    str.gsub(/[a-z]/i) { |letter|
        res = $ALPHA_LOWER[($ALPHA_LOWER.index(letter.downcase) + n) % 26]
        letter == letter.upcase ? res.upcase : res
    }
end

# modified from https://github.com/sagivo/powerset/blob/master/powerset.rb
def powerset(arr)
    a = [[]] 
    (0...arr.size).each { |i|
        len = a.size; j = 0;
        while j < len
            a << (a[j] + [arr[i]])
            j += 1
        end
    }
    a
end

def get_abstract_number(abstract)
    /\d+/ === abstract
    $& ? $&.to_i - 1 : 0
end

def gonal(n, s)
    if n.is_a? Array
        n.map { |e| gonal(e, s) }
    else
        (n * n * (s - 2) - n * (s - 4)) / 2
    end
end

def vectorize_monad(&fn)
    lambda { |inst, x|
        if x.is_a? Array
            x.map { |e| fn[inst, e] }
        else
            fn[inst, x]
        end
    }
end

LEFT  = 0b01
RIGHT = 0b10

def vectorize_dyad(lr = LEFT | RIGHT, &fn)
    left  = lr & LEFT  != 0
    right = lr & RIGHT != 0
    lambda { |inst, *args|
        x, y = args
        use_left = x.is_a?(Array) && left
        use_right = y.is_a?(Array) && right
        if args.size == 1
            if use_left
                x.map { |e| fn[inst, e] }
            else
                fn[inst, x]
            end
        elsif use_left
            if use_right
                x.map.with_index { |e, i| fn[inst, e, y[i]] }
            else
                x.map { |e| fn[inst, e, y] }
            end
        elsif use_right
            y.map { |e| fn[inst, x, e] }
        else
            fn[inst, x, y]
        end
    }
end

def prefixes(list)
    (0..list.size).map{ |i| list[0...i] }
end

def sum(list)
    list.inject(0, :+)
end

$NTH_PRIME_CACHE = [nil, 2, 3, 5, 7, 11, 13, 17, 19]
def nth_prime(n)
    if n < $NTH_PRIME_CACHE.size
        $NTH_PRIME_CACHE[n]
    elsif n >= 6
        n += 2
        upperbound = n * (Math.log(n) + Math.log(Math.log(n))).to_i
        n -= 2
        candidates = Prime.first(upperbound)
        candidates.each_with_index { |k, i|
            i += 1
            unless i < $NTH_PRIME_CACHE.size
                $NTH_PRIME_CACHE[i] = k
            end
        }
        $NTH_PRIME_CACHE[n]
    end
end

class AtState
    @@functions = {
        "Define" => lambda { |inst, *args|
            inst.define *args
        },
        "Print" => lambda { |inst, *args|
            puts args.map(&:to_s).join(" ")
        },
        "Add" => lambda { |inst, *args|
            sum args
        },
        "Count" => lambda { |inst, list, f|
            if f.is_a? Proc
                list.count { |e| f[inst, e] }
            else
                list.count f
            end
        },
        "Even" => vectorize_monad { |inst, n| n.even? },
        "Odd" => vectorize_monad { |inst, n| n.odd? },
        "Greater" => vectorize_dyad { |inst, x, y| x > y },
        "Abs" => vectorize_monad { |inst, n| n.abs },
        "Square" => vectorize_monad { |inst, n| n * n },
        "Sqrt" => vectorize_monad { |inst, n| Math.sqrt n },
        "Map" => lambda { |inst, f, list|
            list.map { |e| f[inst, e] }
        },
        "MaxBy" => lambda { |inst, f, list|
            list.max { |e| f[inst, e] }
        },
        "MinBy" => lambda { |inst, f, list|
            list.min { |e| f[inst, e] }
        },
        "Polygonal" => lambda { |inst, n, order=3|
            gonal n, order
        },
        "Triangular" => lambda { |inst, n| gonal n, 3 },
        
        "Sum" => lambda { |inst, list| sum list },
        "Max" => lambda { |inst, *args| args.flatten.max },
        "Min" => lambda { |inst, *args| args.flatten.min },
        "Flat" => lambda { |inst, list, n=nil| list.flatten(n) },
        "Powerset" => lambda { |inst, list| powerset list },
        "V" => lambda { |inst, *args| args },
        "C" => lambda { |inst, arg| lambda { |inst, *discard| arg } },
        "Range" => lambda { |inst, min, max=nil|
            if max.nil?
                (0..min).to_a
            else
                (min..max).to_a
            end
        },
        "Rot" => lambda { |inst, str, amount=13|
            rotN(str, amount)
        },
        "Reverse" => lambda { |inst, ent| ent.reverse },
        "IsPrime" => vectorize_monad { |inst, n|
            Prime.prime? n
        },
        "Prime" => vectorize_monad { |inst, n|
            nth_prime n
        },
        "Primes" => vectorize_monad { |inst, n|
            Prime.first n
        },
        "Call" => lambda { |inst, f, *args|
            f[inst, *args]
        },
        "Same" => lambda { |inst, *args|
            list = args.flatten
            list.all? { |e| e == list[0] }
        },
        "Get" => vectorize_dyad(RIGHT) { |inst, list, inds|
            # if inds.is_a? Array
                # inds.map { |e| list[e] }
            # else
                list[inds]
            # end
        },
        "Bond" => lambda { |inst, func, rarg|
            lambda { |inst, *args| func[inst, *args, rarg] }
        },
        "Select" => lambda { |inst, f, list|
            # p [f,list]
            list.select { |e| f[inst, e] }
        },
        "Size" => lambda { |inst, list| list.size },
        "Fork" => lambda { |inst, f, g, h|
            lambda { |inst, *args| g[inst, f[inst, *args], h[inst, *args]] }
        },
        "Slices" => vectorize_dyad(RIGHT) { |inst, list, skew|
            list.each_cons(skew).to_a
        },
        "Outers" => vectorize_dyad(RIGHT) { |inst, arr, n=1|
            arr[0...n] + arr[-n..-1]
        },
        "Prefixes" => lambda { |inst, list|
            prefixes list
        },
        "Accumulate" => lambda { |inst, list|
            prefixes(list)[1..-1].map { |e| sum e }
        },
        "Series" => lambda { |inst, f, max|
            i = 0
            collect = []
            loop {
                value = f[inst, i]
                break if value >= max
                collect.push value
                i += 1
            }
            collect
        },
        "Format" => lambda { |inst, str, *args| str % args },
        "Last" => lambda { |inst, list| list.last },
        "First" => lambda { |inst, list| list.first },
        "Stdin" => lambda { |inst| STDIN.read },
        "Stdout" => lambda { |inst, *args| print args.flatten.join },
    }
    @@operators = {
        "*" => vectorize_dyad { |inst, a, b| a * b },
        "/" => vectorize_dyad { |inst, a, b| a * 1.0 / b },
        "-" => vectorize_dyad { |inst, a, b| a - b },
        "+" => vectorize_dyad { |inst, a, b| a + b },
        "^" => vectorize_dyad { |inst, a, b| a ** b },
        "@" => lambda { |inst, f, g|
            # p f,g
            lambda { |inst, *args| f[inst, g[inst, *args]] }
        },
        "@@" => lambda { |inst, f, g|
            lambda { |inst, *args| f[inst, *g[inst, *args]] }
        },
        "=" => vectorize_dyad { |inst, x, y| x == y },
        ">" => vectorize_dyad { |inst, x, y| x > y },
        "<" => vectorize_dyad { |inst, x, y| x < y },
        ">=" => vectorize_dyad { |inst, x, y| x >= y },
        "<=" => vectorize_dyad { |inst, x, y| x <= y },
        ".." => lambda { |inst, x, y| (x..y).to_a },
        "#" => lambda { |inst, x, y|
            Train.new *x, *y
        },
        "&" => lambda { |inst, a, b|
            if a.is_a? Proc
                lambda { |inst, *args| a[inst, *args, b] }
            elsif b.is_a? Proc
                lambda { |inst, *args| b[inst, a, *args] }
            else
                STDERR.puts "idk"
                raise
            end
        },
        "=>" => @@functions["Map"],
        "\\" => @@functions["Select"],
    }
    @@unary_operators = {
        "-" => lambda { |inst, n| -n },
        "#" => lambda { |inst, n| n.size },
    }
    def initialize(program)
        @tokens = parse(program)
        @variables = {}
        @stack = []
        @last_values = []
        @i = 0
    end
    attr_reader :stack, :last_values
    def get_value(obj)
        # p obj
        raw, type = obj
        if type == :reference
            raw
        elsif type == :save_last_value
            @last_values.push @stack.pop
        elsif type == :abstract
            number = @last_values.size - get_abstract_number(raw) - 1
            if number < 0
                raise "no value to obtain"
            end
            p @last_values
            @last_values[number]
        elsif type == :string
            raw[1..-2].gsub(/""/, '"')
        elsif @variables.has_key? raw
            @variables[raw]
        elsif @@functions.has_key? raw
            @@functions[raw]
        elsif type == :op_quote
            ref = raw[1..-1]
            lambda { |inst, *args|
                source = args.size == 1 ? @@unary_operators : @@operators
                source[ref][inst, *args]
            }
        else
            eval(obj[0].to_s)
        end
    end
    
    def define(name, value)
        /\w+/ === name
        @variables[$&] = value
    end
    
    # modifies stack
    def exec_op(tok)
        raw, type = tok
        # puts "tok = #{tok}"
        @stack << case type
            when :call_func
                args = stack.pop(raw)
                func = @stack.pop
                func[self, *args]
                
            when :make_lambda
                lambda { |inst, *args|
                    raw.each { |arg|
                        arg_raw, arg_type = arg
                        if arg_type == :abstract
                            number = get_abstract_number(arg_raw)
                            inst.exec_op [args[number], :normal]
                        else
                            # p inst, arg
                            inst.exec_op arg
                        end
                    }
                    inst.stack.pop
                }
            when :operator
                ref = @@operators[raw]
                if ref.nil?
                    STDERR.puts "Invalid operator #{raw.inspect}"
                    raise
                end
                args = @stack.pop(2)
                ref[self, *args]
            when :unary_operator
                ref = @@unary_operators[raw]
                arg = @stack.pop
                if ref.nil?
                    STDERR.puts "Invalid unary operator #{raw.inspect}"
                    raise
                end
                ref[self, arg]
            when *$DATA
                # p @stack
                get_value(tok)
            when :normal
                raw
            else
                STDERR.puts "Unknown type #{type.inspect}"
                raise
        end
    end
    
    def step
        tok = @tokens[@i]
        exec_op tok
        @i += 1
    end
    
    def run
        step while @i < @tokens.size
    end
end

if ARGV.size == 0
    STDERR.puts "Usage: #{File.basename __FILE__} <filename>"
    exit 2
end
program = File.read ARGV[0]

if ARGV.size > 1
    a=parse(program)
    puts
    a.each { |e| p e }
    puts
    exit if ARGV.size > 2
end

inst = AtState.new program
inst.run
# k.run