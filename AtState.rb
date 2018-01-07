require 'prime'

def print_table(table)
    max_sizes = table.transpose.map { |column|
        column.map(&:size).max
    }
    padding = 4
    table.each { |row|
        row.each_with_index { |e, i|
            e = e.ljust(max_sizes[i] + padding) if i + 1 < row.size
            print e
        }
        puts
    }
    nil
end

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
$WHITESPACE = /\s+/
$UNKNOWN = /./
$PRECEDENCE = {
    "&"     => [26, :left],
    "&:"     => [26, :left],
    "@"     => [24, :left],
    "@@"    => [24, :left],
    "=>"    => [20, :right],
    "\\"    => [20, :right],
    "#"     => [20, :left],
    
    "^"     => [15, :right],
    "!"     => [15, :left],
    "*"     => [13, :left],
    "/"     => [13, :left],
    "|"     => [12, :left],
    "+"     => [11, :left],
    "-"     => [11, :left],
    
    "="     => [7, :left],
    "<"     => [7, :left],
    ">"     => [7, :left],
    "<="    => [7, :left],
    ">="    => [7, :left],
    ".."    => [5, :left],
    "..."   => [5, :left],
    "and"   => [4, :left],
    "or"    => [3, :left],
}
$operators = $PRECEDENCE.keys.sort { |x, y| y.size <=> x.size }
$OPERATOR = Regexp.new($operators.map { |e| Regexp.escape e }.join "|")
$OP_QUOTE = /`#$OPERATOR/
$TYPES = {
    $OPERATOR           => :operator,
    $WORD               => :word,
    $COMMA              => :comma,
    $STRING             => :string,
    $NUMBER             => :number,
    $ABSTRACT           => :abstract,
    $OP_QUOTE           => :op_quote,
    $FUNC_END           => :func_end,
    $REFERENCE          => :reference,
    $FUNC_START         => :func_start,
    $WHITESPACE         => :whitespace,
    $PAREN_OPEN         => :paren_open,
    $PAREN_CLOSE        => :paren_close,
    $BRACKET_OPEN       => :bracket_open,
    $BRACKET_CLOSE      => :bracket_close,
    $UNKNOWN            => :unknown,
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

def tokenize(code)
    Enumerator.new { |enum|
        code.scan($TOKENIZER) { |part|
            $TYPES.each { |k, v|
                if /^#{k}$/ === part
                    enum.yield [part, v]
                    break
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
    tokenize(code).each { |ent|
        # puts "#{out.map{|e|e[0]}} | #{stack.map{|e|e[0]}}"
        raw, type = ent
        # puts "RAW = #{raw.inspect}; TYPE = #{type.}"
        if $DATA.include? type
            out.push ent
        
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
            if last_token.nil? || !($DATA + [:bracket_close, :paren_close, :func_end]).include?(last_token[1])
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
        elsif type == :whitespace
            # do nothing
        else
            STDERR.puts "Unknown type #{type.inspect} (#{raw.inspect}) during shunting"
            raise
        end
        last_token = ent if type != :whitespace
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

def factorial(n)
    prod 1..n
end

def vectorize_monad(&fn)
    res = lambda { |inst, x|
        if x.is_a? Array
            x.map { |e| res[inst, e] }
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
    res = lambda { |inst, *args|
        x, y = args
        use_left = x.is_a?(Array) && left
        use_right = y.is_a?(Array) && right
        if args.size == 1
            if use_left
                x.map { |e| res[inst, e] }
            else
                res[inst, x]
            end
        elsif use_left
            if use_right
                x.map.with_index { |e, i| res[inst, e, y[i]] }
            else
                x.map { |e| res[inst, e, y] }
            end
        elsif use_right
            y.map { |e| res[inst, x, e] }
        else
            fn[inst, x, y]
        end
    }
end

def vectorize(&fn)
    monadic = vectorize_monad { |*a| fn[*a] }
    dyadic  = vectorize_dyad  { |*a| fn[*a] }
    lambda { |inst, *args|
        if args.size == 1
            monadic[inst, *args]
        else
            dyadic[inst, *args]
        end
    }
end

def prefixes(list)
    (1..list.size).map{ |i| list[0...i] }
end

def sum(list)
    list.inject(0, :+)
end

def prod(list)
    list.inject(1, :*)
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

$FIBONACCI_CACHE = [0, 1, 1, 2, 3, 5, 8, 13, 21]
def nth_fibonacci(n)
    if n < $FIBONACCI_CACHE.size
        $FIBONACCI_CACHE[n]
    else
        $FIBONACCI_CACHE[n] = nth_fibonacci(n - 1) + nth_fibonacci(n - 2)
    end
end

def prime_factors(n)
    Prime.prime_division(n).flat_map { |factor, amount|
        [factor] * amount
    }
end

def sign(n)
    n <=> 0
end

def reverse(n)
    if n.is_a? Integer
        n.abs.to_s.reverse.to_i * sign(n)
    else
        n.reverse
    end
end

def lcm(arr)
    arr.reduce(1, :lcm)
end

def gcd(arr)
    arr.reduce(1, :gcd)
end

def from_numlike(n)
    return 0 if n == false
    return 1 if n == true
    return n
end

def simplify_number(n)
    return n.to_i if n == n.to_i
    return n
end

def force_number(n)
    simplify_number case n
        when Array
            n.join.to_f
        when String
            n.to_f
        when Numeric
            n
        else
            n
    end
end

def force_list(list)
    return list         if list.is_a? Array
    return list.chars   if list.is_a? String
    return list.digits  if list.is_a? Numeric
end

def slices(list, skew)
    force_list(list).each_cons(skew).to_a
end

class AtState
    def AtState.truthy?(ent)
        ent && ent != 0 && (ent.size != 0 rescue true)
    end
    def AtState.falsey?(ent)
        !AtState.truthy? ent
    end
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
        "LCM" => lambda { |inst, *args|
            lcm args.flatten
        },
        "GCD" => lambda { |inst, *args|
            gcd args.flatten
        },
        "Nest" => lambda { |inst, f, e, n|
            from_numlike(n).times {
                e = f[inst, e]
            }
            e
        },
        "If" => lambda { |inst, cond, t, f|
            if AtState.truthy? cond
                t
            else
                f
            end
        },
        "All" => lambda { |inst, f, list=nil|
            if list.nil?
                f.all? { |e| AtState.truthy? e }
            else
                list.all? { |e| AtState.truthy?(f[inst, e]) }
            end
        },
        "Count" => lambda { |inst, list, f|
            if f.is_a? Proc
                list.count { |e| f[inst, e] }
            else
                list.count f
            end
        },
        "N" => lambda { |inst, n| force_number n },
        "Chars" => vectorize_monad { |inst, n| n.chars },
        "Sign" => vectorize_monad { |inst, n| sign n },
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
        "Fibonacci" => lambda { |inst, n| nth_fibonacci(n) },
        "Sum" => lambda { |inst, list| sum list },
        "Prod" => lambda { |inst, list| prod list },
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
        "Unique" => lambda { |inst, a| a.uniq },
        "Id" => lambda { |inst, a| a },
        "Rot" => lambda { |inst, str, amount=13|
            rotN(str, amount)
        },
        "Outer" => lambda { |inst, f, a, b|
            a.product(b).map { |e| f[inst, *e] }
        },
        "Reverse" => lambda { |inst, ent| reverse ent },
        "Palindromic" => lambda { |inst, ent| reverse(ent) == ent },
        "IsPrime" => vectorize_monad { |inst, n|
            Prime.prime? n
        },
        "Prime" => vectorize_monad { |inst, n|
            nth_prime n
        },
        "Primes" => vectorize_monad { |inst, n|
            Prime.first n
        },
        "PrimeDivision" => vectorize_monad { |inst, n|
            Prime.prime_division n
        },
        "PrimeFactors" => vectorize_monad { |inst, n|
            prime_factors n
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
        "Bond" => lambda { |inst, func, larg|
            lambda { |inst, *args| func[inst, larg, *args] }
        },
        "RBond" => lambda { |inst, func, rarg|
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
            slices list, skew
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
        "|" => vectorize_dyad { |inst, a, b| b % a == 0 },
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
        "..." => lambda { |inst, x, y| (x...y).to_a },
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
        "&:" => lambda { |inst, a, b|
            if a.is_a? Proc
                lambda { |inst, *args| a[inst, b, *args] }
            elsif b.is_a? Proc
                lambda { |inst, *args| b[inst, *args, a] }
            else
                STDERR.puts "idk#2"
                raise
            end
        },
        "=>" => @@functions["Map"],
        "\\" => @@functions["Select"],
        "or" => lambda { |inst, a, b|
            AtState.truthy?(a) ? a : b
        },
        "and" => lambda { |inst, a, b|
            AtState.falsey?(b) ? b : a
        },
    }
    @@unary_operators = {
        "-" => vectorize_monad { |inst, n| -n },
        "#" => lambda { |inst, n| n.size },
        "@" => lambda { |inst, f|
            if f.is_a? Proc
                vectorize { |inst, *args|
                    f[inst, *args]
                }
            else
                inst.get_value f
            end
        },
        "!" => vectorize_monad { |inst, n| factorial n },
    }
    def initialize(program)
        @tokens = parse(program)
        @variables = {
            "true" => true,
            "false" => false,
        }
        @stack = []
        @last_values = []
        @i = 0
    end
    attr_reader :stack, :last_values
    def get_value(obj)
        # p obj
        raw, type = obj
        if type == :reference
            raw[1..-1]
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
        @variables[name] = value
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