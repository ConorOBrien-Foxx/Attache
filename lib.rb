require 'prime'

# a bunch of function used in Attache
# these are abstract functions not necessarily related to Attache

## CLASS EXTENSIONS AND DEFINITIONS ##

class Proc
    def bind(*args)
        Proc.new { |*rest| self[*args, *rest] }
    end
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

class Array
    def sum
        inject(0, :+)
    end

    def prod
        inject(1, :*)
    end
    
    def average
        sum.to_f / size
    end

    def variance
        m = average
        s = inject(0) { |a, i| a + (i - m) ** 2 }
        s / (size - 1.0)
    end

    def stddev
        Math.sqrt variance
    end

    def prefixes(list)
        (1..size).map { |i| self[0...i] }
    end

    # modified from https://github.com/sagivo/powerset/blob/master/powerset.rb
    def powerset
        a = [[]] 
        (0...size).each { |i|
            len = a.size; j = 0;
            while j < len
                a.push a[j] + [self[i]]
                j += 1
            end
        }
        a
    end

    def median
        sorted = sort
        (sorted[(size - 1) / 2] + sorted[size / 2]) / 2.0
    end

    def delta
        each_cons(2).map { |(x, y)| y - x }
    end
end

# extend TrueClass and FalseClass

[TrueClass, FalseClass].each { |klass|
    klass.define_method(:to_i) {
        # p "arg = #{self}"
        self ? 1 : 0
    }
    klass.define_method(:coerce) { |other|
        [other, self.to_i]
    }
    [:+, :*, :/, :-, :<, :>, :<=, :>=, :<<, :>>, :-@, :~@].each { |prop|
        klass.define_method(prop) { |*args| to_i.send(prop, *args) }
    }
}

## GENERIC HELPER FUNCTIONS ##

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

def debug_obj(title,**obj)
    puts "[#{title}]"
    obj.each{|v,k|puts"  #{v} -> #{k}"}
end

def rotN(str, n)
    str.gsub(/[a-z]/i) { |letter|
        res = $ALPHA_LOWER[($ALPHA_LOWER.index(letter.downcase) + n) % 26]
        letter == letter.upcase ? res.upcase : res
    }
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

# 1-indexed
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

def fixpoint(f, n)
    loop {
        old = n
        n = f[n]
        break n if n == old
    }
end

def periodicloop(f, n)
    results = []
    loop {
        break results if results[0..-2].include? n
        results << n
        n = f[n]
    }
end

def collatz(n)
    array = []
    until n == 1
        array << n
        if n.even?
            n /= 2
        else
            n = n * 3 + 1
        end
    end
    array + [1]
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
    n = from_numlike n
    return n.to_i if n == n.to_i
    return n
end

def to_number(s)
    if /\./ === s
        return s.to_f
    else
        return s.to_i
    end
end

def force_number(n)
    simplify_number case n
        when Array
            to_number n.join
        when String
            to_number n
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

def pythagorean2(m, n)
    [m*m - n*n, 2*m*n, m*m + n*n]
end

$PYTHAGOREAN_GEN_CACHE = []
$PYTHAGOREAN_GEN = Enumerator.new { |enum|
    m = 2
    loop {
        (1...m).each { |n|
            enum.yield pythagorean2 m, n
        }
        m += 1
    }
}.with_index

def pythagorean(n)
    if n < $PYTHAGOREAN_GEN_CACHE.size
        $PYTHAGOREAN_GEN_CACHE[n]
    else
        repeat = n - $PYTHAGOREAN_GEN_CACHE.size + 1
        repeat.times {
            e, i = $PYTHAGOREAN_GEN.next
            $PYTHAGOREAN_GEN_CACHE[i] = e
        }
        $PYTHAGOREAN_GEN_CACHE[n]
    end
end

def slices(list, skew)
    force_list(list).each_cons(skew).to_a
end

def resize(list, n)
    (0...n).map { |i| list[i % list.size] }
end

$MAKE_REGEX_ESCAPE = {
    "l" => "[A-Za-z]",
    "L" => "[^A-Za-z]",
    "m" => "[a-z]",
    "M" => "[^a-z]",
    "o" => "[A-Z]",
    "O" => "[^A-Z]",
    "i" => "[A-Za-z0-9]",
    "I" => "[^A-Za-z0-9]",
}
$MAKE_REGEX_STANDARD = "abcfnrv\\'\"^$*+?/(){}|[]bBdDwWsSc0123456789."
def make_regex(str)
    return str if str.is_a? Regexp
    
    build = ""
    i = 0
    while i < str.size
        if str[i] == "`"
            i += 1
            while i < str.size
                if str[i] == "`"
                    break if str[i + 1] != "`"
                    i += 1
                end
                build += RegExp.escape str[i]
                i += 1
            end
        elsif str[i] == "\\"
            i += 1
            cur = str[i]
            if cur == nil
                build += "\\\\"
            elsif $MAKE_REGEX_STANDARD.include? cur
                build += "\\" + cur
            elsif $MAKE_REGEX_ESCAPE.has_key? cur
                build += $MAKE_REGEX_ESCAPE[cur]
            else
                build += cur
            end
        else
            build += str[i]
        end
        i += 1
    end
    
    Regexp.new build
end

def format_replace(repl, str, data, i)
    repl.gsub(/\$(\d+|.)/) {
        orig = $&
        spec = $1
        case spec
            when /\d+/
                data[spec.to_i]
            when ":"
                str
            when "#"
                i
            when "@"
                data[0].size
            when "&"
                data[0]
            else
                orig
        end
    }
end

def replace(str, search, replace)
    # p [str, search, replace]
    i = 0
    str.gsub(search) {
        data = $~
        res = format_replace(replace, str, data, i)
        i += 1
        res
    }
end

def sample(list, n)
    if n.nil?
        force_list(list).sample
    else
        force_list(list).sample(n)
    end
end