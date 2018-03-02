require 'prime'
require 'date'
require 'matrix'

# a bunch of function used in Attache
# these are abstract functions not necessarily related to Attache

## CONSTANTS ETC. ##

Infinity = Float::INFINITY
$ALPHA_UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
$ALPHA_LOWER = "abcdefghijklmnopqrstuvwxyz"

## CLASS EXTENSIONS AND DEFINITIONS ##

class Proc
    def bind(*args)
        Proc.new { |*rest| self[*args, *rest] }
    end
end

class Train
    def initialize(*a)
        @train = a
        @frozen = false
    end
    
    def append(*args)
        Train.new *@tran, *args
    end
    
    def freeze
        @frozen = true
        self
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
    
    def to_ary
        if @frozen
            yield [@train]
        else
            @train.each { |e| yield e }
        end
    end
end

class Tie
    def initialize(funcs, array=false)
        @funcs = []
        @array = array
        funcs.each { |f|
            @funcs.concat [*f]
            @array ||= f.array if Tie === f
        }
    end
    
    attr_reader :array
    
    def [](inst, *args)
        args = args.first if @array
        args.map.with_index { |e, i|
            get(i)[inst, e]
        }
    end
    
    def get(i)
        @funcs[i % @funcs.size]
    end
    
    def fold(inst, list, start=nil)
        i = 0
        list.inject(start) { |acc, e|
            get(i)[inst, acc, e]
            i += 1
        }
    end
    
    def to_a
        @funcs.dup
    end
    
    def to_ary
        to_a
    end
    
    def to_s
        "Tie(#{@array ? "" : "no "}array)[#{@funcs * "'"}]"
    end
end

class Array
    def fold(inst, f, start)
        inject(start) { |x, y| f[inst, x, y] }
    end
    
    def sum
        inject(0, :+)
    end

    def prod
        inject(1, :*)
    end
    
    def average
        sum.to_f / size
    end
    
    # sample variance
    def variance
        m = average
        s = inject(0) { |a, i| a + (i - m) ** 2 }
        s / (size - 1.0)
    end

    def stddev
        Math.sqrt variance
    end

    def prefixes
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
        if empty?
            raise ArgumentError.new "Median of an empty array is undefined"
        end
        sorted = sort
        (sorted[(size - 1) / 2] + sorted[size / 2]) / 2.0
    end

    def delta
        each_cons(2).map { |(x, y)| y - x }
    end
    
    def indices(ent=nil, &fn)
        if fn.nil?
            each_index.select { |i| self[i] == ent }
        else
            each_index.select { |i| fn[i] }
        end
    end
end

# extend TrueClass and FalseClass
module TruthExtension
    def to_i
        self ? 1 : 0
    end
    
    def coerce(other)
        [other, self.to_i]
    end
    
    def ~@
        not self
    end
    
    [:+, :*, :/, :-, :<, :>, :<=, :>=, :<<, :>>, :>, :<, :>=, :<=, :-@].each { |prop|
        define_method(prop) { |*args| to_i.send(prop, *args) }
    }
end
class TrueClass
    include TruthExtension
end
class FalseClass
    include TruthExtension
end

module TimeChangeExtension
    def years
        365 * days
    end
    def weeks
        7 * days
    end
    def days
        24 * hours
    end
    def hours
        60 * minutes
    end
    def minutes
        60 * seconds
    end
    def seconds
        self
    end
    alias :year     :years
    alias :week     :years
    alias :day      :days
    alias :hour     :hours
    alias :minute   :minutes
    alias :second   :seconds
end

class Numeric
    include TimeChangeExtension
end

def yearlike(n)
    case n
        when Numeric
            Time.new n
        when Time
            n
        else
            Time.new *n rescue Time.new n
    end

end

module TimeExtension
    ## CLASS METHODS ##
    def self.included(base)
        base.extend(ClassMethods)
    end
    
    def self.from_h(hash)
        args = %w(year month day hour minute second).map { |s|
            hash[s] || hash[s.to_sym] || 0
        }
        Time.new *args
    end
    
    module ClassMethods
        def from_h(*args)
            TimeExtension.from_h *args
        end
        
        def iterate(start_time, end_time, step, &block)
            begin
                yield start_time
            end while (start_time += step) <= end_time
        end
    end
    
    ## INSTANCE METHODS ##
    @@WEEKDAYS = %w(Sunday Monday Tuesday Wednesday Thursday Friday Saturday)
    def to_h
        {
            year: year,
            month: month,
            day: day,
            hour: hour,
            minute: min,
            second: sec,
            microsecond: usec
        }
    end
    
    def change(**opts)
        mod = to_h
        opts.each { |opt, by|
            mod[opt] += by
        }
        TimeExtension.from_h mod
    end
    def update(**opts)
        mod = to_h
        opts.each { |opt, to|
            mod[opt] = to
        }
        TimeExtension.from_h mod
    end
    def day_of_year(n=1)
        base = update day: 1, month: 1, hour: 0, minute: 0, second: 0
        base + (n - 1).days
    end
    def year_size
        base = day_of_year(1)
        amount = 365
        while base.year == (base + amount.days).year
            amount += 1
        end
        amount
    end
    def year_days
        base = day_of_year(1)
        (0...year_size).map { |e| base + e.days }
    end
    def week_day
        @@WEEKDAYS[wday]
    end
end
class Time
    include TimeExtension
end

module FloatExtension
    def times(&fn)
        if self.infinite?
            loop &fn if self.positive?
        else
            to_i.times { fn[] }
        end
    end
end

class Float
    include FloatExtension
end

module ReadableArrays
    def readable(factor: 1, method: :rjust)
        repr = to_a.map { |row|
            row.map(&:inspect)
        }
        
        column_widths = repr.transpose.map { |col|
            col.map(&:size).max + factor
        }
        
        res = ""
        repr.each { |row|
            row.each_with_index { |el, j|
                res += el.send method, column_widths[j]
            }
            res += "\n"
        }
        res.chomp
    end
end

class Matrix
    include ReadableArrays
end
class Array
    include ReadableArrays
end


## GENERIC HELPER FUNCTIONS ##

def deep_copy(o)
    case o
        when Array
            o.map { |e| deep_copy e }
        when Hash
            o.map { |e| deep_copy e }.to_h
        else
            if o.respond_to? :dup
                o.dup
            else
                o
            end
    end
end

def cls
    system Gem.win_platform? ? "cls" : "clear"
end

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
    obj.each{|v,k|puts"  #{v} -> #{k.inspect}"}
    puts "[/#{title}]"
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
    (1..n).to_a.prod
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
    arr.reduce(0, :gcd)
end

def random(min=nil, max=nil)
    if min.nil?
        rand
    elsif max.nil?
        rand(min)
    else
        min + rand(max - min + 1)
    end
end

def from_numlike(n)
    return 0 if n == false
    return 1 if n == true
    return n
end

def simplify_number(n)
    n = from_numlike n
    return n if n.abs == Infinity
    return n.to_i if n == n.to_i
    return n
end

def to_number(s)
    if /^\d+$/ === s
        return s.to_i
    else
        return s.to_f
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
    return list                 if list.is_a? Array
    return list.chars           if list.is_a? String
    return list.digits.reverse  if list.is_a? Numeric
    return list.to_a rescue list
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

def sample(list, n=nil)
    if n.nil?
        force_list(list).sample
    else
        force_list(list).sample(n)
    end
end

# reads a character without waiting for ENTER
def getchar(safe=true)
    require 'io/console'
    res = STDIN.getch
    exit -42 if res == "\x03" if safe
    res
end

def prompt_input(prompt=nil, input=STDIN)
    print prompt unless prompt.nil? or not $stdin.tty?
    input.gets.chomp rescue nil
end

$YES_NO = {
              "y" => "yes",
              "n" => "no"
          }
def read_option(prompt, opts, clear=false)
    res = nil
    cls if clear
    puts prompt
    opts = opts.map { |k, v| [k.to_s, v] }.to_h
    opts.each { |char, val|
        puts "  #{char}> #{val}"
    }
    loop {
        res = getchar
        break if opts.has_key? res
        puts "#{res.inspect} is not a valid option."
    }
    res
end

def zipwith(a, b, &fn)
    a.zip(b).map { |x, y|
        fn[x, y]
    }
end

def positions(arr, els)
    els.uniq.map { |k|
        [k, arr.indices(k)]
    }
end

def to_base(n, base)
    res = []
    while n != 0
        n, e = n.divmod base
        res.unshift e
    end
    res.push 0 if res.empty?
    res
end

def from_base(n, base)
    n = n.clone
    if n.is_a? String
        n = n.chars.map(&:to_i)
    end
    pow = 1
    res = 0
    until n.empty?
        res += n.pop * pow
        pow *= base
    end
    res
end

def diagonal(mat, n=0)
    res = []
    i = 0
    
    while i < 0 || n < 0
        i += 1
        n += 1
    end
    
    while i < mat.size && n < mat[i].size
        res << mat[i][n]
        i += 1
        n += 1
    end
    
    res
end

def lower_triangle(mat, strict=false)
    mat.map.with_index { |row, i|
        row.map.with_index { |e, j|
            (strict ? i <= j : i < j) ? 0 : e
        }
    }
end

def upper_triangle(mat, strict=false)
    mat.map.with_index { |row, i|
        row.map.with_index { |e, j|
            (strict ? i >= j : i > j) ? 0 : e
        }
    }
end

def matrix_like?(array)
    begin
        Matrix[*array]
        true
    rescue
        false
    end
end

def simp_array(array)
    matrix_like?(array) ? Matrix[*array] : array
end

def split_at(array, indices)
    indices = [*indices]
    indices << array.size
    sections = []
    build = []
    array.each_with_index { |e, i|
        build << e
        if indices.index(i + 1)
            sections << build
            build = []
        end
    }
    sections
end

def rotate(arr, inds)
    if inds.is_a? Array
        inds = resize(inds, arr.size)
        arr.map.with_index { |e, i|
            rotate(e, inds[i])
        }
    else
        arr.rotate(inds)
    end
end

def matrix_iota(mat)
    res = []
    Matrix[*mat].each_with_index { |e, row, col|
        res[row] ||= []
        res[row][col] = [row, col]
    }
    res
end

def dim(mat)
    begin
        mat = Matrix[*mat]
        [mat.row_count, mat.column_count]
    rescue
        mat.size
    end
end

def overlap(source, inner)
    return false if inner.size > source.size
    
    source.each_cons(inner.size).any? { |e| e == inner }
end

# https://stackoverflow.com/a/2946734/4119004
def convert_to_lambda(&block)
    obj = Object.new
    obj.define_singleton_method(:_, &block)
    return obj.method(:_).to_proc
end

def split_on(iter, func)
    build = [[]]
    iter.each { |el|
        if func[el]
            build << []
        end
        build.last << el
    }
    build
end

def chop(array, size)
    array = array.dup
    size = [*size]
    
    collect = []
    i = 0
    until array.empty?
        collect << array.shift(size[i])
        i += 1
        i = i % size.size rescue 0
    end
    collect
end