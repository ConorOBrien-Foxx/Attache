require 'zlib'

def to_base(n, base)
    res = []
    while n != 0
        n, e = n.divmod base
        res.unshift e
    end
    res
end

def from_base(n, base)
    n = n.clone
    pow = 1
    res = 0
    until n.empty?
        res += n.pop * pow
        pow *= base
    end
    res
end