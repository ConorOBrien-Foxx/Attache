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

def compress_num(n, base=255, pivot=2)
    values = to_base(n, base)
    res = if base <= 95
        values.map { |e| (e + 32).chr }
    elsif base == 255
        values.map { |e| (e >= pivot ? e + 1 : e).chr }
    else
        values.map { |e| e.chr }
    end
    res.join
end

def decompress_num(str, base=255, pivot=2)
    values = str.chars
    values = if base <= 95
        values.map { |e| e.ord - 32 }
    elsif base == 255
        values.map { |e|
            a = e.ord
            a > pivot ? a - 1 : a
        }
    else
        values.map { |e| e.ord }
    end
    from_base values, base
end

def zlibcomp(text)
    Zlib::Deflate.deflate(text, Zlib::BEST_COMPRESSION)
end

def zlibdecomp(text)
    Zlib::Inflate.inflate(text)
end

def id(str); str; end

$DICTIONARY = nil
def load_dictionary
    $DICTIONARY ||= File.read(File.join(File.dirname(__FILE__), "information/dictionary.txt")).split(/\r?\n/)
end
def dictionary_compress(text)
    load_dictionary
    text.gsub("\x02","\x02\x02").gsub(/\w+/) { |e|
        index = $DICTIONARY.index e
        if index
            "\x02#{compress_num(index)}\x02"
        else
            e
        end
    }
end
def dictionary_decompress(text)
    load_dictionary
    text.gsub(/\x02(.*?)\x02/) { |e|
        if $1.empty?
            "\x02"
        else
            $DICTIONARY[decompress_num $1]
        end
    }
end

# meant to compress short, ASCII, English text
$SHORT_HEAD = %w(
    an as at cc ed ee en er es ff he hi in ll nd
    nn of on oo or pp re rr ss st te th the ti to tt
)
$SHORT_HEAD_RE = Regexp.new($SHORT_HEAD.join "|")
# longer, common words: 1 byte equivalents
# excluding:
#   that (= th + at)
#   the  (in short head)
#   of   (")
#   and  (= an + d)
#   a    (too short)
#   in   (in short head)
#   I    (too short)
# etc.
$SHORT_TAIL = %w(
    be have it for not with you but by from they we say
    her she or will my one all would there their what
    about who get which go when person into year now
    look only come over because even work world most day
)
$SHORT_TAIL_RE = Regexp.new($SHORT_TAIL.join "|")
$LAST_INDEX = 126 + $SHORT_TAIL.size
def li(n); ($LAST_INDEX + n + 1).chr; end
def pair_compress(text)
    text.gsub($SHORT_TAIL_RE) { |word|
        ($SHORT_TAIL.index(word) + 126).chr
    }.gsub($SHORT_HEAD_RE) { |word|
        $SHORT_HEAD.index(word).chr
    }
end

$SHORT_TAIL_REVERSE_RE = Regexp.new("[\\x7F-\\x" + $LAST_INDEX.to_s(16).upcase + "]", nil, "n")
p $SHORT_TAIL_REVERSE_RE
def pair_decompress(text)
    text.force_encoding("BINARY").gsub(/[\x00-\x1F]/) { |ind|
        $SHORT_HEAD[ind.ord]
    }.gsub(/[\\x7F-\\xAA]/) { |ind|
        ($SHORT_TAIL[ind.ord] || ind)
    }.gsub(/#{li 0}(.)/) { |text|
        $1.upcase
    }
end



def insane_compress(text)
    zlibcomp dictionary_compress text
end

def insane_decompress(text)
    dictionary_decompress zlibdecomp text
end

$COMPRESSION_CHOICES   = [
    :zlibcomp,
    :id,
    :dictionary_compress,
    # :pair_compress,
    :insane_compress,
]
$DECOMPRESSION_CHOICES = [
    :zlibdecomp,
    :id,
    :dictionary_decompress,
    # :pair_decompress,
    :insane_decompress,
]
def compress(text)
    results = $COMPRESSION_CHOICES.map { |e| send e, text }
    shortest = results.min_by &:size
    type = results.index shortest
    
    (32 + type).chr + shortest
end

def decompress(text)
    type = text[0].ord - 32
    send $DECOMPRESSION_CHOICES[type], text[1..-1]
end

def stats(str)
    c = compress str
    d = decompress c
    puts "[size]            #{str.size}"
    puts "[compressed size] #{c.size}"
    puts "[savings]         #{(c.size * 10000 / str.size) / 100.0}%"
    puts "[success?]        #{d == str ? "yes" : "no"}"
    puts "[type]            #{$COMPRESSION_CHOICES[c[0].ord - 32]}"
    puts "[string]"
    puts c
    puts "[inspect]"
    puts c.inspect
    puts "[inverted]"
    puts decompress c
end

def read_bytes(filename)
    f = File.open(filename, "rb", crlf_newline: false)
    f.each_byte.to_a.map(&:chr).join
end

def write_bytes(filename, content)
    File.write(filename, content, mode:"wb")
end

if $0 == __FILE__
    require 'optparse'
    options = {}
    filename = File.basename __FILE__
    parser = OptionParser.new { |opts|
        opts.banner = "Usage: #{filename} <file>"
        
        opts.on("-s", "--string STR", "Compress a string on the command line") { |s|
            options[:string] = s
        }
        
        opts.on("-o", "--output FILE", "Writes the bytes of the compressed string to a file") { |s|
            options[:outfile] = s
        }
        
        opts.on("-d", "--decompress", "Reverses the operation") { |s|
            options[:decompress] = s
        }
        
        opts.on("-h", "--help", "Prints this help message") { puts opts; exit }
    }
    parser.parse!
    
    if ARGV.empty? && options.empty?
        puts parser
        exit
    end
    
    str = options[:string]
    if str.nil?
        unless File.exist? ARGV[0]
            STDERR.puts "No such file #{ARGV[0].inspect}"
            puts parser
            exit
        end
        str = File.read ARGV[0]
    end
    if options[:decompress]
        p str
        print decompress str
    else
        comp = compress str
        if options[:outfile]
            write_bytes options[:outfile], comp 
        else
            print comp
        end
    end
end