#!/usr/bin/ruby

load 'boilerplates.rb'
require_relative 'AtState.rb'
require_relative 'tio.rb'

$inst = AtState.new "Needs[$visuals]"
$inst.run

def highlight_html(str)
    $inst.variables["highlight_html"][$inst, str]
end

$PUSH_COLLECT = {
    "type" => [:types, 2],
    "param" => [:params, 2],
    "option" => [:options, 2],
}
$APPEND = [
    "example",
    "paramtype",
    "optional"
]

def vector_from_signature(sig)
    if sig.index "vector"
        after = case sig
            when /monad/
                " monadically"
            when /RIGHT/
                " dyadically on the right"
            when /LEFT/
                " dyadically on the left"
            when /dyad/
                " dyadically"
            else
                ""
        end
        "Vectorizes#{after}."
    else
        ""
    end
end

def curry_from_signature(sig)
    if sig.index "curry"
        "Curries."
    else
        ""
    end
end

def reform_from_signature(sig)
    inner = case sig
        when /element|member/
            "'s elements"
        else
            ""
    end
    "Reforms result#{inner}."
end

def fit_least_indent(lines)
    least_indent = lines.map { |e| e.scan(/\s*/)[0].size }.min
    lines.map { |e| e[least_indent..-1] }
end


$RB_AFTER_COMMENT = /(?<=#).+/
$RB_COMMENT_GROUP = /#<<[\s\S]+?#>>\s+[^#].*?\r?\n[\s\S]+?\},\s*$/
$RB_OTHER_COMMENT_GROUP = /##<<[\s\S]+?##>>/
$RB_OTHER_AFTER_COMMENT = /(?<=##).+$/
$RB_SIGNATURE_PARSE = /"(.+?)" => (\w+(?:\(.+?\))?) \{ \|(.+?)\|\s*$/
$DATA_LINE = /@(\w+)(?:\s?(.+))?/
def create_info(body)
    info = Hash.new("")
    $PUSH_COLLECT.values.each { |k| info[k[0]] = {} }
    $APPEND.each { |k| info[k.to_sym] = [] }

    body.each { |line|
        data = line.scan($DATA_LINE)
        if data.empty?
            info[:description] += line + " "
        else
            key, value = data.first
            if $PUSH_COLLECT.has_key? key
                source, arity = $PUSH_COLLECT[key]
                name, *other = value.split(/\b\s*/, arity)
                info[source][name] = other
            elsif $APPEND.include? key
                info[key.to_sym] << value
            else
                info[key.to_sym] = value
            end
        end
    }

    info
end

def get_info_rb(input)
    final = []

    # pass 1
    groups = input.scan($RB_COMMENT_GROUP)

    groups.each { |group|
        group = group.lines

        begin
            last = group.index { |e| e.strip == "#>>" } + 1
        rescue NoMethodError => e
            STDERR.puts "NoMethodError: #{e}"
            puts group
            next
            # exit
        end

        head, *body, tail, signature = group[0..last]
        code_source = group[last..-1]

        body.map! { |e|
            e.scan($RB_AFTER_COMMENT).first.strip
        }

        info = create_info(body)

        name, type, args = signature.scan($RB_SIGNATURE_PARSE).first

        args = args.split(/,\s*/)

        args.shift # remove inst

        final.push [name, {
            info: info,
            type: type,
            args: args,
            source: fit_least_indent(code_source),
        }]
    }

    # pass 2
    groups = input.scan($RB_OTHER_COMMENT_GROUP)
    groups.each { |group|
        group = group.lines
        group = group[1..-2]
        # split into source and non-source
        sorted = group.group_by { |line|
            /^\s*##/ === line
        }
        source = sorted[false]
        body = sorted[true].map { |e|
            e.scan($RB_OTHER_AFTER_COMMENT).first.strip
        }

        head = source.first
        name = head.match(/"(.+?)"/).to_a[1]
        args = nil

        info = create_info(body)

        final.push [name, {
            info: info,
            type: "",
            args: args,
            source: fit_least_indent(source)
        }]
    }

    final
end

$AT_COMMENT_GROUP = /\?{2}<<[\s\S]+?\?{2}>>/
$AT_AFTER_COMMENT = /(?<=\?{3}).+$/
def get_info_attache(input)
    groups = input.scan($AT_COMMENT_GROUP)

    final = []

    groups.each { |group|
        group = group.lines
        group = group[1..-2]
        # split into source and non-source
        sorted = group.group_by { |line|
            /^\s*\?{3}/ === line
        }
        source = sorted[false]
        body = sorted[true].map { |e|
            e.scan($AT_AFTER_COMMENT).first.strip
        }

        head = source.first
        name = head.scan($WORD).find { |e| e != "ClassNamed" }
        args = head.match(/#$WORD\s*\[(.+?)\]\s*[.:]=/).to_a[1]
        args &&= args.split(/,\s*/)

        info = create_info(body)

        source = [highlight_html(source.join)]

        final.push [name, {
            info: info,
            type: "",
            args: args,
            source: fit_least_indent(source)
        }]
    }

    final
end

SYMBOL = /\W/
def generate(title)
    input = File.read title
    final = case title
        when /\.rb$/
            get_info_rb input
        when /\.@$/
            get_info_attache input
        else
            raise "Name #{title.inspect} does not have a valid file extension."
    end

    result = ""
    $toc = Hash.new { |h, k| h[k] = [] }

    final.sort { |(k1, v1), (k2, v2)|
        k1 = k1.downcase
        k2 = k2.downcase
        if (SYMBOL === k1) ^ (SYMBOL === k2)
            SYMBOL === k1 ? -1 : 1
        else
            k1 <=> k2
        end
    }.each { |k, v|
        genre = v[:info][:genre]

        is_operator = genre.index "operator"
        is_unary_operator = genre.index "unary"

        id = is_unary_operator ? "unary_#{k}" : k

        if genre.empty?
            STDERR.puts "Warning: #{k} (#{id}) has no genre"
        end


        $toc[genre] << k

        result += "<div class=\"function\" id=\"#{id}\">"
        args_types = {}

        # associate types with each argument
        v[:args] ||= v[:info][:params].keys
        v[:args] = v[:info][:types].keys if v[:args].empty?
        v[:args].map! { |e|
            # e = e[0]
            name, default = e.split("=")
            decoration = name.scan(/\W+/).first
            name.gsub!(/\W/, "")

            pref, * = v[:info][:types][name]

            disp_name = name.strip

            disp_name = "..." + disp_name if decoration == "*"
            disp_name = "**" + disp_name if decoration == "**"

            ["...", "**"].each { |check|
                if pref && pref[check]
                    pref = pref.gsub(check, "")
                    pref = nil if pref.empty?
                    disp_name = check + disp_name
                end
            }

            head = unless pref.nil?
                BOILERPLATES[:argument] % {
                    type: pref,
                    name: disp_name,
                }
            else
                disp_name
            end

            optional = v[:info][:optional].include? name

            if optional
                head += "?"
            elsif !default.nil?
                head += "=" + default
            end

            args_types[name] = head
        }

        if v[:info][:return].empty?
            STDERR.puts "Warning: no return type given for #{k.inspect}"
        end

        header_plate = is_operator ? is_unary_operator ? :header_op_unary : :header_op : :header

        result += BOILERPLATES[header_plate] % {
            return_type:    v[:info][:return],
            name:           k,
            args:           v[:args].join(", "),
            left:           v[:args][0],
            right:          v[:args][1],
            genre:          genre,
        }

        sig = []
        sig.push vector_from_signature v[:type]
        if v[:info].has_key? :curries
            sig.push "Curries."
        else
            sig.push curry_from_signature v[:type]
        end
        sig.push reform_from_signature v[:info][:reforms] if v[:info].has_key? :reforms

        sig.reject!(&:empty?)

        unless sig.empty?
            result += "<p><em>#{sig.join " "}</em></p>"
        end

        result += v[:info][:description].gsub(/\s+/, " ")

        unless v[:info][:params].empty? && v[:info][:paramtype].empty?
            result += "<h3>Arguments</h3>"
        end

        v[:info][:params].each { |var, (val, *)|
            result += BOILERPLATES[:param] % {
                name: args_types[var],
                description: val,
            }
        }

        v[:info][:paramtype].each { |line|
            type, name, desc = line.split(" ", 3)
            result += BOILERPLATES[:param] % {
                name: BOILERPLATES[:argument] % {
                    name: name,
                    type: type,
                },
                description: desc,
            }
        }

        unless v[:info][:options].empty?
            result += "<h3>Options</h3>"
            result += v[:info][:options].map { |name, (description, *)|
                BOILERPLATES[:option] % {
                    name: name,
                    description: description,
                }
            }.join("\n")
        end

        unless v[:info][:example].empty?
            code = v[:info][:example].join("\n")

            result += BOILERPLATES[:example] % {
                code: highlight_html(code),
            }

            result += "<a href=\"#{tio_encode code}\">Try it online!</a>"
        end


        result += "<button class=\"source-button\" id=\"#{id}-button\">toggle source</button>"
        result += "<div id=\"#{id}-source\" class=\"code-source\"><pre>"
        result += v[:source].join
        result += "</pre></div>"

        result += "</div>"
    }

    toc_result = "<h2>Table of Contents</h2><p><em>Function count: #{final.size}</em></p><p>Click on genres to show all functions in that genre.</p><table id=\"toc\">"
    $toc.sort_by { |e| e[0].downcase }.each { |genre, names|
        # emphasize genre
        genre, specs = genre.split(/(?=\/)/, 2)
        specs = "<span class=\"minimize\">#{specs}</span>"
        genre += specs
        toc_result += "<tr><td class=\"genre-source\">(#{genre})</td><td>"
        names.each { |name|
            toc_result += "<code><a href=\"##{name}\">#{name}</a></code>, "
        }
        toc_result = toc_result[0..-3]
        toc_result += "</td></tr>"
    }

    toc_result += "</table>"

    preamble = "<p><a href=\"./index.html\">Return to the index.</a><p>"

    result = preamble + toc_result + "<div id=\"functions\">#{result}</div>"

    {
        content: BOILERPLATES[:html] % {
            title: title,
            body: result
        },
        count: final.size,
    }
end

sources = %w(
    AtFunctions.rb
    libs/metattache.rb
    libs/ppcg.@
    libs/std.@
    libs/visuals.@
)

index = ""

index += "<p>This is an index of all documentation of the features of Attache. <code>AtState</code> contains most of the functions found in Attache.</p>"

index += "<h2>Documented files</h2>"
index += "<ul>"
sources.each { |source|
    base = File.basename source, ".*"
    dest = "docs/#{base}.html"

    data = generate source
    File.write dest, data[:content]

    index += "<li><a href=\"./#{base}.html\">#{base}</a> (function count: #{data[:count]})</li>"
}
index += "</ul>"

File.write "docs/index.html", BOILERPLATES[:html] % {
    title: "Attache: Documentation",
    body: index
}
