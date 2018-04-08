#!/usr/bin/ruby

load 'boilerplates.rb'
require_relative 'AtState.rb'
require_relative 'tio.rb'

$inst = AtState.new "Needs[$visuals]"
$inst.run

def highlight_html(str)
    $inst.variables["highlight_html"][$inst, str]
end


$AFTER_COMMENT = /(?<=#).+/
$COMMENT_GROUP = /#<<[\s\S]+?#>>\s+.+?\r?\n[\s\S]+?\},/
$SIGNATURE_PARSE = /"(.+?)" => (\w+(?:\(.+?\))?) \{ \|(.+?)\|\s*$/
$DATA_LINE = /@(\w+)(?:\s?(.+))?/

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

def text_from_signature(sig)
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

def fit_least_indent(lines)
    least_indent = lines.map { |e| e.scan(/\s+/)[0].size }.min
    lines.map { |e| e[least_indent..-1] }
end

def generate(title)
    input = File.read title
    
    groups = input.scan($COMMENT_GROUP)
    
    final = {}

    groups.each { |group|
        group = group.lines
        
        last = group.index { |e| e.strip == "#>>" } + 1
        
        head, *body, tail, signature = group[0..last]
        code_source = group[last..-1]
        
        body.map! { |e|
            e.scan($AFTER_COMMENT).first.strip
        }
        
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
        
        name, type, args = signature.scan($SIGNATURE_PARSE).first
        args = args.split(/,\s*/)
        
        args.shift # remove inst
        
        final[name] = {
            info: info,
            type: type,
            args: args,
            source: fit_least_indent(code_source),
        }
    }

    result = ""
    $toc = Hash.new { |h, k| h[k] = [] }

    final.sort.each { |k, v|
        genre = v[:info][:genre]
        
        is_operator = genre.index "operator"
        is_unary_operator = genre.index "unary"
        
        if genre.empty?
            STDERR.puts "Warning: #{k} has no genre"
        end
        
        
        $toc[genre] << k
        
        result += "<div class=\"function\" id=\"#{k}\">"
        args_types = {}
        
        # associate types with each argument
        v[:args].map! { |e|
            # e = e[0]
            name, default = e.split("=")
            decoration = name.scan(/\W+/).first
            name.gsub!(/\W/, "")
            
            pref, * = v[:info][:types][name]
            
            disp_name = name
            
            disp_name = "..." + disp_name if decoration == "*"
            disp_name = "**" + disp_name if decoration == "**"
            
            head = unless pref.nil?
                BOILERPLATES[:argument] % {
                    type: pref,
                    name: disp_name,
                }
            else
                disp_name
            end
            
            unless default.nil?
                if v[:info][:optional].include? name
                    head += "?"
                else
                    head += "=" + default
                end
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
        sig.push text_from_signature v[:type]
        sig.push "Reforms argument." if v[:info].has_key? :reforms
        
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
        
        
        result += "<button class=\"source-button\" id=\"#{k}-button\">toggle source</button>"
        result += "<div id=\"#{k}-source\" class=\"code-source\"><pre>"
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

    BOILERPLATES[:html] % {
        title: title,
        body: result
    }
end

sources = ["AtState.rb"]

index = ""

index += "<p>This is an index of all documentation of the features of Attache. <code>AtState</code> contains most of the functions found in Attache.</p>"

index += "<h2>Documented files</h2>"
index += "<ul>"
sources.each { |source|
    base = File.basename source, ".*"
    dest = "docs/#{base}.html"
    
    File.write dest, generate(source)
    
    index += "<li><a href=\"./#{base}.html\">#{base}</a></li>"
}
index += "</ul>"

File.write "docs/index.html", BOILERPLATES[:html] % {
    title: "Attache: Documentation",
    body: index
}