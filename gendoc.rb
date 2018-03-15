#!/usr/bin/ruby

load 'boilerplates.rb'
title = "AtState.rb"
input = File.read title

require_relative 'AtState.rb'
require_relative 'compress.rb'

$inst = AtState.new "Needs[$visuals]"
$inst.run

def highlight_html(str)
    $inst.variables["highlight_html"][$inst, str]
end

# tio integration
require 'zlib'
require 'base64'
FIELD_SEPARATOR = "\xff"

def finalize(state_string)
    compressed = zlib_deflate(state_string)
    encoded = Base64.encode64 compressed
    encoded.tr("+", "@").gsub(/=+/, "")
end

def tio_encode(program)
    state_string = "attache#{FIELD_SEPARATOR * 2}#{program}#{FIELD_SEPARATOR * 2}"
    url = finalize state_string
    "tio.run/###{url}"
end


$AFTER_COMMENT = /(?<=#).+/
$COMMENT_GROUP = /#<<[\s\S]+?#>>\s+.+?$/
$SIGNATURE_PARSE = /"(\w+)" => (\w+(?:\(.+?\))?) \{ \|(.+?)\|\s*$/
$DATA_LINE = /@(\w+)(?:\s?(.+))?/

groups = input.scan($COMMENT_GROUP)

$final = {}

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
        "<p><em>Vectorizes#{after}.</em></p>"
    else
        ""
    end
end

groups.each { |group|
    head, *body, tail, signature = group.lines
    
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
    
    $final[name] = {
        info: info,
        type: type,
        args: args,
    }
}

result = ""
$toc = Hash.new { |h, k| h[k] = [] }

$final.sort.each { |k, v|
    genre = v[:info][:genre]

    if genre.empty?
        STDERR.puts "Warning: #{k} has no genre"
    end
    
    
    $toc[genre] << k
    
    result += "<div class=\"function\" id=\"#{k}\">"
    args_types = {}
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
    
    result += BOILERPLATES[:header] % {
        return_type:    v[:info][:return],
        name:           k,
        args:           v[:args].join(", "),
        genre:          genre,
    }
    
    result += text_from_signature v[:type]
    
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
        
        result += "<a href=\"https://#{tio_encode code}\">Try it online!</a>"
    end
    
    result += "</div>"
}

toc_result = "<h2>Table of Contents</h2><p><em>Function count: #{$final.size}</em></p><table id=\"toc\">"
$toc.sort_by { |e| e[0].downcase }.each { |genre, names|
    toc_result += "<tr><td>(#{genre})</td><td>"
    names.each { |name|
        toc_result += "<code><a href=\"##{name}\">#{name}</a></code>, "
    }
    toc_result = toc_result[0..-3]
    toc_result += "</td></tr>"
}

toc_result += "</table>"

result = toc_result + result

File.write "docs/index.html", BOILERPLATES[:html] % {
    title: title,
    body: result
}






