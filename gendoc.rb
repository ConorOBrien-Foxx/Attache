#!/usr/bin/ruby

load 'boilerplates.rb'
title = "AtState.rb"
input = File.read title

$AFTER_COMMENT = /(?<=#).+/
$COMMENT_GROUP = /#<<[\s\S]+?#>>\s+.+?$/
$SIGNATURE_PARSE = /"(\w+)" => (\w+) \{ \|(.+?)\|\s*$/
$DATA_LINE = /@(\w+)\s+(.+)/

groups = input.scan($COMMENT_GROUP)

$final = {}

$PUSH_COLLECT = {
    "type" => :types,
    "param" => :params,
}
$APPEND = [
    "example",
]

groups.each { |group|
    head, *body, tail, signature = group.lines
    
    body.map! { |e|
        e.scan($AFTER_COMMENT).first.strip
    }
    
    info = Hash.new("")
    $PUSH_COLLECT.values.each { |k| info[k] = {} }
    $APPEND.each { |k| info[k.to_sym] = [] }
    
    body.each { |line|
        data = line.scan($DATA_LINE)
        if data.empty?
            info[:description] += line + " "
        else
            key, value = data.first
            if $PUSH_COLLECT.has_key? key
                source = $PUSH_COLLECT[key]
                name, type = value.split(/\b\s*/, 2)
                info[source][name] = type
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
toc = ""
$final.sort.each { |k, v|
    result += "<div class=\"function\">"
    args_types = {}
    v[:args].map! { |e|
        name, default = e.split("=")
        
        pref = v[:info][:types][name]
        head = unless pref.nil?
            BOILERPLATES[:argument] % {
                type: pref,
                name: name,
            }
        else
            name
        end
        
        unless default.nil?
            if v[:info][:optional][name]
                head += "?"
            else
                head += "=" + default
            end
        end
        
        args_types[name] = head
    }
    
    result += BOILERPLATES[:header] % {
        return_type:    v[:info][:return],
        name:           k,
        args:           v[:args].join(", "),
        genre:          v[:info][:genre],
    }
    result += v[:info][:description].gsub(/\s+/, " ")
    
    v[:info][:params].each { |var, val|
        result += BOILERPLATES[:param] % {
            name: args_types[var],
            description: val,
        }
    }
    
    unless v[:info][:example].empty?
        result += BOILERPLATES[:example] % {
            code: v[:info][:example].join("\n")
        }
    end
    
    result += "</div>"
}

File.write "docs/out.html", BOILERPLATES[:html] % {
    title: title,
    body: result
}






