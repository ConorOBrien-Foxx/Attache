require 'nokogiri'

# test = Nokogiri::HTML()

AtState.function("HTML") { |inst, arg|
    Nokogiri::HTML(arg)
}

def text_of(str, doc)
    Nokogiri::XML::Text.new str, doc
end

def to_html(arg, doc)
    if String === arg
        text_of arg, doc
    else
        arg
    end
end

AtState.function("HTMLText") { |inst, arg, doc|
    text_of arg, doc
}

AtState.function("ToHTML") { |inst, arg, doc|
    to_html arg, doc
}

AtState.function("HTMLAddNext") { |inst, el, nxt|
    el.next = nxt
}

AtState.function("HTMLAddChild") { |inst, el, nxt|
    el.add_child nxt
}

AtState.function("HTMLRemove") { |inst, el|
    el.remove
}

AtState.function("HTMLReplace") { |inst, source, dest|
    source.replace dest
}

AtState.function("HTMLSelect") { |inst, src, sel|
    src.css(sel)[0] || nil
}

AtState.function("HTMLSelectAll") { |inst, src, sel|
    src.css sel
}

AtState.function("HTMLRepr") { |inst, src|
    src.to_html
}

AtState.function("HTMLCreate") { |inst, name, doc, *content|
    res = doc.create_element name
    content.each { |x|
        res.add_child to_html(x, doc)
    }
    res
}