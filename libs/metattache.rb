# attache module

require_relative '../AtState.rb'

AtState.function("Tokenize") { |inst, text|
    tokenize(text).map { |tok|
        a, b, c = tok
        Token.new a, b.to_s, c
    }
}

AtState.function("Shunt") { |inst, text|
    parse(text).map { |tok|
        a, b, c = tok
        Token.new a, b.to_s, c
    }
}

def at_ast(node)
    node = ast(node) unless Array === node
    node
end

AtState.function("AST") { |inst, text|
    at_ast(text)
}

AtState.function("Traverse", configurable: true) { |inst, nodes, **opts|
    rec = lambda { |inst, node|
        args = node.children.map { |child|
            opts[:getValue][inst, child]
        }
        opts[:operators][node.head.raw][inst, *args]
    }
    
    nodes.map { |child|
        rec[inst, child]
    }
}