# attache module

require_relative '../AtState.rb'

##<<
## Returns the tokens of <code>text</code>.
## @type text string
## @return [token]
## @genre parsing
AtState.function("Tokenize") { |inst, text|
    tokenize(text).map { |tok|
        a, b, c = tok
        Token.new a, b.to_s, c
    }
}
##>>

##<<
## Returns the Shunting Yard algorithm performed on the tokens of <code>text</code>.
## @type text string
## @return [token]
## @genre parsing
AtState.function("Shunt") { |inst, text|
    shunt(text).map { |tok|
        a, b, c = tok
        Token.new a, b.to_s, c
    }
}
##>>

def at_ast(node)
    node = ast(node) unless Array === node
    node
end

##<<
## Returns an Abstract Syntax Tree representing the operator structure of
## <code>text</code> in Attache.
## @type text string
## @return [hash]
## @genre parsing
AtState.function("AST") { |inst, text|
    at_ast(text)
}
##>>

##<<
## Iterates over <code>nodes</code> using the options given by <code>opts</code>
## @type nodes [tree]
## @type opts **
## @return [(*)]
## @genre parsing
AtState.function("Traverse", configurable: true) { |inst, nodes, **opts|
    rec = lambda { |inst, node|
        args = node.children.map { |child|
            if Node === child
                rec[inst, child]
            else
                opts[:getValue][inst, child]
            end
        }
        opts[:operators][node.head.raw][inst, *args]
    }

    nodes = at_ast(nodes)

    nodes.map { |child|
        rec[inst, child]
    }
}
##>>
