#!/usr/bin/ruby

require_relative 'AtState.rb'
# require_relative 'libs/html.rb'
require 'nokogiri'

# <attache>...</attache>
#    raw attache code; final result stringified

# <variable>

# def container?(node)
    # [
        # Nokogiri::XML::DTD,
        # Nokogiri::HTML::Document,
    # ]
    # .any? { |e| e === node }
# end

# compiles a templat document
def templat(str)
    inst = AtState.new "load[str] := EvalHere[str]"
    inst.run
    load = lambda { |name|
        inst.variables["load"][inst, name]
    }
    
    parsed = Nokogiri::HTML(str)
    
    parsed.xpath("//attache").each { |el|
        content = el.children.map(&:to_s).join
        result = load[content].to_s
        text = Nokogiri::XML::Text.new result, el.document
        el.replace text
    }
    
    parsed
end