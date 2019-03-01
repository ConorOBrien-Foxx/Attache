def color_inspect(item, color: true)
    if not color
        return item.inspect
    end
    if item.is_a? String
        "\x1b[32m#{item.inspect}\x1b[0m"
    elsif item.is_a? Array
        "[#{item.map { |el| color_inspect(el, color: color) }.join(", ") }]"
    elsif item.is_a? Hash
        "[#{item.map { |key, value| color_inspect(key, color: color) + " => " + color_inspect(value, color: color) }.join(", ") }]"
    elsif [Integer, Symbol, TrueClass, FalseClass].any? { |type| item.is_a? type }
        "\x1b[35m#{item.inspect}\x1b[0m"
    else
        item.inspect.gsub(/#(\w+)</) { |_, e| "#\x1b[33m#{e}\x1b[0m<" }
    end
end
