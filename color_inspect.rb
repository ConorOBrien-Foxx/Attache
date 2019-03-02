def color_inspect(item, color: true)
    # TODO: generalize for all collections
    if item.is_a? Array
        return "[#{item.map { |el| color_inspect(el, color: color) }.join(", ") }]"
    elsif item.is_a? Hash
        arrow = color ? " \x1b[34m=>\x1b[0m " : " => "
        return "[#{item.map { |key, value| color_inspect(key, color: color) + arrow + color_inspect(value, color: color) }.join(", ") }]"
    end
    if item.method(:inspect).parameters.any? { |param| param == [:key, :color] }
        string = item.inspect(color: color)
        return string if color
    else
        string = item.inspect
    end
    if item.is_a? String
        color ? "\x1b[32m#{string}\x1b[0m" : string
    elsif [Integer, Symbol, TrueClass, FalseClass].any? { |type| item.is_a? type }
        color ? "\x1b[35m#{string}\x1b[0m" : string
    else
        color ? string.sub(/^#(\w+)</) { |_, e| "#\x1b[33m#{e}\x1b[0m<" } : string
    end
end
