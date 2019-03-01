def color_inspect(item, color: true)
    # TODO: generalize for all collections
    if item.is_a? Array
        return "[#{item.map { |el| color_inspect(el, color: color) }.join(", ") }]"
    elsif item.is_a? Hash
        return "[#{item.map { |key, value| color_inspect(key, color: color) + " => " + color_inspect(value, color: color) }.join(", ") }]"
    end
    if item.is_a? String
        color ? "\x1b[32m#{item.inspect}\x1b[0m" : item.inspect
    elsif [Integer, Symbol, TrueClass, FalseClass].any? { |type| item.is_a? type }
        color ? "\x1b[35m#{item.inspect}\x1b[0m" : item.inspect
    else
        args = item.method(:inspect).parameters[0]
        if args && args.include?(:color)
            string = item.inspect(color: color)
        else
            string = item.inspect
        end
        color ? string.sub(/^#(\w+)</) { |_, e| "#\x1b[33m#{e}\x1b[0m<" } : string
    end
end
