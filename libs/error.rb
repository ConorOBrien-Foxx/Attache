# error library

AtState.variable("AttacheError", AttacheError)
AttacheError.descendants.each { |err|
    name = err.name
    base = err.name.gsub(/^Attache/, "")
    if base == name
        STDERR.puts "Warning: Malformed Error name `#{name}`"
    end
    AtState.variable(name, err)
    AtState.function(base) { |inst, message|
        raise err.new(message)
    }
}
