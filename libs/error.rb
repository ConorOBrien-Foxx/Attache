

AtState.variable("AttacheError", AttacheError)
AttacheError.descendants.each { |err|
    AtState.variable(err.name, err)
}
