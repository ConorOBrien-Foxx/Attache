# help library

AtState.function("QueryNames", aliases: ["QN"]) { |inst, head = ""|
    names = inst.all_variables.flat_map(&:keys).select { |e|
        e.start_with? head
    }.sort_by(&:downcase)
}
