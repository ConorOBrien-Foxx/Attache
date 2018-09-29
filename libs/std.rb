# puts "std.rb included"

##<<
## Alias for <a href="AtFunctions.html#String" target="_blank"><code>String</code></a>.
## @type e (*)
## @return string
## @genre string
AtState.alias "S", "String"
##>>

##<<
## Maps <code>fn</code> over <code>left</code>, using <code>...right</code> as arguments.
## @curries
## @reforms
## @type f fn
## @type left [(*)]
## @type right ...(*)
## @return [(*)]
## @genre functional
AtState.function "MapLeft", &curry(3) { |inst, fn, left, *right|
    arr = inst.cast_list left
    res = arr.map { |e|
        fn[inst, e, *right]
    }
    reform_list res, arr
}
##>>

##<<
## Maps <code>fn</code> over <code>right</code>, using <code>...left</code> as arguments.
## @curries
## @reforms
## @type f fn
## @type left ...(*)
## @type right [(*)]
## @return [(*)]
## @genre functional
AtState.function "MapRight", &curry(3) { |inst, fn, *left, right|
    arr = inst.cast_list right
    res = arr.map { |e|
        fn[inst, *left, e]
    }
    reform_list res, arr
}
##>>

##<<
## Returns a list of the prime numbers less than or equal to <code>n</code>.
## @vectorizes monadically
## @type n number
## @return number
## @genre numeric/prime
## @example Print[PrimesUnder[11]]
## @example ?? [2, 3, 5, 7, 11]
AtState.function "PrimesUnder", &vectorize_monad { |inst, n|
    AtState["Series"][
        inst,
        AtState["Prime"],
        n,
        include: true,
    ]
}
##>>

##<<
## Returns the number of prime numbers less than or equal to <code>n</code>.
## @vectorizes monadically
## @type n number
## @return number
## @genre numeric/prime
## @example Print[PrimePi[0:10]]
## @example ?? [0, 0, 1, 2, 2, 3, 3, 4, 4, 4, 4]
AtState.function "PrimePi", &vectorize_monad { |inst, n|
    AtState["PrimesUnder"][inst, n].size
}
##>>

##<<
## Prints <code>Format[...args]</code> without a trailing newline.
## @type args ...(*)
## @return nil
## @genre IO
AtState.function "Printf", &lambda { |inst, *args|
    print AtState["Format"][inst, *args]
}
##>>

##<<
## Prints each element in <code>args</code>, separated by newlines, with no trailing newline. Returns <code>args</code>.
## @type args ...(*)
## @return [(*)]
## @genre IO
AtState.function "Output", &lambda { |inst, args|
    AtState["Print"][inst, *args, joiner: "\n", after: ""]
}
##>>

"
PadMatrix[ragged, padwith, padf] := (
    padwith ..= 0;;
    padf ..= PadRight;;
    maxlen .= Max[Size => ragged];;
    padf<~_, maxlen, padwith~> => ragged
)
"

##<<
## Makes <code>ragged</code> a rectangular matrix, padding to the right with <code>padwith</code>.
## @type ragged [[(*)]]
## @type padwith (*)
## @type padf fn
## @optional padwith
## @optional padf
## @return [[(*)]]
## @genre list/matrix
## @example Display[PadMatrix[1:5:5]]
## @example ??  1 2 3 4 5
## @example ??  2 3 4 5 0
## @example ??  3 4 5 0 0
## @example ??  4 5 0 0 0
## @example ??  5 0 0 0 0
## @example Display[PadMatrix[1:5:5, 9, PadLeft]]
## @example ??  1 2 3 4 5
## @example ??  9 2 3 4 5
## @example ??  9 9 3 4 5
## @example ??  9 9 9 4 5
## @example ??  9 9 9 9 5
AtState.function "PadMatrix", &lambda { |inst, ragged, pad_with=0, pad_fn=NP|
    pad_fn = default_sentinel(pad_fn, AtState["PadRight"])

    list = inst.cast_list(ragged)
    max_size = list.map { |e|
        AtState["Size"][inst, e]
    }.max

    result = list.map { |ent|
        pad_fn[inst, ent, max_size, pad_with]
    }

    reform_list result, ragged
}
##>>
