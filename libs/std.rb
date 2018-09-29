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

##<<
## Returns an array of base <code>base</code> digits of the numbers found in
## <code>Range[...args]</code>
## @type base number
## @type args number...
## @return [[number]]
## @genre numeric/bases
## @example Print => BaseRange[2, 0, 10]
## @example ?? [0, 0, 0, 1]
## @example ?? [0, 0, 1, 0]
## @example ?? [0, 0, 1, 1]
## @example ?? [0, 1, 0, 0]
## @example ?? [0, 1, 0, 1]
## @example ?? [0, 1, 1, 0]
## @example ?? [0, 1, 1, 1]
## @example ?? [1, 0, 0, 0]
## @example ?? [1, 0, 0, 1]
## @example ?? [1, 0, 1, 0]
AtState.function "BaseRange", &lambda { |inst, base, *args|
    range = AtState["Range"][inst, *args].map { |el|
        AtState["ToBase"][inst, el, base]
    }
    AtState["PadMatrix"][inst, range, 0, AtState["PadLeft"]]
}
##>>

##<<
## Returns an array of base <code>base</code> digits less than
## <code>base ^ n</code>.
## @type base number
## @type n number
## @return [[number]]
## @genre numeric/bases
AtState.function "BaseBelow", &lambda { |inst, base, n|
    AtState["BaseRange"][inst, base, base**n - 1]
}
##>>

##<<
## Returns an array of base <code>2</code> digits of the numbers found in
## <code>Range[...args]</code>
## @type args number...
## @return [[number]]
## @genre numeric/bases
AtState.function "BinRange", &lambda { |inst, n|
    AtState["BaseRange"][inst, 2, n]
}
##>>

##<<
## Returns an array of base <code>8</code> digits of the numbers found in
## <code>Range[...args]</code>
## @type args number...
## @return [[number]]
## @genre numeric/bases
AtState.function "OctRange", &lambda { |inst, n|
    AtState["BaseRange"][inst, 8, n]
}
##>>

##<<
## Returns an array of base <code>16</code> digits of the numbers found in
## <code>Range[...args]</code>
## @type args number...
## @return [[number]]
## @genre numeric/bases
AtState.function "HexRange", &lambda { |inst, n|
    AtState["BaseRange"][inst, 16, n]
}
##>>

##<<
## Returns an array of base <code>2</code> digits less than
## <code>2 ^ n</code>.
## @type n number
## @return [[number]]
## @genre numeric/bases
AtState.function "BinBelow", &lambda { |inst, n|
    AtState["BaseBelow"][inst, 2, n]
}
##>>

##<<
## Returns an array of base <code>8</code> digits less than
## <code>8 ^ n</code>.
## @type n number
## @return [[number]]
## @genre numeric/bases
AtState.function "OctBelow", &lambda { |inst, n|
    AtState["BaseBelow"][inst, 8, n]
}
##>>

##<<
## Returns an array of base <code>16</code> digits less than
## <code>16 ^ n</code>.
## @type n number
## @return [[number]]
## @genre numeric/bases
AtState.function "HexBelow", &lambda { |inst, n|
    AtState["BaseBelow"][inst, 16, n]
}
##>>

##<<
## Splits <code>x</code> into <code>n</code> groups.
## @type x [(*)]
## @type n number
## @return [[(*)]]
## @genre list
## @example Print[ChopInto[1:6, 3]]
## @example ?? [[1, 2], [3, 4], [5, 6]]
AtState.function "ChopInto", &lambda { |inst, x, n|
    AtState["Chop"][inst, x, AtState["Ceiling"][
        inst, AtState["/"][inst,
            AtState["#"][inst, x],
            n
        ]
    ]]
}
##>>
