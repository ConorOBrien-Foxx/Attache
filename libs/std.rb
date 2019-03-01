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

##<<
## Returns the distance between points <code>p1</code> and <code>p2</code> on the
## Euclidean plane.
## @type p1 [(*)]
## @type p2 [(*)]
## @return number
## @genre number
## @example Print[Distance[ [1, 2], [1, 4] ]]
## @example ?? 2.0
## @example Print[Distance[ Point[0, 0], Point[5, 12] ]]
## @example ?? 13.0
## @example Print[Distance[ -3-4i, 3+4i ]]
## @example ?? 10.0
## @example Print[Distance[ [0, 0], [1, 1] ]]
## @example ?? 1.4142135623730951
AtState.function "Distance", &lambda { |inst, p1, p2|
    AtState["Norm"][inst,
        AtState["-"][inst, p1, p2]
    ]
}
##>>

##<<
## Returns the norm of <code>x</code>.
## @type x [(*)]
## @return number
## @genre list
## @example Print[Norm[3'4]]
## @example ?? 5.0
## @example Print[Norm[-2+1i]]
## @example ?? 2.23606797749979
AtState.function "Norm", &lambda { |inst, x|
    list = inst["Flat",
        inst["CellReplace",
            AtState["IsImaginary"],
            AtState["ReIm"],
            x
        ]
    ]

    inst["Sqrt",
        inst["Sum",
            inst["^",
                list,
                2
            ]
        ]
    ]
}
##>>

##<<
## Replaces all cells in <code>list</code> matching <code>cond</code> with <code>with</code>.
## @curries
## @type cond (*)
## @type with (*)
## @type list [(*)]
## @return [(*)]
## @genre list
## @example mat1 := Identity[4]
## @example Display[CellReplace[0, 9, mat1]]
## @example ??  1 9 9 9
## @example ??  9 1 9 9
## @example ??  9 9 1 9
## @example ??  9 9 9 1
## @example mat2 := Integers[3, 3]
## @example Display[CellReplace[Odd, Square, mat2]]
## @example ??  0  1  2
## @example ??  9  4 25
## @example ??  6 49  8
## @example Print[CellReplace[Positive, `-, -4:4]]
## @example ?? [-4, -3, -2, -1, 0, -1, -2, -3, -4]
AtState.function "CellReplace", &curry { |inst, cond, with, list|
    cond = inst["MakeFunction", cond, AtState["=="]]
    with = inst["MakeFunction", with]

    inst["@>",
        lambda { |inst, el|
            if AtState.truthy? cond[inst, el]
                with[inst, el]
            else
                el
            end
        },
        list
    ]
}
##>>

##<<
## Returns <code>x</code> if it is a function, otherwise returns a constant function
## returning <code>x</code>.
## @type x (*)
## @type bonder fn
## @optional bonder
## @param bonder If specified, returns a function with <code>x</code> as its left argument instead of a constant function.
## @return fn
## @genre functional
AtState.function "MakeFunction", &lambda { |inst, x, bonder=NP|
    bonder = default_sentinel(bonder, lambda { |inst, e, *args|
        e
    })

    inst["typeof", x] == Type::FUNCTION ? x : inst["&", x, bonder]
}
##>>

##<<
## Returns the real and imaginary portions of <code>n</code>.
## @return [number]
## @type n (*)
## @genre numeric
AtState.function "ReIm", &lambda { |inst, n|
    [inst["Re", n], inst["Im", n]]
}
##>>

##<<
## Returns a function which applies <code>f</code> to the grid of charcters in a string. This returns a string.
## @type f fn
## @return fn
## @genre functional/string
## @example succEach := OverGrid[@Succ]
## @example Print[succEach["hello\nworld"]]
## @example ?? ifmmp
## @example ?? xpsme
AtState.function "OverGrid", &lambda { |inst, f|
    lambda { |inst, arg|
        grid = inst["Grid", arg]
        result = f[inst, grid]
        fitted = inst["UnGrid", result]
    }
}
##>>

##<<
## Returns a function which applies <code>f</code> to the charcters in a string. This returns a string.
## @type f fn
## @return fn
## @genre functional/string
## @example doubleEach := OverChars { _ * 2 }
## @example Print[doubleEach["Hello!"]]
## @example ?? HHeelllloo!!
AtState.function "OverChars", &lambda { |inst, f|
    lambda { |inst, str|
        chars = inst["Chars", str]
        result = f[inst, chars]
        fitted = inst["Join", result]
    }
}
##>>

##<<
## Returns the minimum and the maximum of <code>list</code> in a 2-element array.
## @type list [(*)]
## @return [(*), (*)]
## @genre list
## @example list := [1, 9, 3, -5, 0, 0, 12]
## @example Print[MinMax[list]]
## @example ?? [-5, 12]
AtState.function "MinMax", &lambda { |inst, list|
    list.inject([Infinity, -Infinity]) { |(min, max), el|
        [
            el < min ? el : min,
            el > max ? el : max
        ]
    }
}
##>>

##<<
## Returns all positive integers which divide <code>n</code> which are also
## less than or equal to <code>Sqrt[n]</code>.
## @type n number
## @return [number]
## @genre numeric
AtState.function "RawDivisors", &lambda { |inst, n|
    (1..inst["Sqrt", n]).select { |i|
        inst["|", i, n]
    }
}
##>>

##<<
## Returns all positive integers which divide <code>n</code>.
## @type n number
## @return [number]
## @genre numeric
AtState.function "Divisors", &vectorize_monad { |inst, n|
    divisors = inst["RawDivisors", n]
    divisors + divisors.reverse.map { |e|
        inst["/", n, e]
    }
}
##>>
