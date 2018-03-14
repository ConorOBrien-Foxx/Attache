# Attache

Attache (pronounced "ah-tahsh") is a language that looks a lot like Mathematica. In practice, it is like Mathematica, but "faster" (that is, less capable), and also enjoys a bit less verbosity.

## What Attache looks like


```
(* This
   is
   a
   multiline
   comment
*)

?? this is also a comment

?? I/O
name := Prompt["What's your name? "]
Print["Hello, " + name + "!"]

words := Split[Prompt["I like words, can I have some?\n"]]

Print["Words", after->": "]
Print[...words, joiner->" & "]

?? iterative
ForEach[words,
    Print["The word at position", _2, "is", Repr[_1]]
]

?? recursion
factorial[x] := If[x < 2, 1, factorial[x - 1] * x]
Print[factorial[6]]

?? functional
numbers := Range[1, 3]
Map[Print, numbers]
Print => numbers
Print[factorial => 0:6]
Print => V#factorial => 0:6

?? partially applied functions
truthy := If<~ _, "this is true", "this is false"~>

Print[truthy[0]]
Print[truthy[4]]
```