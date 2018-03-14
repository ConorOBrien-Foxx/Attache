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

Print["Hello, World!"]

factorial[x] := If[x < 2, 1, factorial[x - 1] * x]

Print[factorial[6]]
```