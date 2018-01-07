# Attache

Attache (pronounced "ah-tahsh") is a language that looks a lot like Mathematica. In practice, it is like Mathematica, but "faster" (that is, less capable), and also enjoys a bit less verbosity.

## Example programs

Hello, World!
```Mathematica
Print["Hello, World!"]
```
1000th prime number
```Mathematica
Print[Prime[1000]]
```
Reverse STDIN
```Mathematica
Stdout[Reverse[Stdin[]]]
```
Select only positive integers from a list
```Mathematica
Define[$positive, `>&0]
Define[$list, V[-42, -10, -2, 0, 3, 6, 123]]
Print[positive \ list]
Print[`>&0 \ list]
Define[$posfilter, Bond[`\, `>&0]]
Print[posfilter[list]]
```