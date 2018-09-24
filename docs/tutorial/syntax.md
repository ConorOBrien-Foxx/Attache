# Syntax in Attache
<!-- meta-index: 10 -->

This page is meant to document the syntax of Attache as a reference.

## Data

Numbers are as you might expect, with a few, language-specific quirks:

```attache
3           ?? integer
4.3         ?? system float
7x          ?? extended precision
2-4i        ?? complex number with integer parts
3.4+2.9i    ?? complex number with float parts
9x+12ix     ?? complex number with extended precision parts
3//5        ?? fraction
```

There are various types of strings:

```attache
"Hello!"            ?? normal string
"\x43"              ?? normal string with hex escape
$"sum = ${3 + 4}"   ?? interpolated string
```


## Operators

Operators are either **unary** or **binary** (aka: monadic and dyadic). All operators can exist in either form, even though they may lack definitions for those forms. For example, while `%` is only defined dyadically, the monadic syntax is still available.

## Defining variables

Early versions of Attache used the `Define` and `Local` functions to work with variables. Now, `:=` and `.=` serve those purposes. Usually, a variable assignment looks like this:

```attache
age := 14
```

Here is an example showing the difference between `:=` (global assignment) and `.=` (local assignment):

```attache
x := 100        ?? declared in the global scope
{               ?? begin a new scope
    Print[x]    ?? 100, global x
    x .= 32
    Print[x]    ?? 32, local x    
}[]
Print[x]        ?? 100, global x
```

You can define functions in a few different ways. The following are all equivalent ways of expressing `f(x, y) = x + 2y`:

```attache
f1 := { _ + 2*_2 }
f2 := ${ x + 2*y }
f3[x, y] := x + 2*y
```

`f1` and `f2` are different versions of lambdas, while `f3` is an explicit definition. Under the hood, they all do the same thing.

You can overload operators:

```attache
?? test function
test := {
    If[#__ = 1, ?? if unary
        $"Unary: ${_}",
        $"Other: ${Join[__, "; "]}"
    ]
}

?? overload both arities of an operator
`~ := test

Print[1 ~ 4]    ?? Other: 1; 4
Print[~"q"]     ?? Unary: q

?? overload only one arity of an operator
`!/2 := test    ?? overload binary factorial
`+/1 := test    ?? overload unary plus

Print[!3]       ?? 6
Print[3!4]      ?? Other: 3; 4
Print[+4]       ?? Unary: 4
Print[3+4]      ?? 7
```
