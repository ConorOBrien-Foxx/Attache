# What is Attache?
<!-- meta-index: 0 -->

Attache can be summarized in three particular canons:

 1. Favor syntactic brevity over verbosity
 2. Prefer functional to declarative programming
 3. Minimize boilerplates

In particular, each canon reflects problems apparent in other languages:

 1. Languages like Python and C++ tend to be burdened by their awkward approaches to functional programming. Simple constructs like maps and lambdas are rendered unwieldly. Consider `map(lambda x: x + 1, range(2, 5))` in Python, or some ugly use of `std::transform` in C++. Such languages tend to lack the necessary syntax for convenient functional programming.
 2. Functional programs tend to be closer to how we process information. This is no fault of declarative programming (D does declarative programming well, as far as intuition is concerned), but rather a strength of functional programming.
 3. Java. 'Nuff said.

So, how does Attache address these problems?

 1. Introduce more operators than the regular arithmetic ones, to include functional operators. Syntax that enables functional programming.
 2. Functions. Functions _everywhere_.
 3. It's not Java. Attache has no "main" function besides the immediate context.

## What does Attache look like?

Let's look at a few common programs.

### Hello, World!

```attache
Print["Hello, World!"]
```

### First `n` prime numbers

```attache
n := ReadInt[]
Print[Primes[n]]
```

### That python example from earlier

By that, I mean "mapping increment over the range from 2 to 4"

```attache
{ _ + 1 } => 2:4
```

### Custom Fizzbuzz

```attache
range := 1:100
?? modify below to customize the output
?? format: `multiple -> text`
map := Tr! List[<~
    3 -> "Fizz",
    5 -> "Buzz",
    7 -> "?!"
~>]

range | { Join[Mask[map[0] | _, map[1]]] or _ } | Print
```
