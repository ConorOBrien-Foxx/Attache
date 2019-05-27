# What is Attache?
<!-- meta-index: 0 -->

Attache can be summarized in three particular canons:

 1. Favor syntactic brevity over verbosity
 2. Prefer functional to declarative programming
 3. Minimize boilerplates

In particular, each canon aims to fix problems apparent in other languages:

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

The first thing to note is that Attache, similar to languages like Mathematica, uses square brackets `[` and `]` to call functions. In this example, we are calling the `Print` function, which does what you might expect. It's similar to JavaScript's `console.log` and Python 3's `print` function.

The next thing to note is that strings are written with double quotes `"`. They **cannot** be written with single quotes `'`, as that symbol is actually an operator. Attache has a plethora of operators, and most symbols are, in fact, operators.

### First `n` prime numbers

```attache
n := ReadInt[]
Print[Primes[n]]
```

Here we see how variables are defined: with `:=`. In Attache, what most languages use for variable definition, the equals sign `=`, is used to represent an equality check, which is usually represented by `==` in other languages.

`ReadInt` does what you might expect. It reads an integer from STDIN.

Then, we use the familiar `Print` function to display the result of calling the `Primes` function on `n`. You may have guessed by the title of this subsection that `Primes[n]` returns the first `n` prime numbers. Well, you'd be right.

### That python example from earlier

By that, I mean "mapping increment over the range from 2 to 4"

```attache
Succ => 2:4
?? or
{ _ + 1 } => 2:4
```

I'm introducing a couple of things with this example. First, note that the two examples produce the same result: `[3, 4, 5]`. But, before I begin, note that `?? ...` is a single line comment, and may be placed anywhere on a line.

First, note the use of `:`. This is Attache's range operator. `a:b` produces an inclusive range between `a` and `b`. `2:4` evaluates to `[2, 3, 4]`. The `=>` operator is Attache's map operator. `f => k` maps the function `f` over each element in the list `k`. `Succ` is short for "successor" and represents the natural successor of its argument. For integers, `Succ[x]` is the same as saying `x + 1`.

Pulling it together, `Succ => 2:4` maps the `Succ` function over each element in the list `[2, 3, 4]`, giving us `[Succ[2], Succ[3], Succ[4]]` which evaluates to `[3, 4, 5]`.

Now, as for the second part of this example, I've introduced a lambda `{ _ + 1 }`. All basic lambdas are defined by these brackets `{ ... }`. Here, `_` refers to the first parameter passed to the lambda. This lambda simply adds 1 to its first parameter and returns that result. It could also be written as `{ _1 + 1 }`, being explicit that we want to add 1 to the first parameter. Consequently, we can also have `{ _2 + 1 }`, which adds 1 to the second parameter. These underscore variables are called _blanks_, and usually refer to various kinds of input in Attache.

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

Bear with me, this example is a bit more complicated. I'll outline what we're doing on a higher level first, then we'll dive into the minutia.

First, we defined a variable `range` which is the range of numbers from `1` to `100`. Next, we defined a correspondence between numbers and strings. Multiples of 3, e.g., correspond to "Fizz". Last, we apply this correspondence to each element in `range` and print it out.

See? Not so bad. Now for the nitty-gritty.

`<~ ... ~>` defines a hash; a key-value pair is denoted by `key -> value`.

Then, we convert this hash to a list using the `List` function. This gives us a list of key-value pairs stored in lists `[key, value]`.

Last, we transpose this list of lists, giving us a list of two lists, the first being a list of keys, and the second being a list of values. This value is stored in the variable `map`. In this case, we obtain the list `[[3, 5, 7], ["Fizz", "Buzz", "?!"]]`.

[to be continued]
