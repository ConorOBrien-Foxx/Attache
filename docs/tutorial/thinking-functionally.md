# Thinking Functionally
<!-- meta-index: 1 -->

If you started programming in a language like C or Java (before 1.8), then you're probably used to _imperative_ (or _declarative_) programming. This typically involves constructs such as `if` statements and variants of `while` loops. Typically, this type of programming usually works by building up data from inputs.

Functional programming differs from imperative programming since it's much better at generating data to be mapped and filtered. Attache is more of a functional language, which depends less on modifying the program's state directly with variables and more on using functions to transform data.

To explore the difference, let's write a function to check if a number `n` is prime. In C, you might write something like this:

```
uint isqrt(uint);     // returns floor(sqrt(n))
bool is_prime(uint n) {
    if(n < 2) return false;
    uint upper = isqrt(n);
    for(uint i = 2; i < upper; i++) {
        if(n % i == 0) return false;
    }
    return true;
}
```

You _could_ write a function that uses the same type of constructs, but it'd look different. Attache lacks the `for` loop as a syntactic construct, as well as `if` or `while` loops. Such logic is achieved using functions. Emulating a `for` loop would be quite awkward without devising new functions, and this is an intentional design choice. This is how I would implement a prime-checking function in Attache:

```attache
is_prime[n] :=
    n > 1 and None[2:Sqrt[n] | n]
```

This defines a function `is_prime` which takes a single argument `n` and checks if two conditions hold:

 1. `n > 1`. This covers the corner cases of `0` and `1`, since a prime must be more than `1` (the least prime being `2`).
 2. `None[2:Sqrt[n] | n]` first constructs the range from `2` to `Sqrt[n]` using the range operator `:`. `a | b` operator tests whether or not `a` divides evenly into `b`. Attache vectorizes many arithmetic operators; i.e., operators "reach" into arrays. For example, `[1, 2, 3] + 4` is equivalent to  `[1 + 4, 2 + 4, 3 + 4] = [5, 6, 7]`. So, `2:Sqrt[n] | n` is roughly the same as `[2 | n, 3 | n, ..., Sqrt[n] | n]`. This gives an array of Boolean values (`true` or `false`). Then, `None[...]` checks that no value in the input array is truthy.

In essence, this function asserts that `n` is greater than `1` and that no number in the range from `2` to the square root of `n` divides `n`. This function showcases Attache's minimal syntax:

 - Functions can be defined the simple structure `name[a, b, c, ...] := body`. Compare this to other languages (Java, Python, Ruby, JavaScript) which often require some additional keyword to create a function.
 - No keywords like `return` are necessary to yield the value; it is implied.
 - Vectorizing allows the programmer to omit constructs like maps, overloading otherwise unused behaviour. Tripling each member in an array in most languages is something like `[1, 2, 3].map(e => e * 3)` or `map(lambda x: x * 3, [1, 2, 3])`; in Attache, this expression is simply `[1, 2, 3] * 3`.
 - Operators exist for common operations: `:` for a range, `|` for divisibility, e.g.
