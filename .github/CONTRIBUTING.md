# Contributing

You can contribute in a variety of ways, including:

- Finding bugs and posting them on the issue tracker
- Starting feature requests on the issue tracker
- Writing libraries and requesting to merge them through PR's
- Fixing bugs/implementing feature requests with PR's

When doing so, please keep the following in mind:

- Be nice. Disparaging remarks and viewpoints will not be tolerated, and neither will politically charged and inappropriate discussion.
- Be clean. Comment your code to the best of your ability. You don't need to comment every single line, but perhaps the more obscure aspects of your code.
  - I don't care about strict adherence to programming principles, but please keep DRY (do not repeat yourself) in mind while writing. This means you're free to use any programming constructs you wish, regardless of complexity or simplicity.
  - See [Code Style](#code-style) for more information.

## Code Style

_Something not here? Open up an issue and contribute!_

Attache is written in Ruby, so the following principles will apply:

### Variable casing

- Both Attache and Ruby variables and methods should be `snake_case`.
- Constants should be `SCREAMING_SNAKE_CASE`.
- Global variables should either be `$snake_case` or `$SCREAMING_SNAKE_CASE`.
- Classes and builtin Attache functions should be `PascalCase`.

### Blocks

- Always prefer `{` and `}` to `do` and `end`.
- Blocks can be inlined, and should look like:
```ruby
stuff = "hello".chars.map { |e|
    "<#{e}>"
}.join
```

### Indentation

- All indentation should be 4 spaces. This means no tabs. For consistency.
- All blocks should be indented, and indentation should start after the parameters to the block end.

### Lines

- In general, limit lines to around 80 characters. Use common sense.

### Arrays and Hashes

- Arrays which do not exceed around 60 characters should be kept on one line.
- Arrays which exceed this length should be spit up reasonably, as such:
```ruby
# Don't do this:
bar = [19, 90, 63, 64, 23, 36, 38, 37, 74, 6, 4, 97, 48, 60, 50, 74, 43, 73, 93, 21, 37, 71, 37, 51, 20, 43, 10, 52, 51, 96, 45, 95, 5, 42, 48, 65, 74, 63, 84, 24, 85, 31, 36, 8, 65, 6, 10, 93, 92, 3, 7, 20, 83, 41, 37, 77, 54, 78, 56, 26]
# Do this:
bar = [
    19, 90, 63, 64, 23, 36, 38, 37, 74, 6, 4,
    97, 48, 60, 50, 74, 43, 73, 93, 21, 37, 71,
    37, 51, 20, 43, 10, 52, 51, 96, 45, 95, 5,
    42, 48, 65, 74, 63, 84, 24, 85, 31, 36, 8,
    65, 6, 10, 93, 92, 3, 7, 20, 83, 41, 37, 77,
    54, 78, 56, 26
]
```
- Matrices should have each row on its own line, as such:
```ruby
# Don't do this:
foo = [[7, 8, 9, 1, 5, 1, 8, 9, 2, 8], [5, 6, 8, 3, 6, 6, 1, 5, 8, 4], [4, 4, 3, 4, 3, 4, 0, 1, 3, 6], [2, 1, 1, 5, 3, 0, 6, 9, 7, 9], [4, 3, 6, 6, 1, 0, 2, 3, 9, 4], [7, 3, 0, 0, 3, 6, 6, 0, 6, 9], [2, 0, 5, 0, 4, 3, 2, 6, 5, 1], [9, 1, 4, 4, 5, 5, 9, 5, 8, 3], [5, 1, 0, 3, 0, 6, 5, 9, 6, 5], [6, 3, 9, 6, 0, 7, 7, 3, 9, 6]]
# Do this:
foo = [[7, 8, 9, 1, 5, 1, 8, 9, 2, 8],
       [5, 6, 8, 3, 6, 6, 1, 5, 8, 4],
       [4, 4, 3, 4, 3, 4, 0, 1, 3, 6],
       [2, 1, 1, 5, 3, 0, 6, 9, 7, 9],
       [4, 3, 6, 6, 1, 0, 2, 3, 9, 4],
       [7, 3, 0, 0, 3, 6, 6, 0, 6, 9],
       [2, 0, 5, 0, 4, 3, 2, 6, 5, 1],
       [9, 1, 4, 4, 5, 5, 9, 5, 8, 3],
       [5, 1, 0, 3, 0, 6, 5, 9, 6, 5],
       [6, 3, 9, 6, 0, 7, 7, 3, 9, 6]]
```
- Hashes should be split up across multiple lines, and each entry should have a trailing comma:
```ruby
ages = {
    john: 19,
    david: 17,
    marcus: 1983,
}
```