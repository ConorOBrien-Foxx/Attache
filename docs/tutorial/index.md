# What is Attache?

none of your business haha


...jk, this place is a WIP.

```attache
cancel := Difference:Digits

?? could a pair be trivial?
trivial[x, y] :=
        x % 10 = y % 10
    or  y % 10 = 0
    or  Rational[x % 10, y % 10] = x // y

?? does the pair represent a digit cancelling fraction?
dcf := {
    cancelled .= false
    DoSafe[
        cancelled .= cancel@@__
    ]

    cancelled
and #cancelled = 2
and Rational@@cancelled = Rational@@__
}

PE33 := {
    dcf@@_ not trivial@@_
}

pairs := PE33 \ TriangleRange[10, 99]

prod := Prod[&Rational => pairs]

Print[prod.denominator]
```
