# Lambada

Lambada is a functional language heavily inspired by lambda calculus.
The language attempts to be minimalistic, and implements as much as possible within the language itself.

# Plans

## What should it look like?

Values, lambda expressions, and application.

```
double = \x . * 2 x

five = 5

main = double five
```

```
true = \x . \y . x
false = \x . \y . y
if = \p . \x . p x

main = if true 1 0
```

## Features

## Planned features
* Values
* Lambda abstractions
* Application of lambda abstractions on values
* Evaluation of expressions
* Lists and tuples, in Lambada or host language?

## Possible future features
* A way of printing values?
* Shift/reset?
* Try/catch?
* New data types?

