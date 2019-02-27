# Lambada

## Description
Lambada is a functional language heavily inspired by lambda calculus.

* The language attempts to be minimalistic, and implements as much as possible within the language itself.
* The syntax is very similar to Haskell, meaning that existing syntax highlighting should work.

## Building

With Stack
```
$ stack build
```

You can then run it with
```
$ stack exec lambada-exe
```

Or if you don't want to use Stack, you can build the executable with GHC and the makefile
```
$ make
```

## What does it look like?

Values, lambda expressions, and application.

```haskell
let double = \x . * 2 x in
let five = 5 in
double five

5
```

Basic boolean logic implementation.

```haskell
let true = \x . \y . x in
let false = \x . \y . y in
let not = \p . \x . \y . p y x
let if = \p . \x . p x in
if not true 1 0

0
```
## Features

### Planned features
- [x] Values
- [x] Lambda abstractions
- [x] Application of lambda abstractions on values
- [x] Let-expressions
- [x] Evaluation of expressions
- [ ] Data types like lists, tuples and records
- [ ] Type checking

### Ideas
* A way of printing values?
* New data types?
