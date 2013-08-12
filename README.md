λ-llama
=======

## The team

* [Aleksey Kladov](https://github.com/matklad)
* [Pavel Yakovlev](https://github.com/zmactep)
* [Sergei Lebedev](https://github.com/superbobry)

## The solver

Unfortunately, we were too late to realize that most of the programs aren't
very big in size (even after this hint was [posted](http://icfpc2013.cloudapp.net)
in one of the updates), thus:

### Brute-force

Just like everybody else we started with brut-force and dynamic programming:

1. generate all programs bellow a given size,
2. test **all** of them on random inputs,
3. pick only the programs, which produeced correct outputs,
4. keep dropping incorrect programs, based on the received counter-examples.

This worked well on small problems but was almost useless on problems bigger
than 10 because of the exponential growth of the number of problems to
consider.

### Reducing search space

#### Idea #1: equivalence

Any problem of smaller size can be written as an equivalent problem of a
bigger size with some redundant operations added. So the idea was to
[find] [isNotRedundant] these redudant problems during dynamic programming
and drop the bigger problem **early**. This was done by a set of rules,
some obvious, like [De Morgan laws] [demorgan], others not so obvious.
In all of the rules above we treat two terms as equivalent if they
are either equal syntatically or end up in the same state after abstract
interpretation (see next section).

##### not-not

Eliminate repeated `not` statements.

```lisp
(not (not e)) = e
```

##### collapse-shifts

Collapse repeated smaller shifts into bigger shifts.

```lisp
(shr1 (shr1 (shr1 (shr1 e)))) = (shr4 e)
(shr4 (shr4 (shr4 (shr4 e)))) = (shr16 e)
```

##### plus-shift

Replace repeated plus of **equivalent** expressions with a shift.

```lisp
(plus e1 e2) = (shl1 e1) iff e1 ~ e2
```

##### trivial-if

Simplify if with trivial condition or **equivalent** branches.

```lisp
(if0 e0 e1 e2) = e1 iff e1 ~ e2 || e0 ~ 0
(if0 e0 e1 e2) = e2 iff e0 ~ e1 || e0 ~ 1
```

##### binary-op-zero

Collapse binary operations with 0.

```lisp
(or e 0)   = e
(xor e 0)  = e
(and e 0)  = 0
(plus e 0) = e
```

##### not-zero

Collapse binary operations with `(not 0)`.

```lisp
(and e (not 0))  = e
(or e (not 0))   = (not 0)
(xor e (not 0))  = (not e)
(plus e (not 0)) = (not e)
```

##### binary-op-equal-operands

Collapse binary operations with equal operands.

```lisp
(and e e) = e
(or e e)  = e
(xor e e) = 0
```

##### invalid-order-operands

Impose a total order on operands of binary operators, so that we can ignore
commutativity.

##### if-common-start

Simplify if with branches, starting with **equivalent** subexpressions.

```lisp
(if0 e0 e1 e2) = (if0 e0 e1' e2')
  where
    e1' and e2' lack the equivalent prefix.
```

##### closed-fold

Simplify fold which doesn't use its arguments or it's **equivalent** to
the initial value.

```lisp
e = (fold e0 e1 (lambda (y z) e3)) = e3 iff y ∉ FV(e3)
                                         || e3 is a closed term
                                         || e ~ e1

##### equivalent-to-known-constant

Simplify terms **equivalent** to simple constants.

```
0 1 x
(not 0) (not 1) (not x)
(shr16 x)
(shr16 (not 1))
```

##### clear-last-bit

Clear the last bit with less operations.

```
(or (and (not 1) e)) = (shr1 (shl1 e))
```

[isNotRedundant]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Simplifier.hs#L42
[demorgan]: http://en.wikipedia.org/wiki/De_Morgan's_laws

#### Idea #2: abstract interpretation

Sometimes, we can [tell] [like] if the two programs are equavalent even if
they aren't closed (i. e. contain unknown variables) by interpreting them
on **abstract** bit vectors and comparing final states.

An abstract bit vector is a vector, where each bit can be in four possible
[states] [Sbit]:

* zero or one, just like ordinary bits,
* undefined or `Bot`,
* fresh, i. e. yet untouched by binary bitwise operations. Fresh bits are
  indexed, for example a 8-bit abstract bit vector will be initialized as:

  ```
  [1|2|3|4|5|6|7|8]
   ^
   |
   each bit is fresh
  ```
  Note, that, the above example is only the case for a newly initialzed
  bit vector, because after a shift the `i`-th position in a bit vector
  no longer contains a fresh bit, indexed with `i`.

Each bitwise [operation] [operations] on abstract bit vectors changes the
bits in a special way, for example, [`andBit`] [andBit] works as the usual
bitwise `&&` with two exceptions:

* if any of the bits is `Bot` the resulting bit is also `Bot`,
* if any of the bits are fresh with different indexes the resuling bit is `Bot`.

[like]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/SEval.hs#L61
[sbit]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/Types.hs#L8
[operations]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/Operations.hs
[andBit]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/Operations.hs#L91

#### Unused ideas

* Translate BV into an extended representation, which allows n-ary functions.
* Search for solution **while** generating problems (implicitly assumes that
  the solution is not very big, which turned out to be a correct assumption).
