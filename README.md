λ-llama
=======

![lambda-llama](https://secure.gravatar.com/avatar/11ff8bcc12c392ad337115ca30a38fc1?s=250)

## The task

This year the task was to guess a number of programs in a language called `\BV`. The
organizers set up a web server, which acted as a black box, answering queries of two
types:

* Tell me the outputs of program X on inputs `[1,2,3,...]`.
* Does program X look like `(lambda (x) x)`?

Each program was given a **5 minute** time limit, which started ticking after
any of the queries above. Bellow is the description of `\BV` language syntax and
semantics:

```
 program    P ::= "(" "lambda" "(" id ")" e ")"
 expression e ::= "0" | "1" | id
               | "(" "if0" e e e ")"
               | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
               | "(" op1 e ")"
               | "(" op2 e e ")"
          op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
          op2 ::= "and" | "or" | "xor" | "plus"
          id  ::= [a-z][a-z_0-9]*
```

The language operates on 64-bit vectors (thus the name bit lambda) and has only
two constants 0 and 1. All operators have the usual bitwise semantics, with `fold`
being a notable exception.

* A valid program in `\BV` can have **at most** one fold,
* Fold works per-byte, example:

  ```lisp
  P = (lambda (x) (fold x 0 (lambda (y z) (or y z))))

  P(0x1122334455667788) = (or 0x0000000000000011
                          (or 0x0000000000000022
                          (or 0x0000000000000033
                          (or 0x0000000000000044
                          (or 0x0000000000000055
                          (or 0x0000000000000066
                          (or 0x0000000000000077
                          (or 0x0000000000000088
                              0x0000000000000000))))))))
  ```

For any problem in `\BV` we can define *size*

```
                             |0| = 1
                             |1| = 1
                             |x| = 1
                |(if0 e0 e1 e2)| = 1 + |e0| + |e1| + |e2|
|(fold e0 e1 (lambda (x y) e2))| = 2 + |e0| + |e1| + |e2|
                      |(op1 e0)| = 1 + |e0|
                   |(op2 e0 e1)| = 1 + |e0| + |e1|
                |(lambda (x) e)| = 1 + |e|
``

and *operator set*

```
                             Op 0 = {}
                             Op 1 = {}
                             Op x = {}
                Op (if0 e0 e1 e2) = {"if0"}  U Op e0 U Op e1 U Op e2
Op (fold e0 e1 (lambda (x y) e2)) = {"fold"} U Op e0 U Op e1 U Op e2
                      Op (op1 e0) = {op1}    U Op e0
                   Op (op2 e0 e1) = {op2}    U Op e0 U Op e1
```

Summing up, a team was given a number of randomly generated programs to guess.
Programs varied in complexity, which was indicated by program size and operator
set. Example:

```json
{"id":"dKdeIAoZMyb5y3a74iTcLXyr",
 "size":30,
 "operators":["shr16","if0","xor","plus","not","fold"]},
```

## The [solver](https://github.com/superbobry/icfpc2013/blob/master/bin/Submitter.hs#L123)

Unfortunately, we were too late to realize that most of the programs aren't
very big in size (even after the hint was [posted](http://icfpc2013.cloudapp.net)
in one of the updates), thus:

### Brute-force

Just like everybody else we started with brute-force and dynamic programming:

1. generate all programs bellow a given size,
2. test **all** of them on random inputs,
3. pick only the programs, which produced correct outputs,
4. keep excluding incorrect programs, based on the received counterexamples.

The approach worked well on small problems but was useless on problems of
bigger size (>= 10) because of the exponential growth of the number of cases
to consider.

### Reducing search space

#### Idea #1: equivalence

Any problem of smaller size can be written as an equivalent problem of a
bigger size with some redundant operations added. So the idea was to
[find] [isNotRedundant] these redundant problems during dynamic programming
and drop the bigger problem **early**. This was done by a set of rules,
some obvious, like [De Morgan laws] [demorgan], others not so obvious.

The trick is to use this set of rules efficiently. The straightforward
approach (compare each pair of generated programs for equivalence) has
an obvious drawback: it requires quadratic number of comparisons. Key
observation for improving the running time is that **if** there is a
program which **can** be simplified, it must be [discarded] [filterRedundant].
Since we generate all problems, increasing the size on each step, a
simpler version (with smaller program size) **must** have been
generated already.

*Note*: in all of the rules bellow we treat two terms as **equivalent** if
they are either equal syntactically or end up in the same state after
abstract interpretation (see next section).

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

##### logical-op-not-zero

Collapse logical binary operations with `(not 0)`.

```lisp
(and e (not 0))  = e
(or e (not 0))   = (not 0)
(xor e (not 0))  = (not e)
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
```

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

[filterRedundant]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Gen.hs#L31
[isNotRedundant]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Simplifier.hs#L42
[demorgan]: http://en.wikipedia.org/wiki/De_Morgan's_laws

#### Idea #2: [abstract interpretation] [seval]

Sometimes, we can [tell] [like] if the two expressions are equivalent even
if they aren't closed (i. e. contain unknown variables) by interpreting them
on **abstract** bit vectors and comparing final states.

An abstract bit vector is a vector, where each bit can be in four possible
[states] [Sbit]:

* zero or one, just like ordinary bits,
* `Bot` aka undefined,
* [fresh] [fresh], i. e. yet untouched by binary bitwise operations. Fresh
  bits are indexed, for example an 8-bit abstract bit vector will be
  initialized as:

  ```
  [B 1, B 2, B 3, B 4, B 5, B 6, B 7, B 8]
   ^
   |
   each bit is fresh
  ```
  Note, that, the above example is **only** the case for a newly initialized
  bit vector, because after a shift the `i`-th position in a bit vector
  no longer contains a fresh bit, indexed with `i`.

Each bitwise [operation] [operations] on abstract bit vectors changes the
bits in a **special** way, for example, [`andBit`] [andBit] works as the usual
bitwise `&&` with two exceptions:

* if any of the bits is `Bot` the resulting bit is also `Bot`,
* if any of the bits are fresh with different indexes the resulting bit is `Bot`.

Similar exceptions can be formulated for the remaining operations.

[seval]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/SEval.hs#L16
[fresh]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/Types.hs#L10
[like]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/SEval.hs#L61
[sbit]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/Types.hs#L8
[operations]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/Operations.hs
[andBit]: https://github.com/superbobry/icfpc2013/blob/master/src/Language/BV/Symbolic/Operations.hs#L91

#### Unused ideas

* Translate BV into an [extended][NBV] representation, which allows n-ary
  functions.
* Improve abstract interpreter to store a set of boolean expressions,
  for each `Bot` bit. That way we can compare two abstract bit vectors
  by comparing the corresponding boolean expression, even if some or all
  bits are undefined.
* Pre-compute small-sized programs and build up on that.

[NBV]: https://github.com/superbobry/icfpc2013/tree/master/src/Language/NBV
