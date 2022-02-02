---
title: "Protecting the BC Stack from Mutation"
author: Luke Tierney
output: html_document
---

## Background

In computing an expression like

```r
<expr1> + <expr2>
```

the value of `<expr1>` is computed first and then the value of `<expr2>`.
It is possible, though unusual, that the expression for computing
`<expr2>` might contain complex assignments. These assignments must not
mutate the computed value of `<expr1>`. In R 3.4.4 this was not handled
properly and this example produced the wrong result:

```r
x <- 1 + 0
x + { x[1] <- 2; 1}
## [1] 3  # should be 2
```

This issue was present in all calls to multi-argument `BUILTIN`
functions from the AST interpreter.  A similar issue existed in subset
operations, implemented by a `SPECIAL`:

```r
x <- matrix(1:4, 2)
i <- 1 + 0
x[i, { i[1]<-2; 1}]
## [1] 2  # this is m[2, 1]; should be 1
```

Similar issues also existed in byte compiled code where a complex
assignment could mutate values on the stack from earlier computations.

A variant of this issue was reported to the R-devel list by Lukas
Stadler in August 2017.


## Fixes in R 3.5.0 and R 3.6.0

These issues were partially addressed in R 3.5.0 and more completely
in R 3.6.0. The issue for `BUILTIN` calls is handled in `eval.c` and
needs no further action or maintenance elsewhere.

Implementation changes to `SPECIAL` functions that evaluate more than
one argument need to pay attention to this issue and make sure values
computed earlier cannot be mutated while evaluating later
arguments. Ths should be done using `INCREMENT_LINKS` and
`DECREMENT_LINKS`, e.g. for two arguments as

```c
val1 = eval(arg1, rho);
INCREMENT_LINKS(val1);
val2 = eval(arg2, rho);
DECREMENT_LINKS(val1);
```


## Changes in R 4.0.0

The fix for protecting the byte code stack used in R 3.6.0 required
the compiler to emit additional instructions protecting and
unprotecting values on the stack. This note describes an alternative
approach with less overhead that will be incorporated in R 4.0.0.
(Committed to R_devel in r77275.)

Legitimate mutations through R code can only occur in a few places:

- In byte code between `STARTASSIGN/ENDASSIGN` or
  `STARTASSIGN2/ENDASSIGN2` instructions.

- In calls to `applyDefine` from the AST interpreter.

The byte code interpreter also mutates values on the stack in a few
situations where this is known to be safe. One place is- in updating
the value of the loop variable in `for` loops for some data
types. This is safe for values that are not shared as they cannot also
appear lower on the stack.

To make sure that these operations do not mutate any boxed value on
the BC stack all values on the stack need to have their link counts
(NAMED or REFCNT) incremented and then decrement around these possible
mutation points.  This is done by calling

- `INCLNK_stack` before entering possibly mutating code;
- `DECLNK_stack` after leaving this code.

These functions use a stack pointer `R_BCProtStack` to keep track of
how much of the stack has been protected. The pointer is stored in
contexts and used to decrement link counts on a LONGJMP.

A key assumption is that none of the values on the protected stack
will be changed between matching `INCLNK_stack` and `DECLNK_stack`
operations. The binding cache and the variable value for a `for` loop
may need to change, but these also do not need to be protected. Their
stack fields are marked with the `NLNKSXP` tag so they will not have
their links incremented and decremented.

The stack is protected around every call to `applydefine`. This
ensures that values on the stack are protected during complex
assignment operations during calls into the AST interpreter.

To reduce link count adjustments for objects lower on the stack, the
stack protection pointer adjustment during byte compiled complex
assignments has several components:

- The stack protection pointer is raised at the beginning of a
  `bcEval` call and restored at the end. This means that top level
  complex assignments in byte compiled code need no further
  protection.
	
- The protection pointer does need to be raised around non-top-level
  complex assignments. This is done by `INCLNKSTK` and `DECLNKSTK`
  instructions emitted by the compiler for non-top-level complex
  assignments.

Together these ensure that the stack is protected during all byte
compiled complex assignments.

To further reduce unnecessary link count adjustments:

- For `for` loops the stack protection pointer is adjusted around the
  loop body to avoid processing the loop data for every stack
  protection pointer adjustment needed while executing the body.

- Similarly, the stack protection pointer is adjusted for loops
  needing a jump context to avoid repeated processing of the context
  data.

A final efficiency improvement is to defer actually committing the
link count increments until they are needed. This means that code
written in a functional style that does not use complex assignments
does not need to actually perform the link count adjustments.  The
points where count increments are committed are

- in `applydefine` for the AST interpreter;

- in the `STARTASSIGN` and `STARTASSIGN` instructions.

**Raising the minimal BC level will allow redoing the binding cache to
be much smaller, and allowing the non-small-cache option to be
dropped. The hacks for supporting old byte code can also be dropped at
that point.**
