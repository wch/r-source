---
title: "Simplified Anonymous Function Notation"
author:
- Luke Tierney
output:
  html_document
---

All functions in R are anonymous: they do not need to be bound to a
variable.  If a function does happen to be bound to one or more
variables, there is no way to recover the names of those variables from a
function object.

Functions in R are written and printed using the syntax

```r
function(x) x + 1
```

It is not uncommon to see comments from new users that this is a lot
to write, especially for very simple functions like this one.  It is
hard to see that _writing_ is really a significant issue: Any
reasonable IDE/editor either already has a shortcut or allows one to
be created. The impact on code readability may be another matter.
Functional programming expressions in particular, like

```r
lapply(y, function(t) is.null(t) || anyNA(t))
```

or

```r
Reduce(function(u, v) u + 1 / v, x, right = TRUE)
```

might be more readable with a more concise notation.

The tidyverse has introduced a formula-based notation. The `gsubfun`
package uses another formula-based approach. And there are probably
others. But these only work within the functions for which they are
designed.

An alternative worth exploring is for base R to support a concise
syntax in the parser. Ideally such a syntax should be able to express
any function, allowing for multiple arguments, default arguments, and
`...`, though it would be most useful for simple functions.

Three possibilities we have considered that were mentioned in my _useR
2020_ keynote are

<!-- From Tomas:
https://en.wikipedia.org/wiki/Anonymous_function
-->

1. `(x) => x + 1`, `(x, y) => x + y` ([Javascript](https://en.wikipedia.org/wiki/Anonymous_function#JavaScript); similar to [Julia](https://en.wikipedia.org/wiki/Anonymous_function#Julia)).
2. `\(x) x + 1`, `\(x, y) x + y` (similar to [Haskell](https://en.wikipedia.org/wiki/Anonymous_function#Haskell); `\` simulates $\lambda$).
3. `@(x) x + 1`, `@(x, y) x + y` ([Matlab, Octave](https://en.wikipedia.org/wiki/Anonymous_function#MATLAB,_Octave)).

All three have been implemented in the [`R-syntax` subversion
branch](https://svn.r-project.org/R/branches/R-syntax).

The second and third are the most concise, perhaps too concise.
But they require only minimal parser changes.

The first option requires more extensive parser changes. My first
attempt was not good, but Andrew Craig modified the `R` 3.6.3 `gram.y`
to make (x) => x + 1 work
([tweet](https://twitter.com/andrew_cb2/status/1282969147366760449), [Tokyo.R talk](https://speakerdeck.com/andycraig/x-equals-x-plus-1),
[code on GitHub](https://github.com/andycraig/r-anonymous-functions)),
and I ported his changes to the current parser. These changes do seem
to work, but my concern is that they may impose too great a burden on
maintaining the parser.

All of these are only syntactic sugar for the longer version using `function`.
The parsed AST is the same:

```r
> quote((x) => x + 1)
function(x) x + 1
> quote(\(x) x + 1)
function(x) x + 1
> quote(@(x) x + 1)
function(x) x + 1
```

Parse data and source references do reflect the way the expressions
were entered. This is analogous to the way right assignment `->` is
handled:

```r
> quote(1 -> x)
x <- 1
```

Editor support, e.g. in RStudio or ESS, and code formatting tools,
e.g. `lintr` and `styler` would need to be updated for the new surface
syntax. One drawback of the backslash notation is that it will make
these updates more error-prone.
