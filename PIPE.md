---
title: "A Native Pipe Operator"
author:
- Luke Tierney
output:
  html_document
---

The `magrittr` pipe operator is now widely used, in particular with the
tidyverse. Several other languages have also adopted, or are
considering adopting, a similar operator.
[Discussions](https://github.com/tc39/proposal-pipeline-operator) of
[proposals](https://github.com/tc39/proposal-pipeline-operator/wiki)
for JavaScript are interesting and also provide links to what is done
or proposed for some other languages.

<!-- some other links:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Pipeline_operator
https://www.digitalocean.com/community/tutorials/js-pipeline-operator
https://medium.com/linebyline/javascripts-new-pipeline-operator-2845bbf88b05
-->

This may be a good time to consider whether a pipe operator of some
sort should be incorporated in the base R language. In addition to
allowing a slightly more compact operator notation by adding a new
operator at the parser level, this may provide an opportunity for
addressing some concerns about the current `magrittr` pipe.

The idea of including a pipe operator in base R was suggested by Jim
Hester, among other things, in his [DSC 2017
talk](https://rawgit.com/jimhester/presentations/master/2017_07_03-DSC2017-Syntax_Extensions_To_R/2017_07_03-DSC2017-Syntax_Extensions_To_R.html).
This proposed implementing the pipe as a syntax transformation in the
parser.  The [original
implementation](https://github.com/lionel-/r-source/compare/7e2a6a8...2c774a17f4)
was from Lionel Henry.

The idea of adding a native pipe operator to base R was also raised
more recently by Antoine Fabri in a [thread on the R-devel mailing
list](https://stat.ethz.ch/pipermail/r-devel/2019-October/078520.html)
that contains some interesting discussions.


## Some Issues with the `magrittr` Pipe

### Implicit Passing of LHS As First Argument

By default the `magrittr` pipe operates in a way that can be viewed as
passing the LHS result to the RHS call by inserting it as the first
argument to the call. This is a very simple rule, which makes it easy
to understand. But it has to be used with care if named arguments are
involved. An example where named arguments can help is if `merge` or
`left_join` is to be called with the LHS passed as the second
argument:

```r
Y %>% left_join(x = X)
```

is equivalent to

```r
left_join(X, Y)
```

In other situations this could potentially lead to errors that are
hard to recognize.  Intentionally taking advantage of this it probably
a bad idea.  It is not clear how often unintentional use occurs and
leads to errors.

Another drawback of implicit argument passing is that it makes the
actual computation of a pipeline stage harder to understand since an
invisible argument has to be mentally added to the call. This creates
additional cognitive load for debugging and for someone learning to
use functions in both a pipe context and a non-pipe context.

The LHS placeholder `.` could be used to make the LHS argument more
visible, but, as this is not required, encouraging this would likely lead to
inconsistent code with `.` used some of the time and not other times.


### Explicit LHS Placeholder Issues

`magrittr` does provide the `.` placeholder, primarily for cases where
the LHS needs to be passed as an argument other than the first. The
choice of `.`, the smallest glyph available, for identifying the pipe
stages that are non-standard is less than ideal. For example, seeing
where `.` is used here is quite hard:

```r
rename(airports,
       dest = faa, dest_alt = alt) %>%
    select(dest, dest_alt) %>%
    left_join(flights, ., "dest") %>%
    filter(dest_alt > 6000) %>%
    lm(arr_delay ~ dep_time, data = .)
```

Supporting a LHS placeholder in a RHS call raises some issues:

- Can the placeholder appear more than once? If so, a substitution
  implementation is not possible since this would cause the LHS
  expression to be evaluated more than once.

- Can the placeholder appear only as a top-level argument or inside an
  argument expression? If it can appear within an argument expression
  it will need to obey R's scoping rules, which limits implementation
  options.

Supporting a LHS placeholder _in addition_ to the implicit first
argument convention, as `magrittr` does, raises the question of when
the first argument convention is suspended. It appears that `magrittr`
suspends the first argument convention only if the placeholder appears
as a top-level argument:

```r
> 1 %>% c(2)
[1] 1 2
> 1 %>% c(., 2)
[1] 1 2
> 1 %>% c(c(.), 2)
[1] 1 1 2
```


### Implementation Issues

The `magrittr` implementation seems to have more overhead than needed,
making stack traces messier than they need to be. Jim Hester's slides
show an example that illustrates how a pipe implemented as a syntax
transformation is one way to improve this.

The current implementation creates additional referenced to LHS
objects. This may force copying of possibly large objects in pipe
stages that otherwise could perform mutations in place. Again an
implementation that transforms a pipe into a set of nested calls,
together with the recent change to reference counting for determining
mutability, can improve this.  It is not clear how important this
currently is in practice, but there may be cases where it matters now,
and it might matter more in the future as more optimizations are
introduced.


### Relation to UNIX Pipes

The use of the terms _pipe_ and _pipeline_ is unfortunate. It is often
claimed that the `magrittr` pipe is just like the UNIX pipe.  This is
not true. UNIX pipes are a very different animal: they deal with
streaming data and enable parallel processing of pipe stages. If we
ever wanted to add streaming data support to R, and we might, then we
would have to use non-standard terminology since the standard
pipe/pipeline terms are taken.


## Two Possible Directions

After some discussions within R-core prior to useR 2020 I believe we
converged on two possible approaches if we wanted to add a pipe to
base R:

1. Implicit passing of the LHS as the first argument, with no placeholder.

2. A required placeholder, `_`, that must appear once, and only once,
   as a top-level argument of the RHS call.

Each could be implemented as a syntax transformation in the parser,
but could also be implemented in other ways.  Each also has some
corner cases that need to be considered.

The [`R-syntax` subversion
branch](https://svn.r-project.org/R/branches/R-syntax) provides
experimental implementations of both approaches.  The implicit LHS, no
placeholder option is implemented as `|>`; the explicit placeholder
option is implemented as `>>`.  Both are implemented in the parser as
syntax transformations.


### 1. Implicit LHS, No Placeholder

This is the simplest option. As there is no placeholder, there is no
need to consider issues such as multiple evaluation of the LHS or
proper scoping of a placeholder. The LHS expression is simply inserted
as the first argument in the RHS call:

```r
> quote(x |> f(y))
f(x, y)
```

One issue that needs to be addressed is that inserting an argument
into calls to syntactically special functions, such as `if` or `for`
would produce nonsense and needs to be ruled out:

```r
> x |> for (i in x) y
Error: function 'for' not supported in RHS call of a pipe
```

Since the error is coming from the parser it currently may not be
providing as much context information as one might like.

Inserting the LHS into a call to `function` would also produce
nonsense, but instead of signaling an error a RHS `function`
expression is converted to a call to the function with the LHS as
argument:

```r
> quote(x |> function(y) 1 + y)
(function(y) 1 + y)(x)
```

This can be used in cases were the LHS needs to be passed as
something other than the first argument:

```r
mtcars |> subset(cyl == 4) |> function(d) lm(mpg ~ disp, data = d)
```

Or, with one of the anonymous function shorthands:

```r
mtcars |> subset(cyl == 4) |> \(d) lm(mpg ~ disp, data = d)
```

Adapting an example from the `magrittr` vignette:

```r
car_data <-
    mtcars |>
    subset(hp > 100) |>
    \(lhs) aggregate(. ~ cyl, data = lhs,
                     FUN = \(var) var |> mean |> round(2)) |>
    transform(kpl = 0.4251 * mpg) |>
    print
```

One efficiency drawback of using anonymous functions as pipe stages:
They create a binding and so force duplicating on modify.  It is not
clear how much this matters in realistic settings.

Another variation supported by the implementation is that a symbol on
the RHS is interpreted as the name of a function to call with the LHS
as argument:

```r
> quote(x |> f)
f(x)
```

This can be convenient, but can also be dropped.

Other RHS expressions are parse errors:

```r
> x |> 1
Error: The pipe operator requires a function call, a symbol, or an anonymous function expression as RHS
```

Again the error signaled may need some work.


### 2. Required Top-Level Unique Placeholder

This is also conceptually simple, and explainable as a simple syntax
transformation:

```r
> quote(x >> f(_))
f(x)
> quote(mtcars >> subset(_, cyl == 4) >> lm(mpg ~ disp, data = _))
lm(mpg ~ disp, data = subset(mtcars, cyl == 4))
```

The example from the `magrittr` vignette can be written as

```r
car_data <-
    mtcars >>
    subset(_, hp > 100) >>
    aggregate(. ~ cyl, data = _,
              FUN = function(var) var >> mean(_) >> round(_, 2)) >>
    transform(_, kpl = 0.4251 * mpg) >>
    print
```

There is no need to rule out syntactically special functions called on
the RHS:

```r
1 : 3 >> for (y in _) print(y)
```

It could be useful to allow RHS expressions to not contain a top-level
placeholder argument. In this case the RHS should be an expression
producing a function that is called with the LHS as argument.  This
would allow:

```r
x |> foo                      => foo(x)
x |> \(v) foo(v)              => (\(v) foo(v))(v)
x |> foo()                    => (foo())(v)
```

This is probably more useful in other languages than for R, but it
would avoid the need to signal an error if there is no top-level
placeholder. On the other hand, given the current implicit LHS passing
convention, a RHS call without a place holder in R would most likely be
an error.

Signaling good errors for misuse of the placeholder is
challenging. Checking that one and only one of the top-level arguments
is a placeholder is straightforward. But properly checking at parse
time that the placeholder is not used improperly in non-top-level
position is not possible (it would involve determining variable scope,
which in R can change at runtime). The current implementation rejects
any non-top-level use of the placeholder. This is fine in

```r
> 1 >> _ + (2 + _)
Error in `_` + (2 + `_`) : 
  pipe placeholder must only appear as a top-level argument in the RHS call
```

but not in

```r
> 1 >> c(_, function(_) _)
Error in c(`_`, function(`_`) `_`) : 
  pipe placeholder must only appear as a top-level argument in the RHS call
```

Previously using `_` as a variable without backquotes was not allowed
by the parser. This is a holdover from the days when `_` was dropped
as an assignment operator.  This was changed in the `R-syntax` branch
to make it easier to implement the placeholder. It may be possible to
restore this behavior and to get the parser to enforce that the
placeholder `_` can only appear at top level on the RHS of a pipe
(maybe also that it is required and may only appear once). Much along
the lines of the way `in` can only appear in a `for()` loop. On the
other hand, leaving the once-and-only-once-at-top-level check to code
might make it easier to give intelligible error messages.

If use of the placeholder anywhere other than as a RHS top-level
argument can't be prevented by the parser, then the choice is to be
overly conservative and signal an error for any placeholder appearing
in non-top-level position in a RHS argument, as the implementation
currently does, or to rely on an unbound variable error at runtime.


### Tradeoffs

The choice comes down to the simplicity of the implicit, no
placeholder approach, against the advantages in clarity of an explicit
and required placeholder. There may be a slight within R-core for the
simplicity of the first approach if one of these were to be taken.


## Some Implementation Notes

There are two, essentially independent, things that could be included
in base:

- A new operator, say `>>` or `|>`, added to the parser.

- An implementation of a pipe function.

They are independent: we could provide an implementation under the
name `%>%`, or we could provide syntax without an implementation, as we
do with `:=` now.

At this point the `%>%` is in such widespread use that there may not
be much benefit in introducing a new and different operator. On the
other hand we would need a different operator if we change the
semantics. A downside of changing the parser is that editors that do
syntax highlighting and indentation, and R code that works with parse
data, would need to be updated. A syntax transformation approach also
needs to make sure source references are not messed up.

We could provide in base an implementation of `%>%` that corresponds
to the "implicit LHS, no placeholder" option above. This could still
build and evaluate the nested call and gain most of the benefits of a
parser implementation.  Having an `exec` primitive might also help.





