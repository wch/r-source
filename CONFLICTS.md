---
title: "Search Path Conflicts"
author: Luke Tierney
output: html_document
---


## Background

Hadley Wickham has recently introduced the `conflicted` package.  The
goal is to deal with the problem where multiple packages on the search
path provide a function with a particular name and the one on top
isn't the one you wanted. `conflicted` arranges to signal an error
when evaluating a global variable that is defined by multiple packages
on the search path, unless an explicit resolution of the conflict has
been specified.

Being able to ask for stricter handling of conflicts is definitely a
good idea, but it seems it would more naturally belong in base R than
in a package. There are several downsides to the package approach, at
least as currently implemented, including being fairly heavy-weight,
confusing `find`, and not handling packages that have functions that
do things like

```r
foo <- function(x) { require(bar); ... }
```

Not that this is a good idea these days, but it's out there.

The approach taken in `conflicted` of signaling an error only when a
symbol is used might be described as a dynamic approach. This dynamic
approach could be implemented more efficiently in base R by adjusting
the global cache mechanism. However there is an alternate approach
worth considering, which is to insist that conflicts be resolved at
the point where the `library` or `require` call occurs. This might be
called a static or declarative approach.

To think about these options it is useful to think about a range of
activities that might need increasing levels of strictness in conflict
handling:

0. Interactive work in the console.
1. Interactive work in a notebook.
2. Code in an `Rmarkdown` or `Sweave` document.
3. Scripts.
4. Packages.

Packages are handled by `NAMESPACES`. The others could use some help.

For 2 and 3 the static approach seems clearly better, as long as we
provide the right set of tools to allow it to be expressed
concisely. For notebooks I think the static approach is probably
better as well. For interactive use it may be a matter of personal
preference. Once I have decided I want help with conflicts, my
preference would be to be forced to resolve them on package load
rather than to have my work flow interrupted at random points to go
and sort things out.


## Implementation

A basic implementation of the static approach is quite simple:

- Give library/require additional arguments `mask.ok` and `omit`.
    - `mask.ok`: these functions can mask variables already on the path.
    - `omit`: don't add these to the search path frame.
- If `getOption("error.on.conflicts")` is TRUE then signal an error if
  there are conflicts not covered by mask.ok.

This branch contains modified versions of `library.R` and
`namespace.R` that implement this.  This produces

```r
options(error.on.conflicts = TRUE)
library(dplyr)
## conflict error from filter, lag

library(dplyr, mask.ok = c("filter", "lag",
                           "intersect", "setdiff", "setequal", "union"))
## OK

library(MASS)
## conflict error from select

library(MASS, omit = "select")
## OK
```

To make this easier for commonly used packages, and for implicitly
attached packages, `conflictRules` provides a way to specify default
`mask.ok`/`omit` values for packages, e.g. with

```r
conflictRules("dplyr",
              mask.ok = c("filter", "lag",
                          "intersect", "setdiff", "setequal", "union")))
conflictRules("MASS", omit = "select")
```

Alternate forms that should all work:
```r
## mask, no matter where they are:
library(dplyr, mask.ok = c("filter", "lag",
                           "intersect", "setdiff", "setequal", "union"))

## mask only if in stats/base:
library(dplyr, mask.ok = list(stats = c("filter", "lag"),
                              base = c("intersect", "setdiff",
                                       "setequal", "union")))

## mask anything in stats/base:
library(dplyr, mask.ok = list(base = TRUE, stats = TRUE))

## using conflictRules:
conflictRules("dplyr", mask.ok = list(base = TRUE, stats = TRUE))
conflictRules("MASS", omit = "select")

## to allow Matrix to be loaded with conflict checking:
conflictRules("Matrix", mask.ok = list(stats = TRUE,
                                       graphics = "image",
                                       utils = c("head", "tail"),
                                       base = TRUE))

## for BiocGenerics:
conflictRules("BiocGenerics", mask.ok = list(base = TRUE,
                                             stats = TRUE,
                                             parallel = TRUE,
                                             graphics = c("image", "boxplot"),
                                             utils = "relist"))
```

Some additional features that have been implemented:
  
- `library()` has some heuristics to avoid warning about S4 overrides
  that mask the overrides in `Matrix` (but not those in
  `BiocGenerics`).  These heuristics are disabled when strict
  conflict checking is in force so that they have to be handled
  explicitly in some way.

- Arguments `only` and `attach.required` have been added to
  `library()`. If `only` is supplied as a character vector, then only
  variables named there are included in the attached frame. The
  default value of `attach.required` is `TRUE` if `only` is missing
  and `FALSE` if `only` is supplied.

- The error signaled with strict checking is of class `packageConflictsError`
  with fields `package` and `conflicts`.


## Open Issues

Argument and function names could be adjusted if there are strong
objections to the ones used here.

Additional features that might be useful:

- Provide a restart for retrying with adjusted arguments; that could be used
  by a GUI front end.
- Have a way to say that (some) 'safe' S4 or even S3 generics are OK
  to mask non-generics, at least if the non-generics are used as the default
  method by the generics.
- Add a function to compute the conflicts that would happen on attach.
  Might be useful to lift out and expose the code in `checkConflicts`.

Some questions:

- Should `attach` also signal an error on conflicts? It repeats some
   of the checking logic in `library`, but with some differences.
- Should we allow a package to declare that it is masking things
  from its dependencies, e.g. in its `DESCRIPTION` file?
- Add `only` and `attach.required` to `require()` also?
- Do we need to disallow using both omit and only?
 
TODO list:

- Add more documentation.
- Add some input checking
- Need to check whether the `pos` argument raises any issues.
- Also whether there are any issues with non-package frames.

<!--
Local Variables:
mode: poly-markdown+R
mode: flyspell
End:
-->
