#  File src/library/base/R/version.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## A simple S3 class for numeric versions (including package versions),
## and associated methods.

## We represent "vectors" of numeric versions as lists of sequences of
## integers, as obtained by splitting the version strings on the
## separators.  By default, only valid version specs (sequences of
## integers of suitable length), separated by '.' or '-', are allowed.
## If strictness is turned off, invalid specs result in integer()
## (rather than NA) to keep things simple.  (Note: using NULL would make
## subscripting more cumbersome ...)

## (In fact, the underlying mechanism could easily be extended to more
## general alphanumberic version specs.  E.g., one could allow "letters"
## in version numbers by replacing the non-sep characters in the version
## string by their ASCII codes.  However, this is not straightforward:
## alternatively, one could use an extended scheme with special markup
## for alpha, beta, release candidate, release, and patch versions, as
## used by many open source programs.  See e.g. the version::AlphaBeta
## module on CPAN.)

.make_numeric_version <-
function(x, strict = TRUE, regexp, classes = NULL)
{
    ## Internal creator for numeric version objects.

    x <- as.character(x)
    y <- rep.int(list(integer()), length(x))
    valid_numeric_version_regexp <- sprintf("^%s$", regexp)
    if(length(x) > 0) {
        ok <- (regexpr(valid_numeric_version_regexp, x) > -1)
        if(!all(ok) && strict)
            stop("invalid version specification", call. = FALSE)
        y[ok] <- lapply(strsplit(x[ok], "[.-]"), as.integer)
    }
    class(y) <- unique(c(classes, "numeric_version"))
    y
}

## Basic numeric versions.

numeric_version <-
function(x, strict = TRUE)
    .make_numeric_version(x, strict,
                          "([[:digit:]]+[.-])*[[:digit:]]+")    

is.numeric_version <-
function(x)
    inherits(x, "numeric_version")

as.numeric_version <-
function(x)
{
    if(is.numeric_version(x)) x
    else if(is.package_version(x)) {
        ## Pre 2.6.0 is.package_version() compatibility code ...
        ## Simplify eventually ...
        structure(x, class = c(class(x), "numeric_version"))
    }
    else numeric_version(x)
}

## Package versions must have at least two integers, corresponding to
## major and minor.

package_version <-
function(x, strict = TRUE)
{
    ## Special-case R version lists.
    ## Currently, do this here for backward compatibility.
    ## Should this be changed eventually?
    if(is.list(x) && all(c("major", "minor") %in% names(x)))
        return(R_system_version(paste(x[c("major", "minor")],
                                      collapse = ".")))
    .make_numeric_version(x, strict,
                          .standard_regexps()$valid_package_version,
                          "package_version")
}

is.package_version <-
function(x)
    inherits(x, "package_version")

as.package_version <-
function(x)
    if(is.package_version(x)) x else package_version(x)

## R system versions must have exactly three integers.
## (Not sure if reduced strictness makes a lot of sense here.)

R_system_version <-
function(x, strict = TRUE)
    .make_numeric_version(x, strict,
                          .standard_regexps()$valid_R_system_version,
                          c("R_system_version", "package_version"))

getRversion <- function() package_version(R.version)

## Workhorses.

.encode_numeric_version <-
function(x, base = NULL)
{
    if(!is.numeric_version(x)) stop("wrong class")
    if(is.null(base)) base <- max(unlist(x), 0, na.rm = TRUE) + 1
    classes <- class(x)
    lens <- as.numeric(sapply(x, length))
    ## We store the lengths so that we know when to stop when decoding.
    ## Alternatively, we need to be smart about trailing zeroes.  One
    ## approach is to increment all numbers in the version specs and
    ## base by 1, and when decoding only retain the non-zero entries and
    ## decrement by 1 one again.
    x <- as.numeric(sapply(x,
                           function(t)
                           sum(t / base^seq.int(0, length.out =
                                                length(t)))))
    structure(x, base = base, lens = lens, .classes = classes)
}

## <NOTE>
## Currently unused.
## Is there any point in having a 'base' argument?
## </NOTE>
.decode_numeric_version <-
function(x, base = NULL)
{
    if(is.null(base)) base <- attr(x, "base")
    if(!is.numeric(base)) stop("wrong argument")
    lens <- attr(x, "lens")
    y <- vector("list", length = length(x))
    for(i in seq_along(x)) {
        n <- lens[i]
        encoded <- x[i]
        decoded <- integer(n)
        for(k in seq_len(n)) {
            decoded[k] <- encoded %/% 1
            encoded <- base * (encoded %% 1)
        }
        y[[i]] <- as.integer(decoded)
    }
    class(y) <- unique(c(attr(x, ".classes"), "numeric_version"))
    y
}
                          
## Methods.

`[.numeric_version` <-
function(x, i, j)
{
    y <- if(missing(j))
        unclass(x)[i]
    else
        lapply(unclass(x)[i], "[", j)
    ## Change sequences which are NULL or contains NAs to integer().
    bad <- as.logical(sapply(y,
                             function(t) is.null(t) || any(is.na(t))))
    if(any(bad))
        y[bad] <- rep.int(list(integer()), length(bad))
    class(y) <- class(x)
    y
}

`[[.numeric_version` <-
function(x, i)
    structure(list(unclass(x)[[i]]), class = oldClass(x))
   
Ops.numeric_version <-
function(e1, e2)
{
    if(nargs() == 1)
        stop("unary ", .Generic, " not defined for numeric_version objects")
    boolean <- switch(.Generic, "<" = , ">" = , "==" = , "!=" = ,
        "<=" = , ">=" = TRUE, FALSE)
    if(!boolean)
        stop(.Generic, " not defined for numeric_version objects")
    if(!is.numeric_version(e1)) e1 <- as.numeric_version(e1)
    if(!is.numeric_version(e2)) e2 <- as.numeric_version(e2)
    base <- max(unlist(e1), unlist(e2), 0) + 1
    e1 <- .encode_numeric_version(e1, base = base)
    e2 <- .encode_numeric_version(e2, base = base)
    NextMethod(.Generic)
}

Summary.numeric_version <-
function(..., na.rm)
{
    ok <- switch(.Generic, max = , min = TRUE, FALSE)
    if(!ok)
        stop(.Generic, " not defined for numeric_version objects")
    x <- list(...)
    x <- do.call("c", lapply(x, as.numeric_version))
    ## <FIXME> which.max/min automatically remove NAs
    switch(.Generic,
           max = x[which.max(.encode_numeric_version(x))],
           min = x[which.min(.encode_numeric_version(x))])
}

as.character.numeric_version <-
function(x, ...)
    as.character(unlist(lapply(x, paste, collapse = ".")))

as.data.frame.numeric_version <- as.data.frame.vector

as.list.numeric_version <-
function(x, ...)
    unclass(x)

c.numeric_version <-
function(..., recursive = FALSE)
{
    x <- lapply(list(...), as.numeric_version)
    ## Try to preserve common extension classes.
    ## Note that this does not attempt to turn character strings into
    ## *package* versions if possible.
    classes <- if(length(unique(lapply(x, class))) == 1L)
        class(x[[1L]])
    else
        "numeric_version"
    structure(unlist(x, recursive = FALSE), class = classes)
}

print.numeric_version <-
function(x, ...)
{
    print(noquote(sQuote(as.character(x))), ...)
    invisible(x)
}

## <NOTE>
## Versions of R prior to 2.6.0 had only a package_version class.
## We now have package_version extend numeric_version.
## We only provide named subscripting for package versions.

`$.package_version` <-
function(x, name)
{
    name <- pmatch(name, c("major", "minor", "patchlevel"))
    switch(name,
           major = as.integer(sapply(x, "[", 1)),
           minor = as.integer(sapply(x, "[", 2)),
           patchlevel = as.integer(sapply(x, "[", 3)))
    ## <NOTE>
    ## Older versions used
    ## patchlevel = {
    ##   as.integer(sapply(x,
    ##                     function(s) s[min(3, length(s))]))
    ## }
    ## apparently with the idea to always use the last component as the
    ## patchlevel ...
    ## </NOTE>
}

## To ensure method dispatch for pre 2.6.0 package versions we could
## keep the old methods as "aliases".  Not sure if this is really needed,
## and certainly something to get rid of eventually ...

## `[.package_version` <- `[.numeric_version`
## `[[.package_version` <- `[[.numeric_version`
## Ops.package_version <- Ops.numeric_version
## Summary.package_version <- Summary.numeric_version
## as.character.package_version <- as.character.numeric_version
## c.package_version <- c.numeric_version
## print.package_version <- print.numeric_version
