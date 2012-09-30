#  File src/library/base/R/version.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

    nms <- names(x)
    x <- as.character(x)
    y <- rep.int(list(integer()), length(x))
    valid_numeric_version_regexp <- sprintf("^%s$", regexp)
    if(length(x)) {
        ok <- grepl(valid_numeric_version_regexp, x)
        if(!all(ok) && strict)
            stop(gettextf("invalid version specification %s",
                          paste(sQuote(unique(x[!ok])), collapse = ", ")),
                 call. = FALSE, domain = NA)
        y[ok] <- lapply(strsplit(x[ok], "[.-]"), as.integer)
    }
    names(y) <- nms
    class(y) <- unique(c(classes, "numeric_version"))
    y
}

## Basic numeric versions.

numeric_version <-
function(x, strict = TRUE)
    .make_numeric_version(x, strict,
                          .standard_regexps()$valid_numeric_version)

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

is.package_version <- function(x) inherits(x, "package_version")

as.package_version <- function(x)
    if(is.package_version(x)) x else package_version(x)

## R system versions must have exactly three integers.
## (Not sure if reduced strictness makes a lot of sense here.)

R_system_version <-
function(x, strict = TRUE)
    .make_numeric_version(x, strict,
                          .standard_regexps()$valid_R_system_version,
                          c("R_system_version", "package_version"))

getRversion <-
function()
    package_version(R.version)

## Workhorses.

## <NOTE>
## Could use this for or as as.double.numeric_version() ...
## </NOTE>

.encode_numeric_version <-
function(x, base = NULL)
{
    if(!is.numeric_version(x)) stop("wrong class")
    if(is.null(base)) base <- max(unlist(x), 0, na.rm = TRUE) + 1
    classes <- class(x)
    nms <- names(x)
    x <- unclass(x)
    lens <- vapply(x, length, 1L)
    ## We store the lengths so that we know when to stop when decoding.
    ## Alternatively, we need to be smart about trailing zeroes.  One
    ## approach is to increment all numbers in the version specs and
    ## base by 1, and when decoding only retain the non-zero entries and
    ## decrement by 1 one again.
    x <- vapply(x, function(t)
		sum(t / base^seq.int(0, length.out = length(t))), 1.)
    structure(ifelse(lens > 0L, x, NA_real_),
              base = base, lens = lens, .classes = classes, names = nms)
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
    bad <- vapply(y, function(t) is.null(t) || any(is.na(t)), NA)
    if(any(bad))
        y[bad] <- rep.int(list(integer()), length(bad))
    class(y) <- class(x)
    y
}

`[[.numeric_version` <-
function(x, ..., exact = NA)
{
   if(length(list(...)) < 2L)
      structure(list(unclass(x)[[..., exact=exact]]), class = oldClass(x))
   else
      unclass(x)[[..1, exact=exact]][..2]
}

## allowed forms
## x[[i]] <- "1.2.3"; x[[i]] <- 1L:3L; x[[c(i,j)]] <- <single integer>
## x[[i,j]] <- <single integer>
`[[<-.numeric_version` <-
function(x, ..., value)
{
   z <- unclass(x)
   if(nargs() < 4L) {
       if(length(..1) < 2L) {
           if(is.character(value) && length(value) == 1L)
               value <- unclass(as.numeric_version(value))[[1L]]
           else if(!is.integer(value)) stop("invalid 'value'")
       } else {
           value <- as.integer(value)
           if(length(value) != 1L) stop("invalid 'value'")
       }
       z[[..1]] <- value
   } else {
       value <- as.integer(value)
       if(length(value) != 1L) stop("invalid 'value'")
       z[[..1]][..2] <- value
   }
   structure(z, class = oldClass(x))
}


Ops.numeric_version <-
function(e1, e2)
{
    if(nargs() == 1L)
        stop(gettextf("unary '%s' not defined for \"numeric_version\" objects",
                      .Generic), domain = NA)
    boolean <- switch(.Generic, "<" = , ">" = , "==" = , "!=" = ,
        "<=" = , ">=" = TRUE, FALSE)
    if(!boolean)
        stop(gettextf("'%s' not defined for \"numeric_version\" objects",
                      .Generic), domain = NA)
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
    ok <- switch(.Generic, max = , min = , range = TRUE, FALSE)
    if(!ok)
        stop(gettextf("%s not defined for \"numeric_version\" objects",
                      .Generic), domain = NA)
    x <- do.call("c", lapply(list(...), as.numeric_version))
    v <- .encode_numeric_version(x)
    if(!na.rm && length(pos <- which(is.na(v)))) {
        y <- x[pos[1L]]
        if(as.character(.Generic) == "range")
            c(y, y)
        else
            y
    }
    else
        switch(.Generic,
               max = x[which.max(v)],
               min = x[which.min(v)],
               range = x[c(which.min(v), which.max(v))])
}

as.character.numeric_version <-
function(x, ...)
    as.character(format(x))

as.data.frame.numeric_version <- as.data.frame.vector

as.list.numeric_version <-
function(x, ...)
{
    nms <- names(x)
    names(x) <- NULL
    y <- lapply(seq_along(x), function(i) x[i])
    names(y) <- nms
    y
}

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

duplicated.numeric_version <-
function(x, incomparables = FALSE, ...)
{
    x <- .encode_numeric_version(x)
    NextMethod("duplicated")
}

format.numeric_version <-
function(x, ...)
{
    x <- unclass(x)
    y <- rep.int(NA_character_, length(x))
    names(y) <- names(x)
    ind <- vapply(x, length, 1L) > 0L
    y[ind] <- unlist(lapply(x[ind], paste, collapse = "."))
    y
}

is.na.numeric_version <-
function(x)
    is.na(.encode_numeric_version(x))

print.numeric_version <-
function(x, ...)
{
    y <- as.character(x)
    if(!length(y))
        writeLines(gettext("<0 elements>"))
    else
        print(noquote(ifelse(is.na(y), NA_character_, sQuote(y))), ...)
    invisible(x)
}

rep.numeric_version <-
function(x, ...)
    structure(NextMethod("rep"), class = oldClass(x))

unique.numeric_version <-
function(x, incomparables = FALSE, ...)
    x[!duplicated(x, incomparables, ...)]

xtfrm.numeric_version <-
function(x)
    .encode_numeric_version(x)

## <NOTE>
## Versions of R prior to 2.6.0 had only a package_version class.
## We now have package_version extend numeric_version.
## We only provide named subscripting for package versions.
## </NOTE>

`$.package_version` <-
function(x, name)
{
    name <- pmatch(name, c("major", "minor", "patchlevel"))
    x <- unclass(x)
    switch(name,
	   major = vapply(x, "[", 0L, 1L),
	   minor = vapply(x, "[", 0L, 2L),
	   patchlevel = vapply(x, "[", 0L, 3L))
}
