#  File src/library/base/R/funprog.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

Reduce <-
function(f, x, init, right = FALSE, accumulate = FALSE)
{
    mis <- missing(init)
    len <- length(x)

    if(len == 0L) return(if(mis) NULL else init)

    f <- match.fun(f)

    ## Try to avoid the "obvious"
    ##   if(!mis) x <- if(right) c(x, init) else c(init, x)
    ## to be more efficient ...

    if(!is.vector(x) || is.object(x))
        x <- as.list(x)

    ind <- seq_len(len)

    if(mis) {
        if(right) {
            init <- x[[len]]
            ind <- ind[-len]
        }
        else {
            init <- x[[1L]]
            ind <- ind[-1L]
        }
    }

    if(!accumulate) {
        if(right) {
            for(i in rev(ind))
                init <- forceAndCall(2, f, x[[i]], init)
        }
        else {
            for(i in ind)
                init <- forceAndCall(2, f, init, x[[i]])
        }
        init
    }
    else {
        len <- length(ind) + 1L
        ## We need a list to accumulate the results as these do not
        ## necessarily all have length one (e.g., reducing with c()).
        out <- vector("list", len)
        if(mis) {
            if(right) {
                out[[len]] <- init
                for(i in rev(ind)) {
                    init <- forceAndCall(2, f, x[[i]], init)
                    out[[i]] <- init
                }
            } else {
                out[[1L]] <- init
                for(i in ind) {
                    init <- forceAndCall(2, f, init, x[[i]])
                    out[[i]] <- init
                }
            }
        } else {
            if(right) {
                out[[len]] <- init
                for(i in rev(ind)) {
                    init <- forceAndCall(2, f, x[[i]], init)
                    out[[i]] <- init
                }
            }
            else {
                for(i in ind) {
                    out[[i]] <- init
                    init <- forceAndCall(2, f, init, x[[i]])
                }
                out[[len]] <- init
            }
        }
        ## If all results have length one, we can simplify.
        ## (Note that we do not simplify to arrays in case all results
        ## have a common length > 1.)
	if(all(vapply(out, length, 1.) == 1L))
            out <- unlist(out, recursive = FALSE)
        out
    }
}

Filter <-
function(f, x)
{
    ind <- as.logical(unlist(lapply(x, f)))
    x[!is.na(ind) & ind]
}


Map <-
function(f, ...)
{
    f <- match.fun(f)
    mapply(FUN = f, ..., SIMPLIFY = FALSE)
}

Negate <-
function(f)
{
    f <- match.fun(f) # effectively force f, avoid lazy eval.
    function(...) ! f(...)
}

Position <-
function(f, x, right = FALSE, nomatch = NA_integer_)
{
    ind <- if(right) rev(seq_along(x)) else seq_along(x)

    for(i in ind)
        if(f(x[[i]]))
            return(i)

    nomatch
}

Find <-
function(f, x, right = FALSE, nomatch = NULL)
{
    f <- match.fun(f)
    if((pos <- Position(f, x, right, nomatch = 0L)) > 0L)
        x[[pos]]
    else
        nomatch
}

identity <-
function(x)
    x

dontCheck <- identity
