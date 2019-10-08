#  File src/library/base/R/backquote.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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
#  https://www.R-project.org/Licenses/

## quote() is .Primitive

### PR#15077: need to substitute in a length-one pairlist, so
### handle pairlists first
bquote <- function (expr, where = parent.frame(), splice = FALSE)
{
    unquote <- function(e) {
        if (is.pairlist(e))
            as.pairlist(lapply(e, unquote))
        else if (is.call(e)) {
            if (is.name(e[[1L]]) && as.character(e[[1]]) == ".")
                eval(e[[2L]], where)
            else if (splice) {
                if (is.name(e[[1L]]) && as.character(e[[1L]]) == "..")
                    stop("can only splice inside a call", call. = FALSE)
                else as.call(unquote.list(e))
            }
            else as.call(lapply(e, unquote))
        }
        else e
    }

    is.splice.macro <- function(e)
        is.call(e) && is.name(e[[1L]]) && as.character(e[[1L]]) == ".."

    unquote.list <- function(e) {
        p <- Position(is.splice.macro, e, nomatch = NULL)
        if (is.null(p))
            lapply(e, unquote)
        else {
            n <- length(e)
            head <- if (p == 1) NULL else e[1 : (p - 1)]
            tail <- if (p == n) NULL else e[(p + 1) : n]
            macro <- e[[p]]
            mexp <- eval(macro[[2L]], where)
            c(lapply(head, unquote), mexp, as.list(unquote.list(tail)))
        }
    }

    unquote(substitute(expr))
}

## utility we've used ourselves
enquote <- function(cl) as.call(list(quote(base::quote), cl))
