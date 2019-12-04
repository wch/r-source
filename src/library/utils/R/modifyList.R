#  File src/library/utils/R/modifyList.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

### Originates from Deepayan Sarkar as  updateList() from 'lattice' package

modifyList <- function(x, val, keep.null = FALSE)
{
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    vnames <- names(val)
    ## Will not update unnamed components.  FIXME: What if names are repeated? Warn?
    vnames <- vnames[nzchar(vnames)]
    if (keep.null) {
        for (v in vnames) {
            x[v] <-
                if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
                    list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
                else val[v]
        }
    }
    else {
        for (v in vnames) {
            x[[v]] <-
                if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
                    modifyList(x[[v]], val[[v]], keep.null = keep.null)
                else val[[v]]
        }
    }
    x
}


## Originally from package 'nlme' -- used in its lmList() and nlsList():

## Collect errors from a list 'x',
## produce a "summary warning" and keep that as "warningMsg" attribute
warnErrList <- function(x, warn = TRUE, errValue = NULL) {
    errs <- vapply(x, inherits, NA, what = "error")
    if(any(errs)) {
        v.err <- x[errs]
        e.call <- deparse1(conditionCall(v.err[[1]]), collapse = "\n")
        tt <- table(vapply(v.err, conditionMessage, ""))
        msg <-
            if(length(tt) == 1L)
                sprintf(ngettext(tt[[1L]],
                                 "%d error caught in %s: %s",
                                 "%d times caught the same error in %s: %s"),
                        tt[[1L]], e.call, names(tt)[[1L]])
            else ## at least two different errors caught
                paste(gettextf(
                    "%d errors caught in %s.  The error messages and their frequencies are",
                    sum(tt), e.call),
                    paste(capture.output(sort(tt)), collapse="\n"), sep="\n")

        if(warn)
            warning(msg, call. = FALSE, domain = NA)
        x[errs] <- list(errValue)
        attr(x, "warningMsg") <- msg
    }
    x
}
