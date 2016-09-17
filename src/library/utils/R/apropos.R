#  File src/library/utils/R/apropos.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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

## a list from base-internal.Rd
.dot_internals <- c(".subset", ".subset2", ".getRequiredPackages",
                    ".getRequiredPackages2", ".isMethodsDispatchOn",
                    ".row_names_info", ".set_row_names", ".ArgsEnv",
                    ".genericArgsEnv", ".TAOCP1997init", ".gt",
                    ".gtn", ".primTrace", ".primUntrace",
                    ".POSIXct", ".POSIXlt", ".cache_class",
                    ".Firstlib_as_onload", ".methodsNamespace",
                    ".popath", ".mapply", ".detach", ".maskedMsg")


apropos <- function (what, where = FALSE, ignore.case = TRUE, mode = "any")
{
    stopifnot(is.character(what))
    x <- character(0L)
    check.mode <- mode != "any"
    for (i in seq_along(sp <- search())) {
	li <-
	    if(ignore.case)
		grep(what, ls(pos = i, all.names = TRUE),
		     ignore.case = TRUE, value = TRUE)
	    else ls(pos = i, pattern = what, all.names = TRUE)

        ## Treat anything starting with .__, .C_R or .F_ as internal
        li <- grep("^[.](__|C_|F_)", li, invert = TRUE, value = TRUE)
        if(sp[i] == "package:base") li <- li[! li %in% .dot_internals]

	if(length(li)) {
	    if(check.mode)
		li <- li[sapply(li, exists, where = i,
				mode = mode, inherits = FALSE)]
	    x <- c(x, if(where) structure(li, names = rep.int(i, length(li))) else li)
	}
    }
    sort(x)
}

find <- function(what, mode = "any", numeric = FALSE, simple.words=TRUE)
{
    stopifnot(is.character(what))
    if(length(what) > 1L) {
        warning("elements of 'what' after the first will be ignored")
        what <- what[1L]
    }
    len.s <- length(sp <- search())
    ind <- logical(len.s)
    check.mode <- mode != "any"
    for (i in 1L:len.s) {
        if(simple.words) {
            found <- what %in% ls(pos = i, all.names = TRUE)
            if(found && check.mode)
                found <- exists(what, where = i, mode = mode, inherits=FALSE)
            ind[i] <- found
        } else {
            li <- ls(pos = i, pattern = what, all.names = TRUE)
            ## Treat anything starting with .__, .C_R or .F_ as internal
            li <- grep("^[.](__|C_|F_)", li, invert = TRUE, value = TRUE)
            if(sp[i] == "package:base") li <- li[! li %in% .dot_internals]
            ll <- length(li)
            if(ll > 0 && check.mode) {
                mode.ok <- sapply(li, exists, where = i, mode = mode,
                                  inherits = FALSE)
                ll <- sum(mode.ok)
                if(ll >= 2) # some languages have multiple plurals
                    warning(sprintf(ngettext(ll,
                                             "%d occurrence in %s",
                                             "%d occurrences in %s"), ll, sp[i]),
                            domain = NA)
            }
            ind[i] <- ll > 0L
        }
    }
    ## found name in  search()[ ind ]
    if(numeric) structure(which(ind), names=sp[ind]) else sp[ind]
}

