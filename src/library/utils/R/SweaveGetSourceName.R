#  File src/library/utils/R/SweaveGetSourceName.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2026 The R Core Team
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

SweaveGetSourceName <- function()
{
    if (interactive())
        stop("this function cannot be used from an interactive session")

    args <- commandArgs(FALSE)

    ## If Sweave is launched with
    ##
    ##   R -e "Sweave('foo.Rnw', ...)"
    ##
    ## The expression containing the filename is within an argument
    ## following "-e". Spaces on the command line are encoded as '~+~'
    ## in the arguments.
    pos <- which(args == "-e")
    cmd <- grepv(r"(^[[:space:]]*Sweave[[:space:]]*\()",
                 gsub("~+~", " ", args[pos + 1L], fixed = TRUE))
    m <- match(TRUE, pos & length(cmd), nomatch = 0L)
    if (m)
        return(match.call(Sweave, str2lang(cmd[m]))$file)

    ## If Sweave is launched with
    ##
    ##   R CMD Sweave <options> foo.Rnw
    ##
    ## (where <options> all start with '--') the argument containing
    ## the relevant information is a character string of the form
    ##
    ##   nextArg<option>nextArg<option>...nextArgfoo.Rnw
    ##
    ## The filename is the argument not starting with '--'.
    pos <- which(args == "--args")
    cmd <- grepv("^nextArg", args[pos + 1L])
    m <- match(TRUE, pos & length(cmd), nomatch = 0L)
    if (m)
    {
        s <- strsplit(cmd[m], 'nextArg', fixed = TRUE)[[1L]]
        return(grepv("^--|^$", s, invert = TRUE))
    }

    stop("unrecognized Sweave call")
}
