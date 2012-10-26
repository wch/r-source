#  File src/library/base/R/backquote.R
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

## quote() is .Primitive

### PR15077: need to substitute in a length-one pairlist, so
### handle pairlists first
bquote <- function(expr, where=parent.frame())
{
    unquote <- function(e)
        if (is.pairlist(e)) as.pairlist(lapply(e, unquote))
        else if (length(e) <= 1L) e
        else if (e[[1L]] == as.name(".")) eval(e[[2L]], where)
        else as.call(lapply(e, unquote))

    unquote(substitute(expr))
}

## utility we've used ourselves
enquote <- function(cl) as.call(list(as.name("quote"), cl))
