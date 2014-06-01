#  File src/library/base/R/windows/junctions.R
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

Sys.junction <- function(from, to)
{
    if (!(nf <- length(from))) return(logical())
    if (!(nt <- length(to)))   stop("no files to link to")
    if (nt == 1 && isTRUE(file.info(to, extra_cols = FALSE)$isdir))
        to <- file.path(to, basename(from))
    else if (nf > nt) stop("more 'from' files than 'to' files")
    else if (nf < nt) stop("fewer 'from' files than 'to' files")

    okay <- rep(FALSE, nf)
    for(i in seq_len(nf)) {
        fr <- paste0("\\??\\", normalizePath(from[i]))
        link <- to[i]
        if(file.exists(link)) {
            warning(gettextf("link '%s' already exists", link), domain = NA)
            next
        }
        if(!dir.create(link, showWarnings = FALSE)) {
            warning(gettextf("failed to create directory for link '%s", link),
                    domain = NA)
            next
        }
        if(.Internal(mkjunction(fr, link))) okay[i] <- TRUE
    }
    okay
}
