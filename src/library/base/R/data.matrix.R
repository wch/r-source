#  File src/library/base/R/data.matrix.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

data.matrix <- function(frame, rownames.force = NA)
{
    if(!is.data.frame(frame)) return(as.matrix(frame))

    d <- dim(frame)
    rn <- if(rownames.force %in% FALSE) NULL
    else if(rownames.force %in% TRUE) row.names(frame)
    else {if(.row_names_info(frame) <= 0L) NULL else row.names(frame)}

    for(i in seq_len(d[2L])) {
        xi <- frame[[i]]
        ## at present is.numeric suffices, but let's be cautious
        if(is.integer(xi) || is.numeric(xi)) next
        if(is.logical(xi) || is.factor(xi)) {
            frame[[i]] <- as.integer(xi)
            next
        }
        frame[[i]] <- if(isS4(xi)) methods::as(xi, "numeric") else as.numeric(xi)
    }

    ## it makes sense to find the type needed first.
    intOK <- all(unlist(lapply(frame, is.integer)))
    x <- matrix(if(intOK) NA_integer_ else NA_real_,
                nrow = d[1L], ncol = d[2L],
		dimnames = list(rn, names(frame)) )
    for(i in seq_len(d[2L])) x[, i] <- frame[[i]]
    x
}
