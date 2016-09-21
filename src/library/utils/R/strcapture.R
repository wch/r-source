##  File src/library/utils/R/strcapture.R
##  Part of the R package, https://www.R-project.org
##
##  Copyright (C) 1995-2016 The R Core Team
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  https://www.R-project.org/Licenses/

strcapture <- function(pattern, x, proto, perl = FALSE, useBytes = FALSE) {
    m <- regexec(pattern, x, perl=perl, useBytes=useBytes)
    str <- regmatches(x, m)
    ntokens <- length(proto) + 1L
    nomatch <- lengths(str) == 0L
    str[nomatch] <- list(rep(NA_character_, ntokens))
    mat <- matrix(as.character(unlist(str)), ncol=ntokens,
                  byrow=TRUE)[,-1L,drop=FALSE]
    ans <- lapply(seq_along(proto), function(i) {
                      if (isS4(proto[[i]])) {
                          methods::as(mat[,i], class(proto[[i]]))
                      } else {
                          fun <- match.fun(paste0("as.", class(proto[[i]])))
                          fun(mat[,i])
                      }
                  })
    names(ans) <- names(proto)
    if (isS4(proto)) {
        methods::as(ans, class(proto))
    } else {
        as.data.frame(ans, optional=TRUE, stringsAsFactors=FALSE)
    }
}

## Not yet exported
strextract <- function(pattern, x, perl = FALSE, useBytes = FALSE) {
    m <- regexec(pattern, x, perl=perl, useBytes=useBytes)
    unlist(regmatches(x, m))
}

### TODO: strslice(), like strcapture() but based on strsplit().
