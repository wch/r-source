#  File src/library/tools/R/package.dependencies.R
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

## This is called only with check = FALSE in getDepMtrx/getRemotePkgDepends

package.dependencies <-
    function(x, check = FALSE, depLevel = c("Depends", "Imports", "Suggests"))
{
    depLevel <- match.arg(depLevel)

    if(!is.matrix(x))
        x <- matrix(x, nrow = 1L, dimnames = list(NULL, names(x)))

    deps <- list()
    for(k in 1L:nrow(x)){
        z <- x[k, depLevel]
        if(!is.na(z) & z != ""){
            ## split dependencies, remove leading and trailing whitespace
            z <- unlist(strsplit(z, ",", fixed=TRUE))
            z <- sub("^[[:space:]]*(.*)", "\\1", z)
            z <- sub("(.*)[[:space:]]*$", "\\1", z)

            ## split into package names and version
            pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
            deps[[k]] <-
                cbind(sub(pat, "\\1", z), sub(pat, "\\2", z), NA)

            noversion <- deps[[k]][,1] == deps[[k]][,2]
            deps[[k]][noversion,2] <- NA

            ## split version dependency into operator and version number
            pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
            deps[[k]][!noversion, 2:3] <-
                c(sub(pat, "\\1", deps[[k]][!noversion, 2]),
                  sub(pat, "\\2", deps[[k]][!noversion, 2]))
        }
        else
            deps[[k]] <- NA
    }

    if(check){
        z <- rep.int(TRUE, nrow(x))
        for(k in 1L:nrow(x)) {
            ## currently we only check the version of R itself
            if(!is.na(deps[[k]]) &&
               any(ok <- deps[[k]][,1] == "R")) {
                ## NOTE: currently operators must be `<=' or `>='.
                if(!is.na(deps[[k]][ok, 2])
                   && deps[[k]][ok, 2] %in% c("<=", ">=")) {
                    ## careful.  We don't want 1.9.1 < 1.50
                    op <- deps[[k]][ok,2]
                    x1 <- rep.int(0, 6)
                    y <- c(R.version$major,
                           strsplit(R.version$minor, ".", fixed=TRUE)[[1L]])
                    x1[seq_along(y)] <- y
                    y <- strsplit(deps[[k]][ok,3], ".", fixed=TRUE)[[1L]]
                    x1[3+seq_along(y)] <- y
                    x1 <- format(x1, justify="right")
                    x2 <- paste(x1[4:6], collapse=".")
                    x1 <- paste(x1[1L:3], collapse=".")
                    comptext <- paste0("'", x1, "' ", op, " '", x2, "'")
                    compres <- try(eval(parse(text = comptext)))
                    if(!inherits(compres, "try-error")) {
                        z[k] <- compres
                    }
                }
            }
        }
        names(z) <- x[,"Package"]
        return(z)
    }
    else{
        names(deps) <- x[,"Package"]
        return(deps)
    }
}
