#  File src/library/utils/tests/Sweave-tst.R
#  Part of the R package, http://www.R-project.org
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

## Testing Sweave

.proctime00 <- proc.time()
library(utils)
options(digits=5) # to avoid trivial printed differences
options(show.signif.stars=FALSE) # avoid fancy quotes in o/p

SweaveTeX <- function(file, ...) {
    if(!file.exists(file))
        stop("File",file, "does not exist in", getwd())
    texF <- sub("\\.[RSrs]nw$", ".tex", file)
    Sweave(file, ...)
    if(!file.exists(texF))
        stop("File",texF, "does not exist in", getwd())
    readLines(texF)
}

t1 <- SweaveTeX("swv-keepSrc-1.Rnw")
## now  look at that -- it has an *extra*  continuation ("+ ") line:
writeLines(t1[grep("require", t1) + -1:1])


cat('Time elapsed: ', proc.time() - .proctime00,'\n')
