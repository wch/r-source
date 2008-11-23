#  File src/library/utils/R/URLencode.R
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

URLencode <- function(URL, reserved = FALSE)
{
    ## It is unsafe to use ranges here as collation is locale-dependent.
    ## We want to do this on characters and not on bytes.
    OK <- paste("[^-ABCDEFGHIJKLMNOPQRSTUVWXYZ",
		"abcdefghijklmnopqrstuvwxyz0123456789$_.+!*'(),",
		if(!reserved) ";/?:@=&", "]", sep="")
    x <- strsplit(URL, "")[[1]]
    z <- grep(OK, x)
    if(length(z)) {
        y <- sapply(x[z], function(x)
                    paste("%", as.character(charToRaw(x)), sep="",
                          collapse = ""))
        x[z] <- y
    }
    paste(x, collapse="")
}

URLdecode <- function(URL)
{
    x <- charToRaw(URL)
    pc <- charToRaw("%")
    out <- raw(0)
    i <- 1
    while(i <= length(x)) {
        if(x[i] != pc) {
            out <- c(out, x[i])
            i <- i + 1
        } else {
            y <- as.integer(x[i+1:2])
            y[y > 96] <- y[y > 96] - 32 # a-f -> A-F
            y[y > 57] <- y[y > 57] - 7  # A-F
            y <- sum((y - 48) * c(16, 1))
            out <- c(out, as.raw(as.character(y)))
            i <- i + 3
        }
    }
    rawToChar(out)
}
