#  File src/library/utils/R/progressBar.R
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

txtProgressBar <-
    function(min = 0, max = 1, initial = 0, char = "=",
             width = NA, title, label, style = 1, file = "")
{
    if(!identical(file, "") &&
       !(inherits(file, "connection") && isOpen(file)))
        stop("'file' must be \"\" or an open connection object")
    if(! style %in% 1L:3L) style <- 1
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- -1L # This ensures the initial value is displayed for style = 3
    nw <- nchar(char, "w")
    if(is.na(width)) {
        width <- getOption("width")
        if(style == 3L) width <- width - 10L
        width <- trunc(width/nw)
    }
    if (max <= min) stop("must have 'max' > 'min'")

    up1 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        if(.nb < nb) {
            cat(paste(rep.int(char, nb-.nb), collapse=""), file = file)
            flush.console()
        } else if (.nb > nb) {
            cat("\r", paste(rep.int(" ", .nb*nw), collapse=""),
                "\r", paste(rep.int(char, nb), collapse=""),
                sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }

    up2 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        if(.nb <= nb) {
            cat("\r", paste(rep.int(char, nb), collapse=""),
                sep = "", file = file)
            flush.console()
        } else {
            cat("\r", paste(rep.int(" ", .nb*nw), collapse=""),
                "\r", paste(rep.int(char, nb), collapse=""),
                sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }

    up3 <- function(value) {
        if(!is.finite(value) || value < min || value > max) return()
        .val <<- value
        nb <- round(width*(value - min)/(max - min))
        pc <- round(100*(value - min)/(max - min))
        if(nb == .nb && pc == .pc) return()
        cat(paste(c("\r  |", rep.int(" ", nw*width+6)), collapse=""),
            file = file)
        cat(paste(c("\r  |",
                    rep.int(char, nb),
                    rep.int(" ", nw*(width-nb)),
                    sprintf("| %3d%%", pc)
                    ), collapse=""), file = file)
        flush.console()
        .nb <<- nb
        .pc <<- pc
    }

    getVal <- function() .val
    kill <- function()
        if(!.killed) {
            cat("\n", file = file)
            flush.console()
            .killed <<- TRUE
        }
    up <- switch(style, up1, up2, up3)
    up(initial) # will check if in range
    structure(list(getVal=getVal, up=up, kill=kill),
              class = "txtProgressBar")
}

getTxtProgressBar <- function(pb)
{
    if(!inherits(pb, "txtProgressBar"))
       stop(gettextf("'pb' is not from class %s",
                     dQuote("txtProgressBar")),
            domain = NA)
    pb$getVal()
}

setTxtProgressBar <- function(pb, value, title = NULL, label = NULL)
{
    if(!inherits(pb, "txtProgressBar"))
        stop(gettextf("'pb' is not from class %s",
                      dQuote("txtProgressBar")),
             domain = NA)
    oldval <- pb$getVal()
    pb$up(value)
    invisible(oldval)
}

close.txtProgressBar <- function(con, ...)
{
    con$kill()
    invisible(NULL)
}
