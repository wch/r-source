#  File src/library/base/R/strwrap.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

strtrim <- function(x, width)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(strtrim(x, width))
}

strwrap <-
function(x, width = 0.9 * getOption("width"), indent = 0, exdent = 0,
         prefix = "", simplify = TRUE, initial = prefix)
{
    if(!is.character(x)) x <- as.character(x)

    ## Useful variables.
    indentString <- strrep(" ", indent)
    exdentString <- strrep(" ", exdent)
    y <- list()                         # return value

    ## We use strsplit() to tokenize input into paras and words, and
    ## hence need to tweak how it handles/transforms encodings.  To
    ## preserve encodings, it seems "best" to canonicalize to UTF-8
    ## (ensuring valid UTF-8), and at the end convert back to latin1
    ## where we originally had latin1.
    enc <- Encoding(x)
    x <- enc2utf8(x)
    if(any(ind <- !validEnc(x)))
        x[ind] <- iconv(x[ind], "UTF-8", "UTF-8", sub = "byte")

    z <- lapply(strsplit(x, "\n[ \t\n]*\n", perl = TRUE),
                strsplit, "[ \t\n]", perl = TRUE)

    ## Now z[[i]][[j]] is a character vector of all "words" in
    ## paragraph j of x[i].

    for(i in seq_along(z)) {
        yi <- character()
        for(j in seq_along(z[[i]])) {
            ## Format paragraph j in x[i].
            words <- z[[i]][[j]]
            nc <- nchar(words, type="w")
	    if(anyNA(nc)) {
		## use byte count as a reasonable substitute
		nc0 <- nchar(words, type="b")
		nc[is.na(nc)] <- nc0[is.na(nc)]
	    }

            ## Remove extra white space unless after a period which
            ## hopefully ends a sentence.
            ## Add ? ! as other possible ends, and there might be
            ## quoted and parenthesised sentences.
            ## NB, input could be invalid here.
            if(any(nc == 0L)) {
                zLenInd <- which(nc == 0L)
                zLenInd <- zLenInd[!(zLenInd %in%
                                     (grep("[.?!][)\"']{0,1}$", words,
                                           perl = TRUE, useBytes = TRUE) + 1L))]
                if(length(zLenInd)) {
                    words <- words[-zLenInd]
                    nc <- nc[-zLenInd]
                }
            }

            if(!length(words)) {
                yi <- c(yi, "", initial)
                next
            }

            currentIndex <- 0L
            lowerBlockIndex <- 1L
            upperBlockIndex <- integer()
            lens <- cumsum(nc + 1L)

            first <- TRUE
            maxLength <- width - nchar(initial, type="w") - indent

            ## Recursively build a sequence of lower and upper indices
            ## such that the words in line k are the ones in the k-th
            ## index block.
            while(length(lens)) {
                k <- max(sum(lens <= maxLength), 1L)
                if(first) {
                    first <- FALSE
                    maxLength <- width - nchar(prefix, type="w") - exdent
                }
                currentIndex <- currentIndex + k
                if(nc[currentIndex] == 0L)
                    ## Are we sitting on a space?
                    upperBlockIndex <- c(upperBlockIndex,
                                         currentIndex - 1L)
                else
                    upperBlockIndex <- c(upperBlockIndex,
                                         currentIndex)
                if(length(lens) > k) {
                    ## Are we looking at a space?
                    if(nc[currentIndex + 1L] == 0L) {
                        currentIndex <- currentIndex + 1L
                        k <- k + 1L
                    }
                    lowerBlockIndex <- c(lowerBlockIndex,
                                         currentIndex + 1L)
                }
                if(length(lens) > k)
                    lens <- lens[-seq_len(k)] - lens[k]
                else
                    lens <- NULL
            }

            nBlocks <- length(upperBlockIndex)
	    s <- paste0(c(initial, rep.int(prefix, nBlocks - 1L)),
			c(indentString, rep.int(exdentString, nBlocks - 1L)))
            initial <- prefix
            for(k in seq_len(nBlocks))
		s[k] <- paste0(s[k], paste(words[lowerBlockIndex[k] :
						 upperBlockIndex[k]],
					   collapse = " "))

            yi <- c(yi, s, prefix)
        }
        y <- if(length(yi))
            c(y, list(yi[-length(yi)]))
        else
            c(y, "")
    }

    if(length(pos <- which(enc == "latin1"))) {
        y[pos] <-
            lapply(y[pos],
                   function(s) {
                       e <- Encoding(s)
                       if(length(p <- which(e == "UTF-8")))
                           s[p] <- iconv(s[p], "UTF-8", "latin1",
                                         sub = "byte")
                       s
                   })
    }

    if(simplify) y <- as.character(unlist(y))
    y
}

formatDL <-
function(x, y, style = c("table", "list"),
         width = 0.9 * getOption("width"), indent = NULL)
{
    if(is.list(x)) {
        if(length(x) == 2L && diff(lengths(x)) == 0L) {
            y <- x[[2L]]; x <- x[[1L]]
        }
        else
            stop("incorrect value for 'x'")
    }
    else if(is.matrix(x)) {
        if(NCOL(x) == 2L) {
            y <- x[, 2L]; x <- x[, 1L]
        }
        else
            stop("incorrect value for 'x'")
    }
    else if(missing(y) && !is.null(nms <- names(x))) {
        y <- x
        x <- nms
    }
    else if(length(x) != length(y))
        stop("'x' and 'y' must have the same length")
    x <- as.character(x)
    if(!length(x)) return(x)
    y <- as.character(y)

    style <- match.arg(style)

    if(is.null(indent))
        indent <- switch(style, table = width / 3, list = width / 9)
    ## change 2017-03-12 suggeested by Bill Dunlap
    ## https://stat.ethz.ch/pipermail/r-devel/2017-March/073873.html
    ## if(indent > 0.5 * width)
    ##    warning("'indent' is too large for 'width' and will be reduced")
    indent <- min(indent, 0.5*width)

    indentString <- strrep(" ", indent)

    if(style == "table") {
        i <- (nchar(x, type="w") > indent - 3L)
        if(any(i))
            x[i] <- paste0(x[i], "\n", indentString)
        i <- !i
        if(any(i))
            x[i] <- formatC(x[i], width = indent, flag = "-")
        y <- lapply(strwrap(y, width = width - indent,
                            simplify = FALSE),
                    paste,
                    collapse = paste0("\n", indentString))
        r <- paste0(x, unlist(y))
    }
    else if(style == "list") {
        y <- strwrap(paste0(x, ": ", y), exdent = indent,
                     width = width, simplify = FALSE)
        r <- unlist(lapply(y, paste, collapse = "\n"))
    }
    r
}

trimws <- function(x, which = c("both", "left", "right"),
                   whitespace = "[ \t\r\n]")
{
    which <- match.arg(which)
    mysub <- function(re, x) sub(re, "", x, perl = TRUE)
    switch(which,
           "left" = mysub(paste0("^", whitespace, "+"), x),
           "right"= mysub(paste0(whitespace, "+$"), x),
           "both" = mysub(paste0(whitespace, "+$"),
                          mysub(paste0("^", whitespace, "+"), x)))
}
