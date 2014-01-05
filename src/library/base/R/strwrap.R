#  File src/library/base/R/strwrap.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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
    indentString <- paste(rep.int(" ", indent), collapse = "")
    exdentString <- paste(rep.int(" ", exdent), collapse = "")
    y <- list()                         # return value
    UB <- TRUE
    ## input need not be valid in this locale, e.g. from write.dcf
    ## but if x has UTF-8 encoding we want to preserve it, so
    if(all(Encoding(x) == "UTF-8")) UB <- FALSE
    else {
        ## Let's convert anything else with a marked encoding
        ## to the current locale
        enc <- Encoding(x) %in% c("latin1", "UTF-8")
        if(length(enc)) x[enc] <- enc2native(x[enc])
    }
    z <- lapply(strsplit(x, "\n[ \t\n]*\n", perl = TRUE, useBytes = UB),
                strsplit, "[ \t\n]", perl = TRUE, useBytes = UB)
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

    if(simplify) y <- as.character(unlist(y))
    y
}

formatDL <-
function(x, y, style = c("table", "list"),
         width = 0.9 * getOption("width"), indent = NULL)
{
    if(is.list(x)) {
        if(length(x) == 2L && diff(vapply(x, length, 1L)) == 0L) {
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
    if(indent > 0.5 * width)
        stop("incorrect values of 'indent' and 'width'")

    indentString <- paste(rep.int(" ", indent), collapse = "")

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
