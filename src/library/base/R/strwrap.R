strtrim <- function(x, width)
{
    if(!is.character(x)) x <- as.character(x)
    .Internal(strtrim(x, width))
}

strwrap <-
function(x, width = 0.9 * getOption("width"), indent = 0, exdent = 0,
         prefix = "", simplify = TRUE)
{
    if(!is.character(x)) x <- as.character(x)
    ## Useful variables.
    indentString <- paste(rep.int(" ", indent), collapse = "")
    exdentString <- paste(rep.int(" ", exdent), collapse = "")
    y <- list()                         # return value
    z <- lapply(strsplit(x, "\n[ \t\n]*\n"), strsplit, "[ \t\n]")
    ## Now z[[i]][[j]] is a character vector of all "words" in
    ## paragraph j of x[i].

    for(i in seq_along(z)) {
        yi <- character(0)
        for(j in seq_along(z[[i]])) {
            ## Format paragraph j in x[i].
            words <- z[[i]][[j]]
            nc <- nchar(words, type="w")
	    if(any(is.na(nc))) {
		## use byte count as a reasonable substitute
		nc0 <- nchar(words)
		nc[is.na(nc)] <- nc0[is.na(nc)]
	    }

            ## Remove extra white space unless after a period which
            ## hopefully ends a sentence.
            if(any(nc == 0)) {
                zLenInd <- which(nc == 0)
                zLenInd <- zLenInd[!(zLenInd %in%
                                     (grep("\\.$", words) + 1))]
                if(length(zLenInd) > 0) {
                    words <- words[-zLenInd]
                    nc <- nc[-zLenInd]
                }
            }

            if(length(words) == 0) {
                yi <- c(yi, "", prefix)
                next
            }

            currentIndex <- 0
            lowerBlockIndex <- 1
            upperBlockIndex <- integer(0)
            lens <- cumsum(nc + 1)

            first <- TRUE
            maxLength <- width - nchar(prefix, type="w") - indent

            ## Recursively build a sequence of lower and upper indices
            ## such that the words in line k are the ones in the k-th
            ## index block.
            while(length(lens) > 0) {
                k <- max(sum(lens <= maxLength), 1)
                if(first) {
                    first <- FALSE
                    maxLength <- maxLength + indent - exdent
                }
                currentIndex <- currentIndex + k
                if(nc[currentIndex] == 0)
                    ## Are we sitting on a space?
                    upperBlockIndex <- c(upperBlockIndex,
                                         currentIndex - 1)
                else
                    upperBlockIndex <- c(upperBlockIndex,
                                         currentIndex)
                if(length(lens) > k) {
                    ## Are we looking at a space?
                    if(nc[currentIndex + 1] == 0) {
                        currentIndex <- currentIndex + 1
                        k <- k + 1
                    }
                    lowerBlockIndex <- c(lowerBlockIndex,
                                         currentIndex + 1)
                }
                if(length(lens) > k)
                    lens <- lens[-(1:k)] - lens[k]
                else
                    lens <- NULL
            }

            nBlocks <- length(upperBlockIndex)
            s <- paste(prefix,
                       c(indentString, rep.int(exdentString, nBlocks - 1)),
                       sep = "")
            for(k in (1 : nBlocks))
                s[k] <- paste(s[k], paste(words[lowerBlockIndex[k] :
                                                upperBlockIndex[k]],
                                          collapse = " "),
                              sep = "")
            yi <- c(yi, s, prefix)
        }
        y <- if(length(yi))
            c(y, list(yi[-length(yi)]))
        else
            c(y, "")
    }

    if(simplify) y <- unlist(y)
    y
}

formatDL <-
function(x, y, style = c("table", "list"),
         width = 0.9 * getOption("width"), indent = NULL)
{
    if(is.list(x)) {
        if((length(x) == 2) && (diff(sapply(x, length)) == 0)) {
            y <- x[[2]]; x <- x[[1]]
        }
        else
            stop("incorrect value for 'x'")
    }
    else if(is.matrix(x)) {
        if(NCOL(x) == 2) {
            y <- x[, 2]; x <- x[, 1]
        }
        else
            stop("incorrect value for 'x'")
    }
    else if(length(x) != length(y))
        stop("'x' and 'y' must have the same length")
    x <- as.character(x)
    if(length(x) == 0) return(x)
    y <- as.character(y)

    style <- match.arg(style)

    if(is.null(indent))
        indent <- switch(style, table = width / 3, list = width / 9)
    if(indent > 0.5 * width)
        stop("incorrect values of 'indent' and 'width'")

    indentString <- paste(rep.int(" ", indent), collapse = "")

    if(style == "table") {
        i <- (nchar(x, type="w") > indent - 3)
        if(any(i))
            x[i] <- paste(x[i], "\n", indentString, sep = "")
        i <- !i
        if(any(i))
            x[i] <- formatC(x[i], width = indent, flag = "-")
        y <- lapply(strwrap(y, width = width - indent,
                            simplify = FALSE),
                    paste,
                    collapse = paste("\n", indentString, sep = ""))
        r <- paste(x, unlist(y), sep = "")
    }
    else if(style == "list") {
        y <- strwrap(paste(x, ": ", y, sep = ""), exdent = indent,
                     width = width, simplify = FALSE)
        r <- unlist(lapply(y, paste, collapse = "\n"))
    }
    r
}
