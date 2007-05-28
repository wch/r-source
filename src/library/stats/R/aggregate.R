aggregate <- function(x, ...) UseMethod("aggregate")

aggregate.default <- function(x, ...) {
    if(is.ts(x))
        aggregate.ts(as.ts(x), ...)
    else
        aggregate.data.frame(as.data.frame(x), ...)
}

aggregate.data.frame <- function(x, by, FUN, ...) {
    if(!is.data.frame(x))
        x <- as.data.frame(x)
    if(!is.list(by))
        stop("'by' must be a list")
    if(is.null(names(by)))
        names(by) <- paste("Group", seq_along(by), sep = ".")
    else {
        nam <- names(by)
        ind <- which(!nzchar(nam))
        names(by)[ind] <- paste("Group", ind, sep = ".")
    }
    y <- lapply(x, tapply, by, FUN, ..., simplify = FALSE)
    if(any(sapply(unlist(y, recursive = FALSE), length) > 1L))
        stop("'FUN' must always return a scalar")
    z <- y[[1L]]
    d <- dim(z)
    w <- vector("list", length(d))
    for (i in seq_along(d)) {
        j <- rep.int(rep.int(seq_len(d[i]),
                             prod(d[seq_len(i - 1L)]) * rep.int(1L, d[i])),
                     prod(d[seq.int(from = i + 1L, length.out = length(d) - i)]))
        zz <- dimnames(z)[[i]][j]
        ## zz is character, so match to the levels created in tapply
        ## and not to as.character(by[[i]])
        w[[i]] <- by[[i]][match(zz, as.factor(by[[i]]))]
    }
    ## this gives w row names that may not be consecutive.
    w <- as.data.frame(w, stringsAsFactors = FALSE)[which(!unlist(lapply(z, is.null))), , drop = FALSE]
    y <- data.frame(w, lapply(y, unlist, use.names = FALSE),
                    stringsAsFactors = FALSE)
    names(y) <- c(names(by), names(x))
    row.names(y) <- NULL
    y
}

aggregate.ts <- function(x, nfrequency = 1, FUN = sum, ndeltat = 1,
                         ts.eps = getOption("ts.eps"), ...)
{
    x <- as.ts(x)
    ofrequency <- tsp(x)[3]
    ## Set up the new frequency, and make sure it is an integer.
    if(missing(nfrequency))
        nfrequency <- 1 / ndeltat
    if((nfrequency > 1) &&
        (abs(nfrequency - round(nfrequency)) < ts.eps))
        nfrequency <- round(nfrequency)

    if(nfrequency == ofrequency)
        return(x)
    ratio <- ofrequency /nfrequency
    if(abs(ratio - round(ratio)) > ts.eps)
        stop(gettextf("cannot change frequency from %g to %g",
                      ofrequency, nfrequency), domain = NA)
    ## The desired result is obtained by applying FUN to blocks of
    ## length ofrequency/nfrequency, for each of the variables in x.
    ## We first get the new start and end right, and then break x into
    ## such blocks by reshaping it into an array and setting dim.
    len <- ofrequency %/% nfrequency
    mat <- is.matrix(x)
    if(mat) cn <- colnames(x)
#    nstart <- ceiling(tsp(x)[1] * nfrequency) / nfrequency
#    x <- as.matrix(window(x, start = nstart))
    nstart <- tsp(x)[1]
    # Can't use nstart <- start(x) as this causes problems if
    # you get a vector of length 2.
    x <- as.matrix(x)
    nend <- floor(nrow(x) / len) * len
    x <- apply(array(c(x[1 : nend, ]),
                     dim = c(len, nend / len, ncol(x))),
               MARGIN = c(2, 3), FUN = FUN, ...)
    if(!mat) x <- as.vector(x)
    else colnames(x) <- cn
    ts(x, start = nstart, frequency = nfrequency)
}
