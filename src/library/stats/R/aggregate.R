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
        stop(sQuote("by"), " must be a list")
    if(is.null(names(by)))
        names(by) <- paste("Group", seq(along = by), sep = ".")
    else {
        nam <- names(by)
        ind <- which(nchar(nam) == 0)
        names(by)[ind] <- paste("Group", ind, sep = ".")
    }
    y <- lapply(x, tapply, by, FUN, ..., simplify = FALSE)
    if(any(sapply(unlist(y, recursive = FALSE), length) > 1))
        stop(sQuote("FUN"), " must always return a scalar")
    z <- y[[1]]
    d <- dim(z)
    w <- NULL
    for (i in seq(along = d)) {
        j <- rep.int(rep.int(seq(1 : d[i]),
                     prod(d[seq(length = i - 1)]) * rep.int(1, d[i])),
                 prod(d[seq(from = i + 1, length = length(d) - i)]))
        w <- cbind(w, dimnames(z)[[i]][j])
    }
    w <- w[which(!unlist(lapply(z, is.null))), , drop = FALSE]
    y <- data.frame(w, lapply(y, unlist, use.names = FALSE))
    names(y) <- c(names(by), names(x))
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
    if(abs(ofrequency %% nfrequency) > ts.eps)
        stop("cannot change frequency from ",
             ofrequency, " to ", nfrequency)
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
