fisher.test <- function(x, y = NULL, alternative = "two.sided",
                        workspace = 200000, hybrid = FALSE) {
    DNAME <- deparse(substitute(x))

    if (is.data.frame(x))
        x <- as.matrix(x)
    if (is.matrix(x)) {
        if (any(dim(x) < 2))
            stop("x must have at least 2 rows and columns")
        if (any(x < 0) || any(is.na(x))) 
            stop("all entries of x must be nonnegative and finite")
    }
    else {
        if (is.null(y)) 
            stop("if x is not a matrix, y must be given")
        if (length(x) != length(y)) 
            stop("x and y must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        OK <- complete.cases(x, y)
        x <- as.factor(x[OK])
        y <- as.factor(y[OK])
        if ((nlevels(x) < 2) || (nlevels(y) < 2)) 
            stop("x and y must have at least 2 levels")
        x <- table(x, y)
    }

    CHOICES <- c("two.sided", "less", "greater")
    alternative <- CHOICES[pmatch(alternative, CHOICES)]
    if (length(alternative) > 1 || is.na(alternative)) 
        stop("alternative must be \"two.sided\", \"less\" or \"greater\"")
    
    nr <- nrow(x)
    nc <- ncol(x)

    if ((nr == 2) && (nc == 2) && (alternative != "two.sided")) {
        m <- sum(x[, 1])
        n <- sum(x[, 2])
        k <- sum(x[1, ])
        x <- x[1, 1]
        PVAL <- switch(alternative,
                       less = phyper(x, m, n, k),
                       greater = 1 - phyper(x - 1, m, n, k))
    } else {
        if (hybrid) {
            warning("p-values may be incorrect")
            PVAL <- .C("fexact",
                       as.integer(nr),
                       as.integer(nc),
                       as.double(x),
                       as.integer(nr),
                       as.double(5),
                       as.double(80),
                       as.double(1),
                       as.double(0),
                       p = as.double(0),
                       as.integer(workspace),
                       PACKAGE = "ctest")$p
        } else
            PVAL <- .C("fexact",
                       as.integer(nr),
                       as.integer(nc),
                       as.double(x),
                       as.integer(nr),
                       as.double(-1),
                       as.double(100),
                       as.double(0),
                       as.double(0),
                       p = as.double(0),
                       as.integer(workspace),
                       PACKAGE = "ctest")$p
    }

    structure(list(p.value = PVAL,
                   alternative = alternative,
                   method = "Fisher's Exact Test for Count Data",
                   data.name = DNAME),
              class = "htest")
}
