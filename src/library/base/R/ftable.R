ftable <- function(x, ...) UseMethod("ftable")

ftable.default <- function(..., exclude = c(NA, NaN),
                           row.vars = NULL, col.vars = NULL) {
    args <- list(...)
    if (length(args) == 0)
        stop("Nothing to tabulate")
    x <- args[[1]]
    if(is.list(x))
        x <- table(x, exclude = exclude)
    else if(inherits(x, "ftable")) {
        x <- ftable2table(x)
    }
    else if(!(is.array(x) && (length(dim(x)) > 1))) {
        x <- do.call("table",
                     c(as.list(substitute(list(...)))[-1],
                       list(exclude = exclude)))
    }
    dn <- dimnames(x)
    dx <- dim(x)
    n <- length(dx)
    if(!is.null(row.vars)) {
        if(is.character(row.vars)) {
            i <- pmatch(row.vars, names(dn))
            if(any(is.na(i)))
                stop("incorrect specification for `row.vars'")
            row.vars <- i
        } else if(any((row.vars < 1) | (row.vars > n)))
            stop("incorrect specification for `row.vars'")
    }
    if(!is.null(col.vars)) {
        if(is.character(col.vars)) {
            i <- pmatch(col.vars, names(dn))
            if(any(is.na(i)))
                stop("incorrect specification for `col.vars'")
            col.vars <- i
        } else if(any((col.vars < 1) | (col.vars > n)))
            stop("incorrect specification for `col.vars'")
    }
    i <- 1 : n
    if(!is.null(row.vars) && !is.null(col.vars)) {
        all.vars <- sort(c(row.vars, col.vars))
        if (length(all.vars) < n) {
            x <- apply(x, all.vars, sum)
            row.vars <- match(row.vars, all.vars)
            col.vars <- match(col.vars, all.vars)
            dn <- dn[all.vars]
            dx <- dx[all.vars]
        }
    }
    else if(!is.null(row.vars))
        col.vars <- i[-row.vars]
    else if(!is.null(col.vars))
        row.vars <- i[-col.vars]
    else {
        row.vars <- 1 : (n-1)
        col.vars <- n
    }

    y <- aperm(x, c(rev(row.vars), rev(col.vars)))
    dim(y) <- c(prod(dx[row.vars]), prod(dx[col.vars]))
    attr(y, "row.vars") <- dn[row.vars]
    attr(y, "col.vars") <- dn[col.vars]
    class(y) <- "ftable"
    y
}

ftable.formula <- function(formula, data = NULL, subset, na.action, ...)
{
    if(missing(formula) || !inherits(formula, "formula"))
        stop("formula is incorrect or missing")
    if(length(formula) != 3)
        stop("formula must have both left and right hand sides")
    if(any(attr(terms(formula), "order") > 1))
        stop("interactions are not allowed")
    rvars <- attr(terms(formula[-2]), "term.labels")
    cvars <- attr(terms(formula[-3]), "term.labels")
    rhs.has.dot <- any(rvars == ".")
    lhs.has.dot <- any(cvars == ".")
    if(lhs.has.dot && rhs.has.dot)
        stop("formula has `.' in both left and right hand side")
    if(missing(na.action))
        na.action <- getOption("na.action")
    m <- match.call(expand.dots = FALSE)
    edata <- eval(m$data, sys.frame(sys.parent()))
    if(inherits(edata, "ftable")
       || inherits(edata, "table")
       || length(dim(edata)) > 2) {
        if(inherits(edata, "ftable")) {
            data <- ftable2table(data)
        }
        varnames <- names(dimnames(data))
        if(rhs.has.dot)
            rvars <- NULL
        else {
            i <- pmatch(rvars, varnames)
            if(any(is.na(i)))
                stop("incorrect variable names in rhs of formula")
            rvars <- i
        }
        if(lhs.has.dot)
            cvars <- NULL
        else {
            i <- pmatch(cvars, varnames)
            if(any(is.na(i)))
                stop("incorrect variable names in lhs of formula")
            cvars <- i
        }
        ftable(data, row.vars = rvars, col.vars = cvars)
    }
    else {
        if(is.matrix(edata))
            m$data <- as.data.frame(data)
        m$... <- NULL
        if(!is.null(data) && is.environment(data)) {
            varnames <- names(data)
            if(rhs.has.dot)
                rvars <- seq(along = varnames)[-cvars]
            if(lhs.has.dot)
                cvars <- seq(along = varnames)[-rvars]
        }
        else {
            if(lhs.has.dot || rhs.has.dot)
                stop("cannot use dots in formula with given data")
        }
        m$formula <- formula(paste("~",
                                   paste(c(rvars, cvars),
                                         collapse = "+")))
        m[[1]] <- as.name("model.frame")
        mf <- eval(m, sys.frame(sys.parent()))
        ftable(mf, row.vars = rvars, col.vars = cvars, ...)
    }
}

print.ftable <- function(x) {
    if(!inherits(x, "ftable"))
        stop("x must be an `ftable'")
    makeLabels <- function(lst) {
        lens <- sapply(lst, length)
        cplensU <- c(1, cumprod(lens))
        cplensD <- rev(c(1, cumprod(rev(lens))))
        y <- NULL
        for (i in rev(seq(along = lst))) {
            ind <- 1 + seq(from = 0, to = lens[i] - 1) * cplensD[i + 1]
            tmp <- character(length = cplensD[i])
            tmp[ind] <- lst[[i]]
            y <- cbind(rep(tmp, times = cplensU[i]), y)
        }
        y
    }
    xrv <- attr(x, "row.vars")
    xcv <- attr(x, "col.vars")
    LABS <- cbind(rbind(matrix("", nr = length(xcv), nc = length(xrv)),
                        names(xrv), makeLabels(xrv)),
                  c(names(xcv), rep("", times = nrow(x) + 1)))
    DATA <- rbind(t(makeLabels(xcv)), rep("", times = ncol(x)), x)
    x <- cbind(apply(LABS, 2, formatC, flag = "-"),
               apply(DATA, 2, formatC))
    cat(t(x), sep = c(rep(" ", ncol(x) - 1), "\n"))
}

ftable2table <- function(x) {
    ## Note: it would be nicer to have as.table() for coercion to a
    ## standard contingency table.  But the term ``table'' is also used
    ## differently, so let's wait until this gets straightened out.
    if(!inherits(x, "ftable"))
        stop("x must be an `ftable'")
    xrv <- rev(attr(x, "row.vars"))
    xcv <- rev(attr(x, "col.vars"))
    x <- array(data = c(x),
               dim = c(sapply(xrv, length),
                       sapply(xcv, length)),
               dimnames = c(xrv, xcv))
    nrv <- length(xrv)
    ncv <- length(xcv)
    x <- aperm(x, c(seq(from = nrv, to = 1),
                    seq(from = nrv + ncv, to = nrv + 1)))
    class(x) <- "table"
    x
}
