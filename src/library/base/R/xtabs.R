xtabs <- function(formula = ~., data = parent.frame(), subset,
		  na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE)
{
    if(!missing(formula) && !inherits(formula, "formula"))
	stop("formula is incorrect")
    if(any(attr(terms(formula), "order") > 1))
	stop("interactions are not allowed")
    if(missing(na.action))
	na.action <- getOption("na.action")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    m$... <- m$exclude <- m$drop.unused.levels <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    if(length(formula) == 2) {
	by <- mf
	y <- NULL
    }
    else {
	i <- attr(attr(mf, "terms"), "response")
	by <- mf[-i]
	y <- mf[[i]]
    }
    by <- lapply(by, function(u) {
	if(!is.factor(u)) u <- factor(u, exclude = exclude)
	u[ , drop = drop.unused.levels]
    })
    x <-
	if(is.null(y))
	    do.call("table", mf)
	else if(NCOL(y) == 1)
	    tapply(y, by, sum)
	else {
	    z <- lapply(as.data.frame(y), tapply, by, sum)
	    array(unlist(z),
		  dim = c(dim(z[[1]]), length(z)),
		  dimnames = c(dimnames(z[[1]]), list(names(z))))
	}
    x[is.na(x)] <- 0
    class(x) <- c("xtabs", "table")
    attr(x, "call") <- match.call()
    x
}

print.xtabs <- function(x, ...)
{
    ox <- x
    attr(x, "call") <- NULL
    print.table(x, ...)
    invisible(ox)
}

summary.xtabs <- function(object, ...)
{
    if(!inherits(object, "xtabs"))
	stop("object must inherit from class xtabs")
    n.cases <- sum(object)
    if(0 == (n.vars <- length(dim(object))))
	stop("object must be an array")
    m <- vector("list", length = n.vars)
    for(k in 1:n.vars) {
	m[[k]] <- margin.table(object, k) / n.cases
    }
    expected <- apply(do.call("expand.grid", m), 1, prod) * n.cases
    statistic <- sum((c(object) - expected)^2 / expected)
    parameter <- prod(sapply(m, length) - 1)
    y <- list(n.vars = n.vars,
	      n.cases = n.cases,
	      statistic = statistic,
	      parameter = parameter,
	      approx.ok = all(expected >= 5),
	      p.value = pchisq(statistic, parameter, lower.tail = FALSE),
	      call = attr(object, "call"))
    class(y) <- "summary.xtabs"
    y
}

print.summary.xtabs <- function(x, digits = max(1, getOption("digits") - 3))
{
    if(!inherits(x, "summary.xtabs"))
	stop("x must inherit from class `summary.xtabs'")
    cat("Call:\n")
    print(x$call)
    cat("Number of cases in table:", x$n.cases, "\n")
    cat("Number of factors:", x$n.vars, "\n")
    cat("Test for independence of all factors:\n")
    ch <- .Alias(x$statistic)
    cat("\tChisq = ",	format(round(ch, max(0, digits - log10(ch)))),
	", df = ",	x$parameter,
	", p-value = ",	format.pval(x$p.value, digits, eps = 0),
	"\n", sep = "")
    if(!x$approx.ok)
	cat("\tChi-squared approximation may be incorrect\n")
    invisible(x)
}
