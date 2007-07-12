t.test <- function(x, ...) UseMethod("t.test")

t.test.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95,
         ...)
{
    alternative <- match.arg(alternative)

    if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
        stop("'mu' must be a single number")
    if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
        stop("'conf.level' must be a single number between 0 and 1")
    if( !is.null(y) ) {
	dname <- paste(deparse(substitute(x)),"and",
		       deparse(substitute(y)))
	if(paired)
	    xok <- yok <- complete.cases(x,y)
	else {
	    yok <- !is.na(y)
	    xok <- !is.na(x)
	}
	y <- y[yok]
    }
    else {
	dname <- deparse(substitute(x))
	if( paired ) stop("'y' is missing for paired test")
	xok <- !is.na(x)
	yok <- NULL
    }
    x <- x[xok]
    if( paired ) {
	x <- x-y
	y <- NULL
    }
    nx <- length(x)
    mx <- mean(x)
    vx <- var(x)
    estimate <- mx
    if(is.null(y)) {
        if(nx < 2) stop("not enough 'x' observations")
	df <- nx-1
	stderr <- sqrt(vx/nx)
        if(stderr < 10 *.Machine$double.eps * abs(mx))
            stop("data are essentially constant")
	tstat <- (mx-mu)/stderr
	method <- ifelse(paired,"Paired t-test","One Sample t-test")
	names(estimate) <- ifelse(paired,"mean of the differences","mean of x")
    } else {
	ny <- length(y)
        if(nx < 1 || (!var.equal && nx < 2))
            stop("not enough 'x' observations")
	if(ny < 1 || (!var.equal && ny < 2))
            stop("not enough 'y' observations")
        if(var.equal && nx+ny < 3) stop("not enough observations")
	my <- mean(y)
	vy <- var(y)
	method <- paste(if(!var.equal)"Welch", "Two Sample t-test")
	estimate <- c(mx,my)
	names(estimate) <- c("mean of x","mean of y")
	if(var.equal) {
	    df <- nx+ny-2
            v <- 0
            if(nx > 1) v <- v + (nx-1)*vx
            if(ny > 1) v <- v + (ny-1)*vy
	    v <- v/df
	    stderr <- sqrt(v*(1/nx+1/ny))
	} else {
	    stderrx <- sqrt(vx/nx)
	    stderry <- sqrt(vy/ny)
	    stderr <- sqrt(stderrx^2 + stderry^2)
	    df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
	}
        if(stderr < 10 *.Machine$double.eps * max(abs(mx), abs(my)))
            stop("data are essentially constant")
        tstat <- (mx - my - mu)/stderr
    }
    if (alternative == "less") {
	pval <- pt(tstat, df)
	cint <- c(-Inf, tstat + qt(conf.level, df) )
    }
    else if (alternative == "greater") {
	pval <- pt(tstat, df, lower.tail = FALSE)
	cint <- c(tstat - qt(conf.level, df), Inf)
    }
    else {
	pval <- 2 * pt(-abs(tstat), df)
	alpha <- 1 - conf.level
        cint <- qt(1 - alpha/2, df)
	cint <- tstat + c(-cint, cint)
    }
    cint <- mu + cint * stderr
    names(tstat) <- "t"
    names(df) <- "df"
    names(mu) <- if(paired || !is.null(y)) "difference in means" else "mean"
    attr(cint,"conf.level") <- conf.level
    rval <- list(statistic = tstat, parameter = df, p.value = pval,
	       conf.int=cint, estimate=estimate, null.value = mu,
	       alternative=alternative,
	       method=method, data.name=dname)
    class(rval) <- "htest"
    return(rval)
}

t.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3)
       || (length(attr(terms(formula[-2]), "term.labels")) != 1))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("t.test", c(DATA, list(...)))
    y$data.name <- DNAME
    if(length(y$estimate) == 2)
        names(y$estimate) <- paste("mean in group", levels(g))
    y
}
