t.test <- function(x, y=NULL, alternative = c("two.sided", "less", "greater"),
		   mu=0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
{
    alternative <- match.arg(alternative)

    if(!missing(mu) && (length(mu) != 1 || is.na(mu)))
        stop("mu must be a single number")
    if(!missing(conf.level) &&
       (length(conf.level) != 1 || !is.finite(conf.level) ||
        conf.level < 0 || conf.level > 1))
        stop("conf.level must be a single number between 0 and 1")
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
	if( paired ) stop("y is missing for paired test")
	xok <- !is.na(x)
	yok <- NULL
    }
    x <- x[xok]
    if( paired ) {
	x <- x-y
	y <- NULL
    }
    nx <- length(x)
    if(nx < 2) stop("not enough x observations")
    mx <- mean(x)
    vx <- var(x)
    estimate <- mx
    if(is.null(y)) {
	df <- length(x)-1
	stderr <- sqrt(vx/nx)
	tstat <- (mx-mu)/stderr
	method <- ifelse(paired,"Paired t-test","One Sample t-test")
	names(estimate) <- ifelse(paired,"mean of the differences","mean of x")
    } else {
	ny <- length(y)
	if(ny < 2) stop("not enough y observations")
	my <- mean(y)
	vy <- var(y)
	method <- paste(if(!var.equal)"Welch", "Two Sample t-test")
	estimate <- c(mx,my)
	names(estimate) <- c("mean of x","mean of y")
	if(var.equal) {
	    df <- nx+ny-2
	    v <- ((nx-1)*vx + (ny-1)*vy)/df
	    stderr <- sqrt(v*(1/nx+1/ny))
	} else {
	    stderrx <- sqrt(vx/nx)
	    stderry <- sqrt(vy/ny)
	    stderr <- sqrt(stderrx^2 + stderry^2)
	    df <- stderr^4/(stderrx^4/(nx-1) + stderry^4/(ny-1))
	}
        tstat <- (mx - my - mu)/stderr
    }
    if (alternative == "less") {
	pval <- pt(tstat, df)
	cint <- c(NA, tstat + qt(conf.level, df) )
    }
    else if (alternative == "greater") {
	pval <- pt(tstat, df, lower = FALSE)
	cint <- c(tstat - qt(conf.level, df), NA)
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
