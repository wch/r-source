co.intervals <- function (x, number = 6, overlap = 0.5)
{
    x <- sort(x[!is.na(x)])
    n <- length(x)
    ## "from the record"
    r <- n/(number * (1 - overlap) + overlap)
    ii <- round(0:(number - 1) * (1 - overlap) * r)
    x1 <- x[1 + ii]
    xr <- x[r + ii]
    ## Omit any range of values identical with the previous range;
    ## happens e.g. when `number' is less than the number of distinct x values.
    keep <- c(TRUE, diff(x1) > 0 | diff(xr) > 0)
    ## Set eps > 0 to ensure that the endpoints of a range are never
    ## identical, allowing display of a given.values bar
    j.gt.0 <- 0 < (jump <- diff(x))
    eps <- 0.5 * if(any(j.gt.0)) min(jump[j.gt.0]) else 0
    cbind(x1[keep] - eps, xr[keep] + eps)
}

panel.smooth <- function(x, y, col = par("col"), pch = par("pch"),
			 col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch=pch, col=col)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
        lines(lowess(x[ok], y[ok], f=span, iter=iter), col = col.smooth, ...)
    }
}

coplot <-
    function(formula, data, given.values, panel=points, rows, columns,
             show.given = TRUE, col = par("fg"), pch=par("pch"),
             xlab = c(x.name, paste("Given :", a.name)),
             ylab = c(y.name, paste("Given :", b.name)),
             subscripts = FALSE,
             xlim, ylim, number = 6, overlap = 0.5, ...)
{
    deparen <- function(expr) {
	while (is.language(expr) && !is.name(expr) && deparse(expr[[1]])== "(")
	    expr <- expr[[2]]
	expr
    }
    bad.formula <- function() stop("invalid conditioning formula")
    bad.lengths <- function() stop("incompatible variable lengths")

    ## parse and check the formula

    formula <- deparen(formula)
    if (!inherits(formula, "formula"))
	bad.formula()
    y <- deparen(formula[[2]])
    rhs <- deparen(formula[[3]])
    if (deparse(rhs[[1]]) != "|")
	bad.formula()
    x <- deparen(rhs[[2]])
    rhs <- deparen(rhs[[3]])
    if (is.language(rhs) && !is.name(rhs)
	&& (deparse(rhs[[1]]) == "*" || deparse(rhs[[1]]) == "+")) {
	have.b <- TRUE
	a <- deparen(rhs[[2]])
	b <- deparen(rhs[[3]])
    } else {
	have.b <- FALSE
	a <- rhs
    }

    ## evaluate the formulae components to get the data values

    if (missing(data))
	data <- sys.frame(sys.parent())
    x.name <- deparse(x)
    x <- eval(x, data, sys.frame(sys.parent()))
    nobs <- length(x)
    y.name <- deparse(y)
    y <- eval(y, data, sys.frame(sys.parent()))
    if(length(y) != nobs) bad.lengths()
    a.name <- deparse(a)
    a <- eval(a, data, sys.frame(sys.parent()))
    if(length(a) != nobs) bad.lengths()
    if(is.character(a)) a <- as.factor(a)
    a.levels <- NULL
    if (have.b) {
        b.levels <- NULL
	b.name <- deparse(b)
	b <- eval(b, data, sys.frame(sys.parent()))
	if(length(b) != nobs) bad.lengths()
        if(is.character(b)) b <- as.factor(b)
        missingrows <- which(is.na(x) | is.na(y) | is.na(a) | is.na(b))
    }
    else {
        missingrows <- which(is.na(x) | is.na(y) | is.na(a))
        b <- NULL
	b.name <- "" # for default ylab
    }

    ## generate the given value intervals

    number <- as.integer(number)
    if(length(number)==0 || any(number < 1)) stop("number must be integer >= 1")
    if(any(overlap >= 1)) stop("overlap must be < 1 (and typically >= 0).")

    bad.givens <- function() stop("invalid given.values")
    if(missing(given.values)) {
	a.intervals <-
            if(is.factor(a)) {
                i <- 1:nlevels(a)
                a.levels <- levels(a)
                a <- as.numeric(a)
                cbind(i - 0.5, i + 0.5)
            } else co.intervals(a,number=number[1],overlap=overlap[1])
	b.intervals <-
            if (have.b) {
                if(is.factor(b)) {
                    i <- 1:nlevels(b)
                    b.levels <- levels(b)
                    b <- as.numeric(b)
                    cbind(i - 0.5, i + 0.5)
                }
                else {
                    if(length(number)==1) number  <- rep(number,2)
                    if(length(overlap)==1)overlap <- rep(overlap,2)
                    co.intervals(b,number=number[2],overlap=overlap[2])
                }
            }
    } else {
	if(!is.list(given.values))
	    given.values <- list(given.values)
	if(length(given.values) != (if(have.b) 2 else 1))
	    bad.givens()
	a.intervals <- given.values[[1]]
	if(is.factor(a)) {
            if (is.character(a.intervals))
                a.intervals <- match(a.intervals, levels(a))
            a.intervals <- cbind(a.intervals - 0.5, a.intervals + 0.5)
            a.levels <- levels(a)
	    a <- as.numeric(a)
	}
        else if(is.numeric(a)) {
	    if(!is.numeric(a.intervals)) bad.givens()
	    if(!is.matrix(a.intervals) || ncol(a.intervals) != 2)
		a.intervals <- cbind(a.intervals - 0.5, a.intervals + 0.5)
	}
	if(have.b) {
	    b.intervals <- given.values[[2]]
	    if(is.factor(b)) {
                if (is.character(b.intervals))
                    b.intervals <- match(b.intervals, levels(b))
                b.intervals <- cbind(b.intervals - 0.5, b.intervals + 0.5)
                b.levels <- levels(b)
		b <- as.numeric(b)
	    }
            else if(is.numeric(b)) {
		if(!is.numeric(b.intervals)) bad.givens()
		if(!is.matrix(b.intervals) || ncol(b.intervals) != 2)
                    b.intervals <- cbind(b.intervals - 0.5, b.intervals + 0.5)
	    }
	}
    }
    if(any(is.na(a.intervals)) || (have.b && any(is.na(b.intervals))))
        bad.givens()

    ## compute the page layout

    if (have.b) {
	rows    <- nrow(b.intervals)
	columns <- nrow(a.intervals)
	nplots <- rows * columns
        if(length(show.given) < 2) show.given <- rep(show.given, 2)
    }
    else {
	nplots <- nrow(a.intervals)
	if (missing(rows)) {
	    if (missing(columns)) { ## default
		rows <- ceiling(round(sqrt(nplots)))
		columns <- ceiling(nplots/rows)
	    }
	    else rows <- ceiling(nplots/columns)
	}
	else if (missing(columns))
	    columns <- ceiling(nplots/rows)
	if (rows * columns < nplots)
	    stop("rows * columns too small")
    }
    total.columns <- columns
    total.rows <- rows
    f.col <- f.row <- 1
    if(show.given[1]) {
        total.rows <- rows + 1
        f.row <- rows/total.rows
    }
    if(have.b && show.given[2]) {
        total.columns <- columns + 1
        f.col <- columns/total.columns
    }

    ## Start Plotting only now

    opar <- par(mfrow = c(total.rows, total.columns),
		oma = if(have.b) rep(5, 4) else c(5, 6, 5, 4),
		mar = if(have.b) rep(0, 4) else c(0.5, 0, 0.5, 0),
		new = FALSE)
    on.exit(par(opar))
    plot.new()
    if( missing(xlim) )
        xlim <- range(x[is.finite(x)])
    if( missing(ylim) )
        ylim <- range(y[is.finite(y)])
    pch <- rep(pch, length=nobs)
    col <- rep(col, length=nobs)
    do.panel <- function(index, subscripts = FALSE) {
        ## Use `global' variables
        ##	id;     rows, columns,  total.rows, total.columns, nplots
        ##		xlim, ylim
	istart <- (total.rows - rows) + 1
	i <- total.rows - ((index - 1)%/%columns)
	j <- (index - 1)%%columns + 1
	par(mfg = c(i, j, total.rows, total.columns))
	plot.new()
	plot.window(xlim, ylim, log = "")
        if(any(is.na(id))) id[is.na(id)] <- FALSE
	if(any(id)) {
	    grid(lty="solid")
            if(subscripts)
                panel(x[id], y[id], subscripts = id,
                      col = col[id], pch=pch[id], ...)
            else
                panel(x[id], y[id], col = col[id], pch=pch[id], ...)
	}
	if((i == total.rows) && (j%%2 == 0))
	    axis(1, xpd=NA)
	else if((i == istart || index + columns > nplots) && (j%%2 == 1))
	    axis(3, xpd=NA)
	if((j == 1) && ((total.rows - i)%%2 == 0))
	    axis(2, xpd=NA)
	else if((j == columns || index == nplots) && ((total.rows - i)%%2 == 1))
	    axis(4, xpd=NA)
	## if(i == total.rows)
	##	axis(1, labels = (j%%2 == 0))
	## if(i == istart || index + columns > nplots)
	##	axis(3, labels = (j%%2 == 1))
	## if(j == 1)
	##	axis(2, labels = ((total.rows - i)%%2 == 0))
	## if(j == columns || index == nplots)
	##	axis(4, labels = ((total.rows - i)%%2 == 1))
	box()
    }## END function do.panel()

    if(have.b) {
	count <- 1
	for(i in 1:rows) {
	    for(j in 1:columns) {
		id <- ((a.intervals[j,1] <= a) & (a <= a.intervals[j,2]) &
		       (b.intervals[i,1] <= b) & (b <= b.intervals[i,2]))
		do.panel(count, subscripts)
		count <- count + 1
	    }
	}
    } else {
	for (i in 1:nplots) {
	    id <- ((a.intervals[i,1] <= a) & (a <= a.intervals[i,2]))
	    do.panel(i, subscripts)
	}
    }
    mtext(xlab[1], side=1, at=0.5*f.col, outer=TRUE, line=3.5, xpd=NA)
    mtext(ylab[1], side=2, at=0.5*f.row, outer=TRUE, line=3.5, xpd=NA)

    if(length(xlab) == 1) xlab <- c(xlab, paste("Given :", a.name))

    if(show.given[1]) {
	mar <- par("mar")
	nmar <- mar + c(4,0,0,0)
	par(fig = c(0, f.col, f.row, 1), mar = nmar, new=TRUE)
	plot.new()
	nint <- nrow(a.intervals)
	plot.window(range(a.intervals[is.finite(a.intervals)]),
                    0.5 + c(0, nint), log="")
        bg <-
            if (is.null(a.levels))
                gray(0.9)
            else {
                mid <- apply(a.intervals, 1, mean)
                text(mid, 1:nint, a.levels)
                NULL
            }
        rect(a.intervals[, 1], 1:nint - 0.3,
             a.intervals[, 2], 1:nint + 0.3, col = bg)

	axis(3, xpd=NA)
	axis(1, labels=FALSE)
	box()
	mtext(xlab[2], side=3, at=mean(par("usr")[1:2]), line=3, xpd=NA)
    }
    else { ## i. e. !show.given
        mtext(xlab[2], side=3, at= 0.5*f.col, line= 3.25, outer= TRUE, xpd=NA)
    }
    if(have.b) {
        if(length(ylab) == 1) ylab <- c(ylab, paste("Given :", b.name))
        if(show.given[2]) {
            nmar <- mar + c(0, 4, 0, 0)
	    par(fig = c(f.col, 1, 0, f.row), mar = nmar, new=TRUE)
	    plot.new()
	    nint <- nrow(b.intervals)
	    plot.window(0.5+c(0, nint),
			range(b.intervals, finite=TRUE), log="")
            bg <-
                if (is.null(b.levels))
                    gray(0.9)
                else {
                    mid <- apply(b.intervals, 1, mean)
                    text(1:nint, mid, b.levels, srt = 90)
                    NULL
                }
            rect(1:nint - 0.3, b.intervals[, 1],
                 1:nint + 0.3, b.intervals[, 2], col = bg)
	    axis(4, xpd=NA)
	    axis(2, labels=FALSE)
	    box()
	    mtext(ylab[2], side=4, at=mean(par("usr")[3:4]), line=3, xpd=NA)
	}
        else {
            mtext(ylab[2], side=4, at=0.5*f.row, line= 3.25, outer=TRUE, xpd=NA)
        }
    }
    if (length(missingrows) > 0) {
        cat("\nMissing rows:",missingrows,"\n")
        invisible(missingrows)
    }
}
