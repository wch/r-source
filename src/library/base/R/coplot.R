"co.intervals" <-
function (x, number = 6, overlap = 0.5)
{
	x <- sort(x[!is.na(x)])
	n <- length(x)
	## "from the record"
	r <- n/(number * (1 - overlap) + overlap)
	l <- round(1 + 0:(number - 1) * (1 - overlap) * r)
	u <- round(r + 0:(number - 1) * (1 - overlap) * r)
	cbind(x[l], x[u])
}

panel.smooth <-
function(x, y, col, pch, f=2/3, iter=3, ...)
{
	points(x, y, pch=pch, col=col)
	lines(lowess(x, y, f=f, iter=iter), ...)
}



coplot <-
function (formula, data, given.values, panel=points, rows, columns,
	show.given = TRUE, col = par("fg"), pch=par("pch"), ...)
{
 deparen <- function(expr) {
	while (is.language(expr) && !is.name(expr) && deparse(expr[[1]]) == "(")
	  expr <- expr[[2]]
	expr
 }
 bad.formula <- function() stop("invalid conditioning formula")
 bad.lengths <- function() stop("incompatible variable lengths")

 ## parse and check the formula

 formula <- deparen(formula)
 if (deparse(formula[[1]]) != "~")
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
 x <- eval(x, data)
 nobs <- length(x)
 y.name <- deparse(y)
 y <- eval(y, data)
 if(length(y) != nobs) bad.lengths()
 a.name <- deparse(a)
 a <- eval(a, data)
 if(length(a) != nobs) bad.lengths()
 if (have.b) {
	b.name <- deparse(b)
	b <- eval(b, data)
	if(length(b) != nobs) bad.lengths()
 }
 else b <- NULL

 ## generate the given value intervals

 bad.givens <- function() stop("invalid given.values")
 if(missing(given.values)) {
	if(is.factor(a)) {
		a.intervals <- cbind(1:nlevels(a), 1:nlevels(a))
		a <- as.numeric(a)
	}
	else a.intervals <- co.intervals(a)
	b.intervals <- NULL
	if (have.b)  {
		if(is.factor(b)) {
			b.intervals <- cbind(1:nlevels(b), 1:nlevels(b))
			b <- as.numeric(b)
		}
		else b.intervals <- co.intervals(b)
	}
 } else {
	 if(!is.list(given.values))
		given.values <- list(given.values)
	 if(length(given.values) != (if(have.b) 2 else 1))
		bad.givens()
	 a.intervals <- given.values[[1]]
	 if(is.factor(a)) {
		if(is.character(a.intervals))
			a.levels <- match(a.levels, levels(a))
		else a.levels <- cbind(a.levels, a.levels)
		a <- as.numeric(a)
	 } else if(is.numeric(a)) {
		if(!is.numeric(a)) bad.givens()
		if(!is.matrix(a.intervals) || ncol(a.intervals) != 2)
			a.intervals <- cbind(a.intervals, a.intervals)
	 }
	 if(have.b) {
		b.intervals <- given.values[[2]]
		if(is.factor(b)) {
			if(is.character(b.intervals))
				b.levels <- match(b.levels, levels(b))
			else b.levels <- cbind(b.levels, b.levels)
			b <- as.numeric(b)
		} else if(is.numeric(b)) {
			if(!is.numeric(b)) bad.givens()
			if(!is.matrix(b.intervals) || ncol(b.intervals) != 2)
				b.intervals <- cbind(b.intervals, b.intervals)
		}
	}
 }
 if(any(is.na(a.intervals))) bad.givens()
 if(have.b)
	if(any(is.na(b.intervals))) bad.givens()

 ## compute the page layout

 if (have.b) {
	rows <- nrow(b.intervals)
	columns <- nrow(b.intervals)
	nplots <- rows * columns
	total.rows <- rows + if (show.given) 1 else 0
	total.columns <- columns + if (show.given) 1 else 0
 } else {
	nplots <- nrow(a.intervals)
	if (missing(rows)) {
		if (missing(columns)) {
			rows <- ceiling(round(sqrt(nplots)))
			columns <- ceiling(nplots/rows)
		}
		else rows <- ceiling(nplots/columns)
	}
	else if (missing(columns))
		columns <- ceiling(nplots/rows)
	if (rows * columns < nplots)
		stop("rows * columns too small")
	total.rows <- rows + if (show.given) 1 else 0
	total.columns <- columns
 }

 ## Start Plotting only now

 opar <- par(mfrow = c(total.rows, total.columns),
	     oma = if(have.b) rep(5, 4) else c(5, 6, 5, 4),
	     mar = if(have.b) rep(0, 4) else c(0.5, 0, 0.5, 0),
	     new = FALSE)
 on.exit(par(opar))
 plot.new()
 xlim <- range(x, finite = TRUE)
 ylim <- range(y, finite = TRUE)
 pch <- rep(pch, length=nobs)
 col <- rep(col, length=nobs)
 do.panel <- function(index) {
	istart <- (total.rows - rows) + 1
	i <- total.rows - ((index - 1)%/%columns)
	j <- (index - 1)%%columns + 1
	par(mfg = c(i, j, total.rows, total.columns))
	plot.new()
	plot.window(xlim, ylim, log = "")
	if(any(id)) {
		grid(lty="solid")
		panel(x[id], y[id], col = col[id], pch=pch[id], ...)
	}
	if ((i == total.rows) && (j%%2 == 0))
		axis(1)
	if ((i == istart || index + columns > nplots) && (j%%2 == 1))
		axis(3)
	if ((j == 1) && ((total.rows - i)%%2 == 0))
		axis(2)
	if ((j == columns || index == nplots) && ((total.rows - i)%%2 == 1))
		axis(4)
	## if (i == total.rows)
	##	axis(1, labels = (j%%2 == 0))
	## if (i == istart || index + columns > nplots)
	##	axis(3, labels = (j%%2 == 1))
	## if (j == 1)
	##	axis(2, labels = ((total.rows - i)%%2 == 0))
	## if (j == columns || index == nplots)
	##	axis(4, labels = ((total.rows - i)%%2 == 1))
	box()
 }## do.panel
 if(have.b) {
	count <- 1
	for(i in 1:rows) {
		for(j in 1:columns) {
		 id <- ((a.intervals[j,1] <= a) & (a <= a.intervals[j,2]) &
			(b.intervals[i,1] <= b) & (b <= b.intervals[i,2]))
		 do.panel(count)
		 count <- count + 1
		}
	}
 } else {
	for (i in 1:nplots) {
		id <- ((a.intervals[i,1] <= a) & (a <= a.intervals[i,2]))
		do.panel(i)
	}
 }
 mtext(x.name, side=1, at=0.5*(columns/total.columns),
	outer=TRUE, line=3.5, xpd=TRUE)
 mtext(y.name, side=2, at=0.5*(rows/total.rows),
	outer=TRUE, line=3.5, xpd=TRUE)

 if(show.given) {
	mar <- par("mar")
	nmar <- mar + c(4,0,0,0)
	par(fig = c(0, columns/total.columns, rows/total.rows, 1),
	    mar = nmar, new=TRUE)
	plot.new()
	nint <- nrow(a.intervals)
	plot.window(range(a.intervals, finite=TRUE), .5+c(0, nint), log="")
	rect(a.intervals[,1], 1:nint-0.3,
	     a.intervals[,2], 1:nint+0.3, col=gray(0.9))
	axis(3)
	axis(1, labels=FALSE)
	box()
	mtext(paste("Given :", a.name),
	      side=3, at=mean(par("usr")[1:2]), line=3, xpd=TRUE)
	if(have.b) {
		nmar <- mar + c(0, 4, 0, 0)
		par(fig = c(columns/total.columns, 1, 0, rows/total.rows),
		    mar = nmar, new=TRUE)
		plot.new()
		nint <- nrow(b.intervals)
		plot.window(.5+c(0, nint),
			    range(b.intervals, finite=TRUE), log="")
		rect(1:nint-0.3, b.intervals[,1],
		     1:nint+0.3, b.intervals[,2], col=gray(0.9))
		axis(4)
		axis(2, labels=FALSE)
		box()
		mtext(paste("Given :", b.name),
			side=4, at=mean(par("usr")[3:4]), line=3, xpd=TRUE)
	}
 }
}
