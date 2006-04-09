lcm <- function(x) paste(x, "cm")#-> 3 characters (used in layout!)

layout <-
    function(mat, widths = rep(1, ncol(mat)),
	     heights = rep(1, nrow(mat)), respect = FALSE)
{
    storage.mode(mat) <- "integer"
    mat <- as.matrix(mat) # or barf
    if(!is.logical(respect)) {
	respect <- as.matrix(respect)#or barf
	if(!is.matrix(respect) || any(dim(respect) != dim(mat)))
	    stop("'respect' must be logical or matrix with same dimension as 'mat'")
    }
    num.figures <- as.integer(max(mat))
    ## check that each value in 1..n is mentioned
    for (i in 1:num.figures)
	if (match(i, mat, nomatch=0) == 0)
	    stop("layout matrix must contain at least one reference\nto each of the values {1..n}; here  n = ",
                 num.figures,"\n")

    dm <- dim(mat)
    num.rows <- dm[1]
    num.cols <- dm[2]

    cm.widths  <- if (is.character(widths)) grep("cm", widths, fixed = TRUE)
    cm.heights <- if (is.character(heights)) grep("cm", heights, fixed = TRUE)

    ## pad widths/heights with 1's	and remove "cm" tags
    pad1.rm.cm <- function(v, cm.v, len) {
	if ((ll <- length(v)) < len)
	    v <- c(v, rep.int(1, len-ll))
	if (is.character(v)) {
	    wcm <- v[cm.v]
	    v[cm.v] <- substring(wcm, 1, nchar(wcm, type="c") - 3)
	}
	as.numeric(v)
    }
    widths  <- pad1.rm.cm(widths, cm.widths,  len = num.cols)
    heights <- pad1.rm.cm(heights,cm.heights, len = num.rows)

    if (is.matrix(respect)) {
	respect.mat <- as.integer(respect)
	respect <- 2
    } else {# respect: logical	|--> 0 or 1
	respect.mat <- matrix(as.integer(0), num.rows, num.cols)
    }
    .Internal(layout(num.rows, num.cols,
		     mat,# integer
		     as.integer(num.figures),
		     col.widths = widths,
		     row.heights = heights,
		     cm.widths,
		     cm.heights,
		     respect = as.integer(respect),
		     respect.mat))
    invisible(num.figures)
}

layout.show <- function(n=1)
{
    ## cheat to make sure that current plot is figure 1
    oma.saved <- par("oma")
    par(oma=rep.int(0,4))
    par(oma=oma.saved)

    o.par <- par(mar=rep.int(0,4))
    on.exit(par(o.par))
    for (i in seq(length=n)) {
	plot.new()
	box()
	text(0.5, 0.5, i)
    }
}
