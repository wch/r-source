
lcm <- function(x) { paste(x, "cm") }

layout <- function(mat, 
		   widths=rep(1, dim(mat)[2]),
		   heights=rep(1, dim(mat)[1]),
		   respect=F)
{
	if (is.matrix(mat) &&
	    (is.logical(respect) || (is.matrix(respect) &&
				     dim(respect)[1] == dim(mat)[1] &&
				     dim(respect)[2] == dim(mat)[2])))
	{
		num.rows <- dim(mat)[1]
		num.cols <- dim(mat)[2]
		num.figures <- max(mat)
			# check that each value in 1..n is mentioned
		for (i in 1:num.figures)
			if (match(i, mat, nomatch=0) == 0)
				num.figures <- 0
		if (num.figures > 0)
		{
			if (mode(widths) == "character")
				cm.widths <- grep("cm", widths)
			else
				cm.widths <- NULL
			if (mode(heights) == "character")
				cm.heights <- grep("cm", heights)
			else
				cm.heights <- NULL
			# pad widths/heights with 1's
			# and remove "cm" tags
			if (length(widths) < num.cols)
				widths <- c(widths, 
					    rep(1, num.cols-length(widths)))
			if (mode(widths) == "character")
			{
				widths[cm.widths] <- 
					substring(widths[cm.widths], 1, 
					          nchar(widths[cm.widths])-3)
				widths <- as.numeric(widths)
			}
			col.widths <- widths
			if (length(heights) < num.rows)
				heights <- c(heights, 
					     rep(1, num.rows-length(heights)))
			if (mode(heights) == "character")
			{
				heights[cm.heights] <- 
					substring(heights[cm.heights], 1, 
					          nchar(heights[cm.heights])-3)
				heights <- as.numeric(heights)
			}
			row.heights <- heights
			if (is.matrix(respect))
			{
				respect.mat <- respect
				respect <- as.integer(2)
			}
			else
			{
				respect.mat <- matrix(0, num.rows, num.cols)
				respect <- as.integer(respect)
			}
			.Internal(layout(num.rows, num.cols, mat,
					 as.integer(num.figures), 
					 col.widths, row.heights,
					 cm.widths, cm.heights,
					 respect, respect.mat))
		}
		else
		{
			cat("Layout matrix must contain at least one reference\n")
			cat("to each of the values 1..n (n > 0)\n")
		}
			
	}
	else
	{
		cat("The first parameter to 'layout' must be a matrix\n")
		cat("and the 'respect' parameter must be a logical or\n")
		cat("a matrix\n")
	}
}

layout.show <- function(n=1)
{
	# show the regions that will be allocated to the next
	# n figures

	# cheat to make sure that current plot is figure 1
	oma.saved <- par("oma")
	par(oma=rep(0,4))
	par(oma=oma.saved)

	mar.saved <- par("mar")
	par(mar=rep(0,4))

	for (i in 1:n)
	{
		plot.new()
		box()
		text(0.5, 0.5, i)
	}

	par(mar=mar.saved)
}


