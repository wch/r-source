# returns value, extra protection.  BDR 29/5/98.
qqnorm <-
function(y, ylim, main="Normal Q-Q Plot",
         xlab="Theoretical Quantiles", ylab="Sample Quantiles",
         plot.it=TRUE, ...)
{
        y <- y[!is.na(y)]
        if(!length(y)) stop("y is empty")
        if (missing(ylim)) ylim <- range(y)
        x <- qnorm((1:length(y) - 0.5)/length(y))
        if(plot.it) plot(x, sort(y), main = main, xlab = xlab,
                         ylab = ylab, ylim = ylim, ...)
        invisible(list(x = x, y = y))
}

qqline <- 
function(y, ...)
{
        y <- quantile(y[!is.na(y)],c(0.25, 0.75)) 
        x <- qnorm(c(0.25, 0.75))
        slope <- diff(y)/diff(x)
        int <- y[1]-slope*x[1]
        abline(int, slope, ...)    
}
