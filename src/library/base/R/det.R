### From Doug Bates' 20 Apr 1999 post to R-devel;
### "method" idea from J.K.Lindsey's rmutil
det <- function(x, method = c("qr","eigenvalues"))
{
    if(!is.matrix(x) || (n <- ncol(x)) != nrow(x))
	stop("x must be a square matrix")
    method <- match.arg(method) # ensures one from above
    if(method == "qr") {
        x <- prod(diag(qr(x)$qr)); if(n %% 2 == 1) x else -x
    } else ## method == "eigenvalues"
	Re(prod(eigen(x, only.values=TRUE)$values))
}

## S-plus' Matrix pkg has arg. "logarithm = TRUE" and returns list
##        (which is necessary for keeping the sign when taking log ..)
