###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new) {
    warning(paste(sQuote(as.character(sys.call(sys.parent())[[1]])),
		  " is deprecated.\n",
		  if (!missing(new))
		  paste("Use", sQuote(new), "instead.\n"),
		  "See ?Deprecated.",
		  sep = ""),
            call. = FALSE)
}

## consider keeping one (commented) entry here, for easier additions
## <entry>
## Deprecated in 1.9.0
La.eigen <- function (x, symmetric, only.values = FALSE,
                      method = c("dsyevr", "dsyev"))
{
    .Deprecated("eigen")
    if(!is.numeric(x) && !is.complex(x))
	stop("argument to La.eigen must be numeric or complex")
    method <- match.arg(method)
    x <- as.matrix(x)
    if (nrow(x) != ncol(x)) stop("non-square matrix in La.eigen")
    if (nrow(x) == 0) stop("0 x 0 matrix in La.eigen")
    if (any(!is.finite(x))) stop("infinite or missing values in x")
    complex.x <- is.complex(x)
    if (missing(symmetric)) {
        tx <- if(complex.x) Conj(t(x)) else t(x)
        test <- all.equal.numeric(x, tx, 100 * .Machine$double.eps)
        symmetric <- is.logical(test) && test
    }
    if (is.numeric(x)) storage.mode(x) <- "double"
    if (symmetric) {
        z <- if(!complex.x)
            .Call("La_rs", x, only.values, method, PACKAGE = "base")
        else
            .Call("La_rs_cmplx", x, only.values, PACKAGE = "base")
        ord <- rev(seq(along = z$values))
    } else {
        z <- if(!complex.x)
            .Call("La_rg", x, only.values, PACKAGE = "base")
        else
            .Call("La_rg_cmplx", x, only.values, PACKAGE = "base")
        ord <- sort.list(Mod(z$values), decreasing = TRUE)
    }
    list(values = z$values[ord],
         vectors = if (!only.values) z$vectors[, ord, drop = FALSE])
}
## </entry>


