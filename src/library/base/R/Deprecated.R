###----- NOTE:	../man/Deprecated.Rd   must be synchronized with this!
###		--------------------
.Deprecated <- function(new, package=NULL) {
    warning(paste(sQuote(as.character(sys.call(sys.parent())[[1]])),
		  " is deprecated.\n",
		  if (!missing(new))
		  paste("Use", sQuote(new), "instead.\n"),
		  "See help(\"Deprecated\") ",
                  if(!is.null(package))
                  paste("and help(\"", package, "-deprecated\").", sep=""),
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


## <entry>
## Deprecated in 1.9.0
tetragamma <- function(x) {
    .Deprecated("psigamma(*, deriv=2)")
    psigamma(x, deriv=2)
}
## </entry>


## <entry>
## Deprecated in 1.9.0
pentagamma <- function(x) {
    .Deprecated("psigamma(*, deriv=3)")
    psigamma(x, deriv=3)
}
## </entry>


## <entry>
## Deprecated in 1.9.0
package.description <- function(pkg, lib.loc=NULL, fields=NULL)
{
    .Deprecated("packageDescription")
    file <- system.file("DESCRIPTION", package = pkg, lib.loc = lib.loc)
    if(file != "") {
        retval <- read.dcf(file=file, fields=fields)[1,]
    }

    if((file == "") || (length(retval) == 0)){
        warning(paste("DESCRIPTION file of package", pkg,
                      "missing or broken"))
        if(!is.null(fields)){
            retval <- rep.int(NA, length(fields))
            names(retval) <- fields
        }
        else
            retval <- NA
    }

    retval
}
## </entry>
