prcomp <- function(x, scale=FALSE, use="all.obs") {
	if(scale) cv <- cor(as.matrix(x), use=use)
	else cv <- cov(as.matrix(x), use=use)
	edc <- svd(cv)[c("d", "u")]
	cn <- paste("Comp.", 1:ncol(cv), sep="")
	vn <- dimnames(x)[[2]]
	names(edc$d) <- cn
	dimnames(edc$u) <- list(vn, cn)
	edc <- list(var=edc$d, load=edc$u, scale=scale)
	class(edc) <- "prcomp"
	edc
}

print.prcomp <- function(x) {
	cat("\nPrincipal Components:", if(x$scale) "Correlation" else "Covariance",
		"matrix\n\n")
	cat("Component Variances:\n")
	print(x$var)
	cat("\nLoadings:\n")
	print(x$load)
	cat("\n")
}

plot.prcomp <- function(x, main="Scree Plot", ylab="Variance",
		xlab="Component", ...) {
	plot(x$var, main=main, xlab=xlab, ylab=ylab, ...)
}
