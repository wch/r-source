print.htest <- function(x, digits = 4, quote = TRUE, prefix = "", ...)
{
    cat("\n")
    writeLines(strwrap(x$method, prefix = "\t"))
    cat("\n")
    cat("data: ", x$data.name, "\n")
    out <- character()
    if(!is.null(x$statistic))
	out <- c(out, paste(names(x$statistic), "=",
			    format(round(x$statistic, 4))))
    if(!is.null(x$parameter))
	out <- c(out, paste(names(x$parameter), "=",
			    format(round(x$parameter, 3))))
    if(!is.null(x$p.value)) {
	fp <- format.pval(x$p.value, digits = digits)
	out <- c(out, paste("p-value",
			    if(substr(fp,1,1) == "<") fp else paste("=",fp)))
    }
    writeLines(strwrap(paste(out, collapse = ", ")))
    if(!is.null(x$alternative)) {
	cat("alternative hypothesis: ")
	if(!is.null(x$null.value)) {
	    if(length(x$null.value) == 1) {
		alt.char <-
		    switch(x$alternative,
			   two.sided = "not equal to",
			   less = "less than",
			   greater = "greater than")
		cat("true", names(x$null.value), "is", alt.char,
		    x$null.value, "\n")
	    }
	    else {
		cat(x$alternative, "\nnull values:\n")
		print(x$null.value, ...)
	    }
	}
	else cat(x$alternative, "\n")
    }
    if(!is.null(x$conf.int)) {
	cat(format(100 * attr(x$conf.int, "conf.level")),
	    "percent confidence interval:\n",
	    format(c(x$conf.int[1], x$conf.int[2])), "\n")
    }
    if(!is.null(x$estimate)) {
	cat("sample estimates:\n")
	print(x$estimate, ...)
    }
    cat("\n")
    invisible(x)
}
