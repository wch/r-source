## 'objects <- function(....) ...    --->>> ./attach.R

inherits <- function(x, what, which = FALSE)
	.Internal(inherits(x, what, which))

NextMethod <- function(generic=NULL, object=NULL, ...)
    .Internal(NextMethod(generic, object,...))

methods <- function (generic.function, class)
{
    ## this list is taken from .makeS3MethodsStopList in tools/R/utils.R
    S3MethodsStopList <-
        c("boxplot.stats", "close.screen", "close.socket", "flush.console",
          "format.char", "format.info", "format.pval", "plot.new",
          "plot.window", "plot.xy", "split.screen", "update.packages",
          "solve.QP", "solve.QP.compact","print.graph", "lag.plot")

    an <- lapply(seq(along=(sp <- search())), ls)
    names(an) <- sp
    an <- unlist(an)
    if (!missing(generic.function)) {
	if (!is.character(generic.function))
	    generic.function <- deparse(substitute(generic.function))
	name <- paste("^", generic.function, ".", sep = "")
        ## also look for registered methods in namespaces
        genfun <- get(generic.function)
        defenv <- if (typeof(genfun) == "closure") environment(genfun)
        else .BaseNamespaceEnv
        S3reg <- ls(get(".__S3MethodsTable__.", envir = defenv))
        an <- c(an, S3reg)
    }
    else if (!missing(class)) {
	if (!is.character(class))
	    class <- paste(deparse(substitute(class)))
	name <- paste(".", class, "$", sep = "")
    }
    else stop("must supply generic.function or class")
    res <- sort(grep(gsub("([.[])", "\\\\\\1", name), an, value = TRUE))
    res[! res %in% S3MethodsStopList]
}

data.class <- function(x) {
    if (length(cl <- oldClass(x)))
	cl[1]
    else {
	l <- length(dim(x))
	if (l == 2)	"matrix"
	else if (l > 0)	"array"
	else mode(x)
    }
}
