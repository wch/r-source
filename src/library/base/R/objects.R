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
          "format.char", "format.info", "format.pval", "influence.measures",
          "plot.new", "plot.window", "plot.xy", "split.screen",
          "update.packages", "solve.QP", "solve.QP.compact", "print.graph",
          "lag.plot")
    groupGenerics <- c("Ops", "Math", "Summary")

    an <- lapply(seq(along=(sp <- search())), ls)
    names(an) <- sp
    an <- unlist(an)
    if (!missing(generic.function)) {
	if (!is.character(generic.function))
	    generic.function <- deparse(substitute(generic.function))
	name <- paste("^", generic.function, ".", sep = "")
        ## also look for registered methods in namespaces
        if(generic.function %in% groupGenerics)
            defenv <- .BaseNamespaceEnv
        else {
            genfun <- get(generic.function)
            defenv <- if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
        }
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

getS3method <-  function(f, class, optional = FALSE)
{
    groupGenerics <- c("Ops", "Math", "Summary")
    method <- paste(f, class, sep=".")
    if(exists(method)) return(get(method))
    ## also look for registered method in namespaces
    if(f %in% groupGenerics)
        defenv <- .BaseNamespaceEnv
    else {
        genfun <- get(f)
        defenv <- if (typeof(genfun) == "closure") environment(genfun)
        else .BaseNamespaceEnv
        S3Table <- get(".__S3MethodsTable__.", envir = defenv)
        S3reg <- ls(S3Table)
        if(length(grep(method, S3reg)))
            return(get(method, envir = S3Table))
    }
    if(optional) NULL
    else stop("S3 method ", method, " not found")
}

getFromNamespace <- function(x, ns, pos = -1, envir = as.environment(pos))
{
    if(missing(ns)) {
        nm <- attr(envir, "name")
        if(is.null(nm) || substring(nm, 1, 8) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9))
    } else ns <- asNamespace(ns)
    get(x, envir = ns, inherits = FALSE)
}

fixInNamespace <- function (x, ns, pos = -1, envir = as.environment(pos), ...)
{
    subx <- substitute(x)
    if (is.name(subx))
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1)
        stop("fixInNamespace requires a name")
    if(missing(ns)) {
        nm <- attr(envir, "name")
        if(is.null(nm) || substring(nm, 1, 8) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9))
    } else ns <- asNamespace(ns)
    x <- edit(get(subx, envir = ns, inherits = FALSE), ...)
    if(bindingIsLocked(subx, ns)) {
        unlockBinding(subx, ns)
        assign(subx, x, env = ns)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(subx, ns)
    } else
        assign(subx, x, env = ns)
    if(!isBaseNamespace(ns)) {
        ## now look for possible copy as a method
        S3 <- getNamespaceInfo(ns, "S3methods")
        if(!length(S3)) return(invisible(NULL))
        S3names <- sapply(S3, function(x) x[[3]])
        if(subx %in% S3names) {
            i <- match(subx, S3names)
            genfun <- get(S3[[i]][[1]])
            defenv <- if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
            S3Table <- get(".__S3MethodsTable__.", envir = defenv)
            if(exists(subx, envir = S3Table, inherits = FALSE))
                assign(subx, x, S3Table)
        }
    }
    invisible(NULL)
}
