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
          "plot.new", "plot.window", "plot.xy", "print.coefmat",
          "split.screen",
          "update.packages", "solve.QP", "solve.QP.compact", "print.graph",
          "lag.plot")
    groupGenerics <- c("Ops", "Math", "Summary")

    an <- lapply(seq(along=(sp <- search())), ls)
    names(an) <- sp
    an <- unlist(an)
    if (!missing(generic.function)) {
	if (!is.character(generic.function))
	    generic.function <- deparse(substitute(generic.function))
        ## <FIXME> generalize this later
        if(generic.function == "coefficients") generic.function <- "coef"
        if(generic.function == "fitted.values") generic.function <- "fitted"
	name <- paste("^", generic.function, ".", sep = "")
        ## also look for registered methods from namespaces
        if(generic.function %in% groupGenerics)
            defenv <- .BaseNamespaceEnv
        else {
            genfun <- get(generic.function)
            defenv <- if (typeof(genfun) == "closure") environment(genfun)
            else .BaseNamespaceEnv
        }
        S3reg <- ls(get(".__S3MethodsTable__.", envir = defenv))
        ## might both export and register a method
        an <- unique(c(an, S3reg))
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
    ## <FIXME> generalize this later
    if(f == "coefficients") f <- "coef"
    if(f == "fitted.values") f <- "fitted"
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
        if(length(grep(gsub("([.[])", "\\\\\\1", method), S3reg)))
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

getAnywhere <- function(x)
{
    if(!is.character(x)) x <- deparse(substitute(x))
    objs <- list(); where <- character(0); visible <- logical(0)
    ## first look on search path
    if(length(pos <- find(x, numeric=TRUE))) {
        objs <- lapply(pos, function(pos, x) get(x, pos=pos), x=x)
        where <- names(pos)
        visible <- rep(TRUE, length(pos))
    }
    ## next look for methods
    if(length(grep("\\.", x))) {
        parts <- strsplit(x, "\\.")[[1]]
        for(i in 2:length(parts)) {
            gen <- paste(parts[1:(i-1)], collapse="")
            cl <- paste(parts[2:length(parts)], collapse="")
            if(!is.null(f <- getS3method(gen, cl, TRUE))) {
                ev <- topenv(environment(f))
                nmev <- if(isNamespace(ev)) getNamespaceName(ev) else NULL
                objs <- c(objs, f)
                msg <- paste("registered S3 method for", gen)
                if(!is.null(nmev))
                    msg <- paste(msg, "from namespace", nmev)
                where <- c(where, msg)
                visible <- c(visible, FALSE)
            }
        }
    }
    ## now look in namespaces, visible or not
    for(i in loadedNamespaces()) {
        ns <- asNamespace(i)
        if(exists(x, envir = ns, inherits = FALSE)) {
            f <- get(x, envir = ns, inherits = FALSE)
            objs <- c(objs, f)
            where <- c(where, paste("namespace", i, sep=":"))
            visible <- c(visible, FALSE)
        }
    }
    # now check for duplicates
    ln <- length(objs)
    dups <- rep(FALSE, ln)
    objs2 <- lapply(objs, function(x) {
        if(is.function(x)) environment(x) <- NULL
        x
    })
    if(ln > 1)
        for(i in 2:ln)
            for(j in 1:(i-1))
                if(identical(objs2[[i]], objs2[[j]])) {
                    dups[i] <- TRUE
                    break
                }
    res <- list(name=x, objs=objs, where=where, visible=visible, dups=dups)
    class(res) <- "getAnywhere"
    res
}

print.getAnywhere <- function(x, ...)
{
    n <- sum(!x$dups)
    if(n == 0) {
        cat("no object named `", x$name, "' was found\n", sep="")
    } else if (n == 1) {
        cat("A single object matching `", x$name, "' was found\n", sep="")
        cat("It was found in the following places\n")
        cat(paste("  ", x$where, sep=""), sep="\n")
        cat("with value\n\n")
        print(x$objs[[1]])
    } else {
        cat(n, " differing objects matching `", x$name,
            "' were found\n", sep="")
        cat("in the following places\n")
        cat(paste("  ", x$where, sep=""), sep="\n")
        cat("Use [] to view one of them\n")
    }
    invisible(x)
}

"[.getAnywhere" <- function(x, i)
{
    if(!is.numeric(i)) stop("only numeric indices can be used")
    if(length(i) == 1) x$objs[[i]]
    else x$objs[i]
}
