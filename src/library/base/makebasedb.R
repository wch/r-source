local({
    makeLazyLoadDB <- function(from, filebase, compress = TRUE, ascii = FALSE,
                               variables) {

        envlist <- function(e) {
            names <- ls(e, all=TRUE)
            list <- .Call("R_getVarsFromFrame", names, e, FALSE, PACKAGE="base")
            names(list) <- names
            list
        }

        envtable <- function() {
            idx <- 0
            envs <- NULL
            enames <- character(0)
            find <- function(v, keys, vals)
                for (i in seq(along=keys))
                    if (identical(v, keys[[i]]))
                        return(vals[i])
            getname <- function(e) find(e, envs, enames)
            getenv <- function(n) find(n, enames, envs)
            insert <- function(e) {
                idx <<- idx + 1
                name <- paste("env", idx, sep="::")
                envs <<- c(e, envs)
                enames <<- c(name, enames)
                name
            }
            list(insert = insert, getenv = getenv, getname = getname)
        }

        lazyLoadDBinsertValue <- function(value, file, ascii, compress, hook)
            .Call("R_lazyLoadDBinsertValue", value, file, ascii, compress, hook,
                  PACKAGE = "base")

        lazyLoadDBinsertListElement <- function(x, i, file, ascii, compress, hook)
            .Call("R_lazyLoadDBinsertValue", x[[i]], file, ascii, compress, hook,
                  PACKAGE = "base")

        lazyLoadDBinsertVariable <- function(n, e, file, ascii, compress, hook) {
            x <- .Call("R_getVarsFromFrame", n, e, FALSE, PACKAGE="base")
           .Call("R_lazyLoadDBinsertValue", x[[1]], file, ascii, compress, hook,
                  PACKAGE = "base")
        }

        mapfile <- paste(filebase, "rdx", sep = ".")
        datafile <- paste(filebase, "rdb", sep = ".")
        close(file(datafile, "w")) # truncate to zero
        table <- envtable()
        varenv <- new.env(hash = TRUE)
        envenv <- new.env(hash = TRUE)

        envhook <- function(e) {
            if (is.environment(e)) {
                name <- table$getname(e)
                if (is.null(name)) {
                    name <- table$insert(e)
                    data <- list(bindings = envlist(e),
                                 enclos = parent.env(e))
                    key <- lazyLoadDBinsertValue(data, datafile, ascii,
                                                 compress, envhook)
                    assign(name, key, env = envenv)
                }
                name
            }
        }

        if (is.null(from) || is.environment(from)) {
            if (! missing(variables))
                vars <- variables
            else vars <- ls(from, all = TRUE)
        }
        else if (is.list(from)) {
            vars <- names(from)
            if (length(vars) != length(from) || any(nchar(vars) == 0))
                stop("source list must have names for all elements")
        }
        else stop("source must be an environment or a list");

        for (i in seq(along = vars)) {
            if (is.null(from) || is.environment(from))
                key <- lazyLoadDBinsertVariable(vars[i], from, datafile,
                                                ascii, compress,  envhook)
            else key <- lazyLoadDBinsertListElement(from, i, datafile, ascii,
                                                    compress, envhook)
            assign(vars[i], key, env = varenv)
        }

        vals <- lapply(vars, get, env = varenv, inherits = FALSE)
        names(vals) <- vars

        rvars <- ls(envenv, all = TRUE)
        rvals <- lapply(rvars, get, env = envenv, inherits = FALSE)
        names(rvals) <- rvars

        val <- list(variables = vals, references = rvals,
                    compressed = compress)
       .saveRDS(val, mapfile)
    }

    omit <- c(".Last.value", ".AutoloadEnv")

    if (length(search()[search()!="Autoloads"]) != 2)
        stop("start R with NO packages loaded to create the data base")

    baseFileBase <- file.path(.libPaths()[1],"base","R","base")

    if (file.info(baseFileBase)["size"] < 20000) # crude heuristic
        stop("may already be using lazy loading on base");

    basevars <- ls(NULL,all=TRUE)
    basevars <- basevars[! basevars %in% omit]

# **** need prims too since some prims have several names (is.name, is.symbol)
#    basevars <- ls(NULL,all=TRUE)
#    notPrim <- sapply(basevars, function(n)
#        ! typeof(get(n, NULL)) %in% c("builtin","special"))
#    makeLazyLoadDB(NULL, baseFileBase, variables = basevars[notPrim])

    makeLazyLoadDB(NULL, baseFileBase, variables = basevars)
#    q(save = "no", runLast = FALSE)
})
