autoload <- function(name, package, reset=FALSE, ...)
{
    if (!reset && exists(name, envir = .GlobalEnv, inherits = FALSE))
	stop("object with that name already exists")
    m <- match.call()
    m[[1]] <- as.name("list")
    newcall <- eval(m, parent.frame())
    newcall <- as.call(c(as.name("autoloader"), newcall))
    newcall$reset <- NULL
    if (is.na(match(package, .Autoloaded)))
	assign(".Autoloaded", c(package, .Autoloaded), env =.AutoloadEnv)
    assign(name, do.call("delay", list(newcall)), env = .AutoloadEnv)
    ## no longer return the result, which is a promise
    invisible()
}

autoloader <- function (name, package, ...)
{
    name <- paste(name, "", sep = "")
    rm(list = name, envir = .AutoloadEnv, inherits = FALSE)
    m <- match.call()
    m$name <- NULL
    m[[1]] <- as.name("library")
    ## load the package
    eval(m, .GlobalEnv)
    ## reset the autoloader
    autoload(name, package, reset = TRUE, ...)
    ## reevaluate the object
    where <- match(paste("package", package, sep = ":"), search())
    if (exists(name, where = where, inherits = FALSE))
	eval(as.name(name), as.environment(where))
    else
	stop("autoloader did not find ", sQuote(name), " in ", sQuote(package))
}
