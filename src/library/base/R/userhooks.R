.userHooksEnv <- new.env(hash = FALSE, parent = NULL)

getUserOnLoadHook <- function(pkgname)
{
    nm <- paste(pkgname, "onLoad", sep = "::")
    if (exists(nm, envir = .userHooksEnv, inherits = FALSE))
        get(nm, envir = .userHooksEnv, inherits = FALSE)
    else list()
}

setUserOnLoadHook <- function(pkgname, value,
                              action = c("append", "prepend", "replace"))
{
    action <- match.arg(action)
    nm <- paste(pkgname, "onLoad", sep = "::")
    old <- getUserOnLoadHook(pkgname)
    new <- switch(action,
                  "append" = c(old, value),
                  "prepend" = c(value, old),
                  "replace" = value)
    if (length(new))
        assign(nm, new, envir = .userHooksEnv, inherits = FALSE)
    else if(exists(nm, envir = .userHooksEnv, inherits = FALSE))
        remove(list=nm, envir = .userHooksEnv, inherits = FALSE)
    invisible()
}

getUserAttachHook <- function(pkgname)
{
    pkgname <- strsplit(pkgname, "_", fixed=TRUE)[[1]][1]
    nm <- paste(pkgname, "attach", sep = "::")
    if (exists(nm, envir = .userHooksEnv, inherits = FALSE))
        get(nm, envir = .userHooksEnv, inherits = FALSE)
    else list()
}

setUserAttachHook <- function(pkgname, value,
                              action = c("append", "prepend", "replace"))
{
    action <- match.arg(action)
    pkgname <- strsplit(pkgname, "_", fixed=TRUE)[[1]][1]
    nm <- paste(pkgname, "attach", sep = "::")
    old <- getUserAttachHook(pkgname)
    new <- switch(action,
                  "append" = c(old, value),
                  "prepend" = c(value, old),
                  "replace" = value)
    if (length(new))
        assign(nm, new, envir = .userHooksEnv, inherits = FALSE)
    else if(exists(nm, envir = .userHooksEnv, inherits = FALSE))
        remove(list=nm, envir = .userHooksEnv, inherits = FALSE)
    invisible()
}

getUserDetachHook <- function(pkgname)
{
    pkgname <- strsplit(pkgname, "_", fixed=TRUE)[[1]][1]
    nm <- paste(pkgname, "detach", sep = "::")
    if (exists(nm, envir = .userHooksEnv, inherits = FALSE))
        get(nm, envir = .userHooksEnv, inherits = FALSE)
    else list()
}

setUserDetachHook <- function(pkgname, value,
                              action = c("prepend", "append", "replace"))
{
    action <- match.arg(action)
    pkgname <- strsplit(pkgname, "_", fixed=TRUE)[[1]][1]
    nm <- paste(pkgname, "detach", sep = "::")
    old <- getUserDetachHook(pkgname)
    new <- switch(action,
                  "append" = c(old, value),
                  "prepend" = c(value, old),
                  "replace" = value)
    if (length(new))
        assign(nm, new, envir = .userHooksEnv, inherits = FALSE)
    else if(exists(nm, envir = .userHooksEnv, inherits = FALSE))
        remove(list=nm, envir = .userHooksEnv, inherits = FALSE)
    invisible()
}

getUserOnUnloadHook <- function(pkgname)
{
    nm <- paste(pkgname, "onUnload", sep = "::")
    if (exists(nm, envir = .userHooksEnv, inherits = FALSE))
        get(nm, envir = .userHooksEnv, inherits = FALSE)
    else list()
}

setUserOnUnloadHook <- function(pkgname, value,
                               action = c("prepend", "append", "replace"))
{
    action <- match.arg(action)
    nm <- paste(pkgname, "onUnload", sep = "::")
    old <- getUserOnUnloadHook(pkgname)
    new <- switch(action,
                  "append" = c(old, value),
                  "prepend" = c(value, old),
                  "replace" = value)
    if (length(new))
        assign(nm, new, envir = .userHooksEnv, inherits = FALSE)
    else if(exists(nm, envir = .userHooksEnv, inherits = FALSE))
        remove(list=nm, envir = .userHooksEnv, inherits = FALSE)
    invisible()
}
