.userHooksEnv <- new.env(hash = FALSE, parent = NULL)

packageEvent <-
    function(pkgname, event=c("onLoad", "attach", "detach", "onUnload"))
{
    event <- match.arg(event)
    pkgname <- strsplit(pkgname, "_", fixed=TRUE)[[1]][1]
    paste("UserHook", pkgname, event, sep = "::")
}

getHook <- function(hookName)
{
    if (exists(hookName, envir = .userHooksEnv, inherits = FALSE))
        get(hookName, envir = .userHooksEnv, inherits = FALSE)
    else list()
}

setHook <- function(hookName, value,
                    action = c("append", "prepend", "replace"))
{
    action <- match.arg(action)
    old <- getHook(hookName)
    new <- switch(action,
                  "append" = c(old, value),
                  "prepend" = c(value, old),
                  "replace" = value)
    if (length(new))
        assign(hookName, new, envir = .userHooksEnv, inherits = FALSE)
    else if(exists(hookName, envir = .userHooksEnv, inherits = FALSE))
        remove(list=hookName, envir = .userHooksEnv, inherits = FALSE)
    invisible()
}

getUserOnLoadHook <- function(pkgname)
{
    .Deprecated("getHook(packageEvent())")
    getHook(packageEvent(pkgname, "onLoad"))
}

setUserOnLoadHook <- function(pkgname, value, action = "append")
{
    .Deprecated("setHook(packageEvent())")
    setHook(packageEvent(pkgname, "onLoad"), value, action)
}

getUserAttachHook <- function(pkgname)
{
    .Deprecated("getHook(packageEvent())")
    getHook(packageEvent(pkgname, "attach"))
}

setUserAttachHook <- function(pkgname, value, action = "append")
{
    .Deprecated("setHook(packageEvent())")
    setHook(packageEvent(pkgname, "attach"), value, action)
}

getUserDetachHook <- function(pkgname)
{
    .Deprecated("getHook(packageEvent())")
    getHook(packageEvent(pkgname, "detach"))
}

setUserDetachHook <- function(pkgname, value, action = "append")
{
    .Deprecated("setHook(packageEvent())")
    setHook(packageEvent(pkgname, "detach"), value, action)
}

getUserOnUnloadHook <- function(pkgname)
{
    .Deprecated("getHook(packageEvent())")
    getHook(packageEvent(pkgname, "onUnload"))
}

setUserOnUnloadHook <- function(pkgname, value, action = "append")
{
    .Deprecated("setHook(packageEvent())")
    setHook(packageEvent(pkgname, "onUnload"), value, action)
}
