.userHooksEnv <- new.env(hash = FALSE, parent = NULL)

pkgEvent <-
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
    .Deprecated("getHook(pkgEvent())")
    getHook(pkgEvent(pkgname, "onLoad"))
}

setUserOnLoadHook <- function(pkgname, value, action = "append")
{
    .Deprecated("setHook(pkgEvent())")
    setHook(pkgEvent(pkgname, "onLoad"), value, action)
}

getUserAttachHook <- function(pkgname)
{
    .Deprecated("getHook(pkgEvent())")
    getHook(pkgEvent(pkgname, "attach"))
}

setUserAttachHook <- function(pkgname, value, action = "append")
{
    .Deprecated("setHook(pkgEvent())")
    setHook(pkgEvent(pkgname, "attach"), value, action)
}

getUserDetachHook <- function(pkgname)
{
    .Deprecated("getHook(pkgEvent())")
    getHook(pkgEvent(pkgname, "detach"))
}

setUserDetachHook <- function(pkgname, value, action = "prepend")
{
    .Deprecated("setHook(pkgEvent())")
    setHook(pkgEvent(pkgname, "detach"), value, action)
}

getUserOnUnloadHook <- function(pkgname)
{
    .Deprecated("getHook(pkgEvent())")
    getHook(pkgEvent(pkgname, "onUnload"))
}

setUserOnUnloadHook <- function(pkgname, value, action = "prepend")
{
    .Deprecated("setHook(pkgEvent())")
    setHook(pkgEvent(pkgname, "onUnload"), value, action)
}
