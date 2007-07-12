simpleMessage <-
function(message, call = NULL)
    structure(list(message = message, call = call),
              class=c("condition", "message", "simpleMessage"))

suppressMessages <-
function(expr)
    withCallingHandlers(expr,
                        message = function(c)
                        invokeRestart("muffleMessage"))

message <-
function(..., domain = NULL, appendLF = TRUE)
{
    args <- list(...)
    cond <- if (length(args) == 1 && inherits(args[[1]], "condition")) {
        if(nargs() > 1)
            warning("additional arguments ignored in message()")
        args[[1]]
    } else {
        msg <- .makeMessage(..., domain=domain, appendLF = appendLF)
        call <- sys.call()
        simpleMessage(msg, call)
    }
    defaultHandler <- function(c) {
        ## Maybe use special connection here?
        cat(conditionMessage(c), file=stderr(), sep="")
    }
    withRestarts({
        signalCondition(cond)
        ## We don't get to the default handler if the signal
        ## is handled with a non-local exit, e.g. by
        ## invoking the muffleMessage restart.
        defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
}

## also used by warning() and stop()
.makeMessage <- function(..., domain = NULL, appendLF = FALSE)
 {
    args <- list(...)
    if(length(args) > 0) {
        args <- lapply(list(...), as.character)
        if(is.null(domain) || !is.na(domain))
            args <- .Internal(gettext(domain, unlist(args)))
        msg <- paste(args, collapse = "")
    }
    else msg <- ""
    if(appendLF) paste(msg, "\n", sep = "") else msg
}

.packageStartupMessage <- function (message, call = NULL)
    structure(list(message = message, call = call),
              class = c("packageStartupMessage", "condition", "message",
              "simpleMessage"))

suppressPackageStartupMessages <- function (expr)
    withCallingHandlers(expr, packageStartupMessage=function(c)
                        invokeRestart("muffleMessage"))

packageStartupMessage <- function(..., domain = NULL, appendLF = TRUE)
{
    call <- sys.call()
    msg <- .makeMessage(..., domain=domain, appendLF = appendLF)
    message(.packageStartupMessage(msg, call))
}
