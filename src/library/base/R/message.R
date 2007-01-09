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
    if (length(args) == 1 && inherits(args[[1]], "condition"))
        cond <- args[[1]]
    else {
        if(length(args) > 0) {
            args <- lapply(list(...), as.character)
            if(is.null(domain) || !is.na(domain))
                args <- .Internal(gettext(domain, unlist(args)))
            message <- paste(args, collapse = "")
        }
        else message <- ""
        if(appendLF) message <- paste(message, "\n")
        call <- sys.call()
        cond <- simpleMessage(message, call)
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
    invisible(NULL)
}
