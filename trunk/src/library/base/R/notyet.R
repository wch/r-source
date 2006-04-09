.NotYetImplemented <- function ()
    stop(gettextf("'%s' is not implemented yet",
                  as.character(sys.call(sys.parent())[[1]])), call. = FALSE)

.NotYetUsed <- function(arg, error = TRUE) {
    msg <- gettextf("argument '%s' is not used (yet)", arg)
    if(error) stop(msg, domain = NA, call. = FALSE)
    else warning(msg, domain = NA, call. = FALSE)
}
