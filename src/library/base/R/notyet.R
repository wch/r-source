.NotYetImplemented <- function ()
    stop(sQuote(as.character(sys.call(sys.parent())[[1]])),
         " is not implemented yet", call. = FALSE)

.NotYetUsed <- function(arg, error = TRUE) {
    msg <- paste("argument", sQuote(arg), "is not used (yet)")
    if(error) stop(msg) else warning(msg)
}
