### clean up FUN arguments to *apply, outer, sweep, etc.
### note that this grabs two levels back and is not designed
### to be called at top level
match.fun <- function (FUN, descend = TRUE)
{
    if ( is.function(FUN) )
        return(FUN)
    if (!(is.character(FUN) && length(FUN) == 1 || is.symbol(FUN))) {
        ## Substitute in parent
        FUN <- eval.parent(substitute(substitute(FUN)))
        if (!is.symbol(FUN))
            stop("not function, character, or symbol: ", sQuote(deparse(FUN)))
    }
    envir <- parent.frame(2)
    if( descend )
        FUN <- get(as.character(FUN), mode = "function", env=envir)
    else {
        FUN <- get(as.character(FUN), mode = "any", env=envir)
        if( !is.function(FUN) )
           stop("found non-function: ", sQuote(FUN))
    }
    return(FUN)
}
