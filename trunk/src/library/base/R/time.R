system.time <- function(expr, gcFirst = TRUE)
{
    if(!exists("proc.time")) return(rep(NA_real_, 5))
    loc.frame <- parent.frame()
    if(gcFirst)  gc(FALSE)
    expr <- substitute(expr)
    time <- proc.time()
    ## need on.exit after 'time' has been set:
    ## on some systems proc.time throws an error.
    on.exit(cat("Timing stopped at:", proc.time() - time, "\n"))
    eval(expr, envir = loc.frame)
    new.time <- proc.time()
    on.exit()
    structure(new.time - time, class="proc_time")
}
unix.time <- system.time

date <- function() .Internal(date())

print.proc_time <- function(x, ...)
{
    y <- x
    if(!is.na(y[4])) y[1] <- y[1] + y[4]
    if(!is.na(y[5])) y[2] <- y[2] + y[5]
    y <- y[1:3]
    names(y) <- c(gettext("user"), gettext("system"), gettext("elapsed"))
    print(y, ...)
    invisible(x)
}
