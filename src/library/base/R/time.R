system.time <- function(expr, gcFirst = TRUE)
{
    if(!exists("proc.time")) return(rep(as.numeric(NA), 5))
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
    ## this cannot happen
    ## if(length(new.time) == 3)	new.time <- c(new.time, 0, 0)
    ## if(length(time) == 3)	time	 <- c(	  time, 0, 0)
    new.time - time
}
unix.time <- system.time

date <- function() .Internal(date())
