system.time <- function(expr) {
    if(!exists("proc.time")) return(rep(NA, 5))
    loc.frame <- parent.frame()
    on.exit(cat("Timing stopped at:", proc.time() - time, "\n"))
    expr <- substitute(expr)
    time <- proc.time()
    eval(expr, envir = loc.frame)
    new.time <- proc.time()
    on.exit()
    if(length(new.time) == 3)	new.time <- c(new.time, 0, 0)
    if(length(time) == 3)	time	 <- c(	  time, 0, 0)
    new.time - time
}
unix.time <- .Alias(system.time)

date <- function().Internal(date())
