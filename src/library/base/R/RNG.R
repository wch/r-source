## Random Number Generator

## The available kinds are in
## ../../../include/Random.h  and ../../../main/RNG.c [RNG_Table]
##
RNGkind <- function(kind = NULL, normal.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "default")
    n.kinds <- c("Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller", "default")
    do.set <- length(kind) > 0
    if(do.set) {
	if(!is.character(kind) || length(kind) > 1)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1))
	    stop(paste("'",kind,"' is not a valid abbrevation of an RNG",
		       sep=""))
        if(i.knd == length(kinds) - 1) i.knd <- -1
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) > 1)
	    stop("'normal.kind' must be a character string of length 1.")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1
        if(is.na(normal.kind))
 	    stop(paste("'", normal.kind,"' is not a valid choice", sep=""))
        if(normal.kind == length(n.kinds) - 1) normal.kind <- -1
    }
    r <- 1 + .Internal(RNGkind(i.knd, normal.kind))
    r <- c(kinds[r[1]], n.kinds[r[2]])
    if(do.set || !is.null(normal.kind)) invisible(r) else r
}

set.seed <- function(seed, kind = NULL) {
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied", "default")
    if(length(kind) > 0) {
	if(!is.character(kind) || length(kind) > 1)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1))
	    stop(paste("'",kind,"' is not a valid abbrevation of an RNG",
		       sep=""))
        if(i.knd == length(kinds) - 1) i.knd <- -1
    } else i.knd <- NULL

    invisible(.Internal(set.seed(seed, i.knd)))
}
