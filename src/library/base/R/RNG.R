## Random Number Generator[s]

## The available kinds are in
## ../../../include/Random.h  and ../../../main/RNG.c [RNG_Table]
RNGkind <- function(kind = NULL, normal.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied")
    n.kinds <- c("Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller")
    do.set <- length(kind) > 0
    if(do.set) {
	if(!is.character(kind) || length(kind) > 1)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1))
	    stop(paste("'",kind,"' is not a valid abbrevation of an RNG",
		       sep=""))
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) > 1)
	    stop("'normal.kind' must be a character string of length 1.")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1
        if(is.na(normal.kind))
 	    stop(paste("'", normal.kind,"' is not a valid choice", sep=""))
    }
    r <- 1 + .Internal(RNGkind(i.knd, normal.kind))
    r <- c(kinds[r[1]], n.kinds[r[2]])
    if(do.set || !is.null(normal.kind)) invisible(r) else r
}

set.seed <- function(seed, kind = NULL)
    invisible(.Internal(set.seed(seed, kind)))
