## Random Number Generator[s]

## The available kinds are in
## ../../../include/Random.h  and ../../../nmath/sunif.c [RNG_Table]
RNGkind <- function(kind = NULL) 
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister",
               "Rand")
    do.set <- length(kind) > 0
    if(do.set) {
	if(!is.character(kind) || length(kind) > 1)
	    stop("'kind' must be a character of length 1 (RNG to be used).")
	if(is.na(kind <- pmatch(kind, kinds) - 1))
	    stop(paste("'",kind,"' is not a valid abbrevation of of an RNG",
		       sep=""))
    }
    r <- kinds[1 + .Internal(RNGkind(kind))]
    if(do.set) invisible(r) else r
}

