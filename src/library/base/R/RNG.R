## Random Number Generator[s]

## The available kinds are in
## ../../../include/Random.h  and ../../../nmath/sunif.c [RNG_Table]
RNGkind <- function(kind = NULL) 
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               ## NOT yet: "Mersenne-Twister",
               ##BUG "Rand"
               )
    do.set <- length(kind) > 0
    if(do.set) {
	if(!is.character(kind) || length(kind) > 1)
	    stop("'kind' must be a character of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1))
	    stop(paste("'",kind,"' is not a valid abbrevation of an RNG",
		       sep=""))
    } else i.knd <- NULL
    
    r <- kinds[1 + .Internal(RNGkind(i.knd))]
    if(do.set) invisible(r) else r
}

