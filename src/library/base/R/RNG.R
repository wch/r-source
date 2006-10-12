## Random Number Generator

## The available kinds are in
## ../../../include/Random.h  and ../../../main/RNG.c [RNG_Table]
##
RNGkind <- function(kind = NULL, normal.kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",
               "Knuth-TAOCP-2002", "default")
    n.kinds <- c("Buggy Kinderman-Ramage", "Ahrens-Dieter", "Box-Muller",
                 "user-supplied", "Inversion", "Kinderman-Ramage",
		 "default")
    do.set <- length(kind) > 0
    if(do.set) {
	if(!is.character(kind) || length(kind) > 1)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1))
	    stop(gettextf("'%s' is not a valid abbreviation of an RNG", kind),
                 domain = NA)
        if(i.knd == length(kinds) - 1) i.knd <- -1
    } else i.knd <- NULL

    if(!is.null(normal.kind)) {
	if(!is.character(normal.kind) || length(normal.kind) > 1)
	    stop("'normal.kind' must be a character string of length 1.")
	if (normal.kind == "Buggy Kinderman-Ramage")
		warning("Buggy version of Kinderman-Ramage generator used.")
        normal.kind <- pmatch(normal.kind, n.kinds) - 1
        if(is.na(normal.kind))
 	    stop(gettextf("'%s' is not a valid choice", normal.kind),
                 domain = NA)
        if(normal.kind == length(n.kinds) - 1) normal.kind <- -1
    }
    r <- 1 + .Internal(RNGkind(i.knd, normal.kind))
    r <- c(kinds[r[1]], n.kinds[r[2]])
    if(do.set || !is.null(normal.kind)) invisible(r) else r
}

set.seed <- function(seed, kind = NULL)
{
    kinds <- c("Wichmann-Hill", "Marsaglia-Multicarry", "Super-Duper",
               "Mersenne-Twister", "Knuth-TAOCP", "user-supplied",
               "Knuth-TAOCP-2002", "default")
    if(length(kind) > 0) {
	if(!is.character(kind) || length(kind) > 1)
	    stop("'kind' must be a character string of length 1 (RNG to be used).")
	if(is.na(i.knd <- pmatch(kind, kinds) - 1))
	    stop(gettextf("'%s' is not a valid abbreviation of an RNG", kind),
                 domain = NA)
        if(i.knd == length(kinds) - 1) i.knd <- -1
    } else i.knd <- NULL

    invisible(.Internal(set.seed(seed, i.knd)))
}

# Compatibility function to set RNGkind as in a given R version

RNGversion <- function(vstr)
{
    vnum <- as.numeric(strsplit(vstr,".", fixed=TRUE)[[1]])
    if (length(vnum) < 2)
	stop("malformed version string")
    if (vnum[1] == 0 && vnum[2] < 99)
        RNGkind("Wichmann-Hill", "Buggy Kinderman-Ramage")
    else if (vnum[1] == 0 || vnum[1] == 1 && vnum[2] <= 6)
	RNGkind("Marsaglia-Multicarry", "Buggy Kinderman-Ramage")
    else
	RNGkind("Mersenne-Twister", "Inversion")
}
