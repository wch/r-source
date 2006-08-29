r2dtable <- function(n, r, c)
{
    if(length(n) == 0 || (n < 0) || is.na(n))
	stop("invalid argument 'n'")
    if((length(r) <= 1) || any(r < 0) || any(is.na(r)))
	stop("invalid argument 'r'")
    if((length(c) <= 1) || any(c < 0) || any(is.na(c)))
	stop("invalid argument 'c'")
    if(sum(r) != sum(c))
	stop("arguments 'r' and 'c' must have the same sums")
    .Call("R_r2dtable",
	  as.integer(n), as.integer(r), as.integer(c),
          PACKAGE = "base")
}
