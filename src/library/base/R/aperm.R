aperm <- function(a, perm, resize=TRUE)
{
    if (missing(perm))
	perm <- integer(0) # will reverse the order
    .Internal(aperm(a, perm, resize))
}
