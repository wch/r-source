ppoints <- function (n, a = ifelse(n <= 10, 3/8, 1/2))
{
    if(length(n) > 1) n <- length(n)
    if(n > 0)
	(1:n - a)/(n + 1-2*a)
    else numeric(0)
}
