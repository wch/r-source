persp <-
function(x, y, z, theta=0, phi=15, r=sqrt(3), d=1, ...)
{
	s <- max(diff(range(z)), diff(range(x)), diff(range(y)))/2
	h <- d / (d + r - 1)
	.Internal(persp(
		(z - mean(range(z))) / s,
		(x - mean(range(x))) / s,
		(y - mean(range(y))) / s,
		c(theta, phi, r, d), ...))
}
