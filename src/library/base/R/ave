# Average a vector over the levels of a factor.

ave  <-  function (x, ...) 
{
	l <- list(...)
	if (is.null(l)) {
		x[] <- mean(x)
	}
	else {
		g <- 1
		nlv <- 1
		for (i in 1:length(l)) {
			l[[i]] <- as.factor(l[[i]])
			g <- g + nlv * (as.numeric(l[[i]]) - 1)
			nlv <- nlv * length(levels(l[[i]]))
		}
		x[] <- lapply(split(x, g), mean)[g]
	}
	x
}
