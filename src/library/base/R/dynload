#dyn.load <- function(x)
#{
#	x <- as.character(x)
#	y <- substr(x, 1, 1)
#	if (y == "/") {
#		.Internal(dyn.load(x))
#	}
#	else {
#		.Internal(dyn.load(
#		paste(system("pwd", intern = T), x, sep = "/", collapse="")))
#	}
#}
dyn.load <- function(x)
	.Internal(dyn.load(x))
