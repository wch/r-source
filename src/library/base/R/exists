exists <-
function(x, where=-1, envir=pos.to.env(where), frame,
	mode="any", inherits=TRUE)
{
	if(!missing(frame))
		envir <- sys.frame(frame)
	.Internal(exists(x, envir, mode, inherits))
}
