assign <-
    function(x, value, pos=-1, envir=pos.to.env(pos), inherits=FALSE,
	     immediate=TRUE)
    {
	if ( is.character(pos) )
	    pos <- match(pos,search())
    	.Internal(assign(x, value, envir, inherits))
    }
