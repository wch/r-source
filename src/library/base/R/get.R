get <-
    function(x, pos=-1, envir=pos.to.env(pos), mode="any", inherits=TRUE)
    {
	if (is.character(pos)) 
	    pos<-match(pos,search()) 
	.Internal(get(x, envir, mode, inherits))
    }
