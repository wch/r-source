rm <-
    function(..., list=character(0), pos=-1, envir=pos.to.env(pos), inherits=FALSE)
{
    names<- as.character(substitute(list(...)))[-1]
    list<-c(list, names)
    .Internal(remove(list, envir, inherits))
}

remove <- rm
