assign <-
function(x, value, pos=-1, envir=pos.to.env(pos), inherits=FALSE,
        immediate=TRUE)
.Internal(assign(x, value, envir, inherits))
