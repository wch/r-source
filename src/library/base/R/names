names <-
function(x, ...)
UseMethod("names")

names.default <-
function(x)
.Internal(names(x))

"names<-" <-
function(x, ...)
UseMethod("names<-")

"names<-.default" <- 
function(x, n)
.Internal("names<-"(x, n))
