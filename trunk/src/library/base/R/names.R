names <- function(x) UseMethod("names")
names.default <- function(x) .Internal(names(x))

"names<-" <- function(x, value) UseMethod("names<-")
"names<-.default" <- function(x, value) .Internal("names<-"(x, value))
