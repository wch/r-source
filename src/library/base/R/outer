outer <- function(x, y, FUN="*", ...) {
        if(is.character(FUN))
                FUN <- get(FUN, mode="function", inherits=TRUE)
        nr <- length(x)
        nc <- length(y)
        matrix(
                FUN(matrix(x, nr, nc), matrix(y, nr, nc, byrow=TRUE), ...),
                nr, nc)
}
"%o%"<-outer
