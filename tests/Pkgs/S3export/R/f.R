myList <- function(x) {
    if(!is.list(x)) x <- as.list(x)
    class(x) <- c("myList", oldClass(x))
    x
}
