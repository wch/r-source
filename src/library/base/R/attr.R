attr <- function(x, which) {
    if (!is.character(which))
        stop("attribute name must be of mode character")
    if (length(which) != 1)
        stop("exactly one attribute name must be given")
    attributes(x)[[which]]
}
