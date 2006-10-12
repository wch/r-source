weighted.mean <- function(x, w, na.rm = FALSE) {
    if(missing(w))
      w <- rep.int(1, length(x))
    else if (length(w) != length(x))
      stop("'x' and 'w' must have the same length")
    if(is.integer(w)) w <- as.numeric(w)  # avoid overflow in sum.
    if (na.rm) {
	w <- w[i <- !is.na(x)]
	x <- x[i]
    }
    sum(x*w)/sum(w)
}
