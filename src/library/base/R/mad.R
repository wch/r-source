mad <- function(x, center, constant = 1.4826, na.rm = FALSE) {
    if(na.rm)
	x <- x[!is.na(x)]
    if(missing(center))
	constant * (median(abs(x - median(x))))
    else constant * (median(abs(x - center)))
}
