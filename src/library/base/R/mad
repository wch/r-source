mad <- function(y, center, constant = 1.4826, na.rm = FALSE) {
	if(na.rm)
		y <- y[!is.na(y)]
	if(missing(center))
		constant * (median(abs(y - median(y))))
	else constant * (median(abs(y - center)))
}
