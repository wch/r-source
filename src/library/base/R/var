var <- function(x, y=x, na.rm = FALSE, use) {
	if(missing(use)) {
		if(na.rm) use <- "complete.obs"
		else use <- "all.obs"
	}
	cov(x, y, use=use)
}
