var <- function(x, y = NULL, na.rm = FALSE, use) {
    if(missing(use))
	use <- if(na.rm) "complete.obs" else "all.obs"
    cov(x, y, use=use)
}
