rowsum <- function(x, group, reorder=TRUE) {
    if (!is.numeric(x)) stop("x must be numeric")
    if (is.matrix(x)) dd <- dim(x)
    else              dd <- c(length(x), 1)
    n <- dd[1]

    if (length(group) !=n)  stop("Incorrect length for 'group'")
    if (any(is.na(group)))  stop("Missing values for 'group'")
    na.indicator <- max(1,x[!is.na(x)]) * n   #larger than any possible sum
    x[is.na(x)] <- na.indicator

    if (!is.numeric(group)) group <- as.factor(group)
    storage.mode(x) <- 'double'
    temp <- .C("R_rowsum", dd= as.integer(dd),
			 as.double(na.indicator),
			 x = x,
			 as.double(group), PACKAGE="base")
    new.n <- temp$dd[1]
    ugroup <- unique(group)
    if (is.matrix(x)){
	new.x <- temp$x[1:new.n, , drop=FALSE]
	dimnames(new.x) <- list(ugroup, dimnames(x)[[2]])
	if (reorder) new.x <- new.x[order(ugroup), , drop=FALSE]
	}
    else {
	new.x <- temp$x[1:new.n]
	names(new.x) <- ugroup
	if (reorder) new.x <- new.x[order(ugroup)]
	}

    ifelse(new.x ==na.indicator, NA, new.x)
    }
