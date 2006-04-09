contrasts <-
    function (x, contrasts = TRUE)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if (!is.factor(x))
	stop("contrasts apply only to factors")
    if(!contrasts)
        return(structure(diag(nlevels(x)), dimnames=list(levels(x), levels(x))))
    ctr <- attr(x, "contrasts")
    if (is.null(ctr)) {
        ctrname <- getOption("contrasts")[[if (is.ordered(x)) 2 else 1]]
	ctr <- get(ctrname, mode="function", envir=parent.frame())(levels(x), contrasts = contrasts)
	dimnames(ctr) <- list(levels(x), dimnames(ctr)[[2]])
    }
    else if (is.character(ctr))
	ctr <- get(ctr, mode="function", envir=parent.frame())(levels(x), contrasts = contrasts)
    #if(ncol(ctr)==1) dimnames(ctr) <- list(dimnames(ctr)[[1]], "")
    ctr
}

"contrasts<-" <-
    function(x, how.many, value)
{
    if (is.logical(x)) x <- factor(x, levels=c(FALSE, TRUE))
    if(!is.factor(x))
	stop("contrasts apply only to factors")
    if(nlevels(x) < 2)
        stop("contrasts can be applied only to factors with 2 or more levels")
    if(is.function(value)) value <- value(nlevels(x))
    if(is.numeric(value)) {
	value <- as.matrix(value)
	nlevs <- nlevels(x)
	if(nrow(value) != nlevs)
	    stop("wrong number of contrast matrix rows")
	n1 <- if(missing(how.many)) nlevs - 1 else how.many
	nc <- ncol(value)
	rownames(value) <- levels(x)
	if(nc  < n1) {
	    cm <- qr(cbind(1,value))
	    if(cm$rank != nc+1) stop("singular contrast matrix")
	    cm <- qr.qy(cm, diag(nlevs))[,2:nlevs]
	    cm[,1:nc] <- value
	    dimnames(cm) <- list(levels(x),NULL)
	    if(!is.null(nmcol <- dimnames(value)[[2]]))
		dimnames(cm)[[2]] <- c(nmcol, rep.int("", n1-nc))
	} else cm <- value[, 1:n1, drop=FALSE]
    }
    else if(is.character(value)) cm <- value
    else if(is.null(value)) cm <- NULL
    else stop("numeric contrasts or contrast name expected")
    attr(x, "contrasts") <- cm
    x
}

contr.helmert <-
    function (n, contrasts=TRUE)
{
    if (length(n) <= 1) {
	if(is.numeric(n) && length(n) == 1 && n > 1) levels <- 1:n
	else stop("contrasts not defined for 0 degrees of freedom")
    } else levels <- n
    lenglev <- length(levels)
    if (contrasts) {
	cont <- array(-1, c(lenglev, lenglev-1), list(levels, NULL))
	cont[col(cont) <= row(cont) - 2] <- 0
	cont[col(cont) == row(cont) - 1] <- 1:(lenglev-1)
    } else {
	cont <- array(0, c(lenglev, lenglev), list(levels, levels))
	cont[col(cont) == row(cont)] <- 1
    }
    cont
}

contr.treatment <-
    function(n, base = 1, contrasts = TRUE)
{
    if(is.numeric(n) && length(n) == 1)
	levs <- 1:n
    else {
	levs <- n
	n <- length(n)
    }
    contr <- array(0, c(n, n), list(levs, levs))
    diag(contr) <- 1
    if(contrasts) {
	if(n < 2)
	    stop(gettextf("contrasts not defined for %d degrees of freedom",
                          n - 1), domain = NA)
	if (base < 1 | base > n)
	    stop("baseline group number out of range")
	contr <- contr[, -base, drop = FALSE]
    }
    contr
}

contr.sum <-
    function (n, contrasts=TRUE)
{
    if (length(n) <= 1) {
	if (is.numeric(n) && length(n) == 1 && n > 1)
	    levels <- 1:n
	else stop("not enough degrees of freedom to define contrasts")
    } else levels <- n
    lenglev <- length(levels)
    if (contrasts) {
	cont <- array(0, c(lenglev, lenglev - 1), list(levels, NULL))
	cont[col(cont) == row(cont)] <- 1
	cont[lenglev, ] <- -1
    } else {
	cont <- array(0, c(lenglev, lenglev), list(levels, levels))
	cont[col(cont) == row(cont)] <- 1
    }
    cont
}

contr.SAS <- function(n, contrasts = TRUE)
{
    contr.treatment(n,
                    base = if (is.numeric(n) && length(n) == 1) n else length(n),
                    contrasts)
}
