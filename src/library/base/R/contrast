contrasts <-
function(x, contrasts=TRUE)
{
	if (!is.factor(x))
		stop("contrasts apply only to factors")
	ctr <- attr(x,"contrasts")
	if(is.null(ctr)) {
		if(is.ordered(x))
			ctr <- get(options("contrasts")[[1]][[2]])(levels(x), contrasts=contrasts)
		else
			ctr <- get(options("contrasts")[[1]][[1]])(levels(x), contrasts=contrasts)
		dimnames(ctr) <- list(levels(x), dimnames(ctr)[[2]])
	}
	else if(is.character(ctr))
		ctr <- get(ctr)(levels(x), contrasts=contrasts)
	ctr
}

"contrasts<-" <-
function(x, ctr)
{
	if(!is.factor(x))
		stop("contrasts apply only to factors")
	if(is.numeric(ctr)) {
		ctr <- as.matrix(ctr)
		nlevs <- nlevels(x)
		if(nrow(ctr) != nlevs || ncol(ctr) >= nlevs)
			stop("invalid contrast matrix extents")
		cm <- qr(cbind(1,ctr))
		if(cm$rank != ncol(ctr)+1) stop("singular contrast matrix")
		cm <- qr.qy(cm, diag(nlevs))[,2:nlevs]
		cm[,1:ncol(ctr)] <- ctr
		dimnames(cm) <- list(levels(x),NULL)
	}
	else if(is.character(ctr))
		cm <- ctr
	else if(is.null(ctr))
		cm <- NULL
	else stop("numeric contrasts or contrast name expected")
	attr(x, "contrasts") <- cm
	x
}

contr.poly <-
function(n, contrasts=TRUE)
{
	normalize <- function(x) x/sqrt(sum(x^2))
	if(is.numeric(n) && length(n) == 1)
		levs <- 1:n
	else {
		levs <- n
		n <- length(n)
	}
	if(n < 2)
		stop(paste("Contrasts not defined for", n - 1,
			"degrees of freedom"))
	contr <- matrix(0, n, n)
	x <- 1:n
	d <- x - mean(x)
	contr[,1] <- rep(1/sqrt(n),n)
	contr[,2] <- normalize(d)

	if(n > 2)
		for(i in 3:n) {
			a1 <- sum(d*contr[,i-1]*contr[,i-1])
			a2 <- sum(d*contr[,i-1]*contr[,i-2])
			contr[,i] <- normalize((d-a1)*contr[,i-1]-a2*contr[,i-2])
		}
	dimnames(contr) <- list(levs, paste("^", 0:(n-1), sep=""))
	if(contrasts) {
		contr[, -1, drop=FALSE]
	}
	else {
		contr[, 1] <- 1
		contr
	}
}

contr.helmert <-
function (n, contrasts=TRUE) 
{
	if (length(n) <= 1) {
		if(is.numeric(n) && length(n) == 1 && n > 1) levels <- 1:n
		else stop("contrasts are not defined for 0 degrees of freedom")
	}
	else levels <- n
	lenglev <- length(levels)
	if (contrasts) {
		cont <- array(-1, c(lenglev, lenglev-1), list(levels, NULL))
		cont[col(cont) <= row(cont) - 2] <- 0
		cont[col(cont) == row(cont) - 1] <- 1:(lenglev-1)
	}
	else {
		cont <- array(0, c(lenglev, lenglev), list(levels, levels))
		cont[col(cont) == row(cont)] <- 1
	}
	cont
}

contr.treatment <-
function(n, contrasts = TRUE)
{
	if(is.numeric(n) && length(n) == 1)
		levs <- 1:n
	else {
		levs <- n
		n <- length(n)
	}
	contr <- array(0, c(n, n), list(levs, levs))
	contr[seq(1, n^2, n + 1)] <- 1
	if(contrasts) { 
		if(n < 2)
			stop(paste("Contrasts not defined for", n - 1,
				"degrees of freedom"))
		contr <- contr[, -1, drop = FALSE]
	}
	contr
}

contr.sum <-
function (n, contrasts=TRUE) 
{
	if (length(n) <= 1) {
		if (is.numeric(n) && length(n) == 1 && n > 1) 
			levels <- 1:n
		else stop("Not enough degrees of freedom to define contrasts")
	}
	else levels <- n
	lenglev <- length(levels)
	if (contrasts) {
		cont <- array(0, c(lenglev, lenglev - 1), list(levels, NULL))
		cont[col(cont) == row(cont)] <- 1
		cont[lenglev, ] <- -1
	}
	else {
		cont <- array(0, c(lenglev, lenglev), list(levels, levels))
		cont[col(cont) == row(cont)] <- 1
	}
	cont
}
