which <- function(logic, arr.ind = FALSE)
{
    if(!is.logical(logic))
	stop("argument to \"which\" is not logical")
    wh <- seq(along=logic)[ll <- logic & !is.na(logic)]
    if ((m <- length(wh)) > 0) {
	dl <- dim(logic)
	if (is.null(dl) || !arr.ind) {
	    names(wh) <- names(logic)[ll]
	}
	else { ##-- return a matrix  length(wh) x rank
	    rank <- length(dl)
	    wh1 <- wh - 1
	    wh <- 1 + wh1 %% dl[1]
	    wh <- matrix(wh, nrow = m, ncol = rank,
			 dimnames =
			 list(dimnames(logic)[[1]][wh],
			      if(rank == 2) c("row", "col")# for matrices
			      else paste("dim", 1:rank, sep="")))
	    if(rank >= 2) {
		denom <- 1
		for (i in 2:rank) {
		    denom <- denom * dl[i-1]
		    nextd1 <- wh1 %/% denom# (next dim of elements) - 1
		    wh[,i] <- 1 + nextd1 %% dl[i]
		}
	    }
	    storage.mode(wh) <- "integer"
	}
    }
    wh
}

which.min <- function(x) .Internal(which.min(x))
which.max <- function(x) .Internal(which.max(x))

