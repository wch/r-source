sapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN)
    answer <- lapply(X, FUN, ...)
    if(USE.NAMES && is.character(X) && is.null(names(answer)))
                names(answer) <- X
    if(simplify && length(answer) &&
       length(common.len <- unique(unlist(lapply(answer, length)))) == 1) {
	if(common.len == 1)
	    unlist(answer, recursive = FALSE)
	else if(common.len > 1)
	    array(unlist(answer, recursive = FALSE),
		  dim= c(common.len, length(X)),
		  dimnames= if(!(is.null(n1 <- names(answer[[1]])) &
			         is.null(n2 <- names(answer)))) list(n1,n2))
	else answer
    } else answer
}

