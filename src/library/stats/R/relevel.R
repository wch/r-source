relevel <- function(x, ref, ...) UseMethod("relevel")

relevel.default <- function(x, ref, ...)
    stop("relevel only for factors")

relevel.ordered <- function(x, ref, ...)
    stop("relevel only for factors")

relevel.factor <- function(x, ref, ...)
{
    lev <- levels(x)
    if(is.character(ref))
        ref <- match(ref, lev)
    if(is.na(ref))
        stop("ref must be an existing level")
    nlev <- length(lev)
    if(ref < 1 || ref > nlev)
        stop("ref = ", ref, " must be in 1 :", nlev)
    factor(x, levels = lev[c(ref, seq(along=lev)[-ref])])
}
