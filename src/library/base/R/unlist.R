unlist <- function(x, recursive=TRUE, use.names=TRUE)
{
    if(length(x) > 0 && is.factor(x[[1]]) && all(lapply(x, is.factor))) {
        ## so will not be recursing
        lv <- unique(.Internal(unlist(lapply(x, levels), FALSE, FALSE)))
        nm <- if(use.names) names(.Internal(unlist(x, FALSE, use.names)))
        res <- .Internal(unlist(lapply(x, as.character), FALSE, FALSE))
        res <- match(res, lv)
        ## we cannot make this ordered as level set may have been changed
        structure(res, levels=lv, names=nm, class="factor")
    } else .Internal(unlist(x, recursive, use.names))
}
