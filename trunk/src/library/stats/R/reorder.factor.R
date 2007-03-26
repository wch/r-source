reorder.factor <- function(x, X, FUN = mean, ..., order = is.ordered(x))
{
    scores <- tapply(X, x, FUN, ...)
    ans <- (if (order) ordered else factor)(x, levels = names(sort(scores)))
    attr(ans, "scores") <- scores
    ans
}
