withVisible <- function(x) {
    x <- substitute(x)
    v <- .Internal(eval.with.vis(x, parent.frame(), baseenv()))
    v
}
