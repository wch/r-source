cov.wt <- function(x, wt = rep(1/nrow(x), nrow(x)), cor = FALSE,
                   center = TRUE)
{
  if (!is.matrix(x))
    stop("x must be a matrix")
  if (!all(is.finite(x)))
    stop("x must contain finite values only")
  n <- nrow(x)
  if (with.wt <- !missing(wt)) {
    if (length(wt) != n)
      stop("length of wt must equal the number of rows in x")
    if (any(wt < 0) || (s <- sum(wt)) == 0)
      stop("weights must be non-negative and not all zero")
    wt <- wt / s
  }
  if (is.logical(center)) {
    center <- if (center)
      apply(wt * x, 2, sum)
    else 0
  } else {
    if (length(center) != ncol(x))
      stop("length of center must equal the number of columns in x")
  }
  x <- sqrt(wt) * sweep(x, 2, center)
  cov <- (t(x) %*% x) / (1 - sum(wt^2))
  y <- list(cov = cov, center = center, n.obs = n)
  if (with.wt) 
    y$wt <- wt
  if (cor) {
    sdinv <- diag(1 / sqrt(diag(cov)))
    y$cor <- sdinv %*% cov %*% sdinv
  }
  y
}
