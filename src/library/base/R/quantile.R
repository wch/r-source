quantile <- function(x, ...) UseMethod("quantile")

quantile.default <-
function (x, probs = seq(0, 1, 0.25), na.rm = FALSE) {
  if (na.rm)
    x <- x[!is.na(x)]
  else if (any(is.na(x)))
    stop("Missing values and NaN's not allowed if `na.rm' is FALSE")
  n <- length(x)
  if (any(probs < 0 | probs > 1))
    stop("probs outside [0,1]")
  if (n > 0) {
    index <- 1 + (n - 1) * probs
    lo <- floor(index)
    hi <- ceiling(index)
    x <- sort(x, partial = unique(c(lo, hi)))
    i <- (index > lo)
    qs <- x[lo]
    qs[i] <- qs[i] + (x[hi[i]] - x[lo[i]]) * (index[i] - lo[i])
  } else {
    qs <- rep(as.numeric(NA), length(probs))
  }
  names(qs) <- paste(formatC(100 * probs, format = "fg", wid = 1,
                             dig = max(2,.Options$digits)),
                     "%", sep = "")
  qs
}

IQR <- function (x, na.rm = FALSE)
as.vector(diff(quantile(as.numeric(x), c(0.25, 0.75), na.rm=na.rm)))

