write.table <-
function(x, file = "", append = FALSE, quote = TRUE, sep = " ", eol = "\n",
	 na = NA, row.names = TRUE, col.names = TRUE)
{
  if (!is.data.frame(x))
    x <- data.frame(x)
  else if (is.logical(quote) && quote)
    quote <- which(unlist(lapply(x, is.character)))
  x <- as.matrix(x)
  p <- ncol(x)
  d <- dimnames(x)
  x[is.na(x)] <- na
  if (is.logical(quote))
    quote <- if (quote) 1 : p else NULL
  else if (is.numeric(quote)) {
    if (any(quote < 1 | quote > p))
      stop("invalid numbers in quote")
  }
  else
    stop("invalid quote specification")
      
  if (is.logical(row.names)) {
    if (row.names)
      x <- cbind(d[[1]], x)
  }
  else {
    row.names <- as.character(row.names)
    if (length(row.names) == nrow(x))
      x <- cbind(row.names, x)
    else
      stop("invalid row.names specification")
  }
  if (!is.null(quote) && (p < ncol(x)))
    quote <- c(0, quote) + 1

  if (is.logical(col.names))
    col.names <- if (col.names) d[[2]] else NULL
  else {
    col.names <- as.character(col.names)
    if (length(col.names) != p)
      stop("invalid col.names specification")
  }
  if (!is.null(col.names)) {
    if (append)
      warning("appending column names to file")
    if (!is.null(quote))
      col.names <- paste("\"", col.names, "\"", sep = "")
    cat(col.names, file = file, sep = rep(sep, p - 1), append = append)
    cat(eol, file = file, append = TRUE)
    append <- TRUE
  }

  for (i in quote)
    x[, i] <- paste("\"", x[, i], "\"", sep = "")

  cat(t(x), file = file, sep = c(rep(sep, ncol(x) - 1), eol),
      append = append)
}
