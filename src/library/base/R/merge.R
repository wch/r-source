# Patched merge.data.frame to preserve column names when merging empty data frames

merge.data.frame <- function(x, y, by = intersect(names(x), names(y)),
                             by.x = by, by.y = by,
                             all = FALSE, all.x = all, all.y = all,
                             sort = TRUE, suffixes = c(".x",".y"),
                             no.dups = TRUE, incomparables = NULL, ...) {
  
  fix.by <- function(by, df) {
    if(is.null(by)) by <- numeric()
    by <- as.vector(by)
    nc <- ncol(df)
    if(is.character(by)) {
      poss <- c("row.names", names(df))
      if(any(bad <- !charmatch(by, poss, 0L)))
        stop(ngettext(sum(bad),
                      "'by' must specify a uniquely valid column",
                      "'by' must specify uniquely valid columns"), domain = NA)
      by <- match(by, poss) - 1L
    } else if(is.numeric(by)) {
      if(any(by < 0L) || any(by > nc)) stop("'by' must match numbers of columns")
    } else if(is.logical(by)) {
      if(length(by) != nc) stop("'by' must match number of columns")
      by <- seq_along(by)[by]
    } else stop("'by' must specify one or more columns as numbers, names or logical")
    if(any(bad <- is.na(by)))
      stop(ngettext(sum(bad),
                    "'by' must specify a uniquely valid column",
                    "'by' must specify uniquely valid columns"), domain = NA)
    unique(by)
  }
  
  nx <- nrow(x <- as.data.frame(x)); ny <- nrow(y <- as.data.frame(y))
  if (nx >= 2^31 || ny >= 2^31) stop("long vectors are not supported")
  by.x <- fix.by(by.x, x)
  by.y <- fix.by(by.y, y)
  if((l.b <- length(by.x)) != length(by.y)) stop("'by.x' and 'by.y' specify different numbers of columns")
  
  if(l.b == 0L) {
    # CARTESIAN PRODUCT
    nm <- nm.x <- names(x)
    nm.y <- names(y)
    has.common.nms <- any(cnm <- nm.x %in% nm.y)
    if(has.common.nms) {
      names(x)[cnm] <- paste0(nm.x[cnm], suffixes[1L])
      cnm <- nm.y %in% nm
      names(y)[cnm] <- paste0(nm.y[cnm], suffixes[2L])
    }
    
    # Preserve column names even for empty data frames
    if(nx == 0L || ny == 0L) {
      res <- cbind(
        x[0L, , drop = FALSE],
        y[0L, , drop = FALSE]
      )
    } else {
      ij <- expand.grid(seq_len(nx), seq_len(ny))
      res <- cbind(x[ij[, 1L], , drop = FALSE],
                   y[ij[, 2L], , drop = FALSE])
    }
    
  } else {
    # Non-empty merges, fallback to base merge logic
    res <- base::merge(x, y, by = by, by.x = by.x, by.y = by.y,
                       all = all, all.x = all.x, all.y = all.y,
                       sort = sort, suffixes = suffixes,
                       no.dups = no.dups, incomparables = incomparables, ...)
  }
  
  attr(res, "row.names") <- .set_row_names(nrow(res))
  res
}

