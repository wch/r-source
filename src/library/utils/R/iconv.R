iconv <- function(x, from, to) .Internal(iconv(x, from, to))

iconvlist <- function() sort(.Internal(iconv(NULL, "", "")))
