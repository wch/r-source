read.fwf <- function(file, widths, sep = " ", as.is = FALSE, skip = 0,
		     row.names, col.names, ...) {
  FILE <- tempfile("R.")
  on.exit(unlink(FILE))
  system(paste("${RHOME}/cmd/fwf2table -f",
	       deparse(paste("A", widths, sep = "", collapse = " ")),
	       "-s", deparse(sep), file, ">", FILE))
  read.table(file = FILE, header = FALSE, sep = sep, as.is, skip = skip,
	     row.names = row.names, col.names = col.names)
}
