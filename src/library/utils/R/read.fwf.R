read.fwf <-
function(file, widths, header = FALSE, sep = "\t", as.is = FALSE,
         skip = 0, row.names, col.names, n = -1, ...)
{
    doone <- function(x) {
        x <- substring(x, first, last)
        x[nchar(x)==0] <- as.character(NA)
        x
    }
    FILE <- tempfile("Rfwf.")
    on.exit(unlink(FILE))
    raw <- readLines(file, n=n)
    st <- c(1, 1+cumsum(widths))
    first <- st[-length(st)]
    last <- cumsum(widths)
    cat(file = FILE, sapply(raw, doone),
        sep = c(rep(sep, length.out = length(widths)-1), "\n"))
    read.table(file = FILE, header = header, sep = sep, as.is = as.is,
	       skip = skip, row.names = row.names, col.names =
               col.names, quote="", ...)
}
