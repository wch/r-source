read.fwf <- function(file, widths, sep = "\t", as.is = FALSE,
		     skip = 0, row.names, col.names)
{
    doone <- function(x) {
        x <- substring(x, first, last)
        x[nchar(x)==0] <- "NA"
        x
    }
    FILE <- tempfile("Rfwf.")
    on.exit(unlink(FILE))
    raw <- scan(file, what="", sep="\n", quiet=TRUE)
    st <- c(1, 1+cumsum(widths))
    first <- st[-length(st)]
    last <- cumsum(widths)
    cat(file = FILE, sapply(raw, doone),
        sep = c(rep(sep,,length(widths)-1), "\n"))
    read.table(file = FILE, header = FALSE, sep = sep, as.is = as.is,
	       skip = skip, row.names = row.names, col.names = col.names,
               quote="")
}
