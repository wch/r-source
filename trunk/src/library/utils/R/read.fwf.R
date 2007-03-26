read.fwf <-
function(file, widths, header = FALSE, sep = "\t", as.is = FALSE,
         skip = 0, row.names, col.names, n = -1, buffersize = 2000, ...)
{
    doone <- function(x) {
        x <- substring(x, first, last)
        x[nchar(x) == 0] <- NA_character_
        x
    }

    if (is.list(widths)) {
        recordlength <- length(widths)
        widths <- do.call("c",widths)
    } else recordlength <- 1

    drop<- (widths < 0)
    widths <- abs(widths)


    buffersize <- (buffersize %/% recordlength) * recordlength

    FILENAME <- tempfile("Rfwf.")
    on.exit(unlink(FILENAME))
    FILE <- file(FILENAME,"a")
    on.exit(close(FILE),add=TRUE)

    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file), add=TRUE)
    } else if (!isOpen(file)) {
        file <- open(file, "r")
        on.exit(close(file), add=TRUE)
    }

    if (skip) readLines(file, n=skip)
    if (header) {
        headerline <- readLines(file, n=1)
        cat(file=FILE, headerline, "\n")
    }

    repeat({
        if (n == 0) break
        if (n == -1)
            thisblock <- buffersize
        else
            thisblock <- min(buffersize,n)

        raw <- readLines(file, n = thisblock)
        nread <- length(raw)
        if (recordlength > 1 &&  nread %% recordlength) {
            raw<-raw[1:(nread-nread %% recordlength)]
            warning(gettextf("last record incomplete, %d lines discarded",
                             nread %% recordlength), domain = NA)
        }
        if (recordlength > 1) {
            raw <- matrix(raw, nrow=recordlength)
            raw <- apply(raw, 2, paste, collapse="")
        }

        st <- c(1, 1+cumsum(widths))
        first <- st[-length(st)][!drop]
        last <- cumsum(widths)[!drop]
        cat(file = FILE, sapply(raw, doone),
            sep = c(rep(sep, length.out = length(first)-1), "\n"))

        if (nread < thisblock) break
        if (n > 0) n <- n - length(raw)
    })

    close(FILE)
    FILE <- file(FILENAME,"r")
    read.table(file = FILE, header = header, sep = sep, as.is = as.is,
	       row.names = row.names, col.names = col.names, quote = "", ...)
}
