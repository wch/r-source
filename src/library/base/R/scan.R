scan <-
    function(file = "", what = double(0), nmax = -1, n = -1, sep = "",
	     quote = if (sep=="\n") "" else "'\"",
             dec = ".", skip = 0, nlines = 0,
	     na.strings = "NA", flush = FALSE, fill = FALSE,
             strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE)
{
    na.strings <- as.character(na.strings)# allow it to be NULL
    if(!missing(n)) {
        if(missing(nmax))
            nmax <- n / pmax(length(what), 1)
        else
            stop("Either specify `nmax' or `n', but not both.")
    }
    .Internal(scan(file, what, nmax, sep, dec, quote, skip, nlines,
                   na.strings, flush, fill, strip.white, quiet,
                   blank.lines.skip))
}

readLines <- function(file, n = -1, ok = TRUE)
    .Internal(readLines(file, n, ok))

