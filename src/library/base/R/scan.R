scan <-
function(file = "", what = double(0), nmax = -1, n = -1, sep = "",
         quote = if(identical(sep, "\n")) "" else "'\"",
         dec = ".", skip = 0, nlines = 0,
         na.strings = "NA", flush = FALSE, fill = FALSE,
         strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE,
         multi.line = TRUE, comment.char = "", allowEscapes = TRUE)
{
    na.strings <- as.character(na.strings)# allow it to be NULL
    if(!missing(n)) {
        if(missing(nmax))
            nmax <- n / pmax(length(what), 1)
        else
            stop("either specify 'nmax' or 'n', but not both.")
    }
    if(is.character(file))
        if(file == "") file <- stdin()
        else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    if(!inherits(file, "connection"))
        stop(sQuote("file"), " must be a character string or connection")
    .Internal(scan(file, what, nmax, sep, dec, quote, skip, nlines,
                   na.strings, flush, fill, strip.white, quiet,
                   blank.lines.skip, multi.line, comment.char,
                   allowEscapes))
}
