read.dcf <- function(file, fields = NULL)
{
    if(is.character(file)){
        file <- gzfile(file, "r")
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    .Internal(readDCF(file, fields))
}

write.dcf <-
function(x, file = "", append = FALSE,
         indent = 0.1 * getOption("width"),
         width = 0.9 * getOption("width"))
{
    if(!is.data.frame(x))
        x <- data.frame(x)
    x <- as.matrix(x)
    mode(x) <- "character"

    if(file == "")
        file <- stdout()
    else if(is.character(file)) {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    }
    if(!inherits(file, "connection"))
        stop("'file' must be a character string or connection")

    nr <- nrow(x)
    nc <- ncol(x)

    eor <- character(nr * nc)
    eor[seq(1, nr - 1) * nc] <- "\n"    # newline for end of record

    writeLines(paste(formatDL(rep.int(colnames(x), nr), c(t(x)), style =
                     "list", width = width, indent = indent),
                     eor, sep = ""),
               file)
}
