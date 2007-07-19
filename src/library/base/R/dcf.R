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
    tx <- t(x)
    not.na <- c(!is.na(tx))  # don't write NA fields
    eor <- character(sum(not.na))
    if(sum(not.na))
        eor[ c(diff(c(col(tx))[not.na]),1) >= 1 ] <- "\n" # newline for end of record

    writeLines(paste(formatDL(rep.int(colnames(x), nr), c(tx), style =
                              "list", width = width, indent = indent)[not.na],
                     eor, sep = ""),
               file)
}
