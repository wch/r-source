index.search <- function(topic, path, file = "AnIndex", type = "help")
    .Internal(index.search(topic, path, file, .Platform$file.sep, type))

read.00Index <-
function(file)
{
    if(is.character(file)) {
        if(file == "") file <- stdin()
        else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    }
    if(!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    
    x <- paste(readLines(file), collapse = "\n")
    x <- strsplit(gsub("\n[ \t]+", " ", x), "\n")
    x <- strsplit(unlist(x), "[ \t]")
    y <- cbind(unlist(lapply(x, "[[", 1)),
               unlist(lapply(x, function(t)
                             paste(t[-c(1, which(nchar(t) == 0))],
                                   collapse = " "))))
    colnames(y) <- c("Item", "Description")
    y
}
