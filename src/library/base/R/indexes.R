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

print.libraryIQR <-
function(x, ...)
{
    sQuote <- function(s) paste("`", s, "'", sep = "")
    db <- x$results
    ## Split according to LibPath.
    out <- if(nrow(db) == 0)
        NULL
    else lapply(split(1 : nrow(db), db[, "LibPath"]),
                function(ind) db[ind, c("Package", "Title"),
                                 drop = FALSE])
    outFile <- tempfile("RlibraryIQR")
    outConn <- file(outFile, open = "w")
    first <- TRUE
    for(lib in names(out)) {
        writeLines(paste(ifelse(first, "", "\n"),
                         "Packages in library ", sQuote(lib), ":\n",
                         sep = ""),
                   outConn)
        writeLines(formatDL(out[[lib]][, "Package"],
                            out[[lib]][, "Title"]),
                   outConn)
        first <- FALSE
    }
    if(first) {
        close(outConn)
        unlink(outFile)
        writeLines("no packages found")
    }
    else {
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = "R packages available")
    }
}

print.packageIQR <-
function(x, ...)
{
    sQuote <- function(s) paste("`", s, "'", sep = "")
    db <- x$results
    ## Split according to Package.
    out <- if(nrow(db) == 0)
         NULL
    else
        lapply(split(1 : nrow(db), db[, "Package"]),
               function(ind) db[ind, c("Item", "Title"),
                                drop = FALSE])
    outFile <- tempfile("RpackageIQR")
    outConn <- file(outFile, open = "w")
    first <- TRUE
    for(pkg in names(out)) {
        writeLines(paste(ifelse(first, "", "\n"),
                         switch(x$type,
                                data = "Data sets",
                                demo = "Demos"),
                         " in package ", sQuote(pkg), ":\n",
                         sep = ""),
                   outConn)
        writeLines(formatDL(out[[pkg]][, "Item"],
                            out[[pkg]][, "Title"]),
                   outConn)
        first <- FALSE
    }
    if(first) {
        close(outConn)
        unlink(outFile)
        writeLines(paste("no", x$type, "listings found"))
    }
    else {
        if(!is.null(x$footer))
            writeLines(c("\n", x$footer), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE,
                  title = paste("R",
                                switch(x$type,
                                       data = "data sets",
                                       demo = "demos")))
    }
}
