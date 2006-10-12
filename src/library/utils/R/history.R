loadhistory <- function(file = ".Rhistory")
    invisible(.Internal(loadhistory(file)))

savehistory <- function(file = ".Rhistory")
    invisible(.Internal(savehistory(file)))

history <- function(max.show = 25, reverse = FALSE, pattern, ...)
{
    file1 <- tempfile("Rrawhist")
    savehistory(file1)
    rawhist <- readLines(file1)
    unlink(file1)
    if(!missing(pattern))
        rawhist <- unique(grep(pattern, rawhist, value = TRUE, ...))
    nlines <- length(rawhist)
    if(nlines) {
        inds <- max(1, nlines-max.show):nlines
        if(reverse) inds <- rev(inds)
    } else inds <- integer(0)
    file2 <- tempfile("hist")
    writeLines(rawhist[inds], file2)
    file.show(file2, title = "R History", delete.file = TRUE)
}

timestamp <- function(stamp = date(), prefix = "##------ ",
                      suffix = " ------##", quiet = FALSE)
{
    stamp <- paste(prefix, stamp, suffix, sep = "")
    .Internal(addhistory(stamp))
    if (!quiet) cat(stamp, sep = "\n")
    invisible(stamp)
}
