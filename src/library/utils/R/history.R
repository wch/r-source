loadhistory <- function(file=".Rhistory")
    invisible(.Internal(loadhistory(file)))

savehistory <- function(file=".Rhistory")
    invisible(.Internal(savehistory(file)))

history <- function(max.show=25, reverse=FALSE)
{
    file1 <- tempfile("Rrawhist")
    savehistory(file1)
    rawhist <- scan(file1, what = "", quiet=TRUE, sep="\n")
    unlink(file1)
    nlines <- length(rawhist)
    inds <- max(1, nlines-max.show):nlines
    if(reverse) inds <- rev(inds)
    file2 <- tempfile("hist")
    write(rawhist[inds], file2)
    file.show(file2, title="R History", delete.file=TRUE)
}
