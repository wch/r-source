md5sum <- function(files)
    structure(.Call("Rmd5", files, PACKAGE="tools"), names=files)

.installMD5sums <- function(pkgDir, outDir = pkgDir)
{
    dot <- getwd()
    setwd(pkgDir)
    x <- md5sum(dir(pkgDir, recursive=TRUE))
    setwd(dot)
    x <- x[names(x) != "MD5"]
    cat(paste(x, names(x), sep=" *"), sep="\n",
        file=file.path(outDir, "MD5"))
}

checkMD5sums <- function(pkg, dir)
{
    dir <- .find.package(pkg, quiet=TRUE)
    if(!length(dir)) return(NA)
    md5file <- file.path(dir, "MD5")
    if(!file.exists(md5file)) return(NA)
    infile <- scan(md5file, what=list(md5="", name=""), quiet = TRUE)
    xx <- infile[[1]]
    nmxx <- names(xx) <- sub("^\\*", "", infile[[2]])
    dot <- getwd()
    setwd(dir)
    x <- md5sum(dir(dir, recursive=TRUE))
    setwd(dot)
    x <- x[names(x) != "MD5"]
    nmx <- names(x)
    res <- TRUE
    not.here <- !(nmxx %in% nmx)
    if(any(not.here)) {
        res <- FALSE
        cat("files", paste(nmxx[not.here], collapse=", "),
            "are missing\n", sep=" ")
    }
    nmxx <- nmxx[!not.here]
    diff <- xx[nmxx] != x[nmxx]
    if(any(diff)) {
        res <- FALSE
        cat("files", paste(nmxx[diff], collapse=", "),
            "have the wrong MD5 checksums\n", sep=" ")
    }
    return(res)
}
