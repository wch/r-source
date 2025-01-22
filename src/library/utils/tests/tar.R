### tests of compressiin on un(tar)

options(warn = 1)
old <- getwd()

for(f in  c("none", "gzip", "bzip2", "xz", "zstd"))
{
    setwd(R.home('library'))
    z <- if (f=="none") "utils.tar" else paste0("utils.tar.", f)
    zz <- file.path(old, z)
    message("making ", z)
    ## zstd support is optional
    y <- try(tar(zz, "utils", f))
    if(inherits(y, "try-error")) next
    print(file.size(zz))
    setwd(old)
    print(head(untar(zz, list = TRUE, tar = "internal")))
    untar(zz, tar = "internal")
}

## Now try external untar : skip on Windows due to different handling of paths
if(.Platform$OS.type == "windows") {
    unlink("utils", recursive = TRUE)
    quit("no")
}

for(f in  c("none", "gzip", "bzip2", "xz", "zstd"))
{
    z <- if (f=="none") "utils.tar" else paste0("utils.tar.", f)
    if (!file.exists(z)) next
    message("unpacking ", z)
    y <- untar(z)
    if(inherits(y, "try-error")) next
    print(head(dir("utils"), 5))
}

## and external tar
TAR <- Sys.getenv("TAR", "tar")
for(f in  c("none", "gzip", "bzip2", "xz", "zstd"))
{
    setwd(R.home('library'))
    z <- if (f=="none") "utils.tar" else paste0("utils.tar.", f)
    zz <- file.path(old, z)
    message("making ", z)
    y <- try(tar(zz, "utils", f, tar = TAR))
    if(inherits(y, "try-error")) next
    print(file.size(zz))
    setwd(old)
}

unlink("utils", recursive = TRUE)

