Platform <- function()
.Internal(Platform())

R.home <- function()
.Internal(R.home())

file.show <-
function (..., header=rep("", nfiles), title="R Information",
          delete.file=FALSE, pager=getOption("pager"))
{
    file <- c(...)
    nfiles <- length(file)
    .Internal(file.show(file, header, title, delete.file, pager))
}

file.append <- function(file1, file2)
.Internal(file.append(file1, file2))

file.remove <- function(...)
.Internal(file.remove(c(...)))

list.files <- function(path=".", pattern=NULL,all.files=FALSE,full.names=FALSE)
.Internal(list.files(path, pattern, all.files, full.names))

dir <- .Alias(list.files)

file.path <- function(..., fsep=.Platform$file.sep)
paste(..., sep=fsep)

file.exists <- function(...)
.Internal(file.exists(c(...)))

file.create <- function(...)
.Internal(file.create(c(...)))

file.choose <- function(new=FALSE)
.Internal(file.choose(new))

file.copy <- function(from, to, overwrite=FALSE)
{
    if (!(nf <- length(from))) stop("no files to copy from")
    if (!(nt <- length(to)))   stop("no files to copy to")
    if (nf > nt)               stop("more `from' files than `to' files")
    if(!overwrite) {
        if(nt > nf) from <- rep(from, length = nt)
        exists <- file.exists(from)
        from <- from[exists]
        to <- to[exists]
    }
    file.create(to)
    file.append(to, from)
}

file.info <- function(...)
{
    res <- .Internal(file.info(fn <- c(...)))
    res$mtime <- res$mtime/86400
    res$ctime <- res$ctime/86400
    res$atime <- res$atime/86400
    class(res) <- "data.frame"
    row.names(res) <- fn
    res
}

file.access <- function(names, modes = 0)
{
    res <- .Internal(file.access(names, modes))
    names(res) <- names
    res
}

format.octmode <- as.character.octmode <- function(x)
{
    y <- x
    ans <- character(length(y))
    while(any(y > 0)) {
        z <- y%%8
        y <- floor(y/8)
        ans <- paste(z, ans, sep="")
    }
    ans
}

print.octmode <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}


system.file <- function (..., pkg = .packages(), lib = .lib.loc)
{
    flist <- list(...)
    if(length(flist) > 1 || (length(flist) == 1 && nchar(flist[[1]]) > 0)) {
        FILES <- file.path(t(outer(lib, pkg, paste, sep = .Platform$file.sep)),
                           file.path(...))
    } else {
        if(missing(pkg)) pkg <- "base"
        FILES <- outer(lib, pkg, paste, sep = .Platform$file.sep)
    }
    present <- file.exists(FILES)
    if (any(present)) FILES[present]
    else ""
}

getwd <- function()
    .Internal(getwd())
setwd <- function(dir)
    .Internal(setwd(dir))
basename <- function(path)
    .Internal(basename(path))
dirname <- function(path)
    .Internal(dirname(path))

Sys.info <- function()
    .Internal(Sys.info())

