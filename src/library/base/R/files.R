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
    if(nfiles == 0)
        return(invisible(NULL))
    if(is.function(pager))
	pager(file, header, title, delete.file)
    else
        .Internal(file.show(file, header, title, delete.file, pager))
}

file.append <- function(file1, file2)
.Internal(file.append(file1, file2))

file.remove <- function(...)
.Internal(file.remove(c(...)))

file.rename <- function(from, to)
.Internal(file.rename(from, to))

list.files <- function(path=".", pattern=NULL,all.files=FALSE,full.names=FALSE)
.Internal(list.files(path, pattern, all.files, full.names))

dir <- list.files

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
    if (nt == 1 && file.exists(to) && file.info(to)$isdir)
        to <- file.path(to, from)
    else if (nf > nt)  stop("more `from' files than `to' files")
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
    class(res$mtime) <- class(res$ctime) <- class(res$atime) <-
        c("POSIXt", "POSIXct")
    class(res) <- "data.frame"
    row.names(res) <- fn
    res
}

file.access <- function(names, mode = 0)
{
    res <- .Internal(file.access(names, mode))
    names(res) <- names
    res
}

format.octmode <- function(x, ...)
{
    if(!inherits(x, "octmode")) stop("calling wrong method")
    isna <- is.na(x)
    y <- x[!isna]
    ans0 <- character(length(y))
    while(any(y > 0)) {
        z <- y%%8
        y <- floor(y/8)
        ans0 <- paste(z, ans0, sep="")
    }
    ans <- rep("NA", length(x))
    ans[!isna] <- ans0
    ans
}
as.character.octmode <- format.octmode

print.octmode <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}

system.file <-
function(..., package = "base", lib.loc = NULL, pkg, lib)
{
    if(nargs() == 0)
        return(file.path(.Library, "base"))
    if(!missing(pkg)) {
        warning("argument `pkg' is deprecated.  Use `package' instead.")
        if(missing(package)) package <- pkg
    }
    if(!missing(lib)) {
        warning("argument `lib' is deprecated.  Use `lib.loc' instead.")
        if(missing(lib.loc)) lib.loc <- lib
    }
    if(length(package) != 1)
        stop("argument `package' must be of length 1")
    packagePath <- .find.package(package, lib.loc, quiet = TRUE)
    if(length(packagePath) == 0)
        return("")
    FILES <- file.path(packagePath, ...)
    present <- file.exists(FILES)
    if(any(present))
        FILES[present]
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

Sys.sleep <- function(time)
    invisible(.Internal(Sys.sleep(time)))

path.expand <- function(path)
    .Internal(path.expand(path))
