#Platform <- function()
#.Internal(Platform())

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

list.files <- function(path=".", pattern=NULL, all.files=FALSE,
                       full.names=FALSE, recursive=FALSE)
.Internal(list.files(path, pattern, all.files, full.names, recursive))

dir <- list.files

file.path <-
function(..., fsep=.Platform$file.sep)
{
    if(any(sapply(list(...), length) == 0)) return(character())
    paste(..., sep = fsep)
}


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
        to <- file.path(to, basename(from))
    else if (nf > nt) stop("more 'from' files than 'to' files")
    if(nt > nf) from <- rep(from, length.out = nt)
    if (!overwrite) okay <- !file.exists(to)
    else okay <- rep.int(TRUE, length(to))
    if (any(from[okay] %in% to[okay]))
        stop("file can not be copied both 'from' and 'to'")
    if (any(okay)) { ## care: create could fail but append work.
    	okay[okay] <- file.create(to[okay])
    	if(any(okay)) okay[okay] <- file.append(to[okay], from[okay])
    }
    okay
}

file.symlink <- function(from, to) {
    if (!(length(from))) stop("no files to link from")
    if (!(nt <- length(to)))   stop("no files/directory to link to")
    if (nt == 1 && file.exists(to) && file.info(to)$isdir)
        to <- file.path(to, basename(from))
    .Internal(file.symlink(from, to))
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

dir.create <- function(path, showWarnings = TRUE, recursive = FALSE)
    invisible(.Internal(dir.create(path, showWarnings, recursive)))

format.octmode <- function(x, ...)
{
    if(!inherits(x, "octmode")) stop("calling wrong method")
    isna <- is.na(x)
    y <- x[!isna]
    class(y) <- NULL
    ans0 <- character(length(y))
    z <- NULL
    while(any(y > 0) || is.null(z)) {
        z <- y%%8
        y <- floor(y/8)
        ans0 <- paste(z, ans0, sep="")
    }
    ans <- rep.int(as.character(NA), length(x))
    ans[!isna] <- ans0
    ans
}
as.character.octmode <- format.octmode

print.octmode <- function(x, ...)
{
    print(format(x), ...)
    invisible(x)
}

"[.octmode" <- function (x, i)
{
    cl <- oldClass(x)
    y <- NextMethod("[")
    oldClass(y) <- cl
    y
}

system.file <-
function(..., package = "base", lib.loc = NULL)
{
    if(nargs() == 0)
        return(file.path(.Library, "base"))
    if(length(package) != 1)
        stop("'package' must be of length 1")
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
