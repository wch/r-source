Platform <- function()
.Internal(Platform())

R.home <- function()
.Internal(R.home())

file.show <-
function (..., header=rep("", nfiles), title="R Information",
          delete.file=FALSE, pager=options()$pager)
{
    file <- c(...)
    nfiles <- length(file)
    .Internal(file.show(file, header, title, delete.file, pager))
}

file.append <- function(file1, file2)
.Internal(file.append(file1, file2))

file.remove <- function(...)
.Internal(file.remove(c(...)))


list.files <- function(path, pattern=NULL, all.files=FALSE, full.names=FALSE)
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
