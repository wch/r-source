Platform <- function()
.Internal(Platform())

R.home <- function()
.Internal(R.home())

file.show <-
function (file, header=rep("", length(file)), title = "R Information") 
.Internal(file.show(file, header, title))

file.append <- function(file1, file2)
.Internal(file.append(file1, file2))

file.remove <- function(file)
.Internal(file.remove(file))

list.files <- function(path, pattern=NULL, all.files=FALSE, full.names=FALSE)
.Internal(list.files(path, pattern, all.files, full.names))

file.path <- function(...)
paste(..., sep=.Platform$file.sep)

file.exists <- function(file)
.Internal(file.exists(file))

file.create <- function(file)
.Internal(file.create(file))

file.choose <- function(new=FALSE)
.Internal(file.choose(new))


system.file <- function (..., pkg = .packages(), lib = .lib.loc) 
{
    flist <- list(...)
    if(length(flist) > 1 || (length(flist) == 1 && nchar(flist[[1]]) > 0)) {
        FILES <- paste(t(outer(lib, pkg, paste, sep = .Platform$file.sep)), 
            paste(..., sep = .Platform$file.sep), sep = .Platform$file.sep)
    } else {
        FILES <- paste(lib, pkg, sep = .Platform$file.sep)
    }
    present <- file.exists(FILES)
    if (any(present)) FILES[present]
    else ""
}
