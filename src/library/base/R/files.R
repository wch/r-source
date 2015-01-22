#  File src/library/base/R/files.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

R.home <- function(component="home")
{
    rh <- .Internal(R.home())
    switch(component,
           "home" = rh,
           "bin" = if(.Platform$OS.type == "windows" &&
                      nzchar(p <- .Platform$r_arch)) file.path(rh, component, p)
           else file.path(rh, component),
           "share" = if(nzchar(p <- Sys.getenv("R_SHARE_DIR"))) p
           else file.path(rh, component),
	   "doc" = if(nzchar(p <- Sys.getenv("R_DOC_DIR"))) p
           else file.path(rh, component),
           "include" = if(nzchar(p <- Sys.getenv("R_INCLUDE_DIR"))) p
           else file.path(rh, component),
           "modules" = if(nzchar(p <- .Platform$r_arch)) file.path(rh, component, p)
           else file.path(rh, component),
           file.path(rh, component))
}

file.show <-
    function (..., header = rep("", nfiles), title = "R Information",
              delete.file = FALSE, pager = getOption("pager"), encoding = "")
{
    files <- path.expand(c(...))
    nfiles <- length(files)
    if(nfiles == 0L)
        return(invisible(NULL))
    ## avoid re-encoding files to the current encoding.
    if(l10n_info()[["UTF-8"]] && encoding == "UTF-8") encoding <- ""
    if(l10n_info()[["Latin-1"]] && encoding == "latin1") encoding <- ""
    if(!is.na(encoding) && encoding != "") {
        for(i in seq_along(files)) {
            f <- files[i]
            tf <- tempfile()
            tmp <- readLines(f, warn = FALSE)
            tmp2 <- try(iconv(tmp, encoding, "", "byte"))
            if(inherits(tmp2, "try-error")) file.copy(f, tf)
            else writeLines(tmp2, tf)
            files[i] <- tf
            if(delete.file) unlink(f)
        }
        delete.file <- TRUE
    }
    if(is.function(pager))
	pager(files, header = header, title = title, delete.file = delete.file)
    else
        .Internal(file.show(files, header, title, delete.file, pager))
}

file.append <- function(file1, file2)
    .Internal(file.append(file1, file2))

file.remove <- function(...)
    .Internal(file.remove(c(...)))

file.rename <- function(from, to)
    .Internal(file.rename(from, to))

list.files <-
    function(path = ".", pattern = NULL, all.files = FALSE,
             full.names = FALSE, recursive = FALSE,
             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
    .Internal(list.files(path, pattern, all.files, full.names,
			 recursive, ignore.case, include.dirs, no..))

dir <- list.files

list.dirs <- function(path = ".", full.names = TRUE, recursive = TRUE)
    .Internal(list.dirs(path, full.names, recursive))


file.path <-
function(..., fsep=.Platform$file.sep)
    .Internal(file.path(list(...), fsep))


file.exists <- function(...) .Internal(file.exists(c(...)))

file.create <- function(..., showWarnings =  TRUE)
    .Internal(file.create(c(...), showWarnings))

file.choose <- function(new=FALSE) .Internal(file.choose(new))

file.copy <- function(from, to,
                      overwrite = recursive, recursive = FALSE,
                      copy.mode = TRUE, copy.date = FALSE)
{
    if (!(nf <- length(from))) return(logical())
    if (!(nt <- length(to)))   stop("no files to copy to")
    ## we don't use file_test as that is in utils.
    if (nt == 1 && dir.exists(to)) {
        if (recursive && to %in% from)
            stop("attempt to copy a directory to itself")
        ## on Windows we need \ for the compiled code (e.g. mkdir).
        if(.Platform$OS.type == "windows") {
            from <- gsub("/", "\\", from, fixed = TRUE)
            to <- gsub("/", "\\", to, fixed = TRUE)
        }
        return(.Internal(file.copy(from, to, overwrite, recursive,
                                   copy.mode, copy.date)))
    } else if (nf > nt) stop("more 'from' files than 'to' files")
    else if (recursive)
        warning("'recursive' will be ignored as 'to' is not a single existing directory")
    if(nt > nf) from <- rep_len(from, length.out = nt)
    okay <- file.exists(from)
    if (!overwrite) okay[file.exists(to)] <- FALSE
    if (any(from[okay] %in% to[okay]))
        stop("file can not be copied both 'from' and 'to'")
    if (any(okay)) { # care: file.create could fail but file.append work.
    	okay[okay] <- file.create(to[okay])
    	if(any(okay)) {
            okay[okay] <- file.append(to[okay], from[okay])
            if(copy.mode || copy.date) { # file.info call can be slow
                fi <- file.info(from[okay], extra_cols = FALSE)
                if(copy.mode) Sys.chmod(to[okay], fi$mode, TRUE)
                if(copy.date) Sys.setFileTime(to[okay], fi$mtime)
            }
        }
    }
    okay
}

file.symlink <- function(from, to) {
    if (!(length(from))) stop("no files to link from")
    if (!(nt <- length(to)))   stop("no files/directory to link to")
    if (nt == 1 && file.exists(to) && file.info(to, extra_cols = FALSE)$isdir)
        to <- file.path(to, basename(from))
    .Internal(file.symlink(from, to))
}

file.link <- function(from, to) {
    if (!(length(from))) stop("no files to link from")
    if (!length(to))     stop("no files to link to")
    .Internal(file.link(from, to))
}

file.info <- function(..., extra_cols = TRUE)
{
    res <- .Internal(file.info(fn <- c(...), extra_cols))
    res$mtime <- .POSIXct(res$mtime)
    res$ctime <- .POSIXct(res$ctime)
    res$atime <- .POSIXct(res$atime)
    class(res) <- "data.frame"
    attr(res, "row.names") <- fn # not row.names<- as that does a length check
    res
}
## wrappers introduced in R 3.2.0
file.mode <- function(...) file.info(..., extra_cols = FALSE)$mode
file.mtime <- function(...) file.info(..., extra_cols = FALSE)$mtime
file.size <- function(...) file.info(..., extra_cols = FALSE)$size


file.access <- function(names, mode = 0)
{
    res <- .Internal(file.access(names, mode))
    names(res) <- names
    res
}

dir.exists <- function(paths) .Internal(dir.exists(paths))

dir.create <- function(path, showWarnings = TRUE, recursive = FALSE,
                       mode = "0777")
    .Internal(dir.create(path, showWarnings, recursive, as.octmode(mode)))

system.file <- function(..., package = "base", lib.loc = NULL, mustWork = FALSE)
{
    if(nargs() == 0L)
        return(file.path(.Library, "base"))
    if(length(package) != 1L)
        stop("'package' must be of length 1")
    packagePath <- find.package(package, lib.loc, quiet = TRUE)
    ans <- if(length(packagePath)) {
        FILES <- file.path(packagePath, ...)
        present <- file.exists(FILES)
        if(any(present)) FILES[present] else ""
    } else ""
    if (mustWork && identical(ans, "")) stop("no file found")
    ans
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
    .Internal(Sys.sleep(time))

path.expand <- function(path)
    .Internal(path.expand(path))

Sys.glob <- function(paths, dirmark = FALSE)
    .Internal(Sys.glob(path.expand(paths), dirmark))

unlink <- function(x, recursive = FALSE, force = FALSE)
    .Internal(unlink(as.character(x), recursive, force))

Sys.chmod <- function(paths, mode = "0777", use_umask = TRUE)
    .Internal(Sys.chmod(paths, as.octmode(mode), use_umask))

Sys.umask <- function(mode = NA)
    .Internal(Sys.umask(if(is.na(mode)) NA_integer_ else as.octmode(mode)))

Sys.readlink <- function(paths)
    .Internal(Sys.readlink(paths))

readRenviron <- function(path)
    .Internal(readRenviron(path))

normalizePath <- function(path, winslash = "\\", mustWork = NA)
    .Internal(normalizePath(path.expand(path), winslash, mustWork))

Sys.setFileTime <- function(path, time)
{
    if (!is.character(path) || length(path) != 1L)
        stop("invalid 'path' argument")
    time <- as.POSIXct(time)
    if (is.na(time))  stop("invalid 'time' argument")
    .Internal(setFileTime(path, time))
}
