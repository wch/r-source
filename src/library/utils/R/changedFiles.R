#  File src/library/utils/R/changedFiles.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2013 The R Core Team
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

fileSnapshot <- function(path = ".", file.info = TRUE, timestamp = NULL, 
			 md5sum = FALSE, digest = NULL, 
			 full.names = length(path) > 1, ...) {
			 
    if (length(path) > 1 && !full.names)
	stop("'full.names' must be TRUE for multiple paths.")

    if (length(timestamp) == 1)
	file.create(timestamp)

    path <- normalizePath(path)
    args <- list(...)
    
    fullnames <- names <- character(0)
    for (i in seq_along(path)) {
	newnames <- do.call(list.files, c(path = path[i], full.names = full.names, args))
	names <- c(names, newnames)
	if (full.names) fullnames <- names
	else fullnames <- c(fullnames, file.path(path[i], newnames))
    }
    
    if (file.info) {
	info <- file.info(fullnames)
	if (!full.names)
	    rownames(info) <- names
    } else
	info <- data.frame(row.names = names)
	
    if (md5sum)
	info <- data.frame(info, md5sum = suppressWarnings(tools::md5sum(fullnames)), 
			   stringsAsFactors = FALSE)
	
    if (!is.null(digest))
	info <- data.frame(info, digest = digest(fullnames), stringsAsFactors = FALSE)
   
    structure(list(info = info, path = path, timestamp = timestamp, 
         file.info = file.info, md5sum = md5sum, digest = digest, 
         full.names = full.names, args = args), class = "fileSnapshot")
}
    
changedFiles <- function(before, after, path = before$path, timestamp = before$timestamp, 
                         check.file.info = c("size", "isdir", "mode", "mtime"), 
			 md5sum = before$md5sum, digest = before$digest, 
			 full.names = before$full.names, ...) {
			 
    stopifnot(inherits(before, "fileSnapshot"))

    if (missing(after)) {
        get.file.info <- length(check.file.info) > 0 && before$file.info
	
	args <- before$args
	newargs <- list(...)
	args[names(newargs)] <- newargs
	
	after <- do.call(fileSnapshot, c(list(path = path, timestamp = NULL, 
	                 file.info = get.file.info, md5sum = md5sum, 
	                 digest = digest, full.names = full.names), args))
    }
    stopifnot(inherits(after, "fileSnapshot"))
	      
    preinfo <- before$info
    postinfo <- after$info
    prenames <- rownames(preinfo)
    postnames <- rownames(postinfo)
    
    added <- setdiff(postnames, prenames)
    deleted <- setdiff(prenames, postnames)
    common <- intersect(prenames, postnames)
    
    if (!before$file.info || !after$file.info) 
	check.file.info <- NULL
    
    if (length(check.file.info)) {
        pre <- preinfo[common, check.file.info, drop = FALSE]
        post <- postinfo[common, check.file.info, drop = FALSE]
        changes <- pre != post
    }
    else changes <- matrix(logical(0), nrow = length(common), ncol = 0, 
                           dimnames = list(common, character(0)))
			   
    if (length(timestamp))
	if (file.exists(timestamp)) {
	    fullnames <- if (after$full.names) common else file.path(after$path, common)
	    changes <- cbind(changes, Newer = file_test("-nt", fullnames, timestamp))
	} else
	    warning("Timestamp file no longer exists.")
	
    if (md5sum) {
        pre <- preinfo[common, "md5sum"]
        post <- postinfo[common, "md5sum"]
	changes <- cbind(changes, md5sum = pre != post)
    }
    
    if (!is.null(digest)) {
        pre <- preinfo[common, "digest"]
        post <- postinfo[common, "digest"]
	changes <- cbind(changes, digest = pre != post)
    }
    changed <- rownames(changes)[rowSums(changes, na.rm = TRUE) > 0]
    structure(list(added = added, deleted = deleted, changed = changed, 
        unchanged = setdiff(common, changed), changes = changes), 
	class = "changedFiles")
}
  
print.fileSnapshot <- function(x, verbose = FALSE, ...) {
    cat("File snapshot:\n path = ", x$path, 
        "\n timestamp = ", x$timestamp, 
	"\n file.info = ", x$file.info, 
	"\n md5sum = ", x$md5sum, 
	"\n digest = ", deparse(x$digest, control = NULL),
	"\n full.names = ", x$full.names,
	"\n args = ", deparse(x$args, control = NULL), 
	"\n ", nrow(x$info), " files recorded.\n", sep="")
    if (verbose) {
	if (ncol(x$info)) print(x$info)
	else cat("Files:", rownames(x$info), sep="\n ")
    }
    invisible(x)
}

print.changedFiles <- function(x, verbose = FALSE, ...) {
    if (length(x$added)) 
    	cat("Files added:\n",  paste0("  ", x$added, collapse="\n"), "\n", sep="")
    if (length(x$deleted)) 
    	cat("Files deleted:\n",  paste0("  ", x$deleted, collapse="\n"), "\n", sep="")
    changes <- x$changes
    if (!verbose) {
	changes <- changes[rowSums(changes, na.rm = TRUE) > 0, , drop=FALSE]
	changes <- changes[, colSums(changes, na.rm = TRUE) > 0, drop=FALSE]
    }	
    if (verbose || nrow(changes)) {
        cat("File changes:\n")
        print(changes)
    }
    invisible(x)
}
