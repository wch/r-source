# a srcfile is a file with a timestamp

srcfile <- function(filename, encoding = getOption("encoding")) {
    stopifnot(is.character(filename), length(filename) == 1)

    e <- new.env(parent=emptyenv())

    e$wd <- getwd()
    e$filename <- filename
    
    # If filename is a URL, this will return NA
    e$timestamp <- file.info(filename)[1,"mtime"]
    e$encoding <- encoding

    class(e) <- "srcfile"
    return(e)
}

print.srcfile <- function(x, ...) {
    cat(x$filename, "\n")
    invisible(x)
}

open.srcfile <- function(con, line, ...) {

    srcfile <- con

    oldline <- srcfile$line
    if (!is.null(oldline) && oldline > line) close(srcfile)

    conn <- srcfile$conn
    if (is.null(conn)) {
	olddir <- setwd(srcfile$wd)
	on.exit(setwd(olddir))
	timestamp <- file.info(srcfile$filename)[1,"mtime"]
	if (!is.na(srcfile$timestamp) && ( is.na(timestamp) || timestamp != srcfile$timestamp) )
	    warning("Timestamp of '",srcfile$filename,"' has changed", call.=FALSE)
	srcfile$conn <- conn <- file(srcfile$filename, open="rt", encoding=srcfile$encoding)
	srcfile$line <- 1
	oldline <- 1
    } else if (!isOpen(conn)) {
	open(conn, open="rt")
	srcfile$line <- 1
	oldline <- 1
    }
    if (oldline < line) {
	readLines(conn, line-oldline, warn=FALSE)
	srcfile$line <- line
    }
    invisible(conn)
}

close.srcfile <- function(con, ...) {
    srcfile <- con
    conn <- srcfile$conn
    if (is.null(conn)) return()
    else {
	close(conn)
	rm(list=c("conn", "line"), envir=srcfile)
    }
}

# srcfilecopy saves a copy of lines from a file

srcfilecopy <- function(filename, lines) {
    stopifnot(is.character(filename), length(filename) == 1)

    e <- new.env(parent=emptyenv())

    e$filename <- filename
    e$lines <- as.character(lines)

    class(e) <- c("srcfilecopy", "srcfile")
    return(e)
}

open.srcfilecopy <- function(con, line, ...) {

    srcfile <- con

    oldline <- srcfile$line
    if (!is.null(oldline) && oldline > line) close(srcfile)

    conn <- srcfile$conn
    if (is.null(conn)) {
	srcfile$conn <- conn <- textConnection(srcfile$lines, open="r")
	srcfile$line <- 1
	oldline <- 1
    } else if (!isOpen(conn)) {
	open(conn, open="r")
	srcfile$line <- 1
	oldline <- 1
    }
    if (oldline < line) {
	readLines(conn, line-oldline, warn=FALSE)
	srcfile$line <- line
    }
    invisible(conn)
}

.isOpen <- function(srcfile) {
    conn <- srcfile$conn
    return( !is.null(conn) && isOpen(conn) )
}

getSrcLines <- function(srcfile, first, last) {
    if (first > last) return(character(0))
    if (!.isOpen(srcfile)) on.exit(close(srcfile))
    conn <- open(srcfile, first)
    lines <- readLines(conn, n=last-first+1, warn=FALSE)
    srcfile$line <- first + length(lines)
    return(lines)
}

# a srcref gives start and stop positions of text
# lloc entries are first_line, first_column, last_line, last_column
# all are inclusive

srcref <- function(srcfile, lloc) {
    stopifnot(inherits(srcfile, "srcfile"), length(lloc) == 4)
    structure(as.integer(lloc), srcfile=srcfile, class="srcref")
}

as.character.srcref <- function(x, useSource = TRUE, ...)
{
    srcfile <- attr(x, "srcfile")
    if (useSource) lines <- try(getSrcLines(srcfile, x[1], x[3]), TRUE)
    if (!useSource || inherits(lines, "try-error"))
    	lines <- paste("<srcref: file \"", srcfile$filename, "\" chars ",
                       x[1],":",x[2], " to ",x[3],":",x[4], ">", sep="")
    else {
        if (length(lines) < x[3] - x[1] + 1) 
            x[4] <- .Machine$integer.max
    	lines[length(lines)] <- substring(lines[length(lines)], 1, x[4])
    	lines[1] <- substring(lines[1], x[2])
    }
    lines
}

print.srcref <- function(x, useSource = TRUE, ...)
    cat(as.character(x, useSource = useSource), sep="\n")
