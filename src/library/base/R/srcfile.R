# a srcfile is a file with a timestamp

srcfile <- function(filename) {
    stopifnot(is.character(filename), length(filename) == 1)  

    e <- new.env(parent=emptyenv())

    e$wd <- getwd()
    e$filename <- filename
    e$timestamp <- file.info(filename)[1,"mtime"]

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
	if (timestamp != srcfile$timestamp) warning("Timestamp of '",srcfile$filename,"' has changed", call.=FALSE)
	srcfile$conn <- conn <- file(srcfile$filename, open="rt")
	srcfile$line <- 1
	oldline <- 1
    } else if (!isOpen(conn)) {
	open(conn, open="rt")
	srcfile$line <- 1
	oldline <- 1
    }
    if (oldline < line) {
	readLines(conn, line-oldline)
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

.isOpen <- function(srcfile) {
    conn <- srcfile$conn
    return( !is.null(conn) && isOpen(conn) )
}

getSrcLines <- function(srcfile, first, last) {
    if (first > last) return(character(0))
    if (!.isOpen(srcfile)) on.exit(close(srcfile))
    conn <- open(srcfile, first)
    lines <- readLines(conn, n=last-first+1)
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
  
as.character.srcref <- function(srcref) {
    srcfile <- attr(srcref, "srcfile")
    lines <- getSrcLines(srcfile, srcref[1], srcref[3])  
    lines[length(lines)] <- substring(lines[length(lines)], 1, srcref[4])
    lines[1] <- substring(lines[1], srcref[2])
    lines
}

print.srcref <- function(x, ...) 
    cat(as.character(x), sep="\n")
