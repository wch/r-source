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

print.srcfile <- function(srcfile) {
  cat(srcfile$filename, "\n")
  invisible(srcfile)
}

open.srcfile <- function(srcfile) {
  conn <- srcfile$conn
  if (is.null(conn)) {
    olddir <- setwd(srcfile$wd)
    on.exit(setwd(olddir))   
    timestamp <- file.info(srcfile$filename)[1,"mtime"]
    if (timestamp != srcfile$timestamp) warning("Timestamp of '",srcfile$filename,"' has changed", call.=FALSE)
    srcfile$conn <- file(srcfile$filename, open="rb")
  } else if (!isOpen(conn)) open(conn, open="rb")
}

close.srcfile <- function(srcfile) {
  conn <- srcfile$conn
  if (is.null(conn)) return()
  else {
    close(conn)
    rm("conn", envir=srcfile)
  }
}

.isOpen <- function(srcfile) {
  conn <- srcfile$conn
  return( !is.null(conn) && isOpen(conn) )
}

# a srcref is a start and stop byte in a srcfile

srcref <- function(srcfile, start, len) {
  stopifnot(inherits(srcfile, "srcfile"), length(start) == 1, length(len) == 1)
  structure(as.integer(c(start, len)), srcfile=srcfile, class="srcref")
}
  
as.character.srcref <- function(srcref) {
  srcfile <- attr(srcref, "srcfile")
  if (!.isOpen(srcfile)) {
    open(srcfile)
    on.exit(close(srcfile))
  }
  conn <- srcfile$conn
  seek(conn, srcref[1])

  # Read as raw here, because we want to read n bytes, not chars.
  bytes <- readBin(conn, what="raw", n=srcref[2])  
  rawToChar(bytes)
}

print.srcref <- function(srcref) 
  cat(as.character(srcref), "\n")
