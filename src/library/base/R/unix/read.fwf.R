read.fwf <- function(file, widths, sep = "", as.is = FALSE,
		     skip = 0, row.names, col.names)
{
##-     if(!(is.character(file) && file.exists(file)))
##- 	stop(paste('"',file,'" is not an existing file', sep=""))
    FILE <- tempfile("R.")
    on.exit(unlink(FILE))
    cmd <- paste("${R_HOME}/bin/fwf2table -f",
		 deparse(paste("A", widths, sep = "", collapse = " ")),
		 "-s", deparse(sep), file, ">", FILE)
    system(cmd)
    read.table(file = FILE, header = FALSE, sep = sep, as.is = as.is,
	       skip = skip, row.names = row.names, col.names = col.names)
}
