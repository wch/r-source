## Was in  system.unix.R --  now system-independent
## thanks to Guido's  .Platform$show.data(.) idea.
data <- function(..., list = character(0), package =c(.packages(), .Autoloaded),
		 lib.loc = .lib.loc, verbose = .Options$verbose) 
{
    names <- c(as.character(substitute(list(...))[-1]), list)
    if (!missing(package))
	if (is.name(y <- substitute(package)))# && !is.character(package))
	    package <- as.character(y)
    found <- FALSE
    fsep <- .Platform$file.sep
    if (length(names) == 0) ## give `index' of all possible data sets
        .Platform$ show.data(package,lib.loc,fsep)
    else for (name in names) {
	dn <- paste("data", name, sep = fsep)
	files <- system.file(paste(dn, ".*", sep = ""), package, lib.loc)
	found <- FALSE
	if (files != "") {
	    subpre <- paste(".*", fsep, sep="")
	    for (file in files) {
		if(verbose)
		    cat("name=",name,":\t file= ...",fsep,
			sub(subpre,"",file),"::\t", sep="")
		if (found) break
		found <- TRUE
		ext <- sub(".*\\.", "", file)
		## make sure the match is really for `name.ext'
		if (sub(subpre, "", file) != paste(name, ".", ext, sep = ""))
		    found <- FALSE
		else
		    switch(ext,
			   "R" =, "r" = source(file),
			   "RData" =, "rdata" =, "rda" = load(file),
			   "TXT" =, "txt" =, "tab" =
			   assign(name, read.table(file, header= TRUE),
				  env = .GlobalEnv),
			   "CSV" =, "csv" =
			   assign(name, read.table(file, header= TRUE, sep=";"),
				  env = .GlobalEnv),
			   ## otherwise
			   found <- FALSE)
		if (verbose) cat(if(!found) "*NOT* ", "found\n")
	    }
	}
	if (!found)
	    warning(paste("Data set `", name, "' not found", sep = ""))
    }
    invisible(names)
}
