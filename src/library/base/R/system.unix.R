getenv <- function(x) {
    if (missing(x)) {
	x <- strsplit(.Internal(getenv(character())), "=")
	v <- n <- character(LEN <- length(x))
	for (i in 1:LEN) {
	    n[i] <- x[[i]][1]
	    v[i] <- paste(x[[i]][-1], collapse = "=")
	}
	structure(v, names = n)
    } else {
	structure(.Internal(getenv(x)), names = x)
    }
}

help.start <- function (gui = "irrelevant", browser = .Options$browser,
			remote = NULL) {
    if(is.null(browser))
	stop("Invalid browser name, check options(\"browser\").")
    url <- paste(if (is.null(remote)) "$HOME/.R" else remote,
		 "/doc/html/index.html", sep = "")
    cat("If", browser, " is already running,\tit is *not* restarted,\n",
	"and you must switch to its window.\nOtherwise, be patient..\n")
    system(paste("${R_HOME}/bin/help.links",
		 paste(unique(.lib.loc), sep=" ", collapse=" "),
		 sep =" "))
    system(paste(browser, " -remote \"openURL(", url, ")\" 2>/dev/null || ",
		 browser, " ", url, " &", sep = ""))
    options(htmlhelp=TRUE)
}

system <- function(command, intern = FALSE, ignore.stderr = FALSE)
    .Internal(system(if(ignore.stderr) paste(command, "2>/dev/null") else
                     command, intern))

unix <- function(call, intern = FALSE) {
    .Deprecated("system"); system(call,intern)
}

##--- The following 2  should/could really be done in C [platform !] :
tempfile <- function(pattern = "file") {
    system(paste("for p in", paste(pattern, collapse = " "), ";",
		 "do echo /tmp/$p$$; done"),
	   intern = TRUE)
}

unlink <- function(x) { system(paste("rm -rf ", paste(x, collapse = " "))) }

zip.file.extract <- function(file, zipname="R.zip")
{
    ## somewhat system-specific.
    unzip <- options()$unzip
    if(!length(unzip)) return(file)
    path <- sub("[^/]*$","", file)
    topic <- substr(file, nchar(path)+1, 1000)
    if(file.exists(file.path(path, zipname))) {
        tempdir <- sub("[^/]*$", "", tempfile())
        if(!system(paste(unzip, "-o",
                         file.path(path, zipname), topic, "-d", tempdir,
                         " > /dev/null")))
            file <- paste(tempdir,  topic, sep="")
    }
    file
}
