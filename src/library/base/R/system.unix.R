.Platform <-
    list(OS.type = "Unix",
	 file.sep = "/",
	 dynlib.ext = ".so",
	 show.file = function(file) system(paste(options("pager")[[1]], file)),
	 append.file = function(f1,f2) {# append to 'f1' the file 'f2':
	     system(paste("cat", f2, ">>", f1), trash.errors= TRUE)
	 },
	 show.data = function(package,lib.loc,fsep) {
	 ## give `index' of all possible data sets
	     for (lib in lib.loc)
	     for (pkg in package) {
	      INDEX <- system.file(paste("data", "index.doc", sep = fsep),
			      pkg, lib)
	      if (INDEX != "") {
	       cat(paste("\n\nData sets in package `", pkg, "':\n\n",
			 sep = fsep))
	       .Platform$ show.file(INDEX)
	      }}},	 
	 )

bug.report <- function(send=TRUE, method=.Options$mailer)
{
    methods <- c("mailx", "gnudoit")

    method <-
	if(is.null(method)) "mailx"
	else methods[pmatch(method, methods)]

    body <- paste("\\n\\n",
		  "--please do not edit the information below--\\n\\n",
		  "Version:\\n ",
		  paste(names(version), version, sep=" = ", collapse="\\n "),
		  "\\n\\n",
		  "Search Path:\\n ",
		  paste(search(), collapse=", "),
		  "\\n", sep="", collapse="")

    if(method == "mailx") {
	file <- tempfile()
	cat("Subject: ")
	subject <- readline()
	body <- gsub("\\\\n", "\n", body)
	cat(body, file=file)

	system(paste(.Options$editor, file))
	cmd <- paste("mailx", "-s '", subject,
		     "' r-bugs@biostat.ku.dk < ", file)
	if(send){
	    cat("Submit the bug report? ")
	    answer <- readline()
	    answer <- grep("y", answer, ignore.case=TRUE)
	    if(length(answer)>0){
		cat("Sending email ...\n")
		system(cmd)
	    }
	    else
		cat("OK, not sending email, deleting report ...\n")
	    unlink(file)
	}
	else
	    cat("The unsent bug report can be found in file",
		file, "\n")
    }
    else if(method == "gnudoit") {
	cmd <- paste("gnudoit -q '",
		     "(mail nil \"r-bugs@biostat.ku.dk\")",
		     "(insert \"", body, "\")",
		     "(search-backward \"Subject:\")",
		     "(end-of-line)'",
		     sep="")
	system(cmd)
    }
}

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

help <- function(topic, offline = FALSE, package = c(.packages(), .Autoloaded),
		 lib.loc = .lib.loc, verbose = .Options$verbose,
		 htmlhelp = .Options$htmlhelp) {
    if (!missing(package))
	if (is.name(y <- substitute(package)))# && !is.character(package))
	    package <- as.character(y)
    fsep <- .Platform$file.sep
    if (!missing(topic)) {
	topic <- substitute(topic)
	if (is.name(topic))
	    topic <- as.character(topic)
	else if (!is.character(topic))
	    stop("Unimplemented help feature")
	if (!is.na(match(topic, c("+", "-", "*", "/", "^", "%%"))))
	    topic <- "Arithmetic"
	else if (!is.na(match(topic, c("<", ">", "<=", ">=", "==", "!="))))
	    topic <- "Comparison"
	else if (!is.na(match(topic, c("[", "[[", "$"))))
	    topic <- "Extract"
	else if (!is.na(match(topic, c("&", "&&", "|", "||", "!"))))
	    topic <- "Logic"
	else if (!is.na(match(topic, c("%*%"))))
	    topic <- "matmult"
	topic <- gsub("\\[","\\\\[", topic)# for cmd/help ..
	INDICES <- paste(t(outer(lib.loc, package, paste, sep = fsep)),
			 "help", "AnIndex", sep = fsep, collapse = " ")
	file <- system(paste("${RHOME}/bin/help INDEX '", topic, "' ",
			     INDICES, sep=""),
		       intern = TRUE)
	if (file == "") {		# try data .doc -- this is OUTDATED
	    file <- system.file(paste("data", fsep, topic, ".doc", sep = ""),
				package, lib.loc)
	}
	if (length(file) && file != "") {
	    if (verbose)
		cat ("\t\t\t\t\t\tHelp file name `", sub(".*/", "", file),
		     ".Rd'\n", sep = "")
	    if (!offline) {
		if(!is.null(htmlhelp) && htmlhelp){
		    file <- gsub(paste("/help/", topic, sep=""),
				 paste("/html/", topic, sep=""),
				 file)
		    file <- paste("file:", file, ".html", sep="")
		    if(is.null(.Options$browser))
			stop("options(\"browser\") not set")
		    browser <- .Options$browser
		    system(paste(browser, " -remote \"openURL(", file,
				 ")\" 2>/dev/null || ",
				 browser, " ", file, " &", sep = ""))
		    cat("help() for", topic, " is shown in browser",
			browser, "...\n")
		}
		else
		    .Platform$ show.file(file)
	    }
	    else {
		FILE <- tempfile()
		## on.exit(unlink(paste(FILE, "*", sep = "")))
		cat("\\documentclass[", .Options$papersize, "paper]{article}\n",
		    file = FILE, sep = "")
		.Platform$ append.file(FILE, "${RHOME}/doc/manual/Rd.sty")
		cat("\\InputIfFileExists{Rhelp.cfg}{}{}\n\\begin{document}\n",
		    file = FILE, append = TRUE)
		.Platform$ append.file(FILE,
				       paste(sub("help/","latex/",file),".tex",
					     sep = ""))
		cat("\\end{document}\n", file = FILE, append = TRUE)
		system(paste("${RHOME}/bin/help PRINT", FILE, topic))
		return()
	    }
	} else
	stop(paste("No documentation for `", topic, "'", sep = ""))
    }
    else if (!missing(package))
	library(help = package, lib = lib.loc, character.only = TRUE)
    else if (!missing(lib.loc))
	library(lib = lib.loc)
    else
	help("help", package = "base", lib.loc = .Library)
}

help.start <- function (gui = "irrelevant", browser = .Options$browser,
			remote = NULL) {
    if(is.null(browser))
	stop("Invalid browser name, check options(\"browser\").")
    url <- paste(if (is.null(remote)) "$HOME/.R" else remote,
		 "/doc/html/index.html", sep = "")
    cat("If", browser, " is already running,\tit is *not* restarted,\n",
	"and you must switch to its window.\nOtherwise, be patient..\n")
    system(paste("${RHOME}/bin/help.links",
		 paste(.lib.loc[length(.lib.loc):1], sep=" ", collapse=" "),
		 sep =" "))
    system(paste(browser, " -remote \"openURL(", url, ")\" 2>/dev/null || ",
		 browser, " ", url, " &", sep = ""))
    options(htmlhelp=TRUE)
}

system <- function(call, intern = FALSE, trash.errors = FALSE)
    .Internal(system(if(trash.errors) paste(call, "2>/dev/null") else call,
		     intern))

unix <- function(call, intern = FALSE) {
    .Deprecated("system"); system(call,intern)
}

system.file <- function(file = "", pkg = .packages(), lib = .lib.loc) {
    FILES <- paste(t(outer(lib, pkg, paste, sep = .Platform$file.sep)),
		   file, sep = .Platform$file.sep, collapse = " ")
    system(paste("${RHOME}/bin/filename", FILES), intern = TRUE)
}

##--- All the following should really be done in C [platform !] :
##---> For the first 3, look at Guido's win32 code!

date <- function() { system("date", intern = TRUE) }

tempfile <- function(pattern = "file") {
    system(paste("for p in", paste(pattern, collapse = " "), ";",
		 "do echo /tmp/$p$$; done"),
	   intern = TRUE)
}

unlink <- function(x) { system(paste("rm -rf ", paste(x, collapse = " "))) }

## Unfortunately, the following fails for "-e" on Solaris [/bin/sh-builtin test]
system.test <- function(...) { system(paste("test", ...)) == 0 }
## Martin would like these;  Kurt thinks they're unnecessary:
## file.exists <-function(file){ sapply(file, function(f)system.test("-e", f))} 
## dir.exists  <-function(dir) { sapply(dir,  function(d)system.test("-d", d))} 
## Yet another misuse of  is.xxx naming [S-plus compatibility]:
## is.dir <- .Alias(dir.exists)
