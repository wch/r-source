example <-
function(topic, package = NULL, lib.loc = NULL, local = FALSE,
	 echo = TRUE, verbose = getOption("verbose"), setRNG = FALSE,
	 prompt.echo = paste(abbreviate(topic, 6), "> ", sep = ""))
{
    topic <- substitute(topic)
    if(!is.character(topic))
	topic <- deparse(topic)[1]
    INDICES <- .find.package(package, lib.loc, verbose = verbose)
    file <- index.search(topic, INDICES, "AnIndex", "R-ex")
    if(file == "") {
	warning(paste("No help file found for", sQuote(topic)))
	return(invisible())
    }
    packagePath <- dirname(dirname(file))
    if(length(file) > 1) {
	packagePath <- packagePath[1]
	warning(paste("More than one help file found: using package",
		      sQuote(basename(packagePath))))
	file <- file[1]
    }
    pkg <- basename(packagePath)
    lib <- dirname(packagePath)
    zfile <- zip.file.extract(file, "Rex.zip")
    if(zfile != file) on.exit(unlink(zfile))
    if(!file.exists(zfile)) {
	warning(paste(sQuote(topic),
		      "has a help file but no examples file"))
	return(invisible())
    }
    if(pkg != "base")
	library(pkg, lib = lib, character.only = TRUE)
    if(!is.logical(setRNG) || setRNG) {
	## save current RNG state:
	if((exists(".Random.seed", envir = .GlobalEnv))) {
	    oldSeed <- get(".Random.seed", envir = .GlobalEnv)
	    on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv))
	} else {
	    oldRNG <- RNGkind()
	    on.exit(RNGkind(oldRNG[1], oldRNG[2]))
	}
	## set RNG
	if(is.logical(setRNG)) { # i.e. == TRUE: use the same as R CMD check
	    ## see ../../../../share/perl/massage-Examples.pl
	    RNGkind("default", "default")
	    set.seed(1)
	} else eval(setRNG)
    }
    encoding <-
        if(length(enc <- localeToCharset()) > 1)
            c(enc[-length(enc)], "latin1")
        else ""
    source(zfile, local, echo = echo, prompt.echo = prompt.echo,
	   verbose = verbose, max.deparse.length = 250,
           encoding = encoding)
}
