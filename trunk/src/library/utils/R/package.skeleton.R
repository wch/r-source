package.skeleton <-
    function(name = "anRpackage", list, environment = .GlobalEnv,
	     path = ".", force = FALSE, namespace = FALSE,
             code_files = character())
{
    safe.dir.create <- function(path)
    {
	dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
	if(!dirTest(path) && !dir.create(path))
	    stop(gettextf("cannot create directory '%s'", path), domain = NA)
    }

    if(!is.character(code_files))
        stop("'code_files' should be a character vector")
    use_code_files <- length(code_files) > 0

    if(missing(list)) {
        if(use_code_files) {
            environment <- new.env()
            for(cf in code_files)
                sys.source(cf, envir = environment)
        }
        list <- ls(env = environment)
    }

    if(!is.character(list))
	stop("'list' should be a character vector naming R objects")

    if(!is.logical(namespace) || (length(namespace) != 1))
        stop("'namespace' should be a single logical")

    ## we need to test in the C locale
    curLocale <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", curLocale), add = TRUE)
    if(Sys.setlocale("LC_CTYPE", "C") != "C")
        warning("cannot turn off locale-specific chars via LC_CTYPE")

    have <- sapply(list, exists, envir = environment)
    if(any(!have))
	warning(sprintf(ngettext(sum(!have),
				 "object '%s' not found",
				 "objects '%s' not found"),
			paste(sQuote(list[!have]), collapse=", ")),
		domain = NA)
    list <- list[have]
    if(!length(list))
	stop("no R objects specified or available")

    message("Creating directories ...")
    ## Make the directories
    dir <- file.path(path, name)    
    if(file.exists(dir) && !force)
	stop(gettextf("directory '%s' already exists", dir), domain = NA)

    safe.dir.create(dir)
    safe.dir.create(code_dir <- file.path(dir, "R"))
    safe.dir.create(docs_dir <- file.path(dir, "man"))
    safe.dir.create(data_dir <- file.path(dir, "data"))

    ## DESCRIPTION
    message("Creating DESCRIPTION ...")
    description <- file(file.path(dir, "DESCRIPTION"), "wt")
    cat("Package: ", name, "\n",
	"Type: Package\n",
	"Title: What the package does (short line)\n",
	"Version: 1.0\n",
	"Date: ", format(Sys.time(), format="%Y-%m-%d"), "\n",
	"Author: Who wrote it\n",
	"Maintainer: Who to complain to <yourfault@somewhere.net>\n",
	"Description: More about what it does (maybe more than one line)\n",
	"License: What license is it under?\n",
	file = description, sep = "")
    close(description)

    ## NAMESPACE
    ## <NOTE>
    ## For the time being, we export all non-internal objects we come up
    ## with.  S3 methods, or S4 classes and methods are not handled.
    ## </NOTE>
    if(namespace) {
        message("Creating NAMESPACE ...")
        out <- file(file.path(dir, "NAMESPACE"), "wt")
        writeLines("exportPattern(\"^[[:alpha:]]+\")", out)
        close(out)
    }

    ## Read-and-delete-me
    message("Creating Read-and-delete-me ...")
    out <- file(file.path(dir, "Read-and-delete-me"), "wt")
    msg <-
    c("* Edit the help file skeletons in 'man', possibly combining help files for multiple functions.",
      if(namespace)
      "* Edit the exports in 'NAMESPACE', and add necessary imports.",
      "* Put any C/C++/Fortran code in 'src'.",
      if(namespace)
      "* If you have compiled code, add a useDynLib() directive to 'NAMESPACE'."
      else
      "* If you have compiled code, add a .First.lib() function in 'R' to load the shared library.",
      "* Run R CMD build to build the package tarball.",
      "* Run R CMD check to check the package tarball.",
      "",
      "Read \"Writing R Extensions\" for more information.")
    writeLines(strwrap(msg, exdent = 2), out)
    close(out)

    internalObjInds <- grep("^\\.", list)
    internalObjs <- list[internalObjInds]
    if(any(internalObjInds))
	list <- list[-internalObjInds]

    if(!use_code_files) {
        ## Some object names may not be valid file names, especially
        ## replacement function names.  And if we start changing them
        ## they may collide.
        ## <NOTE>
        ## If we use given code files, we could still check whether
        ## these file are valid across platforms ...
        ## </NOTE>
        list0 <- gsub("[[:cntrl:]\"*/:<>?\\|]", "_", list)
        wrong <- grep("^(con|prn|aux|clock\\$|nul|lpt[1-3]|com[1-4])(\\..*|)$",
                      list0)
        if(length(wrong))
            list0[wrong] <- paste("zz", list0[wrong], sep="")
        ok <- grep("^[[:alnum:]]", list0)
        if(length(ok) < length(list0))
            list0[-ok] <- paste("z", list0[-ok], sep="")
        ## now on Windows lower/uppercase will collide too
        list1 <- tolower(list0)
        list2 <- make.unique(list1, sep="_")
        changed <- (list2 != list1)
        list0[changed] <- list2[changed]
    } else {
        list0 <- list
    }
    names(list0) <- list

    ## Dump the items in 'data' or 'R'
    if(!use_code_files) {
        message("Saving functions and data ...")
        if(any(internalObjInds))
            dump(internalObjs,
                 file = file.path(code_dir,
                                  sprintf("%s-internal.R", name)))
        for(item in list){
            if(is.function(get(item, envir = environment)))
                dump(item,
                     file = file.path(code_dir,
                                      sprintf("%s.R", list0[item])))
            else # we cannot guarantee this is a valid file name
                try(save(list = item,
                         file = file.path(data_dir,
                                          sprintf("%s.rda", item))))
        }
    } else {
        message("Copying code files ...")
        file.copy(code_files, code_dir)
    }

    ## Make help file skeletons in 'man'
    message("Making help files ...")
    if(any(internalObjInds)) {
	Rdfile <- file(file.path(docs_dir,
                                 sprintf("%s-internal.Rd", name)),
                       "wt")
	cat("\\name{", name, "-internal}\n",
	    "\\title{Internal ", name, " objects}\n",
	    file = Rdfile, sep = "")
	for(item in internalObjs) {
	    cat("\\alias{", item, "}\n", file = Rdfile, sep = "")
	}
	cat("\\description{Internal ", name, " objects.}\n",
	    "\\details{These are not to be called by the user.}\n",
	    "\\keyword{internal}",
	    file = Rdfile, sep = "")
	close(Rdfile)
    }
    ## Suppress partially inappropriate messages from prompt().
    yy <- try(suppressMessages({
        promptPackage(name,
                      filename =
                      file.path(docs_dir,
                                sprintf("%s-package.Rd", name)),
                      lib.loc = path)
        sapply(list,
               function(item) {
                   prompt(get(item, envir = environment),
                          name = item,
                          filename =
                          file.path(docs_dir,
                                    sprintf("%s.Rd", list0[item])))
               })
    }))
    if(inherits(yy, "try-error"))
	stop(yy)

    ## Now we may have created an empty data or R directory
    if(length(list.files(code_dir)) == 0)
        unlink(code_dir, recursive = TRUE)
    if(length(list.files(data_dir)) == 0)
        unlink(data_dir, recursive = TRUE)

    message("Done.")
    message(gettextf("Further steps are described in '%s'.",
                     file.path(dir, "Read-and-delete-me")),
            domain = NA)
}
