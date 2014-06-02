#  File src/library/utils/R/package.skeleton.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

package.skeleton <-
    function(name = "anRpackage", list = character(), environment = .GlobalEnv,
	     path = ".", force = FALSE,
             code_files = character())
{
    safe.dir.create <- function(path)
    {
	dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
	if(!dirTest(path) && !dir.create(path))
	    stop(gettextf("cannot create directory '%s'", path), domain = NA)
    }

    if(!is.character(code_files))
        stop("'code_files' must be a character vector")
    use_code_files <- length(code_files) > 0L

    envIsMissing <- missing(environment) # before R clobbers this information

    if(missing(list)) {
        if(use_code_files) {
            environment <- new.env(hash = TRUE)
            methods::setPackageName(name, environment)
            for(cf in code_files)
                sys.source(cf, envir = environment)
        }
	## all.names: crucial for metadata
	list <- ls(environment, all.names=TRUE)
    }
    if(!is.character(list))
	stop("'list' must be a character vector naming R objects")
    if(use_code_files || !envIsMissing) {
        classesList <- getClasses(environment)
        classes0 <- .fixPackageFileNames(classesList)
        names(classes0) <- classesList
        methodsList <- getGenerics(environment)
        methods0 <- .fixPackageFileNames(methodsList)
        names(methods0) <- methodsList
    }
    else { # nobody should  specify classes or methods as object names!
        classesList <- methodsList <- character()
    }
    usingS4 <- length(classesList) > 0L || length(methodsList) > 0L

    ## we need to test in the C locale
    curLocale <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", curLocale), add = TRUE)
    if(Sys.setlocale("LC_CTYPE", "C") != "C")
        warning("cannot turn off locale-specific chars via LC_CTYPE",
                domain = NA)

    have <- unlist(lapply(list, exists, envir = environment))
    if(any(!have))
        warning(sprintf(ngettext(sum(!have),
                                 "object '%s' not found",
                                 "objects '%s' not found"),
                        paste(sQuote(list[!have]), collapse=", ")),
                domain = NA)
    list <- list[have]
    if(!length(list))
	stop("no R objects specified or available")

    message("Creating directories ...", domain = NA)
    ## Make the directories
    dir <- file.path(path, name)
    if(file.exists(dir) && !force)
	stop(gettextf("directory '%s' already exists", dir), domain = NA)

    safe.dir.create(dir)
    safe.dir.create(code_dir <- file.path(dir, "R"))
    safe.dir.create(docs_dir <- file.path(dir, "man"))
    safe.dir.create(data_dir <- file.path(dir, "data"))

    ## DESCRIPTION
    message("Creating DESCRIPTION ...", domain = NA)
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
	if(usingS4) "Depends: methods\n",
	file = description, sep = "")
    close(description)

    ## NAMESPACE
    ## <NOTE>
    ## For the time being, we export all non-internal objects using the pattern
    ## of names beginning with alpha.  All S4 methods and classes are exported.
    ## S3 methods will be exported if the function's name would be exported.
    ## </NOTE>
    message("Creating NAMESPACE ...", domain = NA)
    out <- file(file.path(dir, "NAMESPACE"), "wt")
    writeLines("exportPattern(\"^[[:alpha:]]+\")", out)
    if(length(methodsList)) {
	cat("exportMethods(\n    ", file = out)
	cat(paste0('"', methodsList, '"', collapse = ",\n    "), "\n)\n", file = out)
    }
    if(length(classesList)) {
	cat("exportClasses(\n    ", file = out)
	cat(paste0('"', classesList, '"', collapse = ",\n     "), "\n)\n", file = out)
    }
    close(out)

    ## Read-and-delete-me
    message("Creating Read-and-delete-me ...", domain = NA)
    out <- file(file.path(dir, "Read-and-delete-me"), "wt")
    msg <-
        c("* Edit the help file skeletons in 'man', possibly combining help files for multiple functions.",
          "* Edit the exports in 'NAMESPACE', and add necessary imports.",
          "* Put any C/C++/Fortran code in 'src'.",
          "* If you have compiled code, add a useDynLib() directive to 'NAMESPACE'.",
          "* Run R CMD build to build the package tarball.",
          "* Run R CMD check to check the package tarball.",
          "",
          "Read \"Writing R Extensions\" for more information.")
    writeLines(strwrap(msg, exdent = 2), out)
    close(out)

    internalObjInds <- grep("^\\.", list)
    internalObjs <- list[internalObjInds]
    if(length(internalObjInds))
	list <- list[-internalObjInds]

    list0 <- .fixPackageFileNames(list)
    names(list0) <- list

    ## Dump the items in 'data' or 'R'
    if(!use_code_files) {
        message("Saving functions and data ...", domain = NA)
        if(length(internalObjInds))
            dump(internalObjs,
                 file = file.path(code_dir, sprintf("%s-internal.R", name)),
                 envir = environment)
        for(item in list){
            objItem <- get(item, envir = environment)
            if(is.function(objItem))  {
                if(isS4(objItem))
                    stop(gettextf("generic functions and other S4 objects (e.g., '%s') cannot be dumped; use the 'code_files' argument", item), domain = NA)
                dump(item,
                     file = file.path(code_dir, sprintf("%s.R", list0[item])),
                     envir = environment)
            }
            else       # we cannot guarantee this is a valid file name
                try(save(list = item, envir = environment,
                         file = file.path(data_dir, sprintf("%s.rda", item))))
        }
    } else {
        message("Copying code files ...", domain = NA)
        file.copy(code_files, code_dir)
        ## Only "abc.R"-like files are really ok:
	R_files <- tools::list_files_with_type(code_dir, "code",
					       full.names = FALSE,
					       OS_subdirs = "")
        code_files <- basename(code_files)
	wrong <- code_files[is.na(match(code_files, R_files))]
	if(length(wrong)) {
	    warning("Invalid file name(s) for R code in ", code_dir,":\n",
		    strwrap(paste(sQuote(wrong), collapse = ", "), indent=2),
		    "\n are now renamed to 'z<name>.R'", domain = NA)
	    file.rename(from = file.path(code_dir, wrong),
			to = file.path(code_dir,
			paste0("z", sub("(\\.[^.]*)?$", ".R", wrong))))
        }
    }

    ## Make help file skeletons in 'man'
    message("Making help files ...", domain = NA)
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
	sapply(classesList,
	       function(item) {
		   methods::promptClass(item,
					filename =
					file.path(docs_dir,
						  sprintf("%s-class.Rd", classes0[item])),
					where = environment)
	       })
	sapply(methodsList,
	       function(item) {
		   methods::promptMethods(item,
					  filename =
					  file.path(docs_dir,
						    sprintf("%s-methods.Rd", methods0[item])),
					  findMethods(item, where = environment))
	       })
    }))
    ## don't document generic functions from other packages
    for(item in methodsList) {
        if(exists(item, envir = environment, inherits = FALSE)) {
            ff <- get(item, envir = environment)
            if(is(ff, "genericFunction") && !identical(ff@package, name)) # don't document
                file.remove(file.path(docs_dir, sprintf("%s.Rd", list0[item])))
        }
    }
    if(inherits(yy, "try-error"))
	stop(yy)

    ## Now we may have created an empty data or R directory
    if(length(list.files(code_dir)) == 0L)
        unlink(code_dir, recursive = TRUE)
    if(length(list.files(data_dir)) == 0L)
        unlink(data_dir, recursive = TRUE)

    message("Done.", domain = NA)
    message(sprintf("Further steps are described in '%s'.",
                     file.path(dir, "Read-and-delete-me")),
            domain = NA)
}

.fixPackageFileNames <- function(list) {
        ## Some object names may not be valid file names, especially
        ## replacement function names.  And if we start changing them
        ## they may collide.
        ## <NOTE>
        ## If we use given code files, we could still check whether
        ## these file are valid across platforms ...
        ## </NOTE>
        list <- as.character(list) # remove S4 class if any, to add names() later
        if(length(list) == 0L) return(list)
        list0 <- gsub("[[:cntrl:]\"*/:<>?\\|]", "_", list)
        wrong <- grep("^(con|prn|aux|clock\\$|nul|lpt[1-3]|com[1-4])(\\..*|)$",
                      list0)
        if(length(wrong))
            list0[wrong] <- paste0("zz", list0[wrong])
        ## using grep was wrong, as could give -integer(0)
        ok <- grepl("^[[:alnum:]]", list0)
        if(any(!ok))
            list0[!ok] <- paste0("z", list0[!ok])
        ## now on Mac/Windows lower/uppercase will collide too
        list1 <- tolower(list0)
        list2 <- make.unique(list1, sep = "_")
        changed <- (list2 != list1)
        list0[changed] <- list2[changed]
        list0
}
