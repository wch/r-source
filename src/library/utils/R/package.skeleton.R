package.skeleton <-
function(name = "anRpackage", list, environment = .GlobalEnv,
         path = ".", force = FALSE)
{
    safe.dir.create <- function(path)
    {
        dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
        if(!dirTest(path) && !dir.create(path))
            stop("cannot create directory ", sQuote(path))
    }

    if(missing(list))
        list<-ls(env=environment)

    if(!is.character(list))
        stop("'list' should be a character vector naming R objects")
    have <- sapply(list, exists)
    if(any(!have))
        warning("object(s) ", paste(sQuote(list[!have]), collapse=", "),
                " not found")
    list <- list[have]
    if(!length(list))
        stop("no R objects specified or available")

    cat("Creating directories ...\n")
    ## Make the directories
    if(file.exists(file.path(path,name)) && !force)
        stop("Directory ", sQuote(name), " already exists")

    safe.dir.create(file.path(path, name))
    safe.dir.create(file.path(path, name, "man"))
    safe.dir.create(file.path(path, name, "src"))
    safe.dir.create(file.path(path, name, "R"))
    safe.dir.create(file.path(path, name, "data"))

    ## DESCRIPTION
    cat("Creating DESCRIPTION ...\n")
    description <- file(file.path(path, name, "DESCRIPTION"), "wt")
    cat("Package: the_name_of_the_package\n",
        "Title: What the package does\n",
        "Version: 1.0\n",
        "Author: Who wrote it\n",
        "Description: More about what it does\n",
        "Maintainer: Who to complain to <yourfault@somewhere.net>\n",
        "License: What license is it under?\n",
        file = description, sep = "")
    close(description)

    ## READMEs
    cat("Creating READMEs ...\n")

    ## src/README
    readme <- file(file.path(path, name, "src", "README"), "wt")
    cat("Put C/C++/Fortran code here.\n",
        "If you have compiled code, add a .First.lib() function\n",
        "in the 'R' subdirectory to load it.\n",
        file = readme, sep = "")
    close(readme)

    ## man/README
    readme <- file(file.path(path, name, "man", "README"), "wt")
    cat("Edit these help files.\n",
        "You may want to combine the help files for multiple functions.\n",
        file = readme, sep = "")
    close(readme)

    readme <- file(file.path(path, name, "README"), "wt")
    cat("1. Put any C/C++/Fortran code in 'src'\n",
        "2. If you have compiled code, add a .First.lib() function in 'R'\n",
        "   to load the shared library\n",
        "3. Edit the help file skeletons in 'man'\n",
        "4. Run R CMD build to create the index files\n",
        "5. Run R CMD check to check the package\n",
        "6. Run R CMD build to make the package file\n",
        "\n\nRead \"Writing R Extensions\" for more information.\n",
        file = readme, sep = "")
    close(readme)

    internalObjInds <- grep("^\\.", list)
    internalObjs <- list[internalObjInds]
    if(any(internalObjInds))
        list <- list[-internalObjInds]

    ## Some object names may not be valid file names, especially replacement
    ## function names. And if we start changing them they may collide.
    list0 <- gsub("[[:cntrl:]\"*/:<>?\\|]", "_", list)
    wrong <- grep("^(con|prn|aux|clock\\$|nul|lpt[1-3]|com[1-4])(\\..*|)$",
                  list0)
    if(length(wrong)) list0[wrong] <- paste("zz", list0[wrong], sep="")
    ## now on Windows lower/uppercase will collide too
    list1 <- tolower(list0)
    list2 <- make.unique(list1, sep="_")
    changed <- (list2 != list1)
    list0[changed] <- list2[changed]
    names(list0) <- list

    ## Dump the items in 'data' or 'R'
    cat("Saving functions and data ...\n")
    if(any(internalObjInds))
        dump(internalObjs,
             file = file.path(path, name, "R",
                              paste(name, "-internal.R", sep = "")))
    for(item in list){
        if(is.function(get(item)))
            dump(item,
                 file = file.path(path, name, "R",
                                  paste(list0[item], "R", sep = ".")))
        else # we cannot guarantee this is a valid file name
            try(save(list = item,
                     file = file.path(path, name, "data",
                                      paste(item, "rda", sep = "."))))
    }

    ## Make help file skeletons in 'man'
    cat("Making help files ...\n")
    if(any(internalObjInds)) {
        Rdfile <- file(file.path(path, name, "man",
                                 paste(name, "-internal.Rd", sep = "")),
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
    ## Redirect output so that we do not see the partially inappropriate
    ## messages from prompt().
    outFile <- tempfile()
    outConn <- file(outFile, "w")
    sink(outConn, type = "output")
    yy <- try(sapply(list,
                     function(item) {
                         prompt(item,
                                filename = file.path(path, name, "man",
                                paste(list0[item], "Rd", sep=".")))
                     }))
    sink(type = "output")
    close(outConn)
    unlink(outFile)
    if(inherits(yy, "try-error"))
        stop(yy)

    ## Now we may have created an empty data or R directory
    Rdir <- file.path(path, name, "R")
    if(length(list.files(Rdir)) == 0) unlink(Rdir, recursive=TRUE)
    datadir <- file.path(path, name, "data")
    if(length(list.files(datadir)) == 0) unlink(datadir, recursive=TRUE)

    cat("Done.\n")
    cat(paste("Further steps are described in",
              file.path(path, name, "README"),
              "\n"))
}
