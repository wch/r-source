data <-
function(..., list = character(0),
         ## package = c(.packages(), .Autoloaded),
         package = .packages(),
         lib.loc = .lib.loc, verbose = getOption("verbose"))
{
    names <- c(as.character(substitute(list(...))[-1]), list)
    if(!missing(package))
        if(is.name(y <- substitute(package)))
            package <- as.character(y)
    found <- FALSE
    fsep <- .Platform$file.sep

    if(length(names) == 0) {
        ## Give `index' of all possible data sets.
        ## Currently always returns character(0).  To change this, we
        ## would need to parse the `00Index' files ...

        ## <FIXME>
        ## This is different from what is done for loading data sets:
        ## here, we warn/stop if given packages are not found.
        paths <- .find.package(package, lib.loc)
        ## </FIXME>
        if(missing(lib.loc))
            paths <- c(.path.package(package, TRUE), getwd(), paths)
        paths <- unique(paths[file.exists(paths)])
        
        first <- TRUE
        nodata <- noindex <- character(0)
        outFile <- tempfile("Rdata.")
        outConn <- file(outFile, open = "w")

        for(path in paths) {
            pkg <- basename(path)
            if(!file.exists(file.path(path, "data"))) {
                nodata <- c(nodata, pkg)
                next
            }
            INDEX <- file.path(path, "data", "00Index")
            if(!file.exists(INDEX))
                INDEX <- file.path(path, "data", "index.doc")
            if(file.exists(INDEX)) {
                writeLines(paste(ifelse(first, "", "\n"),
                                 "Data sets in package `",
                                 pkg, "':\n\n", sep = ""),
                           outConn)
                writeLines(readLines(INDEX), outConn)
                first <- FALSE
            } else {
                ## no index: check for datasets -- won't work if zipped
                files <- list.files(file.path(path, "data"))
                if(length(files) > 0) noindex <- c(noindex, pkg)
            }
        }
        if(first) {
            warning("no data listings found")
            close(outConn)
            unlink(outFile)
        }
        else {
            if(missing(package))
                writeLines(paste("\n",
                                 "Use `data(package = ",
                                 ".packages(all.available = TRUE))'\n",
                                 "to list the data sets in all ",
                                 "*available* packages.", sep = ""),
                           outConn)
            close(outConn)
            file.show(outFile, delete.file = TRUE,
                      title = "R data sets")
        }
        if(!missing(package) && (length(package) > 0)) {
            nodata <- nodata[nodata %in% package]
            if(length(nodata) > 1)
                warning(paste("packages `",
                              paste(nodata, collapse=", "),
                              "' contain no datasets", sep=""))
            else if(length(nodata) == 1)
                warning(paste("package `", nodata,
                              "' contains no datasets", sep=""))
        }
        if(length(noindex) > 1)
            warning(paste("packages `", paste(noindex, collapse=", "),
                          "' contain datasets but no index", sep=""))
        else if(length(noindex) == 1)
            warning(paste("package `", noindex,
                          "' contains datasets but no index", sep=""))
        return(invisible(character(0)))
    }

    for(name in names) {
        paths <- .find.package(package, lib.loc, quiet = TRUE)
        if(missing(lib.loc))
            paths <- c(.path.package(package, TRUE), getwd(), paths)
        paths <- file.path(paths, "data")
        paths <- unique(paths[file.exists(paths)])
        files <- NULL
        for (p in paths) {
            if(file.exists(file.path(p, "Rdata.zip"))) {
                if(file.exists(fp <- file.path(p, "filelist")))
                    files <- c(files,
                               file.path(p, scan(fp, what="", quiet = TRUE)))
                else warning(paste("`filelist' is missing for dir", p))
            } else {
                files <- c(files, list.files(p, full=TRUE))
            }
        }
        files <- files[grep(name, files)]
        found <- FALSE
        if (length(files) > 0) {
            subpre <- paste(".*", fsep, sep = "")
            for (file in files) {
                if (verbose)
                    cat("name=", name, ":\t file= ...", fsep,
                        sub(subpre, "", file), "::\t", sep = "")
                if (found)
                    break
                found <- TRUE
                ext <- sub(".*\\.", "", file)
                ## make sure the match is really for `name.ext'
                ## otherwise
                if (sub(subpre, "", file) != paste(name, ".", ext, sep = ""))
                    found <- FALSE
                else {
                    zfile <- zip.file.extract(file, "Rdata.zip")
                    switch(ext,
                           R = ,
                           r = source(zfile, chdir = TRUE),
                           RData = ,
                           rdata = ,
                           rda = load(zfile, envir = .GlobalEnv),
                           TXT = ,
                           txt = ,
                           tab = assign(name, read.table(zfile, header = TRUE),
                           env = .GlobalEnv), CSV = ,
                           csv = assign(name,
                           read.table(zfile, header = TRUE, sep = ";"),
                           env = .GlobalEnv), found <- FALSE)
                    if (zfile != file) unlink(zfile)
                }
                if (verbose)
                    cat(if (!found)
                        "*NOT* ", "found\n")
            }
        }
        if (!found)
            warning(paste("Data set `", name, "' not found", sep = ""))
    }
    invisible(names)
}
