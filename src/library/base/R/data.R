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

        ## <FIXME>
        ## This is different from what is done for loading data sets:
        ## here, we warn/stop if given packages are not found.
        paths <- .find.package(package, lib.loc)
        ## </FIXME>
        if(missing(lib.loc))
            paths <- c(.path.package(package, TRUE), getwd(), paths)
        paths <- unique(paths[file.exists(paths)])

        ## Build the data db.
        db <- matrix(character(0), nr = 0, nc = 4)
        nodata <- noindex <- character(0)
        for(path in paths) {
            pkg <- basename(path)
            if(!file.exists(file.path(path, "data"))) {
                nodata <- c(nodata, pkg)
                next
            }
            INDEX <- file.path(path, "data", "00Index")
            ## <NOTE>
            ## Earlier versions also used to check for `index.doc'.
            ## </NOTE>
            if(file.exists(INDEX))
                db <- rbind(db,
                            cbind(basename(path),
                                  dirname(path),
                                  read.00Index(INDEX)))
            else {
                ## no index: check for datasets -- won't work if zipped
                if(length(list.files(file.path(path, "data"))) > 0)
                    noindex <- c(noindex, pkg)
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")

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

        footer <- if(missing(package))
            paste("Use `data(package = ",
                  ".packages(all.available = TRUE))'\n",
                  "to list the data sets in all ",
                  "*available* packages.", sep = "")
        else
            NULL
        y <- list(type = "data",
                  header = NULL, results = db, footer = footer)
        class(y) <- "packageIQR"
        return(y)
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
