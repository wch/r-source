data <-
function(..., list = character(0),
         package = .packages(), lib.loc = NULL,
         verbose = getOption("verbose"))
{
    sQuote <- function(s) paste("`", s, "'", sep = "")

    names <- c(as.character(substitute(list(...))[-1]), list)
    if(!missing(package))
        if(is.name(y <- substitute(package)))
            package <- as.character(y)
    found <- FALSE
    fsep <- .Platform$file.sep

    ## Find the directories of the given packages and maybe the working
    ## directory.
    paths <- .find.package(package, lib.loc, verbose = verbose)
    if(is.null(lib.loc))
        paths <- c(.path.package(package, TRUE), getwd(), paths)
    paths <- unique(paths[file.exists(paths)])

    ## Find the directories with a 'data' subdirectory.
    nodata <- !file.exists(file.path(paths, "data"))
    nodata[!file.info(file.path(paths, "data"))$isdir] <- TRUE
    if(any(nodata)) {
        if(!missing(package) && (length(package) > 0)) {
            ## Warn about given packages which do not have a 'data'
            ## subdirectory.
            packagesWithNoData <-
                package[package %in% sapply(paths[nodata], basename)]
            if(length(packagesWithNoData) > 1) {
                warning(paste("packages",
                              paste(sQuote(packagesWithNoData),
                                    collapse=", "),
                              "contain no datasets"))
            }
            else if(length(packagesWithNoData) == 1) {
                warning(paste("package", sQuote(packagesWithNoData),
                              "contains no datasets"))

            }
        }
        paths <- paths[!nodata]
    }

    if(length(names) == 0) {
        ## List all possible data sets.

        ## Build the data db.
        db <- matrix(character(0), nr = 0, nc = 4)
        noindex <- character(0)
        for(path in paths) {
            INDEX <- file.path(path, "data", "00Index")
            ## <NOTE>
            ## Earlier versions also used to check for 'index.doc'.
            ## </NOTE>
            if(file.exists(INDEX)) {
                entries <- read.00Index(INDEX)
                if(NROW(entries) > 0) {
                    db <- rbind(db,
                                cbind(basename(path),
                                      dirname(path),
                                      entries))
                }
            }
            else {
                ## no index: check whether subdir 'data' contains files.
                if(length(list.files(file.path(path, "data"))) > 0)
                    noindex <- c(noindex, basename(path))
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")

        if(length(noindex) > 0) {
            if(!missing(package) && (length(package) > 0)) {
                ## Warn about given packages which do not have a data
                ## index.
                packagesWithNoIndex <- package[package %in% noindex]
                if(length(packagesWithNoIndex) > 1) {
                    warning(paste("packages",
                                  paste(sQuote(packagesWithNoIndex),
                                        collapse=", "),
                                  "contain data sets but no index"))
                }
                else if(length(packagesWithNoIndex) == 1)
                    warning(paste("package",
                                  sQuote(packagesWithNoIndex),
                                  "contains data sets but no index"))
            }
        }

        footer <- if(missing(package))
            paste("Use ",
                  sQuote(paste("data(package =",
                               ".packages(all.available = TRUE))")),
                  "\n",
                  "to list the data sets in all *available* packages.",
                  sep = "")
        else
            NULL
        y <- list(type = "data", title = "Data sets",
                  header = NULL, results = db, footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }

    paths <- file.path(paths, "data")
    for(name in names) {
        files <- NULL
        for (p in paths) {
            if(file.exists(file.path(p, "Rdata.zip"))) {
                if(file.exists(fp <- file.path(p, "filelist")))
                    files <-
                        c(files,
                          file.path(p, scan(fp, what="", quiet = TRUE)))
                else warning(paste(sQuote("filelist"),
                                   "is missing for dir",
                                   sQuote(p)))
            } else {
                files <- c(files, list.files(p, full = TRUE))
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
                ## make sure the match is really for 'name.ext'
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
            warning(paste("Data set", sQuote(name), "not found"))
    }
    invisible(names)
}
