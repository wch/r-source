data <-
function(..., list = character(0),
         package = .packages(), lib.loc = NULL,
         verbose = getOption("verbose"), envir = .GlobalEnv)
{
    fileExt <- function(x) sub(".*\\.", "", x)

    names <- c(as.character(substitute(list(...))[-1]), list)

    ## Find the directories of the given packages and maybe the working
    ## directory.
    paths <- .find.package(package, lib.loc, verbose = verbose)
    if(is.null(lib.loc))
        paths <- c(.path.package(package, TRUE), getwd(), paths)
    paths <- unique(paths[file.exists(paths)])

    ## Find the directories with a 'data' subdirectory.
    paths <- paths[tools::fileTest("-d", file.path(paths, "data"))]
    ## Earlier versions remembered given packages with no 'data'
    ## subdirectory, and warned about them.

    dataExts <- tools:::.makeFileExts("data")

    if(length(names) == 0) {
        ## List all possible data sets.

        ## Build the data db.
        db <- matrix(character(0), nr = 0, nc = 4)
        noindex <- character(0)
        for(path in paths) {
            entries <- NULL
            ## Use "." as the 'package name' of the working directory.
            packageName <-
                if(tools::fileTest("-f",
                                   file.path(path, "DESCRIPTION")))
                    basename(path)
                else
                    "."
            ## Check for new-style 'Meta/data.rds', then for '00Index'.
            ## Earlier versions also used to check for 'index.doc'.
            if(tools::fileTest("-f",
                               INDEX <-
                               file.path(path, "Meta", "data.rds"))) {
                entries <- .readRDS(INDEX)
            }
            else if(tools::fileTest("-f",
                                    INDEX <-
                                    file.path(path, "data", "00Index")))
                entries <- read.00Index(INDEX)
            else {
                ## No index: check whether subdir 'data' contains data
                ## sets.  Easy if data files were not collected into a
                ## zip archive ... in any case, as data sets found are
                ## available for loading, we also list their names.
                dataDir <- file.path(path, "data")
                entries <- tools::listFilesWithType(dataDir, "data")
                if((length(entries) == 0)
                   && all(tools::fileTest("-f",
                                          file.path(dataDir,
                                                    c("Rdata.zip",
                                                      "filelist"))))) {
                    entries <- readLines(file.path(dataDir, "filelist"))
                    entries <- entries[fileExt(entries) %in% dataExts]
                }
                if(length(entries) > 0) {
                    entries <-
                        unique(tools::filePathSansExt(basename(entries)))
                    entries <- cbind(entries, "")
                }
                else
                    noindex <- c(noindex, packageName)
            }
            if(NROW(entries) > 0) {
                db <- rbind(db,
                            cbind(packageName, dirname(path),
                                  entries))
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")

        if(length(noindex) > 0) {
            if(!missing(package) && (length(package) > 0)) {
                ## Warn about given packages which do not have a data
                ## index.
                packagesWithNoIndex <- package[package %in% noindex]
                if(length(packagesWithNoIndex) > 0)
                    warning(paste("packages with data sets",
                                  "but no index:",
                                  paste(sQuote(packagesWithNoIndex),
                                        collapse = ",")))
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
        y <- list(title = "Data sets", header = NULL, results = db,
                  footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }

    paths <- file.path(paths, "data")
    for(name in names) {
	if (name == "CO2") name <- "zCO2"
        files <- NULL
        for(p in paths) {
            if(tools::fileTest("-f", file.path(p, "Rdata.zip"))) {
                if(tools::fileTest("-f",
                                   fp <- file.path(p, "filelist")))
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
        files <- files[grep(name, files, fixed = TRUE)]
        found <- FALSE
        if(length(files) > 1) {
            ## more than one candidate
            o <- match(fileExt(files), dataExts, nomatch = 100)
            paths0 <- dirname(files)
            paths0 <- factor(paths0, levels=paths0)
            files <- files[order(paths0, o)]
        }
        if(length(files) > 0) {
            for(file in files) {
                if(verbose)
                    cat("name=", name, ":\t file= ...",
                        .Platform$file.sep, basename(file), "::\t",
                        sep = "")
                if(found)
                    break
                found <- TRUE
                ext <- fileExt(file)
                ## make sure the match is really for 'name.ext'
                ## otherwise
                if(basename(file) != paste(name, ".", ext, sep = ""))
                    found <- FALSE
                else {
                    zfile <- zip.file.extract(file, "Rdata.zip")
                    switch(ext,
                           R = , r =
                           sys.source(zfile, chdir = TRUE,
                                      envir = envir),
                           RData = , rdata = , rda =
                           load(zfile, envir = envir),
                           TXT = , txt = , tab =
                           assign(name,
                                  read.table(zfile, header = TRUE),
                                  envir = envir),
                           CSV = , csv =
                           assign(name,
                                  read.table(zfile, header = TRUE,
                                             sep = ";"),
                                  envir = envir),
                           found <- FALSE)
                    if(zfile != file) unlink(zfile)
                }
                if(verbose)
                    cat(if(!found) "*NOT* ", "found\n")
            }
        }
        if(!found)
            warning(paste("Data set", sQuote(name), "not found"))
    }
    invisible(names)
}
