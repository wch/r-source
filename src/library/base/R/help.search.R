help.search <- function(pattern, fields = c("alias", "title"),
                        apropos, keyword, whatis, ignore.case = TRUE,
                        package = NULL, lib.loc = .lib.loc,
                        help.db = getOption("help.db"),
                        verbose = getOption("verbose"),
                        rebuild = FALSE) {
    TABLE <- c("name", "alias", "title", "keyword")
    if (!missing(pattern)) {
        if (!is.character(pattern) || (length(pattern) > 1))
            stop("`pattern' must be a single character string")
        i <- pmatch(fields, TABLE)
        if (any(is.na(i)))
            stop("incorrect field specification")
        else
            fields <- TABLE[i]
    } else if (!missing(apropos)) {
        if (!is.character(apropos) || (length(apropos) > 1))
            stop("`apropos' must be a single character string")
        else {
            pattern <- apropos
            fields <- c("alias", "title")
        }
    } else if (!missing(keyword)) {
        if (!is.character(keyword) || (length(keyword) > 1))
            stop("`keyword' must be a single character string")
        else {
            pattern <- keyword
            fields <- "keyword"
        }
    } else if (!missing(whatis)) {
        if (!is.character(whatis) || (length(whatis) > 1))
            stop("`whatis' must be a single character string")
        else {
            pattern <- whatis
            fields <- "alias"
        }
    } else {
        stop("don't know what to search")
    }

    ## Set up the help db
    if(rebuild || is.null(help.db) || !file.exists(help.db)) {
        ## Check whether we can save the help db lateron
        save.db <- FALSE
        dir <- switch(.Platform$OS.type,
                      "windows" = Sys.getenv("R_USER"),
                      "unix" = Sys.getenv("HOME"),
                      "")
        if(nchar(dir) == 0) dir <- getwd()
        dir <- file.path(dir, ".R")
        dbfile <- file.path(dir, "help.db")
        if((file.exists(dir) || dir.create(dir)) && (unlink(dbfile) == 0))
            save.db <- TRUE
        ## Create the help db
        db <- NULL
        if(verbose) {
            cat("Packages:\n")
            np <- 0
        }
        if(is.null(package))
            package <-.packages(all.available = TRUE, lib.loc = lib.loc)
        for(p in package) {
            if(verbose)
                cat("", p, if((np <- np + 1)%% 5 == 0) "\n")
            path <- .find.package(p, lib.loc)
            lib <- dirname(path)
            cfile <- file.path(path, "CONTENTS")
            if(file.exists(cfile)) {
                ctext <- read.dcf(cfile,
                                 fields = c("Entry", "Aliases",
                                 "Description", "Keywords"))
                if((nr <- NROW(ctext)) > 0){
                    db <- rbind(db,
                                cbind(rep(p, nr), rep(lib, nr), ctext))
                } else {
                    warning(paste("Empty `CONTENTS' file of pkg", p,
                                  "in", lib))
                }
            }
        }
        if(verbose && (np %% 5 == 0)) cat("\n")
        colnames(db) <- c("pkg", "lib", TABLE)
        ## Maybe save the help db
        if(save.db) {
            save(db, file = dbfile)
            options(help.db = dbfile)
        }
    } else {
        load(file = help.db)
    }

    ## Matching
    if(verbose) cat("\nDatabase of dimension", dim(db))
    i <- NULL
    for (f in fields)
        i <- c(i, grep(pattern, db[, f], ignore.case = ignore.case))

    db <- db[sort(unique(i)), , drop = FALSE]
    if(verbose) cat(", matched", NROW(db),"entries.\n")

    ## Output
    fields <- paste(fields, collapse = " or ")
    if (NROW(db) > 0) {
        outFile <- tempfile()
        outConn <- file(outFile, open = "w")
        writeLines(paste("Help files with ", fields, " matching `",
                         pattern, "':\n", "Type `?FOO' to inspect ",
                         "entry `FOO(PKG) TITLE'.\n\n", sep = ""),
                   outConn)
        dbnam <- paste(db[ , "name"], "(", db[, "pkg"], ")", sep = "")
        dbtit <- paste(db[ , "title"], sep = "")
        writeLines(formatDL(dbnam, dbtit), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE)
    } else {
        cat(paste("No help files found with ", fields, " matching `",
                  pattern, "'\n", sep = ""))
    }

    return(invisible())
}
