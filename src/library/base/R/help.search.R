help.search <-
function(pattern, fields = c("alias", "title"),
         apropos, keyword, whatis, ignore.case = TRUE,
         package = NULL, lib.loc = NULL,
         help.db = getOption("help.db"),
         verbose = getOption("verbose"),
         rebuild = FALSE)
{
    sQuote <- function(s) paste("`", s, "'", sep = "")

    ### Argument handling.
    TABLE <- c("name", "alias", "title", "keyword")
    if(!missing(pattern)) {
        if(!is.character(pattern) || (length(pattern) > 1))
            stop(paste(sQuote("pattern"),
                       "must be a single character string"))
        i <- pmatch(fields, TABLE)
        if(any(is.na(i)))
            stop("incorrect field specification")
        else
            fields <- TABLE[i]
    } else if(!missing(apropos)) {
        if(!is.character(apropos) || (length(apropos) > 1))
            stop(paste(sQuote("apropos"),
                       "must be a single character string"))
        else {
            pattern <- apropos
            fields <- c("alias", "title")
        }
    } else if(!missing(keyword)) {
        if(!is.character(keyword) || (length(keyword) > 1))
            stop(paste(sQuote("keyword"),
                       "must be a single character string"))
        else {
            pattern <- keyword
            fields <- "keyword"
        }
    } else if(!missing(whatis)) {
        if(!is.character(whatis) || (length(whatis) > 1))
            stop(paste(sQuote("whatis"),
                       "must be a single character string"))
        else {
            pattern <- whatis
            fields <- "alias"
        }
    } else {
        stop("don't know what to search")
    }

    if(is.null(lib.loc))
        lib.loc <- .libPaths()

    ## <FIXME>
    ## Currently, the information used for help.search in stored in
    ## package-level CONTENTS files in DCF format.  As it is expensive
    ## to read this information into R we use a global file cache for
    ## this information if possible.  This is wrong because multiple
    ## processes or threads use the same cache (no locking!), and we
    ## should really save the information on a package or library level,
    ## preferably already at package install time.  Argh ...
    ## </FIXME>

    ### Set up the help db.
    if(is.null(help.db) || !file.exists(help.db))
        rebuild <- TRUE
    if(!rebuild) {
        ## Try using the saved help db.
        load(file = help.db)
        ## Need to find out whether this has the info we need.
        ## Note that when looking for packages in libraries we always
        ## use the first location found.  Hence if the library search
        ## path changes we might find different versions of a package.
        ## Thus we need to rebuild the help db in case the specified
        ## library path is different from the one used when building the
        ## help db (stored as its "LibPaths" attribute).
        if(!identical(lib.loc, attr(db, "LibPaths")))
            rebuild <- TRUE
        ## We also need to rebuild the help db in case an existing dir
        ## in the library path was modified more recently than the db,
        ## as packages might have been installed or removed.
        if(any(file.info(help.db)$mtime <
               file.info(lib.loc[file.exists(lib.loc)])$mtime))
            rebuild <- TRUE
    }
    if(rebuild) {
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
        ## If we cannot save the help db only use the given packages.
        packagesInHelpDB <- if(!is.null(package) && !save.db)
            package
        else
            .packages(all.available = TRUE, lib.loc = lib.loc)
        for(p in packagesInHelpDB) {
            if(verbose)
                cat("", p, if((np <- np + 1)%% 5 == 0) "\n")
            path <- .find.package(p, lib.loc, quiet = TRUE)
            if(length(path) == 0)
                stop(paste("could not find package", sQuote(p)))
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
                    warning(paste("Empty", sQuote("CONTENTS"),
                                  "file of pkg", p, "in", lib))
                }
            }
        }
        if(verbose && (np %% 5 == 0)) cat("\n")
        colnames(db) <- c("Package", "LibPath", TABLE)
        ## Maybe save the help db
        if(save.db) {
            attr(db, "LibPaths") <- lib.loc
            save(db, file = dbfile)
            options(help.db = dbfile)
        }
    }

    ### Matching.
    if(verbose) cat("\nDatabase of dimension", dim(db))
    if(!is.null(package)) {
        ## Argument 'package' was given but we built a larger help db to
        ## save for future invocations.  Need to check that all given
        ## packages exist, and only search the given ones.
        posInHelpDB <- match(package, unique(db[, "Package"]),
                             nomatch = 0)
        if(any(posInHelpDB) == 0)
            stop(paste("could not find package",
                       sQuote(package[posInHelpDB == 0][1])))
        db <- db[db[, "Package"] %in% package, , drop = FALSE]
    }
    i <- NULL
    for(f in fields)
        i <- c(i, grep(pattern, db[, f], ignore.case = ignore.case))

    db <- db[sort(unique(i)), , drop = FALSE]
    if(verbose) cat(", matched", NROW(db), "entries.\n")

    ## Retval.
    y <- list(pattern = pattern,
              fields = fields,
              matches = db[, c("name", "title", "Package", "LibPath"),
              drop = FALSE])
    class(y) <- "hsearch"
    y
}

print.hsearch <-
function(x, ...)
{
    fields <- paste(x$fields, collapse = " or ")
    db <- x$matches
    if(NROW(db) > 0) {
        outFile <- tempfile()
        outConn <- file(outFile, open = "w")
        writeLines(paste("Help files with ", fields, " matching `",
                         x$pattern, "',\n",
                         "type `help(FOO, package = PKG)' to inspect ",
                         "entry `FOO(PKG) TITLE':",
                         "\n", sep = ""),
                   outConn)
        dbnam <- paste(db[ , "name"], "(", db[, "Package"], ")",
                       sep = "")
        dbtit <- paste(db[ , "title"], sep = "")
        writeLines(formatDL(dbnam, dbtit), outConn)
        close(outConn)
        file.show(outFile, delete.file = TRUE)
    } else {
        cat(paste("No help files found with ", fields, " matching `",
                  x$pattern, "'\n", sep = ""))
    }
}
