help.search <- function(topic, fields = c("name", "title"),
                        apropos, keyword, whatis, ignore.case = TRUE,
                        packages = NULL, lib.loc = .lib.loc,
                        help.db = .Options$help.db,
                        verbose = .Options$verbose,
                        rebuild = FALSE) {
    TABLE <- c("name", "alias", "title", "keyword")
    if (!missing(topic)) {
        if (!is.character(topic) || (length(topic) > 1))
            stop("`topic' must be a single character string")
        i <- pmatch(fields, TABLE)
        if (any(is.na(i)))
            stop("incorrect field specification")
        else
            fields <- TABLE[i]
    } else if (!missing(apropos)) {
        if (!is.character(apropos) || (length(apropos) > 1))
            stop("`apropos' must be a single character string")
        else {
            topic <- apropos
            fields <- c("name", "title")
        }
    } else if (!missing(keyword)) {
        if (!is.character(keyword) || (length(keyword) > 1))
            stop("`keyword' must be a single character string")
        else {
            topic <- keyword
            fields <- "keyword"
        }
    } else if (!missing(whatis)) {
        if (!is.character(whatis) || (length(whatis) > 1))
            stop("`whatis' must be a single character string")
        else {
            topic <- whatis
            fields <- "name"
        }
    } else {
        stop("don't know what to search")
    }

    ## Set up the help db
    if(rebuild || is.null(help.db) || !file.exists(help.db)) {
        ## Check whether we can save the help db lateron
        save.db <- FALSE
        dir <- switch(.Platform$OS.type,
                      "windows" = getenv("R_USER"),
                      "unix" = getenv("HOME"),
                      "")
        if(nchar(dir) == 0) dir <- getwd()
        dir <- file.path(dir, ".R")
        dbfile <- file.path(dir, "help.db")
        if((file.exists(dir) || dir.create(dir)) && (unlink(dbfile) == 0))
            save.db <- TRUE
        ## Create the help db
        if(verbose)
            RepC <- function(n, ch = " ") paste(rep(ch, n), collapse="")
        db <- NULL
        for (lib in lib.loc) {
            if(verbose) {
                cat("\nLIBRARY ", lib, "\n",
                    RepC(8), RepC(ch = "=", nchar(lib)), "\n", sep = "")
                np <- 0
            }
            pkgs <- {
                if(is.null(packages))
                    .packages(all.available = TRUE, lib.loc = lib)
                else packages
            }
            for (p in pkgs) {
                if(verbose)
                    cat("", p, if((np <- np + 1)%% 5 == 0) "\n")
                cfile <- system.file("CONTENTS", pkg = p, lib = lib)
                if(cfile != "") {
                    ctext <- scan("", file = cfile, sep = "\n",
                                  quiet = TRUE)
                    if(length(ctext) > 0) {
                        ctext <- parse.dcf(ctext,
                                           fields = c("Entry", "Aliases",
                                           "Description", "Keywords"))
                        nr <- NROW(ctext)
                        db <- rbind(db,
                                    cbind(rep(p, nr), rep(lib, nr), ctext))
                    } else {
                        warning(paste("Empty `CONTENTS' file of pkg", p,
                                      "in", lib))
                    }
                }
            }
            if(verbose && np %% 5) cat("\n")
        }
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
        i <- c(i, grep(topic, db[, f], ignore.case = ignore.case))

    db <- db[sort(unique(i)), , drop = FALSE]
    if(verbose) cat(", matched", NROW(db),"entries.\n")

    ## Output
    fields <- paste(fields, collapse = " or ")
    if (NROW(db) > 0) {
        FILE <- tempfile()
        cat(paste("Objects with ", fields, " matching `", topic,
                  "':\n\n", sep = ""),
            file = FILE)
        dbnam <- paste(db[ , "name"], "(", db[, "pkg"], ")", sep = "")
        dbtit <- paste(db[ , "title"], sep = "")
        cat(paste(format(dbnam), dbtit, sep = "   "),
            sep = "\n", file = FILE, append = TRUE)
        file.show(FILE, delete.file = TRUE)
    } else {
        cat(paste("No objects found with ", fields, " matching `",
                  topic, "'\n", sep = ""))
    }

    return(invisible())
}
