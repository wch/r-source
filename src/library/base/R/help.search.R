help.search <- function(what, fields = c("name", "title"),
                        apropos, keyword, whatis, ignore.case = TRUE,
                        packages = NULL, lib.loc = .lib.loc,
                        verbose = .Options$verbose) {
    TABLE <- c("name", "alias", "title", "keyword")
    if (!missing(what)) {
        if (!is.character(what) || (length(what) > 1))
            stop("`what' must be a single character string")
        i <- pmatch(fields, TABLE)
        if (any(is.na(i)))
            stop("incorrect field specification")
        else
            fields <- TABLE[i]
    } else if (!missing(apropos)) {
        if (!is.character(apropos) || (length(apropos) > 1))
            stop("`apropos' must be a single character string")
        else {
            what <- apropos
            fields <- c("name", "title")
        }
    } else if (!missing(keyword)) {
        if (!is.character(keyword) || (length(keyword) > 1))
            stop("`keyword' must be a single character string")
        else {
            what <- keyword
            fields <- "keyword"
        }
    } else if (!missing(whatis)) {
        if (!is.character(whatis) || (length(whatis) > 1))
            stop("`whatis' must be a single character string")
        else {
            what <- whatis
            fields <- "name"
        }
    } else {
        stop("don't know what to search")
    }

    if(verbose) RepC <- function(n, ch = " ") paste(rep(ch, n), collapse="")
    db <- NULL
    for (lib in lib.loc) {
        if(verbose) {
            cat("\nLIBRARY ",lib,"\n",
                RepC(8), RepC(ch="=", nchar(lib)),"\n",sep="")
            np <- 0
        }
        pkgs <- {
            if(is.null(packages)) .packages(all.available = TRUE, lib.loc = lib)
            else packages }
        for (p in pkgs) {
            if(verbose) cat("", p, if((np <- np + 1)%% 5 == 0) "\n")
            cfile <- system.file("CONTENTS", pkg = p, lib = lib)
            if (cfile != "") {
                ctext <- scan("", file = cfile, sep = "\n", quiet = TRUE)
                if(length(ctext)) {
                    ctext <- parse.dcf(ctext,
                                       fields = c("Entry", "Aliases",
                                       "Description", "Keywords"))
                    nr <- NROW(ctext)
                    db <- rbind(db,
                                cbind(rep(p, nr), rep(lib, nr), ctext))
                } else
                warning(paste("Empty \"CONTENTS\" file of pkg", p, "in", lib))
            }
        }
        if(verbose && np %% 5) cat("\n")
    }
    colnames(db) <- c("pkg", "lib", TABLE)

    if(verbose) cat("\nDatabase of dimension", dim(db))
    i <- NULL
    for (f in fields)
        i <- c(i, grep(what, db[, f], ignore.case = ignore.case))

    db <- db[sort(unique(i)), , drop = FALSE]
    if(verbose) cat(", matched", NROW(db),"entries.\n")

    fields <- paste(fields, collapse = " or ")
    if (NROW(db) > 0) {
        FILE <- tempfile()
        cat(paste("Objects with ", fields, " matching `", what, "':\n\n",
                  sep = ""),
            file = FILE)
        dbnam <- paste(db[ , "name"], "(", db[, "pkg"], ")", sep = "")
        dbtit <- paste(db[ , "title"], sep = "")
        cat(paste(format(dbnam), dbtit, sep = "   "),
            sep = "\n", file = FILE, append = TRUE)
        file.show(FILE, delete.file = TRUE)
    } else {
        cat(paste("No objects found with ", fields, " matching `", what,
                  "'\n", sep = ""))
    }

    return(invisible())
}
