help.search <- function(what, fields = c("name", "title"),
                        apropos, keyword, whatis, ignore.case = TRUE,
                        package = NULL, lib.loc = .lib.loc) {
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
    
    db <- NULL
    for (lib in lib.loc) {
        if (!is.null(package))
            pkgs <- package
        else
            pkgs <- .packages(all.available = TRUE, lib.loc = lib)
        for (p in pkgs) {
            cfile <- system.file("CONTENTS", pkg = p, lib = lib)
            if (cfile != "") {
                ctext <- scan("", file = cfile, sep = "\n", quiet =
                              TRUE)
                ctext <- parse.dcf(ctext,
                                   fields = c("Entry", "Aliases",
                                   "Description", "Keywords"))
                nr <- NROW(ctext)
                db <- rbind(db, cbind(rep(p, nr),
                                      rep(lib, nr),
                                      ctext))
            }
        }
    }
    colnames(db) <- c("pkg", "lib", TABLE)

    i <- NULL
    for (f in fields) {
        i <- c(i, grep(what, db[, f], ignore.case = ignore.case))
    }
    db <- db[sort(unique(i)), , drop = FALSE]
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
