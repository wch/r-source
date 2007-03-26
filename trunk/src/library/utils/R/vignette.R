vignette <-
function(topic, package = NULL, lib.loc = NULL)
{
    if(is.null(package))
        package <- .packages(all.available = TRUE, lib.loc)
    paths <- .find.package(package, lib.loc)

    ## Find the directories with a 'doc' subdirectory *possibly*
    ## containing vignettes.

    paths <- paths[file_test("-d", file.path(paths, "doc"))]

    vignettes <-
        lapply(paths,
               function(dir) {
                   tools::list_files_with_type(file.path(dir, "doc"),
                                               "vignette")
               })

    if(!missing(topic)) {
        topic <- topic[1]               # Just making sure ...
        vignettes <- as.character(unlist(vignettes))
        vidx <- (tools::file_path_sans_ext(basename(vignettes)) == topic)
        if(any(vidx)) {

            pdf <- sub("\\.[[:alpha:]]+$", ".pdf", vignettes)
            pidx <- file_test("-f", pdf)
            ok <- vidx & pidx

            if(any(ok)){
                idx <- min(which(ok))
                if(sum(ok)>1){
                    ## <FIXME>
                    ## Should really offer a menu to select from.
                    warning(gettextf("vignette '%s' found more than once,\nusing the one found in '%s'", topic, dirname(pdf[idx])),
                            call. = FALSE, domain = NA)
                    ## </FIXME>
                }

                z <- list(file=vignettes[idx], pdf=pdf[idx])
            }
            else{
                z <- list(file=vignettes[vidx][1], pdf=character(0))
            }
            z$topic <- topic
            class(z) <- "vignette"
            return(z)
        }
        else
            warning(gettextf("vignette '%s' *not* found", topic),
                    call. = FALSE, domain = NA)
    }

    if(missing(topic)) {
        ## List all possible vignettes.

        vDB <- matrix(character(0), nr = 0, nc = 4)
        colnames(vDB) <- c("Dir", "File", "Title", "PDF")

        for(db in vignettes[sapply(vignettes, length) > 0]) {
            dir <- dirname(dirname(db[1]))
            entries <- NULL
            ## Check for new-style 'Meta/vignette.rds' ...
            if(file.exists(INDEX <-
                           file.path(dir, "Meta", "vignette.rds")))
                entries <- .readRDS(INDEX)
            if(NROW(entries) > 0)
                vDB <- rbind(vDB,
                             cbind(dir,
                                   entries$File,
                                   entries$Title,
                                   entries$PDF))
        }

        ## Now compute info on available PDFs ...
        title <- if(NROW(vDB) > 0) {
            paste(vDB[, "Title"],
                  paste(rep.int("(source", NROW(vDB)),
                        ifelse(vDB[, "PDF"] != "", ", pdf", ""),
                        ")",
                        sep = ""))
        }
        else
            character()
        ## ... and rewrite into the form used by packageIQR.
        db <- cbind(Package = basename(vDB[, "Dir"]),
                    LibPath = dirname(vDB[, "Dir"]),
                    Item = tools::file_path_sans_ext(basename(vDB[, "File"])),
                    Title = title)

        y <- list(type = "vignette", title = "Vignettes", header = NULL,
                  results = db, footer = NULL)
        class(y) <- "packageIQR"
        return(y)
    }
}

print.vignette <- function(x, ...){

    if(length(x$pdf)){
        ## <FIXME>
        ## Should really abstract this into a BioC style
        ## openPDF() along the lines of browseURL() ...
        if(.Platform$OS.type == "windows")
            shell.exec(x$pdf)
        else
            system(paste(shQuote(getOption("pdfviewer")), shQuote(x$pdf)),
                   wait = FALSE)
        ## </FIXME>
    } else {
        warning(gettextf("vignette '%s' has no PDF", x$topic),
                call. = FALSE, domain = NA)
    }
}

edit.vignette <- function(name, ...){

    f <- paste(tempfile(name$topic), ".R", sep="")
    Stangle(name$file, output=f, quiet=TRUE)
    file.edit(file=f, ...)
}
