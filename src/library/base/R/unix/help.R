### NOTE: This is for Unix only (cf. ../{mac,windows}/help.R)

help <- function(topic, offline = FALSE, package = .packages(),
                 lib.loc = NULL, verbose = getOption("verbose"),
                 try.all.packages = getOption("help.try.all.packages"),
                 htmlhelp = getOption("htmlhelp"),
                 pager = getOption("pager"))
{
    htmlhelp <- is.logical(htmlhelp) && htmlhelp
    if (!missing(package))
        if (is.name(y <- substitute(package)))
            package <- as.character(y)
    if (!missing(topic)) {
        topic <- substitute(topic)
        if (is.name(topic))
            topic <- as.character(topic)
        else if (!is.character(topic))
            stop("Unimplemented help feature")
        # for cmd/help ..
        if (!is.na(match(topic, c("+", "-", "*", "/", "^", "%%"))))
            topic <- "Arithmetic"
        else if (!is.na(match(topic, c("<", ">", "<=", ">=", "==", "!="))))
            topic <- "Comparison"
        else if (!is.na(match(topic, c("[", "[[", "$"))))
            topic <- "Extract"
        else if (!is.na(match(topic, c("&", "&&", "|", "||", "!"))))
            topic <- "Logic"
        else if (!is.na(match(topic, c("%*%"))))
            topic <- "matmult"
        type <- if(offline) "latex" else if (htmlhelp) "html" else "help"
        INDICES <- .find.package(package, lib.loc, verbose = verbose)
        file <- index.search(topic, INDICES, "AnIndex", type)
        if (length(file) && file != "") {
            if (verbose)
                cat("\t\t\t\t\t\tHelp file name `", sub(".*/", "", file),
                    ".Rd'\n", sep = "")
            if (!offline) {
                if (htmlhelp) {
                    if(file.exists(file)) {
                        ofile <- file
                        base.pos <- match("package:base", search())
                        ## We need to use the version in per-session dir
                        ## if we can.
                        lnkfile <-
                            file.path(tempdir(), ".R",
                                      "library", package, "html",
                                      paste(topic, "html", sep="."))
                        if (any(ex <- file.exists(lnkfile))) {
                            lnkfile <- lnkfile[ex]
                            file <- lnkfile[1] # could be more than one
                        }
                        if (file == ofile) {
                            warning("Using non-linked HTML file: style sheet and hyperlinks may be incorrect")
                        }
                        file <- paste("file://", file, sep = "")
                        if(is.null(browser <- getOption("browser")))
                            stop("options(\"browser\") not set")
                        browseURL(file)
                        cat("help() for",topic, " is shown in browser",browser,
                            "...\nUse\t help(",topic,", htmlhelp=FALSE)\nor\t",
                            "options(htmlhelp = FALSE)\nto revert.\n")
                        return(invisible())
                    } else {
                        if(verbose)
                            cat("no HTML help for `", topic,
                                "' is available\n", sep = "")
                        file <- index.search(topic, INDICES, "AnIndex", "help")
                    }
                }
                ## experimental code
                zfile <- zip.file.extract(file, "Rhelp.zip")
                ## end of experimental code
                if(file.exists(zfile))
                    file.show(zfile,
                              title = paste("R Help on `", topic, "'", sep=""),
                              delete.file = (zfile!=file),
                              pager = pager)
                else
                    stop(paste("The help file for `", topic, "' is missing",
                               sep = ""))
                return(invisible())
            }
            else {
                ## experimental code
                zfile <- zip.file.extract(file, "Rhelp.zip")
                if(zfile != file) on.exit(unlink(zfile))
                ## end of experimental code
                if(file.exists(zfile)) {
                    FILE <- tempfile()
                    on.exit(unlink(FILE))
                    cat("\\documentclass[",
                        getOption("papersize"),
                        "paper]{article}",
                        "\n",
                        "\\usepackage[",
                        Sys.getenv("R_RD4DVI"),
                        "]{Rd}",
                        "\n",
                        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
                        "\\begin{document}\n",
                        file = FILE, sep = "")
                    file.append(FILE, zfile)
                    cat("\\end{document}\n",
                        file = FILE, append = TRUE)
                    ## <NOTE>
                    ## We now have help-print.sh in share/sh but we do
                    ## not use the .Script mechanism because we play
                    ## with the TEXINPUTS environment variable and not
                    ## all systems can be assumed to support Sys.putenv().
                    system(paste(paste("TEXINPUTS=",
                                       file.path(R.home(), "share",
                                                 "texmf"),
                                       ":",
                                       "$TEXINPUTS",
                                       sep = ""),
                                 "/bin/sh",
                                 file.path(R.home(), "share", "sh",
                                           "help-print.sh"),
                                 FILE,
                                 topic,
                                 getOption("latexcmd"),
                                 getOption("dvipscmd")))
                    ## </NOTE>
                    return(invisible())
                }
                else
                    stop(paste("No offline documentation for", topic,
                               "is available"))
            }
        }
        else {
            if(is.null(try.all.packages) || !is.logical(try.all.packages))
                try.all.packages <- FALSE
            if(try.all.packages && missing(package) && missing(lib.loc)) {
                ## try all the remaining packages
                lib.loc <- .libPaths()
                packages <- .packages(all.available = TRUE,
                                      lib.loc = lib.loc)
                packages <- packages[is.na(match(packages, .packages()))]
                pkgs <- libs <- character(0)
                for (lib in lib.loc)
                    for (pkg in packages) {
                        INDEX <- system.file(package = pkg, lib.loc = lib)
                        file <- index.search(topic, INDEX, "AnIndex", "help")
                        if(length(file) && file != "") {
                            pkgs <- c(pkgs, pkg)
                            libs <- c(libs, lib)
                        }
                    }
                if(length(pkgs) == 1) {
                    cat("  topic `", topic, "' is not in any loaded package\n",
                        "  but can be found in package `", pkgs,
                        "' in library `", libs, "'\n", sep = "")
                } else if(length(pkgs) > 1) {
                    cat("  topic `", topic, "' is not in any loaded package\n",
                        "  but can be found in the following packages:\n\n",
                        sep="")
                    A <- cbind(package=pkgs, library=libs)
                    rownames(A) <- 1:nrow(A)
                    print(A, quote=FALSE)
                } else {
                    stop(paste("No documentation for `", topic,
                               "' in specified packages and libraries:\n",
                               "  you could try `help.search(\"", topic,
                               "\")'",
                               sep = ""))
                }
            } else {
                    stop(paste("No documentation for `", topic,
                               "' in specified packages and libraries:\n",
                               "  you could try `help.search(\"", topic,
                               "\")'",
                               sep = ""))
            }
        }
    }
    else if (!missing(package))
        library(help = package, lib.loc = lib.loc, character.only = TRUE)
    else if (!missing(lib.loc))
        library(lib.loc = lib.loc)
    else help("help", package = "base", lib.loc = .Library)
}
