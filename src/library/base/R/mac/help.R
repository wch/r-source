help <-
    function(topic, offline = FALSE, package = .packages(),
             lib.loc = .lib.loc, verbose = getOption("verbose"),
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
        INDICES <- .find.package(package, lib.loc, missing(lib.loc),
                                 quiet = FALSE)
        file <- index.search(topic, INDICES, "AnIndex", type)
        if (length(file) && file != "") {
            if (verbose)
                cat("\t\t\t\t\t\tHelp file name `", sub(".*/", "", file),
                    ".Rd'\n", sep = "")
            if (!offline) {
                if(htmlhelp) {
                    if(file.exists(file)) {
                        .Internal(show.help.item(file, 1, ""))
                        cat("help() for `", topic, "' is shown in browser\n",
                            sep="")
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
                              header = paste("Help for `", topic, "'", sep=""),
                              delete.file = (zfile!=file), pager = pager)
                else
                    stop(paste("The help file for `", topic, "' is missing",
                               sep = ""))
                return(invisible())
            }
            else {
                stop("offline printing is not support on the Macintosh")
                ## experimental code
                zfile <- zip.file.extract(file, "Rhelp.zip")
                if(zfile != file) on.exit(unlink(zfile))
                ## end of experimental code
                if(file.exists(zfile)) {
                    FILE <- "Rdoc"
                    tFILE <- paste(FILE, ".tex", sep="")
                    cat("\\documentclass[",
                        getOption("papersize"),
                        "paper]{article}",
                        "\n",
                        "\\usepackage[",
                        if(nchar(opt <- Sys.getenv("R_RD4DVI"))) opt else "ae",
                        "]{Rd}",
                        "\n",
                        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
                        "\\begin{document}\n",
                        file = tFILE, sep = "")
                    file.append(tFILE, zfile)
                    cat("\\end{document}\n", file = tFILE, append = TRUE)
                    cmd <- paste('"',
                                 paste(R.home(), "bin", "helpPRINT", sep="/"),
                                 '"', sep="")
                    texpath <- gsub("\\\\", "/",
                                    file.path(R.home(), "doc", "manual"))
                    system(paste(cmd, FILE, topic, texpath), wait = FALSE)
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
                packages <- .packages(all.available = TRUE, lib.loc = lib.loc)
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
        library(help = package, lib = lib.loc, character.only = TRUE)
    else if (!missing(lib.loc))
        library(lib = lib.loc)
    else help("help", package = "base", lib.loc = .Library)
}
