index.search <- function(topic, path, file="AnIndex", type="help")
    .Internal(index.search(topic, path, file, .Platform$file.sep, type))

help <-
    function(topic, offline = FALSE, package = c(.packages(), .Autoloaded),
             lib.loc = .lib.loc, verbose = .Options$verbose,
             htmlhelp = .Options$htmlhelp,
             winhelp = .Options$winhelp)
{
    htmlhelp <- is.logical(htmlhelp) && htmlhelp
    winhelp <- is.logical(winhelp) && winhelp
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
            topic<- "matmult"
        type <- "help"
        if(offline) type <- "latex"
        else if (htmlhelp) type <- "html"
        topic <- gsub("\\[","\\\\[", topic)
        INDICES <- system.file(pkg = package, lib = lib.loc)
        file <- index.search(topic, INDICES, "AnIndex", type)
        if (file == "") {
            # try data .doc -- this is OUTDATED
            file <- system.file(file.path("data", paste(topic, ".doc",
                sep = "")), pkg = package, lib = lib.loc)
        }
        if (length(file) && file != "") {
            if (verbose)
                cat("\t\t\t\t\t\tHelp file name `", sub(".*/", "", file),
                    ".Rd'\n", sep = "")
            if (!offline) {
                if(htmlhelp) {
                    file <- gsub("/", "\\\\", file)
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
                if(winhelp) {
                    wfile <- sub("/help/([^/]*)$", "", file)
                    thispkg <- sub(".*/([^/]*)$", "\\1", wfile)
                    hlpfile <- paste(wfile, "/winhlp/", thispkg, ".hlp",
                                     sep = "")
                    hlpfile <- gsub("/", "\\\\", hlpfile)
                    if(verbose) print(hlpfile)
                    if(file.exists(hlpfile)) {
                        .Internal(show.help.item(topic, 2, hlpfile))
                        if(verbose)
                            cat("help() for `", topic, "' is shown in WinHelp\n",
                                sep="")
                        return(invisible())
                    } else {
                       if(verbose)
                           cat("No `", thispkg, ".hlp' is available\n", sep="")
                    }
                }
                ## experimental code
                zfile <- zip.file.extract(file, "Rhelp.zip")
                ## end of experimental code
                if(file.exists(zfile))
                    file.show(zfile,
                              header = paste("Help for `", topic, "'", sep=""),
                              delete.file = (zfile!=file))
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
                    FILE <- "Rdoc"
                    tFILE <- paste(FILE, ".tex", sep="")
                    cat("\\documentclass[", .Options$papersize, "paper]{article}\n",
                        file = tFILE, sep = "")
                    file.append(tFILE,
                                file.path(R.home(), "doc", "manual", "Rd.sty"))
                    cat("\\InputIfFileExists{Rhelp.cfg}{}{}\n\\begin{document}\n",
                        file = tFILE, append = TRUE)
                    file.append(tFILE, zfile)
                    cat("\\end{document}\n", file = tFILE, append = TRUE)
                    system(paste(paste(R.home(), "bin", "helpPRINT", sep="/"),
                                 FILE, topic), wait=F)
                    return(invisible())
                }
                else
                    stop(paste("No offline documentation for", topic, "is available"))
            }
        }
        else stop(paste("No documentation for `", topic, "'",
            sep = ""))
    }
    else if (!missing(package))
        library(help = package, lib = lib.loc, character.only = TRUE)
    else if (!missing(lib.loc))
        library(lib = lib.loc)
    else help("help", package = "base", lib.loc = .Library)
}
