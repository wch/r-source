###-- NOTE: This is for  NON-Windows only  (cf. windows/help.R )

index.search <- function(topic, path, file="AnIndex", type="help")
    .Internal(index.search(topic, path, file, .Platform$file.sep, type))

help <-
    function (topic, offline = FALSE, package = .packages(),
              lib.loc = .lib.loc, verbose = getOption("verbose"),
              htmlhelp = getOption("htmlhelp"))
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
        INDICES <-
            if(missing(lib.loc)) .path.package(package)
            else system.file(pkg = package, lib = lib.loc)
#        INDICES <- system.file(pkg=package, lib=lib.loc)
        file <- index.search(topic, INDICES, "AnIndex", type)
        if (length(file) && file != "") {
            if (verbose)
                cat("\t\t\t\t\t\tHelp file name `", sub(".*/", "", file),
                    ".Rd'\n", sep = "")
            if (!offline) {
                if (htmlhelp) {
                    if(file.exists(file)) {
                        file <- paste("file:", file, sep="")
                        if (is.null(getOption("browser")))
                            stop("options(\"browser\") not set")
                        browser <- getOption("browser")
                        system(paste(browser, " -remote \"openURL(",
                                     file, ")\" 2>/dev/null || ", browser, " ",
                                     file, " &", sep = ""))
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
                              ##header= paste("Help for `", topic, "'", sep=""),
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
                    FILE <- tempfile()
                    on.exit(unlink(FILE))
                    cat("\\documentclass[",
                        getOption("papersize"),
                        "paper]{article}",
                        "\n",
                        "\\usepackage[",
                        getenv("R_RD4DVI"),
                        "]{Rd}",
                        "\n",
                        "\\InputIfFileExists{Rhelp.cfg}{}{}\n",
                        "\\begin{document}\n",
                        file = FILE, sep = "")
                    file.append(FILE, zfile)
                    cat("\\end{document}\n",
                        file = FILE, append = TRUE)
                    system(paste(paste("TEXINPUTS=",
                                       file.path(R.home(), "doc",
                                                 "manual"),
                                       ":",
                                       "$TEXINPUTS",
                                       sep = ""),
                                 file.path(R.home(), "bin", "help"),
                                 "PRINT",
                                 FILE,
                                 topic,
                                 getOption("latexcmd"),
                                 getOption("dvipscmd")))
                    return(invisible())
                }
                else
                    stop(paste("No offline documentation for", topic,
                               "is available"))
            }
        }
        else
            stop(paste("No documentation for `", topic, "'", sep = ""))
    }
    else if (!missing(package))
        library(help = package, lib = lib.loc, character.only = TRUE)
    else if (!missing(lib.loc))
        library(lib = lib.loc)
    else help("help", package = "base", lib.loc = .Library)
}
