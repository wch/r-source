index.search <- function(topic, path, file, html = FALSE)
    .Internal(index.search(topic, path, file, .Platform$file.sep, html))

"help" <-
function (topic, offline = FALSE, package = c(.packages(), .Autoloaded),
          lib.loc = .lib.loc, verbose = .Options$verbose,
          htmlhelp = .Options$htmlhelp)
{
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
        topic <- gsub("\\[","\\\\[", topic)
        INDICES <- system.file(pkg=package, lib=lib.loc)
        file <- index.search(topic, INDICES, "AnIndex")
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
                if (!is.null(htmlhelp) && htmlhelp) {
                    ## replace the last occurence of /help/ in the
                    ## path with /html/, then append .html
                    hfile <- sub("/help/([^/]*)$", "/html/\\1", file)
                    hfile <- paste(hfile, ".html", sep = "")
                    if(file.exists(hfile)) {
                        hfile <- paste("file:", hfile, sep="")
                        if (is.null(.Options$browser))
                            stop("options(\"browser\") not set")
                        browser <- .Options$browser
                        system(paste(browser, " -remote \"openURL(",
                                     hfile, ")\" 2>/dev/null || ", browser, " ",
                                     hfile, " &", sep = ""))
                        cat("help() for", topic, " is shown in browser",
                            browser, "...\n")
                        return(invisible())
                    } else
                    if(verbose)
                        cat("no HTML help for `", topic, "' is available\n",
                            sep = "")
                }
                ## experimental code
                zfile <- zip.file.extract(file, "Rhelp.zip")
                ## end of experimental code
                file.show(zfile, delete.file = (zfile!=file))
                return(invisible())
            }
            else {
                ltxfile <- paste(sub("help/", "latex/", file), ".tex", sep = "")
                ## experimental code
                zfile <- zip.file.extract(ltxfile, "Rhelp.zip")
                if(zfile != ltxfile) on.exit(unlink(ltxfile))
                ## end of experimental code
                if(file.exists(zfile)) {
                    FILE <- tempfile()
                    on.exit(unlink(FILE))
                    cat("\\documentclass[", .Options$papersize, "paper]{article}\n",
                        file = FILE, sep = "")
                    file.append(FILE,
                                file.path(R.home(), "doc", "manual", "Rd.sty"))
                    cat("\\InputIfFileExists{Rhelp.cfg}{}{}\n\\begin{document}\n",
                        file = FILE, append = TRUE)
                    file.append(FILE, zfile)
                    cat("\\end{document}\n", file = FILE, append = TRUE)
                    system(paste(file.path(R.home(), "bin", "help"),
                                 "PRINT", FILE, topic,
                                 .Options$latexcmd, .Options$dvipscmd)
                           )
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
