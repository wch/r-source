index.search <- function(topic, path, file, html = FALSE)
    .Internal(index.search(topic, path, file, .Platform$file.sep, html))

"help" <-
function (topic, offline = FALSE, package = c(.packages(), .Autoloaded),
          lib.loc = .lib.loc, verbose = .Options$verbose,
          htmlhelp = .Options$htmlhelp)
{
    ## && !is.character(package))
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
        else if (!is.na(match(topic, c("<", ">", "<=", ">=",
            "==", "!="))))
            topic <- "Comparison"
        else if (!is.na(match(topic, c("[", "[[", "$"))))
            topic <- "Extract"
        else if (!is.na(match(topic, c("&", "&&", "|", "||",
            "!"))))
            topic <- "Logic"
        else if (!is.na(match(topic, c("%*%"))))
            topic <- "matmult"
        topic <- gsub("\\[", "\\\\[", topic)
        INDICES <- system.file(pkg=package, lib=lib.loc)
        file <- index.search(topic, INDICES, "AnIndex")
        if (file == "") {
            # try data .doc -- this is OUTDATED
            file <- system.file(file.path("data", paste(topic, ".doc",
                sep = "")), pkg = package, lib = lib.loc)
        }
        if (length(file) && file != "") {
            if (verbose)
                cat("\t\t\t\t\t\tHelp file name `", sub(".*/",
                  "", file), ".Rd'\n", sep = "")
            if (!offline) {
                if (!is.null(htmlhelp) && htmlhelp) {
                  ## replace the last occurence of /help/ in the
                  ## path with /html/, then append .html
                  file <- gsub("/help/([^/]*)$", "/html/\\1",
                    file)
                  file <- paste("file:", file, ".html", sep = "")
                  if (is.null(.Options$browser))
                    stop("options(\"browser\") not set")
                  browser <- .Options$browser
                  system(paste(browser, " -remote \"openURL(",
                    file, ")\" 2>/dev/null || ", browser, " ",
                    file, " &", sep = ""))
                  cat("help() for", topic, " is shown in browser",
                    browser, "...\n")
                }
                else file.show(file)
            }
            else {
                FILE <- tempfile()
                on.exit(unlink(FILE))
                cat("\\documentclass[", .Options$papersize, "paper]{article}\n",
                  file = FILE, sep = "")
                file.append(FILE,
                            file.path(R.home(), "doc", "manual", "Rd.sty"))
                cat("\\InputIfFileExists{Rhelp.cfg}{}{}\n\\begin{document}\n",
                  file = FILE, append = TRUE)
                file.append(FILE, paste(sub("help/", "latex/",
                  file), ".tex", sep = ""))
                cat("\\end{document}\n", file = FILE, append = TRUE)
                system(paste(file.path(R.home(), "bin", "help"),
                             "PRINT", FILE, topic,
                             .Options$latexcmd, .Options$dvipscmd)
                       )
                return()
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
