### <NOTE>
###  This is for Unix only (cf. ../windows/help.R)
### </NOTE>

help <-
function(topic, offline = FALSE, package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"),
         try.all.packages = getOption("help.try.all.packages"),
         htmlhelp = getOption("htmlhelp"),
         pager = getOption("pager"))
{
    if(!missing(package))
        if(is.name(y <- substitute(package)))
            package <- as.character(y)
    
    ## If no topic was given ...
    if(missing(topic)) {
        if(!missing(package))           # "Help" on package.
            return(library(help = package, lib.loc = lib.loc,
                           character.only = TRUE))
        if(!missing(lib.loc))           # "Help" on library.
            return(library(lib.loc = lib.loc))
        return(help("help", package = "utils", lib.loc = .Library))
    }

    ischar <- try(is.character(topic), silent = TRUE)
    if(inherits(ischar, "try-error")) ischar <- FALSE
    if(!ischar) topic <- deparse(substitute(topic))

    type <- if(offline)
        "latex"
    else if(is.logical(htmlhelp) && !is.na(htmlhelp) && htmlhelp)
        "html"
    else
        "help"

    ## Note that index.search() (currently?) only returns the first
    ## match for the given sequence of indices, and returns the empty
    ## string in case of no match.
    paths <- sapply(.find.package(package, lib.loc, verbose = verbose),
                    function(p) index.search(topic, p, "AnIndex", type))
    paths <- paths[paths != ""]

    tried_all_packages <- FALSE
    if(!length(paths)
       && is.logical(try.all.packages) && !is.na(try.all.packages)
       && try.all.packages && missing(package) && missing(lib.loc)) {
        ## Try all the remaining packages.
        lib.loc <- .libPaths()
        packages <- .packages(all.available = TRUE, lib.loc = lib.loc)
        packages <- packages[is.na(match(packages, .packages()))]
        for(lib in lib.loc) {
            ## <FIXME>
            ## Why does this loop over packages *inside* the loop
            ## over libraries?
            for(pkg in packages) {
                dir <- system.file(package = pkg, lib.loc = lib)
                paths <- c(paths,
                           index.search(topic, dir, "AnIndex", "help"))
            }
            ## </FIXME>
        }
        paths <- paths[paths != ""]
        tried_all_packages <- TRUE
    }

    attributes(paths) <-
        list(call = match.call(), pager = pager, topic = topic,
             tried_all_packages = tried_all_packages, type = type)
    class(paths) <- "help_files_with_topic"
    paths
}

print.help_files_with_topic <-
function(x, ...)
{
    topic <- attr(x, "topic")
    paths <- as.character(x)
    if(!length(paths)) {
        writeLines(c(paste("No documentation for", sQuote(topic),
                           "in specified packages and libraries:"),
                     paste("you could try",
                           sQuote(paste("help.search(",
                                        dQuote(topic), ")",
                                        sep = "")))))
        return(invisible(x))
    }
    if(attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- paste("Help for topic", sQuote(topic),
                     "is not in any loaded package but can be found",
                     "in the following packages:")
        writeLines(c(strwrap(msg), "",
                     paste(" ",
                           formatDL(c("Package", basename(paths)),
                                    c("Library", dirname(paths)),
                                    indent = 22))))
    }
    else {
        if(length(paths) > 1) {
            file <- paths[1]
            msg <- paste("Help on topic", sQuote(topic),
                         "was found in the following packages:")
            paths <- dirname(dirname(paths))
            writeLines(c(strwrap(msg), "",
                         paste(" ",
                               formatDL(c("Package", basename(paths)),
                                        c("Library", dirname(paths)),
                                        indent = 22)),
                         "\nUsing the first match ..."))
        }
        else
            file <- paths
        type <- attr(x, "type")
        if(type == "html") {
            if(file.exists(file))
                .show_help_on_topic_as_HTML(file, topic)
            else
                stop(paste("No HTML help for ", sQuote(topic),
                           " is available:\n",
                           "corresponding file is missing.",
                           sep = ""))
        }
        else if(type == "help") {
            zfile <- zip.file.extract(file, "Rhelp.zip")
            if(file.exists(zfile))
                file.show(zfile,
                          title = paste("R Help on", sQuote(topic)),
                          delete.file = (zfile != file),
                          pager = attr(x, "pager"))
            else
                stop(paste("No text help for", sQuote(topic),
                           " is available:\n",
                           "corresponding file is missing.",
                           sep = ""))
        }
        else if(type == "latex") {
            zfile <- zip.file.extract(file, "Rhelp.zip")
            if(zfile != file) on.exit(unlink(zfile))
            if(file.exists(zfile))
                .show_help_on_topic_offline(zfile, topic)
            else
                stop(paste("No offline help for ", sQuote(topic),
                           " is available:\n",
                           "corresponding file is missing.",
                           sep = ""))
        }
    }
                   
    invisible(x)
}

.show_help_on_topic_as_HTML <-
function(file, topic)
{
    ofile <- file
    ## We need to use the version in per-session dir if we can.
    third_base_name <-
        file.path(basename(dirname(dirname(file))),
                  basename(dirname(file)),
                  basename(file))
    ## (Ouch.)
    lnkfile <-
        file.path(tempdir(), ".R", "library", third_base_name)
    if(any(ex <- file.exists(lnkfile))) {
        file <- lnkfile[ex][1]          # could be more than one
    }
    if(file == ofile) {
        msg <- paste("Using non-linked HTML file:",
                     "style sheet and hyperlinks may be incorrect")
        warning(paste(strwrap(msg), collapse = "\n"))
    }
    file <- paste("file://", file, sep = "")
    if(is.null(browser <- getOption("browser")))
        stop("options(\"browser\") not set")
    browseURL(file)
    writeLines(c(paste("Help for", sQuote(topic),
                       "is shown in browser", browser, "..."),
                 "Use",
                 paste("\thelp(", dQuote(topic), ", htmlhelp = FALSE)",
                       sep = ""),
                 "or\n\toptions(htmlhelp = FALSE)\nto revert."))
    return(invisible())
}

.show_help_on_topic_offline <-
function(file, topic)
{
    con <- tempfile()
    on.exit(unlink(con))
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
        file = con, sep = "")
    file.append(con, file)
    cat("\\end{document}\n",
        file = con, append = TRUE)
    ## <FIXME>
    ## We now have help-print.sh in share/sh but we do not use the
    ## .Script mechanism because we play with the TEXINPUTS environment
    ## variable and the code goes back to a time when not all systems
    ## could be assumed to support Sys.putenv().
    ## Seems that now we can---rewrite this along the lines of
    ## tools:::.install_package_vignettes().
    system(paste(paste("TEXINPUTS=",
                       file.path(R.home(), "share", "texmf"),
                       ":",
                       "$TEXINPUTS",
                       sep = ""),
                 "/bin/sh",
                 shQuote(file.path(R.home(), "share", "sh",
                                   "help-print.sh")),
                 con,
                 topic,
                 getOption("latexcmd"),
                 getOption("dvipscmd")))
    ## </FIXME>
    return(invisible())
}
