help <-
function(topic, offline = FALSE, package = NULL, lib.loc = NULL,
         verbose = getOption("verbose"),
         try.all.packages = getOption("help.try.all.packages"),
         chmhelp = getOption("chmhelp"),
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
    else if(.Platform$OS.type == "windows" &&
            is.logical(chmhelp) && !is.na(chmhelp) && chmhelp)
        "chm"
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
    if (.Platform$GUI == "AQUA") {
        .Internal(aqua.custom.print("help-files", x))
	return(invisible(x))
    }
    topic <- attr(x, "topic")
    type <- attr(x, "type")
    paths <- as.character(x)
    if(!length(paths)) {
        writeLines(c(gettextf("No documentation for '%s' in specified packages and libraries:",
                              topic),
                     gettextf("you could try 'help.search(\"%s\")'",
                              topic)))
        return(invisible(x))
    }
    if(attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- gettextf("Help for topic '%s' is not in any loaded package but can be found in the following packages:",
                     topic)
        writeLines(c(strwrap(msg), "",
                     paste(" ",
                           formatDL(c(gettext("Package"),
                                      basename(paths)),
                                    c(gettext("Library"),
                                      dirname(paths)),
                                    indent = 22))))
    } else {
        if(length(paths) > 1) {
            file <- paths[1]
            p <- paths
            msg <- gettextf("Help on topic '%s' was found in the following packages:",
                            topic)
            paths <- dirname(dirname(paths))
            txt <- formatDL(c("Package", basename(paths)),
                            c("Library", dirname(paths)),
                            indent = 22)
            writeLines(c(strwrap(msg), "", paste(" ", txt), ""))
            if(interactive()) {
                fp <- file.path(paths, "Meta", "Rd.rds")
                tp <- basename(p)
                titles <- tp
                if(type == "html") tp <- tools::file_path_sans_ext(tp)
                for (i in seq(along = fp)) {
                    tmp <- try(.readRDS(fp[i]))
                    titles[i] <- if(inherits(tmp, "try-error"))
                        "unknown title" else
                    tmp[tools::file_path_sans_ext(tmp$File) == tp[i], "Title"]
                }
                txt <- paste(titles, " {", basename(paths), "}", sep="")
                ## FIXME: use html page for HTML help.
                res <- menu(txt, title = gettext("Choose one"),
                            graphics = getOption("menu.graphics"))
                if(res > 0) file <- p[res]
            } else {
                writeLines(gettext("\nUsing the first match ..."))
            }
        }
        else
            file <- paths

        if(type == "html") {
            if(file.exists(file))
                .show_help_on_topic_as_HTML(file, topic)
            else
                stop(gettextf("No HTML help for '%s' is available:\ncorresponding file is missing", topic), domain = NA)
        }
        else if(type == "chm") {
            ## unneeded but harmless under Unix
            chm.dll <- file.path(R.home("modules"), "Rchtml.dll")
            if(!file.exists(chm.dll))
                stop("Compiled HTML is not installed")
            if(!is.loaded("Rchtml")) dyn.load(chm.dll)
            wfile <- sub("/chm/([^/]*)$", "", file)
            thispkg <- sub(".*/([^/]*)/chm/([^/]*)$", "\\1", file)
            thispkg <- sub("_.*$", "", thispkg) # versioned installs.
            hlpfile <- paste(wfile, "/chtml/", thispkg, ".chm", sep = "")
            if(file.exists(hlpfile)) {
                err <- .C("Rchtml", hlpfile, basename(file),
                          err = integer(1), PACKAGE = "Rchtml")$err
                if(err) stop("CHM file could not be displayed")
            } else {
            	warning(gettextf("No CHM help for '%s' in package '%s' is available:\nthe CHM file for the package is missing", topic, thispkg), domain = NA)
            	att <- attributes(x)
            	x <- sub("/chm/([^/]*$)", "/help/\\1", x)
            	attributes(x) <- att
            	attr(x, "type") <- "help"
            	print(x)
            }
        }
        else if(type == "help") {
            zfile <- zip.file.extract(file, "Rhelp.zip")
            if(file.exists(zfile)) {
                first <- readLines(zfile, n = 1)
                enc <- if(length(grep("\\(.*\\)$", first)) > 0)
                    sub("[^(]*\\((.*)\\)$", "\\1", first) else ""
                if(enc == "utf8") enc <- "UTF-8"
                file.show(zfile,
                          title = gettextf("R Help on '%s'", topic),
                          delete.file = (zfile != file),
                          pager = attr(x, "pager"), encoding = enc)
            } else
                stop(gettextf("No text help for '%s' is available:\ncorresponding file is missing", topic), domain = NA)
        }
        else if(type == "latex") {
            ok <- FALSE
            zfile <- zip.file.extract(file, "Rhelp.zip")
            if(zfile != file) on.exit(unlink(zfile))
            if(file.exists(zfile)) {
                .show_help_on_topic_offline(zfile, topic)
                ok <- TRUE
            } else if(interactive()) {
                ## look for stored Rd files
                path <- dirname(file) # .../pkg/latex
                dirpath <- dirname(path)
                pkgname <- basename(dirpath) # versioning? ...
                Rdpath <- file.path(dirpath, "man",
                                    paste(pkgname, "Rd.gz", sep="."))
                if(file.exists(Rdpath)) {
                    ans <- readline("No latex file is available: shall I try to create it? (y/n) ")
                    if (substr(ans, 1, 1) == "y") {
                        lines <- tools:::extract_Rd_file(Rdpath, topic)
                        tf <- tempfile("Rd")
                        tf2 <- tempfile("Rlatex")
                        writeLines(lines, tf)
                        cmd <- paste("R CMD Rdconv -t latex", tf, ">", tf2)
                        res <- system(cmd)
                        if(res) stop("problems running R CMD Rdconv")
                        .show_help_on_topic_offline(tf2, topic)
                        ok <- TRUE
                    }
                }
            }
            if(!ok)
                stop(gettextf("No offline help for '%s' is available:\ncorresponding file is missing", topic), domain = NA)
        }
    }

    invisible(x)
}

