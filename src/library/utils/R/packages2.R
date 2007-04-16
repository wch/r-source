install.packages <-
    function(pkgs, lib, repos = getOption("repos"),
             contriburl = contrib.url(repos, type),
             method, available = NULL, destdir = NULL,
             installWithVers = FALSE, dependencies = NA,
             type = getOption("pkgType"), configure.args = character(0),
             clean = FALSE)
{
    if (is.logical(clean) && clean)
        clean <- "--clean"
    if(is.logical(dependencies) && is.na(dependencies))
        dependencies <- if(!missing(lib) && length(lib) > 1) FALSE
        else c("Depends", "Imports")

    explode_bundles <- function(a)
    {
        contains <- .find_bundles(a, FALSE)
        extras <- unlist(lapply(names(contains), function(x)
                                paste(contains[[x]], " (", x, ")", sep="")))
        sort(as.vector(c(a[, 1], extras)))
    }

    implode_bundles <- function(pkgs)
    {
    	bundled <- grep(".* \\(.*\\)$", pkgs)
    	if (length(bundled)) {
    	    bundles <- unique(gsub(".* \\((.*)\\)$", "\\1", pkgs[bundled]))
    	    pkgs <- c(pkgs[-bundled], bundles)
    	}
    	pkgs
    }


      # Compute the configuration arguments for a given package.
      # If configure.args is an unnamed character vector, use that.
      # If it is named, match the pkg name to the names of the character vector
      # and if we get a match, use that element.
      # Similarly, configure.args is a list(), match pkg to the names pkg and
      # use that element, collapsing it into a single string.

    getConfigureArgs <-  function(pkg)
    {
        ## Since the pkg argument can be the name of a file rather than
        ## a regular package name, we have to clean that up.
        pkg <- gsub("_\\.(zip|tar\\.gz)", "",
                    gsub(.standard_regexps()$valid_package_version, "", basename(pkg)))

        if(length(pkgs) == 1 && length(configure.args) &&
           length(names(configure.args)) == 0)
            return(paste("--configure-args=",
                         shQuote(paste(configure.args, collapse = " ")),
                         sep = ""))

        if (length(configure.args) && length(names(configure.args))
              && pkg %in% names(configure.args))
            config <- paste("--configure-args=",
                            shQuote(paste(configure.args[[ pkg ]], collapse = " ")),
                            sep = "")
        else
            config <- character(0)

        config
    }

    if(missing(pkgs) || !length(pkgs)) {
        if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA") {
            if(is.null(available))
                available <- available.packages(contriburl = contriburl,
                                                method = method)
            if(NROW(available)) {
                a <- explode_bundles(available)
                pkgs <- implode_bundles(select.list(a, multiple = TRUE, title = "Packages"))
            }
            if(!length(pkgs)) stop("no packages were specified")
        } else if(.Platform$OS.type == "unix" &&
                  capabilities("tcltk") && capabilities("X11")) {
            if(is.null(available))
                available <- available.packages(contriburl = contriburl,
                                                method = method)
            if(NROW(available)) {
                a <- explode_bundles(available)
                pkgs <- implode_bundles(tcltk::tk_select.list(a, multiple = TRUE,
                                              title ="Packages"))
            }
            if(!length(pkgs)) stop("no packages were specified")
        } else
            stop("no packages were specified")
    }

    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        if(length(.libPaths()) > 1)
            warning(gettextf("argument 'lib' is missing: using '%s'", lib),
                    immediate. = TRUE, domain = NA)
    }

    ## check for writeability by user
    ok <- file.info(lib)$isdir & (file.access(lib, 2) == 0)
    if(length(lib) > 1 && any(!ok))
        stop(sprintf(ngettext(sum(!ok),
                              "'lib' element '%s'  is not a writable directory",
                              "'lib' elements '%s' are not writable directories"),
                     paste(lib[!ok], collapse=", ")), domain = NA)
    if(length(lib) == 1 && ok && .Platform$OS.type == "windows") {
        ## file.access is unreliable on Windows, especially Vista.
        ## the only known reliable way is to try it
        fn <- file.path(lib, "_test_dir_")
        unlink(fn, recursive = TRUE) # precaution
        res <- try(dir.create(fn, showWarnings = FALSE))
        if(inherits(res, "try-error") || !res) ok <- FALSE
        else unlink(fn, recursive = TRUE)
    }
    if(length(lib) == 1 && !ok) {
        warning("'lib' is not writable", immediate.=TRUE)
        userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"),
                                   .Platform$path.sep))[1]
        if(interactive() && !file.exists(userdir)) {
            msg <- gettext("Would you like to create a personal library\n'%s'\nto install packages into?")
            if(.Platform$OS.type == "windows") {
                ans <- winDialog("yesno", sprintf(msg, userdir))
                if(ans != "YES") stop("unable to install packages")
            } else {
                ans <-
                    readline(paste(sprintf(msg, userdir), " (y/n) "))
                if(substr(ans, 1, 1) == "n") stop("unable to install packages")
            }
            if(!dir.create(userdir, recursive = TRUE))
                stop("unable to create ", sQuote(userdir))
            lib <- userdir
            .libPaths(c(userdir, .libPaths()))
        } else stop("unable to install packages")
    }

    if(.Platform$OS.type == "windows") {
        if(type == "mac.binary")
            stop("cannot install MacOS X binary packages on Windows")

        if(type == "win.binary") {      # include local .zip files
            .install.winbinary(pkgs = pkgs, lib = lib, contriburl = contriburl,
                               method = method, available = available,
                               destdir = destdir,
                               installWithVers = installWithVers,
                               dependencies = dependencies)
            return(invisible())
        }
    } else {
        if(type == "mac.binary") {
            if(!length(grep("darwin", R.version$platform)))
                stop("cannot install MacOS X binary packages on this plaform")
            .install.macbinary(pkgs = pkgs, lib = lib, contriburl = contriburl,
                               method = method, available = available,
                               destdir = destdir,
                               installWithVers = installWithVers,
                               dependencies = dependencies)
            return(invisible())
        }

        if(type == "win.binary")
            stop("cannot install Windows binary packages on this plaform")

        if(!file.exists(file.path(R.home("bin"), "INSTALL")))
            stop("This version of R is not set up to install source packages\nIf it was installed from an RPM, you may need the R-devel RPM")
    }

    ## we need to ensure that R CMD INSTALL runs with the same
    ## library trees as this session.
    libpath <- .libPaths()
    libpath <- libpath[! libpath %in% .Library]
    if(length(libpath)) libpath <- paste(libpath, collapse=.Platform$path.sep)
    cmd0 <- paste(file.path(R.home("bin"),"R"), "CMD INSTALL")
    if(length(libpath))
        if(.Platform$OS.type == "windows") {
            ## We don't have a way to set an environment variable for
            ## a single command, as we do not spawn a shell.
            oldrlibs <- Sys.getenv("R_LIBS")
            Sys.setenv(R_LIBS = libpath)
            on.exit(Sys.setenv(R_LIBS = oldrlibs))
        } else
            cmd0 <- paste(paste("R_LIBS", shQuote(libpath), sep="="), cmd0)

    if(is.null(repos) & missing(contriburl)) {
        ## install from local source tarballs
        update <- cbind(path.expand(pkgs), lib) # for side-effect of recycling to same length
        if (installWithVers)
            cmd0 <- paste(cmd0, "--with-package-versions")
        if (is.character(clean))
            cmd0 <- paste(cmd0, clean)

        for(i in 1:nrow(update)) {
            cmd <- paste(cmd0, "-l", shQuote(update[i, 2]),
                          getConfigureArgs(update[i, 1]),
                         shQuote(update[i, 1]))
            if(system(cmd) > 0)
                warning(gettextf(
                 "installation of package '%s' had non-zero exit status",
                                update[i, 1]), domain = NA)
        }
        return(invisible())
    }

    oneLib <- length(lib) == 1
    tmpd <- destdir
    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(is.null(destdir) && nonlocalcran) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd))
            stop(gettextf("unable to create temporary directory '%s'", tmpd),
                 domain = NA)
    }

    depends <- is.character(dependencies) ||
    (is.logical(dependencies) && dependencies)
    if(depends && is.logical(dependencies))
        dependencies <-  c("Depends", "Imports", "Suggests")
    if(depends && !oneLib) {
        warning("Do not know which element of 'lib' to install dependencies into\nskipping dependencies")
        depends <- FALSE
    }
    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    bundles <- .find_bundles(available)
    for(bundle in names(bundles))
        pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
    p0 <- unique(pkgs)
    miss <-  !p0 %in% row.names(available)
    if(sum(miss)) {
        warning(sprintf(ngettext(sum(miss),
                                 "package %s is not available",
                                 "packages %s are not available"),
                        paste(sQuote(p0[miss]), collapse=", ")),
                domain = NA)
        flush.console()
    }
    p0 <- p0[!miss]

    if(depends) { # check for dependencies, recursively
        p1 <- p0 # this is ok, as 1 lib only
        have <- .packages(all.available = TRUE)
        not_avail <- character(0)
	repeat {
	    if(any(miss <- ! p1 %in% row.names(available))) {
                not_avail <- c(not_avail, p1[miss])
                p1 <- p1[!miss]
	    }
	    deps <- as.vector(available[p1, dependencies])
	    deps <- .clean_up_dependencies(deps, available)
	    if(!length(deps)) break
	    toadd <- deps[! deps %in% c("R", have, pkgs)]
	    if(length(toadd) == 0) break
	    pkgs <- c(toadd, pkgs)
	    p1 <- toadd
	}
        if(length(not_avail)) {
            warning(sprintf(ngettext(length(not_avail),
                                     "dependency %s is not available",
                                     "dependencies %s are not available"),
                            paste(sQuote(not_avail), collapse=", ")),
                    domain = NA)
            flush.console()
        }

        for(bundle in names(bundles))
            pkgs[ pkgs %in% bundles[[bundle]] ] <- bundle
        pkgs <- unique(pkgs)
        pkgs <- pkgs[pkgs %in% row.names(available)]
        if(length(pkgs) > length(p0)) {
            added <- setdiff(pkgs, p0)
            message(sprintf(ngettext(length(added),
                                     "also installing the dependency %s",
                                     "also installing the dependencies %s"),
                            paste(sQuote(added), collapse=", ")),
                    "\n", domain = NA)
            flush.console()
        }
        p0 <- pkgs
    }

    foundpkgs <- download.packages(p0, destdir = tmpd, available = available,
                                   contriburl = contriburl, method = method,
                                   type = "source")

    ## at this point pkgs may contain duplicates,
    ## the same pkg in different libs
    if(length(foundpkgs)) {
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        found <- pkgs %in% foundpkgs[, 1]
        files <- foundpkgs[match(pkgs[found], foundpkgs[, 1]), 2]
        update <- cbind(update[found, , drop=FALSE], file = files)
        if(nrow(update) > 1) {
            upkgs <- unique(pkgs <- update[, 1])
            DL <- .make_dependency_list(upkgs, available)
            p0 <- .find_install_order(upkgs, DL)
            ## can't use update[p0, ] due to possible multiple matches
            update <- update[sort.list(match(pkgs, p0)), ]
        }
        if (installWithVers)
            cmd0 <- paste(cmd0, "--with-package-versions")
        if (is.character(clean))
            cmd0 <- paste(cmd0, clean)

        for(i in 1:nrow(update)) {
            cmd <- paste(cmd0, "-l", shQuote(update[i, 2]),
                          getConfigureArgs(update[i, 3]), update[i, 3])
            status <- system(cmd)
            if(status > 0)
                warning(gettextf(
                 "installation of package '%s' had non-zero exit status",
                                 update[i, 1]), domain = NA)
        }
        if(!is.null(tmpd) && is.null(destdir))
            cat("\n", gettextf("The downloaded packages are in\n\t%s",
                               normalizePath(tmpd)), "\n", sep = "")
        link.html.help(verbose = TRUE)
    } else if(!is.null(tmpd) && is.null(destdir)) unlink(tmpd, TRUE)

    invisible()
}
