available.packages <-
    function(contriburl = contrib.url(getOption("repos")), method)
{
    .checkRversion <- function(x) {
        if(is.na(xx <- x["Depends"])) return(TRUE)
        xx <- tools:::.split_dependencies(xx)
        if(length(z <- xx[["R"]]) > 1)
            eval(parse(text=paste("currentR", z$op, "z$version")))
        else TRUE
    }

    flds <- c("Package", "Version", "Priority", "Bundle",
              "Depends", "Imports", "Suggests", "Contains")
    res <- matrix(as.character(NA), 0, length(flds) + 1)
    colnames(res) <- c(flds, "Repository")
    for(repos in contriburl) {
        localcran <- length(grep("^file:", repos)) > 0
        if(localcran) {
            ## see note in download.packages
            tmpf <- paste(substring(repos, 6), "PACKAGES", sep = "/")
            tmpf <- sub("^//", "", tmpf)
            if(.Platform$OS.type == "windows") {
                if(length(grep("[A-Za-z]:", tmpf)))
                    tmpf <- substring(tmpf, 2)
            }
            res0 <- read.dcf(file = tmpf, fields = flds)
            if(length(res0)) rownames(res0) <- res0[, "Package"]
        } else {
            dest <- file.path(tempdir(),
                              paste("repos_",
                                    URLencode(repos, TRUE),
                                    ".rds", sep=""))
            if(file.exists(dest)) {
                res0 <- .readRDS(dest)
            } else {
                tmpf <- tempfile()
                on.exit(unlink(tmpf))
                op <- options("warn")
                options(warn = -1)
                ## This is a binary file
                z <- try(download.file(url=paste(repos, "PACKAGES.gz", sep = "/"),
                                       destfile = tmpf, method = method,
                                       cacheOK = FALSE, quiet = TRUE, mode = "wb"),
                         silent = TRUE)
                if(inherits(z, "try-error")) {
                    ## read.dcf is going to interpret CRLF as LF, so use
                    ## binary mode to avoid CRCRLF.
                    z <- try(download.file(url=paste(repos, "PACKAGES", sep = "/"),
                                           destfile = tmpf, method = method,
                                           cacheOK = FALSE, quiet = TRUE,
                                           mode = "wb"),
                             silent = TRUE)
                }
                options(op)
                if(inherits(z, "try-error")) {
                    warning(gettextf("unable to access index for repository %s", repos),
                            call. = FALSE, immediate. = TRUE, domain = NA)
                    next
                }
                res0 <- read.dcf(file = tmpf, fields = flds)
                if(length(res0)) rownames(res0) <- res0[, "Package"]
                .saveRDS(res0, dest, compress = TRUE)
                unlink(tmpf)
                on.exit()
            } # end of download vs cached
        } # end of localcran vs online
        res0 <- cbind(res0, Repository = repos)
        res <- rbind(res, res0)
    }
    ## ignore packages which don't fit our version of R
    if(length(res)) {
        .checkRversion <- function(x) {
            if(is.na(xx <- x["Depends"])) return(TRUE)
            xx <- tools:::.split_dependencies(xx)
            if(length(z <- xx[["R"]]) > 1)
                eval(parse(text=paste("currentR", z$op, "z$version")))
            else TRUE
        }
        currentR <- getRversion()
        res <- res[apply(res, 1, .checkRversion), , drop=FALSE]
    }
    res
}


## unexported helper function
simplifyRepos <- function(repos, type)
{
    tail <- substring(contrib.url("---", type), 4)
    ind <- regexpr(tail, repos, fixed=TRUE)
    ind <- ifelse(ind > 0, ind-1, nchar(repos, type="c"))
    substr(repos, 1, ind)
}

update.packages <- function(lib.loc = NULL, repos = getOption("repos"),
                            contriburl = contrib.url(repos, type),
                            method, instlib = NULL, ask = TRUE,
                            available = NULL, destdir = NULL,
			    installWithVers = FALSE,
                            checkBuilt = FALSE, type = getOption("pkgType"))
{
    ask  # just a check that it is valid before we start work
    text.select <- function(old)
    {
        update <- NULL
        for(k in 1:nrow(old)){
            cat(old[k, "Package"], ":\n",
                "Version", old[k, "Installed"],
                "installed in", old[k, "LibPath"],
                if(checkBuilt) paste("built under R", old[k, "Built"]),
                "\n",
                "Version", old[k, "ReposVer"], "available at",
                simplifyRepos(old[k, "Repository"], type))
            cat("\n")
            answer <- substr(readline("Update (y/N/c)?  "), 1, 1)
            if(answer == "c" | answer == "c") {
                cat("cancelled by user\n")
                return(invisible())
            }
            if(answer == "y" | answer == "Y")
                update <- rbind(update, old[k,])
        }
        update
    }

    if(is.null(lib.loc))
        lib.loc <- .libPaths()

    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)

    old <- old.packages(lib.loc = lib.loc,
                        contriburl = contriburl,
                        method = method,
                        available = available, checkBuilt = checkBuilt)

    if(is.null(old)) return(invisible())
    if(is.character(ask) && ask == "graphics") {
        if(.Platform$OS.type == "unix" && .Platform$GUI != "AQUA"
           && capabilities("tcltk") && capabilities("X11")) {
            k <- tcltk::tk_select.list(old[,1], old[,1], multiple = TRUE,
                                       title = "Packages to be updated")
            update <- old[match(k, old[,1]), , drop=FALSE]
        } else if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA") {
            k <- select.list(old[,1], old[,1], multiple = TRUE,
                             title = "Packages to be updated")
            update <- old[match(k, old[,1]), , drop=FALSE]
        } else update <- text.select(old)
        if(nrow(update) == 0) return(invisible())
    } else if(is.logical(ask) && ask) update <- text.select(old)
    else update <- old


    if(!is.null(update)) {
        if(is.null(instlib)) instlib <-  update[,"LibPath"]

        install.packages(update[,"Package"], instlib,
                         contriburl = contriburl,
                         method = method,
                         available = available, destdir = destdir,
                         installWithVers = installWithVers, type = type)
    }
}

old.packages <- function(lib.loc = NULL, repos = getOption("repos"),
                         contriburl = contrib.url(repos),
                         method, available = NULL, checkBuilt = FALSE)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()

    instp <- installed.packages(lib.loc = lib.loc)
    if(is.null(dim(instp)))
        stop(gettextf("no installed packages for (invalid?) 'lib.loc=%s'",
                      lib.loc), domain = NA)
    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)

    ## For bundles it is sufficient to install the first package
    ## contained in the bundle, as this will install the complete bundle
    ## However, a bundle might be installed in more than one place.
    for(b in unique(instp[, "Bundle"])){
        if(!is.na(b))
            for (w in unique(instp[, "LibPath"])) {
                ok <- which(instp[, "Bundle"] == b & instp[, "LibPath"] == w)
                if(length(ok) > 1) instp <- instp[-ok[-1], ]
            }
    }

    ## for packages contained in bundles use bundle names from now on
    ok <- !is.na(instp[, "Bundle"])
    instp[ok, "Package"] <- instp[ok, "Bundle"]
    ok <- !is.na(available[, "Bundle"])
    available[ok, "Package"] <- available[ok, "Bundle"]

    update <- NULL

    currentR <- minorR <- getRversion()
    minorR[[1]][3] <- 0 # set patchlevel to 0
    for(k in 1:nrow(instp)) {
        if (instp[k, "Priority"] %in% "base") next
        z <- match(instp[k, "Package"], available[,"Package"])
        if(is.na(z)) next
        onRepos <- available[z, ]
        ## works OK if Built: is missing (which it should not be)
	if((!checkBuilt || package_version(instp[k, "Built"]) >= minorR) &&
           package_version(onRepos["Version"]) <=
           package_version(instp[k, "Version"])) next
        deps <- onRepos["Depends"]
        if(!is.na(deps)) {
            Rdeps <- tools:::.split_dependencies(deps)[["R"]]
            if(length(Rdeps) > 1) {
                target <- Rdeps$version
                res <- eval(parse(text=paste("currentR", Rdeps$op, "target")))
                if(!res) next
            }
        }
        update <- rbind(update,
                        c(instp[k, c("Package", "LibPath", "Version", "Built")],
                          onRepos["Version"], onRepos["Repository"]))
    }
    if(!is.null(update))
        colnames(update) <- c("Package", "LibPath", "Installed", "Built",
                              "ReposVer", "Repository")
    rownames(update) <- update[, "Package"]
    update
}

new.packages <- function(lib.loc = NULL, repos = getOption("repos"),
                         contriburl = contrib.url(repos),
                         method, available = NULL, ask = FALSE, destdir = NULL)
{
    ask  # just a check that it is valid before we start work
    if(is.null(lib.loc)) lib.loc <- .libPaths()

    instp <- installed.packages(lib.loc = lib.loc)
    if(is.null(dim(instp)))
        stop(gettextf("no installed packages for (invalid?) 'lib.loc=%s'",
                      lib.loc), domain = NA)
    if(is.null(available))
        available <- available.packages(contriburl = contriburl,
                                        method = method)
    ## For packages contained in bundles use bundle names from now on.
    ## We used not to have enough information to know if they are complete,
    ## as they may be out of date and the contents may have changed
    ## However, as from 2.1.0 we install the Contains: field.
    ok <- !is.na(instp[, "Bundle"])
    if(any(ok)) { # we have at least one bundle installed
        for(b in unique(instp[ok, "Bundle"]))
            if(!is.na(b)) {
                if(! b %in% rownames(available)) next
                ok1 <- which(instp[, "Bundle"] == b)
                contains <- instp[ok1[1], "Contains"]
                if(!is.na(contains)) {
                    contains <- strsplit(contains, "[[:space:]]+")[[1]]
                    if(!all(contains %in% instp[ok1, "Package"]))
                        warning(gettextf("bundle '%s' is incompletely installed", b), domain = NA)
                }
                new <- setdiff(strsplit(available[b, "Contains"], "[[:space:]]+")[[1]],
                               instp[ok1, "Package"])
                if(length(new))
                    warning(gettextf("bundle '%s' has extra contents %s", b,
                                     paste(sQuote(new), collapse = ", ")),
                            domain = NA)
            }
    }
    instp[ok, "Package"] <- instp[ok, "Bundle"]
    installed <- unique(instp[, "Package"])
    poss <- sort(unique(available[ ,"Package"])) # sort in local locale
    res <- setdiff(poss, installed)
    update <- character(0)
    if(is.character(ask) && ask == "graphics") {
        if(.Platform$OS.type == "unix"
           && capabilities("tcltk") && capabilities("X11")) {
            k <- tcltk::tk_select.list(res, multiple = TRUE,
                                       title = "New packages to be installed")
            update <- res[match(k, res)]
        } else if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA") {
            k <- select.list(res, multiple = TRUE,
                             title = "New packages to be installed")
            update <- res[match(k, res)]
        }
    } else if(is.logical(ask) && ask)
        update <- res[match(select.list(res, multiple = TRUE,
                                        title = "New packages to be installed")
                            , res)]
    if(length(update)) {
        install.packages(update, lib = lib.loc[1], contriburl = contriburl,
                         method = method, available = available,
                         destdir = destdir)
        # Now check if they were installed and update 'res'
        dirs <- list.files(lib.loc[1])
        updated <- update[update %in% dirs]
        # Need to check separately for bundles
        av <- available[update, , drop = FALSE]
        bundles <- av[!is.na(av[, "Contains"]), , drop=FALSE]
        for(bundle in rownames(bundles)) {
            contains <- strsplit(bundles[bundle, "Contains"],
                                 "[[:space:]]+")[[1]]
            if(all(contains %in% dirs)) updated <- c(updated, bundle)
        }
        res <- res[!res %in% updated]
    }
    res
}

installed.packages <-
    function(lib.loc = NULL, priority = NULL,  noCache = FALSE)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    pkgFlds <- c("Version", "Priority", "Bundle", "Contains", "Depends",
                 "Suggests", "Imports", "Built")
    if(!is.null(priority)) {
        if(!is.character(priority))
            stop("'priority' must be character or NULL")
        if(any(b <- priority %in% "high"))
            priority <- c(priority[!b], "recommended","base")
    }
    retval <- matrix("", 0, 2+length(pkgFlds))
    for(lib in lib.loc) {
        dest <- file.path(tempdir(),
                          paste("libloc_", URLencode(lib, TRUE), ".rds",
                                sep=""))
        if(!noCache && file.exists(dest) &&
            file.info(dest)$mtime > file.info(lib.loc)$mtime) {
            retval <- rbind(retval, .readRDS(dest))
        } else {
            ret0 <- character()
            ## this excludes packages without DESCRIPTION files
            pkgs <- .packages(all.available = TRUE, lib.loc = lib)
            for(p in pkgs){
                desc <- packageDescription(p, lib = lib, fields = pkgFlds,
                                           encoding = NA)
                ## this gives NA if the package has no Version field
                if (is.logical(desc)) {
                    desc <- rep(as.character(NA), length(pkgFlds))
                    names(desc) <- pkgFlds
                } else {
                    desc <- unlist(desc)
                    Rver <- strsplit(strsplit(desc["Built"], ";")[[1]][1],
                                     "[ \t]+")[[1]][2]
                    desc["Built"] <- Rver
                }
                ret0 <- rbind(ret0, c(p, lib, desc))
            }
            if(length(ret0)) {
                retval <- rbind(retval, ret0)
                .saveRDS(ret0, dest, compress = TRUE)
            }
        }
    }
    colnames(retval) <- c("Package", "LibPath", pkgFlds)
    if(length(retval) && !is.null(priority)) {
        keep <- !is.na(pmatch(retval[,"Priority"], priority,
                              duplicates.ok = TRUE))
        retval <- retval[keep, ]
    }
    if (length(retval)) {
        rownames(retval) <- retval[, "Package"]
    }
    retval
}

remove.packages <- function(pkgs, lib, version) {

    updateIndices <- function(lib) {
        ## This should eventually be made public, as it could also be
        ## used by install.packages() && friends.
        if(lib == .Library) {
            if(exists("link.html.help", mode = "function"))
                link.html.help()
        }
    }

    if(!length(pkgs)) return(invisible())

    hv <- !missing(version)
    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        warning(gettextf("argument 'lib' is missing: using %s", lib),
                immediate. = TRUE, domain = NA)
    }
    have <- installed.packages(lib.loc=lib)
    is_bundle <- pkgs %in% have[, "Bundle"]
    pkgs0 <- pkgs; pkgs <- pkgs[!is_bundle]
    if(hv) {
        names(version) <- pkgs0;
        if(length(pkgs)) pkgs <- manglePackageName(pkgs, version[!is_bundle])
    }
    for(p in pkgs0[is_bundle]) {
        ## for consistency with packages, need unversioned names
        ## and let .find.packages() figure out what to do.
        add <- have[have[, "Bundle"] %in% p, "Package"]
        add <- unique(sub("_[0-9.\-]*$", "", add))
        if(hv) add <- manglePackageName(add, version[p])
        pkgs <- c(pkgs, add)
    }

    paths <- .find.package(pkgs, lib)
    if(length(paths)) {
        unlink(paths, TRUE)
        for(lib in unique(dirname(paths))) updateIndices(lib)
    }
    invisible()
}

download.packages <- function(pkgs, destdir, available = NULL,
                              repos = getOption("repos"),
                              contriburl = contrib.url(repos, type),
                              method, type = getOption("pkgType"))
{
    dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if(nonlocalcran && !dirTest(destdir))
        stop("'destdir' is not a directory")
    if(is.null(available))
        available <- available.packages(contriburl=contriburl, method=method)

    retval <- matrix(character(0), 0, 2)
    for(p in unique(pkgs))
    {
        ok <- (available[,"Package"] == p) | (available[,"Bundle"] == p)
        ok <- ok & !is.na(ok)
        if(!any(ok))
            warning(gettextf("no package '%s' at the repositories", p),
                    domain = NA, immediate. = TRUE)
        else {
            if(sum(ok) > 1) { # have multiple copies
                vers <- package_version(available[ok, "Version"])
                keep <- vers == max(vers)
                keep[duplicated(keep)] <- FALSE
                ok[ok][!keep] <- FALSE
            }
            fn <- paste(p, "_", available[ok, "Version"],
                        switch(type,
                               "source" = ".tar.gz",
                               "mac.binary" = ".tgz",
                               "win.binary" = ".zip"),
                        sep="")
            repos <- available[ok, "Repository"]
            if(length(grep("^file:", repos)) > 0) { # local repository
                ## We need to derive the file name from the URL
                ## This is tricky as so many forms have been allowed,
                ## and indeed external methods may do even more.
                fn <- paste(substring(repos, 6), fn, sep = "/")
                fn <- sub("^//", "", fn)
                fn <- URLdecode(fn)
                ## This should leave us with a path beginning with /
                if(.Platform$OS.type == "windows") {
                    if(length(grep("[A-Za-z]:", fn)))
                        fn <- substring(fn, 2)
                }
                retval <- rbind(retval, c(p, fn))
            } else {
                url <- paste(repos, fn, sep="/")
                destfile <- file.path(destdir, fn)

                res <- try(download.file(url, destfile, method, mode="wb"))
                if(!inherits(res, "try-error") && res == 0)
                    retval <- rbind(retval, c(p, destfile))
                else
                    warning(gettextf("download of package '%s' failed", p),
                            domain = NA, immediate. = TRUE)
            }
        }
    }

    retval
}

contrib.url <- function(repos, type = getOption("pkgType"))
{
    if(is.null(repos)) return(NULL)
    if("@CRAN@" %in% repos && interactive()) {
        cat(gettext("--- Please select a CRAN mirror for use in this session ---\n"))
        flush.console()
        chooseCRANmirror()
        m <- match("@CRAN@", repos)
        nm <- names(repos)
        repos[m] <- getOption("repos")["CRAN"]
        if(is.null(nm)) nm <- rep("", length(repos))
        nm[m] <- "CRAN"
        names(repos) <- nm
    }
    if("@CRAN@" %in% repos) stop("trying to use CRAN without setting a mirror")

    ver <- paste(R.version$major,
                 strsplit(R.version$minor, ".", fixed=TRUE)[[1]][1], sep = ".")
    res <- switch(type,
		"source" = paste(gsub("/$", "", repos), "src", "contrib", sep="/"),
                "mac.binary" = paste(gsub("/$", "", repos), "bin", "macosx", R.version$arch, "contrib", ver, sep = "/"),
                "win.binary" = paste(gsub("/$", "", repos), "bin", "windows", "contrib", ver, sep="/")
               )
    res
}


chooseCRANmirror <- function(graphics = TRUE)
{
    if(!interactive()) stop("cannot choose a CRAN mirror non-interactively")
    m <- try(read.csv(url("http://cran.r-project.org/CRAN_mirrors.csv"),
                      as.is=TRUE))
    if(inherits(m, "try-error"))
        m <- read.csv(file.path(R.home("doc"), "CRAN_mirrors.csv"), as.is=TRUE)
    res <- menu(m[,1], graphics, "CRAN mirror")
    if(res > 0) {
        URL <- m[res, "URL"]
        repos <- getOption("repos")
        repos["CRAN"] <- gsub("/$", "", URL[1])
        options(repos = repos)
    }
    invisible()
}

setRepositories <- function(graphics=TRUE)
{
    if(!interactive()) stop("cannot set repositories non-interactively")
    p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
    if(!file.exists(p))
        p <- file.path(R.home("etc"), "repositories")
    a <- read.delim(p, header=TRUE,
                    colClasses=c(rep("character", 3), rep("logical", 4)))
    thisType <- a[[getOption("pkgType")]]
    a <- a[thisType, 1:3]
    repos <- getOption("repos")
    ## Now look for CRAN and any others in getOptions("repos")
    if("CRAN" %in% row.names(a) && !is.na(CRAN <- repos["CRAN"]))
        a["CRAN", "URL"] <- CRAN
    ## Set as default any already in the option.
    a[(a[["URL"]] %in% repos), "default"] <- TRUE
    new <- !(repos %in% a[["URL"]])
    if(any(new)) {
        aa <- names(repos[new])
        if(is.null(aa)) aa <- rep("", length(repos[new]))
        aa[aa == ""] <- repos[new][aa == ""]
        newa <- data.frame(menu_name=aa, URL=repos[new], default=TRUE)
        row.names(newa) <- aa
        a <- rbind(a, newa)
    }

    default <- a[["default"]]

    res <- integer(0)
    if(graphics) {
        ## return a list of row numbers.
        if(.Platform$OS.type == "windows" || .Platform$GUI == "AQUA")
            res <- match(select.list(a[, 1], a[default, 1], multiple = TRUE,
                                     "Repositories"), a[, 1])
        else if(.Platform$OS.type == "unix" &&
                capabilities("tcltk") && capabilities("X11"))
            res <- match(tcltk::tk_select.list(a[, 1], a[default, 1],
                                            multiple = TRUE, "Repositories"),
                         a[, 1])
    }
    if(!length(res)) {
        ## text-mode fallback
        cat(gettext("--- Please select repositories for use in this session ---\n"))
        nc <- length(default)
        cat("", paste(seq(len=nc), ": ",
                      ifelse(default, "+", " "), " ", a[, 1],
                      sep=""),
            "", sep="\n")
        cat(gettext("Enter one or more numbers separated by spaces\n"))
        res <- scan("", what=0, quiet=TRUE, nlines=1)
        if(!length(res) || (length(res) == 1 && !res[1])) return(invisible())
        res <- res[1 <= res && res <= nc]
    }
    if(length(res)) {
        repos <- a[["URL"]]
        names(repos) <- row.names(a)
        options(repos=repos[res])
    }
}

normalizePath <- function(path) .Internal(normalizePath(path))


## used in some BioC packages and their support in tools.
compareVersion <- function(a, b)
{
    if(is.na(a)) return(-1)
    if(is.na(b)) return(1)
    a <- as.integer(strsplit(a, "[\\.-]")[[1]])
    b <- as.integer(strsplit(b, "[\\.-]")[[1]])
    for(k in 1:length(a)) {
        if(k <= length(b)) {
            if(a[k] > b[k]) return(1) else if(a[k] < b[k]) return(-1)
        } else {
            return(1)
        }
    }
    if(length(b) > length(a)) return(-1) else return(0)
}

## ------------- private functions --------------------
.find_bundles <- function(available, all=TRUE)
{
    ## Sort out bundles. Returns a named list of character vectors
    bundles <- available[!is.na(available[, "Bundle"]), "Contains"]
    ans <- strsplit(bundles, "[[:space:]]+")
    ## As VR is recommended, it may not be the same version
    ## as on CRAN (and for Windows etc may not be there).
    if(all) ans$VR <- c("MASS", "class", "nnet","spatial")
    ans
}

.clean_up_dependencies <- function(x, available)
{
    ## x is a character vector of Depends / Suggests / Imports entries
    ## returns a character vector of all the package dependencies mentioned
    x <- x[!is.na(x)]
    if(!length(x)) return(x)
    x <- unlist(strsplit(x, ","))
    unique(sub("^[[:space:]]*([[:alnum:].]+).*$", "\\1" , x))
}

.make_dependency_list <- function(pkgs, available)
{
    ## given a character vector of packages,
    ## return a named list of character vectors of their dependencies
    if(!length(pkgs)) return(NULL)
    if(is.null(available))
        stop(gettextf("'%s' must be supplied", available), domain = NA)
    info <- available[pkgs, c("Depends", "Imports"), drop = FALSE]
    x <- apply(info, 1, .clean_up_dependencies)
    if(length(pkgs) == 1) {x <- list(as.vector(x)); names(x) <- pkgs}
    bundles <- .find_bundles(available)
    x <- lapply(x, function(x) if(length(x)) {
        for(bundle in names(bundles))
            x[ x %in% bundles[[bundle]] ] <- bundle
        x <- x[! x %in% c("R", "NA")]
        unique(x)
    } else x)
    x
}

.find_install_order <- function(pkgs, dependencyList)
{
    ## given a character vector of packages, find an install order
    ## which reflects their dependencies.
    DL <- dependencyList[pkgs]
    ## some of the packages may be already installed, but the
    ## dependencies apply to those being got from CRAN.
    DL <- lapply(DL, function(x) x[x %in% pkgs])
    lens <- sapply(DL, length)
    if(all(lens > 0)) {
        warning("every package depends on at least one other")
        return(pkgs)
    }
    done <- names(DL[lens == 0]); DL <- DL[lens > 0]
    while(length(DL)) {
        OK <- sapply(DL, function(x) all(x %in% done))
        if(!any(OK)) {
            warning(gettextf("packages %s are mutually dependent",
                             paste(sQuote(names(DL)), collapse = ", ")),
                    domain = NA)
            return(c(done,  names(DL)))
        }
        done <- c(done, names(DL[OK]))
        DL <- DL[!OK]
    }
    done
}
