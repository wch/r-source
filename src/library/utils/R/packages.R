CRAN.packages <- function(CRAN = getOption("CRAN"), method,
                          contriburl = contrib.url(CRAN))
{
    flds <- c("Package", "Version", "Priority", "Bundle",
              "Depends", "Imports", "Suggests", "Contains")
    res <- matrix(as.character(NA), 0, length(flds) + 1)
    colnames(res) <- c(flds, "Repository")
    for(repos in contriburl) {
        localcran <- length(grep("^file:", repos)) > 0
        if(localcran)
            tmpf <- paste(substring(repos,6), "PACKAGES", sep = "/")
        else{
            tmpf <- tempfile()
            on.exit(unlink(tmpf))
            download.file(url = paste(repos, "PACKAGES", sep = "/"),
                          destfile = tmpf, method = method, cacheOK = FALSE)
        }
        res0 <- read.dcf(file = tmpf, fields = flds)
        if(length(res0)) rownames(res0) <- res0[, "Package"]
        res0 <- cbind(res0, Repository = repos)
        res <- rbind(res, res0)
    }
    res
}

update.packages <- function(lib.loc = NULL, CRAN = getOption("CRAN"),
                            contriburl = contrib.url(CRAN),
                            method, instlib = NULL, ask = TRUE,
                            available = NULL, destdir = NULL,
			    installWithVers = FALSE,
                            checkBuilt = FALSE)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()

    if(is.null(available))
        available <- CRAN.packages(contriburl = contriburl, method = method)

    old <- old.packages(lib.loc = lib.loc,
                        contriburl = contriburl,
                        method = method,
                        available = available, checkBuilt = checkBuilt)

    update <- NULL
    if(ask & !is.null(old)){
        for(k in 1:nrow(old)){
            cat(old[k, "Package"], ":\n",
                "Version", old[k, "Installed"],
                "in", old[k, "LibPath"],
                if(checkBuilt) paste("built under", old[k, "Built"]),
                "\n",
                "Version", old[k, "CRAN"], "on CRAN")
            cat("\n")
            answer <- substr(readline("Update (y/N)?  "), 1, 1)
            if(answer == "y" | answer == "Y")
                update <- rbind(update, old[k,])
        }
    }
    else
        update <- old


    if(!is.null(update)) {
        if(is.null(instlib)) instlib <-  update[,"LibPath"]

        install.packages(update[,"Package"], instlib,
                         contriburl = contriburl,
                         method = method,
                         available = available, destdir = destdir,
                         installWithVers = installWithVers)
    }
}

old.packages <- function(lib.loc = NULL, CRAN = getOption("CRAN"),
                         contriburl = contrib.url(CRAN),
                         method, available = NULL, checkBuilt = FALSE)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()

    instp <- installed.packages(lib.loc = lib.loc)
    if(is.null(dim(instp)))
        stop("no installed.packages for (invalid?) lib.loc=",lib.loc)
    if(is.null(available))
        available <- CRAN.packages(contriburl = contriburl, method = method)

    ## for bundles it is sufficient to install the first package
    ## contained in the bundle, as this will install the complete bundle
    ## However, a bundle might be installed in more than one place.
    for(b in unique(instp[,"Bundle"])){
        if(!is.na(b))
            for (w in unique(instp[,"LibPath"])) {
                ok <- which(instp[,"Bundle"] == b & instp[,"LibPath"] == w)
                if(length(ok) > 1) instp <- instp[-ok[-1], ]
            }
    }

    ## for packages contained in bundles use bundle names from now on
    ok <- !is.na(instp[,"Bundle"])
    instp[ok,"Package"] <- instp[ok,"Bundle"]
    ok <- !is.na(available[,"Bundle"])
    available[ok,"Package"] <- available[ok,"Bundle"]

    update <- NULL

    currentR <- minorR <- getRversion()
    minorR[[1]][3] <- 0 # set patchlevel to 0
    for(k in 1:nrow(instp)) {
        if (instp[k, "Priority"] %in% "base") next
        z <- match(instp[k, "Package"], available[,"Package"])
        if(is.na(z)) next
        onCran <- available[z, ]
        ## OK if Built: is missing (which it should not be)
	if((!checkBuilt || package_version(instp[k, "Built"]) >= minorR) &&
           package_version(onCran["Version"]) <=
           package_version(instp[k, "Version"])) next
        deps <- onCran["Depends"]
        if(!is.na(deps)) {
            Rdeps <- tools:::.split_dependencies(deps)[["R"]]
            if(length(Rdeps) > 1) {
                target <- Rdeps$version
                res <- eval(parse(text=paste("currentR", Rdeps$op, "target")))
                if(!res) next
            }
        }
        update <- rbind(update, c(instp[k, c(1:3,8)], onCran["Version"]))
    }
    if(!is.null(update))
        colnames(update) <- c("Package", "LibPath", "Installed", "Built", "CRAN")
    update
}

new.packages <- function(lib.loc = NULL, CRAN = getOption("CRAN"),
                         contriburl = contrib.url(CRAN),
                         method, available = NULL)
{
    if(is.null(lib.loc)) lib.loc <- .libPaths()

    instp <- installed.packages(lib.loc = lib.loc)
    if(is.null(dim(instp)))
        stop("no installed.packages for (invalid?) lib.loc=", lib.loc)
    if(is.null(available))
        available <- CRAN.packages(contriburl = contriburl, method = method)
    ## for packages contained in bundles use bundle names from now on
    ## we don't have enough information to know if they are complete,
    ## as they may be out of date and the contents may have changed
    ok <- !is.na(instp[,"Bundle"])
    instp[ok,"Package"] <- instp[ok,"Bundle"]
    installed <- unique(instp[, "Package"])
    ok <- !is.na(available[,"Bundle"])
    available[ok,"Package"] <- available[ok,"Bundle"]
    poss <- sort(unique(available[ ,"Package"])) # sort in local locale
    setdiff(poss, installed)
}

installed.packages <- function(lib.loc = NULL, priority = NULL)
{
    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    pkgFlds <- c("Version", "Priority", "Bundle", "Depends", "Suggests",
                 "Built")
    if(!is.null(priority)) {
        if(!is.character(priority))
            stop("`priority' must be character or NULL")
        if(any(b <- priority %in% "high"))
            priority <- c(priority[!b], "recommended","base")
    }
    retval <- character()
    for(lib in lib.loc) {
        # this excludes packages without DESCRIPTION files
        pkgs <- .packages(all.available = TRUE, lib.loc = lib)
        for(p in pkgs){
            desc <- packageDescription(p, lib = lib, fields = pkgFlds)
            ## this gives NA if the package has no Version field
            if (is.logical(desc)) {
                desc <- rep(as.character(NA), length(pkgFlds))
                names(desc) <- pkgFlds
            } else {
                desc <- unlist(desc)
                if(!is.null(priority)) # skip if priority does not match
                    if(is.na(pmatch(desc["Priority"], priority))) next
                Rver <- strsplit(strsplit(desc["Built"], ";")[[1]][1],
                                 "[ \t]+")[[1]][2]
                desc["Built"] <- Rver
            }
            retval <- rbind(retval, c(p, lib, desc))
        }
    }
    if (length(retval)) {
        colnames(retval) <- c("Package", "LibPath", pkgFlds)
        rownames(retval) <- retval[, "Package"]
    }
    retval
}

remove.packages <- function(pkgs, lib, version) {

    updateIndices <- function(lib) {
        ## This should eventually be made public, as it could also be
        ## used by install.packages() && friends.
        if(lib == .Library) {
            ## R version of
            ##   ${R_HOME}/bin/build-help --htmllists
            ##   cat ${R_HOME}/library/*/CONTENTS \
            ##     > ${R_HOME}/doc/html/search/index.txt
            if(exists("link.html.help", mode = "function"))
                link.html.help()
        }
    }

    if(!length(pkgs)) return(invisible())

    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        warning(paste("argument", sQuote("lib"),
                      "is missing: using", lib))
    }

    if (!missing(version))
        pkgs <- manglePackageName(pkgs, version)

    paths <- .find.package(pkgs, lib)
    unlink(paths, TRUE)
    for(lib in unique(dirname(paths)))
        updateIndices(lib)
}

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

## private functions
.find_bundles <- function(available)
{
    ## Sort out bundles. Returns a named list of character vectors
    bundles <- available[!is.na(available[, "Bundle"]), "Contains"]
    ans <- strsplit(bundles, "[[:space:]]+")
    ## As VR is recommended, it may not be the same version
    ## as on CRAN (and for Windows etc may not be there).
    ans$VR <- c("MASS", "class", "nnet","spatial")
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
    if(is.null(available)) stop(sQuote(available), " must be supplied")
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
            warning(paste(sQuote(names(DL)), collapse = ", "),
                    "are mutually dependent")
            return(c(done,  names(DL)))
        }
        done <- c(done, names(DL[OK]))
        DL <- DL[!OK]
    }
    done
}
