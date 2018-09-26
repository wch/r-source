#  File src/library/tools/R/tools-deprecated.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

## <entry>
## Deprecated in 3.3.0 (r70156 (Sat, 13 Feb 2016))

## This is called only with check = FALSE in getDepMtrx/getRemotePkgDepends
package.dependencies <-
    function(x, check = FALSE, depLevel = c("Depends", "Imports", "Suggests"))
{
    depLevel <- match.arg(depLevel)
    ## only one 'depLevel' at a time .. Rather use
    .Deprecated("package_dependencies")

    if(!is.matrix(x))
        x <- matrix(x, nrow = 1L, dimnames = list(NULL, names(x)))

    deps <- list()
    for(k in 1L:nrow(x)){
        z <- x[k, depLevel]
        if(!is.na(z) & z != ""){
            ## split dependencies, remove leading and trailing whitespace
            z <- unlist(strsplit(z, ",", fixed=TRUE))
            z <- sub("^[[:space:]]*(.*)", "\\1", z)
            z <- sub("(.*)[[:space:]]*$", "\\1", z)

            ## split into package names and version
            pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
            deps[[k]] <-
                cbind(sub(pat, "\\1", z), sub(pat, "\\2", z), NA)

            noversion <- deps[[k]][,1] == deps[[k]][,2]
            deps[[k]][noversion,2] <- NA

            ## split version dependency into operator and version number
            pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
            deps[[k]][!noversion, 2:3] <-
                c(sub(pat, "\\1", deps[[k]][!noversion, 2]),
                  sub(pat, "\\2", deps[[k]][!noversion, 2]))
        }
        else
            deps[[k]] <- NA
    }

    if(check){
        z <- rep.int(TRUE, nrow(x))
        for(k in 1L:nrow(x)) {
            ## currently we only check the version of R itself
            if(!is.na(deps[[k]]) &&
               any(ok <- deps[[k]][,1] == "R")) {
                ## NOTE: currently operators must be `<=' or `>='.
                if(!is.na(deps[[k]][ok, 2])
                   && deps[[k]][ok, 2] %in% c("<=", ">=")) {
                    ## careful.  We don't want 1.9.1 < 1.50
                    op <- deps[[k]][ok,2]
                    x1 <- rep.int(0, 6)
                    y <- c(R.version$major,
                           strsplit(R.version$minor, ".", fixed=TRUE)[[1L]])
                    x1[seq_along(y)] <- y
                    y <- strsplit(deps[[k]][ok,3], ".", fixed=TRUE)[[1L]]
                    x1[3+seq_along(y)] <- y
                    x1 <- format(x1, justify="right")
                    x2 <- paste(x1[4:6], collapse=".")
                    x1 <- paste(x1[1L:3], collapse=".")
                    comptext <- paste0("'", x1, "' ", op, " '", x2, "'")
                    compres <- try(eval(parse(text = comptext)))
                    if(!inherits(compres, "try-error")) {
                        z[k] <- compres
                    }
                }
            }
        }
        names(z) <- x[,"Package"]
        return(z)
    }
    else{
        names(deps) <- x[,"Package"]
        return(deps)
    }
}

.package_dependencies <- function(packages = NULL, db,
         which = c("Depends", "Imports", "LinkingTo"),
         recursive = FALSE, reverse = FALSE)
{
    .Deprecated("package_dependencies")
    package_dependencies(packages = packages, db = db,
         which = which, recursive = recursive, reverse = reverse)
}


pkgDepends <- function(pkg, recursive=TRUE, local=TRUE,
                       reduce=TRUE, lib.loc=NULL) {
    if (length(pkg) != 1L)
        stop("argument 'pkg' must be of length 1")

    .Deprecated("package_dependencies()")# or also dependsOnPkgs() ?

    instPkgs <- utils::installed.packages(lib.loc=lib.loc)

    depMtrx <- getDepMtrx(pkg, instPkgs, local)
    if (is.null(depMtrx))               # Package was not found
        stop(packageNotFoundError(pkg, lib.loc, sys.call()))

    getDepList(depMtrx, instPkgs, recursive, local, reduce)
}

getDepList <- function(depMtrx, instPkgs, recursive=TRUE,
                       local=TRUE, reduce=TRUE, lib.loc=NULL)
{
    .Deprecated("dependsOnPkgs() or package_dependencies()")
    if(!missing(lib.loc))
        warning("the 'lib.loc' argument has always been unused and is deprecated now")

    out <- structure(list(Depends=character(), Installed=character(),
                          Found=list(), NotFound=character(),
                          R=character()),
                     class = "DependsList")

    if (!is.matrix(depMtrx) && anyNA(depMtrx)) # no dependencies
        return(out)

    mtrxList <- buildDepList(depMtrx, instPkgs, recursive)

    if (!local) {
        toFind <- mtrxList$Depends[!apply(mtrxList$Depends, 1L,
                                          isSatisfied,
                                          mtrxList$Installed),,drop=FALSE]
        if (reduce)
            toFind <- reduceDepends(toFind)

        if (length(toFind)) {
            found <- foundDepends(toFind)
            out$Found <- found$Found
            mtrxList$NotFound <- found$NotFound
        }
    }

    if (reduce) {       # Found and NotFound are already reduced
	mtrxList$R         <- reduceDepends(mtrxList$R)
	mtrxList$Depends   <- reduceDepends(mtrxList$Depends)
	mtrxList$Installed <- reduceDepends(mtrxList$Installed)
    }

    ## Massage the matrices back into dependency strings.  out$Found
    ## is already assigned.
    out$R         <- depMtrxToStrings(mtrxList$R)
    out$Depends   <- depMtrxToStrings(mtrxList$Depends)
    out$Installed <- depMtrxToStrings(mtrxList$Installed)
    out$NotFound  <- depMtrxToStrings(mtrxList$NotFound)

    out
}

isSatisfied <- function(dep, instMtrx) {
    triplets <- apply(instMtrx, 1L, paste, collapse=":")
    paste(dep,collapse=":") %in% triplets
}

buildDepList <- function(depMtrx, instPkgs, recursive=TRUE)
{
    mtrxList <- list(Depends  = matrix(nrow=0L, ncol=3L),
		     Installed= matrix(nrow=0L, ncol=3L),
		     R        = matrix(nrow=0L, ncol=3L))

    ## First check to see if there is a dependency on R
    ## If there is, then check it
    if (any(isR <- depMtrx[,1] == "R")) {
        mtrxList$R <- depMtrx[ isR,, drop=FALSE]
        depMtrx <-    depMtrx[!isR,, drop=FALSE]
    }

    ## Get which of the direct depends are installed
    instDeps <- depMtrx[installedDepends(depMtrx, instPkgs),,drop=FALSE]

    mtrxList$Depends <- depMtrx
    mtrxList$Installed <- instDeps
    if (recursive) {
        for (curPkg in depMtrx[,1]) {
            depMtrx <- getDepMtrx(curPkg, instPkgs)
            ## Make sure this package was found & has deps
            if (is.null(depMtrx) || anyNA(depMtrx))
                next

            curMtrxList <- buildDepList(depMtrx, instPkgs, recursive=TRUE)
            mtrxList$R <- rbind(mtrxList$R, curMtrxList$R)
            mtrxList$Depends <- rbind(mtrxList$Depends,
                                      curMtrxList$Depends)
            mtrxList$Installed <- rbind(mtrxList$Installed, curMtrxList$Installed)
        }
    }

    mtrxList
}

getDepMtrx <- function(pkg, instPkgs, local=TRUE) {
    ## Need to see if pkg is installed - if not, get online
    row <- match(pkg, instPkgs[,"Package"])
    if (!is.na(row)) # package is installed
        package.dependencies(instPkgs[row,])[[1L]]
    else if (!local)
        getRemotePkgDepends(pkg)
    ## else NULL

    ## Either a matrix, NA if no deps or NULL if not found
}

getRemotePkgDepends <- function(pkg, contriburl=getOption("repos")) {
    ## Will get the dependencies of a package from
    ## online repositories.  Returns NULL if it
    ## cannot be found, otherwise returns the row provided
    ## by available.packages().

    if(is.null(contriburl))
        contriburl <- utils::contrib.url(getOption("repos"))

    cran <- utils::available.packages(contriburl=contriburl)
    if (any(iPkg <- pkg == cran[,"Package"]))
        package.dependencies(cran[iPkg,])[[1L]]
    ## else NULL
}

installedDepends <- function(depMtrx, instPkgs) {
    ## Given a matrix of packages, will return a vector of row
    ## numbers that correspond to packages in the matrix where
    ## the dependency is met by installed packages

    pkgs <- depMtrx[,1]
    if (length(pkgs) &&
	nrow(curPkgs <- depMtrx[pkgs %in% instPkgs[,"Package"],,drop=FALSE])) {
	passVersReq <- apply(curPkgs, 1L, function(x) {
	    (is.na(x[2L]) ||
	     compareDependsPkgVersion(instPkgs[instPkgs[,1]==x[1L], "Version"],
				      x[2L], x[3L]) >= 0)
	})
	passPkgs <- curPkgs[passVersReq,1]

	which(depMtrx[,1] %in% passPkgs)
    }
    else
	numeric()
}

foundDepends <- function(depMtrx, contriburl=getOption("repos")) {
    out <- list(Found=list())
    foundRows <- numeric()

    if(is.null(contriburl))
        contriburl <-
            utils::contrib.url(c(CRAN = getOption("repos")["CRAN"],
                                 BIOC = getOption("BIOC")))

    if (nrow(depMtrx) > 0)
        for (c.url in contriburl) {
            cur <- character()
            cran <- utils::available.packages(contriburl=c.url)

            for (i in 1L:nrow(depMtrx)) {
                found <- FALSE
                cranRow <- which(depMtrx[i,1] == cran[,1])
                if (length(cranRow)) {
                    ## Found it in repos
                    if (is.na(depMtrx[i,2])) # no version, automatically okay
                        found <- TRUE
                    else if(compareDependsPkgVersion(cran[cranRow, "Version"],
                                                     depMtrx[i,2],
                                                     depMtrx[i,3]))
                        found <- TRUE
                }
                if (found) {
                    foundRows <- c(foundRows,i)
                    cur <- c(cur,depMtrx[i,1])
                }
            }
            if (length(cur))
                out$Found[c.url] <- cur
        }


    if (length(foundRows) != nrow(depMtrx))
        out$NotFound <- depMtrx[-foundRows,,drop=FALSE]

    out
}

compareDependsPkgVersion <- function(curVersion, versOper, versionReq) {
    ## Returns -1 if FALSE, 0 or 1 if TRUE
    if(versOper == ">=")
        return(utils::compareVersion(curVersion, versionReq))
    if(versOper == "<=")
        return(utils::compareVersion(versionReq, curVersion))
    else
        stop("bad operand")
}

reduceDepends <- function(depMtrx, quietly=TRUE) {
    if ((is.null(depMtrx))||nrow(depMtrx)==0)
        return(character())

    pkgList <- split(depMtrx, depMtrx[,1])
    out <- lapply(pkgList, function(x, quietly) {
        pkgMtrx <- matrix(x,ncol=3L)
        ## there are no version requirements so just return
        ## the pkg name
        if (all(is.na(pkgMtrx[,2])))
            outRow <- 1
        else {
            ## Have version requirements
            ## Get the maximum ">=" requirement if one exists
            gts <- pkgMtrx[pkgMtrx[,2] == ">=",,drop=FALSE]
            if (nrow(gts) > 0) {
               maxGts <- gts[1,3]
               outRow <- 1
               for (i in 1L:nrow(gts)) {
                   if (utils::compareVersion(gts[i,3], maxGts) > 0) {
                       maxGts <- gts[i,3]
                       outRow <- i
                   }
               }
            }

            ## Find the minimal <= requirement if one exists
            lts <- pkgMtrx[pkgMtrx[,2] == "<=",,drop=FALSE]
            if (nrow(lts) > 0) {
                minLts <- lts[1,3]
                minRow <- 1
                for (i in 1L:nrow(lts)) {
                    if (utils::compareVersion(lts[i,3], minLts) < 0) {
                        minLts <- lts[i,3]
                        minRow <- i
                    }
                }
                ## If there is a maxGts and it is larger then
                ## the minLts then we need to record both
                if (exists(maxGts))
                    if (maxGts > minLts)
                        outRow <- c(outRow, minRow)
                else
                    outRow <- minRow
            }
            if(!quietly)
                warning(gettextf("Package '%s' had its dependencies reduced to a minimal set.",
                                 pkgMtrx[1,]),
                        domain = NA)
        }
	pkgMtrx[outRow,]
    }, quietly)

    matrix(unlist(out), ncol=3L, byrow=TRUE)
}

depMtrxToStrings <- function(depMtrx) {
    if (length(depMtrx)) {
        apply(depMtrx, 1L, function(x){
            if (is.na(x[2L]))
                x[1L]
            else
                paste0(x[1L]," (",x[2L]," ",x[3L],")")
        })
    }
    else
        character()
}

installFoundDepends <- function(depPkgList, ...) {

    .Deprecated(msg = "'installFoundDepends()' is deprecated: It is misleading nowadays
 since it has never taken 'Imports' into consideration")

    urls <- names(depPkgList)
    for (i in seq_along(depPkgList)) {
        if (length(depPkgList[[i]]))
            utils::install.packages(depPkgList[[i]],
                                    contriburl = urls[i],
                                    ...)
    }

    NULL
}


## <entry>
## Deprecated in 3.6.0 (r75... (12 Sep 2018)) -- should have belonged to prev.
vignetteDepends <-
    function(vignette, recursive = TRUE, reduce = TRUE,
             local = TRUE, lib.loc = NULL)
{
    .Deprecated("vignetteInfo()$depends or package_dependencies()")

    if (length(vignette) != 1L)
        stop("argument 'vignette' must be of length 1")
    if (!nzchar(vignette)) return(invisible()) # lets examples work.
    if (!file.exists(vignette))
        stop(gettextf("file '%s' not found", vignette), domain = NA)

    vigDeps <- vignetteInfo(vignette)$depends

    depMtrx <- getVigDepMtrx(vigDeps)
    instPkgs <- utils::installed.packages(lib.loc=lib.loc)
    getDepList(depMtrx, instPkgs, recursive, local, reduce)
}## ^^^^^^^^^^ deprecated since Feb. 2016


## </entry>
