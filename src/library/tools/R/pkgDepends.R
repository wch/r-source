#  File src/library/tools/R/pkgDepends.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  http://www.r-project.org/Licenses/

pkgDepends <- function(pkg, recursive=TRUE, local=TRUE,
                       reduce=TRUE, lib.loc=NULL) {
    if (length(pkg) != 1L)
        stop("argument 'pkg' must be of length 1")

    instPkgs <- utils::installed.packages(lib.loc=lib.loc)

    depMtrx <- getDepMtrx(pkg, instPkgs, local)
    if (is.null(depMtrx))               # Package was not found
        stop(gettextf("package '%s' was not found", pkg),
             domain = NA)

    getDepList(depMtrx, instPkgs, recursive, local, reduce, lib.loc)
}

getDepList <- function(depMtrx, instPkgs, recursive=TRUE,
                       local=TRUE, reduce=TRUE, lib.loc=NULL) {
    out <- list(Depends=character(), Installed=character(),
                Found=list(), NotFound=character(),
                R=character())
    class(out) <- c("DependsList", class(out))

    if ((!is.matrix(depMtrx))&&(is.na(depMtrx))) # no dependencies
        return(out)

    mtrxList <- buildDepList(depMtrx, instPkgs, recursive, lib.loc)

    if (local == FALSE) {
        toFind <- mtrxList$Depends[!apply(mtrxList$Depends, 1,
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

    if (reduce == TRUE) {       # Found and NotFound are already reduced
        mtrxList$R <- reduceDepends(mtrxList$R)
        mtrxList$Depends <- reduceDepends(mtrxList$Depends)
        mtrxList$Installed <- reduceDepends(mtrxList$Installed)
    }


    ## Massage the matrices back into dependency strings.  out$Found
    ## is already assigned.
    out$R <- depMtrxToStrings(mtrxList$R)
    out$Depends <- depMtrxToStrings(mtrxList$Depends)
    out$Installed <- depMtrxToStrings(mtrxList$Installed)
    out$NotFound <- depMtrxToStrings(mtrxList$NotFound)

    out
}

isSatisfied <- function(dep, instMtrx) {
    triplets <- apply(instMtrx, 1L, paste, collapse=":")
    match(paste(dep,collapse=":"), triplets, nomatch=0L) > 0L
}

buildDepList <- function(depMtrx, instPkgs, recursive=TRUE,
                         lib.loc=NULL) {
    mtrxList <- list(Depends=matrix(nrow=0L,ncol=3L),
                     Installed=matrix(nrow=0L,ncol=3L), R=matrix(nrow=0L,ncol=3L))


    ## First check to see if there is a dependency on R
    ## If there is, then check it
    whichR <- which(depMtrx[,1] == "R")
    if (length(whichR)) {
        mtrxList$R <- depMtrx[whichR,,drop=FALSE]
        depMtrx <- depMtrx[-whichR,,drop=FALSE]
    }

    ## Get which of the direct depends are installed
    instDeps <- depMtrx[installedDepends(depMtrx, instPkgs),,drop=FALSE]

    if (recursive == TRUE) {
        mtrxList$Depends <- depMtrx
        mtrxList$Installed <- instDeps

        for (curPkg in depMtrx[,1]) {
            depMtrx <- getDepMtrx(curPkg, instPkgs)
            ## Make sure this package was found & has deps
            if ((is.null(depMtrx))||(is.na(depMtrx)))
                next

            curMtrxList <- buildDepList(depMtrx, instPkgs,
                                             recursive=recursive,
                                             lib.loc=lib.loc)
            mtrxList$R <- rbind(mtrxList$R, curMtrxList$R)
            mtrxList$Depends <- rbind(mtrxList$Depends,
                                      curMtrxList$Depends)
            mtrxList$Installed <- rbind(mtrxList$Installed,
                                        curMtrxList$Installed)
        }
    }
    else {                              # recurse is FALSE
        mtrxList$Depends <- depMtrx
        mtrxList$Installed <- instDeps
    }

    mtrxList
}

getDepMtrx <- function(pkg, instPkgs, local=TRUE) {

    ## Need to see if pkg is installed - if not, get online
    row <- match(pkg,instPkgs[,"Package"])
    if (!is.na(row))                    # package is installed
        pkgDeps <- package.dependencies(instPkgs[row,])[[1L]]
    else {
        if (local)
            pkgDeps <- NULL
        else
            pkgDeps <- getRemotePkgDepends(pkg)
    }

    pkgDeps        # Either a matrix, NA if no deps or NULL if not found
}

getRemotePkgDepends <- function(pkg, contriburl=getOption("repos")) {
    ## Will get the dependencies of a package from
    ## online repositories.  Returns NULL if it
    ## cannot be found, otherwise returns the row provided
    ## by available.packages().

    if(is.null(contriburl))
        contriburl <- utils::contrib.url(getOption("repos"))

    cran <- utils::available.packages(contriburl=contriburl)
    whichRow <- which(pkg == cran[,"Package"])
    if (length(whichRow)) {
        return(package.dependencies(cran[whichRow,])[[1L]])
    }
    else
        NULL
}

installedDepends <- function(depMtrx, instPkgs) {
    ## Given a matrix of packages, will return a vector of row
    ## numbers that correspond to packages in the matrix where
    ## the dependency is met by installed packages

    pkgs <- depMtrx[,1]
    passPkgs <- character()
    if (length(pkgs)) {
        installed <- (match(pkgs, instPkgs[,"Package"], nomatch=0L) > 0L)

        curPkgs <- depMtrx[installed,,drop=FALSE]
        if (nrow(curPkgs)) {
            passVersReq <- apply(curPkgs, 1L, function(x) {
                pkgVers <- instPkgs[instPkgs[,1]==x[1L],"Version"]
                if (is.na(x[2L])||
                    (compareDependsPkgVersion(pkgVers,
                                              x[2L], x[3L]) >= 0))
                    TRUE
                else
                    FALSE
            })
            passPkgs <- c(passPkgs,curPkgs[passVersReq,1])

            return(which(match(depMtrx[,1],passPkgs,nomatch=0L) > 0L))
        }
    }

    return(numeric())
}

foundDepends <- function(depMtrx, contriburl=getOption("repos")) {
    out <- list(Found=list())
    foundRows <- numeric()

    if(is.null(contriburl))
        contriburl <-
            utils::contrib.url(c(CRAN = getOption("repos")["CRAN"],
                                 BIOC = getOption("BIOC")))


    for (j in seq_along(contriburl)) {
        cur <- character()
        cran <- utils::available.packages(contriburl=contriburl[j])

        if (nrow(depMtrx) > 0) {
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
        }

        if (length(cur))
            out$Found[contriburl[j]] <- cur
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
            if(quietly == FALSE)
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
    urls <- names(depPkgList)
    for (i in seq_along(depPkgList)) {
        if (length(depPkgList[[i]]))
            utils::install.packages(depPkgList[[i]],
                                    contriburl = urls[i],
                                    ...)
    }

    NULL
}
