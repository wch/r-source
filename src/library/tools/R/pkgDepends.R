pkgDepends <- function(pkg, recursive=TRUE, local=TRUE,
                       reduce=TRUE, lib.loc=NULL) {
    if (length(pkg) != 1)
        stop("Argument 'pkg' must be of length 1")

    instPkgs <- utils::installed.packages(lib.loc=lib.loc)

    depMtrx <- getDepMtrx(pkg, instPkgs, local)
    if (is.null(depMtrx)) ## Package was not found
        stop("Package ", pkg, " was not found.")

    getDepList(depMtrx, instPkgs, recursive, local, reduce, lib.loc)
}

getDepList <- function(depMtrx, instPkgs, recursive=TRUE,
                       local=TRUE, reduce=TRUE, lib.loc=NULL) {
    out <- list(Depends=character(), Installed=character(),
                Found=list(), NotFound=character(),
                R=character())
    class(out) <- c("DependsList", class(out))

    if ((!is.matrix(depMtrx))&&(is.na(depMtrx))) ## no dependencies
        return(out)

    mtrxList <- buildDepList(depMtrx, instPkgs, recursive, lib.loc)

    if (local == FALSE) {
        toFind <- mtrxList$Depends[!apply(mtrxList$Depends, 1,
                                          isSatisfied,
                                          mtrxList$Installed),,drop=FALSE]

        if (reduce)
            toFind <- reduceDepends(toFind)

        if (length(toFind) > 0) {
            found <- foundDepends(toFind)
            out$Found <- found$Found
            mtrxList$NotFound <- found$NotFound
        }
    }

    if (reduce == TRUE) { ## Found and NotFound are already reduced
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
    triplets <- apply(instMtrx,1,paste,collapse=":")
    match(paste(dep,collapse=":"),triplets,nomatch=0) > 0
}

buildDepList <- function(depMtrx, instPkgs, recursive=TRUE,
                         lib.loc=NULL) {
    mtrxList <- list(Depends=matrix(nrow=0,ncol=3),
                     Installed=matrix(nrow=0,ncol=3), R=matrix(nrow=0,ncol=3))


    ## First check to see if there is a dependency on R
    ## If there is, then check it
    whichR <- which(depMtrx[,1] == "R")
    if (length(whichR) > 0) {
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
    else { ##recurse is FALSE
        mtrxList$Depends <- depMtrx
        mtrxList$Installed <- instDeps
    }

    mtrxList
}

getDepMtrx <- function(pkg, instPkgs, local=TRUE) {

    ## Need to see if pkg is installed - if not, get online
    row <- match(pkg,instPkgs[,"Package"])
    if (!is.na(row)) ## package is installed
        pkgDeps <- package.dependencies(instPkgs[row,])[[1]]
    else {
        if (local)
            pkgDeps <- NULL
        else
            pkgDeps <- getRemotePkgDepends(pkg)
    }

    pkgDeps  ## Either a matrix, NA if no deps or NULL if not found
}

getRemotePkgDepends <- function(pkg, contriburl=getOption("repositories")()[1]) {
    ## Will get the dependencies of a package from
    ## online repositories.  Returns NULL if it
    ## can not be found, otherwise returns the row provided
    ## in CRAN.packages().  Defaults to getting packages from CRAN,
    ## but other URLs can be specified.

    if(is.null(contriburl))
        contriburl <- contrib.url(getOption("CRAN"))

    cran <- CRAN.packages(contriburl=contriburl)
    whichRow <- which(pkg == cran[,"Package"])
    if (length(whichRow) > 0) {
        return(package.dependencies(cran[whichRow,])[[1]])
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
    if (length(pkgs) > 0) {
        installed <- (match(pkgs, instPkgs[,"Package"], nomatch=0) > 0)

        curPkgs <- depMtrx[installed,,drop=FALSE]
        if (nrow(curPkgs) > 0) {
            passVersReq <- apply(curPkgs, 1, function(x) {
                pkgVers <- instPkgs[instPkgs[,1]==x[1],"Version"]
                if (is.na(x[2])||
                    (compareDependsPkgVersion(pkgVers,
                                              x[2], x[3]) >= 0))
                    TRUE
                else
                    FALSE
            })
            passPkgs <- c(passPkgs,curPkgs[passVersReq,1])

            return(which(match(depMtrx[,1],passPkgs,nomatch=0) > 0))
        }
    }

    return(numeric())
}

foundDepends <- function(depMtrx, contriburl=getOption("repositories")()) {
    out <- list(Found=list())
    foundRows <- numeric()

    if(is.null(contriburl))
        contriburl <- contrib.url(c(CRAN = getOption("CRAN"),
                                      BIOC = getOption("BIOC")))


    for (j in 1:length(contriburl)) {
        cur <- character()
        cran <- CRAN.packages(contriburl=contriburl[j])

        if (nrow(depMtrx) > 0) {
            for (i in 1:nrow(depMtrx)) {
                found <- FALSE
                cranRow <- which(depMtrx[i,1] == cran[,1])
                if (length(cranRow) > 0) {
                    ## Found it in repos
                    if (is.na(depMtrx[i,2])) ## no version, automatically okay
                        found <- TRUE
                    else if(compareDependsPkgVersion(cran[cranRow,
                                                                  "Version"],
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

        if (length(cur) > 0)
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
        pkgMtrx <- matrix(x,nc=3)
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
               for (i in 1:nrow(gts)) {
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
                for (i in 1:nrow(lts)) {
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
            if (quietly == FALSE)
                warning("Package ",pkg," had its dependencies ",
                        "reduced to a minimal set.")
        }
	pkgMtrx[outRow,]
    }, quietly)

    matrix(unlist(out), nc=3, byrow=TRUE)
}

depMtrxToStrings <- function(depMtrx) {
    if ((!is.null(depMtrx))&&(nrow(depMtrx) > 0)) {
        apply(depMtrx, 1, function(x){
            if (is.na(x[2]))
                x[1]
            else
                paste(x[1]," (",x[2]," ",x[3],")",sep="")
        })
    }
    else
        character()
}

installFoundDepends <- function(depPkgList, ...) {
    urls <- names(depPkgList)
    for (i in seq(along=depPkgList)) {
        if (length(depPkgList[[i]]) > 0)
            install.packages(depPkgList[[i]], contriburl=urls[i],...)
    }

    NULL
}
