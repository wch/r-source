packageStatus <- function(lib.loc = NULL, repositories = NULL, method)
{
    newestVersion <- function(x)
    {
        vers <- package_version(x)
	max <- vers[1]
        for (i in seq(along=vers)) if (max < vers[i]) max <- vers[i]
	which(vers == max)[1]
    }

    if(is.null(lib.loc))
        lib.loc <- .libPaths()
    if(is.null(repositories))
        repositories <- contrib.url(c(CRAN = getOption("CRAN"),
                                      BIOC = getOption("BIOC")))

    ## convert character matrices to dataframes
    char2df <- function(x)
    {
        y <- list()
        for(k in 1:ncol(x)) y[[k]] <- x[,k]
        attr(y, "names") <- colnames(x)
        attr(y, "row.names") <- y[[1]]
        class(y) <- "data.frame"
        y
    }

    y <- char2df(installed.packages(lib.loc = lib.loc))
    y[, "Status"] <- "ok"

    z <- available.packages(repositories, method)
    ## only consider the newest version of each package
    ## in the first repository where it appears
    ztab <- table(z[,"Package"])
    for(pkg in names(ztab)[ztab>1]){
        zrow <- which(z[,"Package"]==pkg)
        znewest <- newestVersion(z[zrow,"Version"])
        ## and now exclude everything but the newest
        z <- z[-zrow[-znewest],]
    }

    z <- cbind(z, Status = "not installed")
    z[z[,"Package"] %in% y$Package, "Status"] <- "installed"
    ## Careful: bundles can be partially installed!
    bundles <- which(!is.na(z[,"Bundle"]))
    for(bundle in bundles) {
        contains <- z[bundle, "Contains"]
        contains <- strsplit(contains, "[[:space:]]+")[[1]]
        if(all(contains %in% y$Package)) z[bundle, "Status"] <- "installed"
    }

    z <- char2df(z)
    z$Package <- ifelse(is.na(z$Bundle), z$Package, z$Bundle)
    attr(z, "row.names") <- z$Package

    for(k in 1:nrow(y)){
        pkg <- ifelse(is.na(y$Bundle[k]), y[k, "Package"], y[k, "Bundle"])
        if(pkg %in% z$Package) {
            if(package_version(y[k, "Version"]) <
               package_version(z[pkg, "Version"])) {
                y[k, "Status"] <- "upgrade"
            }
        } else {
            if(!(y[k, "Priority"] %in% "base")) y[k, "Status"] <- "unavailable"
        }
    }

    y$LibPath <- factor(y$LibPath, levels=lib.loc)
    y$Status <- factor(y$Status, levels=c("ok", "upgrade"))
    z$Repository <- factor(z$Repository, levels=repositories)
    z$Status <- factor(z$Status,
                       levels=c("installed", "not installed", "unavailable"))

    retval <- list(inst=y, avail=z)
    class(retval) <- "packageStatus"
    retval
}

summary.packageStatus <- function(object, ...)
{
    cat(gettext("\nInstalled packages:\n"))
    cat(  "-------------------\n")
    for(k in levels(object$inst$LibPath)){
        ok <- (object$inst$LibPath==k)
        cat("\n*** Library ", k, "\n", sep="")
        if(any(ok)){
            i <- object$inst
            Package <- ifelse(is.na(i$Bundle), i$Package,
                              paste(i$Bundle, i$Package, sep=":"))
            print(tapply(Package[ok], i$Status[ok],
                         function(x) sort(as.character(x))))
        }
    }
    cat(gettext("\n\nAvailable packages:\n"))
    cat(    "-------------------\n")
    cat(gettext("(each package appears only once)\n"))
    for(k in levels(object$avail$Repository)){
        cat("\n*** Repository ", k, "\n", sep="")
        ok <- object$avail$Repository==k
        if(any(ok))
            print(tapply(object$avail$Package[ok],
                         object$avail$Status[ok],
                         function(x) sort(as.character(x))))
    }
    invisible(object)
}

print.packageStatus <- function(x, ...)
{
    cat(gettext("Number of installed packages:\n"))
    print(table(x$inst$LibPath, x$inst$Status))

    cat(gettext("\nNumber of available packages (each package counted only once):\n"))
    print(table(x$avail$Repository, x$avail$Status))
    invisible(x)
}

update.packageStatus <-
    function(object, lib.loc=levels(object$inst$LibPath),
             repositories=levels(object$avail$Repository),
             ...)
{
    packageStatus(lib.loc=lib.loc, repositories=repositories)
}


upgrade <- function(object, ...)
    UseMethod("upgrade")

upgrade.packageStatus <- function(object, ask=TRUE, ...)
{
    update <- NULL
    old <- which(object$inst$Status == "upgrade")
    if(length(old) == 0) {
        cat(gettext("Nothing to do!\n"))
        return(invisible())
    }

    askprint <- function(x)
        write.table(x, row.names=FALSE, col.names=FALSE, quote=FALSE,
                    sep=" at ")

    haveasked <- character(0)
    if(ask) {
        for(k in old) {
            pkg <- ifelse(is.na(object$inst[k, "Bundle"]),
                          object$inst[k, "Package"],
                          object$inst[k, "Bundle"])
            tmpstring <- paste(pkg, as.character(object$inst[k, "LibPath"]))
            if(tmpstring %in% haveasked) next
            haveasked <- c(haveasked, tmpstring)
            cat("\n")
            cat(pkg, ":\n")
            askprint(object$inst[k,c("Version", "LibPath")])
            askprint(object$avail[pkg, c("Version", "Repository")])
            answer <- substr(readline("Update (y/N)?  "), 1, 1)
            if(answer == "y" | answer == "Y")
                update <-
                    rbind(update,
                          c(pkg, as.character(object$inst[k, "LibPath"]),
                            as.character(object$avail[pkg, "Repository"])))
        }
    } else {
        pkgs <- ifelse(is.na(object$inst[ ,"Bundle"]),
                       object$inst[ ,"Package"], object$inst[ ,"Bundle"])
        update <- cbind(pkgs, as.character(object$inst[ , "LibPath"]),
                        as.character(object$avail[pkgs, "Repository"]))
        update <- update[old, , drop=FALSE]
    }

    if(length(update) > 0) {
        for(repo in unique(update[,3])) {
            ok <- update[, 3] == repo
            install.packages(update[ok, 1], update[ok, 2], contriburl = repo)
        }
    }
}
