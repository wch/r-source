packageStatus <- function(lib.loc = NULL,
                           repositories = getOption("repositories")())
{
    .checkRversion <- function(x) {
        if(is.na(xx <- x["Depends"])) return(TRUE)
        xx <- tools:::.split_dependencies(xx)
        if(length(z <- xx[["R"]]) > 1)
            eval(parse(text=paste("currentR", z$op, "z$version")))
        else TRUE
    }

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

    FIELDS <- c("Package", "Version","Priority", "Bundle", "Depends",
                "Built", "Status", "Contains")
    FIELDS1 <- c(FIELDS, "LibPath")
    FIELDS2 <- c(FIELDS, "Repository")

    ## convert character matrices to dataframes
    char2df <- function(x)
    {
        y <- list()
        for(k in 1:ncol(x)) y[[k]] <- x[,k]
        attr(y, "names") <- colnames(x)
        attr(y, "row.names") <- 1:nrow(x)
        class(y) <- "data.frame"
        y
    }

    y <- NULL
    for(lib in lib.loc)
    {
        pkgs <- .packages(all.available=TRUE, lib.loc = lib)
        for(p in pkgs){
            desc <- unlist(packageDescription(p, lib=lib, fields=FIELDS))
            desc["Package"] <-
                ifelse(is.na(desc["Bundle"]),
                       desc["Package"],
                       paste(desc["Bundle"], desc["Package"], sep=":"))
            y <- rbind(y, c(desc, lib))
        }
    }

    y[,"Status"] <- "ok"
    y <- char2df(y)
    names(y) <- FIELDS1

    if(length(repositories) > 0) {
        currentR <- getRversion()
        repositories <- unique(as.character(repositories))
        z <- matrix("", nrow = 0, ncol = length(FIELDS2))
        colnames(z) <- FIELDS2
        for(rep in repositories){
            z1 <- try(read.dcf(paste(rep, "PACKAGES", sep = "/"),
                               fields = FIELDS2), silent = TRUE)
            if(inherits(z1, "try-error")) {
                cat("Warning: unable to access index for repository",
                    rep, "\n")
                repositories <- repositories[repositories != rep]
                next
            }

            ## ignore packages which don't fit our version of R
            z1 <- z1[apply(z1, 1, .checkRversion),,drop=FALSE]
            if(length(z1)==0) next

            z1[,"Repository"] <- rep
            z <- rbind(z[,FIELDS2], z1[,FIELDS2])
        }
    }

    ## only consider the newest version of each package
    ## in the first repository where it appears
    ztab <- table(z[,"Package"])
    for(pkg in names(ztab)[ztab>1]){
        zrow <- which(z[,"Package"]==pkg)
        znewest <- newestVersion(z[zrow,"Version"])
        ## and now exclude everything but the newest
        z <- z[-zrow[-znewest],]
    }

    z[,"Status"] <- "not installed"
    z[z[,"Package"] %in% y$Package, "Status"] <- "installed"
    ## Careful: bundles can be partially installed!
    ## z[!is.na(z[,"Bundle"]) & (z[,"Bundle"] %in% y$Bundle), "Status"] <- "installed"
    bundles <- which(!is.na(z[,"Bundle"]))
    for(bundle in bundles) {
        contains <- z[bundle, "Contains"]
        contains <- strsplit(contains, "[[:space:]]+")[[1]]
        if(all(paste(z[bundle, "Bundle"], contains, sep=":") %in% y$Package))
           z[bundle, "Status"] <- "installed"
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

    y$LibPath <- factor(as.character(y$LibPath), levels=lib.loc)
    y$Status <- as.factor(y$Status)
    z$Repository <- factor(as.character(z$Repository), levels=repositories)
    z$Status <- as.factor(z$Status)

    retval <- list(inst=y, avail=z)
    class(retval) <- "packageStatus"
    retval
}

summary.packageStatus <- function(object, ...)
{
    cat("\nInstalled packages:\n")
    cat(  "-------------------\n")
    for(k in levels(object$inst$LibPath)){
        ok <- (object$inst$LibPath==k)
        cat("\n*** Library ", k, "\n", sep="")
        if(any(ok)){
            print(tapply(object$inst$Package[ok],
                         object$inst$Status[ok],
                         function(x) sort(as.character(x))))
        }
    }
    cat("\n\nAvailable packages:\n")
    cat(    "-------------------\n")
    cat("(each package appears only once)\n")
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
    cat("Number of installed packages:\n")
    print(table(x$inst$LibPath, x$inst$Status))

    cat("\nNumber of available packages (each package counted only once):\n")
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
        cat("Nothing to do!\n")
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
            askprint(object$inst[k,c("Version","LibPath")])
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





