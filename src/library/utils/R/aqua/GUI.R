browse.pkgs <- function (where = c("CRAN", "BIOC"), type = c("binary", "source"),
    contriburl = NULL, global = FALSE)
{
    if (.Platform$GUI != "AQUA")
        stop("this function is intended to work with the Aqua GUI")
    where <- match.arg(where)
    type <- match.arg(type)
    x <- installed.packages()
    i.pkgs <- as.character(x[, 1])
    i.vers <- as.character(x[, 3])
        if (is.null(contriburl)) {
        if (type == "binary")
            contriburl <- contrib.url(getOption(where), type = "mac.binary")
        else contriburl <- contrib.url(getOption(where), type = "source")
        label <- switch(where, CRAN = paste("CRAN (", type, ") @",
            getOption(where)), BIOC = paste("BioC (", type, ") @",
            getOption(where)))
    }
    else label <- paste("(", type, ") @", contriburl)
    if (type == "binary")
        y <- CRAN.binaries(contriburl = contriburl)
    else y <- CRAN.packages(contriburl = contriburl)
    c.pkgs <- as.character(y[, 1])
    c.vers <- as.character(y[, 2])

    idx <- match(i.pkgs, c.pkgs)
    vers2 <- character(length(c.pkgs))
    xx <- idx[which(!is.na(idx))]
    vers2[xx] <- i.vers[which(!is.na(idx))]
    i.vers <- vers2

    want.update <- rep(FALSE, length(i.vers))
    inst <- .Internal(pkgbrowser(c.pkgs, c.vers, i.vers, label,
        want.update))
    ui.pkgs <- c.pkgs[inst]
    idx2 <- which(c.vers[inst] == i.vers[inst])
    if (length(idx2) > 0) {
        cat(paste(ui.pkgs[idx2], collapse = ""), " already up to date, not reinstalled\n")
        ui.pkgs <- ui.pkgs[-idx2]
    }
    if (global)
        locn <- file.path(R.home(), "library")
    else locn <- .libPaths()[1]
    if (length(ui.pkgs) > 0) {
        if (missing(contriburl)) {
            switch(type, source = install.packages(ui.pkgs, CRAN = getOption(where),
                lib = .libPaths()[1]), binary = install.binaries(ui.pkgs,
                CRAN = getOption(where), lib = .libPaths()[1]))
        }
        else {
            switch(type, source = install.packages(ui.pkgs, CRAN = getOption(where),
                contriburl = contriburl, lib = .libPaths()[1]),
                binary = install.binaries(ui.pkgs, CRAN = getOption(where),
                  contriburl = contriburl, lib = .libPaths()[1]))
        }
    }
}


    browse.update.pkgs <- function(where = c("CRAN", "BIOC"),
                                   type = c("binary", "source"),
                                   in.place = TRUE)
   {
     if (.Platform$GUI!="AQUA")
       stop("this function is intended to work with the Aqua GUI")
       where <- match.arg(where)
       type <- match.arg(type)

       installed.packages() -> x
       x[,1] -> i.pkgs
       x[,3] -> i.vers
       x[,2] -> i.locn
       if (type == "binary")
           CRAN.binaries(getOption(where))-> y
       else
           CRAN.packages(getOption(where)) -> y
       y[,1] -> c.pkgs
       y[,2] -> c.vers

       match(i.pkgs, c.pkgs) -> idx
       vers2 <- character(length(i.pkgs))
       vers2 <- c.vers[idx]
       c.vers <- vers2
       ask <- !is.na(idx) & c.vers != i.vers
       if (!any(ask)) {
           cat("Your",where,"packages are all up-to-date.\n")
           return(invisible(NULL))
       }
       i.pkgs <- i.pkgs[ask]
       i.vers <- i.vers[ask]
       c.vers <- c.vers[ask]
       want.update <- rep(TRUE, length(i.vers))
       label <- switch(where,
                       CRAN = paste("CRAN (", type, ") @", getOption(where)),
                       BIOC = paste("BioC (", type ,") @", getOption(where)))
       inst.idx <- which(.Internal(pkgbrowser(i.pkgs, c.vers, i.vers,
                                              label, want.update)))
       ui.pkgs <- i.pkgs[inst.idx]
       if(length(ui.pkgs) > 0) {
           if (in.place)
               locn <- i.locn[inst.idx]
           else
               locn <- .libPaths()[1]
           if (type == "source")
               mapply("install.packages", CRAN = getOption(where),
                      lib = locn, pkgs = ui.pkgs)
           else
               mapply("install.binaries", CRAN = getOption(where),
                      lib = locn, pkgs = ui.pkgs)
       }
   }

"data_by_name"<-function(datanames){
  aliases<-sub("^.+ +\\((.+)\\)$","\\1",datanames)
  data(list=ifelse(aliases=="",datanames,aliases))
}

    data.manager <- function()
    {
     if (.Platform$GUI!="AQUA")
       stop("this function is intended to work with the Aqua GUI")
        data(package = .packages(all.available = TRUE)) -> x
        x$results[,3] -> dt
        x$results[,1] -> pkg
        x$results[,4] -> desc
		len <- NROW(dt)
		url <- character(len)
		for(i in 1:len){
			tmp <- as.character(help(dt[i], package = pkg[i], htmlhelp=TRUE))
			if(length(tmp)>0)
				url[i] <- tmp
		}
		as.character(help("BOD", package="datasets",htmlhelp=T))
        load.idx <- which(.Internal(data.manager(dt,pkg,desc,url)))

        for(i in load.idx) {
            cat("loading dataset:", dt[i],"\n")
            data_by_name( dt[i])
        }
    }

    package.manager <- function()
    {
     if (.Platform$GUI!="AQUA")
       stop("this function is intended to work with the Aqua GUI")
        .packages() -> loaded.pkgs
        library() -> x
        x <- x$results[x$results[,1] != "base",]
        x[,1] -> pkgs
        x[,3] -> pkgs.desc

        is.loaded <- !is.na(match(pkgs,loaded.pkgs))
        pkgs.status <- character(length(is.loaded))
        pkgs.status[which(is.loaded)] <- "loaded"
        pkgs.status[which(!is.loaded)] <- " "
 	pkgs.url <- file.path(.find.package(pkgs),"html","00Index.html")
        load.idx <- .Internal(package.manager(is.loaded,pkgs,pkgs.desc,pkgs.url))

        toload <- which(load.idx & !is.loaded)
        tounload <- which(is.loaded & !load.idx)

        for(i in tounload) {
            cat("unloading package:", pkgs[i],"\n")
            do.call("detach",list(paste("package", pkgs[i], sep = ":")))
        }
        for(i in toload) {
            cat("loading package:", pkgs[i],"\n")
            library(pkgs[i],character.only = TRUE)
        }

    }

flush.console <- function() {if (.Platform$GUI=="AQUA") .Internal(flush.console())}

print.hsearch <- function(x,...)
  {
        if (.Platform$GUI=="AQUA"){
          db <- x$matches
		  rows <- NROW(db)
          if (rows == 0) {
            writeLines(strwrap(paste("No help files found matching",
                                     sQuote(x$pattern), "using", x$type,
                                     "matching\n\n")))
          } else {
			url = character(rows)
			for(i in 1:rows){
				tmp <- as.character(help(db[i,"topic"], package = db[i,"Package"], htmlhelp=TRUE))
				if(length(tmp)>0)
					url[i] <- tmp
			}
            wtitle <- paste("Help topics matching", sQuote(x$pattern))
            showhelp <- which(.Internal(hsbrowser(db[,"topic"], db[,"Package"],
							db[,"title"],  wtitle, url )))
            for(i in showhelp)
              print(help(db[i,"topic"], package = db[i,"Package"]))
          }
          invisible(x)
        } else
          printhsearchInternal(x,...)
}

Rapp.updates <- function() {
  if (.Platform$GUI!="AQUA")
       stop("this function is intended to work with the Aqua GUI")
 readLines("http://cran.r-project.org/bin/macosx/VERSION") -> cran.ver

 strsplit(cran.ver,"\\.") -> ver
 cran.ver <- as.numeric(ver[[1]])

 paste(R.Version()$major,".",R.version$minor,sep="") -> rapp.ver

 strsplit(rapp.ver,"\\.") -> ver
 rapp.ver <- as.numeric(ver[[1]])

 this.ver <- sum(rapp.ver * c(10000,100,1))
 new.ver <- sum(cran.ver * c(10000,100,1))
 if (new.ver > this.ver) {
  cat("\nThis version of R is",paste(rapp.ver,collapse="."))
  cat("\nThere is a newer version of R on CRAN which is",paste(cran.ver,collapse="."), "\n")

  action <- readline("Do you want to visit CRAN now? ")
  if (substr(action, 1, 1) == "y")
   system("open http://cran.r-project.org/bin/macosx/")
  } else { cat("\nYour version of R is up to date\n")}

}




## edited from windows/install.packages
##
install.binaries <- function(pkgs, lib, CRAN=getOption("CRAN"),
                             contriburl=contrib.url(CRAN, type="mac.binary"),
                             method, available=NULL, destdir=NULL,
                             installWithVers=FALSE)
  {

    link.html.help<-function(...,verbose=FALSE)
      {
        html<-getOption("htmlhelp")
        if (!is.null(html) && html)
          make.packages.html()
      }
    untar<-function(what, where)
      {
        xcode <- system(paste("tar zxf", what, "-C", where), intern=FALSE)
        if (xcode) warning("tar returned non-zero exit code: ",xcode)
      }

    ## edited from windows download.packages
    download.binaries <- function(pkgs, destdir, available=NULL,
                                  CRAN=getOption("CRAN"),
                                  contriburl=contrib.url(CRAN,type="mac.binary"),
                                  method)
      {
        dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir

        if(!dirTest(destdir)) stop("destdir is not a directory")
        localcran <- length(grep("^file:", contriburl)) > 0
        if(is.null(available))
          available <- CRAN.packages(contriburl=contriburl, method=method)

        retval <- NULL
        for(p in unique(pkgs))
          {
            ok <- (available[,"Package"] == p) | (available[,"Bundle"] == p)
            ok <- ok & !is.na(ok)
            if(!any(ok))
              warning("no package ", sQuote(p), " on CRAN")
            else{
              fn <- paste(p, "_", available[ok, "Version"], ".tgz", sep="")
              ##fn<-paste(p,".tgz",sep="")
              if(localcran){
                fn <- paste(substring(contriburl, 6), fn, sep="/")
                retval <- rbind(retval, c(p, fn))
              }
              else{
                url <- paste(contriburl, fn, sep="/")
                destfile <- file.path(destdir, fn)

                if(download.file(url, destfile, method, mode="wb") == 0)
                  retval <- rbind(retval, c(p, destfile))
                else
                  warning("Download of package", sQuote(p), "failed")
              }
                }
          }

        retval
      }

    unpackPkg <- function(pkg, pkgname, lib, installWithVers=FALSE)
      {

        ## Create a temporary directory and unpack the zip to it
        ## then get the real package & version name, copying the
        ## dir over to the appropriate install dir.
            tmpDir <- tempfile(, lib)
            if (!dir.create(tmpDir))
              stop('Unable to create temp directory ', tmpDir)
            cDir <- getwd()
            on.exit(setwd(cDir), add = TRUE)
            res <- untar(pkg, tmpDir)
            setwd(tmpDir)
            res <- tools::checkMD5sums(pkgname, file.path(tmpDir,pkgname))
            if(!is.na(res) && res)
              cat("package ", pkgname,
                  " successfully unpacked and MD5 sums checked\n")

            ## Check to see if this is a bundle or a single package
            if (file.exists("DESCRIPTION")) {
              ## Bundle
              conts <- read.dcf("DESCRIPTION",fields="Contains")[1,]
              if (is.na(conts))
                stop("malformed bundle DESCRIPTION file, no Contains field")
              else
                pkgs <- strsplit(conts," ")[[1]]
            } else pkgs <- pkgname

            for (curPkg in pkgs) {
              desc <- read.dcf(file.path(curPkg, "DESCRIPTION"),
                               c("Package", "Version"))
              if (installWithVers) {
                instPath <- file.path(lib, paste(desc[1,1], desc[1,2], sep="_"))
              }
              else instPath <- file.path(lib, desc[1,1])

              ## If the package is already installed w/ this
              ## instName, remove it.  If it isn't there, the unlink call will
              ## still return success.
              ret <- unlink(instPath, recursive=TRUE)
              if (ret == 0) {
                ## Move the new package to the install lib and
                ## remove our temp dir
                file.rename(file.path(tmpDir, curPkg), instPath)
              } else {
                ## !! Can't revert to old 'zip.unpack' as it would
                ## !! potentially leave cruft from a bundle in there
                stop("cannot remove prior installation of package")
              }
            }
            setwd(cDir)
            unlink(tmpDir, recursive=TRUE)
          }

    if(!length(pkgs)) return(invisible())
    if(missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1]
        if(length(.libPaths()) > 1)
            warning("argument 'lib' is missing: using ", lib)
    }
    pkgnames <- basename(pkgs)
    pkgnames <- sub("\\.tgz$", "", pkgnames)
    pkgnames <- sub("_[0-9.-]+$", "", pkgnames)
        ## there is no guarantee we have got the package name right:
    ## foo.zip might contain package bar or Foo or FOO or ....
    ## but we can't tell without trying to unpack it.
    if(is.null(CRAN) & missing(contriburl)) {
      for(i in seq(along=pkgs)) {
        unpackPkg(pkgs[i], pkgnames[i], lib, installWithVers)
      }
      link.html.help(verbose=TRUE)
      return(invisible())
    }
    localcran <- length(grep("^file:", contriburl)) > 0
    if(!localcran) {
      if (is.null(destdir)){
        tmpd <- tempfile("Rinstdir")
        if (!dir.create(tmpd))
          stop('Unable to create temp directory ', tmpd)
      } else tmpd <- destdir
    }

    foundpkgs <- download.binaries(pkgs, destdir=tmpd,
                                   available=available,
                                   contriburl=contriburl, method=method)

    if(!is.null(foundpkgs))
      {
        update <- cbind(pkgs, lib)
        colnames(update) <- c("Package", "LibPath")
        for(lib in unique(update[,"LibPath"]))
          {
            oklib <- lib==update[,"LibPath"]
            for(p in update[oklib, "Package"])
              {
                okp <- p == foundpkgs[, 1]
                if(length(okp) > 0)
                  unpackPkg(foundpkgs[okp, 2], pkgnames[okp], lib,
                            installWithVers)
              }
          }
        cat("\n")
        if(!localcran && is.null(destdir)){
          ## I think we want to always delete the temporary files
          ##answer <- substr(readline("Delete downloaded files (y/N)? "), 1,1)
          answer <- "y"
          if(answer == "y" | answer == "Y") {
            for(file in foundpkgs[, 2]) unlink(file)
            unlink(tmpd, recursive=TRUE)
          } else
          cat("The packages are in", tmpd)
          cat("\n")
        }
        link.html.help(verbose=TRUE)
      }
    else
      unlink(tmpd, recursive=TRUE)
    invisible()
  }

install.from.file <- function(pkg = file.choose(), binary=FALSE)
  {
    if (binary){
      install.binaries(CRAN=NULL, pkg=pkg, lib=.libPaths()[1])
    }

    lib <- .libPaths()[1]
    cmd <- paste(file.path(R.home(), "bin", "R"), "CMD INSTALL")
    cmd <- paste(cmd, "-l", lib)
    cmd <- paste(cmd," '",pkg,"'",sep = "")
    status <- system(cmd)
    if(status == 0)
      cat("\tpackage successfully installed\n")
    else
      cat("\tnpackage installation failed\n")
  }

CRAN.binaries <- function(CRAN=getOption("CRAN"), method,
                          contriburl=contrib.url(CRAN, type="mac.binary"))
  {
    localcran <- length(grep("^file:", contriburl)) > 0
    if(localcran)
      tmpf <- paste(substring(contriburl,6), "PACKAGES", sep="/")
    else{
      tmpf <- tempfile()
      on.exit(unlink(tmpf))
      download.file(url=paste(contriburl, "PACKAGES", sep="/"),
                    destfile=tmpf, method=method, cacheOK=FALSE)
    }
    read.dcf(file=tmpf, fields=c("Package", "Version"))
  }

main.help.url <- function () {
    .Script("sh", "help-links.sh", paste(tempdir(), 	paste(.libPaths(),
        collapse = " ")))
    make.packages.html()
    tmpdir <- paste("file://", tempdir(), "/.R", sep = "")
    url <- paste(tmpdir,"/doc/html/index.html", sep = "")
	options(main.help.url=url)
}

