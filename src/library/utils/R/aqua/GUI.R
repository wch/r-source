## Since 2.2 this function has no side-effect of installing packages,
## they are installed directly from the GUI.
browse.pkgs <- function (repos = getOption("repos"), contriburl = contrib.url(repos, type),
                         type = getOption("pkgType"))
{
  if (.Platform$GUI != "AQUA")
    stop("this function is intended to work with the Aqua GUI")
  x <- installed.packages()
  i.pkgs <- as.character(x[, 1])
  i.vers <- as.character(x[, 3])
  label <- paste("(", type, ") @", contriburl)
  y <- avaliable.packages(contriburl = contriburl)
  c.pkgs <- as.character(y[, 1])
  c.vers <- as.character(y[, 2])

  idx <- match(i.pkgs, c.pkgs)
  vers2 <- character(length(c.pkgs))
  xx <- idx[which(!is.na(idx))]
  vers2[xx] <- i.vers[which(!is.na(idx))]
  i.vers <- vers2

  want.update <- rep(FALSE, length(i.vers))
  .Internal(pkgbrowser(c.pkgs, c.vers, i.vers, label, want.update))
  ## we don't use the return value anymore - it was used to install packages
  ## but now it's for information only
}

data.manager <- function()
  {
    if (.Platform$GUI!="AQUA")
      stop("this function is intended to work with the Aqua GUI")

    data.by.name<-function(datanames){
      aliases<-sub("^.+ +\\((.+)\\)$","\\1",datanames)
      data(list=ifelse(aliases=="",datanames,aliases))
    }

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
      data.by.name(dt[i])
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

## flush.console <- function() {if (.Platform$GUI=="AQUA") .Internal(flush.console())}

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

main.help.url <- function () {
  .Script("sh", "help-links.sh", paste(tempdir(), paste(.libPaths(), collapse = " ")))
  make.packages.html()
  tmpdir <- paste("file://", tempdir(), "/.R", sep = "")
  url <- paste(tmpdir,"/doc/html/index.html", sep = "")
  options(main.help.url=url)
}
