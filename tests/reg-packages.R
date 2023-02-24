.R_LIBS <- function(libp = .libPaths()) { # (>> in utils?)
    libp <- libp[! libp %in% .Library]
    if(length(libp))
        paste(libp, collapse = .Platform$path.sep)
    else "" # character(0) is invalid for Sys.setenv()
}
.libPaths(tail(.libPaths(), 1), include.site=FALSE) # no extra libraries (w/ many pkgs)\n")

Sys.setenv(R_LIBS = .R_LIBS() # for build.pkg() & install.packages()
         , R_BUILD_ENVIRON = "nothing" # avoid ~/.R/build.environ which might set R_LIBS
         , R_ENVIRON = "none"
         , R_PROFILE = "none"
           )
showProc.time <- local({ ## function + 'pct' variable
    pct <- proc.time()
    function(final="\n") { ## CPU elapsed __since last called__
	ot <- pct ; pct <<- proc.time()
	cat('Time elapsed: ',
	    format.default(round((pct - ot)[1:3], digits=3), digits=4),
	    final)
    }
})

## PR 1271  detach("package:base") crashes R.
tools::assertError(detach("package:base"))


## invalid 'lib.loc'
stopifnot(length(installed.packages("mgcv")) == 0)
## gave a low-level error message
showProc.time()



## package.skeleton() with metadata-only code
## work in current (= ./tests/ directory):
tmp <- tempfile()
writeLines(c('setClass("foo", contains="numeric")',
             'setMethod("show", "foo",',
             '          function(object) cat("I am a \\"foo\\"\\n"))'),
           tmp)
if(file.exists("myTst")) unlink("myTst", recursive=TRUE)
package.skeleton("myTst", code_files = tmp)# with a file name warning
file.copy(tmp, (tm2 <- paste(tmp,".R", sep="")))
unlink("myTst", recursive=TRUE)
op <- options(warn=2) # *NO* "invalid file name" warning {failed in 2.7.[01]}:
package.skeleton("myTst", code_files = tm2)
options(op)
##_2_ only a class, no generics/methods:
writeLines(c('setClass("DocLink",',
             'representation(name="character",',
             '               desc="character"))'), tmp)
if(file.exists("myTst2")) unlink("myTst2", recursive=TRUE)
package.skeleton("myTst2", code_files = tmp)
##- end_2_ # failed in R 2.11.0
stopifnot(1 == grep("setClass",
		    readLines(list.files("myTst/R", full.names=TRUE))),
	  c("foo-class.Rd","show-methods.Rd") %in% list.files("myTst/man"))
## failed for several reasons in R < 2.7.0
##
## Part 2: -- build, install, load and "inspect" the package:
build.pkg <- function(dir, destdir = NULL, ignore.stderr = FALSE, no.latex=TRUE) {
    dir <- normalizePath(dir)
    if(length(dir) > 1)
        return(lapply(dir, build.pkg, destdir=destdir,
                      ignore.stderr=ignore.stderr, no.latex=no.latex))
    ## else one 'dir':
    stopifnot(dir.exists(dir), file.exists(DESC <- file.path(dir, "DESCRIPTION")))
    pkgName <- sub("^[A-Za-z]+: ", "", grep("^Package: ", readLines(DESC), value=TRUE))
    patt <- paste(pkgName, ".*tar\\.gz$", sep="_")
    unlink(dir('.', pattern = patt))
    Rcmd <- paste(shQuote(file.path(R.home("bin"), "R")), "CMD")
    r <- system(paste(Rcmd, "build --keep-empty-dirs",
                      if(no.latex) "--no-manual", shQuote(dir)),
                ignore.stderr=ignore.stderr, intern = TRUE)
    ## return name of tar file built {plus the build log} :
    tball <- structure(dir('.', pattern = patt), log3 = r)
    if(is.null(destdir))
        tball
    else {
        file.copy(tball, destdir)
        file.path(destdir, basename(tball))
    }
}
build.pkg("myTst")
## clean up any previous attempt (which might have left a 00LOCK)
unlink("myLib", recursive = TRUE)
dir.create("myLib")
install.packages("myTst", lib = "myLib", repos=NULL, type = "source") # with warnings
print(installed.packages(lib.loc= "myLib", priority= "NA"))## (PR#13332)
stopifnot(require("myTst",lib = "myLib"))
sm <- findMethods(show, where= as.environment("package:myTst"))
stopifnot(sm@names == "foo")
unlink("myTst_*")

## getPackageName()  for "package:foo":
require('methods')
library(tools)
oo <- options(warn=2)
detach("package:tools", unload=TRUE)
options(oo)
## gave warning (-> Error) about creating package name


## More building & installing packages
## NB: tests were added here for 2.11.0.
## NB^2: do not do this in the R sources (but in a build != src directory!)
## and this testdir is not installed.
if(interactive() && Sys.getenv("USER") == "maechler")
    Sys.setenv(SRCDIR = normalizePath("~/R/D/r-devel/R/tests"))
(pkgSrcPath <- file.path(Sys.getenv("SRCDIR"), "Pkgs"))# e.g., -> "../../R/tests/Pkgs"
## SRCDIR not available on windows, so pkgSrcPath won't be populated
## if this happens non-interactively, cleanup and quit gracefully
if(!file_test("-d", pkgSrcPath) && !interactive()) {
    unlink("myTst", recursive=TRUE)
    showProc.time()
    q("no")
}
## else w/o clause:
showProc.time()

do.cleanup <- !nzchar(Sys.getenv("R_TESTS_NO_CLEAN"))
isWIN <- .Platform$OS.type == "windows"
has.symlink <- !isWIN
## Installing "on to" a package existing as symlink in the lib.loc
## -- used to fail with misleading error message (#PR 16725):

if(has.symlink && !unlink("myLib_2", recursive=TRUE) && dir.create("myLib_2") &&
   file.rename("myLib/myTst", "myLib_2/myTst") &&
   file.symlink("../myLib_2/myTst", "myLib/myTst"))
    install.packages("myTst", lib = "myLib", repos=NULL, type = "source")
## In R <= 3.3.2 gave error with *misleading* error message:
## ERROR: ‘myTst’ is not a legal package name

if(isWIN) { # (has no symlinks anyway)
    file.copy(pkgSrcPath, tempdir(), recursive = TRUE)
} else { # above file.copy() not useful as it replaces symlink by copy
    system(paste('cp -R', shQuote(pkgSrcPath), shQuote(tempdir())))
}
pkgPath <- file.path(tempdir(), "Pkgs")
if(!dir.exists(pkgPath))  {
    message("No valid 'pkgPath' (from 'pkgSrcPath') - exit this test")
    if(!interactive()) q("no")
}

## pkgB tests an empty R directory
dir.create(file.path(pkgPath, "pkgB", "R"), recursive = TRUE,
           showWarnings = FALSE)
## (how can this happen reliably more easily?)
##' Copy directory d1 to  *(newly created |cleaned)* directory d2 -- also with*in* same directory
##'     "cp -a d1/ d2/"
dirCopy <- function(d1, d2) {
    stopifnot(exprs = {
        length(d1) == length(d2) ; length(d1) == 1L
        dir.exists(d1)
        dir.exists(D2 <- dirname(d2)) || dir.create(D2, recursive=TRUE)
        !dir.exists(d2) || unlink(d2, recursive=TRUE) == 0
        ## unbelievable contortion just so it works within the *same* directory  (really ??)
        ## 1. copy to *other dir* :
        dir.create(tD <- tempfile("dirCp"))
        file.copy(d1, tD, recursive = TRUE)
        ## 2. correctly "rename" ( i.e., *move*) to "this" dir:
        file.rename(file.path(tD, basename(d1)),
                    file.path(D2, basename(d2)))
    })
}
##
## pkgB{2,3} := pkgB but with missing/incomplete 'Imports:' entry in DESCRIPTION
              pBp <- file.path(pkgPath, "pkgB")
dirCopy(pBp, pB2p <- file.path(pkgPath, "pkgB2"))
dirCopy(pBp, pB3p <- file.path(pkgPath, "pkgB3"))
if(okB2 <- file.exists(DN <- file.path(pB2p, "DESCRIPTION"))) {
  Dlns <- readLines(DN); i <- grep("^Imports:", Dlns)
  ## drop 'Imports: ' completely (and replace [Pp]kgB by [Pp]kgB2):
  writeLines(gsub("kgB", "kgB2", Dlns[-i]), con = DN)
}
if(okB3 <- file.exists(DN <- file.path(pB3p, "DESCRIPTION"))) {
  Dlns <- readLines(DN); i <- grep("^Imports:", Dlns)
  ## Only keep the first 'Imports: '  (and replace [Pp]kgB by [Pp]kgB3):
  Dlns[i] <- sub(",.*", "", Dlns[i])
  writeLines(gsub("kgB", "kgB3", Dlns), con = DN)
}
## PR17859.3 := PR17859.2, but with missing '}' in <pkg>/R/f3.3 :
pP2 <- file.path(pkgPath, "PR17859.2"); f2nm <- file.path(pP2, "R", "f3.R")
pP3 <- file.path(pkgPath, "PR17859.3"); f3nm <- file.path(pP3, "R", "f3.R")
dirCopy(pP2, pP3); DN <- "DESCRIPTION" # --> fix up pkg name
writeLines(sub("^(Package: .*)\\.2$", "\\1.3", readLines(file.path(pP2, DN))),
           file.path(pP3, DN))
f3lns <- f2lns <- readLines(f2nm)
iBrace <- grep("closing brace", f2lns, fixed=TRUE)
(f3lns[iBrace] <- sub("^", "#> ", f2lns[iBrace]))
(writeLines(f3lns, f3nm))
p.fails <- paste0("PR17859.", 1:3)
io859 <- c("--no-help", "--no-test-load", "--no-byte-compile")
InstOpts <- list("exSexpr" = "--html")
for(p in p.fails) InstOpts <- c(InstOpts, `names<-`(list(io859), p))
p.lis <- c(if("Matrix" %in% row.names(installed.packages(.Library)))
               c("pkgA", "pkgB", if(okB2) "pkgB2", if(okB3) "pkgB3", "pkgC"),
           "PR17501",
           p.fails,
           "exNSS4", "exNSS4nil", "exSexpr")
p.lis; (pBlis <- grep("^pkgB", p.lis, value=TRUE))
pkgApath <- file.path(pkgPath, "pkgA")
if("pkgA" %in% p.lis && !dir.exists(d <- pkgApath)) {
    # on Windows, 'pkgA' may end up being a text file with a single line
    # with a note it is meant to be a link to xDir/pkg
    cat("symlink 'pkgA' does not exist as directory ",d,"; copying it\n", sep='')
    unlink(d, recursive=TRUE)
    dir.create(d) # ensure it is a single existing directory
    pkgdir <- file.path(pkgPath, "xDir", "pkg")
    file.copy(file.path(pkgdir, list.files(pkgdir)), to = d, recursive=TRUE)
    ## if even the copy failed (NB: pkgB, pkgC depend on pkgA)
    if(!dir.exists(d)) p.lis <- p.lis[!(p.lis %in% c("pkgA", pBlis, "pkgC"))]
}
dir2pkg <- function(dir) sub("^pkgC", "PkgC", dir)
if(is.na(match("myLib", .lP <- .libPaths()))) {
    .libPaths(c("myLib", .lP)) # PkgC needs pkgA from there
    .lP <- .libPaths()
}
Sys.setenv(R_LIBS = .R_LIBS(.lP)) # for build.pkg() & install.packages()
showProc.time()
for(p in p.lis) {
    p. <- dir2pkg(p) # 'p' is sub directory name;  'p.' is package name
    cat("===--===\nFrom pkgPath sub directory", p, " building package", p., "...\n")
    pkgP <- file.path(pkgPath, p)
    r <- build.pkg(pkgP, ignore.stderr = (p != "exSexpr")) # 1-2 sec
    showProc.time()
    if(!length(r)) # so some sort of failure, show log
        cat(attr(r, "log3"), sep = "\n")
    if(!isTRUE(file.exists(r)))
        stop("R CMD build failed (no tarball) for package ", p.)
    if(p %in% pBlis) { ## R CMD check "dependencies"
        res <- tools:::.check_package_depends(dir=pkgP, force_suggest=FALSE)
        cat("check_package_depends:\n") ; print(res)
        if(length(res)) pres <- capture.output(res)
        switch(p
             , "pkgB" = stopifnot(length(res) == 0)
             , "pkgB2" = stopifnot(exprs = {
                 length(res) == 1
                 identical(res$missing_namespace_depends, c("methods", "pkgA"))
                 grepl("Namespace dependencies .* DESCRIPTION" , pres[1])
                 })
             , "pkgB3" =  stopifnot(exprs = {
                 length(res) == 1
                 identical(res[[1]], "pkgA")
                 grepl("Namespace dependency .* DESCRIPTION" , pres[1])
                 })
               )
        showProc.time()
    } else if(p %in% p.fails) {
        ## NB: Fail with *parse* errors which are *not* tryCatch-able ==> need to call R
        ## tryCatch(error = identity,
        ##          install.packages(r, lib = "myLib", repos=NULL, type = "source",
        ##                           INSTALL_opts = InstOpts[[p.]])) -> err
        ## cat("tryCatch gave "); dput(err)
        ## stopifnot(inherits(err, "error"))
        ## FIXME: do a bit more
        tf <- tempfile(paste0("regP-inst_",p))
        status <- tools:::run_Rcmd(c("INSTALL", r, InstOpts[[p.]]),
                                   out=tf, timeout = 10)
        writeLines(errlns <- readLines(tf))
        stopifnot(exprs = {
            status > 0 # see status == 1L
            length(iE <- grep("Error in parse(", errlns, fixed=TRUE)) > 0
            local({
                parseM1 <- "(syntax error|unexpected symbol)"       # may depend on bison version
                parseM2 <- "(syntax error|unexpected end of input)" #   (ditto)
                switch(p
                 , "PR17859.1" = grepl(paste0(p, "/R/f2.R:3:[0-9]+: ", parseM1), errlns[iE+1])
                 , "PR17859.2" =
                 , "PR17859.3" = grepl(paste0(p, "/R/f2.R:6:0: ",      parseM2), errlns[iE+1])
                 , stop("invalid package p=",  p))
            })
        })
        next # pkg in for(...)
    }
    ## otherwise install the tar file:
    cat("installing package", p., "using built file", r, "...\n")
    ## "FIXME": want to catch warnings in the "console output" of this,
    ## e.g. exNSS4nil, "S4 exports specified in 'NAMESPACE' but not defined .."
    install.packages(r, lib = "myLib", repos=NULL, type = "source",
                     INSTALL_opts = InstOpts[[p.]])
    stopifnot(require(p., lib = "myLib", character.only=TRUE))
    detach(pos = match(p., sub("^package:","", search())))
    showProc.time()
}
cat("\n-------------------end { for(p in p.lis) }----------------------------\n")
(res <- installed.packages(lib.loc = "myLib", priority = "NA"))
(p.lis <- dir2pkg(setdiff(p.lis, p.fails))) # --> *package* names of installed pkgs
stopifnot(exprs = {
    identical(res[,"Package"], setNames(, sort(c(p.lis, "myTst"))))
    res[,"LibPath"] == "myLib"
})
### Specific Tests on our "special" packages: ------------------------------

tf <- tempfile("chk_donttest")
## why does this not work (not catch stderr)?  textConnection("checkTxt", open="w")
system.time(status <-
        tools:::run_Rcmd(c("check", "PR17501_1.0.tar.gz", "--no-manual"),
                         out=tf, timeout = 50))# see 5--7 sec; Solaris needed > 30
stopifnot(exprs = {
    status == 1 # an ERROR now
    is.character(exLines <-
                     readLines(file.path("PR17501.Rcheck", "PR17501-Ex.R")))
    { str(exLines); length(exLines) > 20 } # str(): diagnostic in case
    is.integer(i <- grep("^R\\.Version\\( *# missing closing paren", exLines))
    grepl("^## No test", exLines[i-1])
    { str(tlines <- readLines(tf)); length(tlines) > 20 }
    length(iw <- grep("^Warning: parse error", tlines)) == 1
    (lenN <- length(print(iN <- grep("^[1-9][0-9]:", tlines)))) >= 2
    iN - iw == seq_len(lenN) # these (3) lines come immediately after 'Warning',
    ## and "related" to the some 'missing .. paren' above:
    !is.na(ierr <- as.integer(substr(print(tlines[iN[1]]), 1, 2)))
    8 <= print(ierr - i) & ierr - i <= 14 # see 11
}) ## failed in R <= 4.1.1


## These used to fail because of the sym.link in pkgA
if("pkgA" %in% p.lis && dir.exists(pkgApath)) {
    cat("undoc(pkgA):\n"); print(uA <- tools::undoc(dir = pkgApath))
    cat("codoc(pkgA):\n"); print(cA <- tools::codoc(dir = pkgApath))
    cat("extends(\"classApp\"):\n"); print(ext.cA <- extends("classApp"))
    stopifnot(exprs = {
	identical(uA$`code objects`, c("nil", "search"))
	identical(uA$`data sets`,    "nilData")
	## pkgC's class union is now (after loading pkgC) also visible in the "classApp" subclass
	## (which gave warning). ==> warning "wrong": somehow it *did* get updated:
	"numericA" %in% ext.cA
    })
} else message("'pkgA' not available")
showProc.time()

## - Check conflict message.
## - Find objects which are NULL via "::" -- not to be expected often
##   we have one in our pkgA, but only if Matrix is present.
if(dir.exists(file.path("myLib", "pkgA"))) {
  msgs <- capture.output(require(pkgA, lib="myLib"), type = "message")
  writeLines(msgs)
  stopifnot(length(msgs) > 2,
            length(grep("The following object is masked.*package:base", msgs)) > 0,
            length(grep("\\bsearch\\b", msgs)) > 0)
  data(package = "pkgA") # -> nilData
  stopifnot(is.null( pkgA::  nil),
	    is.null( pkgA::: nil),
	    is.null( pkgA::  nilData)) # <-
  ## R-devel (pre 3.2.0) wrongly errored for NULL lazy data
  ## ::: does not apply to data sets:
  tools::assertError(is.null(pkgA:::nilData))
} else message("'pkgA' not in 'myLib'")
showProc.time()

## Check error from invalid logical field in DESCRIPTION:
(okA <- ("pkgA" %in% p.lis) && dir.exists(pkgApath) &&
     file.exists(DN <- file.path(pkgApath, "DESCRIPTION")))
if(okA) {
  Dlns <- readLines(DN); i <- grep("^LazyData:", Dlns)
  Dlns[i] <- paste0(Dlns[i], ",") ## adding a ","
  writeLines(Dlns, con = DN)
  ## do not test installation failure in myLib as previous pkgA would be removed
  ## from there (because no.q=TRUE causes do_exit_on_error() to be called twice)
  ## and if getNamespaceInfo("pkgA", "path") no longer exists,
  ## sessionInfo() fails in the "exSexpr" test below
  instEXPR <- quote(
      tools:::.install_packages(c("--clean", paste0("--library=", tempdir()), pkgApath), no.q = TRUE)
  )   ##      -----------------                                               ----
  if(interactive()) { ## << "FIXME!"  This (sink(.) ..) fails, when run via 'make'.
    ## install.packages() should give "the correct" error but we cannot catch it
    ## One level lower is not much better, needing sink() as capture.output() fails
    ftf <- file(tf <- tempfile("inst_pkg"), open = "wt")
    sink(ftf); sink(ftf, type = "message")# "message" should be sufficient
    eval(instEXPR)
    sink(type="message"); sink()## ; close(ftf); rm(ftf)# end sink()
    writeLines(paste(" ", msgs <- readLines(tf)))
    message(err <- grep("^ERROR:", msgs, value=TRUE))
    stopifnot(exprs = {
        length(err) > 0
        grepl("invalid .*LazyData .*DESCRIPTION", err)
    })
  } else {
      message("non-interactive -- tools:::.install_packages(..) : ")
      try( eval(instEXPR) ) # showing the error message in the *.Rout file
  }
} else message("pkgA/DESCRIPTION  not available")
showProc.time()

## R CMD check should *not* warn about \Sexpr{} built sections in Rd (PR#17479):
msg <- capture.output(
    tools:::.check_package_parseRd(dir=file.path(pkgPath, "exSexpr")))
if(length(msg))
    stop(".check_package_parseRd() gave message\n",msg)
## in R <= 3.5.1, gave
##  "prepare_Rd: foo.Rd:14: Section \\Sexpr is unrecognized and will be dropped"
showProc.time()


if(dir.exists(file.path("myLib", "exNSS4"))) withAutoprint({
  require("exNSS4", lib="myLib")
  validObject(dd <- new("ddiM"))
  print(is(dd))  #  5 of them ..
  writeLines(myGmeth <- capture.output(show(exNSS4:::myGenf)))
  stopifnot(exprs = {
            is(dd, "mM")
      inherits(dd, "mM")
      grepl("showMethods(exNSS4:::myGenf)", myGmeth[length(myGmeth)], fixed=TRUE)
  })
  ## tests here should *NOT* assume recommended packages,
  ## let alone where they are installed
  if(dir.exists(file.path(.Library, "Matrix"))) {
    for(ns in c(rev(p.lis), "Matrix")) unloadNamespace(ns)
    ## Both exNSS4 and Matrix define "atomicVector" *the same*,
    ## but  'exNSS4'  has it extended - and hence *both* are registered in cache -> "conflicts"
    requireNamespace("exNSS4", lib= "myLib")
    ## Found in cache, since there is only one definition.
    ## Might confuse users.
    stopifnot(isVirtualClass(getClass("atomicVector")))
    requireNamespace("Matrix", lib= .Library)
    ## Throws an error, because there is ambiguity in the cache,
    ## and the dynamic search will not find anything, since the packages
    ## are not attached.
    tools::assertCondition(
        acl <- getClass("atomicVector")
        )
    ## Once Matrix is attached, we find a unique definition.
    library(Matrix)
    stopifnot(isVirtualClass(getClass("atomicVector")))
  }
})
showProc.time()


## Part 3: repository construction ---------------------------------------------
## test tools::write_PACKAGES and tools::update_PACKAGES
oldpkgdir <- file.path(tempdir(), "pkgfiles/old")
newpkgdir <- file.path(tempdir(), "pkgfiles/new")
repodir <- file.path(tempdir(), "pkgrepo")
dir.create(oldpkgdir, recursive = TRUE)
dir.create(newpkgdir)
if(file.exists(repodir))
    unlink(repodir, recursive = TRUE)
dir.create(repodir)

ro <- build.pkg(file.path(pkgPath, c("pkgD",   "pkgB")),   oldpkgdir)
rn <- build.pkg(file.path(pkgPath, c("pkgD_2", "pkgD_3")), newpkgdir)
unlist(ro)
unlist(rn)


##' A repo package database in directory 'dir'
mkPkgfiles <- function(dir)
    file.path(dir, c("PACKAGES",
                     "PACKAGES.gz",
                     "PACKAGES.rds"))

##' safe read.dcf()
read.safe.dcf <- function(f) if(file.exists(f)) read.dcf(f) # else NULL

## this will fail with an error if write_PACKAGES
## and update_PACKAGES do not generate the same
## PACKAGE file entries, in the same order, with
## the same field order.
docompare <- function(..., repdir = repodir, strict = TRUE) {
    Pfiles <- mkPkgfiles(repdir)
    backupPfiles <- file.path(tempdir(), basename(Pfiles))
    indfile <- Pfiles[1]
    ##     vvvvvvvvvvvvvvv
    tools::write_PACKAGES(repdir, type = "source", ...)
    wpres <- read.safe.dcf(indfile) # write_P result
    ## reset the PACKAGES files so that update_PACKAGES thinks any deviations are "new"
    if(all(file.exists(backupPfiles)))
        file.copy(backupPfiles, Pfiles, overwrite = TRUE)
    ##     vvvvvvvvvvvvvvv
    tools::update_PACKAGES(repdir, type = "source", strict=strict, ...)
    upres <- read.safe.dcf(indfile) # update_P result
    stopifnot(identical(wpres, upres))
}

Pfiles <- mkPkgfiles(repodir)
backupPfiles <- file.path(tempdir(), basename(Pfiles))
if(all(file.exists(backupPfiles)))
    unlink(backupPfiles)
showProc.time()

## test write_PACKAGES and update_PACKAGES
## on empty dir
## IGNORE_RDIFF_BEGIN
docompare() ## one warning expected, has a temp path in it so ignore diff
## IGNORE_RDIFF_END

oldpfs <- list.files(oldpkgdir, pattern = "\\.tar\\.gz$", recursive = TRUE, full.names = TRUE)
newpfs <- list.files(newpkgdir, pattern = "\\.tar\\.gz$", recursive = TRUE, full.names = TRUE)

## generate and backup "original repo state"
file.copy(oldpfs, to = repodir)
tools::write_PACKAGES(repodir, type = "source")
file.copy(Pfiles, backupPfiles, overwrite = TRUE)
showProc.time()


## test update_PACKAGES with no change
docompare()

## all old files gone, new files present
unlink(file.path(repodir, basename(oldpfs)))
file.copy(newpfs, to = repodir)
docompare()
docompare(strict=FALSE)

## put old ones back
file.copy(oldpfs, to = repodir)
showProc.time()


if(isWIN){
    nrepodir  <- normalizePath(repodir)
    if(grepl("^\\\\", nrepodir)) #\\laptop\whatever
        repourl  <- paste0("file:", gsub("\\\\", "/", nrepodir))
    else #C:\whatever
        repourl  <- paste0("file:///", gsub("\\\\", "/", nrepodir))
} else
    repourl  <- paste0("file://", normalizePath(repodir))

## make sure the ordering is right when
## old and new entries are mixed in final db
##

##' care: stopifnot(nrow(1) == 2) # does *not* trigger
checkMatrix <- function(x, n) stopifnot(is.matrix(x), nrow(x) == n)

docompare(latestOnly = TRUE)
str(ap <- available.packages(repourl, filters = list()))
checkMatrix(ap, 2)

docompare(latestOnly = FALSE)
str(ap <- available.packages(repourl, filters = list()))
checkMatrix(ap, 4)

docompare(latestOnly = TRUE, strict = FALSE)
str(ap <- available.packages(repourl, filters = list()))
checkMatrix(ap, 2)

docompare(latestOnly = FALSE, strict = FALSE)
str(ap <- available.packages(repourl, filters = list()))
checkMatrix(ap, 4)




## clean up
rmL <- c("myLib", if(has.symlink) "myLib_2", "myTst", "myTst2",
         "PR17501.Rcheck")
if(do.cleanup) {
    for(nm in rmL) unlink(nm, recursive = TRUE)
} else {
    cat("Not cleaning, i.e., keeping ", paste(rmL, collapse=", "), "\n")
}

showProc.time()
## And the final:
environment(showProc.time)[["pct"]]
