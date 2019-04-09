##
## test write_PACKAGES and update_PACKAGES
##
## These tets are  here instead of in the
## src/library/tools/tests because we use SRCDIR
## evironment variable set during the make check(-*)
## process
##

library(tools)

## code below borrowed and slightly modified from reg-packages.R
## these tests could go in that file, but not regression tests...

.R_LIBS <- function(libp = .libPaths()) { # (>> in utils?)
    libp <- libp[! libp %in% .Library]
    if(length(libp))
        paste(libp, collapse = .Platform$path.sep)
    else "" # character(0) is invalid for Sys.setenv()
}
Sys.setenv(R_LIBS = .R_LIBS() # for build.pkg() & install.packages()
         , R_BUILD_ENVIRON = "nothing" # avoid ~/.R/build.environ which might set R_LIBS
         , R_ENVIRON = "none"
         , R_PROFILE = "none"
           )

##modified version here
build.pkg <- function(dir, destdir) {
    if(!all(file.exists(dir))) {
        print(dir)
        stop("not there")
    }
        
    if(length(dir) > 1) {
        sapply(dir, build.pkg, destdir = destdir)
        return(NULL)
    }
    
    stopifnot(dir.exists(dir), file.exists(DESC <- file.path(dir, "DESCRIPTION")))
    pkgName <- sub("^[A-Za-z]+: ", "", grep("^Package: ", readLines(DESC), value=TRUE))
    patt <- paste(pkgName, ".*tar\\.gz$", sep="_")
    unlink(dir('.', pattern = patt))
    Rcmd <- paste(shQuote(file.path(R.home("bin"), "R")), "CMD")
    r <- system(paste(Rcmd, "build --keep-empty-dirs", shQuote(dir)),
                intern = TRUE)
    ## return name of tar file built
    tball <-structure(dir('.', pattern = patt), log3 = r)
    file.copy(tball, destdir)
    NULL
}

pkgSrcPath <- file.path(Sys.getenv("SRCDIR"), "Pkgs")# e.g., -> "../../R/tests/Pkgs"

## end borrowed from reg-packages.R
oldpkgdir <- file.path(tempdir(), "pkgfiles/old")
newpkgdir <- file.path(tempdir(), "pkgfiles/new")
repodir <- file.path(tempdir(), "pkgrepo")
dir.create(oldpkgdir, recursive = TRUE)
dir.create(newpkgdir)
if(file.exists(repodir))
    unlink(repodir, recursive = TRUE)
dir.create(repodir)

build.pkg(file.path(pkgSrcPath, c("pkgA", "pkgB")),
          oldpkgdir)
build.pkg(file.path(pkgSrcPath, c("pkgA_2",
                                  "pkgA_3")),
          newpkgdir)

##strip off strict arg
wpwrapper = function( ..., strict = TRUE)
    write_PACKAGES(...)


docompare = function(...) {
    wpwrapper(repodir, type = "source", ...)
    if(file.exists(indfile))
        wpres <- read.dcf(indfile)
    else
        wpres <- FALSE
    if(all(file.exists(backupPfiles)))
        file.copy(backupPfiles, Pfiles, overwrite = TRUE)
    
    update_PACKAGES(repodir, type = "source", ...)
    if(file.exists(indfile))
        upres <- read.dcf(indfile)
    else
        upres <- FALSE
    stopifnot(identical(wpres, upres))
    TRUE
}

Pfiles <- file.path(repodir, c("PACKAGES",
                               "PACKAGES.gz",
                               "PACKAGES.rds"))

backupPfiles <- file.path(tempdir(), basename(Pfiles))
indfile <- Pfiles[1]
                             
if(all(file.exists(backupPfiles)))
    unlink(backupPfiles)
## tests write_PACKAGES and update_PACKAGES
## on empty dir
## IGNORE_RDIFF_BEGIN
docompare() ## one warning expected, has a temp path in it so ignore diff
## IGNORE_RDIFF_END

oldpfs <- list.files(oldpkgdir, pattern = "\\.tar\\.gz$", recursive = TRUE, full.names = TRUE)

newpfs <- list.files(newpkgdir, pattern = "\\.tar\\.gz$", recursive = TRUE, full.names = TRUE)


indfile <- file.path(repodir, "PACKAGES")
file.copy(oldpfs, to = repodir)


## base truth write_PACKAGES and update_PACKAGES with no change
docompare()

file.copy(Pfiles, backupPfiles)

## all old files gone, new files present

unlink(file.path(repodir, basename(oldpfs)))
file.copy(newpfs, to = repodir)
docompare()
docompare(strict=FALSE)

## put old ones back
file.copy(oldpfs, to = repodir)


repourl = paste0("file://", normalizePath(repodir))
## make sure the ordering is right when
## old and new entries are mixed in final db
##
docompare(latestOnly = TRUE)
stopifnot(nrow(available.packages(contriburl = repourl,
                                  filter = character())) == 2)
docompare(latestOnly = FALSE)
stopifnot(nrow(available.packages(contriburl = repourl,
                                  filter = character())) == 4)
docompare(latestOnly = TRUE, strict = FALSE)
stopifnot(nrow(available.packages(contriburl = repourl,
                                  filter = character())) == 2)
docompare(latestOnly = FALSE, strict = FALSE)
stopifnot(nrow(available.packages(contriburl = repourl,
                                  filter = character())) == 4)
