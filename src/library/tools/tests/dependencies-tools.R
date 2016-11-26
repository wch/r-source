options("repos" = c("CRAN" = "https://cloud.r-project.org"))

require("tools")

# we can extract dependencies from multiple libraries at once

paths <- .libPaths()
dcfs <- file.path(list.dirs(paths, recursive = FALSE), "DESCRIPTION")

pkgs <- packages.dcf(dcfs)
repos <- c(getOption("repos"), repos.dcf(dcfs))
str(pkgs)
str(repos)
stopifnot(as.logical(length(pkgs)))

# we can use any DESCRIPTION file, from pkgs sources, libraries, or even unrelated ones

dcfs <- sapply(c("MASS","lattice"), function(p) system.file("DESCRIPTION", package=p))

pkgs <- packages.dcf(dcfs, which = "all") # take also Suggests and Enhances
repos <- c(getOption("repos"), repos.dcf(dcfs))
print(pkgs)
print(repos)
stopifnot(as.logical(length(pkgs)))

# we can mirror packages tree from upstream repository

subcran <- file.path(tempdir(), "cran")
mp <- mirror.packages(pkgs, repos = repos, repodir = subcran,
                      except.repodir = NULL, # NULL will overwrite existing files
                      quiet = FALSE,
                      # those args are pass via dots to write_PACKAGES
                      fields = "Packaged", # PACKAGES file metadata Packaged and File
                      addFiles = TRUE)
ans <- list.files(contrib.url(subcran))
index <- readLines(file.path(contrib.url(subcran),"PACKAGES"))
stopifnot(
    as.logical(nrow(mp)),
    length(ans) > 1L,
    "PACKAGES" %in% ans,
    any(grepl("^.*\\.tar\\.gz$", ans)),
    any(grepl("^Packaged", index)),
    any(grepl("^File", index))
)

# test if we can install from our mirror

tmplib <- file.path(tempdir(), "library")
dir.create(tmplib, showWarnings = FALSE)
install.packages(pkgs, lib = tmplib, repos = file.path("file:", normalizePath(subcran)), quiet = TRUE)
ans <- sapply(pkgs, function(p) packageVersion(p, tmplib), simplify = FALSE)
stopifnot(length(ans) > 1L)

# mirror package binaries

type <- "win.binary"
mp <- mirror.packages(pkgs, repos = repos, repodir = subcran,
                      type = type,
                      except.repodir = NULL, # this will overwrite existing files
                      quiet = FALSE,
                      # those args are pass via dots to write_PACKAGES
                      fields = "Packaged", # PACKAGES file metadata
                      addFiles = TRUE)
ans <- list.files(contrib.url(subcran, type))
index <- readLines(file.path(contrib.url(subcran, type),"PACKAGES"))
stopifnot(
    as.logical(nrow(mp)),
    length(ans) > 1L,
    "PACKAGES" %in% ans,
    any(grepl("^.*\\.zip$", ans)),
    any(grepl("^Packaged", index)),
    any(grepl("^File", index))
)

# resolve dependencies from local directory of R packages

# assuming wd in tools/tests
path <- file.path(".", "packages")
tools::write_PACKAGES(path, unpacked = TRUE, addFiles = TRUE)
db <- utils::available.packages(contriburl = normalizePath(path))
print(db)
# install pkgA and dep pkgB, and recursive dep pkgC
utils::install.packages("pkgA", lib = tmplib, contriburl = path, available = db)
new.pkgs <- c("pkgA","pkgB","pkgC")
ans <- sapply(new.pkgs, packageVersion, lib.loc = tmplib, simplify = FALSE)
stopifnot(
    length(ans) == 3L,
    sapply(new.pkgs, require, lib.loc = tmplib, character.only = TRUE)
)
co <- capture.output({
    helloA()
    helloB()
    helloC()
})
stopifnot(
    grepl("Hello[A|B|C]", co)
)

# resolve dependencies across dir of R pkgs and standard repos

newer.pkgs <- "pkgD"
## for testing non-recommended pkgs
## imports pkgA (pkgsB (pkgsC)) from local path
## imports drat and data.table from CRAN
curlcran <- contrib.url(getOption("repos"))
dbcran <- utils::available.packages(contriburl = curlcran)
utils::install.packages(newer.pkgs, lib = tmplib,
                        contriburl = c(normalizePath(path), curlcran),
                        ## we can rbind as there are no duplicates
                        available = rbind(db, dbcran))
newer.pkgs.deps <- packages.dcf(file.path(path, newer.pkgs, "DESCRIPTION"))
ans <- sapply(newer.pkgs.deps,
              packageVersion,
              ## we may have some of them already installed
              lib.loc = c(tmplib, .libPaths()),
              simplify = FALSE)
stopifnot(
    length(ans) == 3L
)
# just cleanup tmplib
utils::remove.packages(c(newer.pkgs, new.pkgs), lib = tmplib)
unlink(file.path(path, c("PACKAGES","PACKAGES.gz")))

if (!interactive()) q("no")
