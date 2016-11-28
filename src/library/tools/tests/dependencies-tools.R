options("repos" = c("CRAN" = "https://cloud.r-project.org"))

require("tools")

## we can extract dependencies from multiple libraries at once

paths <- .libPaths()
dcfs <- file.path(list.dirs(paths, recursive = FALSE), "DESCRIPTION")

pkgs <- packages.dcf(dcfs)
repos <- c(getOption("repos"), repos.dcf(dcfs))
str(pkgs)
str(repos)
stopifnot(as.logical(length(pkgs)))

## we can use any DESCRIPTION file, from pkgs sources, libraries, or even unrelated ones

dcfs <- sapply(c("MASS","lattice"), function(p) system.file("DESCRIPTION", package=p))

pkgs <- packages.dcf(dcfs, which = "all") # take also Suggests and Enhances
repos <- c(getOption("repos"), repos.dcf(dcfs))
print(pkgs)
print(repos)
stopifnot(as.logical(length(pkgs)))

## we can mirror packages tree from upstream repository

pkgs <- "bit64"
## this also mirror bit
subcran <- file.path(tempdir(), "cran")
mp <- mirror.packages(pkgs, repos = repos, repodir = subcran,
                      quiet = FALSE,
                      # those args are pass via dots to write_PACKAGES
                      fields = "Packaged", # PACKAGES file metadata Packaged and File
                      addFiles = TRUE)
ans <- list.files(contrib.url(subcran))
index <- readLines(file.path(contrib.url(subcran),"PACKAGES"))
expect.available <- function(pkgs, type = "source") pkgs %in% rownames(available.packages(file.path("file:",normalizePath(contrib.url(subcran, type = type))), type = type))
stopifnot(
    nrow(mp) >= 2L,
    length(ans) >= 3L, # + PACKAGES[.gz]
    "PACKAGES" %in% ans,
    any(grepl("^.*\\.tar\\.gz$", ans)),
    any(grepl("^Packaged", index)),
    any(grepl("^File", index)),
    expect.available(c("bit","bit64"))
)

# check mirror except.repo works, mirror ff

pkgs <- "ff"
mp <- mirror.packages(pkgs, repos = repos, repodir = subcran,
                      quiet = FALSE, fields = "Packaged",
                      addFiles = TRUE)
stopifnot(
    nrow(mp)==1L, # assume ff dep on bit only as now
    expect.available(c("bit","bit64","ff"))
)

# test if we can install from our mirror

pkgs <- "bit64"
tmplib <- file.path(tempdir(), "library")
dir.create(tmplib, showWarnings = FALSE)
install.packages(pkgs, lib = tmplib, repos = file.path("file:", normalizePath(subcran)), quiet = TRUE)
ans <- sapply(pkgs,  # vectorized but it is not used now
              function(p) packageVersion(p, tmplib), 
              simplify = FALSE)
stopifnot(length(ans)==1L, identical(names(ans), pkgs))

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
    any(grepl("^File", index)),
    expect.available(c("bit","bit64"), type = type),
    ## src/contrib should work as pre-win.binary
    expect.available(c("bit","bit64","ff"), type = "source")
)

if (!interactive()) q("no")
