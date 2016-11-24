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

type = "win.binary"
mp <- mirror.packages(pkgs, repos = repos, repodir = subcran,
                      type = type,
                      #except.repodir = NULL, # this will overwrite existing files
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

