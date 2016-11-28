options("repos" = c("CRAN" = "https://cloud.r-project.org"))

require("tools")

## dictionary

# localdir - arbitrary path where subdirs are packages having unpacked source
# localcran - CRAN-like repository hosted with write_PACKAGES or drat
# cran - regular online CRAN mirror

# db - object from available.packages()
# curl - object from contrib.url(), directory where pkgs are located
# repos - location which abstracts "type": src/conbitr, bin

# curl and db both can be used to orchestrate dependencies between various locations
# to be more verbose in code it also make sense to use contriburl=curl (or available=db) rather than repos=localdir
# if we still would like to support repos=localdir then easiest should be to redirect repos to contriburl at start of install.packages/update.packages
# please follow examples below for complete API, no changes to current code were required

dummy.packages <-
function(path, force = FALSE) {
    pkgs.deps <- list(
        pkgA = "pkgB",
        pkgB = "pkgC",
        pkgC = character(),
        ## pkgD will reach localdir, localcran and CRAN
        ## resolve recursive dependencies from all of them
        pkgD = c("pkgA","bit64","splitstackshape")
    )
    make.pkg <- function(pkg) {
        e <- new.env()
        ## each pkg has unique hello world
        abcd <- substr(pkg, 4, 4)
        e[[paste0("hello", abcd)]] <- eval(parse(text = sprintf('function() cat("Hello%s World!\\n")', abcd)))
        deps <- pkgs.deps[[pkg]]
        package.skeleton(name = pkg, path = path, environment = e, force = force)
        if (length(deps)) {
            ## append in NAMESPACE
            nsf <- file.path(path, pkg, "NAMESPACE")
            ns <- readLines(nsf)
            writeLines(c(ns, sprintf("import(%s)", deps)), nsf)
            ## append in DESCRIPTION
            dcf <- file.path(path, pkg, "DESCRIPTION")
            dc <- readLines(dcf)
            writeLines(c(dc, sprintf("Imports: %s", paste(deps, collapse = ", "))), dcf)
        }
        unlink(file.path(path, pkg, "man", "*.Rd"))
        file.remove(file.path(path, pkg, "Read-and-delete-me"))
    }
    sapply(names(pkgs.deps), make.pkg, simplify = FALSE)
}

## setup localdir

localdir <- file.path(tempdir(), "packages")
dir.create(localdir, showWarnings = FALSE)
dummy.packages(localdir, force = TRUE)
## localdir require: unpacked = TRUE, addFiles = TRUE
write_PACKAGES(localdir, unpacked = TRUE, addFiles = TRUE)
localdir.db <- available.packages(file.path("file:", normalizePath(localdir)))
stopifnot(
    ## using "contriburl"
    paste0("pkg",LETTERS[1:4]) %in% rownames(localdir.db)
)

## setup localcran

localcran <- file.path(tempdir(), "cran")
dir.create(localcran, showWarnings = FALSE)
mirror.packages("bit64", repodir = localcran, addFiles = TRUE)
localcran.db <- available.packages(file.path("file:", normalizePath(contrib.url(localcran))))
stopifnot(
    ## using "contriburl"
    c("bit","bit64") %in% rownames(localcran.db),
    ## using "repos"
    c("bit","bit64") %in% rownames(available.packages(repos = file.path("file:", normalizePath(localcran))))
)

## setup cran

cran.db <- available.packages(contrib.url(getOption("repos")))
stopifnot(
    ## using "contriburl"
    c("data.table","splitstackshape") %in% rownames(cran.db),
    ## using "repos"
    c("data.table","splitstackshape") %in% rownames(available.packages())
)

## installation

## pkgD
# imports pkgA (pkgsB (pkgsC)) from localdir
# imports bit64 (bit) from localcran
# imports splitstackshape (data.table) from cran

deps <- packages.dcf(file.path(localdir,"pkgD","DESCRIPTION"))
all.db <- rbind(
    localdir.db, localcran.db,
    ## remove mirrored pkgs to avoid dups
    cran.db[!cran.db[,"Package"] %in% c("bit","bit64"),, drop = FALSE]
)
deps.db <- all.db[all.db[,"Package"] %in% deps,, drop = FALSE]
stopifnot(
    ## confirm deps of pkgD are in 3 diff types
    identical(deps.db["pkgA", "Repository"], file.path("file:", normalizePath(localdir))),
    identical(deps.db["bit64", "Repository"], file.path("file:", contrib.url(localcran))),
    identical(deps.db["splitstackshape", "Repository"], contrib.url(getOption("repos")))
)

tmplib <- file.path(tempdir(), "library")

## install using "contriburl"
unlink(tmplib, recursive = TRUE)
dir.create(tmplib)
curl.all <- c(
    file.path("file:", normalizePath(localdir)),
    file.path("file:", normalizePath(contrib.url(localcran))),
    contrib.url(getOption("repos"))
)
print(curl.all)
install.packages("pkgD", lib = tmplib, contriburl = curl.all)
all.inst <- c("bit","bit64","data.table","splitstackshape",paste0("pkg",LETTERS[1:4]))
stopifnot(
    ## this assume you did not have any of these installed in own libs
    all.inst %in% installed.packages(tmplib)
)

## install using "available"
unlink(tmplib, recursive = TRUE)
dir.create(tmplib)
install.packages("pkgD", lib = tmplib, available = all.db)
stopifnot(
    ## this assume you did not have any of these installed in own libs
    all.inst %in% installed.packages(tmplib)
)

## install using "repos"
# a matter of switch to contriburl at start of install.packages/update.packages?
# switch using contrib.url() for cran and localcran, as is for localdir

if (!interactive()) q("no")
