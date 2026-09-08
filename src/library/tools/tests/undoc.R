require("tools")

(ud4 <- undoc("stats4"))
stopifnot(sapply(ud4, length) == 0)


## PR#18959: undoc() on package dir with S4 classes and methods
pkgdir <- tempfile("testpkg")
dir.create(file.path(pkgdir, "R"), recursive = TRUE)
dir.create(file.path(pkgdir, "man"))

write.dcf(file = file.path(pkgdir, "DESCRIPTION"),
  list(Package = basename(pkgdir),
       Title = "Test Package with Private Classes and Methods",
       Version = "1.0",
       Description = "Test with unexported methods, classes for undoc PR18959",
       License = "GPL-2"))

cat(file = file.path(pkgdir, "NAMESPACE"), r"(
exportClasses(ExpClass)
exportClassPattern("^PatClass")
exportMethods(show)
)")

cat(file = file.path(pkgdir, "R", "code.R"), r"(
ExpClass <- setClass("ExpClass", slots = c(x = "numeric"))
PatClassA <- setClass("PatClassA", slots = c(y1 = "numeric"))
PatClassB <- setClass("PatClassB", slots = c(y2 = "numeric"))
UnexpClass <- setClass("UnexpClass", slots = c(z = "numeric"))
.m1 <- setMethod("show", "ExpClass", function(object) print(object@x))
.m2 <- setMethod("plot", "ExpClass", function(x, y, ...) plot(x@x, ...))
)")

u <- undoc(dir = pkgdir)
stopifnot(exprs = {
    identical(u[["S4 classes"]], c("ExpClass", "PatClassA", "PatClassB"))
    identical(u[["S4 methods"]], "generic 'show' and siglist 'ExpClass'")
})

## Document the exported S4 class and method
cat(file = file.path(pkgdir, "man", "ExpClass.Rd"), r"(
\name{ExpClass-class}
\docType{class}
\alias{ExpClass-class}
\alias{show,ExpClass-method}
\title{ExpClass}
\description{ExpClass}
)")

cat(file = file.path(pkgdir, "man", "PatClassA.Rd"), r"(
\name{PatClassA-class}
\docType{class}
\alias{PatClassA-class}
\title{PatClassA}
\description{PatClassA}
)")
cat(file = file.path(pkgdir, "man", "PatClassB.Rd"), r"(
\name{PatClassB-class}
\docType{class}
\alias{PatClassB-class}
\title{PatClassB}
\description{PatClassB}
)")

u_doc <- undoc(dir = pkgdir)
stopifnot(exprs = {
    length(u_doc[["S4 classes"]]) == 0L
    length(u_doc[["S4 methods"]]) == 0L
})

unlink(pkgdir, recursive = TRUE)


## Package directory without NAMESPACE (nsInfo is NULL)
pkgdir_no_ns <- tempfile("testpkg_no_ns")
dir.create(file.path(pkgdir_no_ns, "R"), recursive = TRUE)
dir.create(file.path(pkgdir_no_ns, "man"))

write.dcf(file = file.path(pkgdir_no_ns, "DESCRIPTION"),
  list(Package = basename(pkgdir_no_ns),
       Title = "Test Package without NAMESPACE",
       Version = "1.0",
       Description = "Test package without NAMESPACE for undoc PR18959.",
       License = "GPL-2"))

cat(file = file.path(pkgdir_no_ns, "R", "code.R"), r"(
Cl1 <- setClass("Cl1", slots = c(x = "numeric"))
.m <- setMethod("show", "Cl1", function(object) print(object@x))
)")

u_no_ns <- undoc(dir = pkgdir_no_ns)
stopifnot(exprs = {
    identical(u_no_ns[["S4 classes"]], "Cl1")
    identical(u_no_ns[["S4 methods"]], "generic 'show' and siglist 'Cl1'")
})

unlink(pkgdir_no_ns, recursive = TRUE)
