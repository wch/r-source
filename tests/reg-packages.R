## PR 1271  detach("package:base") crashes R.
try(detach("package:base"))


## invalid 'lib.loc'
stopifnot(length(installed.packages("mgcv")) == 0)
## gave a low-level error message


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
dir.exists <- function(x)
    is.character(x) && file.exists(x) && file.info(path.expand(x))$isdir
build.pkg <- function(dir) {
    stopifnot(dir.exists(dir))
    patt <- paste(basename(dir), ".*tar\\.gz$", sep="_")
    unlink(dir('.', pattern = patt))
    Rcmd <- paste(file.path(R.home("bin"), "R"), "CMD")
    r <- tail(system(paste(Rcmd, "build --keep-empty-dirs", dir),
                     intern = TRUE), 3)
    ## return name of tar file built
    dir('.', pattern = patt)
}
build.pkg("myTst")
## clean up any previous attempt (which might have left a 00LOCK)
unlink("myLib", recursive = TRUE)
dir.create("myLib")
install.packages("myTst", lib = "myLib", repos=NULL, type = "source") # with warnings
print(installed.packages(lib.loc= "myLib", priority= "NA"))## (PR#13332)
stopifnot(require("myTst",lib = "myLib"))
sm <- findMethods(show, where= as.environment("package:myTst"))
stopifnot(names(sm@names) == "foo")
unlink("myTst_*")

## More building & installing packages
## NB: tests were added here for 2.11.0.
## NB^2: do not do this in the R sources!
## and this testdir is not installed.
pkgSrcPath <- file.path(Sys.getenv("SRCDIR"), "Pkgs")
if(file_test("-d", pkgSrcPath)) {
    ## could use file.copy(recursive = TRUE)
    system(paste('cp -R', shQuote(pkgSrcPath), shQuote(tempdir())))
    pkgPath <- file.path(tempdir(), "Pkgs")
#    op <- options(warn = 2)    # There should be *NO* warnings here!
    ## pkgB tests an empty R directory
    dir.create(file.path(pkgPath, "pkgB", "R"), recursive = TRUE,
               showWarnings = FALSE)
    p.lis <- c("pkgA", "pkgB", "exNSS4")
    for(p. in p.lis) {
        cat("building package", p., "...\n")
        r <- build.pkg(file.path(pkgPath, p.))
        cat("installing package", p., "using file", r, "...\n")
        ## we could install the tar file ... (see build.pkg()'s definition)
        install.packages(r, lib = "myLib", repos=NULL, type = "source")
        stopifnot(require(p.,lib = "myLib", character.only=TRUE))
        detach(pos = match(p., sub("^package:","", search())))
    }
    ## TODO: not just print, but check the "list":
    res <- installed.packages(lib.loc = "myLib", priority = "NA")
    print(res)
#    options(op)
    unlink("myLib", recursive = TRUE)
    unlink(file.path(pkgPath), recursive = TRUE)
}
unlink("myTst", recursive=TRUE)


## getPackageName()  for "package:foo":
require('methods')
library(tools)
oo <- options(warn=2)
detach("package:tools", unload=TRUE)
options(oo)
## gave warning (-> Error) about creating package name

