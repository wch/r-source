### This is the system Rprofile file. It is always run on startup.
### Additional commands can be placed in site or user Rprofile files
### (see ?Rprofile).

### Notice that it is a bad idea to use this file as a template for
### personal startup files, since things will be executed twice and in
### the wrong environment since user profiles are run in .GlobalEnv.

.GlobalEnv <- globalenv()
attach(NULL, name = "Autoloads")
.AutoloadEnv <- pos.to.env(2)
assign(".Autoloaded", NULL, env = .AutoloadEnv)
T <- TRUE
F <- FALSE
R.version <- structure(R.Version(), class = "simple.list")
version <- .Alias(R.version)            # for S compatibility
R.version.string <-
    paste(paste("R version",
                paste(version[c("major", "minor")], collapse = ".")),
          paste(version[c("year", "month","day")], collapse = "-"),
          sep=", ", collapse=" ")

.Machine <- Machine()
.Platform <- Platform()

options(na.action = "na.omit")
options(show.signif.stars = TRUE)
options(show.coef.Pvalues = TRUE)
options(keep.source = interactive())    # was `TRUE' in 1.0.x
options(warn = 0)
options(help.try.all.packages = FALSE)
options(CRAN = "http://cran.r-project.org")
.First <- function() {
    require("ctest", quietly=TRUE)
}
