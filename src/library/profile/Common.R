### This is the system Rprofile file. It is always run on startup.
### Additional commands can be placed in site or user Rprofile files
### (see ?Rprofile).

### Notice that it is a bad idea to use this file as a template for
### personal startup files, since things will be executed twice and in
### the wrong environment since user profiles are run in .GlobalEnv.

.GlobalEnv <- globalenv()
attach(NULL, name = "Autoloads")
.AutoloadEnv <- pos.to.env(2)
assign(".Autoloaded", NULL, env=.AutoloadEnv)
T <- TRUE
F <- FALSE
R.version <- structure(R.Version(), class = "simple.list")
version <- .Alias(R.version)# for S-compatibility
R.version.string <- local({
    cc <- function(...) paste(..., collapse=" ")
    paste(cc("R version", paste(version[c("major","minor")],collapse=".")),
          cc(version[c("year", "month","day")]), sep=", ")
})
.Machine <- Machine()
.Platform <- Platform()

options(na.action = "na.omit")
options(show.signif.stars = TRUE)
options(show.coef.Pvalues = TRUE)
options(keep.source = interactive())# was "TRUE" in 1.0.x
options(warn = 0)
options(CRAN = "http://cran.r-project.org")

local({
    ### Autoloads :	 Should NOT stop on error --- try(.) fails..
    ### The local() makes try.autoload go away after use
    try.autoload <- function(name, file) {
	if (exists(name, envir = .GlobalEnv, inherits = FALSE))
	    warning("Object already exists")
	else
	    autoload(name, file)
    }

    ## The next 3 were in base till 0.90:
    try.autoload("t.test","ctest")
    try.autoload("chisq.test","ctest")
    try.autoload("prop.test","ctest")
    try.autoload("wilcox.test","ctest")
})
