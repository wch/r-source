### This is the system Rprofile file. It is always run on startup.
### Additional commands can be placed in site or user Rprofile files
### (see ?Rprofile).

### Notice that it is a bad idea to use this file as a template for
### personal startup files, since things will be executed twice and in
### the wrong environment since user profiles are run in .GlobalEnv.

.GlobalEnv <- globalenv()
attach(NULL, name = "Autoloads")
.AutoloadEnv <- as.environment(2)
assign(".Autoloaded", NULL, env = .AutoloadEnv)
T <- TRUE
F <- FALSE
R.version <- structure(R.Version(), class = "simple.list")
version <- R.version            # for S compatibility
R.version.string <-
    paste(paste("R version",
                paste(version[c("major", "minor")], collapse = ".")),
          paste(version[c("year", "month","day")], collapse = "-"),
          sep=", ", collapse=" ")

options(na.action = "na.omit")
options(show.signif.stars = TRUE)
options(show.coef.Pvalues = TRUE)
options(keep.source = interactive())
options(warn = 0)
options(help.try.all.packages = FALSE)
options(CRAN = "http://cran.r-project.org")
options(BIOC= "http://www.bioconductor.org")
options(repositories=c(CRAN = "http://cran.r-project.org/src/contrib",
	BIOC = "http://www.bioconductor.org/src/contrib"))
options(timeout = 60)
options(internet.info = 2)
options(encoding = native.enc)
options(show.error.messages = TRUE)
options(scipen = 0)
options(locatorBell = TRUE)
local({dp <- as.vector(Sys.getenv("R_DEFAULT_PACKAGES"))
       if(identical(dp, "")) # marginally faster to do methods last
           dp <- c("ts", "nls", "modreg", "mva", "ctest", "methods")
#           dp <- c("methods", "ctest")
       else if(identical(dp, "NULL")) dp <- character(0)
       else dp <- strsplit(dp, ",")[[1]]
       dp <- sub("[[:blank:]]*([[:alnum:]]+)", "\\1", dp) # strip whitespace
       options(defaultPackages = dp)
    })
.First.sys <- function()
{
    for(pkg in getOption("defaultPackages")) {
        res <- require(pkg, quietly = TRUE, character.only = TRUE, save = FALSE)
        if(!res)
            warning("package ", pkg,
                    ' in options("defaultPackages") was not found', call.=FALSE)
    }
}
