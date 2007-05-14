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

## for backwards compatibility only
R.version.string <- R.version$version.string

## NOTA BENE: options() for non-base package functionality are in places like
##            --------- ../utils/R/zzz.R

options(keep.source = interactive())
options(warn = 0)
# options(repos = c(CRAN="@CRAN@"))
# options(BIOC = "http://www.bioconductor.org")

options(timeout = 60)
options(encoding = "native.enc")
options(show.error.messages = TRUE)
## keep in sync with PrintDefaults() in  ../../main/print.c :
options(scipen = 0)
options(max.print = 99999)# max. #{entries} in internal printMatrix()
options(add.smooth = TRUE)# currently only used in 'plot.lm'
options(stringsAsFactors = TRUE)

local({dp <- as.vector(Sys.getenv("R_DEFAULT_PACKAGES"))
       if(identical(dp, "")) # marginally faster to do methods last
           dp <- c("datasets", "utils", "grDevices", "graphics",
                   "stats", "methods")
       else if(identical(dp, "NULL")) dp <- character(0)
       else dp <- strsplit(dp, ",")[[1]]
       dp <- sub("[[:blank:]]*([[:alnum:]]+)", "\\1", dp) # strip whitespace
       options(defaultPackages = dp)
    })

## Expand R_LIBS_* environment variables.
Sys.setenv(R_LIBS_SITE =
           .expand_R_libs_env_var(Sys.getenv("R_LIBS_SITE")))
Sys.setenv(R_LIBS_USER =
           .expand_R_libs_env_var(Sys.getenv("R_LIBS_USER")))

.First.sys <- function()
{
    for(pkg in getOption("defaultPackages")) {
        res <- require(pkg, quietly = TRUE, warn.conflicts = FALSE,
                       character.only = TRUE, save = FALSE)
        if(!res)
            warning("package ", pkg,
                    ' in options("defaultPackages") was not found', call.=FALSE)
    }
}

.OptRequireMethods <- function()
{
      if("methods" %in% getOption("defaultPackages")) {
        res <- require("methods", quietly = TRUE, warn.conflicts = FALSE,
                       character.only = TRUE, save = FALSE)
        if(!res)
            warning("package \"methods\"",
                    ' in options("defaultPackages") was not found', call.=FALSE)
    }
}

if(Sys.getenv("R_BATCH") != "") {
    .Last.sys <- function()
    {
        cat("> proc.time()\n")
        print(proc.time())
    }
    ## avoid passing on to spawned R processes
    ## A system has been reported without Sys.unsetenv, so try this
    Sys.setenv(R_BATCH="")
}
