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
options(repos = c(CRAN="@CRAN@"))
# options(BIOC = "http://www.bioconductor.org")

options(timeout = 60)
options(internet.info = 2)
options(encoding = "native.enc")
options(show.error.messages = TRUE)
options(scipen = 0)
options(locatorBell = TRUE)
options(ts.eps = 1e-5)   # default as S
options(pkgType = .Platform$pkgType)
options(max.print = 10000)

local({dp <- as.vector(Sys.getenv("R_DEFAULT_PACKAGES"))
       if(identical(dp, "")) # marginally faster to do methods last
           dp <- c("datasets", "utils", "grDevices", "graphics",
                   "stats", "methods")
       else if(identical(dp, "NULL")) dp <- character(0)
       else dp <- strsplit(dp, ",")[[1]]
       dp <- sub("[[:blank:]]*([[:alnum:]]+)", "\\1", dp) # strip whitespace
       options(defaultPackages = dp)
    })

.First.sys <- function()
{
    for(pkg in getOption("defaultPackages")) {
        res <- require(pkg, quietly = TRUE, warn.conflicts = FALSE,
                       character.only = TRUE, save = FALSE)
        if(!res)
            warning("package ", pkg,
                    ' in options("defaultPackages") was not found', call.=FALSE)
    }
    sch <- search()
    if(! "package:utils" %in% sch) {
        autoload("example", "utils")
        autoload("methods", "utils")
        autoload("str", "utils")
    }
    if(! "package:stats" %in% sch) {
        autoload("aov", "stats")
        autoload("lm", "stats")
        autoload("lowess", "stats")
        autoload("model.frame", "stats")
        autoload("predict", "stats")
        autoload("quantile", "stats")
        autoload("rexp", "stats")
        autoload("rnorm", "stats")
        autoload("rpois", "stats")
        autoload("rt", "stats")
        autoload("runif", "stats")
        autoload("ts", "stats")
        autoload("var", "stats")
    }
    if(! "package:graphics" %in% sch) {
        autoload("barplot", "graphics")
        autoload("boxplot", "graphics")
        autoload("contour", "graphics")
        autoload("coplot", "graphics")
        autoload("hist", "graphics")
        autoload("identify", "graphics")
        autoload("image", "graphics")
        autoload("layout", "graphics")
        autoload("lines", "graphics")
        autoload("matplot", "graphics")
        autoload("pairs", "graphics")
        autoload("par", "graphics")
        autoload("persp", "graphics")
        autoload("plot", "graphics")
        autoload("points", "graphics")
        autoload("text", "graphics")
        autoload("xy.coords", "graphics")
    }
}
