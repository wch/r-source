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
options(keep.source = TRUE)
options(warn = 0)
options(CRAN = "http://cran.r-project.org")

