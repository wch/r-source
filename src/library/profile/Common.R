.GlobalEnv <- globalenv()
attach(NULL, name = "Autoloads")
.AutoloadEnv <- pos.to.env(2)
assign(".Autoloaded", NULL, env=.AutoloadEnv)
T <- TRUE
F <- FALSE
version <- structure(Version(), class = "simple.list")
.Machine <- Machine()

options(na.action = "na.omit")
options(show.signif.stars = TRUE)
