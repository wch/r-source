.GlobalEnv <- globalenv()
attach(NULL, name = "Autoloads")
.AutoloadEnv <- pos.to.env(2)
assign(".Autoloaded", NULL, env=.AutoloadEnv)
T <- TRUE
F <- FALSE
version <- Version()
.Machine <- Machine()

options(na.action = "na.omit")
options(show.P.values = TRUE)
options(show.signif.stars = TRUE)
