#### Run all demos that do not depend on tcl and other specials.
.ptime <- proc.time()
.Random.seed <- c(0,rep(7654, 3))

## Currently only do `base', but others make sense, not tcltk though :

## Drop these for strict testing {and add them to \testonly{.} examples
## in ../src/library/base/man/demo.Rd }:
dont <- list(base = c("nlm", "lm.glm")
             )
for(pkg in c("base")) { ## maybe other packages; 

    demos <- list.files(file.path(system.file(pkg = pkg), "demo"),
                        pattern = "\\.R$")
    demos <- demos[is.na(match(demos, paste(dont[[pkg]], "R",sep=".")))]

    for(nam in sub("\\.R$", "", demos))
        demo(nam, character.only = TRUE)
}

cat("Time elapsed: ", proc.time() - .ptime, "\n")
