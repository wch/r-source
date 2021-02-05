#### Checking Translation / domains etc: *NOT* called with LC_*=* or LANGUAGE=* settings

tryCid <- function(expr) tryCatch(expr, error = identity)
tryCmsg<- function(expr) tryCatch(expr, error = conditionMessage) # typically == *$message
identCO <- function(x,y, ...) identical(capture.output(x), capture.output(y), ...)
assertErrV <- function(...) tools::assertError(..., verbose=TRUE)
onWindows <- .Platform$OS.type == "windows"
.M <- .Machine
str(.M[grep("^sizeof", names(.M))]) ## also differentiate long-double..
b64 <- .M$sizeof.pointer == 8

Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")# warnings on Windows
## typically does not matter anyway, given LANGUAGE

## Translation domain for a function not in a package: PR#17998
chk0 <- function(x) stopifnot(x == 0)
LNG <- Sys.getenv("LANGUAGE") ; Sys.setenv(LANGUAGE = "fr")
(mE <- tryCmsg(chk0( ))) # "l'argument \"x\" est manquant, avec aucune valeur par défaut"
(m1 <- tryCmsg(chk0(1))) # (*not* translated in R <= 4.0.x)
               chk0(0)
if(FALSE) ## This works *sometimes* .. depending on LC_* settings *before* R is started ???
stopifnot(grepl("manquant\\b.* par d.faut", mE), # using 'é' here fails on Windows
          grepl("n'est pas", m1))
Sys.setenv(LANGUAGE = LNG); Sys.getenv("LANGUAGE") # reset to previous
## Default 'domain' for stop()  was not determined correctly in R <= 4.0.3


