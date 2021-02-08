#### Checking Translation / domains etc: *NOT* called with LC_*=* or LANGUAGE=* settings

if (!capabilities("NLS")) { ## e.g. when R was configured by  --disable-nls
    warning("no natural language support")
    q("no")
}

.pt <- proc.time()
tryCid <- function(expr) tryCatch(expr, error = identity)
tryCmsg<- function(expr) tryCatch(expr, error = conditionMessage) # typically == *$message
identCO <- function(x,y, ...) identical(capture.output(x), capture.output(y), ...)
assertErrV <- function(...) tools::assertError(..., verbose=TRUE)
onWindows <- .Platform$OS.type == "windows"
.M <- .Machine
str(.M[grep("^sizeof", names(.M))]) ## also differentiate long-double..
b64 <- .M$sizeof.pointer == 8

Sys.setenv(LANGUAGE = "en")
##                     ---  only has effect in conjunction with valid LC_*  ( <--> ./reg-tests-3.R )
oloc <- Sys.getlocale("LC_CTYPE")
mbyte.lc <- {
    if(.Platform$OS.type == "windows")
	"English_United States.28605"
    else if(grepl("[.]UTF-8$", oloc, ignore.case=TRUE)) # typically nowadays
	oloc
    else
	"en_US.UTF-8" # or rather "C.UTF-8" or from  system("locale -a | fgrep .utf8")
}
identical(Sys.setlocale("LC_CTYPE", mbyte.lc), mbyte.lc) # "ok" if not
if(.Platform$OS.type != "windows")# <-- check for now
    Sys.setlocale("LC_MESSAGES", mbyte.lc)

## Translation domain for a function not in a package: PR#17998
chk0 <- function(x) stopifnot(x == 0)
LNG <- Sys.getenv("LANGUAGE") ; Sys.setenv(LANGUAGE = "fr")
(mE <- tryCmsg(chk0( ))) # "l'argument \"x\" est manquant, avec aucune valeur par défaut"
(m1 <- tryCmsg(chk0(1))) # (*not* translated in R <= 4.0.x)
               chk0(0)
grepl4 <- function(pattern, x, ...)
    c(def = grepl(pattern, x, perl=FALSE, useBytes=FALSE),
      perl= grepl(pattern, x, perl=TRUE,  useBytes=FALSE),
      useB= grepl(pattern, x, perl=FALSE, useBytes=TRUE),
      BOTH= grepl(pattern, x, perl=TRUE,  useBytes=TRUE))

symnum(abbr = FALSE,
    rbind(
      aigu = grepl4("manquant\\b.* par défaut",   mE)
    ,  d   = grepl4("manquant\\b.* par d.faut",   mE)
    ,  dQ  = grepl4("manquant\\b.* par d.?faut",  mE)
    , ddQ  = grepl4("manquant\\b.* par d..?faut", mE)
    )
)
## On Windows, using
##   source("reg-translation.R", echo=TRUE, encoding="UTF-8", max.deparse.length=300)
## gives an (4 x 4 matrix) of __all__ TRUE  ~~~~~~~~~~~~~~~~

stopifnot(grepl("manquant\\b.* par d..?faut", mE), # using 'é'(e-aigu) fails on Windows
          grepl("n'est pas", m1))
Sys.setenv(LANGUAGE = LNG); Sys.getenv("LANGUAGE") # reset to previous
## Default 'domain' for stop()  was not determined correctly in R <= 4.0.3



## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
