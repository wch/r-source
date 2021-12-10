#### Regression test of translation not working outside packages for R < 4.1.0.
#### We try French (set in Makefile.common).

### First off, message translation needs to be supported.
if (!capabilities("NLS")) { ## e.g. when R was configured with --disable-nls
    message("no natural language support")
    q("no")
}

#### Report locale, LANG* and charset
Sys.getlocale()
Sys.getenv(c("LANGUAGE","LANG"))
str(l10n_info())

#### Skip locales that do not support French (especially C)
OK <- l10n_info()[["UTF-8"]] || l10n_info()[["Latin-1"]]
if (!OK) {
    if(.Platform$OS.type == "windows") {
        OK <- l10n_info()[["codepage"]] == 28605 ## Latin-9
    } else {
        z <- l10n_info()[["codeset"]]
        ## macOS and Solaris have the first, Linux the second.
        OK <- tolower(z) %in% c("iso8859-15", "iso-8859-15", "iso885915")
    }
}
if( !OK ) {
    message("The locale encoding is not known to support French")
    q("no")
}

## Translation domain for a function not in a package: PR#17998
tryCEmsg <- function(expr) tryCatch(expr, error   = conditionMessage)
tryCWmsg <- function(expr) tryCatch(expr, warning = conditionMessage)
chk0 <- function(x) stopifnot(x == 0)
nsSt <- asNamespace("stats")
(Sys.setLanguage("fr") -> oldLang) # print previous
(m1 <- tryCEmsg(chk0(1))) # (not translated in R < 4.1.0)
## switch back to English (if possible) for final report.
Sys.setLanguage("en")
m2 <- "x == 0 n'est pas TRUE"
if(m1 != m2) stop("message was not translated to French")

## More -- for PR#18902 (<--> PR#17998, part 2)
enTxt <- "incompatible dimensions"
deTxt <- "inkompatible Dimensionen"
Sys.setLanguage("de")
stopifnot(identical(deTxt, gettext(enTxt, domain="R-stats")))
f <- function(...) stop(enTxt)
environment(f) <- nsSt
stopifnot(identical(deTxt, tryCEmsg(f()))) # failed in R <= 4.1.x
## 2nd example (base vs stats):
enTxt <- "namespace is already attached"
deTxt <- "Namensraum ist bereits angehängt"
Encoding(deTxt) <- "UTF-8" # e.g. on Windows where it was  "latin1"
all.equal(gettext(enTxt, domain="R-stats"), enTxt)
(trTxt <- gettext(enTxt, domain="R-base")); Encoding(trTxt) # unknown
all.equal(trTxt, deTxt)
f <- function(...) warning(enTxt)  # warning() returns the message
environment(f) <- .BaseNamespaceEnv; trTxtB <- f(); (trTxtBt <- tryCWmsg(f()))
environment(f) <- nsSt;              trTxtS <- f(); (trTxtSt <- tryCWmsg(f()))
stopifnot(exprs = {
    identical(trTxt, deTxt)
    identical(gettext(enTxt, domain="R-stats"), enTxt)
    identical({environment(f) <- .BaseNamespaceEnv; tryCWmsg(f())}, deTxt)
    identical({environment(f) <- nsSt;              tryCWmsg(f())}, enTxt)# not in R <= 4.1.x
    identical(trTxtB , trTxt)
    identical(trTxtBt, trTxt) # (not in 4.0.5)
    identical(trTxtS , enTxt) # (not in 4.1.x, but in 4.0.5)
    identical(trTxtSt, enTxt) # (not in 4.1.x, but in 4.0.5)
})# in all cases:  not present in stats  =>  not translated

## gettextf
chk <- function(tx) stopifnot(tx == sprintf("file '%s' not found", "/foo/bR"))
f <- function() gettextf("file '%s' not found", "/foo/bR"); trTxt <- f()
chk(trTxt) # failed in R 4.1.x
trTxt <- gettextf("file '%s' not found", "/foo/bR"); chk(trTxt) # failed in R <= 4.1.x
## gettext
chk <- function(tx) stopifnot(tx == "file '%s' not found")
     (trTxt <- gettext("file '%s' not found")); chk(trTxt) # failed in R 4.1.x
print(trTxt <- gettext("file '%s' not found")); chk(trTxt) # failed in R <= 4.1.x


## Functions not *from* package namespace, but "as if" (PR#17998, from Comment 35):
enT <- "empty model supplied"
(deT <- gettext(enT, domain="R-stats"))# "leeres Modell angegeben"
isD <- function(tx) identical(deT, tx)
stopifnot(exprs = {
    ## 1-4: translated in R 4.0.z *and* 4.1.z
    isD(evalq(function() gettext(enT), nsSt)())
    isD(evalq(function() do.call(gettext, list(enT)), nsSt)())
    isD(evalq(function() evalq(gettext(enT)), nsSt)())
    isD(evalq(function() local(gettext(enT)), nsSt)())
    ## 5-7: not translated in R 4.0.*; translated in R 4.1.* (incl. R-patched)
    ##      ditto in R-devel *after* the Oct.20 (2021) patch:
    isD(evalq(local(gettext(enT)), nsSt))
    isD(evalq(gettext(enT), nsSt))
    isD(do.call("gettext", list(enT), envir=nsSt))
    ## 8-11: in comment #37, Suharto added  " Other cases: "
    isD(evalq(function() (function() gettext(enT))(), nsSt)())
    isD(evalq(function() function() gettext(enT), nsSt)()())
    isD(evalq((function() function() gettext(enT))(), nsSt)())
    isD(evalq(local(function() gettext(enT)), nsSt)())
    require(compiler) ## and more cases with byte compiler consideration
    isD(cmpfun(evalq(function() gettext(enT), nsSt))())
    isD(cmpfun(evalq(function() do.call("gettext", list(enT)), nsSt))())
    isD(cmpfun(evalq(function() evalq(gettext(enT)), nsSt))())
    isD(cmpfun(evalq(function() local(gettext(enT)), nsSt))())
    isD(eval(compile(quote(local(gettext(enT)))), nsSt))
    isD(eval(compile(quote(gettext(enT))), nsSt))
})

## Getting messages with trailing \n : either via ngettext() or w/ new trim=FALSE:
txtE <- "Execution halted\n"
Sys.setLanguage("fr")
(n <- ngettext(1, txtE, "", domain="R"))
(t. <- gettext(txtE, domain="R"))# default: translation *not* found
t.T <- gettext(txtE, domain="R", trim=TRUE)# == default
t.F <- gettext(txtE, domain="R", trim=FALSE)
cbind(t.F, n, t.T)
stopifnot(exprs = {
    identical(t. , txtE)
    identical(t.T, txtE)
    identical(t.F, n)
    t.F != t.T
})
