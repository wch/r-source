## Regression tests for R >= 4.3.0

.pt <- proc.time()
tryCid <- function(expr) tryCatch(expr, error = identity)
tryCmsg<- function(expr) tryCatch(expr, error = conditionMessage) # typically == *$message
assertErrV <- function(...) tools::assertError(..., verbose=TRUE)
`%||%` <- function (L, R)  if(is.null(L)) R else L
##' get value of `expr` and keep warning as attribute (if there is one)
getVaW <- function(expr) {
    W <- NULL
    withCallingHandlers(val <- expr,
                        warning = function(w) {
                            W <<- conditionMessage(w)
                            invokeRestart("muffleWarning") })
    structure(val, warning = W)
}
options(nwarnings = 10000, # (rather than just 50)
        warn = 2, # only caught or asserted warnings
        width = 99) # instead of 80

onWindows <- .Platform$OS.type == "windows"
englishMsgs <- {
    ## 1. LANGUAGE takes precedence over locale settings:
    if(nzchar(lang <- Sys.getenv("LANGUAGE")))
        lang == "en"
    else { ## 2. Query the  locale
        if(!onWindows) {
            ## sub() :
            lc.msgs <- sub("\\..*", "", print(Sys.getlocale("LC_MESSAGES")))
            lc.msgs == "C" || substr(lc.msgs, 1,2) == "en"
        } else { ## Windows
            lc.type <- sub("\\..*", "", sub("_.*", "", print(Sys.getlocale("LC_CTYPE"))))
            lc.type == "English" || lc.type == "C"
        }
    }
}
cat(sprintf("English messages: %s\n", englishMsgs))


## very small size hashed environments
n <- 123
l <- setNames(vector("list", n), seq_len(n))
ehLs <- lapply(1:6, function(sz) list2env(l, hash=TRUE, size = sz))
(nch <- vapply(ehLs, \(.) env.profile(.)$nchains, 0))# gave  1 2 3 4 109 109
stopifnot(nch >= 24) # seeing  106 .. 106 111
## hashed environments did not grow for size <= 4 in  R <= 4.1.x


## as.character.Rd(deparse = TRUE) with curly braces in TEXT -- PR#18324
rd <- tools::parse_Rd(textConnection(txt0 <- r"(\link[=Paren]{\{})"),
                      fragment = TRUE)
cat(txt1 <- paste0(as.character(rd, deparse = TRUE), collapse = ""))
stopifnot(identical(paste0(txt0, "\n"), txt1))
## failed to re-escape curly brace in R <= 4.2.x
## curly braces used for grouping tokens are not escaped:
rdgroup <- tools::parse_Rd(textConnection(r"(a {b} c)"), fragment = TRUE)
stopifnot(identical(as.character(rdgroup, deparse = TRUE),
                    as.character(rdgroup, deparse = FALSE)))
##


## Errors from parsing (notably with |> ) now return *classed* errors with line numbers
## From  PR#18328 - by Duncan Murdoch
txts <- setNames(, c(
    "f <- function(x, x) {}"
  , "123 |> str"
  , "123 |> return()"
  , "123 |> `+`(_, 4)"
  , "123 |> (_ + 4)"
  , "123 |> f(a = _, b = _)"
  , "123 |> (\\(x) foo(bar = _))()"
  , "123 |> x => log(x)"
  , "'\\uh'"
  , "'\\Uh'"
  , "'\\xh'"
  , "'\\c'"
  , "'\\0'"
  , "'\\U{badf00d}"
  , "'\\Ubadf00d"
))
errs <- lapply(txts, function(ch) tryCatch(parse(text = ch), error=identity))
## nicely print them
msgs <- lapply(errs, `[[`, "message") ; str(msgs)
(cls <- t(sapply(errs, class)))
uerrs <- unname(errs) # (speed)
nL <- vapply(uerrs, `[[`, 0L, "lineno")
nC <- vapply(uerrs, `[[`, 0L, "colno")
stopifnot(exprs = {
    vapply(uerrs, inherits, NA, what = "error")
    vapply(uerrs, inherits, NA, what = "parseError")
    nL == 1L
    nC == c(18L, rep(8L, 6), 10L, rep(3L, 5), 12L, 10L)
    ## see all "<l>:<n>" strings as part of the message:
    mapply(grepl, paste(nL, nC, sep = ":"), msgs)
})
## gave just simpleError s; no line:column numbers in R <= 4.2.0


## fisher.test() with "too full" table:  PR#18336
d <- matrix(c(1,0,5,2,1,90
             ,2,1,0,2,3,89
             ,0,0,0,1,0,14
             ,0,0,0,0,0, 5
             ,0,0,0,0,0, 2
             ,0,0,0,0,0, 2
              ), nrow=6, byrow = TRUE)
(r <- tryCid(fisher.test(d)))
stopifnot(inherits(r, "error"))
if(englishMsgs)
    stopifnot(grepl("hash key .* > INT_MAX", conditionMessage(r)))
## gave a seg.fault in R <= 4.2.0


## Testing fix for PR#18344 [ tar() warning about illegal uid/gid ]:
sys <- Sys.info() # Only 'root' can create files with illegal uid/gid
if(sys[["sysname"]] == "Linux" & sys[["effective_user"]] == "root"
   ## not a "weakened root".  Or just && !nzchar(Sys.getenv("container")) :
   && !(Sys.getenv("container") %in% c("oci", "docker", "podman"))
   ) {
    dir.create(mdir <- file.path(tempdir(),"stuff"))
    for(f in letters[1:3])
        writeLines("first line", file.path(mdir, f))
    owd <- setwd(tempdir())
    system(paste("chown 654321 stuff/a")) ## system(paste("chgrp 123456 stuff/b"))
    r <- tryCatch( tar('stuff.tar', "stuff"), warning = identity)
    stopifnot(inherits(r, "warning"))
    if(englishMsgs)
        stopifnot(grepl("^invalid uid ", conditionMessage(r)))
    ## cat("Inside directory ", getwd(),":\n"); system("ls -l stuff.tar")
    setwd(owd)# go back
} else
    message("You are not root, hence cannot change uid / gid to invalid values")
## gave 2 warnings per wrong file; the first being    In sprintf(gettext(....):
##    "one argument not used by format 'invalid uid value replaced .... 'nobody''"


## sort(x, partial, *) notably for na.last=FALSE and TRUE -- PR#18335
chkSortP <- function(x, partial) {
    stopifnot(partial == as.integer(partial),
              1 <= partial, partial <= length(x))
    nok <- sum(!is.na(x))
    if(anyNA(x) && any(partial > nok)) ## cannot use na.last=NA
         Ls <- c(   FALSE,TRUE)
    else Ls <- c(NA,FALSE,TRUE)
    S <- lapply(Ls, function(v) sort(x, na.last=v))
    P <- lapply(Ls, function(v) sort(x, na.last=v, partial=partial))
    ok1 <- identical(lapply(S, `[`, partial),
                     lapply(P, `[`, partial))
    ## test "ones below" and "ones above" the (min and max) partials
    mip <- min(partial)
    map <- max(partial)
    noNA <- function(u) u[!is.na(u)]
    chkPord <- function(y) {
        n <- length(y)
        all(noNA(y[if(mip > 1) 1L:(mip-1L)]) <= noNA(y[mip])) &&
        all(noNA(y[if(map < n)  (map+1L):n]) >= noNA(y[map]))
    }
    ok1 && all(vapply(P, chkPord, logical(1)))
}

x <- c(7, 2, 4, 5, 3, 6, NA)
x1 <- c( 2,3,1, NA)
x2 <- c(NA,3,1, NA)
x14 <- c(7, 2, 0, 8, -1, -2, 9, 4, 5, 3, 6, 1, NA,NA)
stopifnot(exprs = {
    chkSortP(x, partial = 3)
    chkSortP(x, partial = c(3,5))
    chkSortP(x1, partial = 3)
    chkSortP(x1, partial = 4)
    chkSortP(x1, partial = 3:4)
    chkSortP(x2, partial = 4)
    chkSortP(x2, partial = 3)
    chkSortP(x2, partial = 2:4)
    sapply(seq_along(x14), function(p) chkSortP(x14, partial = p))
    chkSortP(x14, partial = c(10, 13))
    chkSortP(x14, partial = c(2, 14))
})
set.seed(17)
for(i in 1:128) { # tested for 1:12800
    x <- runif(rpois(1, 100))
    x[sample(length(x), 12)] <- NA
    p <- sample(seq_along(x), size = max(1L, rpois(1, 3)))
    stopifnot(chkSortP(x, partial = p))
}
## several of these failed for na.last=FALSE and TRUE


## head(letters, "7") should not silently do nonsense; PR#18357
assertErrV( head(letters, "3") )
## returned complete 'letters' w/o a warning
stopifnot(identical("a", head(letters, TRUE)))
## keep treating <logical> n  as integer


## x[[]] should give error in all cases, even for NULL;  PR#18367
(E <- tryCid(c(a = 1, 2)[[]]))
xx <- c(a = 1, 2:3)
E2 <- tryCid(xx[[]])
EN <- tryCid(NULL[[]]) # <=> c()[[]]
stopifnot(exprs = {
    inherits(E, "error")
    inherits(E, "MissingSubscriptError")
    identical(quote(c(a = 1, 2)[[]]), E$call)
    identical(class(E), class(E2))
    identical(class(E), class(EN))
    identical(msg <- "missing subscript", conditionMessage(E2))
    identical(msg, conditionMessage(EN))
    (nm <- c("call","object")) %in% names(EN)
    identical(EN[nm], list(call = quote(NULL[[]]), object = NULL))
})
## [[]]  matched '2' as which has name ""
E <- tryCid(xx[[]] <- pi)
stopifnot(inherits(E, "MissingSubscriptError"))
## using new error class


## PR#18375, use PRIMNAME not *VAL in message:
(M <- tryCmsg(date > 1))
stopifnot(grepl("(>)", M, fixed=TRUE))
## showed '(6)' previously


## isGeneric() with wrong name -- correct warning msg (PR#18370)
setGeneric("size", function(x) standardGeneric("size"))
tryCatch(stopifnot(!isGeneric("haha", fdef = size)),
         warning = conditionMessage) -> msg
msg; if(englishMsgs)
    stopifnot(grepl("name .size. instead of .haha.", msg))
## msg was confusing


### poly(<Date>,*) etc:  lm(... ~ poly(<Date>, .)) should work :
d. <- data.frame(x = (1:20)/20, f = gl(4,5), D = .Date(17000 + c(1:7, 1:13 + 100)))
cf0 <- c(Int=100, x=10, f = 5*(1:3))
nD <- as.numeric(d.[,"D"])
y0 <- model.matrix(~x+f, d.) %*% cf0  +   10*(nD - 17000) - 20*((nD - 17000)/10)^2
set.seed(123)
head(d. <- cbind(d., y = y0 + rnorm(20)))
fm1  <- lm(y ~ x + f + poly(D,3), data = d.)
fm1r <- lm(y ~ x + f + poly(D,2, raw=TRUE), data = d.)
newd <- data.frame(x = seq(1/3, 1/2, length=5), f = gl(4,5)[5:9], D = .Date(17000 + 51:55))
yhat <- unname(predict(fm1,  newdata = newd))
yh.r <- unname(predict(fm1r, newdata = newd))
cbind(yhat, yh.r)
stopifnot(all.equal(yhat, c(96.8869, 92.3821, 81.9967, 71.2076, 60.0147), tol=1e-6), # 3e-7
          all.equal(yh.r, c(97.7595, 93.0218, 82.3533, 71.2806, 59.8036), tol=1e-6))
## poly(D, 3) failed since R 4.1.x,  poly(.., raw=TRUE) in all earlier R versions


## as.difftime() tweaks: coerce to "double", keep names
stopifnot(
    identical(as.difftime(c(x = 1L), units="secs"),
                .difftime(c(x = 1.), units="secs")))
## integers where kept (and difftime arithmetic could overflow) in R <= 4.2.x


## ordered() with missing 'x' -- PR#18389
factor( levels = c("a", "b"), ordered=TRUE) -> o1
ordered(levels = c("a", "b")) -> o2
stopifnot(identical(o1,o2))
## the ordered() call has failed in R <= 4.2.x


## source() with multiple encodings
if (l10n_info()$"UTF-8" || l10n_info()$"Latin-1") {
    writeLines('x <- "fa\xE7ile"', tf <- tempfile(), useBytes = TRUE)
    tools::assertError(source(tf, encoding = "UTF-8"))
    source(tf, encoding = c("UTF-8", "latin1"))
    ## in R 4.2.{0,1} gave Warning (that would now be an error):
    ##   'length(x) = 2 > 1' in coercion to 'logical(1)'
    if (l10n_info()$"UTF-8") stopifnot(identical(Encoding(x), "UTF-8"))
}


## multi-line Rd macro definition
rd <- tools::parse_Rd(textConnection(r"(
\newcommand{\mylongmacro}{
  \LaTeX
}
\mylongmacro
)"), fragment = TRUE)
tools::Rd2txt(rd, out <- textConnection(NULL, "w"), fragment = TRUE)
stopifnot(any(as.character(rd) != "\n"),
          identical(textConnectionValue(out)[2L], "LaTeX")); close(out)
## empty output in R <= 4.2.x



## expand.model.frame() for non-data fits (PR#18414)
y <- 1:10
g <- gl(2, 5)
fit <- lm(log(y) ~ g, subset = y > 3)
mf <- expand.model.frame(fit, ~0)
stopifnot(all.equal(fit$model, mf, check.attributes = FALSE))
myexpand <- function(model) expand.model.frame(model, "y")
stopifnot(identical(myexpand(fit)$y, 4:10))  # failed in R <= 1.4.1 (PR#1423)
env <- list2env(list(y = y, g = g))
rm(y, g)
fit2 <- with(env, lm(log(y) ~ g, subset = y > 3))
stopifnot(identical(myexpand(fit2)$y, 4:10)) # failed in R <= 4.2.1 with
## Error in eval(predvars, data, env) : object 'y' not found


## time() returning numbers very slightly on the wrong side of an integer
x <- ts(2:252, start = c(2002, 2), freq = 12)
true.year <- rep(2002:2022, each = 12)[-1]
stopifnot(floor(as.numeric(time(x))) == true.year)
## seen 10 differences in R <= 4.2.x


## Sorted printing of factor analysis loadings with 1 factor, PR#17863
f1 <- factanal(d <- mtcars[,1:4], factors = 1) ; print(f1, sort=TRUE)
prl <- capture.output(print(loadings(f1), sort=TRUE))
stopifnot(identical(1:4, charmatch(colnames(d),
                                   prl[charmatch("Loadings", prl)+ 1:4+1L])))
## printed these as vector instead of 1-column matrix in R <= 4.2.x


## print() of zero - length, PR#18422
i0 <- integer(0)
stopifnot(exprs = {
    identical("<0-length octmode>", capture.output(as.octmode(i0)))
    identical("<0-length hexmode>", capture.output(as.hexmode(i0)))
    identical("<0-length roman>",   capture.output(as.roman  (i0)))
    identical("person()",           capture.output(    person()))
    identical("bibentry()",         capture.output(  bibentry()))
    identical("<0-length citation>",capture.output(  citation()[0L]))
})
## printed nothing at all or invalid R-code in R <= 4.2.x


## isS3method() for names starting with a dot
stopifnot(!isS3method(".Internal"))
## failed with "invalid first argument" in R <= 4.2.x


## cor.test.formula() scoping issue -- PR#18439
form <- ~ CONT + INTG
local({
    USJudgeRatings <- head(USJudgeRatings)
    stopifnot(cor.test(form, data = USJudgeRatings)$parameter == 4)
})
## R <= 4.2.x evaluated the constructed call in environment(formula)


## PR # 18421 by Benjamin Feakins (and follow up):
## ---------- "roman", "hexcode" and "octcode" all cannot easily be added to data frames
for(x in list(as.roman(1:14), as.octmode(1:11), as.hexmode(1:19), as.raw(0:65), (0:17) %% 7 == 0,
              as.difftime(c(0,30,60:64), units="mins"),
              seq(ISOdate(2000,2,10), by = "23 hours", length.out = 50)
 )) {
  cat("x:"); str(x, vec.len=8)
  ## the error can be triggered by the following methods:
  ### 1. as.data.frame()
  dat1 <- as.data.frame(x) # now works, previously signalled
  ## Error in as.data.frame.default(x) :
  ##   cannot coerce class ‘"roman"’ to a data.frame
  ### 2. data.frame()
  dat2 <- data.frame(x) # gave error as above, now works
  stopifnot(identical(dat1, dat2),
            identical(   data.frame(my.x = x),
                      as.data.frame(x, nm="my.x")))
  ### 3. cbind()
  dat3 <- data.frame(y = seq_along(x))
  dat3 <- cbind(dat3, x) # gave error, now works
  stopifnot(identical(dat2, dat3[,"x", drop=FALSE]))
  ## These worked already previously:
  dat <- data.frame(x = integer(length(x)))
  dat$x <- x
  datl <- list2DF(list(x=x))
  stopifnot(identical(dat, dat2),
            identical(dat, datl))
}
## --- such data.frame() coercions gave errors in R <= 4.2.x


## Deprecation of {direct calls to}  as.data.frame.<cls>()
cls <- c("raw", "logical", "integer", "numeric", "complex",
         "factor", "ordered", "Date", "difftime", "POSIXct",
         "noquote", "numeric_version")
names(cls) <- cls
be <- baseenv()
asF  <- lapply(cls, \(cl) be[[paste0("as.",cl)]] %||% be[[cl]])
## objects of the respective class:
obs  <- lapply(cls, \(cl) asF[[cl]](switch(cl, "difftime" = "2:1:0", "noquote" = letters,
                                           "numeric_version" = as.character(1:2), 1:2)))
asDF <- lapply(cls, \(cl) getVaW(be[[paste0("as.data.frame.", cl)]](obs[[cl]])))
r <- local({ g <- as.data.frame.logical; f <- function(L=TRUE) g(L)
    getVaW(f()) })
dfWarn <- "deprecated.*as\\.data\\.frame\\.vector"
stopifnot(exprs = {
    vapply(obs, \(.) class(.)[1], "") == cls
    vapply(asDF, is.data.frame, NA)
    ## the first column of each data frame is of the original class:
    vapply(lapply(asDF, `[[`, 1), \(.) class(.)[1], "") == cls
    ## all should have a deprecation warning
    is.character(asDwarn <- vapply(asDF, attr, "<text>", "warning"))
    !englishMsgs || all(grepl(dfWarn, asDwarn))
    length(unique(vapply(cls, \(cl) sub(cl, "<class>", asDwarn[[cl]], fixed=TRUE), ""))) == 1L
    all.equal(r, data.frame(L=TRUE), check.attributes=FALSE)
    !englishMsgs || grepl(dfWarn, attr(r, "warning"))
})
## as.data.frame.<cls>(.) worked w/o deprecation warning in R <= 4.2.x

## useMethod() dispatch error in case of long class strings - PR#18447
mkCh <- function(n, st=1L) substr(strrep("123456789 ", ceiling(n/10)), st, n)
useMethErr <- function(n=500, nrep=25)
    (function(.) UseMethod("foo")(.))(
        structure(1, class = paste(sep=":", format(1:nrep),
                                   mkCh(n, 2L + (nrep > 9)))))
tools::assertError( useMethErr(500,25) )
## gave a segfault  in R <= 4.2.2
clsMethErr <- function(...) {
 sub(    '"[^"]*$', "",
     sub('^[^"]*"', "", tryCmsg(useMethErr(...))))
}
showC <- function(..., n1=20, n2=16) {
    r <- clsMethErr(...)
    cat(sprintf('%d: "%s<....>%s"\n', (nr <- nchar(r)),
                substr(r, 1,n1), substr(r, nr-n2, nr)))
    invisible(r)
}
invisible(lapply(11:120, function(n) showC(n, 1030 %/% n)))
## (mostly the truncation works "nicely", but sometimes even misses the closing quote)


## download.file() with invalid option -- PR#18455
op <- options(download.file.method = "no way")
# website does not matter as will not be contacted.
Edl <- tryCid(download.file("http://cran.r-project.org/", "ping.txt"))
stopifnot(inherits(Edl, "error"),
          !englishMsgs || grepl("should be one of .auto.,", conditionMessage(Edl)))
options(op)
## error was  "object 'status' not found"  in R <= 4.2.2


## handling of invalid Encoding / unsupported conversion in packageDescription()
dir.create(pkgpath <- tempfile())
writeLines(c("Version: 1.0", "Encoding: FTU-8"), # (sic!)
           file.path(pkgpath, "DESCRIPTION"))
stopifnot(suppressWarnings(packageVersion(basename(pkgpath),
                                          dirname(pkgpath))) == "1.0")
## outputs try()-catched iconv() errors but does not fail
## gave a "packageNotFoundError" in 3.5.0 <= R <= 4.2.2


## format.bibentry() with preloaded Rd macros
ref <- bibentry("misc", author = "\\authors", year = 2023)
macros <- tools::loadRdMacros(textConnection("\\newcommand{\\authors}{\\R}"))
stopifnot(identical(print(format(ref, macros = macros)), "R (2023)."))
## macro definitions were not used in R <= 4.2.2


## predict.lm() environment used for evaluating offset -- PR#18456
mod <- local({
     y <- rep(0,10)
     x <- rep(c(0,1), each=5)
     list(lm(y ~ x),
          lm(y ~ offset(x)))
})
stopifnot(exprs = {
    ## works fine, using the x variable of the local environment
    identical(predict(mod[[1]], newdata=data.frame(z=1:10)),
              setNames(rep(0,10), as.character(1:10)))
    ## gave  error in offset(x) : object 'x' not found :
    identical(predict(mod[[2]], newdata=data.frame(z=1:10)),
              setNames(rep(c(-.5,.5), each=5), as.character(1:10)))
})
x <- rep(1,5)
mod2 <- local({
    x <- rep(2,5) # 2, not 1
    y <- rep(0,5)
    lm(rep(0,5) ~ x + offset({ print("hello"); x+2*y }),
       offset = { print("world"); x-y })
}) # rank-deficient in "subtle" way {warning/NA may not be needed}; just show for now:
nd <- data.frame(x = 1:5)
tools::assertWarning(print(predict(mod2, newdata=nd, rankdeficient = "warnif")))
                           predict(mod2, newdata=nd, rankdeficient = "NA")
nm5 <- as.character(1:5)
stopifnot(exprs = {
    all.equal(setNames(rep(0, 5), nm5), predict(mod2), tol=1e-13) # pred: 1.776e-15
    is.numeric(p2 <- predict(mod2, newdata = data.frame(y=rep(1,5)))) # no warning, no NA:
    identical(p2,    predict(mod2, newdata = data.frame(y=rep(1,5)), rankdeficient="NA"))
    all.equal(p2, setNames(rep(1, 5), nm5), tol=1e-13)# off.= x+2y + x-y = 2x+y =4+1=5; 5+<intercept> = 1
})
## fine, using model.offset() now


## "numeric_version" methods
x <- numeric_version(c("1", "2.0"))
stopifnot(identical(format(x[,2]), c(NA_character_, "0")))
is.na(x)[1] <- TRUE; stopifnot(identical(is.na(x), c(TRUE, FALSE)))
## gave two spurious warnings in R <= 4.2.2


mChk <- function(m) stopifnot(exprs = {
    identical(attributes(m), list(dim=2:3))
    identical(class(m), c("matrix", "array"))
})
(m <- m0 <- diag(1:3, 2,3))
mChk(m)
##
class(m) <- "matrix"  # instead of c("matrix", "array") ...
mChk(m); stopifnot(identical(m, m0))# .. m is *unchanged* - back compatibly
## since R 4.0.0,
class(m) # is  "matrix" "array"
class(m) <- class(m) # should *not* change 'm'
mChk(m); stopifnot(identical(m, m0))# m is unchanged as it should, but
## failed in R version v  4.0.0 <= v <= 4.2.x : 'm' got a class *attribute* there.


## uniroot() close to Inf function values could wrongly converge outside interval
f <- function(x) 4469/x - 572/(1-x)
str(urf <- uniroot(f, c(1e-6, 1))) # interval = (eps, 1]; f(1) = Inf
stopifnot(all.equal(urf$root, 0.88653, tolerance = 1e-4))
## Instead of 0.886.., gave a small *negative* root in R < 4.3.0


## chkDots() in subset.data.frame() to prevent usage errors
tools::assertWarning(subset(data.frame(y = 1), y = 2), verbose = TRUE)
## R < 4.3.0 was silent about unused ... arguments


## a:b -- both should be of length 1  -- PR#18419
Sys.getenv("_R_CHECK_LENGTH_COLON_") -> oldV
Sys.setenv("_R_CHECK_LENGTH_COLON_" = "true")# ~> the future behavior
a <- 1:2
assertErrV(a:1) # numerical expression has length > 1
assertErrV(2:a) #  "         "          "      "
Sys.unsetenv("_R_CHECK_LENGTH_COLON_")
tools::assertWarning(s1 <- a:1, verbose=TRUE)
tools::assertWarning(s2 <- 2:a, verbose=TRUE)
stopifnot(identical(s1, 1L), identical(s2, 2:1))
Sys.setenv("_R_CHECK_LENGTH_COLON_" = oldV)# reset
## always only warned in R <= 4.2.z


## keep rm(list=NULL) working (now documentedly) {PR#18422}
a <- ls() # 'a' is part
rm() ; rm(list=NULL)
stopifnot(identical(a, ls()))
## (for a short time, list=NULL failed)


## ns() fails when quantiles end on the boundary {PR#18442}
if(no.splines <- !("splines" %in% loadedNamespaces())) require("splines")
tt <- c(55, 251, 380, 289, 210, 385, 361, 669)
nn <- rep(0:7, tt) # => knots at (0.25,0.5,0.75); quantiles = (2,5,7)
tools::assertWarning(verbose=TRUE, ns.n4 <- ns(nn,4))
stopifnot(is.matrix(ns.n4), ncol(ns.n4) == 4, qr(ns.n4)$rank == 4)
if(no.splines) unloadNamespace("splines")
## ns() gave  Error in qr.default(t(const)) : NA/NaN/Inf in foreign function call


## Krylov's issue with sum(), min(), etc on R-devel: Now errors instead of silently computing:
mT <- tryCid( sum(3,4,na.rm=5, 6, NA, 8, na.rm=TRUE) )
mF <- tryCid( min(3,4,na.rm=5, 6, NA, 8, na.rm=FALSE) )
stopifnot(inherits(mT, "error"),
          inherits(mF, "error"), all.equal(mT, mF))
if(englishMsgs)
    stopifnot(grepl("formal argument \"na.rm\" matched by multiple", conditionMessage(mT)))
## these gave numeric (or NA) results without any warning in R <= 4.3.z


## as.complex(NA_real_) |-> ... Im(.) == 0 as for NA (logical) and NA_integer_
if(FALSE) # in R-devel Apr--Sep 2023 only:
stopifnot(identical(as.complex(NA_real_), NA_complex_))
Cmplx <- function(re = numeric(), im = numeric()) complex(real = re, imaginary = im)
showC <- function(z) noquote(sprintf("(R = %g, I = %g)", Re(z), Im(z)))
showC(print(asCnum <- Cmplx(NA_real_, 0)))
showC(print(NA_rP0i <- NA_real_ + 0i)) # arithmetic here via as.complex(.)
stopifnot(exprs = {
    identical(as.complex(NA_real_),    asCnum)
    identical(as.complex(NA_integer_), asCnum)
    identical(as.complex(NA),          asCnum)
    identical(NA_rP0i,                 asCnum)
    identical(as.complex( NaN), Cmplx(NaN, 0))
    identical(as.complex( Inf), Cmplx(Inf, 0))
    identical(as.complex(-Inf),Cmplx(-Inf, 0))
})
## as.complex( <real-number-like> ) keeps imaginary part 0 even for NA


## methods() in {base} pkg are visible
mmeths <- methods(merge)
imeth <- attr(mmeths, "info")
stopifnot(exprs = {
    sum(iB <- imeth[,"from"] == "base") >= 2 # 'default' and 'data.frame'
    imeth[iB, "visible"] # {base} methods *are* visible
})
## was wrong in R 4.3.0 (and R-devel for a while)


## Methods of a non-generic function
foo <- function(x) { bar(x) }
(m <- methods(foo))
stopifnot(inherits(m, "MethodsFunction"), length(m) == 0L)
## .S3methods() failed in R-devel for a few days after r84400.


## getS3method() error
myFUN <- function(x) UseMethod("myFUN")
(msg <- tryCmsg(getS3method("myFUN", "numeric")))
if(englishMsgs)
    stopifnot(grepl("S3 method 'myFUN.numeric' not found", msg))
## failed with "wrong" message after r84400


## head(.,n) / tail(.,n) error reporting - PR#18362
try(head(letters, 1:2)) # had "Error in checkHT(..)"); now ".. in head.default(..)"
try(tail(letters, NA))
try(head(letters, "1"))
tryCcall1 <- function(expr) tryCid(expr)$call[[1L]]
stopifnot(exprs = {
    tryCcall1(head(letters, 1:2)) == quote(head.default)
    tryCcall1(tail(letters, NA )) == quote(tail.default)
    tryCcall1(head(letters, "1")) == quote(head.default)
})
## more helpful error msg


## na.contiguous() w/ result at beginning -- Georgi Boshnakov, R-dev, 2023-06-01
## and does not set "tsp" for non-ts
x <- c(1:3, NA, NA, 6:8, NA, 10:12)
(naco <- na.contiguous(      x ))
(nact <- na.contiguous(as.ts(x)))
dput( setdiff(attributes(nact), attributes(naco)) ) # and check -- TODO
n0 <- numeric(0)
stopifnot(identical(`attributes<-`(naco, NULL), 1:3)
        , identical(na.contiguous(n0), n0)
        , is.null(attr(naco, "tsp"))
        , nact == naco
        , identical(c(na.contiguous(presidents)), presidents[32:110])
          )
## 'naco' gave *2nd*, not *first* run till R 4.3.0


## accidental duplicated options() entry
if(i <- anyDuplicated(no <- names(ops <- options())))
    stop("duplicated options(): ", no[i])
## had one in R 4.3.0 (and R-devel)


## .S3methods() and methods() in  R 4.3.0
library(stats)# almost surely unneeded
##
methi <- function(...) attr(methods(...), "info")
(mdensi <- methi(density)) # only density.default
stopifnot(mdensi["density.default", "visible"]) # FALSE in R 4.3.0
if(requireNamespace('cluster', lib.loc=.Library, quietly = TRUE)) withAutoprint({
    try(detach("package:cluster"), silent=TRUE)# just in case
    (mCf1 <- methi(coef))

    require(cluster)
    (mCf2 <- methi(coef))
    stopifnot(mCf2["coef.hclust", "visible"],
              mCf2["coef.hclust", "from"] == "cluster")
    ## ... and
    detach("package:cluster")
    (mcf <- methods(coef)) # again gets marked as invisible:  coef.hclust*
    stopifnot(!attr(mcf, "info")["coef.hclust", "visible"])
}) # when  {cluster}
## in any case {and "always" worked}:
coef.foo <- function(object, ...) "the coef.foo() method"
(m3 <- methi(coef))# -> coef.foo is visible in .GlobalEnv
stopifnot(m3["coef.foo", "visible"],
          m3["coef.foo", "from"] == ".GlobalEnv")
## *and* this is still true, after registering it:
.S3method("coef", "foo", coef.foo)
stopifnot(identical(methi(coef), m3)) # did not change
rm(coef.foo)
m4 <- methi(coef)
stopifnot(!m4["coef.foo", "visible"],
           m4["coef.foo", "from"] == "registered S3method for coef")
## coef.foo  part  always worked


## R <= 4.3.z would split into two invalid characters (PR#18546)
splitmbcs <- length(strsplit("\u00e4", "^", perl=TRUE)[[1]])
stopifnot(identical(splitmbcs, 1L))


## contrib.url() should "recycle0"
stopifnot(identical(contrib.url(character()), character()))
## R <= 4.3.1 returned "/src/contrib" or similar


## .local() S4 method when generic has '...'  *not* at end, PR#18538
foo <- function(x, ..., z = 22) z
setMethod("foo", "character", function(x, y = -5, z = 22) y)
stopifnot(identical(foo("a"), -5))
removeGeneric("foo")
## foo("a") gave 22 in R <= 4.3.z


## `substr<-` overrun in case of UTF-8 --- private bug report by 'Architect 95'
s0 <- "123456"; nchar(s0) #  6
substr(s0, 6, 7) <- "cc"
s0 ; nchar(s0) # {"12345c", 6}: all fine: no overrun, silent truncation
(s1 <- intToUtf8(c(23383, 97, 97, 97, 97, 97))); nchar(s1)  # "字aaaaa" , 6
substr(s1, 6, 7) <- "cc"
# Now s1 should be "字aaaac", but  actually did overrunn nchar(s1);
s1; nchar(s1) ## was "字aaaacc", nchar  = 7
(s2 <- intToUtf8(c(23383, 98, 98))); nchar(s2)  # "字bb" 3
substr(s2, 4, 5) <- "dd" # should silently truncate as with s0:
## --> s2 should be "字bb", but was "字bbdddd\x97" (4.1.3) or "字bbdd字" (4.3.1)
s2; nchar(s2) ## was either 6 or  "Error ... : invalid multibyte string, element 1"
#-------------
## Example where a partial UTF-8 character is included in the second string
## 3) all fine
(s3 <- intToUtf8(c(23383, 97, 97, 97, 97, 97))); nchar(s3)  # "字aaaaa" 6
substr(s3, 6, 6) <- print(intToUtf8(23383))  # "字"
s3 ; nchar(s3) # everything as expected:  ("字aaaa字", 6)
## 4) not good
(s4 <- intToUtf8(c(23383, 98, 98, 98, 98))); nchar(s4) # "字bbbb" 5
substr(s4, 5, 7) <- "ddd"
# Now s4 should be "字bbbd", but was "字bbbddd\x97", (\x97 = last byte of "字" in UTF-8)
s4; nchar(s4)## gave "字bbbddd\x97" and "Error ...: invalid multibyte string, element 1"
stopifnot(exprs = {
    identical(s0, "12345c") # always ok
    identical(utf8ToInt(s1), c(23383L, rep(97L, 4), 99L))           ; nchar(s1) == 6
    identical(utf8ToInt(s2), c(23383L, 98L, 98L))                   ; nchar(s2) == 3
    identical(utf8ToInt(s3), c(23383L, 97L, 97L, 97L, 97L, 23383L)) ; nchar(s3) == 6
    identical(utf8ToInt(s4), c(23383L, 98L, 98L, 98L, 100L))        ; nchar(s4) == 5
    Encoding(c(s1,s2,s3,s4)) == rep("UTF-8", 4)
})
## did partly overrun to invalid strings, nchar(.) giving error in R <= 4.3.1


## PR#18555 : see ---> ./misc-devel.R


## PR#18557 readChar() with large 'nchars'
ch <- "hello\n"; tf <- tempfile(); writeChar(ch, tf)
tools::assertWarning((c2 <- readChar(tf, 4e8)))
stopifnot(identical(c2, "hello\n"))
## had failed w/   cannot allocate memory block of size 16777216 Tb


## Deprecation of *direct* calls to as.data.frame.<someVector>
dpi <- as.data.frame(pi)
d1 <- data.frame(dtime = as.POSIXlt("2023-07-06 11:11")) # gave F.P. warning
r <- lapply(list(1L, T=T, pi=pi), as.data.frame)
stopifnot(is.list(r), is.data.frame(d1), inherits(d1[,1], "POSIXt"), is.data.frame(r$pi), r$pi == pi)
stopifnot(local({adf <- as.data.frame; identical(adf(1L),(as.data.frame)(1L))}))
## Gave 1 + 3 + 2  F.P. deprecation warnings in 4.3.0 <= R <= 4.3.1
str(d2 <- mapply(as.data.frame, x=1:3, row.names=letters[1:3]))
stopifnot(is.list(d2), identical(unlist(unname(d2)), 1:3))
## gave Error .. sys.call(-1L)[[1L]] .. comparison (!=) is possible only ..
##
## Should not warn for a call from a derivedDefaultMethod to the
## raw S3 method -- implementation detail of S4 dispatch
setGeneric('as.data.frame')
as.data.frame(factor(1))
removeGeneric('as.data.frame')
## wrongly gave  " Direct call of 'as.data.frame.factor()' is deprecated. "


## qqplot(x,y, *) confidence bands for unequal sized x,y, PR#18570:
x <- (7:1)/8; y <- (1:63)/64
r <- qqplot(x,y, plot.it=FALSE, conf.level = 0.90)
r2<- qqplot(y,x, plot.it=FALSE, conf.level = 0.90)
(d <- 64 * as.data.frame(r)[,3:4])
stopifnot(identical(d, data.frame(lwr = c(NA, NA, NA, 6, 15, 24, 33),
                                  upr = c(31, 40, 49, 58, NA, NA, NA))),
          identical(8 * as.data.frame(r2[3:4]),
                    data.frame(lwr = c(NA,NA,NA, 1:4 +0), upr = c(4:7 +0, NA,NA,NA))))
## lower and upper confidence bands were nonsensical in R <= 4.3.1


## New <object> type {{partly experimental}}
mkObj <- function(...) {
    ob <- asS3(getClass("S4")@prototype, complete=FALSE) # "hack"
    if(...length()) attributes(ob) <- list(...)
    ob
}
(oo <- mkObj())
str(oo) # the same: '<object>'
(x4 <- asS4(oo))
dput(x4) # same as print(.)
dput(oo) # <object> again {possibly to be changed}
(o2 <- mkObj(name = "Obi", age = 67))
str(o2) # good!
dput(o2) # <object>  .. to be changed -- once something like mkObj() becomes API
stopifnot(exprs = {
    identical(x4, getClass("S4")@prototype)
    identical(oo, get("oo", mode="object"))
    identical(x4, get("x4", mode="S4"))
    identical(attr(o2, "name"), "Obi")
})
assertErrV(o2[ 1 ])
assertErrV(o2[[1]])

stopifnot(isFALSE(inherits(oo, "S4")))
stopifnot(isTRUE(inherits(oo, "object")))
stopifnot(isTRUE(inherits(x4, "S4")))
stopifnot(isFALSE(inherits(x4, "object")))
assertErrV(get("oo", .GlobalEnv, mode = "S4"))
stopifnot(identical(get("oo", .GlobalEnv, mode = "object"), oo))
stopifnot(identical(get("x4", .GlobalEnv, mode = "S4"), x4))
assertErrV(get("x4", .GlobalEnv, mode = "object"))
assertErrV(get("oo", mode = "integer"))
assertErrV(get("x4", .GlobalEnv, mode = "integer"))


## kappa(), rcond() [& norm()] -- new features and several bug fixes, PR#18543:
(m <- rbind(c(2, 8, 1),
            c(6, 4, 3),
            c(5, 7, 9)))
## 1) kappa(z=<n-by-n>, norm="1", method="direct")` ignores lower triangle of z
km1d <- kappa(m, norm = "1", method = "direct")
all.equal(km1d, 7.6, tol=0) # 1.17e-16  {was wrongly 11.907 in R <= 4.3.1}
## 2) kappa(z, norm="2", LINPACK=TRUE) silently returns estimate of the *1*-norm cond.nr.
(km1 <- kappa(m, norm = "1")) # 4.651847 {unchanged}
tools::assertWarning(verbose=TRUE, # now *warns*
                     km2L <- kappa(m, norm="2", LINPACK=TRUE))
## 3) kappa(z, norm="2", LINPACK=FALSE) throws an error
tools::assertWarning(verbose=TRUE, # *same* warning (1-norm instead of 2-)
                     km2La <- kappa(m, norm="2", LINPACK=FALSE))
km2La
## 4) kappa.qr(z) implicitly assumes nrow(z$qr) >= ncol(z$qr), not true in general
(kqrm2 <- kappa(qr(cbind(m, m + 1))))
## Error in .kappa_tri(R, ...) : triangular matrix should be square
## 5) rcond(x=<n-by-n>, triangular=TRUE) silently ignores the lower (rather than upper)
##                                      triangle of `x`, contradicting `help("rcond")`.
## ==> Fixing help page; but *also* adding  uplo = "U"  argument
all.equal(4/65, (rcTm <- rcond(m, triangular=TRUE)),         tol = 0) # {always}
all.equal(9/182,(rcTL <- rcond(m, triangular=TRUE, uplo="L")), tol=0) # 1.4e-16
##
## New features, can use norm "M" or "F" for exact=TRUE via  norm(*, type=<norm>)
(kM <- kappa(m, norm="M", exact = TRUE)) # 2.25     "M" is allowed type for norm()
(kF <- kappa(m, norm="F", exact = TRUE)) # 6.261675 "F" is allowed type for norm()
all.equal(6.261675485, kF, tol=0) # 2.81e-11
stopifnot(exprs = {
    all.equal(4.6518474224, km1)
    km1 == kappa(m) # same computation
    km1 == kappa(qr.R(qr(m))) # "
    all.equal(km1d, 7.6, tol = 1e-15)
    km1d == kappa(m, method = "direct") # identical computation {always ok}
    identical(km2L, km1)
    all.equal(km2La, 5.228678219)
    all.equal(kqrm2, km1) # even identical
    rcTm == rcond(m, triangular=TRUE, uplo = "U") # uplo="U" was default always
    all.equal(4/65,  rcTm, tol = 1e-14)
    all.equal(9/182, rcTL, tol = 1e-13)
    1/rcond(m) == km1d # same underlying Lapack code
    ## 6) kappa(z=<m-by-0>) throws bad errors due to 1:0 in kappa.qr():
    kappa(m00 <- matrix(0, 0L, 0L)) == 0
    kappa(m20 <- matrix(0, 2L, 0L)) == 0
    ## 7) kappa(z=<0-by-0>, norm="1", method="direct", LINPACK=)
    ##                     is an error for LINPACK=FALSE & returns Inf for L..=TRUE)
    kappa(m00, norm = "1", method = "direct", LINPACK= TRUE) == 0
    kappa(m00, norm = "1", method = "direct", LINPACK=FALSE) == 0
    ## Fixed more problems (by MM):
    rcond(  m00 ) == Inf # gave error
    rcond(  m20 ) == Inf # gave error from infinite recursion
    rcond(t(m20)) == Inf #  (ditto)
    ## norm "M" or "F" for exact=TRUE:
    2.25 == kM  # exactly
    all.equal(6.261675485, kF, tol=1e-9)
})
## -- Complex matrices --------------------------------------------------
(zm <- m + 1i*c(1,-(1:2))*(m/4))
(kz1d <- kappa(zm, norm = "1", method = "direct"))
(kz1  <- kappa(zm, norm = "1"))# meth = "qr"
tools::assertWarning(verbose=TRUE, # now *warns* {gave *error* previously}
                     kz2L <- kappa(zm, norm="2", LINPACK=TRUE))
tools::assertWarning(verbose=TRUE, # *same* warning (1-norm instead of 2-)
                     kz2La <- kappa(zm, norm="2", LINPACK=FALSE))
kz2La
## 4) kappa.qr(z) implicitly assumes nrow(z$qr) >= ncol(z$qr) ..
(kzqr2 <- kappa(qr(cbind(zm, zm + 1)))) # gave Error .. matrix should be square
all.equal(0.058131632, (rcTm <- rcond(zm, triangular=TRUE          )), tol=0) # 3.178e-9
all.equal(0.047891278, (rcTL <- rcond(zm, triangular=TRUE, uplo="L")), tol=0) # 4.191e-9
## New: can use norm "M" or "F" for exact=TRUE:
(kz <- kappa(zm, norm="M", exact = TRUE)) # 2.440468
(kF <- kappa(zm, norm="F", exact = TRUE)) # 6.448678
stopifnot(exprs = {
    all.equal(7.8370264, kz1d) # was wrong {wrongly using .kappa_tri()}
    all.equal(6.6194289, kz1)  # {always ok}
    all.equal(0.058131632, rcTm) #  "
    all.equal(0.047891278, rcTL)
    all.equal(6.82135883, kzqr2)
    all.equal(2.44046765, kz, tol = 1e-9) # 1.8844e-10
    all.equal(6.44867822, kF, tol = 4e-9) # 4.4193e-10
})
## norm() and  kappa(., exact=TRUE, ..)  now work ok in many more cases


## argument matching for round/signif (not handled properly in R <= 4.3.x)
round("days", x = Sys.time())
round(, x = 1)
signif(, x = 1)
(function(...) round(..., 1, ))()
(function(...) signif(..., 1, ))()
tools::assertError(round(digits = 1, x =))
tools::assertError(signif(digits = 1, x =))


## transform() should not check.names -- PR#17890
df <- data.frame(`A-1` = 11:12, B = 21:22, check.names = FALSE)
stopifnot(identical(transform(df), df))  # no-op
stopifnot(exprs = {
    identical(names(transform(df, `A-1` = `A-1` + 1)), names(df))
    identical(names(transform(df, C = 3)), c(names(df), "C"))
    identical(transform(as.matrix(df), B = B), df)
})
## in all three cases, "A-1" inadvertently became "A.1" in R < 4.4.0


## byte compiled sqrt() was not warning about creating NaNs for
## negative integer scalars
tools::assertWarning(compiler::cmpfun(function(x) sqrt(x))(-1L))


## is.atomic(NULL) is no longer true
if(is.atomic(NULL)) stop("Should no longer happen: 'NULL' is not atomic")
## untested previously
stopifnot(is.null(sort(NULL)), is.null(sort.int(NULL)))
## failed in first version of `R-is` branch


## isoreg() seg.faulted with Inf - PR#18603 - in R <= 4.3.1
assertErrV(isoreg(Inf))
assertErrV(isoreg(c(0,Inf)))
assertErrV(isoreg(rep(1e307, 20))) # no Inf in 'y'
## ==> Asserted error: non-finite sum(y) == inf is not allowed


## format() and print() of complex numbers, PR#16752
100+ 0:4 + 10000i  # no 'e'
100+ 0:4 + 100000i # using 'e' as it is shorter
z <- 100+ 0:4 + 1e9i
## for a long time printed identical 5 time  0e+00+1e+09i
(asCz <- as.character(z))
oZ <- capture.output(z)
stopifnot(exprs = {
        substr(asCz, 1,6) == paste0(100+ 0:4, "+1e")
    as.complex(asCz) == z # has been fulfilled for a long time
    identical(oZ, paste("[1]", paste(asCz, collapse=" ")))
})
## had exponential/scientific format for Re() as well, from R 3.3.0 to R 4.3.z


## PR#18579 (thanks to Mikael Jagan) -- cbind/rbind deparse.level for *methods*
.S3method("cbind", "zzz",
          function(..., deparse.level = 1)
              if(!missing(deparse.level)) deparse.level)
x <- structure(0, class = "zzz")
stopifnot(exprs = {
    is.null(cbind(x)) # deparse.level  *missing* {always ok}
    identical(0,  cbind(x, deparse.level = 0))
    identical(1,  cbind(x, deparse.level = 1))
    identical(2,  cbind(x, deparse.level = 2))
    identical(2L, cbind(x, deparse.level = 2L))
})
## passing to S3/S4 method did not work in R <= 4.3.x


## Conversion of LaTeX accents: \~{n} etc vs. \~{}, accented I and i
stopifnot(identical(
    print(tools::parseLatex("El\\~{}Ni\\~{n}o") |>
          tools::latexToUtf8() |>
          tools::deparseLatex(dropBraces = TRUE)),
    "El~Ni\u00F1o")) # "El~Niño"
## gave "El~Ni~no" in R 4.3.{0,1} (\~ treated as 0-arg macro)
stopifnot(tools:::cleanupLatex(r"(\`{I}\'{I}\^{I}\"{I})")
          == "\u00cc\u00cd\u00ce\u00cf")
## was wrongly converted as "ËÌÍÏ" in R <= 4.3.1
stopifnot(tools:::cleanupLatex(r"(\`{i}\'{i}\^{i}\"{i})")
          == "\u00ec\u00ed\u00ee\u00ef")
## codes with i instead of \i were unknown thus not converted in R <= 4.3.1


## tools::deparseLatex() can drop successive LaTeX groups
bib1 <- bibentry("misc", key = "test", year = "2023",
                 author = r"(averig{\"u}{\'e})")
bib2 <- bib1; bib2$author$family <- r"(averig\"{u}\'{e})"
roundtrip <- function (tex, drop = FALSE)
    tex == tools::deparseLatex(tools::parseLatex(tex), dropBraces = drop)
stopifnot(exprs = {
    citeNatbib("test", bib1) == "(averig\u00fc\u00e9 2023)"
    citeNatbib("test", bib2) == citeNatbib("test", bib1)
    roundtrip(r"(\{R\})", TRUE) # escaped braces are not dropped
    roundtrip(r"(\href{https://bugs.R-project.org/}{Bugs})", TRUE)
})
## the first produced "(averigü{é} 2023)" in R < 4.4.0
stopifnot(roundtrip(r"(\item text)"))
## space was lost in R < 4.4.0


## PR#18618: match()  incorrect  with POSIXct || POSIXlt || fractional sec
(dCT <- seq(as.POSIXct("2010-10-31", tz = "Europe/Berlin"), by = "hour", length = 5))
(dd <- diff(dCT))
chd <- as.character(dCT)
vdt <- as.vector   (dCT)
dLT <- as.POSIXlt  (dCT)
dat <- as.Date     (dCT)
dL2 <- dLT[c(1:5,5)]; dL2[6]$sec <- 1/4
dL. <- dL2          ; dL.[6]$sec <- 1e-9
stopifnot(exprs = {
    inherits(dCT, "POSIXct")
    inherits(dLT, "POSIXlt")
    !duplicated(dCT)
    dd == 1
    units(dd) == "hours"
    diff(as.integer(dCT)) == 3600L # seconds
    identical(match(chd, chd), c(1:3, 3L, 5L))
    identical(match(vdt, vdt), seq_along(vdt))
    identical(match(dat, dat), c(1L,1L, 3L,3L,3L)) # always ok
    identical(match(dCT, dCT), seq_along(dCT)) # wrong in 4.3.{0,1,2}
    identical(match(dLT, dLT), seq_along(dLT)) #  "    "   "
    identical(match(dL2, dL2), seq_along(dL2)) #  "    "   "
    identical(match(dL., dL.), seq_along(dL.)) #  "    "  now ok, as indeed,
  ! identical(dL.[5], dL.[6]) # NB: `==`, diff(), ... all lose precision, from as.POSIXct():
    inherits(dC. <- as.POSIXct(dL.), "POSIXct")
    identical(match(dC., dC.), c(1:5, 5L))
    identical(dC.[5], dC.[6])
    dC.[5] == dC.[6]
} )
## failed (partly) in R versions  4.3.0 -- 4.3.2


## PR#18598: *wrong* error message
writeLines(eMsg <- tryCmsg(
    diff(1:6, differences = integer(0L))
))
if(englishMsgs) stopifnot(grepl("must be integers >= 1", eMsg))
## errored with "missing value where TRUE/FALSE needed" in R <= 4.3.2


## PR#18563: drop.terms(*, dropx = <0-length vector>)
tt <- terms(y ~ a+b)
stopifnot(identical(tt, drop.terms(tt, dropx = 0[0], keep.response=TRUE)))
## errored in R <= 4.3.2


## as.complex("<num>i") -- should work (and fail/warn) as the parser does:
tools::assertWarning(cc <- as.complex("12iL"), verbose=TRUE)
tools::assertWarning(cF <- as.complex("12irene"))
tools::assertWarning(cI <- as.complex("12I"))
stopifnot(is.na(cc), is.na(cF), is.na(cI),
          identical(cc, cF), identical(cF, cI), identical(cI, NA_complex_))
stopifnot(exprs = {
    identical(1i,        as.complex("1i"))
    identical(4i,        as.complex("+4.i"))
    identical(-0.1i,     as.complex("-.1i"))
    identical(-4.321i,   as.complex("-4.321i"))
    identical(-4.3e-17i, as.complex("-4.3e-17i"))
    identical(+.123e6i,  as.complex("+.123e6i"))
    identical(-4.3e6i,   as.complex("-4.3e6i"))
    identical( 30000i,   as.complex("+.3e+5i"))
})
## returned NA_complex_ *with* a warning, in R <= 4.4.0


## c(NA, <cplx>); cumsum(<cplx_w_NA>) -- related to as.complex(NA_real_); R-devel ML
cx <- function(r,i) complex(real=r, imaginary=i)
pz <- function(z) noquote(paste0("(", Re(z), ",", Im(z), ")"))
pz(z <- c(1i, NA))
NA.1 <- cx(NA, 1)
stopifnot(exprs = {
    Im(z) == 1:0 # was  (TRUE  NA)
    identical(z, cx(c(0,NA), c(1,0))) # new
    identical(sum(z), NA.1)           #  "
    ## these were all TRUE already :
    identical(prod(z), NA_complex_)
    identical(Im(cumsum(z)), cumsum(Im(z)))
    identical(Re(cumsum(z)), cumsum(Re(z)))
    identical( sum(z), tail( cumsum(z), 1L))
    identical(prod(z), tail(cumprod(z), 1L))
})
## gave NA_complex_ in more cases in R <= 4.4.0


## PR#18627 - getS3method() should match method dispatch {and isS3method()}
stopifnot(exprs = {
            ! isS3method("t", "test")
    is.null( getS3method("t", "test", optional=TRUE) )
    identical(dim(t(structure(matrix(, 3, 2), class = "test"))), # t.test() is *not* called:
              2:3)
})
## getS3method(..) did return the t.test function, in R <= 4.3.2


## PR#18564,5,6:  drop.terms(*)
tt <- terms(y ~ a+b)
stopifnot(formula(drop.terms(tt))                     == {   ~ a+b }) # was  y ~ a + b
stopifnot(formula(drop.terms(tt, keep.response=TRUE)) == { y ~ a+b }) # (unchanged)
## did not drop y (with default keep.response=FALSE) in R <= 4.3.2
##
## PR#18565: offset() in formula
tto <- terms(y ~ a + b + offset(h))
(ttF <- drop.terms(tto, 1L, keep.response = FALSE))
(ttT <- drop.terms(tto, 1L, keep.response = TRUE ))
(tt.2 <- tto[2L])
stopifnot(exprs = {
    formula(ttF) ==     ~ b + offset(h)
    formula(ttT) == { y ~ b + offset(h) }
    formula(tt.2)== { y ~ b + offset(h) }
    identical(attr(ttF, "offset"), 2L)
    identical(attr(ttT, "offset"), 3L)
    identical(attr(tt.2,"offset"), 3L)
}) ## all dropped 'offset' in R <= 4.3.2
##
## PR#18566:
remattr <- function(x) { attributes(x) <- NULL; x } ## do we already have this?
t2 <- terms(~ a + b)
str(dt2 <- drop.terms(t2, 1, keep.response = TRUE))
stopifnot( drop.terms(t2, 1) == dt2, remattr(dt2) == quote(~ b))
## gave a+b ~ b in R <= 4.3.2


## cov2cor(<0x0>) PR#18423
m00 <- matrix(0,0,0)
stopifnot(identical(cov2cor(m00), m00))
## gave error in R <= 4.3.2


## cov2cor(.) warning(s) with negative/NA diag(.) - PR#18424
(D_1 <- diag(-1, 3L))
op <- options(warn=1)
m <- capture.output(r <- cov2cor(D_1), type = "message")
matrix(rep_len(c(1, rep(NaN,3)),3*3), 3) -> r0
stopifnot(all.equal(r, r0, tol = 0, check.attributes = FALSE),# always ok
          length(m) == 2, grepl("^ *diag.V. ", m[2]))
options(op) # revert
## cov2cor() gave 2 warnings on 3 lines, the 2nd one inaccurate in R <= 4.3.2


## `formals<-` failing for _explicit_ constant body and empty formals
fbList <- c(Sys.info, body, lm
            ## constant body:
            , function() "foo"
            , function()r"(')"
            , function()   1L
            , function() TRUE
            , function()   1i
            , function() 3.14
            , function()  Inf
            )
for(f in fbList) {
    g <- f ; formals(g) <- formals(g)
    h <- f ;    body(h) <- body(h)
    stopifnot(identical(g, f),
              identical(h, f))
}
## those w/ constant body failed in `formals<-`  in R <= 4.3.x
## with Error in as.function.default(....): list argument expected



## fix error message of as.function(..1, *)
(msg <- tryCmsg(as.function(list({}, 1), .GlobalEnv)))
stopifnot(!englishMsgs || grepl("invalid formal argument list", msg),
          grepl('"as.function"', msg, fixed=TRUE))
## had "function" wrongly in R <= 4.3.x


## removeSource() checking *formals* incl in sub-functions -- PR#18638
f <- function(x = {}) {
    function(y = {}) { NULL }
}
str(lapply(formals(f), attributes)) # list(x = list(srcref = .., srcfile = .. wholeSrcref = ..))
f0 <- removeSource(f) # was unchanged in R <= 4.3.2
## in sub function {not atttrib}:
(toplev <- !sys.nframe())
op <- options(keep.source = TRUE)
qf <- quote(function() NULL)
str(qf4 <- qf[[4]]) # srcref, now removed:
qf0 <- removeSource(qf)
stopifnot(exprs = {
    ## no "srcref" anymore for the formals of f0 or its result:
    identical(lapply(formals(f0),   attributes), list(x = NULL))
    identical(lapply(formals(f0()), attributes), list(y = NULL))
    ##
    length(qf) == 4L
    length(qf0)== 4L
    is.integer(qf4)
    length(qf4) >= 8
    if(toplev) # e.g., when source()d
        qf4 == c(1L, 13L, 1L, 27L, 13L, 27L, 1L, 1L) # in qf[] but not in qf0[]
    else
        qf4 >= 1L
    is.null(qf0[[4L]])
})
options(op)
## f0 and qf0 were unchanged, keeping srcref in R <= 4.3.*


## startDynamicHelp(): port out of range, PR#18645
op <- options(help.ports = 123456L)
assertErrV(tools::startDynamicHelp())
assertErrV(help.start(browser = identity))
options(op)
## silently failed much later in R <= 4.3.2.


## checks for x == y when operands are call objects, PR18676
## disabled if == for calls would signal an error
if (is.na(Sys.getenv("_R_COMPARE_LANG_OBJECTS", unset = NA_character_))) {
    stopifnot(quote({a}) != quote({b}))
    stopifnot(quote(c(1)) != quote(c(1L)))
    stopifnot(quote(c(1.234567890123456)) != quote(c(1.2345678901234567)))
}

## <POSIXlt>[] -- PR#18681
(x <- as.POSIXlt(.POSIXct(0, tz = "UTC"))) # "1970-01-01 UTC"
x$mon <- 12L
stopifnot(exprs = {
    identical(12L, x[,"mon"]) # had "balanced" attr.!
    identical(x, (x1 <- (x[1L])))
    identical(12L, x1$mon)				# (never bug)
    identical("1971-01-01 UTC", format(x, usetz=TRUE))	#  "
    identical(71L, balancePOSIXlt(x)$year)		#  "
    is.na(attr(x1, "balanced")) # was 'TRUE'
})
## subsetting set "balanced" incorrectly sometimes in R 4.3.*


## str(<classed-language>)
x_u <- structure(quote(a > 2 * b), class = 'new_class')
writeLines(sto <- capture.output(str(x_u)))
stopifnot(grepl("a > 2 * b", sto[[1]], fixed=TRUE), # had ' > a 2 * b'
          grepl('attr\\(\\*,.*"new_class"', sto[[2]]))
## previously used as.character() as "last resort" in R <= 4.3.*


## Rd2ex() with code directly following a \dont...{} tag
rd <- tools::parse_Rd(textConnection(c(
    "\\name{test}\\title{test}\\examples{",
    "\\dontshow{if(TRUE)} stop('catch me')",
    "print(0)}"
)))
tools::Rd2ex(rd, tf <- tempfile())
tools::assertError(source(tf), verbose = TRUE)
## skipped the stop() and printed 0 in R < 4.4.0


## as.data.frame(<empty matrix) ; Davis Vaughan R-devel, 2024-03-21
for(nr in 0:2) {
    dput(d0 <- as.data.frame(matrix(nrow = nr, ncol = 0)))
    stopifnot("names" %in% names(attributes(d0)),
              identical(character(), names(d0)))
}
## had no .$names at all in R < 4.4.0


## C level R_nonInt() less tolerant, used more often
(gd <- getVaW(dbinom(1234560:1234570, 9876543.2, .5)))
gp  <- getVaW(pbinom(1234560:1234570, 9876543.2, 1/8))
(gdp <- getVaW(dpois(9876543 + (2:8)/10, 1e7)))
stopifnot(exprs = {
    identical(gd, structure(rep(NaN, 11), warning = "NaNs produced"))
    identical(gd, gp)
    identical(gdp, structure(rep(0,7), # only *last* warning:
                             warning = "non-integer x = 9876543.800000"))
})
## did not warn; just treat 98... as an integer in R < 4.4.0


## Finally deprecate terms.formula()'s  'abb' and 'neg.out' args:
tt <- terms(y ~ a+b)
t0 <- getVaW(terms(y ~ a+b, abb = 1))
t1 <- getVaW(terms(y ~ a+b, neg.out = 0))
t2 <- getVaW(terms(y ~ a+b, abb=NA, neg.out=NA))
stopifnot(exprs = {
    identical(t0, structure(tt, warning = "setting 'abb' in terms.formula() is deprecated"))
    identical(t1, structure(tt, warning = "setting 'neg.out' in terms.formula() is deprecated"))
    identical(t2, t1)
})
## deprecation was only on help page  for R 4.3.*


## error jump happened after mutation through R 4.3.3
x <- expression(a)
tryCatch(x[[2]] <- list(), error = invisible)
stopifnot(identical(x, expression(a)))


## table |> as.data.frame() |> xtabs() round-trip with missing counts
tab <- replace(UCBAdmissions[,,1], 1, NA)
stopifnot(identical(c(xtabs(Freq ~ ., as.data.frame(tab))), c(tab)))
## NA turned into 0 in R < 4.4.0



## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
