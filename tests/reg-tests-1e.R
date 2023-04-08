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
Sys.setenv("_R_CHECK_AS_DATA_FRAME_EXPLICIT_METHOD_" = "true")# just in case


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
obs  <- lapply(cls, \(cl) asF[[cl]](switch(cl, "difftime" = "2:1:0", "noquote" = letters, 1:2)))
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
Edl <- tryCid(download.file("http://httpbin.org/get", "ping.txt"))
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




## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
