## Regression tests for R >= 4.3.0

.pt <- proc.time()
tryCid <- function(expr) tryCatch(expr, error = identity)
tryCmsg<- function(expr) tryCatch(expr, error = conditionMessage) # typically == *$message
assertErrV <- function(...) tools::assertError(..., verbose=TRUE)

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



## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
