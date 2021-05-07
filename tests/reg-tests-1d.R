## Regression tests for R >= 3.4.0

pdf("reg-tests-1d.pdf", encoding = "ISOLatin1.enc")
.pt <- proc.time()
tryCid <- function(expr) tryCatch(expr, error = identity)
tryCmsg<- function(expr) tryCatch(expr, error = conditionMessage) # typically == *$message
identCO <- function(x,y, ...) identical(capture.output(x), capture.output(y), ...)
assertErrV <- function(...) tools::assertError(..., verbose=TRUE)
onWindows <- .Platform$OS.type == "windows"
.M <- .Machine
str(.M[grep("^sizeof", names(.M))]) ## also differentiate long-double..
b64 <- .M$sizeof.pointer == 8


## body() / formals() notably the replacement versions
x <- NULL; tools::assertWarning(   body(x) <-    body(mean))	# to be error
x <- NULL; tools::assertWarning(formals(x) <- formals(mean))	# to be error
x <- NULL; tools::assertWarning(f <-    body(x)); stopifnot(is.null(f))
x <- NULL; tools::assertWarning(f <- formals(x)); stopifnot(is.null(f))
## these all silently coerced NULL to a function in R <= 3.2.x

## A good guess if we have _not_ translated error/warning/.. messages:
## (should something like this be part of package tools ?)
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


## match(x, t): fast algorithm for length-1 'x' -- PR#16885
## a) string 'x'  when only encoding differs
tmp <- "年付"
tmp2 <- "\u5e74\u4ed8" ; Encoding(tmp2) <- "UTF-8"
for(ex in list(c(tmp, tmp2), c("foo","foo"))) {
    cat(sprintf("\n|%s|%s| :\n----------\n", ex[1], ex[2]))
    for(enc in c("latin1", "UTF-8", "unknown")) { # , "MAC", "WINDOWS-1251"
	cat(sprintf("%9s: ", enc))
	tt <- ex[1]; Encoding(tt) <- enc; t2 <- ex[2]
	if(identical(i1 <- (  tt       %in% t2),
		     i2 <- (c(tt, "a") %in% t2)[1]))
	    cat(i1,"\n")
	else
	    stop("differing: ", i1, ", ", i2)
    }
}
##
outerID <- function(x,y, ...) outer(x,y, Vectorize(identical,c("x","y")), ...)
## b) complex 'x' with different kinds of NaN
x0 <- c(0,1, NA_real_, NaN)
z <- outer(x0,x0, complex, length.out=1L)
z <- c(z[is.na(z)], # <- of length 4 * 4 - 2*2 = 12
       as.complex(NaN), as.complex(0/0), # <- typically these two differ in bits
       complex(real = NaN), complex(imaginary = NaN),
       NA_complex_, complex(real = NA), complex(imaginary = NA))
## 1..12 all differ, then
symnum(outerID(z,z, FALSE,FALSE,FALSE,FALSE))# [14] differing from all on low level
symnum(outerID(z,z))                         # [14] matches 2, 13,15
(mz <- match(z, z)) # (checked with m1z below)
zRI <- rbind(Re=Re(z), Im=Im(z)) # and see the pattern :
print(cbind(format = format(z), t(zRI), mz), quote=FALSE)
stopifnot(apply(zRI, 2, anyNA)) # NA *or* NaN: all TRUE
is.NA <- function(.) is.na(.) & !is.nan(.)
(iNaN <- apply(zRI, 2, function(.) any(is.nan(.))))
(iNA <-  apply(zRI, 2, function(.) any(is.NA (.)))) # has non-NaN NA's
## use iNA for consistency check once FIXME happened
m1z <- sapply(z, match, table = z)
stopifnot(exprs = {
    identical(m1z, mz)
    identical(m1z == 1L,             iNA)
    identical(match(z, NA, 0) == 1L, iNA)
    identical(mz[mz != 1L], c(2L, 4L, 9L, 10L, 12L, 2L, 2L, 2L, 9L))
})
## m1z uses match(x, *) with length(x) == 1 and failed in R 3.3.0
set.seed(17)
for(. in 1:20) {
    zz <- sample(z)
    stopifnot(identical(match(zz,zz), vapply(zz, match, -1L, table = zz)))
}
##
## PR#16909 - a consequence of the match() bug; check here too:
dvn <- paste0("var\xe9", 1:2); Encoding(dvn) <- "latin1"
dv <- data.frame(1:3, 3); names(dv) <- dvn; dv[,"var\u00e92"] <- 2
stopifnot(ncol(dv) == 2, dv[,2] == 2, identical(names(dv), dvn))
## in R 3.3.0, got a 3rd column


## deparse(<complex>,  "digits17")
fz <- format(z <- c(outer(-1:2, 1i*(-1:1), `+`)))
(fz0 <- sub("^ +","",z))
r <- c(-1:1,100, 1e20); z2 <- c(outer(pi*r, 1i*r, `+`)); z2
dz2 <- deparse(z2, control="digits17")
stopifnot(exprs = {
    identical(deparse(z, 200, control = "digits17"),
              paste0("c(", paste(fz0, collapse=", "), ")"))
    print((sum(nchar(dz2)) - 2) / length(z2)) < 22 # much larger in <= 3.3.0
    ## deparse <-> parse equivalence, 17 digits should be perfect:
    all.equal(z2, eval(parse(text = dz2)), tolerance = 3e-16) # seen 2.2e-35 on 32b
})
## deparse() for these was "ugly" in R <= 3.3.x

## deparse of formals of a function
fun <- function(a=1,b){}
frmls <- tryCid(eval(parse(text=deparse(formals(fun)))))
stopifnot(identical(frmls, formals(fun)))


## length(environment(.)) == #{objects}
stopifnot(identical(length(      baseenv()),
                    length(names(baseenv()))))
## was 0 in R <= 3.3.0


## "srcref"s of closures
op <- options(keep.source = TRUE)# as in interactive use
getOption("keep.source")
stopifnot(exprs = {
    identical(function(){}, function(){})
    identical(function(x){x+1},
              function(x){x+1})
}); options(op)
## where all FALSE in 2.14.0 <= R <= 3.3.x because of "srcref"s etc


## PR#16925, radix sorting INT_MAX w/ decreasing=TRUE and na.last=TRUE
## failed ASAN check and segfaulted on some systems.
data <- c(2147483645L, 2147483646L, 2147483647L, 2147483644L)
stopifnot(identical(sort(data, decreasing = TRUE, method = "radix"),
                    c(2147483647L, 2147483646L, 2147483645L, 2147483644L)))


## as.factor(<named integer>)
ni <- 1:2; Nni <- names(ni) <- c("A","B")
stopifnot(exprs = {
    identical(Nni, names(as.factor(ni)))
    identical(Nni, names(   factor(ni)))
    identical(Nni, names(   factor(ni+0))) # +0 : "double"
    identical(Nni, names(as.factor(ni+0)))
})
## The first one lost names in  3.1.0 <= R <= 3.3.0


## strtrim(<empty>, *) should work as substr(<empty>, *) does
c0 <- character(0)
stopifnot(identical(c0, strtrim(c0, integer(0))))
## failed in R <= 3.3.0


## Factors with duplicated levels {created via low-level code}:
set.seed(11)
f0 <- factor(sample.int(9, 20, replace=TRUE))
(f <- structure(f0, "levels" = as.character(c(2:7, 2:4))))
tools::assertWarning(print(f))
tools::assertError(validObject(f))
## no warning in print() for R <= 3.3.x


## R <= 3.3.0 returned integer(0L) from unlist() in this case:
stopifnot(identical(levels(unlist(list(factor(levels="a")))), "a"))


## diff(<difftime>)
d <- as.POSIXct("2016-06-08 14:21", tz="UTC") + as.difftime(2^(-2:8), units="mins")
dd  <- diff(d)
ddd <- diff(dd)
d3d <- diff(ddd)
d7d <- diff(d, differences = 7)
(ldd <- list(dd=dd, ddd=ddd, d3d=d3d, d7d=d7d))
stopifnot(exprs = {
    identical(ddd, diff(d, differences = 2))
    identical(d3d, diff(d, differences = 3))
    vapply(ldd, units, "") == "secs"
    vapply(ldd, class, "") == "difftime"
    lengths(c(list(d), ldd)) == c(11:8, 11-7)
})
## was losing time units in R <= 3.3.0


## sample(NA_real_) etc
for(xx in list(NA, NA_integer_, NA_real_, NA_character_, NA_complex_, "NA", 1i))
    stopifnot(identical(xx, sample(xx)))
## error in R <= 3.3.1


## merge.data.frame with names matching order()'s arguments (PR#17119)
nf <- names(formals(order))
nf <- nf[nf != "..."]
v1 <- c(1,3,2)
v2 <- c(4,2,3)
for(nm in nf)  {
    cat(nm,":\n")
    mdf <- merge(
        as.data.frame(setNames(list(v1), nm=nm)),
        as.data.frame(setNames(list(v2), nm=nm)), all = TRUE)
    stopifnot(identical(mdf,
                        as.data.frame(setNames(list(0+ 1:4), nm=nm))))
}
## some were wrong, others gave an error in R <= 3.3.1


## PR#16936: table() dropping "NaN" level & 'exclude' sometimes failing
op <- options(warn = 2)# no warnings allowed
(fN1 <- factor(c("NA", NA, "NbN", "NaN")))
(tN1 <- table(fN1)) ##--> was missing 'NaN'
(fN <- factor(c(rep(c("A","B"), 2), NA), exclude = NULL))
(tN  <- table(fN, exclude = "B"))       ## had extraneous "B"
(tN. <- table(fN, exclude = c("B",NA))) ## had extraneous "B" and NA
stopifnot(exprs = {
    identical(c(tN1), c(`NA`=1L, `NaN`=1L, NbN=1L))
    identical(c(tN),  structure(2:1, .Names = c("A", NA)))
    identical(c(tN.), structure(2L,  .Names = "A"))
})
## both failed in R <= 3.3.1
stopifnot(identical(names(dimnames(table(data.frame(Titanic[2,2,,])))),
		    c("Age", "Survived", "Freq"))) # was wrong for ~ 32 hours
##
## Part II:
x <- factor(c(1, 2, NA, NA), exclude = NULL) ; is.na(x)[2] <- TRUE
x # << two "different" NA's (in codes | w/ level) looking the same in print()
stopifnot(identical(x, structure(as.integer(c(1, NA, 3, 3)),
				 .Label = c("1", "2", NA), class = "factor")))
(txx <- table(x, exclude = NULL))
stopifnot(identical(txx, table(x, useNA = "ifany")),
	  identical(as.vector(txx), c(1:0, 3L)))
## wrongly gave  1 0 2  for R versions  2.8.0 <= Rver <= 3.3.1
u.opt <- list(no="no", ifa = "ifany", alw = "always")
l0 <- c(list(`_` = table(x)),
           lapply(u.opt, function(use) table(x, useNA=use)))
xcl <- list(NULL=NULL, none=""[0], "NA"=NA, NANaN = c(NA,NaN))
options(op) # warnings ok:
lt <- lapply(xcl, function(X)
    c(list(`_` = table(x, exclude=X)), #--> 4 warnings from (exclude, useNA):
      lapply(u.opt, function(use) table(x, exclude=X, useNA=use))))
(y <- factor(c(4,5,6:5)))
ly <-  lapply(xcl, function(X)
    c(list(`_` = table(y, exclude=X)), #--> 4 warnings ...
      lapply(u.opt, function(use) table(y, exclude=X, useNA=use))))
lxy <-  lapply(xcl, function(X)
    c(list(`_` = table(x, y, exclude=X)), #--> 4 warnings ...
      lapply(u.opt, function(use) table(x, y, exclude=X, useNA=use))))
op <- options(warn = 2)# no warnings allowed

stopifnot(exprs = {
    vapply(lt, function(i) all(vapply(i, class, "") == "table"), NA)
    vapply(ly, function(i) all(vapply(i, class, "") == "table"), NA)
    vapply(lxy,function(i) all(vapply(i, class, "") == "table"), NA)
    identical((ltNA  <- lt [["NA"  ]]), lt [["NANaN"]])
    identical((ltNl  <- lt [["NULL"]]), lt [["none" ]])
    identical((lyNA  <- ly [["NA"  ]]), ly [["NANaN"]])
    identical((lyNl  <- ly [["NULL"]]), ly [["none" ]])
    identical((lxyNA <- lxy[["NA"  ]]), lxy[["NANaN"]])
    identical((lxyNl <- lxy[["NULL"]]), lxy[["none" ]])
})
## 'NULL' behaved special (2.8.0 <= R <= 3.3.1)  and
##  *all* tables in l0 and lt were == (1 0 2) !
ltN1 <- ltNA[[1]]; lyN1 <- lyNA[[1]]; lxyN1 <- lxyNA[[1]]
lNl1 <- ltNl[[1]]; lyl1 <- lyNl[[1]]; lxyl1 <- lxyNl[[1]]

stopifnot(exprs = {
    vapply(names(ltNA) [-1], function(n) identical(ltNA [[n]], ltN1 ), NA)
    vapply(names(lyNA) [-1], function(n) identical(lyNA [[n]], lyN1 ), NA)
    vapply(names(lxyNA)[-1], function(n) identical(lxyNA[[n]], lxyN1), NA)
    identical(lyN1, lyl1)
    identical(2L, dim(ltN1)); identical(3L, dim(lyN1))
    identical(3L, dim(lNl1))
    identical(dimnames(ltN1), list(x = c("1","2")))
    identical(dimnames(lNl1), list(x = c("1","2", NA)))
    identical(dimnames(lyN1), list(y = paste(4:6)))
    identical(  1:0    , as.vector(ltN1))
    identical(c(1:0,3L), as.vector(lNl1))
    identical(c(1:2,1L), as.vector(lyN1))
    identical(c(1L, rep(0L, 5)), as.vector(lxyN1))
    identical(dimnames(lxyN1), c(dimnames(ltN1), dimnames(lyN1)))
    identical(c(1L,1:0), as.vector(table(3:1, exclude=1, useNA = "always")))
    identical(c(1L,1L ), as.vector(table(3:1, exclude=1)))
})

x3N <- c(1:3,NA)
(tt <- table(x3N, exclude=NaN))
stopifnot(exprs = {
    tt == 1
    length(nt <- names(tt)) == 4
    is.na(nt[4])
    identical(tt, table(x3N, useNA = "ifany"))
    identical(tt, table(x3N, exclude = integer(0)))
    identical(t3N <- table(x3N), table(x3N, useNA="no"))
    identical(c(t3N), setNames(rep(1L, 3), as.character(1:3)))
    ##
    identical(c("2" = 1L), c(table(1:2, exclude=1) -> t12.1))
    identical(t12.1, table(1:2, exclude=1, useNA= "no"))
    identical(t12.1, table(1:2, exclude=1, useNA= "ifany"))
    identical(structure(1:0, .Names = c("2", NA)),
              c(     table(1:2, exclude=1, useNA= "always")))
})
options(op) # (revert to default)


## contour() did not check args sufficiently
tryCmsg(contour(matrix(rnorm(100), 10, 10), levels = 0, labels = numeric()))
## caused segfault in R 3.3.1 and earlier


## unique.warnings() needs better duplicated():
invisible(warnings())
.tmp <- lapply(list(0, 1, 0:1, 1:2, c(1,1), -1:1), function(x) wilcox.test(x))
if(!interactive())
stopifnot(length(print(uw <- unique(warnings()))) == 2)
## unique() gave only one warning in  R <= 3.3.1


options(warn = 2)# no warnings allowed

## findInterval(x, vec)  when 'vec' is of length zero
n0 <- numeric(); TF <- c(TRUE, FALSE)
stopifnot(0 == unlist(lapply(TF, function(L1)
    lapply(TF, function(L2) lapply(TF, function(L3)
        findInterval(x=8:9, vec=n0, L1, L2, L3))))))
## did return -1's for all.inside=TRUE  in R <= 3.3.1


## droplevels(<factor with NA-level>)
L3 <- c("A","B","C")
f <- d <- factor(rep(L3, 2), levels = c(L3, "XX")); is.na(d) <- 3:4
(dn <- addNA(d)) ## levels: A B C XX <NA>
stopifnot(exprs = {
    identical(levels(print(droplevels(dn))), c(L3, NA))
    ## only XX must be dropped; R <= 3.3.1 also dropped <NA>
    identical(levels(droplevels(f)), L3)
    identical(levels(droplevels(d)), L3) # do *not* add <NA> here
    identical(droplevels(d ), d [, drop=TRUE])
    identical(droplevels(f ), f [, drop=TRUE])
    identical(droplevels(dn), dn[, drop=TRUE])
})


## summary.default() no longer rounds (just its print() method does):
set.seed(0)
replicate(256, { x <- rnorm(1); stopifnot(summary(x) == x)}) -> .t
replicate(256, { x <- rnorm(2+rpois(1,pi))
    stopifnot(min(x) <= (sx <- summary(x)), sx <= max(x))}) -> .t
## was almost always wrong in R <= 3.3.x


## NULL in integer arithmetic
i0 <- integer(0)
stopifnot(exprs = {
    identical(1L + NULL, 1L + integer())
    identical(2L * NULL, i0)
    identical(3L - NULL, i0)
})
## gave double() in R <= 3.3.x


## factor(x, exclude)  when  'x' or 'exclude' are  character -------
stopifnot(identical(factor(c(1:2, NA), exclude = ""),
		    factor(c(1:2, NA), exclude = NULL) -> f12N))
fab <- factor(factor(c("a","b","c")), exclude = "c")
stopifnot(identical(levels(fab), c("a","b")))
faN <- factor(c("a", NA), exclude=NULL)
stopifnot(identical(faN, factor(faN, exclude="c")))
## differently with NA coercion warnings in R <= 3.3.x

## factor(x, exclude = X) - coercing 'exclude' or not
## From r-help/2005-April/069053.html :
fNA <- factor(as.integer(c(1,2,3,3,NA)), exclude = NaN)
stopifnot(identical(levels(fNA), c("1", "2", "3", NA)))
## did exclude NA wrongly in R <= 3.3.x
## Now when 'exclude' is a factor,
cc <- c("x", "y", "NA")
ff <- factor(cc)
f2 <- factor(ff, exclude = ff[3]) # it *is* used
stopifnot(identical(levels(f2), cc[1:2]))
## levels(f2) still contained NA in R <= 3.3.x


## arithmetic, logic, and comparison (relop) for 0-extent arrays
(m <- cbind(a=1[0], b=2[0]))
Lm <- m; storage.mode(Lm) <- "logical"
Im <- m; storage.mode(Im) <- "integer"
stopifnot(exprs = {
    identical( m, m + 1 ); identical( m,  m + 1 [0]); identical( m,  m + NULL)
    identical(Im, Im+ 1L); identical(Im, Im + 1L[0]); identical(Im, Im + NULL)
    identical(m, m + 2:3); identical(Im, Im + 2:3)
    identical(Lm, m & 1);  identical(Lm,  m | 2:3)
    identical(Lm,  m & TRUE [0])
    identical(Lm, Lm | FALSE[0])
    identical(Lm, m & NULL) # gave Error (*only* place where NULL was not allowed)
    identical(Lm, m > 1)
    identical(Lm, m > .1[0]); identical(Lm, m > NULL)
    identical(Lm, m <= 2:3)
})
mm <- m[,c(1:2,2:1,2)]
tools::assertError(m + mm) # ... non-conformable arrays
tools::assertError(m | mm) # ... non-conformable arrays
tools::assertError(m == mm)# ... non-conformable arrays
## in R <= 3.3.x, relop returned logical(0) and  m + 2:3  returned numeric(0)

## arithmetic, logic, and comparison (relop) -- inconsistency for 1x1 array o <vector >= 2>:
(m1 <- matrix(1,1,1, dimnames=list("Ro","col")))
(m2 <- matrix(1,2,1, dimnames=list(c("A","B"),"col")))
if(FALSE) { # in the future (~ 2018):
tools::assertError(m1  + 1:2) ## was [1] 2 3  even w/o warning in R <= 3.3.x
} else tools::assertWarning(m1v <- m1 + 1:2); stopifnot(identical(m1v, 1+1:2))
tools::assertError(m1  & 1:2) # ERR: dims [product 1] do not match the length of object [2]
tools::assertError(m1 <= 1:2) # ERR:                  (ditto)
##
## non-0-length arrays combined with {NULL or double() or ...} *fail*
n0 <- numeric(0)
l0 <- logical(0)
stopifnot(exprs = {
    identical(m1 + NULL, n0) # as "always"
    identical(m1 +  n0 , n0) # as "always"
    identical(m1 & NULL, l0) # ERROR in R <= 3.3.x
    identical(m1 &  l0,  l0) # ERROR in R <= 3.3.x
    identical(m1 > NULL, l0) # as "always"
    identical(m1 >  n0 , l0) # as "always"
    ## m2 was slightly different:
    identical(m2 + NULL, n0) # ERROR in R <= 3.3.x
    identical(m2 +  n0 , n0) # ERROR in R <= 3.3.x
    identical(m2 & NULL, l0) # ERROR in R <= 3.3.x
    identical(m2 &  l0 , l0) # ERROR in R <= 3.3.x
    identical(m2 == NULL, l0) # as "always"
    identical(m2 ==  n0 , l0) # as "always"
})

## strcapture()
stopifnot(identical(strcapture("(.+) (.+)",
                               c("One 1", "noSpaceInLine", "Three 3"),
                               proto=data.frame(Name="", Number=0)),
                    data.frame(Name=c("One", NA, "Three"),
                               Number=c(1, NA, 3))))


## PR#17160: min() / max()  arg.list starting with empty character
TFT <- 1:3 %% 2 == 1
stopifnot(exprs = {
    identical(min(character(), TFT), "0")
    identical(max(character(), TFT), "1")
    identical(max(character(), 3:2, 5:7, 3:0), "7")
    identical(min(character(), 3:2, 5:7), "2")
    identical(min(character(), 3.3, -1:2), "-1")
    identical(max(character(), 3.3, 4:0), "4")
})
## all gave NA in R <= 3.3.0


## PR#17147: xtabs(~ exclude) fails in R <= 3.3.1
exc <- exclude <- c(TRUE, FALSE)
xt1 <- xtabs(~ exclude) # failed : The name 'exclude' was special
xt2 <- xtabs(~ exc)
xt3 <- xtabs(rep(1, length(exclude)) ~ exclude)
noCall  <- function(x) structure(x, call = NULL)
stripXT <- function(x) structure(x, call = NULL, dimnames = unname(dimnames(x)))
stopifnot(exprs = {
    identical(dimnames(xt1), list(exclude = c("FALSE", "TRUE")))
    identical(names(dimnames(xt2)), "exc")
    all.equal(stripXT(xt1), stripXT(xt2))
    all.equal(noCall (xt1), noCall (xt3))
})
## [fix was to call table() directly instead of via do.call(.)]


## str(xtabs( ~ <var>)):
stopifnot(grepl("'xtabs' int", capture.output(str(xt2))[1]))
## did not mention "xtabs" in R <= 3.3.1


## findInterval(x_with_ties, vec, left.open=TRUE)
stopifnot(identical(
    findInterval(c(6,1,1), c(0,1,3,5,7), left.open=TRUE), c(4L, 1L, 1L)))
set.seed(4)
invisible(replicate(100, {
 vec <- cumsum(1 + rpois(6, 2))
 x <- rpois(50, 3) + 0.5 * rbinom(50, 1, 1/4)
 i <- findInterval(x, vec, left.open = TRUE)
 .v. <- c(-Inf, vec, Inf)
 isIn <-  .v.[i+1] < x  &  x <= .v.[i+2]
 if(! all(isIn)) {
     dump(c("x", "vec"), file=stdout())
     stop("not ok at ", paste(which(!isIn), collapse=", "))
 }
}))
## failed in R <= 3.3.1


## PR#17132 -- grepRaw(*, fixed = TRUE)
stopifnot(
    identical(1L,        grepRaw("abcd",     "abcd",           fixed = TRUE)),
    identical(integer(), grepRaw("abcdefghi", "a", all = TRUE, fixed = TRUE)))
## length 0 and seg.faulted in R <= 3.3.2


## format()ing invalid hand-constructed  POSIXlt  objects
if(hasTZ <- nzchar(.TZ <- Sys.getenv("TZ"))) cat(sprintf("env.var. TZ='%s'\n",.TZ))
d <- as.POSIXlt("2016-12-06", tz = "Europe/Vienna")
op <- options(warn = 1)# ==> assert*() will match behavior
if(is.null(d$zone)) cat("Skipping timezone-dependent POSIXlt formatting\n") else
for(EX in expression({}, Sys.setenv(TZ = "UTC"), Sys.unsetenv("TZ"))) {
    cat(format(EX),":\n---------\n")
    eval(EX)
    dz <- d$zone
    d$zone <- 1
    tools::assertError(format(d))
    d$zone <- NULL # now has 'gmtoff' but no 'zone' --> warning:
    tools::assertWarning(stopifnot(identical(format(d),"2016-12-06")))
    d$zone <- dz # = previous, but 'zone' now is last
    tools::assertError(format(d))
}
if(hasTZ) Sys.setenv(TZ = .TZ); options(op)# revert

dlt <- structure(
    list(sec = 52, min = 59L, hour = 18L, mday = 6L, mon = 11L, year = 116L,
         wday = 2L, yday = 340L, isdst = 0L, zone = "CET", gmtoff = 3600L),
    class = c("POSIXlt", "POSIXt"), tzone = "CET")
dlt$sec <- 10000 + 1:10 # almost three hours & uses re-cycling ..
fd <- format(dlt)
stopifnot(length(fd) == 10, identical(fd, format(dct <- as.POSIXct(dlt))))
dlt2 <- as.POSIXlt(dct)
stopifnot(identical(format(dlt2), fd))
## The two assertError()s gave a seg.fault in  R <= 3.3.2


stopifnot(inherits(methods("("), "MethodsFunction"),
          inherits(methods("{"), "MethodsFunction"))
## methods("(") and ..("{")  failed in R <= 3.3.2


## moved after commit in r71778
f <- eval(parse(text = "function() { x <- 1 ; for(i in 1:10) { i <- i }}",
                keep.source = TRUE))
g <- removeSource(f)
stopifnot(is.null(attributes(body(g)[[3L]][[4L]])))

## pmin/pmax of ordered factors -- broken in R 3.3.2  [PR #17195]
of <- ordered(c(1,5,6))
asI <- as.integer # < shorter code
set.seed(6); rof <- sample(of, 12, replace=TRUE)
stopifnot(exprs = {
    identical(pmax(rof, of), ordered(pmax(asI(rof), asI(of)), labels=levels(rof)) -> pmar)
    identical(pmax(of, rof), pmar)
    identical(pmin(rof, of), ordered(pmin(asI(rof), asI(of)), labels=levels(rof)) -> pmir)
    identical(pmin(of, rof), pmir)
    identical(pmin(rof, 5), ordered(pmin(asI(rof), 2), levels=1:3, labels=levels(rof)))
    identical(pmax(rof, 6), ordered(pmax(asI(rof), 3), levels=1:3, labels=levels(rof)))
    identical(pmax(rof, 1), rof)
    identical(pmin(rof, 6), rof)
    identical(pmax(of, 5, rof), ordered(pmax(asI(of),2L,asI(rof)), levels=1:3,
                                        labels=levels(of)))
})
## these were "always" true .. but may change (FIXME ?)
stopifnot(exprs = {
    identical(of,   pmin(of, 3)) # what? error? at least warning?
    identical(pmar, pmax(of, 3, rof))
})
## pmin/pmax() of 0-length S3 classed  [PR #17200]
for(ob0 in list(I(character()), I(0[0]), I(0L[0]),
                structure(logical(), class="L"),
                structure(character(), class="CH"))) {
    stopifnot(exprs = {
        identical(ob0, pmax(ob0, ob0))
        identical(ob0, pmin(ob0, ob0))
        identical(ob0, pmin(ob0, FALSE))
        identical(ob0, pmax(ob0, FALSE))
    })
}
## pmin()/pmax() of matching numeric data frames
mUSJ <- data.matrix(dUSJ <- USJudgeRatings)
stopifnot(exprs = {
    identical(              pmin(dUSJ, 10 - dUSJ),
              as.data.frame(pmin(mUSJ, 10 - mUSJ)))
    identical(              pmax(dUSJ, 10 - dUSJ),
              as.data.frame(pmax(mUSJ, 10 - mUSJ)))
})
## had failed for a while.   Note however :
d1 <- data.frame(y0 = 0:3 +1/2) ; (d1.2 <- d1[1:2, , drop=FALSE])
stopifnot(exprs = {  ## FIXME: The 'NA's really are wrong
    identical(pmax(d1,2),     data.frame(y0 = c(2, NA, 2.5, 3.5)))
    identical(pmax(d1, 3-d1), data.frame(y0 = .5+c(2, 1:3)))
    identical(pmax(d1.2, 2),  data.frame(y0 = c(2, NA)))
    identical(pmax(d1.2, 2-d1.2),data.frame(y0=c(1.5,1.5)))
    identical(pmin(d1, 2),    data.frame(y0 = c(.5+0:1, NA,NA)))
    identical(pmin(d1, 3-d1), data.frame(y0 = .5+c(0, 1:-1)))
    identical(pmin(d1.2, 2),  data.frame(y0 = c(.5, 1.5)))
    identical(pmin(d1.2, 2-d1.2),data.frame(y0 = c(.5,.5)))
})
## some CRAN pkgs have been relying that these at least "worked somehow"


## quantile(x, prob) monotonicity in prob[] - PR#16672
sortedQ <- function(x, prob, ...)
    vapply(1:9, function(type)
        !is.unsorted(quantile(x, prob, type=type, names=FALSE, ...)), NA)
xN <- c(NA, 10.5999999999999996, NA, NA, NA, 10.5999999999999996,
        NA, NA, NA, NA, NA, 11.3000000000000007, NA, NA,
        NA, NA, NA, NA, NA, 5.2000000000000002)
sQ.xN <- sortedQ(xN, probs = seq(0,1,1/10), na.rm = TRUE)
x2 <- rep(-0.00090419678460984, 602)
stopifnot(sQ.xN, sortedQ(x2, (0:5)/5))
## both not fulfilled in R < 3.4.0


## seq.int() anomalies in border cases, partly from Mick Jordan (on R-devel):
stopifnot(exprs = {
    identical(1,         seq.int(to=1,  by=1 ))
    identical(1:2,       seq.int(to=2L, by=1L))
    identical(c(1L, 3L), seq.int(1L, 3L, length.out=2))
})
## the first was missing(.), the others "double" in R < 3.4.0
assertErrV(seq(1,7, by = 1:2))# gave warnings in R < 3.4.0
## seq() for <complex> / <integer>
stopifnot(exprs = {
    all.equal(seq(1+1i, 9+2i, length.out = 9) -> sCplx,
              1:9 + 1i*seq(1,2, by=1/8))
    identical(seq(1+1i, 9+2i, along.with = 1:9), sCplx)
    identical(seq(1L, 3L, by=1L), 1:3)
})
## had failed in R-devel for a few days
D1 <- as.Date("2017-01-06")
D2 <- as.Date("2017-01-12")
seqD1 <- seq.Date(D1, D2, by = "1 day")
stopifnot(exprs = {
    identical(seqD1, seq(D1, D2, by = "1 days"))
    ## These two work "accidentally" via seq -> seq.default + "Date"-arithmetic
    identical(seqD1, seq(by = 1, from = D1, length.out = 7))
    identical(seqD1, seq(by = 1,   to = D2, length.out = 7))
    ## swap order of (by, to) ==> *FAILS* because directly calls seq.Date() - FIXME?
    TRUE ||
    identical(seqD1, seq(to = D2,  by = 1, length.out = 7))
    ## above had failed in R-devel for a couple of days
    identical(seq(9L, by = -1L, length.out = 4L), 9:6)
    identical(seq(9L, by = -1L, length.out = 4 ), 9:6)
})
## for consistency, new in R >= 3.4.0


## Underflow happened when parsing small hex constants PR#17199
stopifnot(exprs = {
    as.double("0x1.00000000d0000p-987") > 0   # should be 7.645296e-298
    as.double("0x1.0000000000000p-1022") > 0  # should be 2.225074e-308
    as.double("0x1.f89fc1a6f6613p-974") > 0   # should be 1.23456e-293
})
##


## format.POSIX[cl]t() after print.POSIXct()
dt <- "2012-12-12 12:12:12"
x <- as.POSIXct(dt, tz = "GMT")
stopifnot(identical(format(x), dt))
op <- options(warn=1)# allow
(Sys.t <- Sys.timezone()) # may occasionally warn (and work)
options(op)
someCET <- paste("Europe", c("Berlin", "Brussels", "Copenhagen", "Madrid",
                             "Paris", "Rome", "Vienna", "Zurich"), sep="/")
if(Sys.t %in% someCET)
    stopifnot(print(TRUE), identical(format(x, tz = ""), "2012-12-12 13:12:12"))
## had failed for almost a month in R-devel & R-patched


## xtabs() , notably with NA's :
asArr <- function(x) {
    attributes(x) <- list(dim=dim(x), dimnames=dimnames(x)); x }
as_A <- function(x, A) array(x, dim=dim(A), dimnames=dimnames(A))
eq_A <- function(a,b) ## equality of arrays, notably sparseMatrix vs dense
    identical(dim(a),dim(b)) && identical(dimnames(a),dimnames(b)) &&
        identical(as.vector(a), as.vector(b))
esoph2 <- droplevels(subset(esoph, subset = tobgp > "10-19" & alcgp >= "40-79"))
(xt <- xtabs(~ agegp + alcgp + tobgp, esoph2))
stopifnot(identical(dim(xt), c(6L, 3L, 2L)), # of the 6 x 3 x 2 = 36 entries,
          identical(which(xt == 0), c(7L, 12L, 18L, 23L, 30L, 32L, 36L)),
          ## the above 8 are zeros and the rest is 1 :
          all(xt[xt != 0] == 1))
xtC <- xtabs(ncontrols ~ agegp + alcgp + tobgp, data = esoph2)
stopifnot(# no NA's in data, hence result should have none, just 0's:
    identical(asArr(unname(xtC)),
 	      array(c(4, 13, 10, 13, 4, 3,   0, 2, 4, 3, 1, 0,	 1, 2, 1, 1, 0, 0,
 		      7,  8,  2,  3, 0, 0,   2, 1, 2, 0, 0, 0,	 2, 0, 0, 1, 0, 0),
 		    dim = dim(xt))))

DF <- as.data.frame(UCBAdmissions)
xt <- xtabs(Freq ~ Gender + Admit, DF)
stopifnot(identical(asArr(xt),
		    array(c(1198, 557, 1493, 1278), dim = c(2L, 2L),
			  dimnames = list(Gender = c("Male", "Female"),
					  Admit = c("Admitted", "Rejected")))))
op <- options(na.action = "na.omit")
DN <- DF; DN[cbind(6:9, c(1:2,4,1))] <- NA; DN

tools::assertError(# 'na.fail' should fail :
	   xtabs(Freq ~ Gender + Admit, DN, na.action = na.fail))
xt. <- xtabs(Freq ~ Gender + Admit, DN)
xtp <- xtabs(Freq ~ Gender + Admit, DN, na.action = na.pass)
xtN <- xtabs(Freq ~ Gender + Admit, DN, addNA = TRUE)
stopifnot(exprs = {
    identical(asArr(xt - xt.), as_A(c(120,17, 207, 8 ), xt))
    identical(asArr(xt - xtp), as_A(c(120,17, 207, NA), xt)) # not ok in R <= 3.3.2
    identical(asArr(-xtN + rbind(cbind(xt, 0), 0)),
              as_A(c(120, 17, -17, 207, NA, 0, -327, 0, 0), xtN))
})
## 'sparse = TRUE requires recommended package Matrix
if(requireNamespace('Matrix', lib.loc=.Library)) {
    xtS <- xtabs(Freq ~ Gender + Admit, DN, na.action = na.pass, sparse = TRUE)# error in R <= 3.3.2
    xtNS <- xtabs(Freq ~ Gender + Admit, DN, addNA = TRUE, sparse = TRUE)
    stopifnot(
        eq_A(xt., xtabs(Freq ~ Gender + Admit, DN, sparse = TRUE)),
        eq_A(xtp, xtS),
        eq_A(xtN, xtNS)
   )
}
## NA treatment partly wrong in R < 3.4.0; new option 'addNA'
ee <- esoph[esoph[,"ncases"] > 0, c(1:2,4)]
ee[,"ncases"] <- as.integer(ee[,"ncases"])
(tt <- xtabs(ncases ~ ., ee)); options(op)
stopifnot(identical(as.vector(tt[1:2,]), # *integer* + first value
		    c(0L, 1L, 0L, 4L, 0L, 0L, 1L, 4L)))
## keeping integer in sum()mation of integers


## tapply() with FUN returning raw  |  with factor -> returning integer
stopifnot(identical(tapply(1:3, 1:3, as.raw),
                    array(as.raw(1:3), 3L, dimnames=list(1:3))), ## failed in R < 3.4.0
          identical(3:1, as.vector(tapply(1:3, 1:3, factor, levels=3:1))))
x <- 1:2 ; (txx <- tapply(x, list(x, x), function(x) "a"))
##   1   2
## 1 "a" NA
## 2 NA  "a"
stopifnot(identical(txx,
  matrix(c("a", NA, NA, "a"), 2, dimnames = rep(list(as.character(x)),2L))))
## Failed in R 3.4.[01]


## str(<list of list>, max.level = 1)
LoL <- function(lenC, FUN = identity)
    lapply(seq_along(lenC), function(i) lapply(seq_len(lenC[i]), FUN))
xx <- LoL(c(7,3,17,798,3))
str(xx, list.len = 7, max.level = 1)
str2 <- capture.output(
 str(xx, list.len = 7, max.level = 2))
stopifnot(exprs = {
    grepl("List of ", capture.output(str(xx, list.len = 7, max.level = 1)))
    length(str2) == 35
    sum(grepl("list output truncated", str2)) == 2
    vapply(paste("List of", lengths(xx)), function(pat) any(grepl(pat, str2)), NA)
})
## wrongly showed '[list output truncated]'  in R < 3.4.0


## stopifnot(all.equal(.)) message abbreviation
msg <- tryCmsg(stopifnot(all.equal(rep(list(pi),4), list(3.1, 3.14, 3.141, 3.1415))))
writeLines(msg)
stopifnot(length(strsplit(msg,"\n")[[1]]) == 1+3+1)
## was wrong for months in R-devel only


## available.packages() (not) caching in case of errors
tools::assertWarning(ap1 <- available.packages(repos = "http://foo.bar"))
tools::assertWarning(ap2 <- available.packages(repos = "http://foo.bar"))
stopifnot(nrow(ap1) == 0, identical(ap1, ap2))
## had failed for a while in R-devel (left empty *.rds file)


## rep()/rep.int() : when 'times' is a list
stopifnot(exprs = {
    identical(rep    (4,   list(3)), c(4,4,4))
    identical(rep.int(4,   list(3)), c(4,4,4))
    identical(rep.int(4:5, list(2,1)), c(4L,4:5))
    identical(rep    (4:5, list(2,1)), c(4L,4:5))
})
## partly failed in R 3.3.{2,3}


## quantile(ordered(.)) - error message more directly useful
OL <- ordered(sample(LETTERS, 20, replace=TRUE))
(e <- tryCmsg(quantile(OL)))
stopifnot(exprs = {
    grepl("type.*1.*3", e) # typically works in several locales
    is.ordered(quantile(OL, type = 1))
    is.ordered(quantile(OL, type = 3))
})
## gave  "factors are not allowed" in R <= 3.3.x

## terms() ignored arg names (PR#17235)
a1 <- attr(terms(y ~ f(x, a = z) + f(x, a = z)),
           "term.labels")
a2 <- attr(terms(y ~ f(x, a = z) + f(x, b = z)),
           "term.labels")
stopifnot(length(a1) == 1, length(a2) == 2)
## both gave length 1


## by.data.frame() called not from toplevel w different arg names
dby <- function(dat, ind, F) by(dat, ind, FUN=F)
dby(warpbreaks, warpbreaks[,"tension"], summary)
if(!interactive())
stopifnot(is.list(r <- .Last.value), inherits(r, "by"))
## failed after r72531


## status returned by 'R CMD Sweave'
fil <- "Sweave-test-1.Rnw"
file.copy(system.file("Sweave", fil, package="utils"), tempdir())
owd <- setwd(tempdir())
(o <- capture.output(utils:::.Sweave(fil, no.q = TRUE), type = "message"))
stopifnot(grepl("exit status 0", o[2]))
setwd(owd)
## R CMD Sweave gave status 1 and hence an error in R 3.4.0 (only)


## print.noquote(*,  right = *)
nq <- noquote(LETTERS[1:9]); stopifnot(identical(nq, print(nq, right = TRUE)))
## print() failed a few days end in R-devel ca. May 1, 2017; non-identical for longer
tt <- table(c(rep(1, 7), 2,2,2))
stopifnot(identical(tt, print.noquote(tt)))
## print.noquote(<table>) failed for 6 weeks after r72638


## accessing  ..1  when ... is empty and using ..0, etc.
t0 <- function(...) ..0
t1 <- function(...) ..1
t2 <- function(...) ..2
stopifnot(identical(t1(pi, 2), pi), identical(t1(t1), t1),
	  identical(t2(pi, 2), 2))
et1 <- tryCid(t1())
if(englishMsgs)
    stopifnot(identical("the ... list contains fewer than 1 element",
			conditionMessage(et1)))
## previously gave   "'nthcdr' needs a list to CDR down"
et0   <- tryCid(t0()) ; (mt0   <- conditionMessage(et0))
et2.0 <- tryCid(t2()) ; (mt2.0 <- conditionMessage(et2.0))
et2.1 <- tryCid(t2(1)); (mt2.1 <- conditionMessage(et2.1))
if(englishMsgs)
    stopifnot(grepl("indexing '...' with .* index 0", mt0),
	      identical("the ... list contains fewer than 2 elements", mt2.0),
	      identical(mt2.0, mt2.1))
assertErrV(t0(1))
assertErrV(t0(1, 2))
## the first gave a different error msg, the next gave no error in R < 3.5.0


## stopifnot(e1, e2, ...) .. evaluating expressions sequentially
one <- 1
try(stopifnot(3 < 4:5, 5:6 >= 5, 6:8 <= 7, one <<- 2))
stopifnot(identical(one, 1)) # i.e., 'one <<- 2' was *not* evaluated
## all the expressions were evaluated in R <= 3.4.x
(et <- tryCid(stopifnot(0 < 1:10, is.numeric(..vaporware..), stop("FOO!"))))
stopifnot(exprs = {
    inherits(et, "simpleError")
    ## condition call now *does* contain 'stopifnot':
    ## !grepl("^stopifnot", deparse(conditionCall(et), width.cutoff=500))
    grepl("'..vaporware..'", conditionMessage(et))
})
## call was the full 'stopifnot(..)' in R < 3.5.0 .. and again in R > 3.6.x
## (don't afford tryCatch()ing everything)


## path.expand shouldn't translate to local encoding PR#17120
## This has been fixed on Windows, but not yet on Unix non-UTF8 systems
if(onWindows) {
    filename <- "\U9b3c.R"
    stopifnot(identical(path.expand(paste0("~/", filename)),
		 	      paste0(path.expand("~/"), filename)))
}
## Chinese character was changed to hex code


## aggregate.data.frame(*, drop=FALSE)  {new feature in R 3.3.0}
## PR#16918 : problem with near-eq. factor() levels "not quite matching"
group <- c(2 + 2^-51, 2)
d1 <- data.frame(n = seq(group))
b1 <- list(group = group)
stopifnot(
    identical(aggregate(d1, b1, length, drop = TRUE),
              aggregate(d1, b1, length, drop = FALSE)))
## drop=FALSE gave two rows + deprec. warning in R 3.3.x, and an error in 3.4.0


## line() [Tukey's resistant line]
cfs <- t(sapply(2:50, function(k) {x <- 1:k; line(x, 2+x)$coefficients }))
set.seed(7)
cf2 <- t(sapply(2:50, function(k) {
    x <- sample.int(k)
    line(x, 1-2*x)$coefficients }))
stopifnot(all.equal(cfs, matrix(c(2,  1), 49, 2, byrow=TRUE), tol = 1e-14), # typically exact
          all.equal(cf2, matrix(c(1, -2), 49, 2, byrow=TRUE), tol = 1e-14))
## had incorrect medians of the left/right third of the data (x_L, x_R), in R < 3.5.0


## 0-length Date and POSIX[cl]t:  PR#71290
D <- structure(17337, class = "Date") # Sys.Date() of "now"
D; D[0]; D[c(1,2,1)] # test printing of NA too
stopifnot(identical(capture.output(D[0]), "Date of length 0"))
D <- structure(1497973313.62798, class = c("POSIXct", "POSIXt")) # Sys.time()
D; D[0]; D[c(1,2,1)] # test printing of NA too
stopifnot(identical(capture.output(D[0]), "POSIXct of length 0"))
D <- as.POSIXlt(D)
D; D[0]; D[c(1,2,1)] # test printing of NA too
stopifnot(identical(capture.output(D[0]), "POSIXlt of length 0"))
## They printed as   '[1] "Date of length 0"'  etc in R < 3.5.0


## aggregate.data.frame() producing spurious names  PR#17283
dP <- state.x77[,"Population", drop=FALSE]
by <- list(Region = state.region, Cold = state.x77[,"Frost"] > 130)
a1 <- aggregate(dP, by=by, FUN=mean, simplify=TRUE)
a2 <- aggregate(dP, by=by, FUN=mean, simplify=FALSE)
stopifnot(exprs = {
    is.null(names(a1$Population))
    is.null(names(a2$Population))
    identical(unlist(a2$Population), a1$Population)
    all.equal(unlist(a2$Population),
              c(8802.8, 4208.12, 7233.83, 4582.57, 1360.5, 2372.17, 970.167),
              tol = 1e-6)
})
## in R <= 3.4.x, a2$Population had spurious names


## factor() with duplicated labels allowing to "merge levels"
x <- c("Male", "Man", "male", "Man", "Female")
## The pre-3.5.0 way {two function calls, nicely aligned}:
xf1 <- factor(x, levels = c("Male", "Man",  "male", "Female"))
           levels(xf1) <- c("Male", "Male", "Male", "Female")
## the new "direct" way:
xf <- factor(x, levels = c("Male", "Man",  "male", "Female"),
                labels = c("Male", "Male", "Male", "Female"))
stopifnot(identical(xf1, xf),
          identical(xf, factor(c(rep(1,4),2), labels = c("Male", "Female"))))
## Before R 3.5.0, the 2nd factor() call gave an error
aN <- c("a",NA)
stopifnot(identical(levels(factor(1:2, labels = aN)), aN))
## the NA-level had been dropped for a few days in R-devel(3.5.0)
##

## Factor behavior -- these have been unchanged, also in R >= 3.5.0 :
ff <- factor(c(NA,2,3), levels = c(2, NA), labels = c("my", NA), exclude = NULL)
stopifnot(exprs = { ## all these have been TRUE "forever" :
    identical(as.vector(ff), as.character(ff))
    identical(as.vector(ff), c(NA, "my", NA))
    identical(capture.output(ff), c("[1] <NA> my   <NA>",
				    "Levels: my <NA>"))
    identical(factor(ff),
	      structure(c(NA, 1L, NA), .Label = "my", class = "factor"))
    identical(factor(ff, exclude=NULL),
	      structure(c(2L, 1L, 2L), .Label = c("my", NA), class = "factor"))
    identical(as.integer(       ff),                c(2:1,NA))
    identical(as.integer(factor(ff, exclude=NULL)), c(2:1,2L))
})


## within.list({ .. rm( >=2 entries ) }) :
L <- list(x = 1, y = 2, z = 3)
stopifnot(identical(within(L, rm(x,y)), list(z = 3)))
## has failed since R 2.7.2 patched (Aug. 2008) without any noticeable effect
sortN <- function(x) x[sort(names(x))]
LN <- list(y = 2, N = NULL, z = 5)
stopifnot(exprs = {
    identical(within(LN, { z2 <- z^2 ; rm(y,z,N) }),
              list(z2 = 5^2)) ## failed since Aug. 2008
    identical(within(LN, { z2 <- z^2 ; rm(y,z) }),
              list(N = NULL, z2 = 5^2)) ## failed for a few days in R-devel
    ## within.list() fast version
    identical(sortN(within(LN, { z2 <- z^2 ; rm(y,z) }, keepAttrs=FALSE)),
              sortN(list(N = NULL, z2 = 5^2)))
})


## write.csv did not signal an error if the disk was full PR#17243
if (file.access("/dev/full", mode = 2) == 0) { # Not on all systems...
    cat("Using  /dev/full  checking write errors... ")
    # Large writes should fail mid-write
    tools::assertError(write.table(data.frame(x=1:1000000), file = "/dev/full"))
    # Small writes should fail on closing
    tools::assertWarning(write.table(data.frame(x=1), file = "/dev/full"))
    cat("[Ok]\n")
}
## Silently failed up to 3.4.1


## model.matrix() with "empty RHS" -- PR#14992 re-opened
row.names(trees) <- 42 + seq_len(nrow(trees))
.RN <- row.names(mf <- model.frame(log(Volume) ~ log(Height) + log(Girth), trees))
stopifnot(identical(.RN, row.names(model.matrix(~ 1, mf))),
	  identical(.RN, row.names(model.matrix(~ 0, mf))))
## had 1:nrow()  up to 3.4.x


## "\n" etc in calls and function definitions
(qq <- quote(-"\n"))
stopifnot(exprs = {
    identical('-"\\n"', cq <- capture.output(qq))
    identical(5L, nchar(cq))
    identical(6L, nchar(capture.output(quote(("\t")))))
})
## backslashes in language objects accidentally duplicated in R 3.4.1


## length(<pairlist>) <- N
pl <- pairlist(a=1, b=2); length(pl) <- 1
al <- formals(ls);        length(al) <- 2
stopifnot(identical(pl, pairlist(a = 1)),
	  identical(al, as.pairlist(alist(name = , pos = -1L))))
## both `length<-` failed in R <= 3.4.1; the 2nd one for the wrong reason


## dist(*, "canberra") :
x <- cbind(c(-1,-5,10), c(-2,7,8)); (dc <- dist(x, method="canberra"))
##          1        2
## 2 1.666667
## 3 2.000000 1.066667
stopifnot(all.equal(as.vector(dc), c(25, 30, 16)/15))
## R's definition wrongly assumed x[] entries all of the same sign


## sigma( <rank-deficient model> ), PR#17313
dd <- data.frame(x1 = LETTERS[c(1,2,3, 1,2,3, 1,2,3)],
                 x2 = letters[c(1,2,1, 2,1,1, 1,2,1)], y = 1:9)
(sf <- summary(fit <- lm(y ~ x1*x2, data = dd))) ## last coef is NA
stopifnot(all.equal(sigma(fit)^2,  27/2,  tol = 1e-14),
	  all.equal(sigma(fit), sf$sigma, tol = 1e-14))
## was too large because of wrong denom. d.f. in R <= 3.4.1


## nclass.FD() and nclass.scott() for "extreme" data, PR#17274
NC <- function(x) c(Sturges = nclass.Sturges(x),
                    Scott = nclass.scott(x), FD = nclass.FD(x))
xE <- function(eps, n = 5) {
    stopifnot(n >= 2, is.numeric(eps), eps >= 0)
    c(rep.int(1, n-2), 1+eps, 2)
}
ncE <- c(Sturges = 4, Scott = 2, FD = 3)
stopifnot(exprs = {
    sapply(-5:-16, function(E) identical(NC(xE(10^E)), ncE))
    identical(NC(xE(1e-4)), c(Sturges = 4, Scott = 2, FD = 8550))
    identical(NC(xE(1e-3)), c(Sturges = 4, Scott = 2, FD =  855))
})
## for these, nclass.FD() had "exploded" in R <= 3.4.1
## Extremely large diff(range(.)) : NB: this gives a UBSAN warning
XXL <- c(1:9, c(-1,1)*1e300)
stopifnot(nclass.scott(XXL) == 1)
## gave 0 in R <= 3.4.1
tools::assertWarning(hh <- hist(XXL, "FD", plot=FALSE))
stopifnot(sum(hh$counts) == length(XXL))
## gave error from pretty.default + NA coercion warning in R <= 3.4.1


## methods:::rbind / cbind no longer deeply recursive also fixes bug:
library(methods)
myM <- setClass("myMatrix", contains="matrix")
T <- rbind(1:2, c=2, "a+"=10, myM(4:1,2), deparse.level=0)
stopifnot(identical(rownames(T), c("", "c", "a+", "", "")))
## rownames(.) wrongly were NULL in R <= 3.4.1
proc.time() - .pt; .pt <- proc.time()


## qr.coef(qr(X, LAPACK=TRUE)) when X has column names, etc
X <- cbind(int = 1,
           c2 = c(2, 8, 3, 10),
           c3 = c(2, 5, 2, 2)); rownames(X) <- paste0("r", 1:4)
y <- c(2,3,5,7); yc <- as.complex(y)
q.Li <- qr(X);              cfLi <- qr.coef(q.Li, y)
q.LA <- qr(X, LAPACK=TRUE); cfLA <- qr.coef(q.LA, y)
q.Cx <- qr(X + 0i);         cfCx <- qr.coef(q.Cx, y)
e1 <- tryCid(qr.coef(q.Li, y[-4])); e1
e2 <- tryCid(qr.coef(q.LA, y[-4]))
stopifnot(exprs = {
    all.equal(cfLi,    cfLA , tol = 1e-14)# 6.376e-16 (64b Lx)
    all.equal(cfLi, Re(cfCx), tol = 1e-14)#  (ditto)
    identical(conditionMessage(e1), conditionMessage(e2))
})
## 1) cfLA & cfCx had no names in R <= 3.4.1
## 2) error messages were not consistent


## invalid user device function  options(device = *) -- PR#15883
graphics.off() # just in case
op <- options(device=function(...){}) # non-sense device
assertErrV(plot.new())
if(no.grid <- !("grid" %in% loadedNamespaces())) requireNamespace("grid")
assertErrV(grid::grid.newpage())
if(no.grid) unloadNamespace("grid") ; options(op)
## both errors gave segfaults in R <= 3.4.1


## readRDS(textConnection())
abc <- c("a", "b", "c"); tmpC <- ""
zz <- textConnection('tmpC', 'wb')
saveRDS(abc, zz, ascii = TRUE)
sObj <- paste(textConnectionValue(zz), collapse='\n')
close(zz); rm(zz)
stopifnot(exprs = {
    identical(abc, readRDS(textConnection(tmpC)))
    identical(abc, readRDS(textConnection(sObj)))
})
## failed in R 3.4.1 only


## Ops (including arithmetic) with 0-column data frames:
d0 <- USArrests[, FALSE]
stopifnot(exprs = {
    identical(d0, sin(d0))
    identical(d0, d0 + 1); identical(d0, 2 / d0) # failed
    all.equal(sqrt(USArrests), USArrests ^ (1/2)) # now both data frames
    is.matrix(m0 <- 0 < d0)
    identical(dim(m0), dim(d0))
    identical(dimnames(m0)[1], dimnames(d0)[1])
    identical(d0 & d0, m0)
})
## all but the first failed in R < 3.5.0


## pretty(x, n) for n = <large> or  large diff(range(x)) gave overflow in C code
(fLrg <- Filter(function(.) . < 9e307, c(outer(1:8, 10^(0:2))*1e306)))
pL  <- vapply(fLrg, function(f)length(pretty(c(-f,f), n = 100,  min.n = 1)), 1L)
pL
pL3 <- vapply(fLrg, function(f)length(pretty(c(-f,f), n = 10^3, min.n = 1)), 1L)
pL3
stopifnot(71 <= pL, pL <= 141, # 81 <= pL[-7], # not on Win-64: pL[-15] <= 121,
          701 <= pL3, pL3 <= 1401) # <= 1201 usually
## in R < 3.5.0, both had values as low as 17
## without long doubles, min(pl[-7]) is 71.


### Several returnValue() fixes (r 73111) --------------------------
##          =============
## returnValue() corner case 1: return 'default' on error
hret <- NULL
fret <- NULL
h <- function() {
  on.exit(hret <<- returnValue(27))
  stop("h fails")
}
f <- function() {
    on.exit(fret <<- returnValue(27))
    h()
    1
}
res <- tryCatch(f(), error=function(e) 21)
stopifnot(exprs = {
    identical(fret, 27)
    identical(hret, 27)
    identical(res, 21)
})
##
## returnValue corner case 2: return 'default' on non-local return
fret <- NULL
gret <- NULL
f <- function(expr) {
  on.exit(fret <<- returnValue(28))
  expr
  1
}
g <- function() {
  on.exit(gret <<- returnValue(28))
  f(return(2))
  3
}
res <- g()
stopifnot(exprs = {
    identical(fret, 28)
    identical(gret, 2)
    identical(res, 2)
})
##
## returnValue corner case 3: return 'default' on restart
mret <- NULL
hret <- NULL
lret <- NULL
uvarg <- NULL
uvret <- NULL
h <- function(x) {
  on.exit(hret <<- returnValue(29))
  withCallingHandlers(
    myerror = function(e) invokeRestart("use_value", 1),
    m(x)
  )
}
m <- function(x) {
  on.exit(mret <<- returnValue(29))
  res <- withRestarts(
    l(x),
    use_value = function(x) {
      on.exit(uvret <<- returnValue(29))
      uvarg <<- x
      3
    }
  )
  res
}
l <- function(x) {
  on.exit(lret <<- returnValue(29))
  if (x > 1) {
    res <- x+1
    return(res)
  }
  cond <- structure(
    class = c("myerror", "error", "condition"),
    list(message = c("This is not an error", call = sys.call()))
  )
  stop(cond)
}
res <- h(1)
stopifnot(exprs = {
    identical(res, 3)
    identical(mret, 3)
    identical(hret, 3)
    identical(lret, 29)
    identical(uvarg, 1)
    identical(uvret, 3)
})
##
## returnValue: callCC
fret <- NULL
f <- function(exitfun) {
  on.exit(fret <<- returnValue(30))
  exitfun(3)
  4
}
res <- callCC(f)
stopifnot(identical(res, 3), identical(fret, 30))
##
## returnValue: instrumented callCC
fret <- NULL
mycallCCret <- NULL
funret <- NULL
mycallCC <- function(fun) {
  value <- NULL
  on.exit(mycallCCret <<- returnValue(31))
  delayedAssign("throw", return(value))
  fun(function(v) {
    on.exit(funret <<- returnValue(31))
    value <<- v
    throw
  })
}
f <- function(exitfun) {
  on.exit(fret <<- returnValue(31))
  exitfun(3)
  4
}
res <- mycallCC(f)
stopifnot(exprs = {
    identical(res, 3)
    identical(fret, 31)
    identical(mycallCCret, 3)
    identical(funret, 31)
})
## end{ returnValue() section}


## array(<empty>, *)  should create (corresponding) NAs for non-raw atomic:
a <- array(character(), 1:2)
stopifnot(identical(a, matrix(character(), 1,2)), is.na(a))
## had "" instead of NA in R < 3.5.0


## chaining on.exit handlers with return statements
x <- 0
fret1 <- NULL
fret2 <- NULL
f <- function() {
  on.exit(return(4))
  on.exit({fret1 <<- returnValue(); return(5)}, add = T)
  on.exit({fret2 <<- returnValue(); x <<- 2}, add = T)
  3
}
res <- f()
stopifnot(exprs = {
    identical(res, 5)
    identical(x, 2)
    identical(fret1, 4)
    identical(fret2, 5)
})


## splineDesign(*, derivs = <too large>):
if(no.splines <- !("splines" %in% loadedNamespaces())) requireNamespace("splines")
x <- (0:8)/8
aKnots <- c(rep(0, 4), c(0.3, 0.5, 0.6), rep(1, 4))
assertErrV(splines::splineDesign(aKnots, x, derivs = 4))
## gave seg.fault in R <= 3.4.1


## allow on.exit handlers to be added in LIFO order
x <- character(0)
f <- function() {
    on.exit(x <<- c(x, "first"))
    on.exit(x <<- c(x, "last"), add = TRUE, after = FALSE)
}
f()
stopifnot(identical(x, c("last", "first")))
##
x <- character(0)
f <- function() {
    on.exit(x <<- c(x, "last"), add = TRUE, after = FALSE)
}
f()
stopifnot(identical(x, "last"))


## deparse(<symbol>)
##_reverted_for_now
##_ brc <- quote(`{`)
##_ stopifnot(identical(brc, eval(parse(text = deparse(brc, control="all")))))
## default was to set  backtick=FALSE  so parse() failed in R <= 3.4.x


## sys.on.exit() is called in the correct frame
fn <- function() {
    on.exit("foo")
    identity(sys.on.exit())
}
stopifnot(identical(fn(), "foo"))


## rep.POSIXt(*, by="n  DSTdays") - PR#17342
x <- seq(as.POSIXct("1982-04-15 05:00", tz="US/Central"),
         as.POSIXct("1994-10-15",       tz="US/Central"), by="360 DSTdays")
stopifnot(length(x) == 13, diff((as.numeric(x) - 39600)/86400) == 360)
## length(x) was 1802 and ended in many NA's in R <= 3.4.2


## 0-length logic with raw()
r0 <- raw(0)
stopifnot(exprs = {
    identical(r0 & r0, r0)
    identical(r0 | r0, r0)
})
## gave logical(0) in R 3.4.[012]


## `[[`  and  `[[<-`  indexing with <symbol>
x <- c(a=2, b=3)
x[[quote(b)]] <- pi
stopifnot(exprs = {
    identical(2, x[[quote(a)]])
    identical(x, c(a=2, b=pi))
})
## `[[` only worked after fixing PR#17314, i.e., not in R <= 3.4.x


## range(<non-numeric>, finite = TRUE)
stopifnot(identical(0:1, range(c(NA,TRUE,FALSE), finite=TRUE)))
## gave NA's in R <= 3.4.2


## `[<-` : coercion should happen also in 0-length case:
x1 <- x0 <- x <- n0 <- numeric(); x0[] <- character(); x1[1[0]] <- character()
x[] <- numeric()
stopifnot(identical(x0, character()), identical(x1, x0), identical(x, n0))
## x0, x1 had remained 'numeric()' in  R <= 3.4.x
x[1] <- numeric(); stopifnot(identical(x, n0))
## had always worked; just checking
NUL <- NULL
NUL[3] <- integer(0); NUL[,2] <- character() ; NUL[3,4,5] <- list()
stopifnot(is.null(NUL))
## above had failed for one day in R-devel; next one always worked
NUL <- NULL; NUL[character()] <- "A"
stopifnot(identical(NUL, character()))
## 0-0-length subassignment should not change atomic to list:
ec <- e0 <- matrix(, 0, 4) # a  0 x 4  matrix
ec[,1:2] <- list()
x <- 1[0]; x[1:2] <- list()
a <- a0 <- array("", 0:2); a[,1,] <- expression()
stopifnot(exprs = {
    identical(ec, e0)
    identical(x, 1[0])
    identical(a, a0)
})## failed for a couple of days in R-devel


## as.character(<list>) should keep names in some nested cases
cl <-     'list(list(a = 1, "B", ch = "CH", L = list(f = 7)))'
E <- expression(list(a = 1, "B", ch = "CH", L = list(f = 7)))
str(ll <- eval(parse(text = cl)))
stopifnot(exprs = {
    identical(eval(E), ll[[1]])
    identical(as.character(E), as.character(ll) -> cll)
    grepl(cll, cl, fixed=TRUE) # currently, cl == paste0("list(", cll, ")")
    ## the last two have failed in R-devel for a while
    identical(as.character(list(list(one = 1))), "list(one = 1)")
    identical(as.character(list(  c (one = 1))),    "c(one = 1)")
})## the last gave "1" in all previous versions of R


## as.matrix( <data.frame in d.fr.> ) -- prompted by Patrick Perry, R-devel 2017-11-30
dm <- dd <- d1 <- data.frame(n = 1:3)
dd[[1]] <- d1            # -> 'dd' has "n" twice
dm[[1]] <- as.matrix(d1) #    (ditto)
d. <- structure(list(d1), class = "data.frame", row.names = c(NA, -3L))
d2. <- data.frame(ch = c("A","b"), m = 10:11)
d2  <- data.frame(V = 1:2); d2$V <- d2.; d2
d3 <- structure(list(A = 1:2, HH = cbind(c(.5, 1))),
                class = "data.frame", row.names=c(NA,-2L))
d3.2 <- d3; d3.2 $HH <- diag(2)
d3.2.<- d3; d3.2.$HH <- matrix(1:4, 2,2, dimnames=list(NULL,c("x","y")))
d0 <- as.data.frame(m0 <- matrix(,2,0))
d3.0 <- d3; d3.0 $HH <- m0
d3.d0<- d3; d3.d0$HH <- d0
stopifnot(exprs = {
    identical(unname(as.matrix(d0)), m0)
    identCO  (dd, d.)
    identical(as.matrix(d3.0 ), array(1:2, dim = 2:1, dimnames = list(NULL, "A")) -> m21)
    identical(as.matrix(d3.d0), m21)
    identical(as.matrix(dd), (cbind(n = 1:3) -> m.))
    identical(as.matrix(d.), m.)
    identical(as.matrix(d2), array(c("A", "b", "10", "11"), c(2L, 2L),
                                   dimnames = list(NULL, c("V.ch", "V.m"))))
    identical(as.matrix(dm), m.)
    identical(as.matrix(d1), m.)
    identical(colnames(m2 <- as.matrix(d2)), c("V.ch", "V.m"))
    identical(colnames(as.matrix(d3   )), colnames(d3   )) # failed a few days
    identical(colnames(as.matrix(d3.2 )), colnames(format(d3.2 )))
    identical(colnames(as.matrix(d3.2 )), c("A", paste("HH",1:2,sep=".")))
    identical(colnames(as.matrix(d3.2.)), colnames(format(d3.2.)))
    identical(colnames(as.matrix(d3.2.)), c("A", "HH.x", "HH.y"))
})
## the first  5  as.matrix() have failed at least since R-1.9.1, 2004


## Impossible conditions should at least give a warning - PR#17345
tools::assertWarning(
           power.prop.test(n=30, p1=0.90, p2=NULL, power=0.8)
       ) ## may give error in future
## silently gave p2 = 1.03 > 1 in R versions v, 3.1.3 <= v <= 3.4.3


## 1) removeSource() [for a function w/ body containing NULL]:
op <- options(keep.source=TRUE)
bod <- quote( foo(x, NULL) )
testf  <- function(x) { }; body(testf)[[2]] <- bod
testf
testfN <- removeSource(testf)
stopifnot(identical(body(testf )[[2]], bod)
        , identical(body(testfN)[[2]], bod)
)
## erronously changed  '(x, NULL)'  to  '(x)'  in R version <= 3.4.3
##
## 2) source *should* be kept:
f <- function(x=1) { # 'x' not really needed
    x+x + 2*x+1 # (note spaces)
}
stopifnot(exprs = {
    identical(capture.output(f) -> fsrc,
              capture.output(print(f)))
    length(fsrc) == 3
    grepl("(x=1)",             fsrc[1], fixed=TRUE)
    grepl("really needed",     fsrc[1], fixed=TRUE)
    grepl("x + 2*x+1 # (note", fsrc[2], fixed=TRUE)
})
options(op)
## (was fine, but not tested in R <= 3.5.0)


## ar.yw(x) with missing values in x, PR#17366
which(is.na(presidents)) # in 6 places
arp <- ar(presidents, na.action = na.pass)
## check "some" consistency with cheap imputation:
prF <- presidents
prF[is.na(presidents)] <- c(90, 37, 40, 32, 63, 66) # phantasy
arF <- ar(prF)
stopifnot(exprs = {
    all.equal(arp[c("order", "ar", "var.pred", "x.mean")],
              list(order = 3, ar = c(0.6665119, 0.2800927, -0.1716641),
                   var.pred = 96.69082, x.mean = 56.30702), tol = 7e-7)
    all.equal(arp$ar, arF$ar,                     tol = 0.14)
    all.equal(arp$var.pred, arF$var.pred,         tol = 0.005)
    all.equal(arp$asy.var.coef, arF$asy.var.coef, tol = 0.09)
})
## Multivariate
set.seed(42)
n <- 1e5
(i <- sample(n, 12))
u <- matrix(rnorm(2*n), n, 2)
y <- filter(u, filter=0.8, "recursive")
y. <- y; y.[i,] <- NA
est  <- ar(        y  , aic = FALSE, order.max = 2) ## Estimate VAR(2)
es.  <- ar(        y. , aic = FALSE, order.max = 2, na.action=na.pass)
## checking ar.yw.default() multivariate case
estd <- ar(unclass(y) , aic = FALSE, order.max = 2) ## Estimate VAR(2)
es.d <- ar(unclass(y.), aic = FALSE, order.max = 2, na.action=na.pass)
stopifnot(exprs = {
    all.equal(est$ar[1,,], diag(0.8, 2), tol = 0.08)# seen 0.0038
    all.equal(est[1:6], es.[1:6], tol = 5e-3)
    all.equal(estd$x.mean, es.d$x.mean, tol = 0.01) # seen 0.0023
    all.equal(estd[c(1:3,5:6)],
              es.d[c(1:3,5:6)], tol = 1e-3)## seen {1,3,8}e-4
    all.equal(lapply(estd[1:6],unname),
              lapply(est [1:6],unname), tol = 2e-12)# almost identical
    all.equal(lapply(es.d[1:6],unname),
              lapply(es. [1:6],unname), tol = 1e-11)
})
## NA's in x gave an error, in R versions <= 3.4.3


## as.list(<Date>) method:
toD <- Sys.Date(); stopifnot(identical(as.list(toD)[[1]], toD))
## was wrong for 20 hours

options(warn = 2)# no warnings allowed

## PR#17372: sum(<ints whose sum overflows>, <higher type>)
iL <- rep(1073741824L, 2) # 2^30 + 2^30 = 2^31 integer overflows to NA
r1 <- tryCmsg(sum("foo", iL))
r2 <- tryCmsg(sum(iL, "foo"))
stopifnot(exprs = {
    identical(r1, r2)
    grepl("invalid 'type' (character) ", r1, fixed=TRUE)
    ## each _gave_ an overflow warning + NA
    identical(sum(3.14, iL), sum(iL, 3.14))
    identical(sum(1+2i, iL), sum(iL, 1+2i))
    if(identical(.Machine$sizeof.longlong, 8L))
        TRUE # no longer overflows early when we have LONG_INT :
    else { # no LONG_INT [very rare in 2018-02 !]
        identical(sum(3.14, iL), NA_real_) &&
        identical(sum(1+2i, iL), NA_complex_)
    }
})
## r2 was no error and sum(iL, 1+2i) gave NA_real_ in R <= 3.4.x
## Was PR#1408 Inconsistencies in sum() {in ./reg-tests-2.R}
x <- as.integer(2^31 - 1)## = 2147483647L = .Machine$integer.max ("everywhere")
x24 <- rep.int(x, 2^24) # sum = 2^55 - 2^24
stopifnot(exprs = {
    sum(x, x)   == 2^32-2 # did not warn in 1.4.1 -- no longer overflows in 3.5.0
    sum(c(x,x)) ==(2^32-2 -> sx2) # did warn -- no longer overflows
    (z <- sum(x, x, 0.0)) == sx2 # was NA in 1.4.1
    typeof(z) == "double"
    is.integer(x24)
    sum(x24) == 2^55 - 2^24 # was NA (+ warning) in R <= 3.4.x
})


## aggregate.data.frame(*, drop=FALSE)  wishlist PR#17280
## [continued from above]
aF <- aggregate(dP, by=by, FUN=mean,   drop=FALSE)
lF <- aggregate(dP, by=by, FUN=length, drop=FALSE)
stopifnot(exprs = {
    identical(dim(aF), c(8L, 3L))
    identical(aF[6,3], NA_real_)
    identical(lF[6,3], NA_integer_)
})
DF <- data.frame(a=rep(1:3,4), b=factor(rep(1:2,6), levels=1:3))
aT <- aggregate(DF["a"], DF["b"], length)# drop=TRUE
aF <- aggregate(DF["a"], DF["b"], length,  drop=FALSE)
stopifnot(exprs = {
    identical(dim(aT), c(2L,2L))
    identical(dim(aF), c(3L,2L))
    identical(aT, aF[1:2,])
    identical(aF[3,"a"], NA_integer_)
    })
## In R <= 3.4.x, the function (FUN) was called on empty sets, above,
## giving NaN (and 0) or <nothing>;  now the result is NA.


## PR#16107  is.na(NULL) throws warning (contrary to all other such calls)
stopifnot(identical(is.na(NULL), logical(0)))
## gave a warning in R <= 3.4.x


## subtle [[<- , e.g.,  <nestedList>[[ c(i,j,k) ]]  <-  val :
xx0 <-
xx <- list(id = 1L,
           split = list(varid = 1L, breaks = NULL,
                        index = 1:3, right = TRUE, info = "s"),
           kids = list(id = 2L,
                       split = list(varid = 3L, breaks = 75,
                                    right = TRUE, info = "KS"),
                       kids = list(list(id = 3L, info = "yes"),
                                   list(id = 4L, info = "no")),
                       info = NULL),
           list(id = 5L,
                split = list(varid = 3L, breaks = 20,
                             right = TRUE, info = "4s"),
                kids = list(list(id = 6L, info = "no"),
                            list(id = 7L, info = "yes")),
                info = NULL),
           info = NULL)

## no-ops:
xx[[1]] <- xx0[[1]]
xx[["kids"]] <- xx0[["kids"]]
xx[[2:1]] <- xx0[[2:1]] ; stopifnot(identical(xx, xx0))
xx[[3:1]] <- xx0[[3:1]] ; stopifnot(identical(xx, xx0)) # (err)
## replacements
              xx[[c(2,3)]]   <- 5:3
              xx[[c(4,2,4)]] <- c(4,2,c=4) # (err: wrong xx)
              xx[[c(4,2,3)]] <- c(ch="423")# (err)
              xx[[c(3,2,2)]] <- 47         # (err)
stopifnot(exprs = {
    identical(xx[[c(2,3)]],     5:3)
    identical(xx[[c(4,2,4)]],   c(4,2,c=4))
    identical(xx[[c(4,2,3)]],   c(ch="423"))
    identical(xx[[c(3,2,2)]],   47)
    identical(lengths(xx), lengths(xx0))
    identical(  names(xx),   names(xx0))
    identical(lapply(xx, lengths),
              lapply(xx0,lengths))
    identical(lapply(xx, names),
              lapply(xx0,names))
})
## several of these failed for a bit more than a day in R-devel


## PR#17369 and PR#17381 -- duplicated() & unique() data frame methods:
d22 <- data.frame(x = c(.3 + .6, .9), y = 1)
d21 <- d22[,"x", drop=FALSE]
dRT <- data.frame(x = c("\r", "\r\r"), y = c("\r\r", "\r"))
stopifnot(exprs = {
    identical(unique(d22), d22) # err
    is.data.frame(d21)
    identical(dim(d21), 2:1)
    identical(unique(d21), d21)
    identical(unique(dRT), dRT) # err
    })
## with a POSIXct column (with tz during Daylight Saving change):
Sys.setenv("TZ" = "Australia/Melbourne") # <== crucial (for most)!
x <- as.POSIXct(paste0("2013-04-06 ", 13:17, ":00:00"), tz = "UTC")
attr(x, "tzone") <- ""
(xMelb <- as.POSIXct(x, tz = "Australia/Melbourne"))# shows both AEDT & AEST
dMb <- data.frame(x = xMelb, y = 1)
stopifnot(exprs = {
    identical(unique(dMb), dMb)
    identical(anyDuplicated(dMb), 0L)
}) # both differing in R <= 3.4.x


## when sep is given, an opening quote may be preceded by non-space
stopifnot(  ncol(read.table(              text="=\"Total\t\"\t1\n",sep="\t")) == 2)
stopifnot(length(scan(what=list("foo",1), text="=\"Total\t\"\t1\n",sep="\t")) == 2)
##
## in 3.4.x, read.table failed on this
stopifnot(  ncol(read.table(              text="=\"CJ01 \"\t550\n",sep="\t")) == 2)
stopifnot(length(scan(what=list("foo",1), text="=\"CJ01 \"\t550\n",sep="\t")) == 2)
##
## when no sep is given, quotes preceded by non-space have no special
## meaning and are retained (related to PR#15245)
stopifnot(read.table(                 text="HO5\'\'\tH")[1,1] == "HO5\'\'")
stopifnot(read.table(                 text="HO5\'\tH")[1,1]   == "HO5\'")
stopifnot(scan(what=list("foo","foo"),text="HO5\'\'\tH")[[1]] == "HO5\'\'")
stopifnot(scan(what=list("foo","foo"),text="HO5\'\tH")[[1]]   == "HO5\'")
##
## when no sep is given, there does not have to be a separator between
## quoted entries; testing here to ensure read.table and scan agree,
## but without claiming this particular behavior is needed
stopifnot(read.table(                 text="\"A\"\" B \"")$V2   == " B ")
stopifnot(scan(what=list("foo","foo"),text="\"A\"\" B \"")[[2]] == " B ")


## merge() names when by.y
parents <- data.frame(name = c("Sarah", "Max", "Qin", "Lex"),
                      sex = c("F", "M", "F", "M"), age = c(41, 43, 36, 51))
children <- data.frame(parent = c("Sarah", "Max", "Qin"),
                       name = c("Oliver", "Sebastian", "Kai-lee"),
                       sex = c("M", "M", "F"), age = c(5,8,7))
# merge.data.frame() no longer creating a duplicated col.names
(m   <- merge(parents, children, by.x = "name", by.y = "parent"))
 m._ <- merge(parents, children, by.x = "name", by.y = "parent", all.x=TRUE)
(m_. <- merge(parents, children, by.x = "name", by.y = "parent", all.y=TRUE))
 m__ <- merge(parents, children, by.x = "name", by.y = "parent", all = TRUE)
## all four gave duplicate column 'name' with a warning in R <= 3.4.x
stopifnot(exprs = {
    identical(m,   m_.)
    identical(m._, m__)
    ## not identical(m, m__[-1,]) : row.names differ
    identical(names(m), names(m__))
    all(m == m__[-1,])
    identical(dim(m),   c(3L, 6L))
    identical(dim(m__), c(4L, 6L))
})


## scale(*, <non-numeric>)
if(requireNamespace('Matrix', lib.loc=.Library)) {
    de <- data.frame(Type = structure(c(1L, 1L, 4L, 1L, 4L, 2L, 2L, 2L, 4L, 1L),
				      .Label = paste0("T", 1:4), class = "factor"),
		     Subj = structure(c(9L, 5L, 8L, 3L, 3L, 4L, 3L, 6L, 6L, 1L),
				      .Label = as.character(1:9), class = "factor"))
    show(SM <- xtabs(~ Type + Subj, data = de, sparse=TRUE))
    stopifnot(exprs = {
	inherits(SM, "sparseMatrix")
	all.equal(scale(SM, Matrix::colMeans(SM)),
		  scale(SM, Matrix::colMeans(SM, sparse=TRUE)),
		  check.attributes=FALSE)
    })
}
## 2nd scale() gave wrong error "length of 'center' must equal [..] columns of 'x'"
## in R <= 3.4.x


## as.data.frame.matrix() method not eliminating duplicated rownames
(m <- rbind(x = 1:3, x = 2:4, z = 0)) # matrix with duplicated rownams
rownames(d <- as.data.frame(m)) # --> fixed up to  "x" "x.1" "z"
## new feature -- 'make.names = *'  with '*' in non-defaults :
dN <- as.data.frame(m, make.names=NA)
tools::assertError( dF <- as.data.frame(m, make.names=FALSE) )
stopifnot(exprs = {
    !anyDuplicated(rownames(d))
    identical(colnames(d), paste0("V", 1:3))
    ## dN has correct automatic row names:
    identical(.row_names_info(dN, 0), .set_row_names(3L))
})
## as.data.frame(m)  kept the duplicated row names in R 3.4.x


## check that sorting preserves names and no other attributes
v <- sort(c(1,2,3))
names(v) <- letters[1:3]
stopifnot(identical(sort(v), v))
vv <- sort(c(1,2,3))
names(vv) <- names(v)
attr(vv, "foo") <- "bar"
stopifnot(identical(sort(vv), v))
## failed initially in ALTREP


## check that "TRUE", "FALSE" work in order, sort.int
order(1:3, decreasing = "TRUE")
order(1:3, decreasing = "FALSE")
sort.int(1:3, decreasing = "TRUE")
sort.int(1:3, decreasing = "FALSE")
## failed initially in ALTREP

## this failed until 3.5.x
c1 <- c(1,1,2,2)
c2 <- as.Date(c("2010-1-1", "2011-1-1", "2013-1-1", "2012-1-1"))
order(c1, c2, decreasing = c(TRUE, FALSE), method="radix")


## check sort argument combinations
sort(1:3, decreasing = TRUE, na.last = NA)
sort(1:3, decreasing = TRUE, na.last = TRUE)
sort(1:3, decreasing = TRUE, na.last = FALSE)
sort(1:3, decreasing = FALSE, na.last = NA)
sort(1:3, decreasing = FALSE, na.last = TRUE)
sort(1:3, decreasing = FALSE, na.last = FALSE)

## match.arg()s 'choices' evaluation, PR#17401
f <- function(x = y) {
    y <- c("a", "b")
    match.arg(x)
}
stopifnot(identical(f(), "a"))
## failed in R <= 3.4.x


## getOption(op, def) -- where 'def' is missing (passed down):
getO <- function(op, def) getOption(op, def)
stopifnot(is.null(getO("foobar")))
## failed for a few days in R-devel, when using MD's proposal of PR#17394,
## notably "killing"  parallelMap::getParallelOptions()


## Mantel-Haenszel test in "large" case, PR#17383:
set.seed(101); n <- 500000
aTab <- table(
    educ = factor(sample(1:3, replace=TRUE, size=n)),
    score= factor(sample(1:5, replace=TRUE, size=n)),
    sex  = sample(c("M","F"), replace=TRUE, size=n))
(MT <- mantelhaen.test(aTab))
stopifnot(all.equal(
    lapply(MT[1:3], unname),
    list(statistic = 9.285642, parameter = 8, p.value = 0.3187756), tol = 6e-6))
## gave integer overflow and error in R <= 3.4.x


## check for incorect inlining of named logicals
foo <- compiler::cmpfun(function() c("bar" = TRUE),
                        options = list(optimize = 3))
stopifnot(identical(names(foo()), "bar"))
foo <- compiler::cmpfun(function() c("bar" = FALSE),
                        options = list(optimize = 3))
stopifnot(identical(names(foo()), "bar"))
## Failed after changes to use isTRUE/isFALSE instead of identical in r74403.


## check that reverse sort is stable
x <- sort(c(1, 1, 3))
stopifnot(identical(sort.list(x, decreasing=TRUE), as.integer(c(3, 1, 2))))
stopifnot(identical(order(x, decreasing=TRUE), as.integer(c(3, 1, 2))))
## was incorrect with wrapper optimization (reported by Suharto Anggono)


## dump() & dput() where influenced by  "deparse.max.lines" option
op <- options(deparse.max.lines=NULL) # here
oNam <- "simplify2array" # (base function which is not very small)
fn <- get(oNam)
ffn <- format(fn)
dp.1 <- capture.output(dput(fn))
dump(oNam, textConnection("du.1", "w"))
stopifnot(length(ffn) > 3, identical(dp.1, ffn), identical(du.1[-1], dp.1))
options(deparse.max.lines = 2) ## "truncate heavily"
dp.2 <- capture.output(dput(fn))
dump(oNam, textConnection("du.2", "w"))
stopifnot(identical(dp.2, dp.1),
          identical(du.2, du.1))
options(op); rm(du.1, du.2) # connections
writeLines(tail(dp.2))
## dp.2 and du.2  where heavily truncated in R <= 3.4.4, ending  "  ..."


## optim() with "trivial bounds"
flb <- function(x) { p <- length(x); sum(c(1, rep(4, p-1)) * (x - c(1, x[-p])^2)^2) }
o1 <- optim(rep(3, 5), flb)
o2 <- optim(rep(3, 5), flb, lower = rep(-Inf, 5))
stopifnot(all.equal(o1,o2))
## the 2nd optim() call gave a warning and switched to "L-BFGS-B" in R <= 3.5.0


## Check that call matching doesn't mutate input
cl <- as.call(list(quote(x[0])))
cl[[1]][[3]] <- 1
v <- .Internal(match.call(function(x) NULL, cl, TRUE, .GlobalEnv))
cl[[1]][[3]] <- 2
stopifnot(v[[1]][[3]] == 1)
## initial patch proposal to reduce duplicating failed on this


## simulate.lm(<glm gaussian, non-default-link>), PR#17415
set.seed(7); y <- rnorm(n = 1000, mean = 10, sd = sqrt(10))
fmglm <- glm(y ~ 1, family = gaussian(link = "log"))
dv <- apply(s <- simulate(fmglm, 99, seed=1), 2, var) - var(y)
stopifnot(abs(dv) < 1.14, abs(mean(dv)) < .07)
## failed in R <= 3.5.0 (had simulated variances ~ 0.1)


## unlist() failed for nested lists of empty lists:
isLF <- function(x) .Internal(islistfactor(x, recursive=TRUE))
ex <- list(x0 = list()
         , x1 = list(list())
         , x12 = list(list(), list())
         , x12. = list(list(), expression(list()))
         , x2 = list(list(list(), list())) # <-- Steven Nydick's example
         , x212 = list(list(list(), list(list())))
         , x222 = list(list(list(list()), list(list())))
)
(exis <- vapply(ex, isLF, NA))
ue <- lapply(ex, unlist)# gave errors in R <= 3.3.x  but not 3.{4.x,5.0}
stopifnot(exprs = {
    !any(exis)
    identical(names(ue), names(ex))
    vapply(ue[names(ue) != "x12."], is.null, NA)
})


## qr.coef(qr(<all 0, w/ colnames>))
qx <- qr(x <- matrix(0, 10, 2, dimnames = list(NULL, paste0("x", 1:2))))
qc <- qr.coef(qx, x[,1])
stopifnot(identical(qc, c(x1 = NA_real_, x2 = NA_real_)))
## qr.coef() gave  Error ...: object 'pivotted' not found | in R <= 3.5.0


## unlist(<factor-leaves>)
x <- list(list(v=factor("a")))
y <- list(data.frame(v=factor("a")))
x. <- list(list(factor("a")), list(factor(LETTERS[2:4])), factor("lol"))
fN <- factor(LETTERS[c(2:4,30)])
xN <- list(list(factor("a")), list(list(fN)), L=factor("lol"))
stopifnot(exprs = {
    .valid.factor(ux <- unlist(x))
    identical(ux, unlist(y))
    identical(ux, as.factor(c(v="a")))
    .valid.factor(ux. <- unlist(x.))
    .valid.factor(uxN <- unlist(xN))
    identical(levels(ux.), c("a", "B", "C", "D", "lol"))
    identical(levels      (uxN), levels(ux.))
    identical(as.character(uxN), levels(ux.)[c(1:4,11L,5L)])
})
## gave invalid factor()s [if at all]


## printCoefMat()  w/ unusual arguments
cm <- matrix(c(9.2, 2.5, 3.6, 0.00031), 1, 4,
            dimnames = list("beta", c("Estimate", "Std.Err", "Z value", "Pr(>z)")))
cc <- capture.output(printCoefmat(cm))
stopifnot(grepl(" [*]{3}$", cc[2]),
          identical(cc, capture.output(
                     printCoefmat(cm, right=TRUE))))
## gave Error: 'formal argument "right" matched by multiple actual arguments'


## print.noquote() w/ unusual argument -- inspite of user error, be forgiving:
print(structure("foo bar", class="noquote"), quote=FALSE)
## gave Error: 'formal argument "quote" matched by multiple actual arguments'


## agrep(".|.", ch, fixed=FALSE)
chvec <- c(".BCD", "yz", "AB", "wyz")
patt <- "ABC|xyz"
stopifnot(identical(c(list(0L[0]), rep(list(1:4), 2)),
    lapply(0:2, function(m) agrep(patt, chvec, max.distance=m, fixed=FALSE))
))
## all three were empty in R <= 3.5.0


## str(<invalid>)
typeof(nn <- c(0xc4, 0x88, 0xa9, 0x02))
cc <- ch <- rawToChar(as.raw(nn))
str(ch)# worked already
nchar(cc, type="bytes")# 4, but  nchar(cc)  gives  "invalid multibyte string"
Encoding(cc) <- "UTF-8" # << makes it invalid for strtrim(.)!
as.octmode(as.integer(nn))
str(cc)
## In R <= 3.5.0, [strtrim() & nchar()] gave invalid multibyte string at '<a9>\002"'


## multivariate <empty model> lm():
y <- matrix(cos(1:(7*5)), 7,5) # <- multivariate y
lms <- list(m0 = lm(y ~ 0), m1 = lm(y ~ 1), m2 = lm(y ~ exp(y[,1]^2)))
dcf <- sapply(lms, function(fm) dim(coef(fm)))
stopifnot(dcf[1,] == 0:2, dcf[2,] == 5)
## coef(lm(y ~ 0)) had 3 instead of 5 columns in R <= 3.5.1
proc.time() - .pt; .pt <- proc.time()


## confint(<mlm>)
n <- 20
set.seed(1234)
datf <- local({
    x1 <- rnorm(n)
    x2 <- x1^2 + rnorm(n)
    y1 <- 100*x1 + 20*x2 + rnorm(n)
    data.frame(x1=x1, x2=x2, y1=y1, y2 = y1 + 10*x1 + 50*x2 + rnorm(n))
})
fitm <- lm(cbind(y1,y2) ~ x1 + x2, data=datf)
zapsmall(CI <- confint(fitm))
ciT <- cbind(c(-0.98031,  99.2304, 19.6859, -0.72741, 109.354, 69.4632),
             c( 0.00984, 100.179,  20.1709,  0.60374, 110.63,  70.1152))
dimnames(ciT) <- dimnames(CI)
## also checking confint(*, parm=*) :
pL <- list(c(1,3:4), rownames(CI)[c(6,2)], 1)
ciL  <- lapply(pL, function(ii) confint(fitm, parm=ii))
ciTL <- lapply(pL, function(ii) ciT[ii, , drop=FALSE])
stopifnot(exprs = {
    all.equal(ciT, CI,  tolerance = 4e-6)
    all.equal(ciL, ciTL,tolerance = 8e-6)
})
## confint(<mlm>) gave an empty matrix in R <= 3.5.1
## For an *empty* mlm :
mlm0 <- lm(cbind(y1,y2) ~ 0, datf)
stopifnot(identical(confint(mlm0),
                    matrix(numeric(0), 0L, 2L, dimnames = list(NULL, c("2.5 %", "97.5 %")))))
## failed inside vcov.mlm() because summary.lm()$cov.unscaled was NULL

## cooks.distance.(<mlm>), rstandard(<mlm>) :
fm1 <- lm(y1 ~ x1 + x2, data=datf)
fm2 <- lm(y2 ~ x1 + x2, data=datf)
stopifnot(exprs = {
    all.equal(cooks.distance(fitm),
              cbind(y1 = cooks.distance(fm1),
                    y2 = cooks.distance(fm2)))
    all.equal(rstandard(fitm),
              cbind(y1 = rstandard(fm1),
                    y2 = rstandard(fm2)))
    all.equal(rstudent(fitm),
              cbind(y1 = rstudent(fm1),
                    y2 = rstudent(fm2)))
})
## were silently wrong in R <= 3.5.1


## kruskal.test(<non-numeric g>), PR#16719
data(mtcars)
mtcars$type <- rep(letters[1:2], c(16, 16))
kruskal.test(mpg ~ type, mtcars)
## gave 'Error: all group levels must be finite'


## Multivariate lm() with matrix offset, PR#17407
ss <- list(s1 = summary(fm1 <- lm(cbind(mpg,qsec) ~ 1, data=mtcars, offset=cbind(wt,wt*2))),
           s2 = summary(fm2 <- lm(cbind(mpg,qsec) ~ offset(cbind(wt,wt*2)), data=mtcars)))
## drop "call" and "terms" parts which differ; rest must match:
ss[] <- lapply(ss, function(s) lapply(s, function(R) R[setdiff(names(R), c("call","terms"))]))
stopifnot(all.equal(ss[["s1"]], ss[["s2"]], tolerance = 1e-15))
## lm() calls gave error 'number of offsets is 64, should equal 32 ...' in R <= 3.5.1


## print.data.frame(<non-small>)
USJ   <- USJudgeRatings
USJe6 <- USJudgeRatings[rep_len(seq_len(nrow(USJ)), 1e6),]
op <- options(max.print=500)
system.time(r1 <- print(USJ))
system.time(r2 <- print(USJe6))# was > 12 sec in R <= 3.5.1, now typically 0.01
                               # because the whole data frame was formatted.
## Now the timing ratio between r1 & r2 print()ing is typically in [1,2]
system.time(r3 <- print(USJe6, row.names=FALSE))
out <- capture.output(print(USJe6, max = 600)) # max > getOption("max.print")
stopifnot(exprs = {
    identical(r1, USJ  )# print() must return its arg
    identical(r2, USJe6)
    identical(r3, USJe6)
    length(out) == 52
    grepl("CALLAHAN", out[51], fixed=TRUE)
    identical(2L, grep("omitted", out[51:52], fixed=TRUE))
})
options(op); rm(USJe6)# reset


## hist.default() in rare cases
hh <- hist(seq(1e6, 2e6, by=20), plot=FALSE)
hd <- hh$density*1e6
stopifnot(0.999 <= hd, hd <= 1.001)
## in R <= 3.5.1: warning 'In n * h : NAs produced by integer overflow' and then NA's


## some things broken by sort.int optimization for sorted integer vectors
sort.int(integer(0))  ## would segfault with barrier testing
stopifnot(identical(sort.int(NA_integer_), integer(0)))


## attribute handling in the fastpass was not quite right
x <- sort.int(c(1,2))
dim(x) <- 2
dimnames(x) <- list(c("a", "b"))
stopifnot(! is.null(names(sort.int(x))))

## is.unsorted fastpass incorrectly returned TRUE when constant-valued x was sorted descending
x <- c(1, 1, 1)
xs <- sort(x, decreasing = TRUE)
stopifnot(!is.unsorted(xs, strictly = FALSE)) ## is.unsorted should be FALSE
y <- as.integer(x)
ys <- sort(x, decreasing = TRUE)
stopifnot(!is.unsorted(ys, strictly = FALSE))

## match() with length one x and POSIXlt table (PR#17459):
d <- as.POSIXlt("2018-01-01")
match(0, d)
## Gave a segfault in R < 3.6.0.
proc.time() - .pt; .pt <- proc.time()


## as(1L, "double") - PR#17457
stopifnot(exprs = {
    identical(as(1L,   "double"), 1.) # new
    identical(new("double"), double())
  ## 1. "double" is quite the same as "numeric" :
    local({
        i1 <- 1L; as(i1, "numeric") <- pi
        i2 <- 1L; as(i2, "double" ) <- pi
        identical(i1, i2)
    })
    validObject(Dbl <- getClass("double"))
    validObject(Num <- getClass("numeric"))
    c("double", "numeric") %in% extends(Dbl)
    setdiff(names(Num@subclasses),
            names(Dbl@subclasses) -> dblSub) == "double"
    "integer" %in% dblSub
  ## 2. These all remain as they were in R <= 3.5.x , the first one important for back-compatibility:
    identical(1:2, local({
        myN <- setClass("myN", contains="numeric", slots = c(truly = "numeric"))
        myN(log(1:2), truly = 1:2) })@truly)
    removeClass("myN")
    identical(as(1L,  "numeric"), 1L) # << disputable, but hard to change w/o changing myN() behavior
    identical(as(TRUE, "double"), 1.)
    identical(as(TRUE,"numeric"), 1.)
    !is(TRUE, "numeric") # "logical" should _not_ be a subclass of "numeric"
    ## We agree these should not change :
    typeof(1.0) == "double"  &  typeof(1L) == "integer"
    class (1.0) == "numeric" &  class (1L) == "integer"
    mode  (1.0) == "numeric" &  mode  (1L) == "numeric"
})
## as(*, "double") now gives what was promised


## next(n) for largish n
stopifnot(exprs = {
    nextn(214e7 ) == 2^31
    nextn(2^32+1) == 4299816960
    identical(nextn(NULL), integer())
})
## nextn(214e7) hang in infinite loop; nextn(<large>) gave NA  in R <= 3.5.1


## More strictness in '&&' and '||' :
Sys.getenv("_R_CHECK_LENGTH_1_LOGIC2_", unset=NA) -> oEV
Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = "warn") # only warn
tools::assertWarning(1 && 0:1)
Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = TRUE) # => error (when triggered)
tools::assertError(0 || 0:1)
if(is.na(oEV)) { # (by default)
    Sys.unsetenv ("_R_CHECK_LENGTH_1_LOGIC2_")
    2 && 0:1 # should not even warn
} else Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = oEV)


## polym() in "vector" case PR#17474
fm <- lm(Petal.Length ~ poly(cbind(Petal.Width, Sepal.Length), 2),
         data = iris)
p1 <- predict(fm, newdata = data.frame(Petal.Width = 1, Sepal.Length = 1))
stopifnot(all.equal(p1, c("1" = 4.70107678)))
## predict() calling polym() failed in R <= 3.5.1


## sample.int(<fractional>, k, replace=TRUE) :
(tt <- table(sample.int(2.9, 1e6, replace=TRUE)))
stopifnot(length(tt) == 2)
## did "fractionally" sample '3' as well in 3.0.0 <= R <= 3.5.1


## lm.influence() for simple regression through 0:
x <- 1:7
y <- c(1.1, 1.9, 2.8, 4, 4.9, 6.1, 7)
f0 <- lm(y ~ 0+x)
mi <- lm.influence(f0)
stopifnot(identical(dim(cf <- mi$coefficients), c(7L, 1L)),
          all.equal(range(cf), c(-0.0042857143, 0.0072527473)))
## gave an error for a few days in R-devel


## cut(<constant 0>), PR#16802
c0 <- cut(rep(0L, 7), breaks = 3)
stopifnot(is.factor(c0), length(c0) == 7, length(unique(c0)) == 1)
## cut() gave error  _'breaks' are not unique_  in R <= 3.5.1


## need to record OutDec in deferred string conversions (reported by
## Michael Sannella).
op <- options(scipen=-5, OutDec=",")
xx <- as.character(123.456)
options(op)
stopifnot(identical(xx, "1,23456e+02"))


## parseRd() and Rd2HTML() with some \Sexpr{} in *.Rd:
x <- tools::Rd_db("base")
## Now check that \Sexpr{}  "installed" correctly:
of <- textConnection("DThtml", "w")
tools::Rd2HTML(x$DateTimeClasses.Rd, out = of, stages = "install"); close(of)
(iLeap <- grep("leap seconds", DThtml)[[1]])
stopifnot(exprs = {
        grepl("[0-9]+ days",     DThtml[iLeap+ 1])
    any(grepl("20[1-9][0-9]-01", DThtml[iLeap+ 2:4]))
})



## if( "length > 1" )  buglet in plot.data.frame()
Sys.getenv("_R_CHECK_LENGTH_1_CONDITION_", unset=NA) -> oEV
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
plot(data.frame(.leap.seconds))
if(!is.na(oEV)) Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = oEV)
## gave Error in ... the condition has length > 1,  in R <= 3.5.1


## duplicated(<dataframe with 'f' col>) -- PR#17485
d <- data.frame(f=gl(3,5), i=1:3)
stopifnot(exprs = {
    identical(which(duplicated(d)), c(4:5, 9:10, 14:15))
    identical(anyDuplicated(d), 4L)
    identical(anyDuplicated(d[1:3,]), 0L)
})
## gave error from do.call(Map, ..) as Map()'s first arg. is 'f'


## print.POSIX[cl]t() - not correctly obeying "max.print" option
op <- options(max.print = 50, width = 85)
cc <- capture.output(print(dt <- .POSIXct(154e7 + (0:200)*60)))
c2 <- capture.output(print(dt, max = 6))
writeLines(tail(cc, 4))
writeLines(c2)
stopifnot(expr = {
    grepl("omitted 151 entries", tail(cc, 1))
                  !anyDuplicated(tail(cc, 2))
    grepl("omitted 195 entries", tail(c2, 1))
}); options(op)
## the omission had been reported twice because of a typo in R <= 3.5.1


## <data.frame>[ <empty>, ] <- v                    should be a no-op and
## <data.frame>[ <empty>, <existing column>] <- v   a no-op, too
df <- d0 <- data.frame(i=1:6, p=pi)
n <- nrow(df)
as1NA <- function(x) `is.na<-`(rep_len(unlist(x), 1L), TRUE)
for(i in list(FALSE, integer(), -seq_len(n)))
  for(value in list(numeric(), 7, "foo", list(1))) {
    df[i ,  ] <- value
    df[i , 1] <- value # had failed after svn c75474
    stopifnot(identical(df, d0))
    ## "expand": new column created even for empty <i>; some packages rely on this
    df[i, "new"] <- value ## -> produces new column of .. NA
    stopifnot(identical(df[,"new"], rep(as1NA(value), n)))
    df <- d0
  }
## gave error in R <= 3.5.1
df[7:12,] <- d0 + 1L
stopifnot(exprs = {
    is.data.frame(df)
    identical(dim(df), c(12L, 2L))
    identical(df[1:6,], d0)
})
## had failed after svn c75474


## Check that active binding uses primitive quote() and doesn't pick
## up `quote` binding on the search path
quote <- function(...) stop("shouldn't be called")
if (exists("foo", inherits = FALSE)) rm(foo)
makeActiveBinding("foo", identity, environment())
x <- (foo <- "foo")
stopifnot(identical(x, "foo"))
rm(quote, foo, x)


## .format.zeros() when zero.print is "wide":
x <- c(outer(c(1,3,6),10^(-5:0)))
(fx <- formatC(x))
stopifnot(identical(nchar(fx), rep(c(5L, 6:3, 1L), each=3)))
x3 <- round(x, 3)
tools::assertWarning(
  fz1. <- formatC(x3,          zero.print="< 0.001",   replace.zero=FALSE))# old default
 (fz1  <- formatC(x3,          zero.print="< 0.001"))#,replace.zero=TRUE  :  new default
 (fzw7 <- formatC(x3, width=7, zero.print="< 0.001"))
for(fz in list(fz1, fz1., fzw7)) stopifnot(identical(grepl("<", fz), x3 == 0))
## fz1, fzw7 gave error (for 2 bugs) in R <= 3.5.x


## Attempting to modify an object in a locked binding could succeed
## before signaling an error:
foo <- function() {
    zero <- 0           ## to fool constant folding
    x <- 1 + zero       ## value of 'x' has one reference
    lockBinding("x", environment())
    tryCatch(x[1] <- 2, ## would modify the value, then signal an error
             error = function(e) NULL)
    stopifnot(identical(x, 1))
}
foo()


## formalArgs()  should conform to names(formals()) also in looking up fun: PR#17499
by <- function(a, b, c) "Bye!" # Overwrites base::by, as an example
foo <- function() {
  f1 <- function(a, ...) {}
  list(nf = names(formals("f1")),
       fA = formalArgs   ("f1"))
}
stopifnot(exprs = {
    identical(names(formals("by")), letters[1:3])
    identical(formalArgs   ("by") , letters[1:3])
    { r <- foo(); identical(r$nf, r$fA) }
})
## gave "wrong" result and error in R <= 3.5.x



## Subassigning multiple new data.frame columns (with specified row), PR#15362, 17504
z0 <- z1 <- data.frame(a=1, s=1)
z0[2, c("a","r","e")] <- data.frame(a=1, r=8, e=9)
z1[2, "r"] <- data.frame(r=8)
x <- x0 <- data.frame(a=1:3, s=1:3)
x[2, 3:4] <- data.frame(r=8, e=9)
stopifnot(exprs = {
    identical(z0, data.frame(a = c(1, 1), s = c(1, NA), r = c(NA, 8), e = c(NA, 9)))
    identical(z1, data.frame(a = c(1,NA), s = c(1, NA), r = c(NA, 8)))
    identical(x, cbind(x0,
                       data.frame(r = c(NA, 8, NA), e = c(NA, 9, NA))))
})
d0 <- d1 <- d2 <- d3 <- d4 <- d5 <- d6 <- d7 <- data.frame(n=1:4)
##
d0[, 2] <- c2 <- 5:8
d0[, 3] <- c3 <- 9:12
d1[, 2:3] <- list(c2, c3)
d2[  2:3] <- list(c2, c3)
d3[TRUE, 2] <- c2 ; d3[TRUE, 3] <- c3
d4[TRUE, 2:3] <- list(c2, c3)
d5[1:4,  2:3] <- list(c2, c3)
d6[TRUE, 1:2] <- list(c2, c3)
d7[    , 1:2] <- list(c2, c3)
stopifnot(exprs = {
    identical(d0, d1)
    identical(d0, d2)
    identical(d0, d3)
    identical(d0, d4)
    identical(d0, d5)
    ##
    identical(d6, d7)
    identical(d6, structure(list(n = c2, V2 = c3),
                            row.names = c(NA, -4L), class = "data.frame"))
})
## d4, d5 --> 'Error in `*tmp*`[[j]] : subscript out of bounds'
## d6     --> 'Error in x[[j]] <- `*vtmp*` :
##				more elements supplied than there are to replace
## in R <= 3.5.1


## str() now even works with invalid S4  objects:
## this needs Matrix loaded to be an S4 generic
if(requireNamespace('Matrix', lib.loc = .Library)) {
moS <- mo <- findMethods("isSymmetric")
attr(mo, "arguments") <- NULL
print(validObject(mo, TRUE)) # shows what's wrong
tools::assertError(capture.output( mo ))
op <- options(warn = 1)# warning:
str(mo, max.level = 2)
options(op)# revert
## in R <= 3.5.x, str() gave error instead of the warning
}


## seq.default() w/ integer overflow in border cases: -- PR#17497, Suharto Anggono
stopifnot(is.integer(iMax <- .Machine$integer.max), iMax == 2^31-1,
          is.integer(iM2 <- iMax-1L), # = 2^31 - 2
          (t30 <- 1073741824L) == 2^30 ,
          is.integer(i3t30 <- c(-t30, 0L, t30)))
for(seq in c(seq, seq.int)) # seq() -> seq.default() to behave as seq.int() :
  stopifnot(exprs = {
    seq(iM2, length=2L) == iM2:(iM2+1L) # overflow warning and NA
    seq(iM2, length=3L) == iM2:(iM2+2 ) # Error in if (from == to) ....
              seq(-t30, t30, length=3) == i3t30 # overflow warning and NA
    ## Next two ok for the "seq.cumsum-patch" (for "seq.double-patch", give "double"):
    identical(seq(-t30, t30, length=3L),  i3t30)# Error in if(is.integer(del <- to - from)
    identical(seq(-t30, t30, t30)      ,  i3t30)# Error .. invalid '(to-from)/by'+NA warn.
  })
## each of these gave integer overflows  errors  or  NA's + warning in  R <= 3.5.x
stopifnot(identical(7:10, seq.default(7L, along.with = 4:1) ))
## errored for almost a day after r76062


## seq.int(*, by=<int.>, length = n) for non-integer 'from' or 'to'
stopifnot(exprs = {
    identical(seq.int(from = 1.5, by = 2, length = 3),
              s <- seq(from = 1.5, by = 2, length = 3))
    s == c(1.5, 3.5, 5.5)
    identical(seq.int(to = -0.1, by = -2, length = 2),
              s <- seq(to = -0.1, by = -2, length = 2))
    all.equal(s, c(1.9, -0.1))
    identical(seq.int(to = pi, by = 0, length = 1), pi)
})
## returned integer sequences in all R versions <= 3.5.1


## Check for modififation of arguments
## Issue originally reported by Lukas Stadler
x <- 1+0
stopifnot(x + (x[] <- 2) == 3)
f <- compiler::cmpfun(function(x) { x <- x + 0; x + (x[] <- 2) })
stopifnot(f(1) == 3)

x <- 1+0
stopifnot(log(x, x[] <- 2) == 0)
f <- compiler::cmpfun(function(x) { x <- x + 0; log(x, x[] <- 2)})
stopifnot(f(1) == 0)

f <- function() x + (x[] <<- 2)
x <- 1 + 0; stopifnot(f() == 3)
fc <- compiler::cmpfun(f)
x <- 1 + 0; stopifnot(fc() == 3)

f <- function() x[{x[2] <<- 3; 1}] <<- 2
fc <- compiler::cmpfun(f)
x <- c(1,2); f(); stopifnot(x[2] == 2)
x <- c(1,2); fc(); stopifnot(x[2] == 2)

x <- 1+0
stopifnot(c(x, x[] <- 2)[[1]] == 1)
f <- compiler::cmpfun(function(x) { x <- x + 0; c(x, x[] <- 2)})
stopifnot(f(1)[[1]] == 1)

x <- c(1,2)
x[{x[2] <- 3; 1}] <- 2
stopifnot(x[2] == 2)
f <- compiler::cmpfun(function(a,b) { x <- c(a, b); x[{x[2] <- 3; 1}] <- 2; x})
f(1, 2)
stopifnot(f(1, 2) == 2)

m <- matrix(1:4, 2)
i <- (1:2) + 0
stopifnot(m[i, {i[] <- 2; 1}][1] == 1)
f <- compiler::cmpfun(function(i) { i <- i + 0; m[i, {i[] <- 2; 1}]})
stopifnot(f(1:2)[1] == 1)

m <- matrix(1:4, 2)
eval(compiler::compile(quote(m[1,1])))
stopifnot(max(.Internal(named(m)), .Internal(refcnt(m))) == 1)

ma <- .Internal(address(m))
eval(compiler::compile(quote(m[1,1] <- 2L)))
stopifnot(identical(.Internal(address(m)), ma))

a <- array(1:8, rep(2, 3))
eval(compiler::compile(quote(a[1,1,1])))
stopifnot(max(.Internal(named(a)), .Internal(refcnt(a))) == 1)

aa <- .Internal(address(a))
eval(compiler::compile(quote(a[1,1,1] <- 2L)))
stopifnot(identical(.Internal(address(a)), aa))

m <- matrix(1:4, 2)
i <- (1:2) + 0
stopifnot(m[i, {i[] <- 2; 1}][1] == 1)
f <- compiler::cmpfun(function(i) { i <- i + 0; m[i, {i[] <- 2; 1}]})
stopifnot(f(1:2)[1] == 1)

a <- array(1:8, rep(2, 3))
i <- (1:2) + 0
stopifnot(a[i, {i[] <- 2; 1}, 1][1] == 1)
f <- compiler::cmpfun(function(i) { i <- i + 0; a[i, {i[] <- 2; 1}, 1]})
stopifnot(f(1:2)[1] == 1)

i <- (1:2) + 0
stopifnot(a[i, {i[] <- 2; 1}, 1][1] == 1)
f <- compiler::cmpfun(function(i) { i <- i + 0; a[1, i, {i[] <- 2; 1}]})
stopifnot(f(1:2)[1] == 1)

x <- 1 + 0
stopifnot(identical(rep(x, {x[] <- 2; 2}), rep(1, 2)))
x <- 1 + 0
v <- eval(compiler::compile(quote(rep(x, {x[] <- 2; 2}))))
stopifnot(identical(v, rep(1, 2)))

x <- 1 + 0
stopifnot(round(x, {x[] <- 2; 0}) == 1)
x <- 1 + 0
v <- eval(compiler::compile(quote(round(x, {x[] <- 2; 0}))))
stopifnot(v == 1)

f <- function() {
    x <- numeric(1)
    y <- 0
    rm("y")
    makeActiveBinding("y", function() { x[] <<- 1; 0}, environment())
    x + y
}
stopifnot(f() == 0)
stopifnot(compiler::cmpfun(f)() == 0)

f <- function(y = {x[] <- 1; 0}) { x <- numeric(1); x + y }
stopifnot(f() == 0)
stopifnot(compiler::cmpfun(f)() == 0)


## This failed under REFCNT:
for (i in 1:2) { if (i == 1) { x <- i; rm(i) }}
stopifnot(x == 1)


## gamma & lgamma should not warn for correct limit cases:
stopifnot(exprs = {
    lgamma(0:-10) == Inf
    gamma(-180.5) == 0
    gamma(c(200,Inf)) == Inf
    lgamma(c(10^(306:310), Inf)) == Inf
})
## had  "Warning message:  value out of range in 'lgamma' "  for ever


## sub() with non-ASCII replacement failed to set encodings (PR#17509):
x <- c("a", "b")
x <- sub("a", "\u00e4", x)
stopifnot(Encoding(x)[1L] == "UTF-8")
x <- sub("b", "\u00f6", x)
stopifnot(Encoding(x)[2L] == "UTF-8")
## [1] has been "unknown" in R <= 3.5.x


## formula(model.frame()) -- R-devel report by Bill Dunlap
d <- data.frame(A = log(1:6), B = LETTERS[1:6], C = 1/(1:6), D = letters[6:1], Y = 1:6)
m0 <- model.frame(Y ~ A*B, data=d)
stopifnot(exprs = {
    DF2formula(m0) == (Y ~ A+B) # the previous formula(.) behavior
       formula(m0) == (Y ~ A*B)
})
## formula(.)  gave  Y ~ A + B  in R <= 3.5.x


## These used to fail (PR17514) in a NAMED build but not with REFCNT:
L <- matrix(list( c(0) ), 2, 1)
L[[2]][1] <- 11
stopifnot(L[[1]] == 0)
L <- matrix(list( c(0) ), 2, 1, byrow = TRUE)
L[[2]][1] <- 11
stopifnot(L[[1]] == 0)


## ar.ols() - PR#17517
ar_ols <- ar.ols(lynx)
stopifnot(exprs = {
    is.list(pa <- predict(ar_ols, n.ahead = 2))# must *not* warn
    all.equal(ar_ols$var.pred, 592392.12774) # not a matrix
})
## .$var.pred had been a 1x1 matrix in R <= 3.5.2


## check that parse lines are properly initialized in the parser
d <- getParseData(parse(text="{;}", keep.source=TRUE))
l <- d[ d[,"token"] == "exprlist", "line1" ]
stopifnot(identical(l, 1L))
## failed in 3.5 and earlier


## check that NA is treated as non-existent file (not file named "NA")
tools::assertError  (normalizePath(c(NA_character_,getwd()), mustWork=TRUE))
tools::assertWarning(normalizePath(c(NA_character_,getwd()), mustWork=NA))
stopifnot(identical (normalizePath(c(NA_character_,getwd()), mustWork=FALSE)[1],
                     NA_character_))
stopifnot(identical(unname(file.access(NA_character_)), -1L))
## NA treated as error
tools::assertError(file.edit(NA_character_))
tools::assertError(file(NA_character_))


## strtoi("") :
stopifnot(is.na(strtoi("")),
          is.na(strtoi("", 2L)))
## was platform dependent [libC strtol()] in R <= 3.5.x


## formula.data.frame() thinko at modularization [r75911]:
f <- function(df) {
    stopifnot(is.data.frame(df))
    d <- 4
    f2(formula(df))
}
f2 <- function(form) eval(quote(d), envir = environment(form))
rf <- f(data.frame(x=1, f="b")) ## gave error inside f2() in R-devel
stopifnot(identical(rf, 4))
## as after 75911 a wrong parent.frame() was used.


## format(.) when there's no method gives better message:
ee <- tryCid(format(.Internal(bodyCode(ls))))
stopifnot(exprs = {
    conditionCall(ee)[[1]] == quote(format.default)
    grepl("no format() method", conditionMessage(ee), fixed=TRUE)
})
## signalled from long .Internal(...) call + "must be atomic" in R <= 3.5.x


## writeLines(readLines(F), F)  -- PR#17528
tf <- tempfile("writeL_test")
writeLines("1\n2\n3", tf)
c123 <- paste(1:3)
stopifnot(identical(readLines(tf), c123))
writeLines(readLines(tf), tf)
stopifnot(identical(readLines(tf), c123))
## writeLines had opened the output for writing before readLines() read it


## max.col(<empty>)
stopifnot(identical(NA_integer_, max.col(matrix(,1,0))))
## gave 1 in R <= 3.5.x


## model.matrix() should warn on invalid 'contrasts.arg'
## suggested by Ben Bolker on R-devel list, Feb 20, 2019
data(warpbreaks)
   mf1 <- model.matrix(~tension, data=warpbreaks) # default
tools::assertWarning(
   mf2 <- model.matrix(~tension, data=warpbreaks, contrasts.arg = "contr.sum") )# wrong
tools::assertWarning(
   mf3 <- model.matrix(~tension, data=warpbreaks, contrasts.arg = contr.sum) )  # wrong
   mf4 <- model.matrix(~tension, data=warpbreaks, contrasts.arg = list(tension=contr.sum))
stopifnot(exprs = {
    identical(mf1, mf2)
    identical(mf1, mf3)
    ## and mf4 has sum contrasts :
    is.matrix(C <- attr(mf4, "contrasts")$tension)
    identical(dim(C), 3:2)
    all.equal(unname(C), rbind(diag(2), -1))
})
## gave no warnings but same results in R <= 3.5.0


## axTicks() should zap "almost zero" to zero, PR#17534
## (caused by non-exact floating point arithmetic -- (platform dependently!)
plot(c(-0.1, 0.2), axes=FALSE, ann=FALSE)
(a2 <- axTicks(2)) # -0.10 -0.05  0.00  0.05  0.10  0.15  0.20
axis(2, at = a2) # was ugly
stopifnot(exprs = {
    a2[3] == 0 # exactly
    all.equal(a2, (-2:4)/20, tol=1e-14) # closely
})
## a2[3] was 1.38778e-17  on typical platforms in R <= 3.5.x


## isSymmetric(<1x1-matrix>) and <0x0 matrix>  with dimnames
stopifnot(exprs = {
    ! isSymmetric(matrix(0, dimnames = list("A","b"))) # *non*-symmetric dimnames
      isSymmetric(matrix(0, dimnames = list("A","b")), check.attributes=FALSE) # dimn. not checked
    ## isSymmetric() gave TRUE wrongly in R versions 3.4.0 -- 3.5.x
    ! isSymmetric(matrix(1, dimnames = list("A", NULL)))
    ! isSymmetric(matrix(1, dimnames = list(NULL, "A")))
      isSymmetric(matrix(1, dimnames = list(NULL, "A")), check.attributes=FALSE)
      isSymmetric(matrix(1))
      isSymmetric(matrix(1,  dimnames = list("a", "a")))
      isSymmetric(matrix(1,  dimnames = list(NULL, NULL)))
      isSymmetric(matrix(,0,0, dimnames=list(NULL, NULL)))
      isSymmetric(matrix(,0,0))
})


## bxp() did not signal anything about duplicate actual arguments:
set.seed(3); bx.p <- boxplot(split(rt(100, 4), gl(5, 20)), plot=FALSE)
tools::assertWarning(bxp(bx.p, ylab = "Y LAB", ylab = "two"), verbose=TRUE)
w <- tryCatch(bxp(bx.p, ylab = "Y LAB", ylab = "two", xlab = "i", xlab = "INDEX"),
              warning = conditionMessage)
stopifnot(is.character(w), grepl('ylab = "two"', w), grepl('xlab = "INDEX"', w))


## reformulate() bug  PR#17359
(form <- reformulate(c("u", "log(x)"), response = "log(y)"))
stopifnot(identical(form, log(y) ~ u + log(x)))
## had *symbol*  `log(y)`  instead of call in R <= 3.5.1
newf <- function(terms, resp)
    list(e   = environment(),
         form= reformulate(terms, resp))
ef <- newf("x", "log(y)")
stopifnot( identical(ef$e, environment(ef$form)),
	  !identical(ef$e, .GlobalEnv),
	  identical(format(ef$form), "log(y) ~ x"))
## Back compatibility + deprecation warning:
notC <- "Model[no 4]"
form <- `Model[no 4]` ~ .
f1 <- function(p) reformulate(".", notC)
f2 <- function(e) f1(e)
stopifnot(exprs = {
    identical(form, suppressWarnings(reformulate(".", notC))) # << will STOP working!
    identical(form, reformulate(".", as.name(notC)))
    identical(form, reformulate(".", paste0("`", notC, "`")))
    inherits(tt <- tryCatch(reformulate(".", notC), warning=identity),
             "deprecatedWarning")
    inherits(tt, "warning")
    conditionCall(tt)[[1]] == quote(reformulate)
    inherits(t1 <- tryCatch(f1(pi), warning=identity), "deprecatedWarning")
    inherits(t2 <- tryCatch(f2(27), warning=identity), "deprecatedWarning")
    all.equal(t1, tt) # including call 'reformulate(..)'
    all.equal(t2, tt)
})
writeLines(conditionMessage(tt))


## stopifnot() now works *nicely* with expression object (with 'exprs' name):
ee <- expression(xpr=all.equal(pi, 3.1415927), 2 < 2, stop("foo!"))
te <- tryCid(stopifnot(exprObject = ee))
stopifnot(conditionMessage(te) == "2 < 2 is not TRUE")
## conditionMessage(te) was  "ee are not all TRUE" in R 3.5.x
t2 <- tryCid(stopifnot(exprs = { T }, exprObject = ee))
t3 <- tryCid(stopifnot(TRUE, 2 < 3,   exprObject = ee))
f <- function(ex) stopifnot(exprObject = ex)
t4 <- tryCid(f(ee))
stopifnot(grepl("one of 'exprs', 'exprObject' ", conditionMessage(t2)),
          conditionMessage(t2) == conditionMessage(t3),
          conditionMessage(t4) == conditionMessage(te)
          )
(function(e) stopifnot(exprObject = e))(expression(1 < 2, 2 <= 2:4))
## the latter (with 'exprs = e') gave  Error in eval(exprs) : object 'e' not found


##
## Empty 'exprs' should work in almost all cases:
stopifnot()
stopifnot(exprs = {})
e0 <- expression()
stopifnot(exprObject = e0)
do.call(stopifnot, list(exprObject = expression()))
do.call(stopifnot, list(exprObject = e0))
## the last three (w 'exprs = ')  failed in R 3.5.x


## as.matrix.data.frame() w/ character result and logical column, PR#17548
cx <- as.character(x <- c(TRUE, NA, FALSE))
stopifnot(exprs = {
    identical(cx, as.matrix(data.frame(x, y="chr"))[,"x"])
    identical(x, as.logical(cx))
})


## Failed to work after r76382--8:
assertErrV(formula("3"))
stopifnot(exprs = {
    ## New formula(<character>) specs:
    ## These give deprecation warnings:
    is.list(op <- options(warn = 1))
    identical(formula("ran = ~ 1|G"), ~ 1 | G)
    identical(formula(c("~", "foo")), ~ foo )
    identical(formula("({y ~ x})"), y ~ x)
    identical(formula("{ ~ x }"),   ~ x)
  TRUE || { ## all these "bugs" not yet in R <= 3.6.0
    identical(formula(c("y", "~", "x +    (1 | G)")), y ~ x + (1 | G))
    identical(formula(c("y", "~", "x +", "(1 | G)")), y ~ x + (1 | G))
  }## not yet
    identical(formula(c("~",    "x","+    (1 | G)")), ~x) ## NOT YET:   ~ x + (1 | G))
    is.list(options(op))
})
tools::assertWarning(formula("ran= ~ 1|G"),"deprecatedWarning", verbose=TRUE)
tools::assertWarning(formula(c("~", "x")), "deprecatedWarning", verbose=TRUE)
tools::assertWarning(formula("({y ~ x})"), "deprecatedWarning", verbose=TRUE)
tools::assertWarning(formula("{ ~ x }"),   "deprecatedWarning", verbose=TRUE)


## str2expression(<empty>) :
stopifnot(identical(str2expression(character()), expression()))


## quasi(*, variance = list()) - should not deparse(); PR#17560
## like quasipoisson() :
devRes <- function(y, mu, wt) { 2 * wt * (y * log(ifelse(y == 0, 1, y/mu)) - (y-mu)) }
init <- expression({
    if(any(y < 0)) stop("y < 0")
    n <- rep.int(1, nobs)
    mustart <- y + 0.1
})
myquasi <- quasi(link = "log",
                 variance = list(name = "my quasi Poisson",
                     varfun  = function(mu) mu,
                     validmu = function(mu) all(is.finite(mu)) && all(mu > 0),
                     dev.resids = devRes,
                     initialize = init))
x  <- runif(100, min=0, max=1)
y  <- rpois(100, lambda=1)
fq1 <- glm(y ~ x, family = myquasi)
fqP <- glm(y ~ x, family = quasipoisson)
str(keep <- setdiff(names(fq1), c("family", "call")))
identNoE <- function(x,y, ...) identical(x,y, ignore.environment=TRUE, ...)
stopifnot(exprs = {
    all.equal(fq1[keep], fqP[keep])
    ## quasi() failed badly "switch(vtemp, ... EXPR must be a length 1 vector" in R <= 3.6.0
    identNoE(quasi(var = mu),        quasi(variance = "mu"))
    identNoE(quasi(var = mu(1-mu)),  quasi(variance = "mu(1- mu)"))# both failed in R <= 3.6.0
    identNoE(quasi(var = mu^3),      quasi(variance = "mu ^ 3"))   #  2nd failed in R <= 3.6.0
    is.character(msg <- tryCmsg(quasi(variance = "log(mu)"))) &&
        grepl("variance.*log\\(mu\\).* invalid", msg) ## R <= 3.6.0: 'variance' "NA" is invalid
})


## rbind.data.frame() should *not* drop NA level of factors -- PR#17562
fcts <- function(N=8, k=3) addNA(factor(sample.int(k, N, replace=TRUE), levels=1:k))
set.seed(7) # <- leads to some  "0 counts" [more interesting: they are kept]
dfa <- data.frame(x=fcts())
dfb <- data.frame(x=fcts()) ; rbind(table(dfa), table(dfb))
dfy <- data.frame(y=fcts())
yN <- c(1:3, NA_character_, 5:8)
dfay  <- cbind(dfa, dfy)
dfby  <- cbind(dfa, data.frame(y = yN, stringsAsFactors = TRUE))
dfcy  <- dfa; dfcy$y <- yN # y: a <char> column
## dNay := drop unused levels from dfay incl NA
dNay <- dfay; dNay[] <- lapply(dfay, factor)
str(dfay) # both (x, y) have NA level
str(dfby) # (x: yes / y: no) NA level
str(dNay) # both: no NA level
stopifnot(exprs = { ## "trivial" (non rbind-related) assertions :
    identical(levels(dfa$x), c(1:3, NA_character_) -> full_lev)
    identical(levels(dfb$x),  full_lev)
    identical(levels(dfay$x), full_lev) # cbind() does work
    identical(levels(dfay$y), full_lev)
    identical(levels(dfby$x), full_lev)
    is.character(dfcy$y)
	   anyNA(dfcy$y)
    identical(levels(dfby$y), as.character((1:8)[-4]) -> levN) # no NA levels
    identical(lapply(dNay, levels),
              list(x = c("2","3"), y = levN[1:3])) # no NA levels
})
dfaby <- rbind(dfay, dfby)
dNaby <- rbind(dNay, dfby)
dfacy <- rbind(dfay, dfcy)
dfcay <- rbind(dfcy, dfay) # 1st arg col. is char => rbind() keeps char
stopifnot(exprs = {
    identical(levels(rbind(dfa, dfb)$x), full_lev) # <== not in  R <= 3.6.0
    identical(levels(dfaby$x),           full_lev)
    identical(levels(dfaby$y),                 yN) # failed a while
    identical(levels(dNaby$y),               levN) #  (ditto)
    identical(dfacy, dfaby)
    is.character(dfcay$y)
	   anyNA(dfcay$y)
    identical(dfacy$x, dfcay$x)
    identical(lapply(rbind(dfby, dfay), levels),
              list(x = full_lev, y = c(levN, NA)))
    identical(lapply(rbind(dfay, dfby, factor.exclude = NA), levels),
              list(x = as.character(1:3), y = levN))
    identical(lapply(rbind(dfay, dfby, factor.exclude=NULL), levels),
	      list(x = full_lev, y = yN))
})

## rbind.data.frame() should work in all cases with "matrix-columns":
m <- matrix(1:12, 3) ## m.N := [m]atrix with (row)[N]ames :
m.N <- m ; rownames(m.N) <- letters [1:3]
## data frames with these matrices as *column*s:
dfm   <- data.frame(c = 1:3, m = I(m))
dfm.N <- data.frame(c = 1:3, m = I(m.N))
(mNm <- rbind(m.N, m))
dfmmN <- rbind(dfm, dfm.N)
dfmNm <- rbind(dfm.N, dfm)
stopifnot(exprs = {
    identical(     dim(dfmNm), c(6L, 2L))
    identical(dimnames(dfmNm), list(c(letters[1:3],1:3), c("c","m")))
    is.matrix(m. <- dfmNm[,"m"])
    identical(dim(m.), c(6L, 4L))
    identical(dfmNm, dfmmN[c(4:6, 1:3), ])
    identical(unname(mNm), unname(m.))
})
## The last rbind() had failed since at least R 2.0.0


## as.data.frame.array(<1D array>) -- PR#17570
str(x2 <- as.data.frame(array(1:2)))
stopifnot(identical(x2[[1]], 1:2))
## still was "array" in R <= 3.6.0


## vcov(<quasi>, dispersion = *) -- PR#17571
counts <- c(18,17,15,20,10,20,25,13,12)
treatment <- gl(3,3)
outcome <- gl(3,1,9)
## Poisson and Quasipoisson
 poisfit <- glm(counts ~ outcome + treatment, family = poisson())
qpoisfit <- glm(counts ~ outcome + treatment, family = quasipoisson())
spois     <- summary( poisfit)
sqpois    <- summary(qpoisfit)
sqpois.d1 <- summary(qpoisfit, dispersion=1)
SE1 <- sqrt(diag(V <- vcov(poisfit)))
(noLdbl <- (.Machine$sizeof.longdouble <= 8)) ## TRUE when --disable-long-double
stopifnot(exprs = { ## Same variances and same as V
    all.equal(vcov(spois), V)
    all.equal(vcov(qpoisfit, dispersion=1), V) ## << was wrong
    all.equal(vcov(sqpois.d1), V)
    all.equal(spois    $coefficients[,"Std. Error"], SE1)
    all.equal(sqpois.d1$coefficients[,"Std. Error"], SE1)
    all.equal(sqpois   $coefficients[,"Std. Error"],
              sqrt(sqpois$dispersion) * SE1)
})
## vcov(. , dispersion=*) was wrong on R versions 3.5.0 -- 3.6.0
proc.time() - .pt; .pt <- proc.time()


## runmed(<x_with_NA>, "Turlach") still seg.faults in 3.6.0 {reported by Hilmar Berger}
dd1 <- c(rep(NaN,82), rep(-1, 144), rep(1, 74))
xT1 <-  runmed(dd1, 21, algorithm="T", print.level=1)# gave seg.fault
xS1 <-  runmed(dd1, 21, algorithm="S", print.level=1)
if(FALSE)
cbind(dd1, xT1, xS1)
nN <- !is.na(xT1)
stopifnot(xT1[nN] == c(rep(-1, 154), rep(1, 74)))
dd2 <- c(rep(-1, 144), rep(1, 74), rep(NaN,82))
xS2 <- runmed(dd2, 21, algorithm = "Stuetzle", print.level=1)
xT2 <- runmed(dd2, 21, algorithm = "Turlach" , print.level=1)
if(FALSE)
cbind(dd2, xS2, xT2) # here, "St" and "Tu" are "the same"
nN <- !is.na(xT2)
stopifnot(exprs = { ## both NA|NaN and non-NA are the same:
    identical(xT2[nN], xS2[nN])
    identical(is.na(xS2) , !nN)
    { i <- 1:(144+74); xT2[i] == dd2[i] }
})
## close to *minimal* repr.example:
x5 <- c(NA,NA, 1:3/4)
rS <- runmed(x5, k= 3, algorithm = "St", print.level=3)
rT <- runmed(x5, k= 3, algorithm = "Tu", print.level=3)
stopifnot(exprs = {
    identical(rS, rT)
    rT == c(1,1,1:3)/4
})
## a bit larger:
x14 <- c(NA,NA,NA,NA, 1:10/4)
rS14 <- runmed(x14, k = 7, algorithm="S", print.level=2)
rT14 <- runmed(x14, k = 7, algorithm="T", print.level=2)
## cbind(x14, rT14, rS14)
(naActs <- eval(formals(runmed)$na.action)); names(naActs) <- naActs
allT14 <- lapply(naActs, function(naA)
    tryCatch(runmed(x14, k = 7, algorithm="T", na.action=naA, print.level=2),
             error=identity, warning=identity))
rTo14 <- runmed(na.omit(x14), k=7, algorithm="T")
stopifnot(exprs = {
    identical(  rT14, rS14)
    identical(c(rT14), c(NaN,NaN, .5, .5, .5, .75, x14[-(1:6)]))
    identical(  rT14, allT14$"+Big_alternate")
    (allT14$"-Big_alternate" >= rT14)[-(1:2)] # slightly surprisingly
    identical(allT14$na.omit[-(1:4)], c(rTo14))
    inherits(Tfail <- allT14$fail, "error")
    !englishMsgs || grepl("^runmed\\(.*: .*NA.*x\\[1\\]", Tfail$message)
})


## conformMethod()  "&& logic" bug, by Henrik Bengtsson on R-devel list, 2019-06-22
setClass("tilingFSet", slots = c(x = "numeric"))
if(!is.null(getGeneric("oligoFn"))) removeGeneric("oligoFn")
setGeneric("oligoFn",
           function(object, subset, target, value) { standardGeneric("oligoFn") })
Sys.setenv("_R_CHECK_LENGTH_1_LOGIC2_" = "true")
if(getRversion() <= "3.6")## to run this with R 3.6.0, 3.5.3, ..
    Sys.unsetenv("_R_CHECK_LENGTH_1_LOGIC2_")
setMethod("oligoFn", signature(object = "tilingFSet", value="array"),	## Method _1_
          function(object, value) { list(object=object, value=value) })
setMethod("oligoFn", signature(object = "matrix", target="array"),	## Method _2_
          function(object, target) list(object=object, target=target))
setMethod("oligoFn", signature(object = "matrix", subset="integer"),	## Method _3_
          function(object, subset) list(object=object, subset=subset))	#   *no* Note
setMethod("oligoFn", signature(object = "matrix"),			## Method _4_
          function(object) list(object=object))				#   *no* Note
setMethod("oligoFn", signature(subset = "integer"),			## Method _5_
          function(subset) list(subset=subset))
setMethod("oligoFn", signature(target = "matrix"),			## Method _6_
          function(target) list(target=target))
setMethod("oligoFn", signature(value = "array"),			## Method _7_
          function(value) list(value=value))
setMethod("oligoFn", signature(subset = "integer", target = "matrix"),  ## Method _8_
          function(subset, target) list(subset=subset, target=target))
setMethod("oligoFn", signature(subset = "integer", value = "array"),	## Method _9_
          function(subset, value) list(subset=subset, value=value))
setMethod("oligoFn", signature(target = "matrix", value = "array"),	## Method _10_
          function(target, value) list(target=target, value=value))
##
showMethods("oligoFn", include=TRUE) # F.Y.I.:  in R 3.6.0 and earlier: contains "ANY" everywhere
##=========            ------------
stopifnot(exprs = {
    is.function(mm <- getMethod("oligoFn",
                                signature(object="tilingFSet",
                                          subset="missing", target="missing",
                                          value="array")))
    inherits(mm, "MethodDefinition")
    identical(
        sort(names(getMethodsForDispatch(oligoFn))) # sort(.) the same for "all" locales
        ## Now,  "ANY" only appear "at the end" .. otherwise have "missing"
      , c("matrix#ANY#ANY#ANY",
          "matrix#integer#ANY#ANY",
          "matrix#missing#array#ANY",
          "missing#integer#ANY#ANY",
          "missing#integer#matrix#ANY",
          "missing#integer#missing#array",
          "missing#missing#matrix#ANY",
          "missing#missing#matrix#array",
          "missing#missing#missing#array",
          "tilingFSet#missing#missing#array"))
})
## Testing all 10 methods:
r1 <- oligoFn(object=new("tilingFSet"), value=array(2))
r2 <- oligoFn(object=diag(2),          target=array(42))
## These 2 work fine in all versions of R: Here the "ANY" remain at the end:
r3 <- oligoFn(object=diag(2),          subset=1:3)
r4 <- oligoFn(object=diag(2))
## All these do *not* specify 'object' --> Error in R <= 3.6.x {argument ... is missing}
r5 <- oligoFn(subset = 1:5)
r6 <- oligoFn(target = cbind(7))
r7 <- oligoFn(value = array(47))
r8 <- oligoFn(subset = -1:1, target = diag(3))
r9 <- oligoFn(subset = 2:6,  value = array(7))
r10<- oligoFn(target = cbind(1,2), value = array(1,1:3))
## in R <= 3.6.0, e.g., the first  setMethod(..)  gave
## Error in omittedSig && (signature[omittedSig] != "missing") :
##   'length(x) = 4 > 1' in coercion to 'logical(1)'


## apply(., MARGIN) when MARGIN is outside length(dim(.)):
a <- tryCid(apply(diag(3), 2:3, mean))
stopifnot(exprs = {
    inherits(a, "error")
    conditionCall(a)[[1]] == quote(`apply`)
    !englishMsgs || !grepl("missing", (Msg <- conditionMessage(a)), fixed=TRUE)
    !englishMsgs || grepl("MARGIN", Msg, fixed=TRUE)
})


## cbind() of data frames with no columns lost names -- PR#17584
stopifnot(identical(names(cbind(data.frame())),
                    character()))
stopifnot(identical(names(cbind(data.frame(), data.frame())),
                    character()))
## names() came out as NULL instead of character().


## NUL inserted incorrectly in adist trafos attribute -- PR#17579
s <- c("kitten", "sitting", "hi")
ad <- adist(s, counts = TRUE)
adc <- attr(ad, "counts")
adt <- attr(ad, "trafos")
## Follow analysis in the bug report: in the diagonal, we should have
## only matches for each character in the given string.
stopifnot(exprs = {
    nchar(diag(adt)) == nchar(s)
    ## The del/ins/sub counts should agree with the numbers of D/I/S
    ## occurrences in the trafos.
    nchar(gsub("[^D]", "", adt)) == adc[, , "del"]
    nchar(gsub("[^I]", "", adt)) == adc[, , "ins"]
    nchar(gsub("[^S]", "", adt)) == adc[, , "sub"]
})

## list2env preserves values semantics
v <- list(x=c(1)) # << subtlety!
e <- list2env(v)
with(e, x[[1]] <- 42)
v
stopifnot(identical(v$x,1))


## misleading error message when coercing language object to atomic, etc:
e <- tryCid(as.double(quote(foo(1))))
stopifnot(inherits(e, "error"), grepl("'language'", e$message, fixed=TRUE))
## had 'pairlist' in R <= 3.6.1


## print(ls.str(<environment with error object with "missing" in message text>))
msg <- "arguments in the signature are missing"
e1 <- new.env(hash=FALSE)
e1$Err <- structure(list(message = msg, call = quote(foo(bar))),
                    class = c("simpleError", "error", "condition"))
writeLines(prE <- capture.output(ls.str(e1)))
## was "Err: <missing>" in R <= 3.6.1
stopifnot(exprs = { length(prE) >= 3
    grepl("List of 2", prE[[1]], fixed=TRUE)
    grepl(msg,         prE[[2]], fixed=TRUE)
    grepl("call.* foo\\(bar\\)", prE[[3]])
})


## format(x, scientific = FALSE)  for large x
xMAX <- .Machine$double.xmax
ch <- format(xMAX, scientific = 400) # << scientific as 'scipen'
op <- options(digits=1, scipen = 303)
co <- capture.output(cat(xMAX))
options(op)# back to normal
stopifnot(exprs = {
    nchar(ch) == 309
    identical(ch, co)
    ch == format(xMAX, scientific=FALSE)
})## format(*, scientific=FALSE) was "not obeyed" in R < 4.0.0


## format(<symbol>) aka format(<name>) :
for(ch in c("foo", "bar", "1", "a:b", "B space A", "`ABC", "'CBA"))
    stopifnot(identical(ch, format(as.symbol(ch))))
## gave  'Found no format() method for class "name"' in R <= 3.6.x


## x %% +- Inf -- PR#17611  //  also  %/%  for "large" args
for(x in list(0:3, c(0, 0.5+0:2))) {
    xp <- x[x != 0] # x "positive"
    for(L in c(2^(2:9), 1000^(1:7), Inf))
        stopifnot(exprs = {
            ## ----------------- %% -------------
            ## same signs :
               x  %%  L ==  x
             (-x) %% -L == -x
            ## opposite signs, x > 0 :
            (-xp) %%  L == L-xp
              xp  %% -L == xp-L
            ## ----------------- %/% ------------
              x  %/%  L == pmin(0, sign(x))
            (-x) %/% -L == x  %/%  L
            (-x) %/%  L == pmin(0, sign(-x))
              x  %/% -L == (-x) %/% L
              ## L . x :
              L %/%  xp == (-L) %/% -xp
              L %/% -xp == (-L) %/%  xp
        })
    stopifnot(exprs = {
        Inf %/%   x == sign( x+(x==0))*Inf
        Inf %/% -xp == -Inf
    })
}
## these all returned  NaN  when L == Inf  in R <= 3.6.1
##
## Further - very basics and some large (working "since ever"):
L <- 1e111 * c(-1,1)
stopifnot(exprs = {
    L %%  L == 0  # failed for a few days in R-devel
    L %% -L == 0
    -6:17 %%  3L == 0:2
    -5:15 %% -3L == -2:0
    is.finite(x <- 2^(1:1022))
    x %% (x.2 <- x/2) == 0
    x %/% 2 == x.2
    x[1:52] %% 3 == 2:1
   -x[1:52] %% 3 == 1:2
}) # larger x suffer from cancellation (well, warning too early now):
(iCrit <- ## depends on the presence and version of "long double":
    if(noLdbl)
        50:55
    else if(is.integer(digLd <- .Machine$longdouble.digits) && digLd == 64)
        60:68
    else if(is.integer(digLd) && digLd == 113) ## aarch64 {PR#17718}
        110:118
    else 250:258 # "wild guess" should always work
)
tools::assertWarning(x[iCrit] %% 3, verbose=TRUE)


## Hilmar Berger's on R-devel list: 'data.frame() == NULL' etc
d0. <- data.frame(a = numeric(0)) # zero length data.frame [ 0 x 1 ]
d0  <- unname(d0.) # zero length data.frame __without names__
d3   <- data.frame(a=1:3) # non-empty data.frame
d30. <- d3[,FALSE] # <3 x 0>
d30  <- unname(d30.)
for(DF in list(d0., d0, d30., d30))
    for(R in list(1, NULL, logical(0)))
	stopifnot(exprs = {
	    is.logical(r <- DF == R)
	    is.matrix(r) ## ~~~~~~~
	    length(r) == 0
	    dim(r) <= dim(DF) # sometimes r is <0 x 0> when DF is not
	})
## many of these '==' calls failed in R <= 3.6.x


## Can selectively suppress warnings
w <- function(class) {
    w <- simpleWarning("warned")
    w <- structure(w, class = c(class, class(w)))
    warning(w)
}
catch <- function(expr) tryCatch({ expr; FALSE }, warning = function(...) TRUE)
stopifnot(! catch(suppressWarnings(w("foo"))))
stopifnot(! catch(suppressWarnings(w("foo"), classes = c("bar", "foo"))))
stopifnot(  catch(suppressWarnings(w("foo"), classes = c("bar", "baz"))))
rm(w, catch)


## Can selectively suppress messages
m <- function(class) {
    m <- simpleMessage("notified")
    m <- structure(m, class = c(class, class(m)))
    message(m)
}
catch <- function(expr) tryCatch({ expr; FALSE }, message = function(...) TRUE)
stopifnot(! catch(suppressMessages(m("foo"))))
stopifnot(! catch(suppressMessages(m("foo"), classes = c("bar", "foo"))))
stopifnot(  catch(suppressMessages(m("foo"), classes = c("bar", "baz"))))
rm(m, catch)


## grepl(<NA>, ...)
N <- grepl(NA_character_, "something")
stopifnot(is.na(N), is.logical(N))
## gave integer instead of logical in R <= 3.6.1


## options(warn=1e11) leading to infinite loop -> "C Stack ..." error
tools::assertError(options(warn = 1+.Machine$integer.max))
## "worked" and gave problems later in R <= 3.6.1


## PR#17628
df <- data.frame(x = 1, y = 2); class(df$y) <- "object_size"
df ## --> print.data.frame(*, digits=NULL)' -- error in R <= 3.6.1
format(object.size(pi), digits=NULL)
## error in R <= 3.6.1

## PR#15522
pos <- barplot(1:2, space=c(9, 1),
    ylim=c(0, 21), xlim=c(0, 11), horiz=TRUE,
    plot=FALSE)
stopifnot(all.equal(pos, cbind(c(9.5, 11.5))))
## bar spacing was wrong in R <= 3.6.1

## methods(class = <{length > 1}>)  giving many non-helpful warnings
tools::assertWarning(mc <- methods(class = class(ordered(4:1))), verbose=TRUE)
                                        # class = ".S3methods",
stopifnot(is.character(mc), inherits(mc, "MethodsFunction"),
          is.data.frame(attr(mc,"info")))
## warns once only, in R >= 3.6.2


## PR#17580 -- using max.lines, "truncated"
op <- options(error = expression(NULL)) # {careful! : errors do *NOT* stop}
is.t.back <- function(x) is.pairlist(x) && all(vapply(x, is.character, NA))
f <- function(...) stop(deparse(substitute(...)))
g <- function(...) f(...)
do.call(g, mtcars)
tb. <- .traceback()
traceback(tb1 <- .traceback(max.lines=1))# prints with '...' as it's truncated
stopifnot(exprs = {
    is.t.back(tb.)
    is.t.back(tb1)
    length(tb.) == length(tb1)
    vapply(tb1, length, 0L) == 1
    length(tb.[[3]]) > 20
})
f <- function() options(warn = 1+.Machine$integer.max)
do.call(g, mtcars)
tb0 <- .traceback()
traceback(tb3  <- .traceback(max.lines = 3))
traceback(tb00 <- .traceback(max.lines = 0))
options(op)# revert to normal
stopifnot(exprs = {
    is.t.back(tb0)
    is.t.back(tb3)
    is.t.back(tb00)
    vapply(tb0, function(.) is.null(attributes(.)), NA)
    length(tb0) == length(tb3)
    vapply(tb3 , length, 0L) <= 3
    vapply(tb00, length, 0L) == 0L
    identical(lapply(tb3, attributes),
              list(list(truncated = TRUE), NULL))
    identical(lapply(tb00, attributes),
              rep(list(list(truncated = TRUE)), 2))
})
f <- function(...) .traceback(2, max.lines=1)
g(
  'hello hello hello hello hello hello hello hello hello hello hello',
  'world world world world world world world world world world world'
) -> tb2n1
stopifnot(is.character(t1 <- tb2n1[[1]]), length(t1) == 1L, attr(t1, "truncated"))
## partly not possible in R < 4.0.0; always deparsed in full


## PR#13624 : get_all_vars(*, <matrix>):
ok_get_all_vars <- function(form,d) { ## get_all_vars() :<=> model_frame() apart from "terms"
    mf <- if(missing(d)) model.frame(form) else model.frame(form,d)
    attr(mf, "terms") <- NULL
    identical(mf,
              if(missing(d)) get_all_vars(form) else get_all_vars(form,d))
}
M <- .Date(matrix(1:15, 5,3)) # has class to be kept
n <- 26:30
T <- TRUE
m <- 2:7
stopifnot(exprs = { is.matrix(M) ; dim(M) == c(5,3)
    ok_get_all_vars(~ M)
    ok_get_all_vars(~M+n)
    ok_get_all_vars(~ X ,               list(X=  M))
    ok_get_all_vars(~z+X,               list(X=  M,  z=n))
    ok_get_all_vars(~z+X,               list(X=I(M), z=n))
    ok_get_all_vars(~z+X,    data.frame(     X=I(M), z=n))
    ok_get_all_vars(~z+X,    data.frame(list(X=I(M), z=n)))
    ok_get_all_vars(~z+X, as.data.frame(list(X=I(M), z=n)))
    lengths(d <- get_all_vars(~ n + T, "2n" = 2*n)) == 5L
    identical(d[,"T"], rep.int(TRUE, 5))
    ## recycling works when commensurate:
    lengths(d6 <- get_all_vars(~ m + T, one=1, "2 s"=1:2, "3's"=3:1, `f 3` = gl(3,2))) == 6
    identical(colnames(d6), c("m", "T", "one", "2 s", "3's", "f 3"))
})
## all but the first 4 cases worked already in R <= 3.6.1


## two-arg Rd macros (PR#17627)
parse_Rd_txt <- function(ch) tools::parse_Rd(textConnection(ch), fragment = TRUE)
rd1 <- parse_Rd_txt(t1 <- "\\if{html}{\\out{<hr>}}")
rd2 <- parse_Rd_txt(t2 <- "\\href{https://www.r-project.org}{some text}")
(tx1 <- paste(as.character(rd1), collapse = ""))
(tx2 <- paste(as.character(rd2), collapse = ""))
stopifnot(exprs = {
    identical(paste0(t1,"\n"), tx1)
    identical(paste0(t2,"\n"), tx2)
})
## had duplicated braces in R < 4.0.0


## power.t.test() failure for very small (unreasonable) n;  R-devel m.list Oct.4, 2019
(ptt0 <- power.t.test(delta=10,  sd=1,       power=0.9 , sig.level=0.05, tol = 1e-8))
(ptt1 <- power.t.test(delta=0.6, sd=0.00001, power=0.9 , sig.level=0.05))
(ptt2 <- power.t.test(delta=2,   sd = 1e-8,  power=0.99, sig.level=0.01))
stopifnot(exprs = {
    all.equal(0.9, power.t.test(delta=10, sd=1, n = ptt0 $ n)$power)
    all.equal(ptt1$n, 1.00428,   tol = 1e-5)
    all.equal(ptt2$n, 1.1215733, tol = 1e-5)
})
## when uniroot() was trying n < 1, the code failed previously (in 2nd and 3rd case)


## improved error message from contour():
tt <- tryCid(contour(volcano, levels = c(20*c(4:6, -Inf, 8:10))))
print(tt)
## The rest of this message is OS-dependent: gcc 5.x on Solaris has '= -Inf'
## others have " = -inf"
stopifnot(inherits(tt, "error"), grepl("non-finite level.*\\[4\\]", tt$message))
## had "invalid NA contour values"


## get_all_vars() when some variables are data frames - PR#14905
x <- (1:10)/10
Y <- data.frame(A = 2^x, B = pi*x)
gav <- get_all_vars(Y[,1] ~ x)
stopifnot(exprs = {
    is.data.frame(gav)
    ncol(gav) == 3
    identical(gav, cbind(Y, x))
    identical(get_all_vars(x ~ Y), cbind(x, Y))
})
## the last were wrong in R <= 3.6.1


## get all arguments from matched argument list; failed in R <= 4.0.0
y <- list()
stopifnot(identical(attr(`attr<-`(y, value = 1, "A"), "A"), 1))
y <- structure(list(), AA = 1)
stopifnot(is.null(attr(y, exact = TRUE, "A")))


## 1) A matrix is an array, too:
stopifnot( vapply(1:9, function(N) inherits(array(pi, dim = 1:N), "array"), NA) )
## was false for N=2 in R < 4.0.0
##
## 2) Matrix must dispatch for array methods, too :
foo <- function(x) UseMethod("foo")
foo.array <- function(x) "made in foo.array()"
stopifnot(
    vapply(1:9, function(N) foo(array(pi, dim = 1:N)), "chr") == foo.array())
## foo(array(*)) gave error for N=2 in R < 4.0.0


## PR#17659: Some *.colors() producers have appended (alpha=1) info even by default
fnms <- c(apropos("[.]colors$"), "rainbow") # 8 x "<foo>.colors" + rainbow
for(fn in fnms) {
    Fn <- get(fn, mode="function")
    cat(sprintf("%14s(n), n = 1,2,3 : ", fn))
    for(n in 1:3)
        stopifnot(length(cc <- Fn(n)) == n,
                  nchar(cc) == 1L+6L, # just RGB, no alpha
                  identical(cc, Fn(n, alpha=NULL)))
    cat("[Ok]\n")
}
## in R <= 3.6.x, four of these functions gave extra alpha=1 info (appended "FF")


## Generalized head(x, n) and tail() methods - for length(n) > 1 and arbitrary array x
## PR#17652
## -------- pkg glmmTMB uses head(.) on calls quite a bit
cForm <- quote(some ~ really + quite + longish + but:still:not:very:long *
                   (formula | reality / extreme:cases:you:never:think:of))
fL <- eval(cForm)
length(fRHS <- fL[[3]])
cLong <- quote(fun_with_many_args(1,2,3, 4,5,6, 7,8,9))
a1 <- structure(array(1:7,  7  ), class = "foo")
a3 <- structure(array(1:24, 2:4), class = "foo")
stopifnot(exprs = {
    ## these all work as previously
    head(cForm,1) == `~`()
    head(cForm,2) == ~some
    head(cForm) == cForm
    is.call(cl <- quote((Days|Subject)))
    is.call(fL)
    inherits(fL, "formula")
    head(fL) == fL
    ## == tail ===
    identical(tail(cForm,1), cForm[3])
    tail(cForm,2) == cForm[2:3]
    tail(cForm) == cForm
    tail(fL) == fL
    ##
    ## -------------failed from here -----------------------
    identical(head(cl), cl) ## for a few days, gave Error in do.call(..):  object 'Days' not found
    identical( head(fRHS), fRHS)
    identical(head(cLong), cLong[1:6])
    identical(head(cLong, 2), cLong[1:2])
    identical(head(cLong, 1), quote(fun_with_many_args()))
    ## == tail ===
    identical(tail(cl), cl) ## for a few days, gave Error ...:  object 'Days' not found
    identical( tail(fRHS), fRHS)
    identical(tail(cLong), cLong[tail(seq_along(cLong))])
    identical(tail(cLong, 2), cLong[9:10])
    identical(tail(cLong, 1), cLong[10])
    ## funny arrays
    identical(head(a1,1), a1[1,    drop=FALSE])
    identical(head(a3,1), a3[1, ,, drop=FALSE])
    identical(tail(a3,1), a3[2, ,, drop=FALSE])
})
##
## Ensure that the code does not access dimensions it does not need (pkg TraMineR):
`[.noCol` <- function(x, i, j, drop = FALSE) {
    if(!missing(j)) stop(" [!] Column subscripts not allowed", call. = FALSE)
    NextMethod("[")
}
noC <- structure(datasets::trees, class = c("noCol", "data.frame"))
assertErrV( noC[1,2]) # fails indeed
stopifnot(exprs = {
    identical(head(noC), noC[1:6,])
    identical(head(noC, 1), noC[1, ])
    identical(tail(noC, 1), noC[31,])
})
##
## For all arrays 'a',  head(a, 1)  should correspond to  a[1, {,}* , drop = FALSE]
## length(n) > length(dim(x)) (or 1L if dim(x) is NULL) is an error
str(Alis <- lapply(1:4, function(n) {d <- 1+(1:n); array(seq_len(prod(d)), d) }))
h2 <- lapply(Alis, head, 2)
t2 <- lapply(Alis, head, 2)
assertErrV( head(Alis[[1]], c(1, NA)))
assertErrV( tail(1:5, c(1, NA)))
h1 <- lapply(Alis, head, 1)
t1 <- lapply(Alis, tail, 1)
dh1 <- lapply(h1, dim)
## n =1L and n=c(1, NA) equivalent (only ones with 2+ dimensions)
Alis2p <- Alis[-1]
h1N <- lapply(Alis2p, head, c(1, NA))
t1N <- lapply(Alis2p, tail, c(1, NA))
Foolis <- lapply(Alis, `class<-`, "foo")
assertErrV( head(Foolis[[1]], c(1, NA)))
h1F  <- lapply(Foolis, head, 1)
h2F  <- lapply(Foolis, head, 2)
t1F  <- lapply(Foolis, tail, 1)
t2F  <- lapply(Foolis, tail, 2)
Foolis2p <- Foolis[-1]
h1FN <- lapply(Foolis2p, head, c(1, NA))
t1FN <- lapply(Foolis2p, tail, c(1, NA))
stopifnot(exprs = {
    identical(h2, Alis)
    identical(t2, Alis)
    vapply(h1, is.array, NA)
    vapply(t1, is.array, NA)
    identical(dh1, lapply(1:4, function(n) seq_len(n+1L)[-2L]))
    identical(dh1, lapply(t1, dim))
    identical(h1,  c(list(Alis  [[1]][1, drop=FALSE]), h1N))
    identical(t1,  c(list(Alis  [[1]][2, drop=FALSE]), t1N))
    identical(h1F, c(list(Foolis[[1]][1, drop=FALSE]), h1FN))
    identical(t1F, c(list(Foolis[[1]][2, drop=FALSE]), t1FN))
})
## This was *not the case for  1d arrays in R <= 3.6.x
##
tools::assertWarning(t3 <- tail(iris3[,1,], addrownums = FALSE), verbose=TRUE)
stopifnot( identical(t3,   tail(iris3[,1,],  keepnums  = FALSE)) )
##
## 4-dim array
## 4th dimension failed transiently when I using switch() in keepnums logic
adims <- c(11, 12, 4, 3)
arr <- array(seq_len(prod(adims)), adims)
headI4 <- function(M, n) {
    d <- dim(M)
    M[head(seq_len(d[1]), n[1]),
      head(seq_len(d[2]), n[2]),
      head(seq_len(d[3]), n[3]),
      head(seq_len(d[4]), n[4]),
      drop = FALSE]
}
tailI4 <- function(M, n) {
    d <- dim(M)
    M[tail(seq_len(d[1]), n[1]),
      tail(seq_len(d[2]), n[2]),
      tail(seq_len(d[3]), n[3]),
      tail(seq_len(d[4]), n[4]),
      drop = FALSE]
}

n.set2 <- lapply(-2:2, rep, times = 4)
stopifnot(
    vapply(n.set2, function(n) identCO (head(arr, n), headI4(arr, n)), NA),
    vapply(n.set2, function(n) identCO (tail (arr, n, keepnums=FALSE),
                                        tailI4(arr, n)), NA),
    vapply(n.set2, function(n) all.equal(tail(arr, n), tailI4(arr, n),
                                         check.attributes=FALSE), NA))

## full output
aco <- capture.output(arr)
## extract all dimnames from full output
## assumes no spaces in names
## assumes NO WRAPPING when printing rows!
getnames <- function(txt, ndim = 4) {
    el <- which(!nzchar(txt))
    ## first handled elsewhere, last is just trailing line
    el <- el[-c(1L, length(el))]
    hdln  <- c(1L, el[seq(2, length(el), by = 2)] - 1L)
    hdraw <- lapply(txt[hdln], function(tx) strsplit(tx, ", ")[[1L]])

    ## line 1 is higher indices, 2 is blank, 3 is columns
    cnms <- strsplit(trimws(txt[3], which = "left"), split = "[[:space:]]+")[[1]]
    cnms <- cnms[nzchar(cnms)]
    matln <- 4:(el[1] - 1L)
    rnms <- gsub("^([[:space:]]*[^[:space:]]+)[[:space:]].*", "\\1", txt[matln])
    hdnms <- lapply(3:ndim, ## blank ones are left in so this is ok
                    function(i) unique(sapply(hdraw, `[`, i )))
    c(list(rnms, cnms),
      hdnms)
}
fpnms <- getnames(aco, length(adims))
## ensure all dimnames correct for keepnums = TRUE
stopifnot(
    vapply(n.set2, function(n) identical(dimnames(tail(arr, n)),
                                         mapply(function(x, ni) if(ni != 0) tail(x, ni),
                                                x = fpnms, ni = n, SIMPLIFY = FALSE)),
           NA)
)
## mix named and non-named dimensions to catch bug in initial keepnums patch
arr2 <- arr
adnms <- lapply(seq_along(adims),
                function(i) paste0("dim_", i, "_", seq(1L, adims[i])))
adnms[3L] <- list(NULL)
dimnames(arr2) <- adnms
ii <- seq_along(adnms)
stopifnot(
    vapply(n.set2, function(n)
        identical(dimnames(tail(arr2, n)),
                  mapply(function(i, ni) {
                            x <- adnms[[i]]
                            if(is.null(x))
                                x <- as.character(seq_len(adims[i]))
                            if(ni != 0L)
                                tail(x, ni)
                         },
                         i = ii, ni = n, SIMPLIFY = FALSE)),
        NA)
)
##
## matrix of "language" -- with expression()
is.arr.expr <- function(x) is.array(x) && is.expression(x)
e <- matrix(expression(foo(2), bar(x), r(foobar), foo(rbar)), 2)
str(h1 <- head(e, 1))
str(t1 <- tail(e, 1))
stopifnot(exprs = {
    is.arr.expr(e)  && identical(dim(e),  c(2L, 2L))
    is.arr.expr(h1) && identical(dim(h1), c(1L, 2L))
    is.arr.expr(t1) && identical(dim(t1), c(1L, 2L))
    is.arr.expr(ee <- e[rep(1:2, 3), rep(1:2, 2)]) && identical(dim(ee), c(6L, 4L))
    is.arr.expr(hee <- head(ee, n=c(2,-1))) && identical(dim(hee), 2:3)
    is.arr.expr(tee <- tail(ee, n=c(-3,1))) && identical(dim(tee), c(3L, 1L))
})
## (for length(n) == 1,  has worked the same "always")


## Forgotten 'drop=FALSE' in plot.formula()
df <- data.frame(x=1:3, grp=c("A","A","B"), stringsAsFactors = TRUE)
plot( ~grp, data=df, subset = x > 1)
## failed in R <= 3.6.1


## dnorm() etc border cases, notably sigma = -Inf
tools::assertWarning(v0Neg  <- dnorm(0:1, sd = -Inf))
tools::assertWarning(dlInf0 <- dlnorm(Inf,Inf, sd = 0))
stopifnot(is.nan(v0Neg), is.nan(dlInf0))
## in R <= 3.6.2, v0Neg was 0 w/o any warning; dlnorm(...) was +Inf


## Unusual frequency and start not supported by ts() and window()
x <- ts(x, start = 2.5, end = 107.5, frequency = 0.2)
(wx <- window(x, start = 20, end = 30, extend = TRUE))
stopifnot(exprs = {
    all.equal(attributes(x),         list(tsp = c(2.5, 107.5, 0.2), class = "ts"))
    all.equal(wx, structure(c(0.5, 0.6), .Tsp = c(22.5, 27.5, 0.2), class = "ts"))
})
assertErrV(cbind(ts(1:2, start = 0.5, end = 1.5),
                 ts(1:2, start = 0  , end = 1)))
## Wrong results in R < 4.0.0
## New checks needed tweaks :
## -- 1 --
frYr <- 365.25
tt <- (0:3652)/frYr
timeO <- structure(tt, .Tsp = c(1981, 1990.998631, frYr), class = "ts")
ttt <- time(timeO) # Error "'end' must be a whole number of cycles after 'start'"
## -- 2 --
set.seed(7); tt <- ts(rnorm(60), frequency=12)
dt2 <- diff(tt, differences = 2) # Error in .cbind.ts(..): not all series have the same phase
tsD <- ts(1:49, start=as.Date("2019-12-12"), frequency=12)
stopifnot(exprs = {
    all.equal(timeO, ttt - 1981, tol = 1e-8)
    inherits(ttt, "ts")
    inherits(dt2, "ts")
    length(dt2) == length(tt) - 2L
    all.equal(6*tsp(dt2), c(7, 35.5, 72))
    all.equal(dt2[1:2], c(3.986498, -0.22047961))
    all.equal(tsD, structure(1:49, .Tsp = c(18242, 18246, 12), class = "ts"))
})
## failed for a while in R-devel 2019-12-*



## Using deparse1() fixing potential naming problems in many places, PR#17671
(acl <- do.call(acf, list(lynx, plot=FALSE)))
set.seed(7); t44 <- table(sample(LETTERS[1:4], size = 50, replace=TRUE),
                          sample(letters[1:4], size = 50, replace=TRUE))
ft44 <- do.call(fisher.test, list(t44))
stopifnot(length(acl$series) == 1,
          length(do.call(pacf, list(lynx, plot=FALSE))$series) == 1,
          identical(t44, eval(str2lang(ft44$data.name))))
## funny data names in R < 4.0.0


## wilcox.test(x,{y,} ..): when 'x' and/or 'y' contain +/- Inf
dfn <- c(shifted = function(L) 1/8 + c(9:4, L), # <- without, and
            ties = function(L)       c(9:4, L)) # <- with ties
oWarn <- getOption("warn")
for(nm in names(dfn)) {
    y7 <- dfn[[nm]]
    options(warn = if(nm == "ties") 1 else 2) ## "ties" : ==> 2 x 3 (different) warnings
    w2  <- lapply(c(1000, Inf), function(L) wilcox.test(1:7, y7(L)))
    w1  <- lapply(c(1000, Inf), function(L) wilcox.test( y7(L) ))
    w2p <- lapply(c(1000, Inf), function(L) wilcox.test(1:7, y7(L), paired= TRUE))
    w2n <- lapply(c(1000, Inf), function(L) wilcox.test(1:7, y7(L), exact = FALSE))
    w2pn<- lapply(c(1000, Inf), function(L) wilcox.test(1:7, y7(L), exact = FALSE, paired=TRUE))
    stopifnot(exprs = {
        identical(w2  [[1]], w2  [[2]]) # was FALSE in R <= 3.6.x
        identical(w1  [[1]], w1  [[2]]) # was FALSE ..
        identical(w2p [[1]], w2p [[2]])
        identical(w2n [[1]], w2n [[2]]) # was FALSE ..
        identical(w2pn[[1]], w2pn[[2]])
    })
}; options(warn = oWarn)
## non-paired cases treated 'Inf' non-robustly in R <= 3.6.x
wII <- wilcox.test(c(-Inf, 1:5, Inf), c(-Inf, 4*(0:4), Inf), paired=TRUE) # error in R <= 3.6.x
 w1 <- wilcox.test(c(      1:5, Inf), c(      4*(0:4), Inf), paired=TRUE) # ditto
(w0 <- wilcox.test(        1:5,               4*(0:4),       paired=TRUE))
sel <- names(w0) != "data.name"
stopifnot(identical(w0[sel], w1[sel]), identical(w0[sel], wII[sel]))
## Inf-Inf  etc broken in paired case in R <= 3.6.x


if(FALSE){ ## pro tem
## round(x, n) "to even" failed in some cases -- PR#17668
dd <- 0:12
x55 <- 55 + as.numeric(vapply(dd+1, function(k) paste0(".", strrep("5",k)), ""))

rnd.x <- vapply(dd+1L, function(k) round(x55[k], dd[k]), 1.1)
noquote(formatC(cbind(x55, dd, rnd.x), w=1, digits=15))
signif (rnd.x - x55, 3) # look at .. but don't test (yet)
stopifnot(exprs = {
      all.equal(abs(rnd.x - x55), 5 * 10^-(dd+1), tol = 1e-11) # see diff. of 6e-13
})
## more than half of the above were rounded *down* in R <= 3.6.x
## Some "wrong" test cases from CRAN packages (partly relying on wrong R <= 3.6.x behavior)
stopifnot(exprs = {
    all.equal(round(10.7775, digits=3), 10.778, tolerance = 1e-12) # even tol=0, was 10.777
    all.equal(round(12345 / 1000,   2), 12.35 , tolerance = 1e-12) # even tol=0, was 12.34 in Rd
    all.equal(round(9.18665, 4),        9.1866, tolerance = 1e-12) # even tol=0, was  9.1867
})
## This must work, too, the range of 'e' depending on 'd'
EE <- c(-307, -300, -250, -200,-100,-50, -20, -10, -2:2,
        10, 20, 50, 100, 200, 250, 290:307)
for(d in 0:16) { cat("digits 'd' = ", d, ": ")
    for(e in EE[EE+d <= 308]) {
        f <- 10^e
        cat(".")
        stopifnot(all.equal(tolerance = if(d < 14) 1e-15
                                        else if(d == 14) 1e-14 else 1e-13,
                            round(pi/f, e + d) * f,
                            round(pi, d)))
    };cat("\n")
}
## (2nd part: continued working)
i <- c(-2^(33:10), -10:10, 2^(10:33))
for(digi in c(0:10, 500L, 1000L, 100000L, .Machine$integer.max))
    stopifnot(identical(i, round(i, digi)),
              identical(i+round(1/4, digi), round(i+1/4, digi)))
x <- 7e-304; rx <- round(x, digits=307:322); xx <- rep(x, length(rx))
print(cbind(rx), digits=16) # not really what ideally round() should do; but "ok"
          all.equal(rx, xx, tol = 0)# show "average relative difference" ("5.6856 e -16")
stopifnot(all.equal(rx, xx, tol = 1e-4)) # tol may change in future
## the round(i, *) failed, for ~ 2 days, in R-devel
e <- 5.555555555555555555555e-308
(e10 <- e * 1e298) # 5.555556e-10 -- much less extreme, for comparison
ds <- 20:1 ;   s.e <- signif(e, ds) ; names(s.e) <- paste0("d", ds)

## currently, for round,  digits := pmin(308, digits) -- not going further than 310
d <- 310:305; r.e   <- round (e,   d) ; names(r.e)   <- paste0("d", d)
d <- d - 298; r.e10 <- round (e10, d) ; names(r.e10) <- paste0("d", d)
op <- options(digits=18)
cbind(signif = c(e, s.e)) ##-- this always rounds up (= to even)
cbind( round = c(e, r.e), round.10 = c(e10, r.e10))
iSub <- 6 : (18 + capabilities("long.double"))
stopifnot(exprs = {
    ## the regularity of signif()'s result is amazing:
    is.integer(d <- ds[iSub] - 1L)
    all.equal(log10(abs(1 -  diff(unname(s.e))[iSub] * 1e308*10^d / 4)),
              d - 16, tol = 0.08) # tol: seen 0.0294 / 0.02988 (Win 32b)
    all.equal(r.e * 1e298, r.e10,
              check.attributes = FALSE, countEQ=TRUE, tol=1e-14)
})
## was not true for digits = 309, 310 in R <= 3.6.x
##
## round(*, digits < 0)
M <- .Machine$double.xmax
rM <- round(M, -(1:400))
stopifnot(exprs = {
    rM[(1:400) > 308] == 0
### platform (compiler configuration) dependent:
    ## identical(which(rM == Inf),
    ##           c(if(!b64) 294L, 298L, 299L, 304:308) -> II)
    ## is.finite(rM[-II])
})
## had many Inf and NaN; now looks optimal: 'Inf' are "correct" rounding up
##
(mm <- 2^-(1022+52)) # denormalized smallest number
mm == 1.49*mm # yes, that's "denormal"
dr <- diff(rmm <- round(mm, 301:500))
(inz <- which(dr != 0))
stopifnot(length(inz) == 1, dr[inz] == mm, dr[-inz] == 0,
          rmm[-(1:23)] == mm)
options(op) ## in R <= 3.6.x, all(rmm == 0)
}

## update.formula() triggering terms.formula() bug -- PR#16326
mkF <- function(nw) as.formula(paste("y ~ x + x1",
                                     paste0("- w", seq_len(nw), collapse="")),
                               env = .GlobalEnv)
fterms <- function(n, simplify=TRUE) formula(terms.formula(mkF(n), simplify=simplify))
if(interactive())
    for(n in 1:20) print(fterms(66)) # always correct now:  y ~ x + x1
## used to have a '-1'  (and much more, see below) in R <= 3.6.2
## NB: had memory / random behavior -- and sometimes ended in
##     malloc(): corrupted top size
##     Process R... aborted (core dumped)
set.seed(17)
N <- 1024
Ns <- sort(1 + rpois(N, 3)+ 16*rpois(N, 3))
FN <- lapply(Ns, fterms)
(UFN <- unique(FN))
stopifnot(identical(y ~ x + x1, UFN[[1]]))
## Ended in this error [which really comes from C code trying to set dimnames !] :
##   Error in terms.formula(mkF(n), simplify = simplify) :
##     'dimnames' applied to non-array
##
##--TODO: less severe now (no seg.fault / corrupt memory crashes), but still really bad ! ---


## Corner cases in choose(),
## misbehaved when n was _nearly_ int, and n - k < k
stopifnot(choose(4 - 1e-7, 4) == 1)
stopifnot(choose(4 + 1e-7, 4) == 1)
## These gave 0 and 4 in R <= 3.6.x


## correct error message:
tt <- tryCid(strptime(100, pi))
stopifnot(inherits(tt, "error"), grepl("'format'", tt$message))
## had 'x' instead of 'format'


## r<integer-RV>() now return double if integer would overflow:
set.seed(47)
Npi <- rpois(100, 0.9999 *2^31)
Npd <- rpois(100, 0.99999*2^31)# had 33 NA's
Nbi <- rbinom(100, 2^31, 1/2)
Nbd <- rbinom(100, 2^32, 1/2)# 51 NA's
Ngi <- rgeom(999, 1e-8)
Ngd <- rgeom(999, 1e-9) # 106 NA's
stopifnot(is.integer(Npi), is.double(Npd), !anyNA(Npi), !anyNA(Npd),
          is.integer(Nbi), is.double(Nbd), !anyNA(Nbi), !anyNA(Nbd),
          is.integer(Ngi), is.double(Ngd), !anyNA(Ngi), !anyNA(Ngd),
          TRUE)
## had many NA's in  3.0.0 <= R <= 3.6.x


## rhyper() for some large arguments, PR#17694
n <- 2e9 # => .Machine$integer.max ~= 1.07 * N
set.seed(6860); N <- rhyper(1, n,n,n)
x <- 1.99e9; Nhi <- rhyper(256, x,x,x)
stopifnot(#identical(N, 999994112L), # (wrong) implementation detail
          is.integer(Nhi),
          all.equal(mean(Nhi), x/2, tol = 6e-6)) # ==> also: no NAs
## NA's and warnings, incl "SHOULD NOT HAPPEN!" in R <= 3.6.2


## assertCondition(*, "error") etc triggered errors *twice* (accidentally)
stopifnot(identical(tools::assertError(sqrt("a")),
                    list(     tryCid(sqrt("a")))))
## The former contained the error object twice in R <= 3.6.2


## Overriding encoding in parse()
if (l10n_info()$"UTF-8" || l10n_info()$"Latin-1") {
    x8 <- "'\uf6'"
    x8.2 <- substr(x8, 2, 2)
    stopifnot(identical(Encoding(x8), "UTF-8"))
    f8 <- tempfile()
    writeLines(x8, f8, useBytes=TRUE) # save in UTF-8
    ##
    chk_x82 <- function(x) stopifnot(identical(Encoding(x), "UTF-8"),
                                     identical(x, x8.2))
    ## parse(*, encoding = "UTF-8", ..) :
    for(FF in c(function(.) parse(text=., encoding="UTF-8", keep.source=TRUE),
                function(.) parse(text=., encoding="UTF-8", keep.source=FALSE)
                )) {
        x <- eval(FF(x8))
        chk_x82(x)
    }
    for(K.S in c(TRUE, FALSE)) {
        x <- eval(parse(file=f8, encoding="UTF-8", keep.source = K.S))
        chk_x82(x)
    }
    ## latin1 <--> UTF-8
    xl <- iconv(x8, from="UTF-8", to="latin1")
    stopifnot(identical(Encoding(xl), "latin1"))
    stopifnot(identical(x8, iconv(xl, from="latin1", to="UTF-8")))
    unlist(l10n_info()) # to see ..
}
if (l10n_info()$"UTF-8") {
    for(x in c(eval(parse(text=x8)),
               eval(parse(text=xl, keep.source=TRUE)),
               eval(parse(text=xl, keep.source=FALSE)),
               eval(parse(file=f8)),
               str2lang(x8),
               str2expression(x8)))
        stopifnot(identical(x, x8.2))
}
if (l10n_info()$"Latin-1") {
    for(x in c(eval(parse(text=xl)),
               eval(parse(text=x8, keep.source=TRUE)),
               eval(parse(text=x8, keep.source=FALSE)),
               str2lang(x8),
               str2expression(x8)))
        stopifnot(identical(x, x8.2))
}
## parse(text=xl) had failed w/ "EOF whilst reading MBCS char at line 2"


## smoothEnds(<integer>, .) - PR#17693
y1 <- as.integer(c(8,5,4,1,1,1,1))
y2 <- y1; y2[3] <- 6L
s1 <- smoothEnds(y1); s1.5 <- smoothEnds(y1, 5)
s2 <- smoothEnds(y2); s2.5 <- smoothEnds(y2, 5)
stopifnot(is.integer(y1), is.integer(y2), y1[-3] == y2[-3],
          is.integer(s1), is.integer(s2),
          is.integer(s1.5), is.integer(s2.5),
          s1[1] == 7L, s1[-1] == y1[-1], identical(s1.5, s1),
          s2[1] == 5L, s2[-1] == y2[-1], identical(s2.5, rep(c(6L, 1L), 3:4)))
## s1, s1.5 were double in R <= 3.6.x


## stopifnot() custom message now via <named> args:
e <- assertErrV(stopifnot("ehmm, you must be kidding!" = 1 == 0))
stopifnot(grepl("must be kidding!", e[[1]]$message))
e2 <- assertErrV(stopifnot("2 is not approximately 2.1" = all.equal(2, 2.1)))
stopifnot(grepl("not approximately", e2[[1]]$message))
## did not work in original stopifnot(<named>) patch
CHK <- function(...) stopifnot(...)
e  <- tryCid(CHK(1 == 1, 1 == 0))
e2 <- tryCid(CHK(1 == 1, "not possible" = 1 == 0))
stopifnot(inherits(e , "error"), grepl("is not TRUE", e$message),
          inherits(e2, "error"), identical("not possible", e2$message))
## wrapping stopifnot() in this way did not work in some unreleased R-devel


## norm(<matrix-w-NA>, "2")
stopifnot(is.na( norm(diag(c(1, NA)), "2") ))
## gave error from svd() in R <= 3.6.x


## norm(<matrix-w-NA>, "F")
(m <- cbind(0, c(NA, 0), 0:-1))
nTypes <- eval(formals(base::norm)$type) # "O" "I" "F" "M" "2"
print( # stopifnot( -- for now, as Lapack is still broken in some OpenBLAS -- FIXME
    is.na( print(vapply(nTypes, norm, 0., x = m)) )) # print(): show NA *or* NaN
## "F" gave non-NA with LAPACK 3.9.0, before our patch in R-devel and R-patched


## dimnames(<matrix>)[[.]] <- v -- inconsistency when length(v) == 1 : PR#17719
aa <- matrix(1:2); dimnames(aa)[[1]] <- c("a", "b") # OK (always)
 a <- matrix(1)  ; dimnames(a )[[1]] <-   "a"       # gave error: 'dimnames' must be a list
stopifnot(exprs = {
    identical(dimnames(a ), list(  "a",      NULL))
    identical(dimnames(aa), list(c("a","b"), NULL))
})
## The above works, because now, `[[<-` is consistently returning a list:
N <- NULL; N[["a"]] <- 1:2; stopifnot(identical(N, list(a = 1:2)))
N <- NULL; N[["a"]] <- 1  ; stopifnot(identical(N, list(a = 1)))
## the latter gave c(a = 1) in earlier versions of R


## deparse(), dput(), etc :  "all" now includes "digits17"; new "exact"
x <- 1 - 2^-51 ; dput(x, , "all")
stopifnot(exprs = {
    identical(deparse(x), as.character(x))
    identical(deparse(x), "1") # default only uses 15 (= DBL_DIG) digits
    if(!capabilities("long.double")) TRUE else
        identical(x, as.numeric(deparse(x, control="all")))
    identical(x, as.numeric(deparse(x, control="exact") -> dx.x))
    identical(print(dx.x),  deparse(x, control="hexNumeric"))
    TRUE || ## maybe not on all platforms ?
        identical(dx.x, "0x1.ffffffffffffcp-1") # on 32-bit, too
})
## "all" gave "1" in R <= 3.6.z


## Can suppress warnings with missing restarts
cnd <- simpleWarning("foo")
out <- tryCatch(suppressWarnings(stop(cnd)), warning = identity)
stopifnot(identical(out, cnd))
## Can suppress messages with missing restarts
cnd <- simpleMessage("foo")
out <- tryCatch(suppressMessages(stop(cnd)), message = identity)
stopifnot(identical(out, cnd))


## PR#17730 -- data() should no longer "lie" and warn {already have getOption("warn") == 2
for(p in c("base", "stats")) {
    dd <- data(package=p)
    stopifnot(inherits(dd, "packageIQR"), is.list(dd),
              nrow(dd$results) == 0)
}
## gave all data from pkg 'datasets'  *and* warned in  R <= 3.6.3


## PR#17756: x[[Inf]] and also x[[ -i ]] ,  for i in (Inf, 1,2,...):
obj <- list(
    a = 1:3
  , L3 = as.list(1:3)
  , L  = list(a = 1:2)
  , L2 = list(a = 1:2, b = 3:5)
  , LL2 = list(a = list(a1=1:3, a2=letters[1:4]),
               b = list(b1=10, b2=-(1:3)))
    )
obj$ LL3 <- c(obj$ LL2, list(c = list(c1= 7, c2= -11)))
stopifnot( print(vapply(obj[-1], function(x) is.null(x[[Inf]]), NA)) )
t_mInf <- lapply(obj, function(x) tryCid(x[[-Inf]]))
getMsg <- function(tryClist) vapply(tryClist, conditionMessage, "..")
stopifnot(length(print(table(msg_Inf <- getMsg(t_mInf)))) == 1)
## in R <= 3.6.3:
## attempt to select less than one element in get1index <real> : 1 x
## attempt to select more than one element in get1index <real> : 5 x

umInf <- unique(msg_Inf)
str(t_m1 <- lapply(obj, function(x) tryCid(x[[-1]]))) # L2, LL2 "work" - why?
    t_m2 <- lapply(obj, function(x) tryCid(x[[-2]]))  # L2, LL2 "work" - why?
    t_m3 <- lapply(obj, function(x) tryCid(x[[-3]]))
nonL2 <- grep("L2$", names(t_m1), value=TRUE, invert=TRUE)
stopifnot(exprs = {
    identical(getMsg(t_m3), msg_Inf)
    identical(t_m2$L2, 1:2)
    identical(t_m2$LL2, obj$LL2[[1]])
    identical(getMsg(t_m1[nonL2]), msg_Inf[nonL2])
    identical(getMsg(t_m2[nonL2]), msg_Inf[nonL2])
})
if(englishMsgs) { cat("checking (default = ) English error messages\n")
    stopifnot(grepl("negative subscript", umInf))
}
##


## paste(...,  recycle0=TRUE)  uses the "normal" 0-length recycling rule:
##                                "if one argument has length zero, the result has too."
ch0 <- character(0)
stopifnot(exprs = {
    ## a) when paste() has 0 '...' arguments :
    identical(paste (), ch0)               ## collapse = NULL -----------
    identical(paste0(), ch0)
    identical(paste (collapse= "A"  ), "")
    identical(paste0(collapse="foof"), "")
    identical(paste (collapse= "A"  , recycle0 = TRUE), "") # new!
    identical(paste0(collapse="foof", recycle0 = TRUE), "") # new!
    ##
    ## b) when all '...'  arguments have length 0 :
    ## ---- collapse = NULL -------------
    identical(paste({}),                  ch0)
    identical(paste({}, recycle0 = TRUE), ch0)
    identical(paste({}, NULL, ch0),                  ch0)
    identical(paste({}, NULL, ch0, recycle0 = TRUE), ch0)
    ## ---- collapse not NULL ---------
    identical(paste({}, collapse=""),                              "")
    identical(paste({}, collapse="", recycle0 = TRUE),             "") # new!
    identical(paste({}, NULL, ch0, collapse=""),                   "")
    identical(paste({}, NULL, ch0, collapse="", recycle0 = TRUE),  "") # new!
    ##
    ## c) when *one* of the ...-args has length 0 :
    identical(paste ("foo", character(0), "bar", recycle0 = FALSE), "foo  bar")
    identical(paste0("foo", character(0), "bar", recycle0 = FALSE), "foobar")
    identical(paste ("foo", character(0), "bar", recycle0 = TRUE), ch0)
    identical(paste0("foo", character(0), "bar", recycle0 = TRUE), ch0)
    identical(paste ("foo", character(0), "bar", recycle0 = TRUE, collapse="A"), "")
    identical(paste0("foo", character(0), "bar", recycle0 = TRUE, collapse="A"), "")
})
## 0-length recycling with default recycle0 = FALSE has always been "unusual"
## -----------------  with     recycle0 = TRUE      returns 0-length i.e. character(0)


## aov() formula deparsing  {Jan Hauffa, Apr 11, 2020, on R-devel}
mkAov <- function(nms, n = 50) {
    dflong <- as.data.frame(matrix(pi, n, length(nms),
                                   dimnames = list(NULL, nms)))
    forml <- as.formula(paste0("cbind(", paste(nms, collapse = ","), ") ~ 1 + Error(1)"))
    aov(forml, data=dflong)
}
nLng <- paste0("someReallyLongVariableName", 1:20)
cf1 <- coef(fm1 <- mkAov(vnms <- paste0("v", 1:20)))
cfL <- coef(fmL <- mkAov(nLng)); colnames(cfL[[1]]) <- vnms
stopifnot(all.equal(cf1, cfL))
## mkAov(nLng)  failed in R <= 4.0.0


## UTF8 validity checking internal in R (from PCRE, PR#17755)
## This is the byte representation of U+D800 (a part of a surrogate
## pair) in UTF-8, but do not rely on parser (which on some platforms
## has mis-parsed that)
stopifnot(identical(validUTF8("\xed\xa0\x80"), FALSE))

## summary.warnings()  -- reported by Allison Meisner, jhmi.edu
testf <- function(x) {
    if(x > 30)
        warning("A big problem (should be 20 of these)")
    else
        warning("Bigger problem (should be 30 of these)")
}
op <- options(warn=0)
for(i in 1:50) testf(i) # -> 50 warnings ..
options(op)# reset
(sw <- summary(warnings()))
stopifnot(identical(unlist(lapply(names(sw), substr, 1, 6)), c("Bigger", "A big ")),
          identical(attr(sw, "counts"), c(30L, 20L)))
## was wrong (mis-sorted counts) in R <= 4.0.0


## plot.formula(..,  ylab = <call>)
dd <- list(x = -4:4, w = 1/(1+(-4:4)^2))
plot(w ~ x, data=dd, type = "h", xlab = quote(x[j]))                    # worked before
plot(w ~ x, data=dd, type = "h", xlab = quote(x[j]), ylab = quote(y[j]))# *now* works
## main, sub, xlab worked (PR#10525)  but ylab did not in R <= 4.0.0


## ...names()
F <- function(x, ...) ...names()
F(a, b="bla"/0, c=c, D=d, ..) # << does *not* evaluate arguments
# |->  c("b", "c", "D", NA)
stopifnot(exprs = {
    identical(F(pi), character(0))
    F(foo = "bar") == "foo"
    identical(F(., .., .not.ok. = "a"-b, 2, 3, last = LAST),
              c(  NA, ".not.ok.",      NA, NA,"last"))
})
# .. was wrong for a few days


## check raw string parse data
p <- parse(text = 'r"-(hello)-"', keep.source = TRUE)
stopifnot(identical(getParseData(p)$text, c("r\"-(hello)-\"", "")))
rm(p)
# (wrong in R 4.0.0; reported by Gabor Csardi)

## check 0x...L parse data
p <- parse(text = '0x2L', keep.source = TRUE)
stopifnot(identical(getParseData(p)$text, c("0x2L", "")))
rm(p)
# (wrong in R 4.0.0; reported by Gabor Csardi)


## make sure there is n aliasing in assignments with partial matching
v <- list(misc = c(1))
v$mi[[1]] <- 2
stopifnot(v$misc == 1)
rm(v)
# defensive reference counts needed; missing in R 4.0.0


## round() & signif() with one / wrong (named) argument(s):
cat("Case 1 : round(1.12345):            ", round(  1.12345),"\n")
cat("Case 2 : round(x=1.12345,2):        ", round(x=1.12345, 2),"\n")
cat("Case 3 : round(x=1.12345,digits=2): ", round(x=1.12345, digits=2),"\n")
cat("Case 4 : round(digits=2,x=1.12345): ", round(digits=2, x=1.12345),"\n")
cat("Case 4b: round(digits=2,1.12345):   ", round(digits=2,1.12345),"\n")
## R <= 4.0.0 does not produce error in cases 5,6 but should :
cat("Case 5:    round(digits=x): \n")
assertErrV(cat("round(digits=99.23456): ", round(digits=99.23456)))
cat("Case 6:    round(banana=x): \n")
assertErrV(cat("round(banana=99.23456): ", round(banana=99.23456)))
## Cases 7,8 have been given an error already:
cat("Case 7: round(x=1.12345, digits=2, banana=3):\n")
assertErrV(  round(x=1.12345, digits=2, banana=3))
cat("Case 8 : round(x=1.12345, banana=3):\n")
assertErrV(  round(x=1.12345, banana=3))
## (by Shane Mueller, to the R-devel m.list)


## source(*, echo=TRUE) with srcref's and empty lines; PR#
exP <- parse(text=c("1;2+", "", "3"), keep.source=TRUE)
r <- source(exprs=exP, echo=TRUE)
stopifnot(identical(r, list(value = 5, visible = TRUE)))
## failed in R <= 4.0.1


## boxplot() with call (instead of expression) in labels; Marius Hofert on R-devel
rm(x,y,X,f)
boxplot(cbind(x = 1:10, y = c(16,9:1)), xlab = quote(x^{y[2]}), ylab = quote(X[t]),
        sub = quote(f^2 == f %*% f), main = quote(e^{-x^2/2}))
## failed in R <= 4.0.1


## on.exit() argument matching -- PR#17815
f <- function() { on.exit(add=FALSE, expr=cat('bar\n')) ; 'foo' }
stopifnot(identical(f(), 'foo')) # and write 'bar' line
g <- function() { on.exit(add=stop('boom'), expr={cat('bar\n'); FALSE}) ; "foo" }
assertErrV(g())
## f() :> "Error in on.exit(....): invalid 'add' argument"  and no error for g() in R <= 4.0.1


## multi-encodings in vector-case for duplicated/match -- PR#17809
c_latin1 <- "\xe4"
Encoding(c_latin1) <- "latin1"
c_utf8 <- enc2utf8(c_latin1)
x <- list(c_latin1, c_utf8, letters)
stopifnot(identical(duplicated(x)[2], TRUE))
## failed in R <= 4.0.1


## str(<S4 w/ extra attributes>)
mixW <- setClass("mixW", contains = "numeric")
Summ <- setClass("Summ", representation(call = "language", wts = "mixW"))
S <- Summ(call = quote(foo(x)), wts = structure(mixW(pi), CA="sunny"))
stopifnot(length(c.str <- capture.output(str(S))) >= 5,
          grepl(r"(\$ CA: chr "sunny")", c.str[5]))
deparse(S)# FIXME: is still wrong (no trace of "CA")
## "CA" was not shown in R <= 4.0.2


## sort(), order(), rank() for "raw - object":
int8 <- function(x) structure(x, class = c("int8", oldClass(x)))
`[.int8` <- function(x, ...) structure(NextMethod("["), class=class(x))
set.seed(2); si <- sample.int(37)
rI <- int8(as.raw(si))
stopifnot(exprs = {
    identical(rank (rI), rank (si))
    identical(order(rI), order(si))
    identical(sort (rI), int8(as.raw(1:37)))
})
## failed in  R <= 4.0.2


## PR#16814: r2dtable() and chisq.test(*, simulate.p.value=TRUE) for large numbers
rc <- c(63194L, 4787074L)
cc <- c(34677L, 4815591L)
set.seed(28); system.time(R2 <- simplify2array(r2dtable(1000, rc, cc)))
(c.t <- chisq.test(R2[,,1], simulate.p.value = TRUE))
set.seed(2*3); R2x3 <- r2dtable(5000, c(3,13), c(4,4,8))
stopifnot(exprs = {
    sum(!(dupR2 <- duplicated.array(R2, MARGIN=3))) == 109
    all.equal(c.t$p.value, 1929/2001)
    ## From here, true also in previous R versions:
    is.matrix(cR2 <- colSums(R2))
    cR2[1,] == cc[1]
    cR2[2,] == cc[2]
    identical(c(table(vapply(R2x3, function(m) 10*m[2,1]+m[2,3], 1))),
              c(`18` = 42L, `27` = 405L, `28` = 217L, `36` = 1021L, `37` = 1159L,
                `38` = 192L, `45` = 491L, `46` = 967L, `47` = 464L, `48` = 42L))
})
## The large tables and p-values were completely wrong in R <= 4.0.2


## PR#16877: glm()-internal refitting for the null deviance
y <- c(1, 1, 0, 0)
x <- c(5, 3, 2, 4)
fit <- glm(y ~ 1 + x + offset(log(x)), family = gaussian("log"), start = c(0,0))
## failed in R < 4.1.0 due to missing starting values for glm-internal 'fit2'
fit0 <- glm.fit(x = rep(1, length(y)), y = y, offset = log(x),
                family = gaussian("log"), start = 0)
stopifnot(all.equal(fit$null.deviance, fit0$deviance))
proc.time() - .pt; .pt <- proc.time()


## UTF-8 truncation tests
if (l10n_info()$"UTF-8") {
    ## These tests fail on R < 4.0

    ## Use .Internal(seterrmessage(old.err)) to trigger truncation via
    ## Rsnprintf (mbcsTruncateToValid).
    trunc_string <- function(x) {
        old.err <- geterrmessage()
        on.exit(.Internal(seterrmessage(old.err)))
        unname(
            vapply(
                x,
                function(y) {
                    .Internal(seterrmessage(y))
                    geterrmessage()
                },
                ""
            )
        )
    }
    ## limits to detect the internal buffer size for truncation (now 8192)
    buff.min <- 8
    buff.max <- 7e4  # > buff.min
    buff.size <- nchar(
        trunc_string(paste0(rep(0:9, length.out = buff.max), collapse="")),
        type='bytes'
    )
    stopifnot(buff.size >= buff.min + 1)
    if(buff.size == buff.max)
        ## possibly, the buffer is no longer fixed size?
        warning('BUFSIZE too large for UTF-8 truncation test?')
    else {
        string.base <- paste0(
            rep(0:9, length.out = buff.size),
            collapse=""
        )
        ## Append UTF-8 sequences at the end of strings that are just
        ## a bit shorter than the buffer, each one byte longer than the
        ## previous.
        string.starts <- substr(
            rep(string.base, 6), 1,
            nchar(string.base) - seq(buff.min, 3, -1)
        )
        ## For each of the increasing length string, append 2, 3, and 4 byte
        ## (valid) UTF-8 characters.
        string.ends <- rep(
            c(
                '\u00A2',            # <C2><A2>           (cent symbol)
                '\u20AC',            # <E2><82><AC>       (euro symbol)
                '\U00010348',        # <F0><90><8D><88>   (circle with dot)
                NULL
            ),
            each=length(string.starts)
        )
        strings <- paste0(
            string.starts,
            '\U0001F600',  # 4 byte grinning face, extra padding char
            string.ends
        )
        output <- trunc_string(strings)
        stopifnot(validUTF8(strings)) # sanity check
        stopifnot(validUTF8(output))
    }
    ## These tests fail on R < 4.1
    ##
    ## Checking that truncation and `...` concatenation are working
    ## correctly in verrorcall_dflt.  Prior to 4.1 truncation detection did
    ## not work with call set, and multibyte characters could be mangled by
    ## the `...`.
    ##
    ## We assume getttext strings are not translated (or are translated
    ## to the same byte-length as the ones in source).

    ## We cannot use `tryCatch` as we're testing the C-level error construction
    ## and that is not invoked when signalled errors are caught, hence:
    capt_err_msg <- function(expr) {
        tmp <- tempfile()
        on.exit(unlink(tmp))
        err.con <- getConnection(sink.number(type='message'))
        sink(file(tmp, 'w'), type='message')
        withRestarts(expr, abort=function() sink(err.con, type='message'))
        ## add back newlines consumed by readlines; we assume a trailing one
        ## exists, if it doesn't readLines will issue a warning
        paste0(c(readLines(tmp), ""), collapse="\n")
    }
    ## Generate errors with long messages (length buff.size + overflow), ending
    ## in `x`, to test truncation.  Will need to be updated if buff.size is
    ## increased.  Function names / etc. are all carefully counted.
    long_error <- function(x, overflow=0, buff.size=8192) {
        overflow <- as.integer(overflow)
        x <- paste0(as.character(x), collapse="")

        ## Compute how many chars needed to fill buffer
        call.len <- 51   # nchar of a_really...(stop(x)) - see below
        extra.len <- 12  # "Error in  : "
        extra.ws <- 3    # +2 spaces +1 \n from `tail`
        chars.left <- buff.size - call.len - extra.len - extra.ws
        chars <- nchar(x, type = 'bytes')
        pad.chars <- chars.left - chars + as.integer(overflow)
        stopifnot(pad.chars >= 0)
        err.msg <- paste0(paste0(rev(rep_len(rev(LETTERS), pad.chars)),
                                 collapse = ""), x)
        ## force truncation despite 8170 warn length limit
        old.opt <- options(warning.length = 8170, warn=2)
        on.exit(options(old.opt))
        a_really_long_function_to_cause_truncation <- function(x) x
        f <- function(x)
            a_really_long_function_to_cause_truncation(stop(x))
        ## trigger error and capture std.err
        capt_err_msg(f(err.msg))
    }
    buff.size.2 <- buff.size + 1     # .Internal(seterrmessage) drops 1 byte

    ## 2 byte and 4 byte utf-8 encoded chars, avoid code points between \u00a0
    ## and \u0100 as some iconv implementations will translate them into char
    ## values in those ranges instead of into "<U+...>" in C locales.
    utf8.test <- '\u0238\U00010348'

    if(buff.size.2 != 8192) {
        warning('These tests assume BUFSIZE = 8192')
    } else {
        ## Mangled multibyte in R < 4.1
        stopifnot(validUTF8(long_error(utf8.test, overflow=-1)))

        ## Truncation detection fails in R < 4.1, so newline isn't appended, so
        ## we get a "incomplete final line" warning (converted to error)
        long_error(utf8.test, overflow=0)

        overflow <- c(
             -6,   # Buffer unambiguosly unfilled for MB_CUR_MAX=6
             -5,   # Buffer maybe filled for MB_CUR_MAX=6
             -4,   # Buffer full with '...\n\0'
             -3,   # Lose 4 byte UTF-8 char
             -2,
             -1,
              0,   # 4 byte UTF-8 char exactly replaced by '...\n', buffer full
              1,   # Lose 2 byte UTF-8 char
              2,
              3,   # Lose first non UTF-8
            # These will need to change if R_ConciseTraceback changes
            -87,   # Room for traceback; options(showErrorCalls=TRUE)
            -86    # No room for traceback.
        )
        le.res <- vapply(overflow, long_error, character(1),
                         buff.size = buff.size.2, x = utf8.test)
        stopifnot(validUTF8(utf8.test))  # sanity check
        stopifnot(validUTF8(le.res))

        ## # For first one, before truncation test, we've used 8186 bytes, so we
        ## # know there was no truncation.  Code adds a trailing newline, which
        ## # is why we get 8187.  For the second, we add one byte to the
        ## # message, which puts us in maybe-truncated state, which adds 3 more
        ## # bytes via with "...", so total of 8187 + 1 + 3 == 8191.
        ## le.res.nc <- nchar(le.res)
        ## data.frame(overflow,
        ##            bytes=nchar(le.res, type='bytes'),
        ##            snippet=substr(le.res, le.res.nc - 5, le.res.nc))
        ##
        ##    overflow bytes snippet
        ## 1        -6  8187 XYZȸ𐍈\n
        ## 2        -5  8191 ȸ𐍈...\n
        ## 3        -4  8192 ȸ𐍈...\n
        ## 4        -3  8189 Zȸ...\n
        ## 5        -2  8190 Zȸ...\n
        ## 6        -1  8191 Zȸ...\n
        ## 7         0  8192 Zȸ...\n
        ## 8         1  8191 YZ...\n
        ## 9         2  8192 YZ...\n
        ## 10        3  8192 XY...\n
        ## 11      -87  8192 ation\n
        ## 12      -86  8107 XYZȸ𐍈\n
        ## test recursive errors in handler, Fails R < 4.0

        handler_error <- function(x, overflow=0, buff.size=8192) {
            overflow <- as.integer(overflow)
            x <- paste0(as.character(x), collapse="")
            pad.chars <- buff.size - nchar(x, type='bytes') + overflow
            err.msg <- paste0(
                paste0(rev(rep_len(rev(LETTERS), pad.chars)), collapse=""), x
            )
            old.opt <- options(
                error=function(...) {
                    options(error=old.opt[['error']])
                    stop(err.msg)
                }
            )
            capt_err_msg(stop('initial error'))
        }
        handler.error.trunc <- vapply(
            c(0, 1, 5), handler_error, x=utf8.test, "", buff.size=buff.size.2
        )
        stopifnot(validUTF8(handler.error.trunc))

        ## Test when warning.length is limiting

        short_error <- function(call.=TRUE) {
            old.opt <- options(warning.length=100)
            on.exit(old.opt)
            f <- function()
                stop(paste0(rep_len(0:9, 110), collapse=""), call.=call.)
            capt_err_msg(f())
        }
        ## trailing newline adds 1
        stopifnot(nchar(short_error(call.=FALSE)) == 101L)
    }
    ## PrintGenericVector truncations
    ##
    ## New printing in r78508 needs to account for UTF-8 truncation
    grin <- "\U0001F600"
    lc1 <- paste0(c(rep(LETTERS, length.out=110), grin), collapse="")
    lc2 <- paste0(c(rep(LETTERS, length.out=111), grin), collapse="")
    list.mats <- list(matrix(list(structure(1:2, class=lc1))),
                      matrix(list(structure(1:2, class=lc2))))

    ## Allowed UTF-8 truncation in R < 4.1
    ls1 <- paste0(c(rep(0:9, length.out=95), "\U0001F600"), collapse="")
    ls2 <- paste0(c(rep(0:9, length.out=96), "\U0001F600"), collapse="")
    long.strings <- list(matrix(list(ls1)), matrix(list(ls2)))

    ## Invalid UTF-8 output as "\xf0\x9f..." so needs to be parsed to un-escape
    capt_parse <- function(x) {
        out <- capture.output(print(x))
        eval(parse(text=paste0(c('c(', sprintf("'%s',", out), 'NULL)'),
                               collapse=""))[[1]])
    }
    capt.parsed <- unlist(lapply(c(list.mats, long.strings), capt_parse))
    stopifnot(validUTF8(capt.parsed))

    ## Allowed MBCS truncation in R < 4.1
    fmt <- paste0(c(rep_len("a", 253), "\U0001f600"), collapse="")
    stopifnot(validUTF8(format(as.POSIXlt('2020-01-01'), fmt)))

    f <- file(paste0(c(rep_len("a", 992), "\U0001F600"), collapse=""))
    suppressWarnings(g <- gzcon(f))
    stopifnot(!grepl("xf0", capture.output(g)[2]))
}

## c() generic removes all NULL elements --- *but* the first --- before dispatch
c.foobar <- function(...) list("ok", ...)
foobar <- structure(list(), class = "foobar")
stopifnot(exprs = {
    identical(c(foobar, NULL, one=1,NULL), list("ok", foobar, one=1))
    identical(c(a = foobar, pi, NULL, b="B",NULL), list("ok", a = foobar, pi, b="B"))
    identical(c(a = foobar, b = NULL),     list("ok", a = foobar))
    identical(c(foobar, b = foobar),       list("ok", foobar, b=foobar))
    ## Back compatibly, w/ initial NULL, using c()'s default method:
    ##  ==> result has list() for foobar
    identical(c(NULL,     foobar, NULL, NULL, 1), c(  list(), 1))
    identical(c(NULL, b = foobar, NULL, NULL, 1), c(b=list(), 1))
    identical(c(a = NULL, b = foobar),                 list())
    identical(c(a = NULL, b = foobar, c = NULL),       list())
    identical(c(NULL, a = NULL, b = foobar, c = NULL), list())
})
## the first three cases failed in R <= 4.0.x


## quantile(*, pr)  allows pr values very slightly outside [0,1] -- PR#17891
stopifnot( identical(quantile(0:1, 1+1e-14), c("100%" = 1)) )
## failed  in R <= 4.0.2


## quantile(*, pr, names=FALSE)  with NA's in 'pr' -- PR#17892
x <- (0:99)/64
prN <- c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100
qxN  <- quantile(x, probs = prN)
qxNN <- quantile(x, probs = prN, names = FALSE)
stopifnot(exprs = {
    is.null(names(qxNN))
    identical(qxNN, unname(qxN))
    identical(NA_real_, quantile(x, probs = NA, names = FALSE))
})
## qxNN gave "Error in names(o.pr)[p.ok] <- names(qs) : ..."  in R <= 4.0.2


## Vectorize() no longer keeps "garbage":
vrep <- Vectorize(rep.int, "times")
stopifnot(identical(sort(names(environment(vrep))),
                    c("FUN", "SIMPLIFY", "USE.NAMES", "vectorize.args")))
## names(..) was of length 7 in R <= 4.0.2


## as.Date( "" ) -- PR#17909
dd <- c("", "2001-09-11")
(D1 <- as.Date(    dd)) # failed in R <= 4.0.2
(D2 <- as.Date(rev(dd)))
stopifnot(is.na(D1[1]), identical(D1, rev(D2)))
## "" was not treated correctly when at [1] in R <= 4.0.2


## ..elt() propagates visibility consistently with ..n and other args, PR#17905
local({
    fn <- function(...) list(withVisible(...elt(1)), withVisible(..2))
    stopifnot(identical(
	fn(invisible(NULL), invisible(NULL)),
	rep(list(withVisible(invisible(NULL))), 2)
    ))
})


## PR#17913 -- numToBits() accidentally was destructive
n0 <- c(-7.7, 2.34e55)
b0 <- numToBits(n0)
stopifnot(sum(l0 <- as.logical(b0)) == 62L,
          identical(which(head(l0, 10)), c(1L, 3:4, 7:8)),
          identical(n0, c(-7.7, 2.34e55)))
## was '0 0' for almost a month in R-devel


## No longer assuming integer length()
if(.Machine$sizeof.pointer >= 8) {
  .Internal(inspect(-199900000:2e9))
}
## gave an error in R <= 4.0.2


## PR#17907 -- capture.output() now using standard evaluation (SE) :
## parent.frame() returns the correct environment in capture.output()
local({
    fn <- function(env = parent.frame()) {
	capture.output(env)
	list(
	    env,
	    parent.frame()
	)
    }
    env <- environment()
    out <- fn()
    stopifnot(
	identical(out[[1]], out[[2]]),
	identical(out[[1]], env)
    )
})
## capture.output() works with forwarded dots
local({
    wrapper <- function(...) {
	capture.output(..., type = "output")
    }
    out <- local({
	foo <- 1
	wrapper(foo)
    })
    stopifnot(identical(out, capture.output(1)))
})
## Failed when capture.output() was using NSE


## Inverse of numToBits() via
stopifnot(identical(packBits(b0, "double"), n0))
r <- c(pi, 1+ (0:8)/4); head(b <- numToBits(r), 25)
stopifnot(identical(packBits(b, "double"), r))
## thanks to PR#17914 by Bill Dunlap


## quantile(x, probs) when probs has NA's, PR#17899
stopifnot(identical(quantile(NULL), quantile(numeric())), # back-compatibility
	  identical(quantile(structure(numeric(), names = character()), names = FALSE),
		    rep(NA_real_, 5)))
L <- list(ordered(letters[1:11]), # class "ordered" "factor"
          seq(as.Date("2000-01-07"), as.Date("1997-12-17"), by="-1 month"))
ct <- seq(as.POSIXct("2020-01-01 12:13:14", tz="UTC"), by="1 hour", length.out = 47)
LL <- c(L, list(o0 = L[[1]][FALSE], D0 = L[[2]][FALSE],
                ct = ct, lt = as.POSIXlt(ct), num= as.numeric(ct)))
prb <- seq(0, 1, by=2^-8) # includes 0.25, 0.5, etc
for(x in LL) {
    cat("x : "); str(x, vec.len=3)
    clx <- class(if(inherits(x, "POSIXlt")) as.POSIXct(x) else x)
    ## for "ordered" *and* "Date", type must be 1 or 3
    for(typ in if(any(clx %in% c("ordered", "Date"))) c(1,3) else 1:7) {
        cat(typ, ": ")
        stopifnot(exprs = {
            identical(clx, class(q1 <- quantile(x, probs=  prb,     type=typ)))
            identical(clx, class(qN <- quantile(x, probs=c(prb,NA), type=typ))) # failed
            ## for "POSIXct", here q1 is integer, qN "double":
            { if(inherits(q1, "POSIXct")) storage.mode(qN) <- storage.mode(q1); TRUE }
            identical(qN[seq_along(q1)], q1)
            is.na(    qN[length(qN)])
        })
    }; cat("\n")
}
## qN often lost class() e.g. for "ordered" and "Date" in  R <= 4.0.2


## isS3stdGeneric() traced function:
trace(print)
stopifnot( isS3stdGeneric(print) )
untrace(print)
## was FALSE in R <= 4.0.2


## PR#17897: all.equal.factor() did not distinguish the two different NA in factors
labs <- c("a", "b", NA)
x <- factor(      3:1,                labels = labs)
y <- factor(c(NA, 2:1), levels = 1:3, labels = labs)
x
dput(x) ; dput(y) ## --> they are clearly different, but print the same:
stopifnot(exprs = {
    identCO(x,y)
    is.character(print(ae <- all.equal(x,y)))
    !englishMsgs || grepl("NA mismatch", ae, fixed=TRUE)
})
## all.equal() gave TRUE wrongly, from 2012 till R <= 4.0.2


## PR#17935:  `[.formula` for formulas with NULL:
forms <- list(f0 = (~ NULL)
            , f1 = (z ~ NULL)
            , f2 = (NULL ~ x)
            , f3 = (NULL ~ NULL)
              )
rr <- lapply(forms, function(f)
        lapply(seq_along(f), function(ii) f[ii]))
cN <- quote(NULL())
stopifnot(exprs = {
    identical( unique(lapply(rr , `[[`, 1)), list(`~`()))
    identical( lapply(unname(rr), `[[`, 2),  list(cN, quote(z()), cN,cN) )
})
## subsetting failed for all 4 formulas in R <= 4.0.3
(tm1 <- (~ "~")[-1])
(tq1 <- (~ `~`)[-1])
stopifnot(exprs = {
    identical((~ NA)[-1], quote(NA())) ## subsetting (~ NA) failed in R <= 4.0.3
    identical(tm1,        `[[<-`(call("T"), 1L, "~")) ;  is.call(tm1)
    identical(tq1,        structure(call("~"), class="formula", ".Environment" = globalenv()))
})
## zero-length formulas from subsetting are now equal to formula(NULL)
exps <- expression(
           (~ x)[FALSE]
         , (~ x)[rep(FALSE, 2)]
         , (y ~ x)[FALSE])
formL <- lapply(exps, eval)
stopifnot( length(unique(formL)) == 1,
          all.equal(formL[[1]], formula(NULL)) )
## Gave error  "attempt to set an attribute on NULL" in R <= 4,0.3


## Regression in .traceback()  PR#17930
op <- options(keep.source=TRUE)
f <- function() .traceback(1)
g <- function() f()
x <- g()
stopifnot(inherits(attr(x[[1]], 'srcref'), "srcref"))
options(op)
## had worked up to R 3.6.3, but not from 4.0.0 to 4.0.3


## Summary() and Math() data.frame methods with *logical* columns
a <- na.omit(airquality)
aF <- a[FALSE,] # 0-row version of it
dL0 <- data.frame(x=numeric(), L=logical()) # logical column
stopifnot(exprs = {
    ## "Summary" :
    sum(aF) == 0 # gave Error  "only defined on a data frame with all numeric variables"
    sum(subset(a, Ozone > 200)) == 0 # (ditto)
    suppressWarnings(range(dL0) == c(Inf, -Inf)) # (2 warnings)
    ## "Math" , gave Error..: non-numeric variable(s) in data frame :
    identical(exp(data.frame(L=TRUE)), data.frame(L=exp(TRUE)))
    identical(sinL0 <- sin(dL0), data.frame(x=numeric(), L=numeric()))
    identical(sinL0, log1p(dL0))
    identical(cumsum(dL0),       data.frame(x=numeric(), L=integer()))
})
## probably never worked in any R <= 4.0.3


## unlist(<pairlist w/ list>, recursive=FALSE), PR#17950
l.ex <- list(a = list(1:5, LETTERS[1:5]), b = "Z", c = NA)
stopifnot(identical(
    unlist(as.pairlist(l.ex), recursive = FALSE),
    unlist(            l.ex , recursive = FALSE)))
##
l2 <- list(a = "a", b = quote(b), c = pi+2i)# no list-entries
stopifnot(
    identical(
        unlist(as.pairlist(l2), recursive = FALSE) -> ul2,
        unlist(as.pairlist(l2))),
    identical(ul2, unlist(l2, recursive = FALSE)))
## lost content in R <= 4.0.3  ('FIXME' in source went lost in 2006)


## `class<-` was mutating outside of an assignment context
x <- c(1)
xx <- `class<-`(x, "foo")
stopifnot(identical(class(x), "numeric"))


## Can splice expression vectors with attributes -- PR#17869
local({
    exprs <- structure(expression(1, 2, 3), attr = TRUE)
    exprsSrcrefs <- parse(text = "1;2;3", keep.source = TRUE)
    stopifnot(
	identical(
	    bquote({ ..(exprs) }, splice = TRUE),
	    call("{", 1, 2, 3)
	),
	identical(
	    bquote({ ..(exprsSrcrefs) }, splice = TRUE),
	    call("{", 1, 2, 3)
	)
    )
})


## some issues with task callbacks:
## avoid adding a reference to the value:
x <- c(1)
old_xr <- .Internal(refcnt(x))
TCB <- addTaskCallback(function(...) TRUE)
x
stopifnot(.Internal(refcnt(x)) == old_xr)
removeTaskCallback(TCB)

## these used to fail with "object 'foo' not found":
TCB <- addTaskCallback(function(e, v, ...) { v; TRUE})
quote(foo)
removeTaskCallback(TCB)
TCB <- addTaskCallback(function(...) { length(list(...)); TRUE},
                       data = quote(foo))
removeTaskCallback(TCB)


## all.equal(<functions>) should check.environment (Kevin Van Horn, R-devel)
f <- function(x) function(y) x+y
dif <- all.equal(f(5), f(0))
stopifnot(is.function(f(5)),
          is.character(dif), grepl("difference", dif))
## all.equal() gave TRUE in  R <= 4.0.x


## p.adjust(<empty>, n=0) - PR#18002
## (1st fix-proposal computed wrongly w/ NAs:
pp <- 2^-(40:1); pp[17:19] <- NA
ppa <- p.adjust(pp, "holm") # worked always but was not strictly tested
stopifnot(all.equal(c(3.365e-11, 6.548e-11, 1.273e-10, 2.474e-10, 4.802e-10,
                      9.313e-10, 1.804e-09), ppa[1:7]))
n0 <- numeric()
stopifnot(identical(n0, p.adjust(n0, n = length(n0))))
## errored in R <= 4.0.3


## show(<standardGeneric>) where it has package ".GlobalEnv"
f <- function(x) x
setGeneric("f")
f # failed for a while (in R-devel only)


## all.equal.function() in case the env contains '...' -- PR#18010
a <- (function(...) function() NULL)(1)
b <- (function(...) function() NULL)(1) # want "a .eq. b"
D <- (function(...) function() NULL)(1:2 < 3) # want "D .NE. b"
e.. <- (function(...) environment())(1)
##' General creator of "..."  (DOTSXP) objects (short form by Suharto Anggono):
...maker <- function(...) get("...") ## fails if called without argument
...maker <- function(...) (function(...) environment())(...)[["..."]]
str( ddd <- ...maker(1) )
str( Ddd <- environment(D)[["..."]] ) # length 1, mode "...":
str( D2  <- ...maker(TRUE,TRUE))      # length 2, mode "...":
str( D3n <- ...maker(ch = {cat("HOO!\n"); "arg1"}, 2, three=1+2) )
## These all worked "accidentally" in R <= 4.0.x
assertErrV(lD2 <- D2[]) #  type '...' is not subsettable
assertErrV(D3n[]) #   (ditto)
assertErrV(D3n[][["three"]]) #  (ditto)
assertErrV(D3n $ ch) #  (ditto)
str( D3n <- ...maker(ch = {cat("HOO!\n"); "arg1"}, 2, three=1+2) )
stopifnot(exprs = {
    identical(alist(a=)$a, ...maker())# "*the* missing", the empty symbol
    identical(ddd, ...maker(1))
    identical(Ddd, ...maker(1:2 < 3))
    is.character(aeLD <- all.equal(quote(x+1), ddd))
    grepl("Mode",    aeLD[1])
    grepl("deparse", aeLD[2])
    all.equal(a, b) # failed with "Component “...”: target is not list-like" since r79585 (2020-12-07)
    all.equal(e.., environment(a))
    ## all.equal() dispatch for "..." objects ('ddd') directly:
    typeof(ddd) == "..."
    typeof(D2) == "..."
    length(D2) == 2
    is.character(aeD <- all.equal(a, D) )
    grepl("same length", aeD)
    grepl("...", aeD, fixed=TRUE)
    grepl("not identical", aeD)
    ##
    ## names(<DOTSXP>):
    is.null(names(ddd))
    identical(c("ch", "", "three"), names(D3n))
})
##  for identical() ==> ./reg-tests-2.R  -- as it's about "output"
op <- options(keep.source = FALSE) # don't keep "srcref" etc
##
Qlis <- list(NULL
## Next 4 now must work as identical(X,X) is true:
, ddd = ddd
, Ddd = Ddd
, D2  = D2
, D3n = D3n
, Qass   = quote(x <- 1)
, Qbrc   = quote({1})
, Qparen = quote((1))
, Qif    = quote(if(1)2)
, Qif2   = quote(if(1)2 else 3)
, Qwhile = quote(while(1) 2)
)
##
sapply(Qlis, class)
stopifnot( sapply(Qlis, function(obj) all.equal(obj, obj)) )
## only the first failed in R <= 4.0.3


## See PR#18012 -- may well change
aS <- (function(x) function() NULL)(stop('hello'))
bS <- (function(x) function() NULL)(stop('hello'))
try( all.equal(aS, bS) ) ## now (check.environment=TRUE) triggers the promise ..
## Now have a way *not* to evaluate aka force the promise:
(aeS <- all.equal(aS, bS, evaluate=FALSE)) # no promises forced
stopifnot(grepl("same names.* not identical", aeS))


## PR#18032: identical(<DOTSXP>,*)
ddd <- ...maker(47)
DDD <- ...maker(ch = {cat("Hu hu!\n"); "arg1"}, two = 1+1, pi, ABC="A")
stopifnot(exprs = {
    identical(ddd,ddd)
    identical(DDD,DDD)
    identical  (ddd, ...maker(47))
    ! identical(ddd, ...maker(7 )) # these *are* different
    ! identical(ddd, DDD)
})
options(op)


## PR#18034 : explicit and implicit row.names=NULL for as.data.frame.list()
data(mtcars, package="datasets")
lmtcars <- as.list(mtcars)
names(lmtcars[[3]]) <- RN <- c(letters[1:26], LETTERS[1:6])
dfcars1 <- as.data.frame.list(lmtcars)# default: missing(row.names); uses RN
dfcarsN <- as.data.frame.list(lmtcars, row.names = NULL)# does *not* use  RN
stopifnot(identical(RN,    rownames      (dfcars1)) ,
          identical(-32L, .row_names_info(dfcarsN))) # now has "automatic" (integer) row names
## dfcarsN == dfcars1  in  R <= 4.0.3


## str(x) when x has "unusal" length() semantics such that lapply() / vapply() fails:
length.Strange4 <- function(x) 4
`[[.Strange4` <- function(x, i) {
    stopifnot(length(i) == 1)
    if(i %in% 1:4) paste(sprintf("content of  x[[%d]]", i))
    else stop("invalid [[-index, partly out of 1..4")
}
`[.Strange4` <- function(x, i) {
    isM <- length(i) > 1
    if(all(i %in% 1:4)) paste(sprintf("content of  x[%s]",
                                      if(isM) paste0("c(", i, collapse=", ", ")")
                                      else paste0(i, collapse=", ")))
    else stop("invalid indices, partly out of 1..4")
}
L <- structure(as.list(1:6), class="Strange4")
stopifnot(is.list(L), length(L) == 4, length(unclass(L)) == 6)
assertErrV(lapply(L, length))
assertErrV(vapply(L, typeof, ""))
lns <- capture.output(str(L)) # no longer fails
stopifnot(length(lns) == 1+6,  grepl("hidden list", lns[1]))
## str() failed for these and similar in R <= 4.0.x


## PR#18041:  checkRdaFiles(<more-than-1>) $ version
save(pi, file = rda2 <- tempfile(fileext = ".rda"), version = 2)
save(pi, file = rda3 <- tempfile(fileext = ".rda"), version = 3)
stopifnot(identical(2:3, tools::checkRdaFiles(c(rda2, rda3))$version))
## gave '3 3' in R <= 4.0.3


if (l10n_info()$"UTF-8") {
  x <- "d\xc3\xa9faut" # "défaut" flagged as native
  stopifnot(grepl("d.faut", x)) # incorrectly FALSE in in R < 4.1
}


## constructing the names() of quantile():
str(L <- lapply(c(2,3,5,7), function(dig) { options(digits = dig)
    names(quantile(lynx, probs = 1 - 10^(-1:-5))) }))
stopifnot(length(unique(L)) == 1)
## in R <= 4.0.x,  L contained 3 different results


## PR#18079:  sub() & gsub(patt, repl, x) -- when patt is NA
(x <- c(a="abc", b="bd", d=NA, foo="babar"))
stopifnot(exprs = {
    identical(names(x1  <-  sub("a", "_", x)), names(x)) ; x1[["foo"]] == "b_bar"
    identical(names(x2  <- gsub("a", "_", x)), names(x)) ; x2[["foo"]] == "b_b_r"
    identical(names(xN2 <- gsub(NA , "_", x)), names(x)) ; is.na(xN2)
    identical(names(xN1 <-  sub(NA , "_", x)), names(x)) ; is.na(xN1)
})
## NA-pattern did not keep any attributes in R <= 4.0


## svn c80082/80141's change to grep.R broke several of these -- the PR#18063 saga
check_regexetc <- function(txt, fx.ptn, s.ptn, gr.ptn, msg = stop) {
    stopifnot(is.character(txt))
    chkString <- function(ch) {
        if(!is.character(ch)) { str(ch); stop("is not a character") }
        if(length(ch) != 1)   { str(ch); stop("is not of length 1") }
    }
    chkString(fx.ptn)
    chkString( s.ptn)
    chkString(gr.ptn)
    ## ref: result using "character";
    ## x  : from as.character(.) w/ "lost" attributes
    identC <- function(x, ref) {
        attributes(ref) <- attributes(ref)[names(attributes(x))]
        identical(x, ref)
    }

    a2_fns <- expression(grepl,  regexpr, gregexpr,  regexec) # plus possibly:
    if(getRversion() >= "4.1") a2_fns <- c(a2_fns, expression(gregexec))

    exclude <- NA # (the default, used in  factor(.., exclude=*)
    ##
    for (txt_i in 1:3) {
        if (txt_i == 2) { # txt_i  \in {2, 3}  will have  NA in 'txt'
            txt <- c(NA_character_, txt, NA_character_)
        } else if (txt_i == 3) {
            exclude <- NULL
        }
        txt_fkt <- factor(txt, exclude = exclude)
        cat("txt_i = ", txt_i,"; str(<factor>):\n", sep="") ; str(txt_fkt)
        if(chkpre <- !is.null(names(txt_fkt)) && length(levels(txt_fkt)) < length(txt_fkt)) {
            txt_fkt_pre <- txt_fkt[seq(levels(txt_fkt))]
            cat("str(txt_fkt_pre):\n") ; str(txt_fkt_pre)
        }

        for (ptn in c(fx.ptn, s.ptn, gr.ptn, NA_character_)) {
            fixed <- (!is.na(ptn) && ptn == fx.ptn)
            perl  <- (!is.na(ptn) && ptn == gr.ptn)
            ptn_ch <- if(is.na(ptn)) ptn else dQuote(ptn, q=NULL)
            cat(sprintf(" pattern=%16s, fixed=%s, perl=%s:  ", ptn_ch, fixed, perl))
            for (e_2 in a2_fns) {
                f_2 <- eval(e_2)
                f_2s <- as.character(e_2)
                ## when ptn ==  NA_character_  only test grep() & grepl() :
                if(is.na(ptn) && !(f_2s %in% c("grep", "grepl"))) next
                cat(f_2s,"")
                if(!identical(
                    f_2(ptn, txt_fkt, fixed = fixed, perl = perl),
                    f_2(ptn, txt,     fixed = fixed, perl = perl)
                    )) msg(sprintf(
                           "not identical: %s(%s, txt*, fixed=%s, perl=%s)",
                           f_2s, ptn_ch, fixed, perl))
            }

            cat("\n\t grep(*, invert=F/T, value = F/T): ")
            for(iv in list(c(FALSE,FALSE), c(TRUE,FALSE), c(FALSE,TRUE), c(TRUE,TRUE)))
              if(!identical(
                grep(ptn, txt_fkt, fixed = fixed, perl = perl, invert=iv[1], value = iv[2]),
                grep(ptn, txt,     fixed = fixed, perl = perl, invert=iv[1], value = iv[2])
              )) msg(sprintf(
                    "not identical: grep(%s, txt*, fixed=%s, perl=%s, invert=%s, value=%s)",
                    ptn_ch, fixed, perl, iv[1], iv[2]))
            cat("f_3, i.e. *sub() :")
            for (e_3 in expression(sub, gsub)) {
                ##                 ---  -----
                f_3 <- eval(e_3)
                f_3s <- as.character(e_3)
                cat(f_3s,"")
                if(!identC(##identical(
                    res <-
                    f_3(ptn, "@@", txt_fkt, fixed = fixed, perl = perl),
                    f_3(ptn, "@@", txt,     fixed = fixed, perl = perl)
                )) msg(sprintf(
                    "not identical: %s(%s, \"@@\", txt*, fixed=%s, perl=%s)",
                    f_3s, ptn_ch, fixed, perl))
                if(chkpre &&
                   is.null(names(f_3(ptn, "@@", txt_fkt_pre, fixed = fixed, perl = perl))) !=
                   is.null(names(res))
                ) msg(sprintf(
                    "not identical pre: names(%s(%s, \"@@\", txt_fkt*, fixed=%s, perl=%s))",
                    f_3s, ptn_ch, fixed, perl))
            }
            cat("\n")
        }
        cat("--------- finished  txt_i = ", txt_i,"\n")
    }
} ## end{ check_regexetc }

codetools::findGlobals(check_regexetc,merge=FALSE)
## "default check"
txt <- c(
    "The", "licenses", "for", "most", "software", "are",  "designed", "to",
    "take", "away", "your", "freedom",  "to", "share", "and", "change", "it.",
    "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
    "is", "intended", "to", "guarantee", "your", "freedom", "to", "share",
    "and", "change", "free", "software", "--", "to", "make", "sure", "the",
    "software", "is", "free", "for", "all", "its", "users")
names(txt) <- paste0("c", seq_along(txt))
if(FALSE)
 system.time(check_regexetc(txt, fx.ptn = "e", s.ptn = "e.", gr.ptn = "(?<a>e)(?<b>.)", msg=warning))
check_regexetc(txt, fx.ptn = "e", s.ptn = "e.", gr.ptn = "(?<a>e)(?<b>.)")
##============


x <- c("e", "\xe7")
Encoding(x) <- "UTF-8"
x <- factor(c(1, 1, 2), c(1, 2), x)
tools::assertWarning(grep("e", x, fixed = TRUE))
## broken by svn c80136


## "difftime" objects pmin() .. & modifications when "units" differ -- PR#18066
x_hr <- as.difftime(1:10, units = "hours")
y_hr <- as.difftime( 5,   units = "hours")
y_mi <- `units<-`(y_hr, "mins")
x_na <- `[<-`(x_hr, 2L, NA_real_)
stopifnot(exprs = { ## these all are FALSE in R <= 4.0.*
    inherits(rep(y_hr, 5L), "difftime")
    identical(`[<-`(x_hr, 1L, y_hr), `[<-`(x_hr, 1L, y_mi))
    identical(pmin(x_hr, y_hr), pmin(x_hr, y_mi))
    identical(pmin(x_na, y_hr, na.rm = TRUE),
              pmin(x_na, y_mi, na.rm = TRUE))
})
## objects became wrong without warning in R <= 4.0.x

## Bytes Enc may be unset directly to unknown (impossible R <= 4.0.x)
x <- "fa\xE7ile"
Encoding(x) <- "bytes"
xu <- x
Encoding(xu) <- "unknown"
stopifnot(identical(Encoding(c(x, xu)), c("bytes", "unknown")))


## Correctness tests for sorted ALTREP handling of unique/duplicated (PR#17993)


altrep_dup_test <- function(vec, nalast, fromlast, s3class) {
    svec_ar <- sort(vec, na.last = nalast)
    svec_std <- svec_ar
    svec_std[1] <- svec_std[1] ## this clobbers ALTREP-ness
    if(!is.null(s3class)) {
        class(svec_ar) <- s3class
        class(svec_std) <- s3class
    }
    stopifnot(identical(duplicated(svec_ar, fromLast = fromlast),
                        duplicated(svec_std, fromLast = fromlast)),
              identical(unique(svec_ar, fromLast = fromlast),
                        unique(svec_std, fromLast = fromlast)),
              identical(anyDuplicated(svec_ar, fromLast = fromlast),
                        anyDuplicated(svec_std, fromLast = fromlast))
              )
}

altint_dup_check <- function(vec, numna, nalast, fromlast, s3class = NULL) {
     if(length(vec) > 0 && numna > 0) {
         vec[1:numna] = NA_integer_
     }
     altrep_dup_test(vec, nalast = nalast, fromlast = fromlast, s3class = s3class)
}

altint_dup_multicheck <- function(vec, numna, s3class = NULL) {
    altint_dup_check(ivec, numna, FALSE, FALSE, s3class = s3class)
    altint_dup_check(ivec, numna, FALSE, TRUE, s3class = s3class)
    altint_dup_check(ivec, numna, TRUE, FALSE, s3class = s3class)
    altint_dup_check(ivec, numna, TRUE, TRUE, s3class = s3class)
}

altreal_dup_check <- function(vec, numna, numnan, numinf, nalast, fromlast, s3class = NULL) {
    if(length(vec) > 0) {
        if(numna > 0) {
            vec[1:numna] <- NA_real_
        }
        if(numnan > 0) {
            vec[seq(1+numna, 1+numnan)] <- NaN
        }
        if(numinf > 0) {
            infstrt <- 1 + numna + numnan
            vec[seq(infstrt, infstrt + numinf - 1)] <- rep(c(Inf, -Inf), length.out = numinf)
        }
    } ## end length(vec) > 0
    altrep_dup_test(vec, nalast = nalast, fromlast = fromlast, s3class = s3class)
}

altreal_dup_multicheck <- function(vec, numna, numnan, numinf, s3class = NULL) {
    altreal_dup_check(ivec, numna, numnan, numinf, FALSE, FALSE, s3class = s3class)
    altreal_dup_check(ivec, numna, numnan, numinf, FALSE, TRUE, s3class = s3class)
    altreal_dup_check(ivec, numna, numnan, numinf, TRUE, FALSE, s3class = s3class)
    altreal_dup_check(ivec, numna, numnan, numinf, TRUE, TRUE, s3class = s3class)
}

## NB buffer size used by ITERATE_BY_REGION macros is 512, so we need to test
## handling of NAs, NaNs, and Infs around/past that barrier.

set.seed(83); dvec <- round(runif(2000, 1, 20), 1)
ivec <- ceiling(dvec)

altint_dup_multicheck(ivec, 0)
## just before buffer break
altint_dup_multicheck(ivec, 512)
## just after buffer break
altint_dup_multicheck(ivec, 513)
## all nas
altint_dup_multicheck(ivec, 2000)

altreal_dup_multicheck(dvec, 0, 0, 0)
## NA/NaN up to edge of 1 buffer
altreal_dup_multicheck(dvec, 256, 256, 0)
## NA/NaN  crossing buffer barrier
altreal_dup_multicheck(dvec, 256, 257, 0)
## all NA/NaN
altreal_dup_multicheck(dvec, 1000, 1000, 0)
## non-finite filling exactly one buffer on each side
altreal_dup_multicheck(dvec, 0, 0, 1024)
## non-finite  across more than one buffer on both sides
altreal_dup_multicheck(dvec, 0, 0, 1026)
## all non-finite
altreal_dup_multicheck(dvec, 0, 0, 2000)

## sanity checks
## no breakage on length 0 vectors
altint_dup_multicheck(integer(0), 0)
altreal_dup_multicheck(numeric(0), 0, 0, 0)
## works on on length 1 vectors
altint_dup_multicheck(1L, 0)
altreal_dup_multicheck(1.0, 0, 0, 0)


## s3 methods take precedence over altrep methods
## these methods are (very) wrong on purpose so there can be
## no doubt they are hit rather than the altrep code even in the sorted case
duplicated.fake_class <-  function(x, incomparables = FALSE, ...) {
    rep(c(TRUE, FALSE), length.out = length(x))
}

unique.fake_class <- function(x, incomparables = FALSE, ...) {
    x[c(1, 5, length(x))]
}

altint_dup_multicheck(ivec, 0, s3class = "fake_class")
altreal_dup_multicheck(dvec, 0, 0, 0, s3class = "fake_class")


## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
