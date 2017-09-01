## Regression tests for R >= 3.4.0

pdf("reg-tests-1d.pdf", encoding = "ISOLatin1.enc")
.pt <- proc.time()

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
    else { ## query the  locale
        if(.Platform$OS.type != "windows") {
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
stopifnot(identical(m1z, mz),
	  identical(m1z == 1L,             iNA),
          identical(match(z, NA, 0) == 1L, iNA),
	  identical(mz[mz != 1L], c(2L, 4L, 9L, 10L, 12L, 2L, 2L, 2L, 9L)))
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
stopifnot(identical(deparse(z, 200, control = "digits17"),
                    paste0("c(", paste(fz0, collapse=", "), ")")),
          print((sum(nchar(dz2)) - 2) / length(z2)) < 22, # much larger in <= 3.3.0
          ## deparse <-> parse equivalence, 17 digits should be perfect:
	  all.equal(z2, eval(parse(text = dz2)), tolerance = 3e-16)) # seen 2.2e-35 on 32b
## deparse() for these was "ugly" in R <= 3.3.x


## length(environment(.)) == #{objects}
stopifnot(identical(length(      baseenv()),
                    length(names(baseenv()))))
## was 0 in R <= 3.3.0


## "srcref"s of closures
op <- options(keep.source = TRUE)# as in interactive use
getOption("keep.source")
stopifnot(identical(function(){}, function(){}),
          identical(function(x){x+1},
                    function(x){x+1})); options(op)
## where all FALSE in 2.14.0 <= R <= 3.3.x because of "srcref"s etc


## PR#16925, radix sorting INT_MAX w/ decreasing=TRUE and na.last=TRUE
## failed ASAN check and segfaulted on some systems.
data <- c(2147483645L, 2147483646L, 2147483647L, 2147483644L)
stopifnot(identical(sort(data, decreasing = TRUE, method = "radix"),
                    c(2147483647L, 2147483646L, 2147483645L, 2147483644L)))


## as.factor(<named integer>)
ni <- 1:2; Nni <- names(ni) <- c("A","B")
stopifnot(identical(Nni, names(as.factor(ni))),
	  identical(Nni, names(   factor(ni))),
	  identical(Nni, names(   factor(ni+0))), # +0 : "double"
	  identical(Nni, names(as.factor(ni+0))))
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
stopifnot(identical(ddd, diff(d, differences = 2)),
	  identical(d3d, diff(d, differences = 3)))
stopifnot(vapply(ldd, units, "") == "secs",
	  vapply(ldd, class, "") == "difftime",
	  lengths(c(list(d), ldd)) == c(11:8, 11-7))
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
stopifnot(identical(c(tN1), c(`NA`=1L, `NaN`=1L, NbN=1L))
        , identical(c(tN),  structure(2:1, .Names = c("A", NA)))
        , identical(c(tN.), structure(2L,  .Names = "A"))
)
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

stopifnot(
    vapply(lt, function(i) all(vapply(i, class, "") == "table"), NA),
    vapply(ly, function(i) all(vapply(i, class, "") == "table"), NA),
    vapply(lxy,function(i) all(vapply(i, class, "") == "table"), NA)
    , identical((ltNA  <- lt [["NA"  ]]), lt [["NANaN"]])
    , identical((ltNl  <- lt [["NULL"]]), lt [["none" ]])
    , identical((lyNA  <- ly [["NA"  ]]), ly [["NANaN"]])
    , identical((lyNl  <- ly [["NULL"]]), ly [["none" ]])
    , identical((lxyNA <- lxy[["NA"  ]]), lxy[["NANaN"]])
    , identical((lxyNl <- lxy[["NULL"]]), lxy[["none" ]])
)
## 'NULL' behaved special (2.8.0 <= R <= 3.3.1)  and
##  *all* tables in l0 and lt were == (1 0 2) !
ltN1 <- ltNA[[1]]; lyN1 <- lyNA[[1]]; lxyN1 <- lxyNA[[1]]
lNl1 <- ltNl[[1]]; lyl1 <- lyNl[[1]]; lxyl1 <- lxyNl[[1]]

stopifnot(
    vapply(names(ltNA) [-1], function(n) identical(ltNA [[n]], ltN1 ), NA),
    vapply(names(lyNA) [-1], function(n) identical(lyNA [[n]], lyN1 ), NA),
    vapply(names(lxyNA)[-1], function(n) identical(lxyNA[[n]], lxyN1), NA),
    identical(lyN1, lyl1),
    identical(2L, dim(ltN1)), identical(3L, dim(lyN1)),
    identical(3L, dim(lNl1)),
    identical(dimnames(ltN1), list(x = c("1","2"))),
    identical(dimnames(lNl1), list(x = c("1","2", NA))),
    identical(dimnames(lyN1), list(y = paste(4:6))),
    identical(  1:0    , as.vector(ltN1)),
    identical(c(1:0,3L), as.vector(lNl1)),
    identical(c(1:2,1L), as.vector(lyN1))
    , identical(c(1L, rep(0L, 5)), as.vector(lxyN1))
    , identical(dimnames(lxyN1), c(dimnames(ltN1), dimnames(lyN1)))
    , identical(c(1L,1:0), as.vector(table(3:1, exclude=1, useNA = "always")))
    , identical(c(1L,1L ), as.vector(table(3:1, exclude=1)))
)

x3N <- c(1:3,NA)
(tt <- table(x3N, exclude=NaN))
stopifnot(tt == 1, length(nt <- names(tt)) == 4, is.na(nt[4])
	, identical(tt, table(x3N, useNA = "ifany"))
	, identical(tt, table(x3N, exclude = integer(0)))
	, identical(t3N <- table(x3N), table(x3N, useNA="no"))
	, identical(c(t3N), setNames(rep(1L, 3), as.character(1:3)))
	##
	, identical(c("2" = 1L), c(table(1:2, exclude=1) -> t12.1))
	, identical(t12.1, table(1:2, exclude=1, useNA= "no"))
	, identical(t12.1, table(1:2, exclude=1, useNA= "ifany"))
	, identical(structure(1:0, .Names = c("2", NA)),
		    c(     table(1:2, exclude=1, useNA= "always")))
)
options(op) # (revert to default)


## contour() did not check args sufficiently
tryCatch(contour(matrix(rnorm(100), 10, 10), levels = 0, labels = numeric()),
         error = function(e) e$message)
## caused segfault in R 3.3.1 and earlier


## unique.warnings() needs better duplicated():
.tmp <- lapply(list(0, 1, 0:1, 1:2, c(1,1), -1:1), function(x) wilcox.test(x))
stopifnot(length(uw <- unique(warnings())) == 2)
## unique() gave only one warning in  R <= 3.3.1


op <- options(warn = 2)# no warnings allowed

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
stopifnot(identical(levels(print(droplevels(dn))), c(L3, NA))
	  ## only XX must be dropped; R <= 3.3.1 also dropped <NA>
	  , identical(levels(droplevels(f)), L3)
	  , identical(levels(droplevels(d)), L3) # do *not* add <NA> here
	  , identical(droplevels(d ), d [, drop=TRUE])
	  , identical(droplevels(f ), f [, drop=TRUE])
	  , identical(droplevels(dn), dn[, drop=TRUE])
	  )


## summary.default() no longer rounds (just its print() method does):
set.seed(0)
replicate(256, { x <- rnorm(1); stopifnot(summary(x) == x)}) -> .t
replicate(256, { x <- rnorm(2+rpois(1,pi))
    stopifnot(min(x) <= (sx <- summary(x)), sx <= max(x))}) -> .t
## was almost always wrong in R <= 3.3.x


## NULL in integer arithmetic
i0 <- integer(0)
stopifnot(identical(1L + NULL, 1L + integer()),
	  identical(2L * NULL, i0),
	  identical(3L - NULL, i0))
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
stopifnot(
    identical( m, m + 1 ), identical( m,  m + 1 [0]), identical( m,  m + NULL),
    identical(Im, Im+ 1L), identical(Im, Im + 1L[0]), identical(Im, Im + NULL),
    identical(m, m + 2:3), identical(Im, Im + 2:3),
    identical(Lm, m & 1),  identical(Lm,  m | 2:3),
    identical(Lm, m & TRUE[0]), identical(Lm, Lm | FALSE[0]),
    identical(Lm, m & NULL), # gave Error (*only* place where NULL was not allowed)
    identical(Lm, m > 1), identical(Lm, m > .1[0]), identical(Lm, m > NULL),
    identical(Lm, m <= 2:3)
)
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
stopifnot(identical(m1 + NULL, n0), # as "always"
	  identical(m1 +  n0 , n0), # as "always"
	  identical(m1 & NULL, l0), # ERROR in R <= 3.3.x
	  identical(m1 &  l0,  l0), # ERROR in R <= 3.3.x
	  identical(m1 > NULL, l0), # as "always"
	  identical(m1 >  n0 , l0)) # as "always"
## m2 was slightly different:
stopifnot(identical(m2 + NULL, n0), # ERROR in R <= 3.3.x
	  identical(m2 +  n0 , n0), # ERROR in R <= 3.3.x
	  identical(m2 & NULL, l0), # ERROR in R <= 3.3.x
	  identical(m2 &  l0 , l0), # ERROR in R <= 3.3.x
	  identical(m2 == NULL, l0), # as "always"
	  identical(m2 ==  n0 , l0)) # as "always"


## strcapture()
stopifnot(identical(strcapture("(.+) (.+)",
                               c("One 1", "noSpaceInLine", "Three 3"),
                               proto=data.frame(Name="", Number=0)),
                    data.frame(Name=c("One", NA, "Three"),
                               Number=c(1, NA, 3))))


## PR#17160: min() / max()  arg.list starting with empty character
TFT <- 1:3 %% 2 == 1
stopifnot(
    identical(min(character(), TFT), "0"),
    identical(max(character(), TFT), "1"),
    identical(max(character(), 3:2, 5:7, 3:0), "7"),
    identical(min(character(), 3:2, 5:7), "2"),
    identical(min(character(), 3.3, -1:2), "-1"),
    identical(max(character(), 3.3, 4:0), "4"))
## all gave NA in R <= 3.3.0


## PR#17147: xtabs(~ exclude) fails in R <= 3.3.1
exc <- exclude <- c(TRUE, FALSE)
xt1 <- xtabs(~ exclude) # failed : The name 'exclude' was special
xt2 <- xtabs(~ exc)
xt3 <- xtabs(rep(1, length(exclude)) ~ exclude)
noCall  <- function(x) structure(x, call = NULL)
stripXT <- function(x) structure(x, call = NULL, dimnames = unname(dimnames(x)))
stopifnot(
    identical(dimnames(xt1), list(exclude = c("FALSE", "TRUE"))),
    identical(names(dimnames(xt2)), "exc"),
    all.equal(stripXT(xt1), stripXT(xt2)),
    all.equal(noCall (xt1), noCall (xt3)))
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


## PR#17186 - Sys.timezone() on some Debian-derived platforms
(S.t <- Sys.timezone())
if(is.na(S.t) || !nzchar(S.t)) stop("could not get timezone")
## has been NA_character_  in Ubuntu 14.04.5 LTS


## format()ing invalid hand-constructed  POSIXlt  objects
d <- as.POSIXlt("2016-12-06"); d$zone <- 1
tools::assertError(format(d))
d$zone <- NULL
stopifnot(identical(format(d),"2016-12-06"))
d$zone <- "CET" # = previous, but 'zone' now is last
tools::assertError(format(d))
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
set.seed(7); rof <- sample(of, 12, replace=TRUE)
stopifnot(identical(pmax(rof, of), ordered(pmax(c(rof), c(of)), labels=levels(rof)) -> pmar),
	  identical(pmax(of, rof), pmar),
	  identical(pmin(rof, of), ordered(pmin(c(rof), c(of)), labels=levels(rof)) -> pmir),
	  identical(pmin(of, rof), pmir),
	  identical(pmin(rof, 5), ordered(pmin(c(rof), 2), levels=1:3, labels=levels(rof))),
	  identical(pmax(rof, 6), ordered(pmax(c(rof), 3), levels=1:3, labels=levels(rof))),
	  identical(pmax(rof, 1), rof),
	  identical(pmin(rof, 6), rof),
	  identical(pmax(of, 5, rof), ordered(pmax(c(of),2L,c(rof)), levels=1:3,
                                              labels=levels(of)))
	  )
## these were "always" true .. but may change (FIXME ?)
stopifnot(
    identical(of,   pmin(of, 3)) # what? error? at least warning?
    ,
    identical(pmar, pmax(of, 3, rof))
)
## pmin/pmax() of 0-length S3 classed  [PR #17200]
for(ob0 in list(I(character()), I(0[0]), I(0L[0]),
                structure(logical(), class="L"),
                structure(character(), class="CH"))) {
    stopifnot(identical(ob0, pmax(ob0, ob0)),
              identical(ob0, pmin(ob0, ob0)),
              identical(ob0, pmin(ob0, "")),
              identical(ob0, pmax(ob0, "")))
}
## pmin()/pmax() of matching numeric data frames
mUSJ <- data.matrix(dUSJ <- USJudgeRatings)
stopifnot(
    identical(              pmin(dUSJ, 10 - dUSJ),
              as.data.frame(pmin(mUSJ, 10 - mUSJ))),
    identical(              pmax(dUSJ, 10 - dUSJ),
              as.data.frame(pmax(mUSJ, 10 - mUSJ))))
## had failed for a while.   Note however :
d1 <- data.frame(y0 = 0:3 +1/2) ; (d1.2 <- d1[1:2, , drop=FALSE])
stopifnot(## FIXME: The 'NA's really are wrong
    identical(pmax(d1,2),     data.frame(y0 = c(2, NA, 2.5, 3.5)))
   ,
    identical(pmax(d1, 3-d1), data.frame(y0 = .5+c(2, 1:3)))
   ,
    identical(pmax(d1.2, 2),  data.frame(y0 = c(2, NA)))
   ,
    identical(pmax(d1.2, 2-d1.2),data.frame(y0=c(1.5,1.5)))
   ,
    identical(pmin(d1, 2),    data.frame(y0 = c(.5+0:1, NA,NA)))
   ,
    identical(pmin(d1, 3-d1), data.frame(y0 = .5+c(0, 1:-1)))
   ,
    identical(pmin(d1.2, 2),  data.frame(y0 = c(.5, 1.5)))
   ,
    identical(pmin(d1.2, 2-d1.2),data.frame(y0 = c(.5,.5)))
)
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
stopifnot(
    identical(1,         seq.int(to=1,  by=1 )),
    identical(1:2,       seq.int(to=2L, by=1L)),
    identical(c(1L, 3L), seq.int(1L, 3L, length.out=2))
)
## the first was missing(.), the others "double" in R < 3.4.0
tools::assertError(seq(1,7, by = 1:2))# gave warnings in R < 3.4.0
## seq() for <complex> / <integer>
stopifnot(all.equal(seq(1+1i, 9+2i, length.out = 9) -> sCplx,
                    1:9 + 1i*seq(1,2, by=1/8)),
          identical(seq(1+1i, 9+2i, along.with = 1:9), sCplx),
          identical(seq(1L, 3L, by=1L), 1:3)
)
## had failed in R-devel for a few days
D1 <- as.Date("2017-01-06")
D2 <- as.Date("2017-01-12")
seqD1 <- seq.Date(D1, D2, by = "1 day")
stopifnot(identical(seqD1, seq(D1, D2, by = "1 days")),
## These two work "accidentally" via seq -> seq.default + "Date"-arithmetic
          identical(seqD1, seq(by = 1, from = D1, length.out = 7)),
          identical(seqD1, seq(by = 1,   to = D2, length.out = 7))
## swap order of (by, to) ==> *FAILS* because directly calls seq.Date() - FIXME?
        , TRUE ||
          identical(seqD1, seq(to = D2,  by = 1, length.out = 7))
          )
## had failed in R-devel for a couple of days
stopifnot(identical(seq(9L, by = -1L, length.out = 4L), 9:6),
	  identical(seq(9L, by = -1L, length.out = 4 ), 9:6))
## for consistency, new in R >= 3.4.0


## Underflow happened when parsing small hex constants PR#17199
stopifnot(
    as.double("0x1.00000000d0000p-987") > 0,   # should be 7.645296e-298
    as.double("0x1.0000000000000p-1022") > 0,  # should be 2.225074e-308
    as.double("0x1.f89fc1a6f6613p-974") > 0    # should be 1.23456e-293
)
##


## format.POSIX[cl]t() after print.POSIXct()
dt <- "2012-12-12 12:12:12"
x <- as.POSIXct(dt, tz = "GMT")
stopifnot(identical(format(x), dt))
(Sys.t <- Sys.timezone())
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
	      array(c(4, 14, 15, 17, 9, 3,   0, 2, 5, 6, 3, 0,	 1, 4, 3, 3, 1, 0,
		      7,  8,  7,  6, 0, 1,   2, 1, 4, 4, 1, 0,	 2, 0, 4, 6, 1, 0),
		    dim = dim(xt))))

DF <- as.data.frame(UCBAdmissions)
xt <- xtabs(Freq ~ Gender + Admit, DF)
stopifnot(identical(asArr(xt),
		    array(c(1198, 557, 1493, 1278), dim = c(2L, 2L),
			  dimnames = list(Gender = c("Male", "Female"),
					  Admit = c("Admitted", "Rejected")))))
options(na.action = "na.omit")
DN <- DF; DN[cbind(6:9, c(1:2,4,1))] <- NA; DN

tools::assertError(# 'na.fail' should fail :
	   xtabs(Freq ~ Gender + Admit, DN, na.action = na.fail))
xt. <- xtabs(Freq ~ Gender + Admit, DN)
xtp <- xtabs(Freq ~ Gender + Admit, DN, na.action = na.pass)
xtN <- xtabs(Freq ~ Gender + Admit, DN, addNA = TRUE)
stopifnot(
    identical(asArr(xt - xt.), as_A(c(120,17, 207, 8 ), xt)),
    identical(asArr(xt - xtp), as_A(c(120,17, 207, NA), xt)), # not ok in R <= 3.3.2
    identical(asArr(-xtN + rbind(cbind(xt, 0), 0)),
              as_A(c(120, 17, -17, 207, NA, 0, -327, 0, 0), xtN))
)
## 'sparse = TRUE requires recommended package Matrix
if(requireNamespace('Matrix')) {
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
(tt <- xtabs(ncases ~ ., ee))
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
stopifnot(
    grepl("List of ", capture.output(str(xx, list.len = 7, max.level = 1))),
    length(str2) == 35, sum(grepl("list output truncated", str2)) == 2,
    vapply(paste("List of", lengths(xx)), function(pat) any(grepl(pat, str2)), NA)
)
## wrongly showed '[list output truncated]'  in R < 3.4.0


## stopifnot(all.equal(.)) message abbreviation
msg <- tryCatch(stopifnot(all.equal(rep(list(pi),4), list(3.1, 3.14, 3.141, 3.1415))),
		error = conditionMessage)
writeLines(msg)
stopifnot(length(strsplit(msg,"\n")[[1]]) == 1+3+1)
## was wrong for months in R-devel only


## available.packages() (not) caching in case of errors
tools::assertWarning(ap1 <- available.packages(repos = "http://foo.bar"))
tools::assertWarning(ap2 <- available.packages(repos = "http://foo.bar"))
stopifnot(nrow(ap1) == 0, identical(ap1, ap2))
## had failed for a while in R-devel (left empty *.rds file)


## rep()/rep.int() : when 'times' is a list
stopifnot(identical(rep    (4,   list(3)), c(4,4,4)),
          identical(rep.int(4,   list(3)), c(4,4,4)),
          identical(rep.int(4:5, list(2,1)), c(4L,4:5)),
          identical(rep    (4:5, list(2,1)), c(4L,4:5)))
## partly failed in R 3.3.{2,3}


## quantile(ordered(.)) - error message more directly useful
OL <- ordered(sample(LETTERS, 20, replace=TRUE))
(e <- tryCatch(quantile(OL), error = conditionMessage))
stopifnot(grepl("type.*1.*3", e),# typically works in several locales
	  is.ordered(quantile(OL, type = 1)),
	  is.ordered(quantile(OL, type = 3)))
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
et1 <- tryCatch(t1(), error=identity)
if(englishMsgs)
    stopifnot(identical("the ... list does not contain any elements",
			conditionMessage(et1)))
## previously gave   "'nthcdr' needs a list to CDR down"
et0   <- tryCatch(t0(),  error=identity); (mt0   <- conditionMessage(et0))
et2.0 <- tryCatch(t2(),  error=identity); (mt2.0 <- conditionMessage(et2.0))
et2.1 <- tryCatch(t2(1), error=identity); (mt2.1 <- conditionMessage(et2.1))
if(englishMsgs)
    stopifnot(grepl("indexing '...' with .* index 0", mt0),
	      identical("the ... list does not contain 2 elements", mt2.0),
	      identical(mt2.0, mt2.1))
tools::assertError(t0(1))
tools::assertError(t0(1, 2))
## the first gave a different error msg, the next gave no error in R < 3.5.0


## stopifnot(e1, e2, ...) .. evaluating expressions sequentially
one <- 1
try(stopifnot(3 < 4:5, 5:6 >= 5, 6:8 <= 7, one <- 2))
stopifnot(identical(one, 1))
## all the expressions were evaluated in R <= 3.4.x
et <- tryCatch(stopifnot(0 < 1:10, is.numeric(..vaporware..)),
	       error=identity)
stopifnot(identical(print(conditionCall(et))[[1]],
		    quote(is.numeric)))
## call was the full 'stopifnot(..)' in R < 3.5.0


## path.expand shouldn't translate to local encoding PR#17120
## This has been fixed on Windows, but not yet on Unix non-UTF8 systems
if(.Platform$OS.type == "windows") {
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
stopifnot(is.null(names(a1$Population)),
	  is.null(names(a2$Population)),
	  identical(unlist(a2$Population), a1$Population),
	  all.equal(unlist(a2$Population),
		    c(8802.8, 4208.12, 7233.83, 4582.57, 1360.5, 2372.17, 970.167),
		    tol = 1e-6))
## in R <= 3.4.1, a2$Population had spurious names


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
## This slightly changed - for the better - in R >= 3.5.0 :
ff <- factor(c(NA,2,3), levels = c(2, NA), labels = c("my", NA), exclude = NULL)
stopifnot( ## all these but the last were TRUE "forever" :
    identical(as.vector(ff), as.character(ff)),
    identical(as.vector(ff), c(NA, "my", NA)),
    identical(capture.output(ff), c("[1] <NA> my   <NA>",
				    "Levels: my <NA>")),
    identical(factor(ff),
	      structure(c(NA, 1L, NA), .Label = "my", class = "factor")),
    identical(factor(ff, exclude=NULL),
	      structure(c(2L, 1L, 2L), .Label = c("my", NA), class = "factor")),
    identical(as.integer(ff), # <- new in R 3.5.0 : c(2, 1, 2); before was c(2, 1, NA)
	      as.integer(factor(ff, exclude=NULL))))


## within.list({ .. rm( >=2 entries ) }) :
L <- list(x = 1, y = 2, z = 3)
stopifnot(identical(within(L, rm(x,y)), list(z = 3)))
## has failed since R 2.7.2 patched (Aug. 2008) without any noticeable effect
sortN <- function(x) x[sort(names(x))]
LN <- list(y = 2, N = NULL, z = 5)
stopifnot(
    identical(within(LN, { z2 <- z^2 ; rm(y,z,N) }),
              list(z2 = 5^2)) ## failed since Aug. 2008
   ,
    identical(within(LN, { z2 <- z^2 ; rm(y,z) }),
              list(N = NULL, z2 = 5^2)) ## failed for a few days in R-devel
   , # within.list() fast version
    identical(sortN(within(LN, { z2 <- z^2 ; rm(y,z) }, keepAttrs=FALSE)),
              sortN(list(N = NULL, z2 = 5^2)))
)


## write.csv did not signal an error if the disk was full PR#17243
if (file.access("/dev/full", mode = 2) == 0) { # Not on all systems...
    # Large writes should fail mid-write
    stopifnot(inherits(tryCatch(write.table(data.frame(x=1:1000000),
                                            file = "/dev/full"),
                                error = identity),
                       "error"))
    # Small writes should fail on closing
    stopifnot(inherits(tryCatch(write.table(data.frame(x=1),
                                                file = "/dev/full"),
                                    warning = identity),
                       "warning"))
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
stopifnot(identical('-"\\n"', cq <- capture.output(qq)),
          identical(5L, nchar(cq)),
          identical(6L, nchar(capture.output(quote(("\t"))))))
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
stopifnot(sapply(-5:-16, function(E) identical(NC(xE(10^E)), ncE)),
	  identical(NC(xE(1e-4)), c(Sturges = 4, Scott = 2, FD = 8550)),
	  identical(NC(xE(1e-3)), c(Sturges = 4, Scott = 2, FD =  855)))
## for these, nclass.FD() had "exploded" in R <= 3.4.1
## Extremely large diff(range(.)) :
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


## qr.coef(qr(X, LAPACK=TRUE)) when X has column names, etc
X <- cbind(int = 1,
           c2 = c(2, 8, 3, 10),
           c3 = c(2, 5, 2, 2)); rownames(X) <- paste0("r", 1:4)
y <- c(2,3,5,7); yc <- as.complex(y)
q.Li <- qr(X);              cfLi <- qr.coef(q.Li, y)
q.LA <- qr(X, LAPACK=TRUE); cfLA <- qr.coef(q.LA, y)
q.Cx <- qr(X + 0i);         cfCx <- qr.coef(q.Cx, y)
e1 <- tryCatch(qr.coef(q.Li, y[-4]), error=identity); e1
e2 <- tryCatch(qr.coef(q.LA, y[-4]), error=identity)
stopifnot(
    all.equal(cfLi,    cfLA , tol = 1e-14)# 6.376e-16 (64b Lx)
   ,all.equal(cfLi, Re(cfCx), tol = 1e-14)#  (ditto)
   ,identical(conditionMessage(e1), conditionMessage(e2)))
## 1) cfLA & cfCx had no names in R <= 3.4.1
## 2) error messages were not consistent


## invalid user device function  options(device = *) -- PR#15883
graphics.off() # just in case
options(device=function(...){}) # non-sense device
tools::assertError(plot.new(), verbose = TRUE)
if(no.grid <- !("grid" %in% loadedNamespaces())) requireNamespace("grid")
tools::assertError(grid::grid.newpage(), verbose = TRUE)
if(no.grid) unloadNamespace("grid")
## both errors gave segfaults in R <= 3.4.1


## readRDS(textConnection())
abc <- c("a", "b", "c"); tmpC <- ""
zz <- textConnection('tmpC', 'wb')
saveRDS(abc, zz, ascii = TRUE)
sObj <- paste(textConnectionValue(zz), collapse='\n')
close(zz); rm(zz)
stopifnot(identical(abc, readRDS(textConnection(tmpC))),
          identical(abc, readRDS(textConnection(sObj))))
## failed in R 3.4.1 only


## Ops (including arithmetic) with 0-column data frames:
d0 <- USArrests[, FALSE]
stopifnot(identical(d0, sin(d0))
        , identical(d0, d0 + 1), identical(d0, 2 / d0) # failed
        , all.equal(sqrt(USArrests), USArrests ^ (1/2)) # now both data frames
        , is.matrix(m0 <- 0 < d0)
        , identical(dim(m0), dim(d0))
        , identical(dimnames(m0)[1], dimnames(d0)[1])
        , identical(d0 & d0, m0)
          )
## all but the first failed in R < 3.5.0


## pretty(x, n) for n = <large> or  large diff(range(x)) gave overflow in C code
(fLrg <- Filter(function(.) . < 9e307, c(outer(1:8, 10^(0:2))*1e306)))
pL  <- vapply(fLrg, function(f)length(pretty(c(-f,f), n = 100,  min.n = 1)), 1L)
pL
pL3 <- vapply(fLrg, function(f)length(pretty(c(-f,f), n = 10^3, min.n = 1)), 1L)
pL3
stopifnot(71 <= pL, pL <= 141, 81 <= pL[-7], # not on Win-64: pL[-15] <= 121,
          701 <= pL3, pL3 <= 1401) # <= 1201 usually
## in R < 3.5.0, both had values as low as 17


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
stopifnot(identical(fret, 27)
	 , identical(hret, 27)
	 , identical(res, 21)
)
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
stopifnot(identical(fret, 28)
	 , identical(gret, 2)
	 , identical(res, 2)
)
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
stopifnot(identical(res, 3)
	, identical(mret, 3)
	, identical(hret, 3)
	, identical(lret, 29)
	, identical(uvarg, 1)
	, identical(uvret, 3)
)
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
stopifnot(identical(res, 3)
	, identical(fret, 31)
	, identical(mycallCCret, 3)
	, identical(funret, 31)
)
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
stopifnot(identical(res, 5))
stopifnot(identical(x, 2))
stopifnot(identical(fret1, 4))
stopifnot(identical(fret2, 5))

## keep at end
rbind(last =  proc.time() - .pt,
      total = proc.time())
