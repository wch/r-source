#### Some examples of the KS and Wilcoxon tests

### ------ Kolmogorov Smirnov (KS) --------------

## unrealistic one of PR#14561
ds1 <- c(1.7,2,3,3,4,4,5,5,6,6)
ks.test(ds1, "pnorm", mean = 3.3, sd = 1.55216)
# how on earth can sigma = 1.55216 be known?

# R >= 2.14.0 allows the equally invalid
ks.test(ds1, "pnorm", mean = 3.3, sd = 1.55216, exact = TRUE)

## Try out the effects of rounding
set.seed(123)
ds2 <- rnorm(1000)
ks.test(ds2, "pnorm") # exact = FALSE is default for n = 1000
ks.test(ds2, "pnorm", exact = TRUE)
## next two are still close
ks.test(round(ds2, 2), "pnorm")
ks.test(round(ds2, 2), "pnorm", exact = TRUE)
# now D has doubled, but p-values remain similar (if very different from ds2)
ks.test(round(ds2, 1), "pnorm")
ks.test(round(ds2, 1), "pnorm", exact = TRUE)
summary(w1 <- warnings())


### ------ Wilcoxon (Mann Whitney) --------------

options(nwarnings = 1000)
(alts <- setNames(, eval(formals(stats:::wilcox.test.default)$alternative)))
x0 <- setNames(0:4, paste0("n_", 1L + 0:4))
str(x.set <- list(s0 = lapply(x0, function(m) 0:m),
                  s. = lapply(x0, function(m) c(1e-9, seq_len(m)))))
stats <- setNames(nm = c("statistic", "p.value", "conf.int", "estimate"))
dig.r <- c(Inf, 20, 15, 10, 7, 5); names(dig.r) <- paste0("d.rank=", dig.r)
RL <- lapply(dig.r, function(dig.rank)
             lapply(x.set, ## for all data sets
                    function(xs)
                        lapply(alts, ## for all three alternatives
                               function(alt)
                                   lapply(xs, function(x)
                                       ## try( nolonger: Even with  conf.int = TRUE, do not want errors :
                                       wilcox.test(x, exact=TRUE, conf.int=TRUE, alternative = alt,
                                                   digits.rank = dig.rank)
                                       ## )
                                       ))))
length(ww <- warnings()) # 1 ; was 52 (or 43 for x0 <- 0:3)
if(!identical(w1, ww)) # typically they *are* identical ==> no new warnings
    summary(ww)
## unique(ww) # (were 4 different ones)

## RL is very nested tree of "htest" results
str(RL, max.level = 2)
str(RL[["d.rank=7"]]$s.$two.sided, max.level = 1) # 5 x {list of 9, class "htest"}
str(RL[["d.rank=7"]]$s.$two.sided [1:2], give.attr = FALSE)

cc <- lapply(RL, function(A) lapply(A, function(B) lapply(B, function(bb) lapply(bb, class))))
##N table(unlist(cc))
## in R <= 3.3.1,  with try( .. ) above and only digits.rank=Inf, we got
## htest try-error
##    23         7
stopifnot(identical("htest", unique(unlist(cc))))
##N uc <- unlist(cc[["s0"]]); noquote(names(uc)[uc != "htest"]) ## these 7 cases :
## two.sided1 two.sided2 two.sided3
## less1      less2
## greater1   greater2

##--- How close are the stats of  (0:m)  to those of  (eps, 1:m) ------------

## Contained errors in older versions:
stR <- lapply(stats, function(COMP)
           lapply(RL, function(A)
               lapply(A, function(B)
                   lapply(B, function(bb)
                       lapply(bb, `[[`, COMP) ))))
if(interactive()) {
    str(stR, max.level = 2)
    str(stR$p.value$`d.rank=7`, give.attr = FALSE)
}

## short cut & speedup:
unList <- function(x) .Internal(unlist(x, FALSE,  TRUE))
##                                        recurs. use.nms
## a) P-value s
pv <- stR$p.value

str(apv <- simplify2array(
        lapply(pv, \(L0) simplify2array(
                             lapply(L0, \(L) simplify2array(lapply(L, unList))))))
    )
## num [1:5, 1:3, 1:2, 1:6] 1 1 0.5 0.25 0.125 ...
## - attr(*, "dimnames")=List of 4
##  ..$ : chr [1:5] "n_1" "n_2" "n_3" "n_4" ...
##  ..$ : chr [1:3] "two.sided" "less" "greater"
##  ..$ : chr [1:2] "s0" "s."
##  ..$ : chr [1:6] "d.rank=Inf" "d.rank=20" "d.rank=15" "d.rank=10" ...
object.size(apv) 				# 3144 bytes
object.size(dpv <- as.data.frame.table(apv))	# 8176 bytes
str(dpv)
str(ps. <- apv[,,"s.",])
str(ps0 <- apv[,,"s0",])
(epsC <- .Machine$double.eps) # 2.2e-16
allEQ <- function(x,y) all.equal(x, y, tolerance = 8*epsC)
stopifnot(exprs = {
    0 <= unlist(apv) ; unlist(apv) <= 1 # (no NA, ..)
    ## ps0 does *not* vary with digits.rank :
    identical(dim(print(ups0 <- drop(unique(ps0, MARGIN = 3)))), c(5L, 3L))
    is.function(uNam <- function(x) `names<-`(x, NULL))
    allEQ(uNam(ups0[,"two.sided"]), c(1, 2^(0:-3)))
    allEQ(uNam(ups0[, "greater" ]), 2^(0:-4))
    allEQ(uNam(ups0[,  "less"   ]), c(1,1,1,1,1))
    ## NB: This has changed from R 4.6.0 (now using digits.zap = digits.rank):
    ## ps., eps=1e-9: two possible results, boundary for 1e-9 between digits.rank = 7 and 10
    identical(dim(print(ups. <- unique(ps., MARGIN = 3))), c(5L, 3L, 2L))
    allEQ(ps.[,,"d.rank=Inf"], ps.[,,"d.rank=10"])
    is.function(uNam <- function(x) `dimnames<-`(x, NULL))
    allEQ(uNam(ups.[,"two.sided", ]), 2^cbind(0:-4, c(0L,0:-3)))
    allEQ(uNam(ups.[, "greater" , ]), cbind(2^-(1:5), 2^-c(1,1:4)))
    allEQ(uNam(ups.[,  "less"   , ]), cbind(1, rep(1, 5)))
})


## b) Statistic - Estimate

(astat <- simplify2array(
     lapply(stR[["statistic"]],
            \(L0) simplify2array(
                      lapply(L0, \(L) simplify2array(sapply(L, unList)))))))

(aEst <- simplify2array(
     lapply(stR[["estimate"]],
            \(L0) simplify2array(
                      lapply(L0, \(L) simplify2array(lapply(L, unList)))))))
str(aEst)
 ## num [1:5, 1:3, 1:2, 1:6] 0 0.5 1 1.5 2 0 0.5 1 1.5 2 ...
 ## - attr(*, "dimnames")=List of 4
 ##  ..$ : chr [1:5] "n_1.(pseudo)median" "n_2.(pseudo)median" "n_3.(pseudo)median" "n_4.(pseudo)median" ...
 ##  ..$ : chr [1:3] "two.sided" "less" "greater"
 ##  ..$ : chr [1:2] "s0" "s."
 ##  ..$ : chr [1:6] "d.rank=Inf" "d.rank=20" "d.rank=15" "d.rank=10" ...

## Confidence Interval :
## c)  C.I. extraction
CI <- stR[["conf.int"]]
str(CI, max.level = 2)

if(FALSE) { # printing chCI below is much nicer
  str(aCI <- simplify2array(lapply(CI,
                     function(A) simplify2array(lapply(A,
                             function(B) simplify2array(lapply(B,
                                        function(C) t(simplify2array(C)))))))))
  ##  num [1:5, 1:2, 1:3, 1:2, 1:6] 0 0 0 0 0 ...
  ## - attr(*, "dimnames")=List of 5
  ##  ..$ : chr [1:5] "n_1" "n_2" "n_3" "n_4" ...
  ##  ..$ : NULL
  ##  ..$ : chr [1:3] "two.sided" "less" "greater"
  ##  ..$ : chr [1:2] "s0" "s."
  ##  ..$ : chr [1:6] "d.rank=Inf" "d.rank=20" "d.rank=15" "d.rank=10" ...
  aCI # has *lost* the conf.level which is varying !!
}

## d) confidence interval __printing__
formatCI <- function(ci)
    sprintf("[%g, %g] (%g%%)", ci[[1]], ci[[2]],
	    round(100*attr(ci,"conf.level")))
nx <- length(x0)
Fval <- array("", dim = c(nx, length(alts), length(x.set)))
(chCI <- noquote(vapply(CI, function(D)
    vapply(D, function(ss)
        vapply(ss, function(alt) vapply(alt, formatCI, ""), character(nx)),
        Fval[,,1]),
    Fval)
    ))


##-------- 2-sample tests (working unchanged) ------------------

R2 <- lapply(alts, ## for all three alternatives
             function(alt)
                 lapply(seq_along(x0), function(k)
                         wilcox.test(x = x.set$s0[[k]], y = x.set$s.[[k]],
                                     exact=TRUE, conf.int=TRUE, alternative = alt)))
length(w2 <- warnings()) # 1  (was '27')
if(!identical(w2, w1)) # typically they *are* identical ==> no new warnings
    summary(w2)


table(uc2 <- unlist(c2 <- lapply(R2, function(A) lapply(A, class))))
stopifnot(uc2 == "htest")

stR2 <- lapply(stats,
               function(COMP)
                   lapply(R2, function(A) lapply(A, `[[`, COMP)))

lapply(stats[-3], ## -3: "conf.int" separately
       function(ST) sapply(stR2[[ST]], unlist))

noquote(sapply(stR2[["conf.int"]], function(.) vapply(., formatCI, "")))

