#### lm, glm, aov, etc --- typically *strict* tests (no *.Rout.save)

options(warn = 2) # all warnings must be asserted below

data(mtcars)
mtcar2 <- within(mtcars, {
    mpg_c <- mpg * (1+am) + 5
    am <- factor(am)
})
fm2 <- glm(disp ~ am * mpg + mpg_c, data = mtcar2)
c2 <- coef(fm2)
V2 <- vcov(fm2)
jj <- !is.na(c2)
stopifnot(names(which(!jj)) == "am1:mpg"
	, identical(length(c2), 5L), identical(dim(V2), c(5L,5L))
	, all.equal(c2[jj],    coef(fm2, complete=FALSE))
	, all.equal(V2[jj,jj], vcov(fm2, complete=FALSE))
	, all.equal(c2[jj], c(`(Intercept)`= 626.0915, am1 = -249.4183,
			      mpg = -33.74701, mpg_c = 10.97014),
		    tol = 7e-7)# 1.01e-7 [F26 Lnx 64b]
)


### predict.lm(<rank-deficient>,  newdata = *) -- PR#15072, PR#16158 --------------

## constructed "exactly" rank-deficient
x1 <- -4:4
x2 <- c(-2,1,-1,2, 0,
        2,-1,1,-2)
x3 <- 3*x1 - 2*x2
x4 <- x2 - x1 + 4
y <- 1 + x1 + x2 + x3 + x4 + c(-.5,.5,.5,-.5, 0,
                               .5,-.5,-.5,.5)
cbind(x1,x2,x3,x4,y)
## Fit a model
mod1234 <- lm(y ~ x1 + x2 + x3 + x4)
(al <- alias(mod1234)) # x3: 3*x1 - 2*x2  \\  x4 :  4 -x1 +x2
stopifnot(all.equal(rbind(x3 = c(0,  3,-2),
                          x4 = c(4, -1, 1)),
                    unclass(al$Complete), check.attributes=FALSE))
## new.x
new.x <- data.frame(
    row.names = LETTERS[1:6],
    x1 = c(3, 6, 6, 0, 0, 1),
    x2 = c(1, 2, 2, 0, 0, 2),
    x3 = c(7,14,14, 0, 0, 3),
    x4 = c(2, 4, 0, 4, 0, 4))
## where do we have the same aliasing subspace?
new.ok <- with(new.x, (x4 == 4 - x1 + x2) &
                      (x3 == 3*x1 - 2*x2))
which(new.ok) # 1 3 4
## old (hard-wired) R <= 4.2.x behavior:
tools::assertWarning(ps <- predict(mod1234, newdata=new.x, rankdeficient = "simple"), verbose=TRUE)
ps1 <-   predict(mod1234, newdata=data.frame(x1,x2,x3,x4), rankdeficient = "warnif") # *not* warning anymore
## new
                    (pN <- predict(mod1234, new.x, rankdeficient = "NA"))
tools::assertWarning(pN.<- predict(mod1234, new.x, rankdeficient = "NAwarn"))
## "compromise": old predictions with extra info (no warning):
(pne <- predict(mod1234, new.x, rankdeficient = "non-estim"))
stopifnot(exprs = {
    identical(pN, pN.)
    all.equal(fitted(mod1234), ps1, tol = 2e-15) # seen 3.11e-16
    identical(i.ne <- attr(pne, "non-estim"),
              c(B = 2L, E = 5L, F = 6L))
    which(!new.ok) == i.ne
    is.na(pN[i.ne])
    identical(ps[-i.ne], pN[-i.ne])
    identical(unname(ps), `attributes<-`(pne, NULL))
})


d8 <- data.frame(
    y = c(747625803, -74936705, -750056726, -299805697,
          76131520, -225971209, 301836031, 2249594776, 300581863, -2999324198,
          450274906, -600962167, 1800954652, 900083298, -1498452810),
    X1 = c(149999999, -225000002, -149999999, 149999998, 225000002,
           -675000006, -149999998, 449999997, 900000008, -599999996,
           1350000012, 299999996, -899999988, -449999994, -299999998),
    X2 = c(300000000.5, -149999999, -300000000.5, 1, 149999999,
           -449999997, -1, 900000001.5, 599999996, -1200000002,
           899999994, 2, -6, -3, -600000001),
    X3 = c(-1, 149999998, 1, -150000002, -149999998,
           449999994, 150000002, -3, -599999992, 4,
           -899999988, -300000004, 900000012, 450000006, 2))
coef(fm8.  <- lm(y ~ . -1, data = d8)) # the one for X3 is NA
cf8. <- c(X1 = -1.999854802642, X2 = 3.499496934397, X3 = NA)
          all.equal(cf8., coef(fm8.), tol=0)# -> "Mean rel..diff.: ~ 3e-15
stopifnot(all.equal(cf8., coef(fm8.)))
coef(fm8.9 <- lm(y ~ . -1, data = d8, tol = 1e-9)) # no NA , but "instable" -- not too precise
cf8.9 <- c(X1 = 45822.830422, X2 = -22908.915871, X3 = 45824.830295)
all.equal(cf8.9, coef(fm8.9), tol=0)# -> "Mean rel..diff.: 5.3e-9 | 5.15e-12
## was < 2e-8 in R 4.2.2
## x86_64 Linux/gcc12 gives ca 5e-12
## vanilla M1mac gives 6.16e-11, Accelerate on M1 macOS gives 3.99e-10;
## Debian with "generic" (i.e. not R's) BLAS/Lapack *still* gave 5.2985e-09 (?!)
stopifnot(all.equal(cf8.9, coef(fm8.9), tol = 7e-9))

## predict :
nd <- d8[,-1] + rep(outer(c(-2:2),10^(1:3)), 3) # 5 * 9 = 45 = 15 * 3 (nrow * ncol)
row.names(nd) <- LETTERS[1:nrow(nd)]
tools::assertWarning(verbose=TRUE, # "... rank-deficient .. consider predict(., rankdeficient="NA")
 ps <- predict(fm8. , newdata=nd, rankdeficient = "simple") )
tools::assertWarning(verbose=TRUE, # "... rank-deficient ..  attr(*, "non-estim") has doubtful cases
 ps.<- predict(fm8. , newdata=nd) ) # default
pN  <- predict(fm8. , newdata=nd, rankdeficient = "NA")
pne <- predict(fm8. , newdata=nd, rankdeficient = "non-estim")
p.9 <- predict(fm8.9, newdata=nd)
print(digits=9, cbind(ps, pne, pN, p.9))
all.equal(p.9, ps, tol=0)# 0.035..
dropAtt <- function(x) `attributes<-`(x, NULL)
stopifnot(exprs = {
    ps == ps. # numbers;
    identical(unname(ps), dropAtt(ps.))
    identical(ps., pne) # both have "non-estim"
    identical(i.ne <- attr(pne, "non-estim"),
              c(K = 11L, L = 12L, N = 14L, O = 15L))
    is.na(pN[i.ne])
    identical(ps[-i.ne], pN[-i.ne])
})

## play with tol
str(tls <- sort(outer(c(1,2,4), 10^-(9:5))))
nT <- length(tls <- setNames(tls, formatC(tls)))
pls <- t(sapply(tls, function(TL) predict(fm8. , newdata=nd, tol = TL, rankdeficient = "NA")))
stopifnot(is.finite(plsLst <- pls[nT,])) # (no NA)
plsLst
sweep(pls, 2L, plsLst, `-`)
## This *is* monotone in tol -- still somewhat amazing how much changes
## within two factors of 4 (of tol), i.e., between 2e-7 ... 4e-6
##        A  B  C  D  E  F  G  H  I  J  K  L  M  N  O
## 1e-09 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## 2e-09 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## 4e-09 NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
## 1e-08 NA NA  0 NA NA NA NA  0 NA NA NA NA NA NA NA
## 2e-08 NA NA  0 NA NA NA NA  0 NA NA NA NA  0 NA NA
## 4e-08 NA NA  0  0 NA NA NA  0 NA NA NA NA  0 NA NA
## 1e-07  0  0  0  0  0 NA NA  0  0 NA NA NA  0 NA NA
## 2e-07  0  0  0  0  0 NA NA  0  0  0 NA NA  0 NA NA
## 4e-07  0  0  0  0  0  0 NA  0  0  0 NA NA  0 NA NA
## 1e-06  0  0  0  0  0  0  0  0  0  0 NA NA  0 NA NA
## 2e-06  0  0  0  0  0  0  0  0  0  0  0 NA  0  0 NA
## 4e-06  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## 1e-05  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## 2e-05  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
## 4e-05  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0

(iFi <- apply(pls, 2, function(.) which.max(is.finite(.))))
## A  B  C  D  E  F  G  H  I  J  K  L  M  N  O
## 7  7  4  6  7  9 10  4  7  8 11 12  5 11 12
stopifnot(exprs = {
    ## checking monotonicity: each column is  (NA NA ... NA | p_i p_i ... p_i)
    vapply(seq_along(tls),
           function(i) length(unique(pls[iFi[i]:nT, i])) == 1L,
           NA)
    ## allow 1 off :
  3 <= iFi
  iFi <= 13
})


## __FIXME__
## predict(*, ... type="terms" .. ) does *not* obey   rankdeficient=".."
