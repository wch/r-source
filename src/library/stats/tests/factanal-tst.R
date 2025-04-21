## Subject: [Bug 18417] factanal returns useless SS loading values
## Date: Sat, 22 Mar 2025 23:21:10 +0000
## --- Comment #21 from Coen Bernaards

## create unrotated loadings matrix
data(ability.cov, package="datasets")
res <- factanal(covmat = ability.cov$cov, factors = 3, rotation = "none")

if(FALSE) {
    ## create the output from oblimin of GPArotation as a list;
    require(GPArotation)
    ## keep only what's needed from rotation:
    keepImp <- function(rot)
        `[<-`(rot, setdiff(names(rot), c("loadings", "Phi", "Th", "orthogonal")), NULL)
    set.seed(100)
    obli100 <- keepImp(oblimin(res$loadings, Tmat = Random.Start(3), eps = 1e-8, maxit = 10000))
    set.seed(108)
    obli108 <- keepImp(oblimin(res$loadings, eps = 1e-7, randomStarts = 1))
    saveRDS(obli100, "obli100.rds")
    saveRDS(obli108, "obli108.rds")
}

obli100 <- readRDS("obli100.rds") # in ../stats/tests/
## perform the factanal using this oblimin rotation
if(getRversion() >= "4.5") { # rotation now may be _function_
  resO100 <- factanal(covmat = ability.cov$cov, factors = 3, rotation = \(...) obli100)
} else { ## old R:
  obli_FN <- function(...) obli100
  resO100 <- factanal(covmat = ability.cov$cov, factors = 3, rotation = "obli_FN")
}
resO100 # print.factanal(..)  --> print.loadings(..)

oriL <-   res     $ loadings
ldns   <- resO100 $ loadings
rotmat <- resO100 $ rotmat
# max |diff| between unrotated and recreated unrotated loadings should be near zero:
print.default(dev <- ldns %*% solve(rotmat) - oriL)
stopifnot(max(abs(dev)) < 1e-9) # max|dev| was 2.000982 (R <= 4.4.x),
##                                now 2.22e-16 [Lnx x86_64, gcc]; 6e-12 on M1mac

ldns # prints loadings too
## check the "SS loadings .... " line
L <- ldns
Phi <- attr(L, "covariance")
print(digits = 11, SSld <- diag(Phi %*% crossprod(L)))
stopifnot(all.equal(c(1.8964924541, 1.1243062406, 1.0373362639), SSld)) # seen  8.49e-12

##----------- Checking rotmat in the 108 case ------------------
## [Bug 18886] New: R 4.5.0: rotmat not correctly updated  in factanal sortLoadings
## 20 Apr 2025 / Reporter: coen.bernaards@gmail.com

obli108 <- readRDS("obli108.rds")

res108 <- factanal(covmat = ability.cov$cov, factors = 3, rotation = \(...) obli108)
res108
(Cov <- tcrossprod( solve(res108$rotmat) ))
## this difference should be near 0 but was not:
(Dcov <- attr(res108$loadings, "covariance")  -  Cov) # seen 3.33e-16, 4.44e-16 [Lnx x86_64, gcc]
stopifnot(max(abs(Dcov)) < 1e-9)
