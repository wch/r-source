## Subject: [Bug 18417] factanal returns useless SS loading values
## Date: Sat, 22 Mar 2025 23:21:10 +0000
## --- Comment #21 from Coen Bernaards

## create unrotated loadings matrix
data(ability.cov, package="datasets")
res <- factanal(covmat = ability.cov$cov, factors = 3, rotation = "none")

if(FALSE) {
    ## create the output from oblimin of GPArotation as a list;
    require(GPArotation)
    set.seed(100)
    obli100 <- oblimin(res$loadings, Tmat = Random.Start(3), eps = 1e-8, maxit = 10000)
    ## keep only what's needed:
    obli100[setdiff(names(obli100),c("loadings", "Phi", "Th", "orthogonal"))] <- NULL
    saveRDS(obli100, "obli100.rds")
}

obli100 <- readRDS("obli100.rds") # in ../stats/tests/
if(getRversion() >= "4.6") { ## perform the factanal using this oblimin rotation [ rotation now may be _function_
  resO100 <- factanal(covmat = ability.cov$cov, factors = 3, rotation = \(...) obli100)
} else { ## old R:
  obli_FN <- function(...) obli100
  resO100 <- factanal(covmat = ability.cov$cov, factors = 3, rotation = "obli_FN")
}
resO100 # print.factanal(..)  --> print.loadings(..)

oriL <-   res     $ loadings
ldns   <- resO100 $ loadings
rotmat <- resO100 $ rotmat
# no longer can the unrotated matrix be recreated. the max abs diff between
# unrotated and recreated unrotated loadings should be near zero but is not:
print.default(dev <- ldns %*% solve(rotmat) - oriL)
stopifnot(max(abs(dev)) < 1e-9) # max|dev| was 2.000982 (R <= 4.4.x), now 2.22e-16 [Lnx x86_64, gcc]; 6e-12 on M1mac

ldns # prints loadings too
## check the "SS loadings .... " line
L <- ldns
Phi <- attr(L, "covariance")
(SSld <- diag(Phi %*% crossprod(L)))
##      1.896492 1.124306 1.037336
stopifnot(all.equal(c(1.8964924541, 1.1243062406, 1.0373362639), SSld)) # seen  8.49e-12
