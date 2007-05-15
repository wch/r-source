vcov <- function(object, ...) UseMethod("vcov")

## The next three have to call the method, as classes which
## inherit from "glm" need not have summary methods which
## inherit from "summary.glm".
vcov.glm <- function(object, ...)
{
    so <- summary.glm(object, corr=FALSE, ...)
    so$dispersion * so$cov.unscaled
}

vcov.lm <- function(object, ...)
{
    so <- summary.lm(object, corr=FALSE)
    so$sigma^2 * so$cov.unscaled
}

vcov.mlm <- function(object, ...)
{
    so <- summary.mlm(object, corr=FALSE)[[1]]
    kronecker(estVar(object), so$cov.unscaled, make.dimnames = TRUE)
}

## These should be moved to nlme
vcov.gls <- function (object, ...) object$varBeta

vcov.lme <- function (object, ...) object$varFix
