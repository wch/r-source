vcov <- function(object, ...) UseMethod("vcov")

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

vcov.gls <- function (object, ...) object$varBeta

vcov.lme <- function (object, ...) object$varFix
