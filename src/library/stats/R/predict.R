#  File src/library/stats/R/predict.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

predict <- function(object,...) UseMethod("predict")

## This is not used anywhere anymore, is it ?
## It would only work with objects very much like  "lm", would it?
if(FALSE)
predict.default <- function (object, ...) {
    namelist <- list(...)
    names(namelist) <- substitute(list(...))[-1L]
    m <- length(namelist)
    X <- as.matrix(namelist[[1L]])
    if (m > 1)
	for (i in (2:m)) X <- cbind(X, namelist[[i]])
    if (object$intercept)
	X <- cbind(rep(1, NROW(X)), X)
    k <- NCOL(X)
    n <- NROW(X)
    if (length(object$coefficients) != k)
	stop("wrong number of predictors")
    predictor <- X %*% object$coefficients
    ip <- numeric(n)
    names(ip) <- paste("P", 1L:n, sep = "")
    for (i in 1L:n)
	ip[i] <- sum(X[i, ] * (object$covmat %*% X[i, ]))
    stderr1 <- sqrt(ip)
    stderr2 <- sqrt(object$rms^2 + ip)
    tt <- qt(0.975, object$df)
    predictor + tt * cbind(Predicted=0,
                           "Conf lower"=-stderr1, "Conf upper"=stderr1,
                           "Pred lower"=-stderr2, "Pred upper"=stderr2)
}
