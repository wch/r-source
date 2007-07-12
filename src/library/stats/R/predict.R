predict <- function(object,...) UseMethod("predict")

## This is not used anywhere anymore, is it ?
## It would only work with objects very much like  "lm", would it?
if(FALSE)
predict.default <- function (object, ...) {
    namelist <- list(...)
    names(namelist) <- substitute(list(...))[-1]
    m <- length(namelist)
    X <- as.matrix(namelist[[1]])
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
    names(ip) <- paste("P", 1:n, sep = "")
    for (i in 1:n)
	ip[i] <- sum(X[i, ] * (object$covmat %*% X[i, ]))
    stderr1 <- sqrt(ip)
    stderr2 <- sqrt(object$rms^2 + ip)
    tt <- qt(0.975, object$df)
    predictor + tt * cbind(Predicted=0,
                           "Conf lower"=-stderr1, "Conf upper"=stderr1,
                           "Pred lower"=-stderr2, "Pred upper"=stderr2)
}
