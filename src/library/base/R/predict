predict <- function(fit,...) UseMethod("predict")

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
                 if (length(object$coef) != k) 
                   stop("Wrong number of predictors")
                 predictor <- X %*% object$coef
                 ip <- real(NROW(X))
                 for (i in (1:NROW(X))) ip[i] <- sum(X[i, ] * 
                       (object$covmat %*% X[i, ]))
                 stderr1 <- sqrt(ip)
                 stderr2 <- sqrt(object$rms^2 + ip)
                 tt <- qt(0.975, object$df)
                 conf.l <- predictor - tt * stderr1
                 conf.u <- predictor + tt * stderr1
                 pred.l <- predictor - tt * stderr2
                 pred.u <- predictor + tt * stderr2
                 z <- cbind(predictor, conf.l, conf.u, pred.l, pred.u)
                 rownames(z) <- paste("P", 1:NROW(X), sep = "")
                 colnames(z) <- c("Predicted", "Conf lower", "Conf upper",
                                  "Pred lower", "Pred upper")
                 z
}
