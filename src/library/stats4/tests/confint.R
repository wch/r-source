## PR#14646

library(stats4)
minusLogL1 <- function(mu, logsigma2)
    N*log(2*pi*exp(logsigma2))/2 + N*(var(x)+(mean(x)-mu)^2)/(2*exp(logsigma2))

minusLogL2 <- function(mu) {
    logsigma2 <- 0;
    N*log(2*pi*exp(logsigma2))/2 + N*(var(x)+(mean(x)-mu)^2)/(2*exp(logsigma2))
}

N <- 100
set.seed(123)
x <- rnorm(N, 0, 1)

fit <- mle(minusLogL1, start = list(mu=0, logsigma2=0))
confint(fit)

fit2 <- mle(minusLogL1, start = list(mu=0), fixed = list(logsigma2=0))
confint(fit2) # failed

fit3 <- mle(minusLogL2, start = list(mu=0))
confint(fit3) # same


## stats::confint.default() now uses the stats4 generics for S4-classed input:
confint.default(fit) # failed in R < 4.6.0 with:
## Error in object$coefficients : $ operator not defined for this S4 class
