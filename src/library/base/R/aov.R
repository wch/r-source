#### copyright (C) 1998 W. N. Venables and B. D. Ripley
aov <-
  function(formula, data = sys.parent(), projections = FALSE, contrasts =
        NULL, ...)
{
  if(projections) stop("projections are not implemented")
  lmcall <- Call <- match.call()
  lmcall[[1]] <- as.name("lm")
  lmcall$singular.ok <- TRUE
  lmcall$projections <- NULL
  fit <- eval(lmcall, sys.frame(sys.parent()))
  class(fit) <- c("aov", "lm")
  fit$call <- Call
  fit
}
print.aov <-
function(x, intercept = FALSE, tol = .Machine$double.eps^0.5, ...)
{
  if(!is.null(cl <- x$call)) {
    cat("Call:\n   ")
    dput(cl)
  }
  asgn <- x$assign
  effects <- x$effects[seq(along=asgn)]
  nmeffect <- c("(Intercept)", attr(x$terms, "term.labels"))
  nterms <- max(asgn)+1
  df <- ss <- numeric(nterms)
  if(nterms) {
    for(i in seq(nterms)) {
      ai <- asgn==(i-1)
      ai <- !is.na(x$coef) & ai
      df[i] <- sum(ai)
      ss[i] <- sum(effects[ai]^2)
    }
    keep <- df > 0
    if(!intercept) keep[1] <- FALSE
    nmeffect <- nmeffect[keep]
    df <- df[keep]
    ss <- ss[keep]
    nterms <- length(df)
  }
  cat("\nTerms:\n")
  if(nterms == 0) {
    # empty model
    print(matrix(0, 1, 2, dimnames=list("<empty>",
                            c("Sum of Squares", "Deg. of Freedom"))))
    return(invisible(x))
  }
  df.res <- x$df.resid
  if(is.null(df.res))
    df.res <- length(x$resid) - length(asgn)
  if(df.res > 0) {
    resid <- x$resid
    nterms <- nterms + 1
    df[nterms] <- df.res
    ss[nterms] <- sum(resid^2)
    nmeffect[nterms] <- "Residuals"
  }
  print(matrix(c(format(ss), format(df)), 2, nterms, byrow=TRUE,
               dimnames=list(c("Sum of Squares", "Deg. of Freedom"), nmeffect)),
        quote = FALSE, right = TRUE)
  rank <- x$rank
  int <- attr(x$terms, "int")
  nobs <- length(x$residuals) - !(is.null(int) || int == 0)
  rdf <- x$df.resid
  if(is.null(rdf)) rdf <- nobs - rank
  cat("\n")
  if(rdf > 0)
    cat("Residual standard error:", format(sqrt(sum(x$residuals^2)/
                                                rdf)), "\n")
  coef <- x$coef
  R <- x$qr$qr
  R <- R[1:ncol(R), ]
  R[lower.tri(R)] <- 0
  if(rank < (nc <- length(coef)))
    {
      cat(paste(nc - rank, "out of", nc, "effects not estimable\n"))
      R <- R[, 1:rank, drop = FALSE]
    }
  dm <- dim(R)
  d <- seq(1, length = min(dm), by = dm[1] + 1)
  R <- abs(R)
  if(sum(R[ - d])/sum(R[d]) > tol)
    cat("Estimated effects may be unbalanced\n")
  else cat("Estimated effects are balanced\n")
  invisible(x)
}

summary.aov <- function(object, intercept = FALSE, keep.zero.df = TRUE,
                        signif.stars= .Options$show.signif.stars, ...)
{
  asgn <- object$assign
  nterms <- max(asgn)+1
  effects <- object$effects[seq(along=asgn)]
  nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
  resid <- object$residuals
  wt <- object$weights
  if(!is.null(wt)) resid <- resid * wt^0.5
  if(is.null(effects)) {
    df <- nterms  <- 0
    ss <- ms <- numeric(0)
    nmrows <- character(0)
  }
  else {
    nobs <- length(resid)
    nterms <- max(asgn)+1
    df <- ss <- numeric(nterms)
    nmrows <- character(nterms)
    for(i in 1:nterms) {
      ai <- (asgn == i-1) & !is.na(object$coef)
      df[i] <- sum(ai)
      ss[i] <- sum(effects[ai]^2)
      nmrows[i] <- nmeffect[i]
    }
  }
  df.res <- object$df.resid
  if(df.res > 0) {
    nterms <- nterms + 1
    df[nterms] <- df.res
    ss[nterms] <- sum(resid^2)
    nmrows[nterms] <- "Residuals"
  }
  ok <- df > 0
  if(all(ok)) ms <- ss/df
  else {
    ms <- rep(NA, nterms)
    ms[ok] <- ss[ok]/df[ok]
  }
  x <- list(Df = df, "Sum of Sq" = ss, "Mean Sq" = ms)
  if(df.res > 0) {
    TT <- ms/ms[nterms]
    TP <- 1 - pf(TT, df, df.res)
    TT[nterms] <- TP[nterms] <- NA
    x$"F Value" <- TT
    x$"Pr(F)" <- TP
    if(signif.stars)
      x$Signif <- c(symnum(TP[ - nterms], corr = FALSE,
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                           symbols = c("***", "**", "*", ".", " ")),
                    "") ## 'nterms' ~= 'Residuals' have no P-value
  }
  class(x) <- c("anova", "data.frame")
  row.names(x) <- format(nmrows)
  if(!keep.zero.df) x <- x[df > 0, ]
  if(!intercept) x <- x[-1,]
  x
}

coef.aov <- function(x) {
  z <- x$coef
  z[!is.na(z)]
}

alias <- function(object, ...) UseMethod("alias")

alias.formula <- function(object, data, ...)
{
  lm.obj <- if(missing(data)) aov(object) else aov(object, data)
  alias(lm.obj, ...)
}

alias.lm <-
function(object, complete = TRUE, partial = FALSE, ...)
{
  pattern <- function(x, ...) {
    x[abs(x) < 1e-6] <- 0
    if(exists("fractions", mode="function")) fractions(x) else x
  }
  Model <- object$terms
  attributes(Model) <- NULL
  value <- list(Model = Model)
  R <- object$qr$qr
  R <- R[1:ncol(R), ]
  R[lower.tri(R)] <- 0
  d <- dim(R)
  rank <- object$rank
  p <- d[2]
  if(complete) { # full rank, no aliasing
    value$Complete <-
      if(is.null(p) || rank == p) NULL else {
        p1 <- 1:rank
        dn <- dimnames(R)[[2]]
        X <- R[p1, p1]
        Y <-  R[p1, -p1, drop = FALSE]
        beta12 <- as.matrix(qr.coef(qr(X), Y))
        dimnames(beta12) <- list(dn[p1], dn[ -p1])
        pattern(t(beta12))
      }
  }
  if(partial) {
    tmp <- summary.lm(object)$cov.unscaled
    ses <- sqrt(diag(tmp))
    beta11 <- tmp /outer(ses, ses)
    beta11[row(beta11) >= col(beta11)] <- 0
    beta11[abs(beta11) < 1e-6] <- 0
    if(all(beta11 == 0)) beta11 <- NULL
    value$Partial <- beta11
  }
  class(value) <- "listof"
  value
}

print.listof <- function(x, ...)
{
  nn <- names(x)
  ll <- length(x)
  if(length(nn) != ll) nn <- paste("Component", seq(ll))
  for(i in seq(ll)) {
    cat(nn[i], ":\n"); print(x[[i]], ...); cat("\n")
  }
}
