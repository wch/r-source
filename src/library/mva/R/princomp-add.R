# file princomp-add.R
# copyright (C) 1998 W. N. Venables and B. D. Ripley
#
predict.princomp <- function(object, newdata,...)
{
  if (missing(newdata)) return(object$scores)
  scale(newdata, object$center, object$scale) %*% object$loadings  
}

print.princomp <- function(x, ...)
{
  cat("Standard deviations:\n")
  print(x$sdev, ...)
  invisible(x)
}

summary.princomp <- 
function(object, loadings = F, cutoff = 0.1, digits=3, ...)
{
  vars <- object$sdev^2
  vars <- vars/sum(vars)
  cat("Importance of components:\n")
  print(rbind("Standard deviation" = object$sdev,
              "Proportion of Variance" = vars,
              "Cumulative Proportion" = cumsum(vars)))
  if(loadings) {
    cat("\nLoadings:\n")
    cx <- format(round(object$loadings, digits=digits))
    cx[abs(object$loadings) < cutoff] <-
      substring("       ", 1, nchar(cx[1,1]))
    print(cx, quote = F, ...)
  }
  invisible(object)
}

plot.princomp <- function(...) screeplot(...)

screeplot <-
function(x, npcs=min(10, length(x$sdev)), type=c("barplot", "lines"),
         main = deparse(substitute(x)), ...)
{
  eval(main)
  type <- match.arg(type)
  pcs <- x$sdev^2
  xp <- seq(length=npcs)
  if(type=="barplot") barplot(pcs[xp], names = names(pcs[xp]), 
       main = main, ylab = "Variances", ...)
  else {
    plot(xp, pcs[xp], type = "b", axes = F, main = main, xlab="",
            ylab = "Variances", ...)
    axis(2)
    axis(1, at = xp, labels = names(pcs[xp]))
  }
  invisible()
}

loadings <- function(x) x$loadings

