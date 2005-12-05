## deprecated in 2.2.1
mauchley.test <- function(object, Sigma=diag(nrow=p),
                          T = Thin.row(proj(M)-proj(X)),
                          M = diag(nrow=p),
                          X = ~0,
                          idata=data.frame(index=seq(length=p)),...)
{
	.Deprecated("mauchly.test")
	UseMethod("mauchly.test")
}
