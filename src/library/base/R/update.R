# file update.default.R
# copyright (C) 1998 W. N. Venables and B. D. Ripley
#
update.default <- function(object, formula, ...)
{
  if(is.null(call <- object$call))
    stop("need an object with call component")
  this <- match.call()
  allargs <- names(this)[-(1:2)] # remove function, object
  vals <- as.character(this)[-(1:2)]
  names(vals) <- allargs
  oargs <- names(call)[-1]
  posform <- pmatch("formula", allargs, 0)
  if(posform) allargs <- allargs[-posform] # remove formula
  if (!missing(formula))
    call$formula <- update.formula(call$formula, formula)
  for(a in allargs) {
    tt <- paste(" call$", a, "<- substitute(", vals[a], ")", sep="")
    eval(parse(text=tt))
  }
  eval(call, sys.frame(sys.parent()))
}

# Added by KH on 1998/06/16
update.formula <- function (old, new) {
  tmp <-.Internal(update.formula(as.formula(old), as.formula(new)))
  tmp2 <- attr(terms.formula(tmp), "term.labels")
  rhs <- if(length(tmp2)) paste(tmp2, collapse = " + ") else "1"
  formula(paste(deparse(tmp[[2]]), "~", rhs))
}
