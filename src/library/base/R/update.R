# file update.default.R
# copyright (C) 1998 W. N. Venables and B. D. Ripley
#
update.default <-
function (object, formula, ..., evaluate=T) 
{
  if (is.null(call <- object$call)) 
    stop("need an object with call component")
  extras <- match.call(expand.dots=F)$...
  if (!missing(formula)) 
    call$formula <- update.formula(call$formula, formula)
  for (a in names(extras)) {
    if(extras[[a]] == "") term <- NULL else term <- extras[[a]]
    if(!is.null(call[[a]])) call[[a]] <- term
    else {
      # can't ADD list components with [[a]] in R
      term <-
        if(is.character(term)) deparse(term) else as.character(term)
      tt <- paste(" call$", a, "<- substitute(", term, ")", sep = "")
      eval(parse(text = tt))
    }
  }
  if(evaluate) eval(call, sys.frame(sys.parent()))
  else call
}

# Added by KH on 1998/06/16
update.formula <- function (old, new) {
  tmp <-.Internal(update.formula(as.formula(old), as.formula(new)))
  tmp2 <- attr(terms.formula(tmp), "term.labels")
  rhs <- if(length(tmp2)) paste(tmp2, collapse = " + ") else "1"
  formula(paste(deparse(tmp[[2]]), "~", rhs))
}
