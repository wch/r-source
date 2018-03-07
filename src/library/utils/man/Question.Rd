% File src/library/utils/man/Question.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{Question}
\alias{Question}
\alias{?}
\title{Documentation Shortcuts}
\description{
  These functions provide access to documentation.
  Documentation on a topic with name \code{name} (typically, an \R
  object or a data set) can be displayed by either \code{help("name")} or
  \code{?name}.
}
% it is in fact `?`(e1, e2)
\usage{
\special{?topic}

\special{type?topic}
}
\arguments{
  \item{topic}{Usually, a \link{name} or character string specifying the
    topic for which help is sought.

    Alternatively, a function call to ask for documentation on a
    corresponding S4 method: see the section on S4 method documentation.
    The calls \code{\var{pkg}::\var{topic}} and
    \code{\var{pkg}:::\var{topic}} are treated specially, and look for
    help on \code{topic} in package \pkg{\var{pkg}}.
  }
  \item{type}{the special type of documentation to use for this topic;
    for example, if the type is \code{class}, documentation is
    provided for the class with name \code{topic}.
    See the Section \sQuote{S4 Method Documentation} for the uses of
    \code{type} to get help on formal methods, including
    \code{methods?\var{function}} and \code{method?\var{call}}.
  }
}
\details{
  This is a shortcut to \code{\link{help}} and uses its default type of help.

  Some topics need to be quoted (by \link{backtick}s) or given as a
  character string.  There include those which cannot syntactically
  appear on their own such as unary and binary operators,
  \code{function} and control-flow \link{reserved} words (including
  \code{if}, \code{else} \code{for}, \code{in}, \code{repeat},
  \code{while}, \code{break} and \code{next}).  The other \code{reserved}
  words can be used as if they were names, for example \code{TRUE},
  \code{NA} and \code{Inf}.
}

\section{S4 Method Documentation}{
  Authors of formal (\sQuote{S4}) methods can provide documentation
  on specific methods, as well as overall documentation on the methods
  of a particular function.  The \code{"?"} operator allows access to
  this documentation in three ways.

  The expression \code{methods?\var{f}} will look for the overall
  documentation methods for the function \code{\var{f}}.  Currently,
  this means the documentation file containing the alias
  \code{\var{f}-methods}.

  There are two different ways to look for documentation on a
  particular method.  The first is to supply the \code{topic} argument
  in the form of a function call, omitting the \code{type} argument.
  The effect is to look for documentation on the method that would be
  used if this function call were actually evaluated. See the examples
  below.  If the function is not a generic (no S4 methods are defined
  for it), the help reverts to documentation on the function name.

  The \code{"?"} operator can also be called with \code{doc_type} supplied
  as \code{method}; in this case also, the \code{topic} argument is
  a function call, but the arguments are now interpreted as specifying
  the class of the argument, not the actual expression that will
  appear in a real call to the function.  See the examples below.

  The first approach will be tedious if the actual call involves
  complicated expressions, and may be slow if the arguments take a
  long time to evaluate.  The second approach avoids these
  issues, but you do have to know what the classes of the actual
  arguments will be when they are evaluated.

  Both approaches make use of any inherited methods; the signature of
  the method to be looked up is found by using \code{selectMethod}
  (see the documentation for \code{\link{getMethod}}).  A limitation
  is that methods in packages (as opposed to regular functions) will only 
  be found if the package exporting them is on the search list, even 
  if it is specified explicitly using the \code{?package::generic()} 
  notation.  
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{help}}

  \code{\link{??}} for finding help pages on a vague topic.
}
\examples{
?lapply

?"for"                  # but quotes/backticks are needed
?`+`

?women                  # information about data set "women"

\dontrun{
require(methods)
## define a S4 generic function and some methods
combo <- function(x, y) c(x, y)
setGeneric("combo")
setMethod("combo", c("numeric", "numeric"), function(x, y) x+y)

## assume we have written some documentation
## for combo, and its methods ....

?combo  # produces the function documentation

methods?combo  # looks for the overall methods documentation

method?combo("numeric", "numeric")  # documentation for the method above

?combo(1:10, rnorm(10))  # ... the same method, selected according to
                         # the arguments (one integer, the other numeric)

?combo(1:10, letters)    # documentation for the default method
}}
\keyword{documentation}
