% File src/library/methods/man/languageEl.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{languageEl}
\alias{languageEl}
\alias{languageEl<-}
\alias{isGrammarSymbol}
\title{Elements of Language Objects}
\description{
  Internal routines to support some operations on language objects.
}
\usage{
languageEl(object, which)

isGrammarSymbol(symbol)

}
\section{Summary of Functions}{
  \describe{
    \item{\code{languageEl}:}{
      extract an element of a language object, consistently for
      different kinds of objects.

      The 1st., etc. elements of a function are the corresponding formal
      arguments, with the default expression if any as value.

      The first element of a call is the name or the function object
      being called.

      The 2nd, 3rd, etc. elements are the 1st, 2nd, etc. arguments
      expressions.  Note that the form of the extracted name is
      different for R and S-Plus.  When the name (the first element) of
      a call is replaced, the languageEl replacement function coerces a
      character string to the internal form for each system.

      The 1st, 2nd, 3rd elements of an \code{if} expression are the
      test, first, and second branch.

      The 1st element of a \code{for} object is the name (symbol) being
      used in the loop, the second is the expression for the range of
      the loop, the third is the body of the loop.

      The first element of a \code{while} object is the loop test, and
      the second the body of the loop.
    }

    \item{\code{isGrammarSymbol}:}{
      Checks whether the symbol is part of the grammar.
      Don't use this function directly.
    }
  }
}
\keyword{internal}
