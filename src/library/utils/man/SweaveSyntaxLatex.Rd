% File src/library/utils/man/SweaveSyntaxLatex.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{SweaveSyntaxLatex}
\alias{SweaveSyntaxLatex}
\title{\LaTeX Syntax for Sweave}
\description{
  Definition of the Sweave \LaTeX-style syntax.
}
\usage{SweaveSyntaxLatex}
\format{
  A list of class \code{"SweaveSyntax"} with the following named regular
  expressions (default values in parentheses):
  \describe{
    \item{\code{doc}}{Start of a documentation chunk
      (\code{"^[[:space:]]*\\\\end\\{Scode\\}"}).}
    \item{\code{code}}{Start of a code chunk
      (\code{"^[[:space:]]*\\\\begin\\{Scode\\}\\{?([^}]*)\\}?.*"}).}
    \item{\code{coderef}}{Reference to a named code chunk
      (\code{"^[[:space:]]*\\\\Scoderef\\{([^}]*)\\}.*"}).}
    \item{\code{docopt}}{Command to set options in a documentation chunk
      (\code{"^[[:space:]]*\\\\SweaveOpts\\{([^}]*)\\}"})}
    \item{\code{docexpr}}{Command to use scalar in text
      (\code{"\\\\Sexpr\\{([^}]*)\\}"})}
    \item{\code{extension}}{Input file extensions matched by the syntax
      (\code{"\\.[rsRS]tex$"})}
    \item{\code{syntaxname}}{Command to change the syntax in text
      (\code{"^[[:space:]]*\\\\SweaveSyntax\\{([^}]*)\\}"})}
    \item{\code{input}}{Command to include input files
      (\code{"^[[:space:]]*\\\\SweaveInput\\{([^}]*)\\}"})}
    \item{\code{trans}}{Translation table for
      \code{\link{SweaveSyntConv}}; named list of matched patterns
      replacement strings to create the components above.}
  }
}
\author{
  Friedrich Leisch and R-core.
}
\seealso{
  \sQuote{\href{../doc/Sweave.pdf}{Sweave User Manual}}, a vignette in
  the \pkg{utils} package.

  \code{\link{Sweave}}, \code{\link{SweaveSyntConv}}.
}
\keyword{sysdata}
