% File src/library/tools/man/pkg2HTML.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2024 R Core Team
% Distributed under GPL 2 or later

\name{pkg2HTML}
\alias{pkg2HTML}
\title{ Rd Converters }
\description{
  Generate a single-page HTML reference manual from the Rd sources
  contained in an installed or source R package.
}
\usage{
pkg2HTML(package, dir = NULL, lib.loc = NULL,
         outputEncoding = "UTF-8",
         stylesheet = file.path(R.home("doc"), "html", "R-nav.css"),
         hooks = list(pkg_href = function(pkg) sprintf("\%s.html", pkg)),
         texmath = getOption("help.htmlmath"),
         prism = TRUE,
         out = NULL,
         toc_entry = c("title", "name"),
         ...,
         Rhtml = FALSE,
         mathjax_config = file.path(R.home("doc"), "html", "mathjax-config.js"),
         include_description = TRUE)
}
\arguments{
  \item{package}{ typically a character string giving the name of an
    installed package. Can also be a file path or URL pointing to a source
    tarball (this feature is experimental).
  }
  \item{dir}{ character string giving the path to a directory containing
    an installed or source package.
  }
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}. Passed on to
    \code{\link{find.package}} and \code{\link{Rd_db}}.
  }
  \item{outputEncoding}{ character string; see \code{\link{Rd2HTML}}. }
  \item{stylesheet}{ character string giving URL containing CSS style
    information.
  }
  \item{hooks}{A list of functions controlling details of
    output. Currently the only component used is \code{pkg_href}, which
    is used to determine the output HTML file path given a package name
    as input.
  }
  \item{texmath}{ character string controlling math rendering library to
    be used, either \code{"katex"} or \code{"mathjax"}. The default is
    to use \code{"katex"}, unless the package appears to use the
    \code{\link[mathjaxr]{mathjaxr}} package in at least one of its
    documentation files. The support for \I{MathJax} is experimental and may
    not work well.
  }
  \item{prism}{ logical flag controlling code highlighting, as described in
    \code{\link{Rd2HTML}}.
  }
  \item{out}{a filename or connection object to which to write the
    output.  By default (\code{NULL}), the filename is inferred from
    \code{hooks$pkg_href}, which defaults to \file{\var{pkg}.html}.}
  \item{toc_entry}{ Determines whether the entry for a help page in the
    table of contents is the name of the help page or its title. }
  \item{...}{additional arguments, passed on to
    \code{\link{Rd2HTML}}. The \code{stages} argument, if specified, is
    passed on to \code{\link{Rd_db}}.
  }
  \item{Rhtml}{logical: whether the output is intended to be a \I{Rhtml}
    file that can be processed using \CRANpkg{knitr}. If \code{TRUE}, the
    examples section is wrapped inside a \code{rcode} block.
  }
  \item{mathjax_config}{ character string giving path of file containing
    configuration instructions for \I{MathJax}. Relevant only if
    \code{texmath = "mathjax"}.
  }
  \item{include_description}{ logical flag indicating whether the output
    should begin with the contents of the \code{DESCRIPTION} file.
  }
}

\details{
  The \code{pkg2HTML} function is intended to produce a single-page HTML
  reference manual for a given package, with links to other
  packages. The URLs of links to external packages are controlled by the
  provided hooks.

  The handling of \verb{\Sexpr}-s are necessarily incomplete, but can be
  controlled to some extent by specifying the \code{stages}
  argument. Best results are likely for installed packages.
}

\value{
  The name of the output file (invisibly).
}
\author{
  Deepayan Sarkar
}
\seealso{
  \code{\link{parse_Rd}}, \code{\link{Rd_db}},
  \code{\link{Rd2HTML}}.
}
\examples{
\donttest{
pkg2HTML("tools", out = tempfile(fileext = ".html")) |> browseURL()
}
}
\keyword{ documentation }
