% File src/library/tools/man/encoded.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{encoded_text_to_latex}
\alias{encoded_text_to_latex}
\title{Translate non-ASCII Text to LaTeX Escapes}
\description{
  Translate non-ASCII characters in text to LaTeX escape sequences.
}
\usage{
encoded_text_to_latex(x,
                      encoding = c("latin1", "latin2", "latin9",
                                   "UTF-8", "utf8"))
}
\arguments{
  \item{x}{a character vector.}
  \item{encoding}{the encoding to be assumed.  \code{"latin9"} is
    officially ISO-8859-15 or Latin-9, but known as latin9 to LaTeX's
    \code{inputenc} package.}
}
\details{
  Non-ASCII characters in \code{x} are replaced by an appropriate LaTeX
  escape sequence, or \samp{?} if there is no appropriate sequence.

  Even if there is an appropriate sequence, it may not be supported by
  the font in use.  Hyphen is mapped to \samp{\\-}.
}
\value{
  A character vector of the same length as \code{x}.
}
\seealso{
  \code{\link{iconv}}
}
\examples{
x <- "fa\xE7ile"
encoded_text_to_latex(x, "latin1")
\dontrun{
## create a tex file to show the upper half of 8-bit charsets
x <- rawToChar(as.raw(160:255), multiple = TRUE)
(x <- matrix(x, ncol = 16, byrow = TRUE))
xx <- x
xx[] <- encoded_text_to_latex(x, "latin1") # or latin2 or latin9
xx <- apply(xx, 1, paste, collapse = "&")
con <- file("test-encoding.tex", "w")
header <- c(
"\\\\documentclass{article}",
"\\\\usepackage[T1]{fontenc}",
"\\\\usepackage{Rd}",
"\\\\begin{document}",
"\\\\HeaderA{test}{}{test}",
"\\\\begin{Details}\\relax",
"\\\\Tabular{cccccccccccccccc}{")
trailer <- c("}", "\\\\end{Details}", "\\\\end{document}")
writeLines(header, con)
writeLines(paste0(xx, "\\\\"), con)
writeLines(trailer, con)
close(con)
## and some UTF_8 chars
x <- intToUtf8(as.integer(
    c(160:383,0x0192,0x02C6,0x02C7,0x02CA,0x02D8,
      0x02D9, 0x02DD, 0x200C, 0x2018, 0x2019, 0x201C,
      0x201D, 0x2020, 0x2022, 0x2026, 0x20AC)),
               multiple = TRUE)
x <- matrix(x, ncol = 16, byrow = TRUE)
xx <- x
xx[] <- encoded_text_to_latex(x, "UTF-8")
xx <- apply(xx, 1, paste, collapse = "&")
con <- file("test-utf8.tex", "w")
writeLines(header, con)
writeLines(paste(xx, "\\\\", sep = ""), con)
writeLines(trailer, con)
close(con)
}}
\keyword{utilities}
