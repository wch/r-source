% File src/library/base/man/array2DF.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2023 R Core Team
% Distributed under GPL 2 or later

\name{array2DF}
\title{Convert array to data frame}
\alias{array2DF}

\description{
  \code{array2DF} converts an array, including list arrays commonly
  returned by \code{tapply}, into data frames for use in further
  analysis or plotting functions.
}
\usage{
array2DF(x, responseName = "Value",
         sep = "", base = list(LETTERS),
         simplify = TRUE, allowLong = TRUE)
}
\arguments{
  \item{x}{an array object.}
  \item{responseName}{character string, used for creating column name(s)
    in the result, if required. }
  \item{sep}{character string, used as separator when creating new
    names, if required. }
  \item{base}{character vector, giving an initial set of names to create
    dimnames of \code{x}, if missing. }
  \item{simplify}{logical, whether to attempt simplification of the
    result. }
  \item{allowLong}{logical, specifying whether a long format data frame
    should be returned if \code{x} is a list array and all elements of
    \code{x} are unnamed atomic vectors. Ignored unless \code{simplify =
    TRUE}. }
}
\details{
  The main use of \code{array2DF} is to convert an array, as typically
  returned by \code{\link{tapply}}, into a data frame.

  When \code{simplify = FALSE}, this is similar to
  \code{\link{as.data.frame.table}}, except that it works for list
  arrays as well as atomic arrays. Specifically, the resulting data
  frame has one row for each element of the array, with one column for
  each dimension of the array giving the corresponding
  \code{\link{dimnames}}. The contents of the array are placed in a
  column whose name is given by the \code{responseName} argument. The
  mode of this column is the same as that of \code{x}, usually an atomic
  vector or a list.

  If \code{x} does not have \code{\link{dimnames}}, they are
  automatically created using \code{base} and \code{sep}.
  
  In the default case, when \code{simplify = TRUE}, some common cases
  are handled specially.

  If all components of \code{x} are data frames with identical column
  names (with possibly different numbers of rows), they are
  \code{\link{rbind}}-ed to form the response. The additional columns
  giving \code{dimnames} are repeated according to the number of
  rows, and \code{responseName} is ignored in this case.

  If all components of \code{x} are \emph{unnamed} atomic vectors
  \emph{and} \code{allowLong = TRUE}, each component is treated as a
  single-column data frame with column name given by
  \code{responseName}, and processed as above.

  In all other cases, an attempt to simplify is made by
  \code{\link{simplify2array}}. If this results in multiple unnamed
  columns, names are constructed using \code{responseName} and
  \code{sep}.

}
\value{
  A data frame with at least \code{length(dim(x)) + 1} columns. The
  first \code{length(dim(x))} columns each represent one dimension of
  \code{x} and gives the corresponding values of \code{dimnames}, which
  are implicitly created if necessary. The remaining columns contain the
  contents of \code{x}, after attempted simplification if requested.
}
\seealso{
  \code{\link{tapply}}, \code{\link{as.data.frame.table}},
  \code{\link{split}}, \code{\link{aggregate}}.
}
\examples{
s1 <- with(ToothGrowth,
           tapply(len, list(dose, supp), mean, simplify = TRUE))

s2 <- with(ToothGrowth,
           tapply(len, list(dose, supp), mean, simplify = FALSE))

str(s1) # atomic array
str(s2) # list array

str(array2DF(s1, simplify = FALSE)) # Value column is vector
str(array2DF(s2, simplify = FALSE)) # Value column is list
str(array2DF(s2, simplify = TRUE))  # simplified to vector

### The remaining examples use the default 'simplify = TRUE' 

## List array with list components: columns are lists (no simplification)

with(ToothGrowth,
     tapply(len, list(dose, supp),
     function(x) t.test(x)[c("p.value", "alternative")])) |>
  array2DF() |> str()

## List array with data frame components: columns are atomic (simplified)

with(ToothGrowth,
     tapply(len, list(dose, supp),
     function(x) with(t.test(x), data.frame(p.value, alternative)))) |>
  array2DF() |> str()

## named vectors

with(ToothGrowth,
     tapply(len, list(dose, supp),
            quantile)) |> array2DF()

## unnamed vectors: long format

with(ToothGrowth,
     tapply(len, list(dose, supp),
            sample, size = 5)) |> array2DF()

## unnamed vectors: wide format

with(ToothGrowth,
     tapply(len, list(dose, supp),
            sample, size = 5)) |> array2DF(allowLong = FALSE)

## unnamed vectors of unequal length

with(ToothGrowth[-1, ],
     tapply(len, list(dose, supp),
            sample, replace = TRUE)) |>
  array2DF(allowLong = FALSE)

## unnamed vectors of unequal length with allowLong = TRUE
## (within-group bootstrap)

with(ToothGrowth[-1, ],
     tapply(len, list(dose, supp), sample, replace = TRUE)) |>
  array2DF() |> str()

## data frame input

tapply(ToothGrowth, ~ dose + supp, FUN = with,
       data.frame(n = length(len), mean = mean(len), sd = sd(len))) |>
  array2DF()

}
\keyword{array}
