% File src/library/stats/man/reshape.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{reshape}
\alias{reshape}
\title{Reshape Grouped Data}
\description{
  This function reshapes a data frame between \sQuote{wide} format (with
  repeated measurements in separate columns of the same row) and
  \sQuote{long} format (with the repeated measurements in separate
  rows).
}
\usage{
reshape(data, varying = NULL, v.names = NULL, timevar = "time",
        idvar = "id", ids = 1:NROW(data),
        times = seq_along(varying[[1]]),
        drop = NULL, direction, new.row.names = NULL,
        sep = ".",
        split = if (sep == "") {
            list(regexp = "[A-Za-z][0-9]", include = TRUE)
        } else {
            list(regexp = sep, include = FALSE, fixed = TRUE)}
        )

### Typical usage for converting from long to wide format:

# reshape(data, direction = "wide",
#         idvar = "___", timevar = "___", # mandatory
#         v.names = c(___),    # time-varying variables
#         varying = list(___)) # auto-generated if missing

### Typical usage for converting from wide to long format:

### If names of wide-format variables are in a 'nice' format

# reshape(data, direction = "long",
#         varying = c(___), # vector 
#         sep)              # to help guess 'v.names' and 'times'

### To specify long-format variable names explicitly

# reshape(data, direction = "long",
#         varying = ___,  # list / matrix / vector (use with care)
#         v.names = ___,  # vector of variable names in long format
#         timevar, times, # name / values of constructed time variable
#         idvar, ids)     # name / values of constructed id variable

}
\arguments{
  \item{data}{a data frame}
  \item{varying}{names of sets of variables in the wide format that
    correspond to single variables in long format
    (\sQuote{time-varying}).  This is canonically a list of vectors of
    variable names, but it can optionally be a matrix of names, or a
    single vector of names.  In each case, when \code{direction =
    "long"}, the names can be replaced by indices which are interpreted
    as referring to \code{names(data)}.  See \sQuote{Details} for more
    details and options.}
  \item{v.names}{names of variables in the long format that correspond
    to multiple variables in the wide format.  See \sQuote{Details}.}
  \item{timevar}{the variable in long format that differentiates multiple
    records from the same group or individual.  If more than one record
    matches, the first will be taken (with a warning). }
  \item{idvar}{Names of one or more variables in long format that
    identify multiple records from the same group/individual.  These
    variables may also be present in wide format.}
  \item{ids}{the values to use for a newly created \code{idvar}
    variable in long format.}
  \item{times}{the values to use for a newly created \code{timevar}
    variable in long format.  See \sQuote{Details}.}
  \item{drop}{a vector of names of variables to drop before reshaping.}
  \item{direction}{character string, partially matched to either
    \code{"wide"} to reshape to wide format, or \code{"long"} to reshape
    to long format.}
  \item{new.row.names}{character or \code{NULL}: a non-null value will be
    used for the row names of the result.}
  \item{sep}{A character vector of length 1, indicating a separating
    character in the variable names in the wide format.  This is used for
    guessing \code{v.names} and \code{times} arguments based on the
    names in \code{varying}.  If \code{sep == ""}, the split is just before
    the first numeral that follows an alphabetic character.  This is
    also used to create variable names when reshaping to wide format.}
  \item{split}{A list with three components, \code{regexp},
    \code{include}, and (optionally) \code{fixed}.  This allows an
    extended interface to variable name splitting.  See \sQuote{Details}.}
}
\details{
  Although \code{reshape()} can be used in a variety of contexts, the
  motivating application is data from longitudinal studies, and the
  arguments of this function are named and described in those terms. A
  longitudinal study is characterized by repeated measurements of the
  same variable(s), e.g., height and weight, on each unit being studied
  (e.g., individual persons) at different time points (which are assumed
  to be the same for all units). These variables are called time-varying
  variables. The study may include other variables that are measured
  only once for each unit and do not vary with time (e.g., gender and
  race); these are called time-constant variables.

  A \sQuote{wide} format representation of a longitudinal dataset will
  have one record (row) for each unit, typically with some time-constant
  variables that occupy single columns, and some time-varying variables
  that occupy multiple columns (one column for each time point).  A
  \sQuote{long} format representation of the same dataset will have
  multiple records (rows) for each individual, with the time-constant
  variables being constant across these records and the time-varying
  variables varying across the records.  The \sQuote{long} format
  dataset will have two additional variables: a \sQuote{time} variable
  identifying which time point each record comes from, and an
  \sQuote{id} variable showing which records refer to the same unit.

  The type of conversion (long to wide or wide to long) is determined by
  the \code{direction} argument, which is mandatory unless the
  \code{data} argument is the result of a previous call to
  \code{reshape}.  In that case, the operation can be reversed simply
  using \code{reshape(data)} (the other arguments are stored as
  attributes on the data frame).

  Conversion from long to wide format with \code{direction = "wide"} is
  the simpler operation, and is mainly useful in the context of
  multivariate analysis where data is often expected as a wide-format
  matrix. In this case, the time variable \code{timevar} and id variable
  \code{idvar} must be specified. All other variables are assumed to be
  time-varying, unless the time-varying variables are explicitly
  specified via the \code{v.names} argument.  A warning is issued if
  time-constant variables are not actually constant.

  Each time-varying variable is expanded into multiple variables in the
  wide format.  The names of these expanded variables are generated
  automatically, unless they are specified as the \code{varying}
  argument in the form of a list (or matrix) with one component (or row)
  for each time-varying variable. If \code{varying} is a vector of
  names, it is implicitly converted into a matrix, with one row for each
  time-varying variable. Use this option with care if there are multiple
  time-varying variables, as the ordering (by column, the default in the
  \code{\link{matrix}} constructor) may be unintuitive, whereas the
  explicit list or matrix form is unambiguous.

  %% (FIXME: delete?) If 'direction = "wide"' and no \code{varying} or
  %% \code{v.names} arguments are supplied it is assumed that all variables
  %% except \code{idvar} and \code{timevar} are time-varying.

  Conversion from wide to long with \code{direction = "long"} is the
  more common operation as most (univariate) statistical modeling
  functions expect data in the long format. In the simpler case where
  there is only one time-varying variable, the corresponding columns in
  the wide format input can be specified as the \code{varying} argument,
  which can be either a vector of column names or the corresponding
  column indices. The name of the corresponding variable in the long
  format output combining these columns can be optionally specified as
  the \code{v.names} argument, and the name of the time variables as the
  \code{timevar} argument. The values to use as the time values
  corresponding to the different columns in the wide format can be
  specified as the \code{times} argument.  If \code{v.names} is
  unspecified, the function will attempt to guess \code{v.names} and
  \code{times} from \code{varying} (an explicitly specified \code{times}
  argument is unused in that case).  The default expects variable names
  like \code{x.1}, \code{x.2}, where \code{sep = "."}  specifies to
  split at the dot and drop it from the name.  To have alphabetic
  followed by numeric times use \code{sep = ""}.

  Multiple time-varying variables can be specified in two ways, either
  with \code{varying} as an atomic vector as above, or as a list (or a
  matrix). The first form is useful (and mandatory) if the automatic
  variable name splitting as described above is used; this requires the
  names of all time-varying variables to be suitably formatted in the
  same manner, and \code{v.names} to be unspecified. If \code{varying}
  is a list (with one component for each time-varying variable) or a
  matrix (one row for each time-varying variable), variable name
  splitting is not attempted, and \code{v.names} and \code{times} will
  generally need to be specified, although they will default to,
  respectively, the first variable name in each set, and sequential
  times.

  Also, guessing is not attempted if \code{v.names} is given explicitly,
  even if \code{varying} is an atomic vector. In that case, the number
  of time-varying variables is taken to be the length of \code{v.names},
  and \code{varying} is implicitly converted into a matrix, with one row
  for each time-varying variable. As in the case of long to wide
  conversion, the matrix is filled up by column, so careful attention needs
  to be paid to the order of variable names (or indices) in
  \code{varying}, which is taken to be like \code{x.1}, \code{y.1},
  \code{x.2}, \code{y.2} (i.e., variables corresponding to the same time
  point need to be grouped together).

  The \code{split} argument should not usually be necessary.  The
  \code{split$regexp} component is passed to either
  \code{\link{strsplit}} or \code{\link{regexpr}}, where the latter is
  used if \code{split$include} is \code{TRUE}, in which case the
  splitting occurs after the first character of the matched string.  In
  the \code{\link{strsplit}} case, the separator is not included in the
  result, and it is possible to specify fixed-string matching using
  \code{split$fixed}.
}
\value{
  The reshaped data frame with added attributes to simplify reshaping
  back to the original form.
}
\seealso{\code{\link{stack}}, \code{\link{aperm}};
  \code{\link{relist}} for reshaping the result of
  \code{\link{unlist}}. \code{\link{xtabs}} and
  \code{\link{as.data.frame.table}} for creating contingency tables and
  converting them back to data frames.
}
\examples{
summary(Indometh) # data in long format

## long to wide (direction = "wide") requires idvar and timevar at a minimum
reshape(Indometh, direction = "wide", idvar = "Subject", timevar = "time")

## can also explicitly specify name of combined variable
wide <- reshape(Indometh, direction = "wide", idvar = "Subject",
                timevar = "time", v.names = "conc", sep= "_")
wide

## reverse transformation
reshape(wide, direction = "long")
reshape(wide, idvar = "Subject", varying = list(2:12),
        v.names = "conc", direction = "long")

## times need not be numeric
df <- data.frame(id = rep(1:4, rep(2,4)),
                 visit = I(rep(c("Before","After"), 4)),
                 x = rnorm(4), y = runif(4))
df
reshape(df, timevar = "visit", idvar = "id", direction = "wide")
## warns that y is really varying
reshape(df, timevar = "visit", idvar = "id", direction = "wide", v.names = "x")


##  unbalanced 'long' data leads to NA fill in 'wide' form
df2 <- df[1:7, ]
df2
reshape(df2, timevar = "visit", idvar = "id", direction = "wide")

## Alternative regular expressions for guessing names
df3 <- data.frame(id = 1:4, age = c(40,50,60,50), dose1 = c(1,2,1,2),
                  dose2 = c(2,1,2,1), dose4 = c(3,3,3,3))
reshape(df3, direction = "long", varying = 3:5, sep = "")


## an example that isn't longitudinal data
state.x77 <- as.data.frame(state.x77)
long <- reshape(state.x77, idvar = "state", ids = row.names(state.x77),
                times = names(state.x77), timevar = "Characteristic",
                varying = list(names(state.x77)), direction = "long")

reshape(long, direction = "wide")

reshape(long, direction = "wide", new.row.names = unique(long$state))

## multiple id variables
df3 <- data.frame(school = rep(1:3, each = 4), class = rep(9:10, 6),
                  time = rep(c(1,1,2,2), 3), score = rnorm(12))
wide <- reshape(df3, idvar = c("school", "class"), direction = "wide")
wide
## transform back
reshape(wide)

}
\keyword{manip}
