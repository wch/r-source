\name{addmargins}

\alias{addmargins}
\concept{totals}
\concept{margins}

\title{
  Puts arbitrary margins on multidimensional tables or arrays.
} 

\description{
  For a given table one can specify which of the classifying factors to
  expand by one or more levels to hold margins to be calculated. One may for
  example form sums and means over the first dimension and medians over the
  second. The resulting table will then have two extra levels for the first
  dimension and one extra level for the second. The default is to sum over
  all margins in the table. Other possibilities may give results that
  depend on the order in which the margins are computed. This is flagged
  in the printed output from the function. 
}

\usage{
  addmargins(A, margin = 1:length(dim(A)), FUN = sum, quiet = FALSE)
}

\arguments{
  \item{A}{A table or array. The function uses the presence of the
    \code{"dim"} and \code{"dimnames"} attributes of \code{A}}
  \item{margin}{Vector of dimensions over which to form margins. Margins
    are formed in the order in which dimensions are specified in
    \code{margin}.} 
  \item{FUN}{List of the same length as \code{margin}, each element of
    the list being either a function or a list of functions. Names of
    the list elements will appear as levels in dimnames of the result.
    Unnamed list elements will have names constructed:  the name
    of a function or a constructed name based on the position in the table.}
  \item{quiet}{Logical which suppresses the message telling the order in
    which the margins were computed.}
}

\details{
  If the functions used to form margins are not commutative the result
  depends on the order in which margins are computed. Annotation
  of margins is done via naming the \code{FUN} list.
}

\value{
  A table with the same number of dimensions as A, but with extra levels
  of the dimensions mentioned in \code{margin}. The number of levels
  added to each dimension is the length of the entries in \code{FUN}.
  A message with the order of computation of margins is printed.
}

\author{
  Bendix Carstensen, Steno Diabetes Center \& Department of
  Biostatistics, University of Copenhagen,
  \url{http://www.biostat.ku.dk/~bxc}, autumn 2003.  
  Margin naming enhanced by Duncan Murdoch.
}

\seealso{
  \code{\link{table}}, \code{\link{ftable}}, \code{\link{margin.table}}.
}

\examples{
Aye <- sample( c("Yes","Si","Oui"), 177, replace=TRUE )
Bee <- sample( c("Hum","Buzz"), 177, replace=TRUE )
Sea <- sample( c("White","Black","Red","Dead"), 177, replace=TRUE )
A <- table( Aye, Bee, Sea )
A
addmargins( A )
ftable( A )
ftable( addmargins( A ) )

# Non commutative functions - note differences between resulting tables:
ftable(addmargins(A, c(1,3),
       FUN = list(Sum=sum, list(Min=min, Max=max))))
ftable(addmargins(A, c(3,1),
        FUN = list(list(Min=min, Max=max), Sum=sum)))

# Weird function needed to return the N when computing percentages
sqsm <- function( x ) sum( x )^2/100 
B <- table(Sea, Bee)
round(sweep(addmargins(B, 1, list(list(All=sum, N=sqsm))), 2, 
            apply( B, 2, sum )/100, "/" ), 1)
round(sweep(addmargins(B, 2, list(list(All=sum, N=sqsm))), 1, 
            apply(B, 1, sum )/100, "/"), 1)

# A total over Bee requires formation of the Bee-margin first:
mB <-  addmargins(B, 2, FUN=list(list(Total=sum)) )
round(ftable(sweep(addmargins(mB, 1, list(list(All=sum, N=sqsm))), 2, 
                   apply(mB,2,sum )/100, "/" )), 1)
}

\keyword{ manip }
\keyword{ methods }
\keyword{ array }
