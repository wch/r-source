\name{getNumCConverters}
\alias{getNumCConverters}
\alias{getCConverterDescriptions}
\alias{getCConverterStatus}
\alias{setCConverterStatus}
\alias{removeCConverter}

\title{Management of .C argument conversion list}
\description{
These functions provide facilities to manage the extensible
list of converters used to translate R objects to C
pointers for use in \code{\link{.C}} calls.
The number and a description of each element in the list 
can be retrieved. One can also query and set the activity
status of individual elements, temporarily ignoring them.
And one can remove individual elements.
}
\usage{
getNumCConverters()
getCConverterDescriptions()
getCConverterStatus()
setCConverterStatus(id, status)
removeCConverter(id)
}
\arguments{
\item{id}{either a number or a string identifying the element of interest in the converter
   list. A string is matched against the description strings for each element to identify
   the element. Integers are specified starting at 1 (rather than 0).}

\item{status}{a logical value specifying whether the element is to be considered
   active (\code{TRUE}) or not (\code{FALSE}).}
}
\details{
 The internal list of converters is potentially used when converting
individual arguments in a \code{\link{.C}} call.  If an argument has a
non-trivial class attribute, we iterate over the list of converters
looking for the first that \dQuote{matches}.  If we find a matching
converter, we have it create the C-level pointer corresponding to the
R object.  When the call to the C routine is complete, we use the same
converter for that argument to reverse the conversion and create an R
object from the current value in the C pointer. This is done
separately for all the arguments.

The functions documented here provide R user-level capabilities
for investigating and managing the list of converters.
There is currently no mechanism for adding an element to the
converter list within the R language. This must be done in
C code using the routine \code{R_addToCConverter()}.
}
\value{
\code{getNumCConverters} returns an integer giving the number
of elements in the list, both active and inactive.

\code{getCConverterDescriptions} returns a character vector containing
the description string of each element of the converter list.

\code{getCConverterStatus} returns a logical vector with a value for
each element in the converter list. Each value indicates whether that
converter is active (\code{TRUE}) or inactive (\code{FALSE}).
The names of the elements are the description strings returned
by \code{getCConverterDescriptions}.

\code{setCConverterStatus} returns the logical value indicating the
activity status of the specified element before the call to change it
took effect.  This is \code{TRUE} for active and \code{FALSE} for
inactive.

\code{removeCConverter} returns \code{TRUE} if
an element in the converter list was identified and removed.
In the case that no such element was found, an error occurs.
}
\references{\url{http://developer.R-project.org/CObjectConversion.pdf}}
\author{Duncan Temple Lang}

\seealso{\code{\link{.C}}}

\examples{
getNumCConverters()
getCConverterDescriptions()
getCConverterStatus()
\dontrun{
old <- setCConverterStatus(1, FALSE)

setCConverterStatus(1, old)
}
\dontrun{
removeCConverter(1)
removeCConverter(getCConverterDescriptions()[1])
}}
\keyword{interface}
\keyword{programming}
