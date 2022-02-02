\name{classesToAM}
\alias{classesToAM}
\title{
Compute an Adjacency Matrix for Superclasses of Class Definitions
}
\description{
Given a vector of class names or a list of class definitions, the
function returns an adjacency matrix of the superclasses of these
classes; that is, a matrix with class names as the row and column
names and with element [i, j] being 1 if the class in column j is a
direct superclass of the class in row i, and 0 otherwise.

The matrix has the information implied by the \code{contains} slot of
the class definitions, but in a form that is often more convenient for
further analysis; for example, an adjacency matrix is used in packages
and other software to construct graph representations of relationships.
}
\usage{
classesToAM(classes, includeSubclasses = FALSE,
       abbreviate = 2)
}
\arguments{
  \item{classes}{
      Either a character vector of class names or a list, whose
      elements can be either class names or class definitions.  The
      list is convenient, for example, to include the package slot for
      the class name. See the examples.
}
  \item{includeSubclasses}{
      A logical flag; if \code{TRUE}, then the matrix will include all
      the known subclasses of the specified classes as well as the
      superclasses.  The argument can also be a logical vector of the
      same length as \code{classes}, to include subclasses for some
      but not all the classes.
}
    \item{abbreviate}{
        Control of the abbreviation of the row and/or  column labels of
        the matrix returned: values 0, 1, 2, or 3 abbreviate neither,
        rows, columns or both.  The default, 2, is useful for printing
        the matrix, since class names tend to be more than one
        character long, making for spread-out printing.  Values of 0
        or 3 would be appropriate for making a graph (3 avoids the
        tendency of some graph plotting software to produce labels in
        minuscule font size).
      }
}
\details{
  For each of the classes, the calculation gets all the superclass
  names from the class definition, and finds the edges in those classes'
  definitions; that is, all the superclasses at distance 1.  The
  corresponding elements of the adjacency matrix are set to 1.

  The adjacency matrices for the individual class definitions are
  merged.  Note two possible kinds of inconsistency, neither of which
  should cause problems except possibly with identically named classes from
  different packages.  Edges are computed from each superclass
  definition, so that information overrides a possible inference from
  extension elements with distance > 1 (and it should).  When
  matrices from successive classes in the argument are merged, the
  computations do not currently check for inconsistencies---this is
  the area where possible multiple classes with the same name could
  cause confusion.  A later revision may include consistency checks.
}
\value{
  As described, a matrix with entries 0 or 1, non-zero values
  indicating that the class corresponding to the column is a direct
  superclass of the class corresponding to the row.  The row and
  column names are the class names (without package slot).
}


\seealso{
  \code{\link{extends}} and \linkS4class{classRepresentation} for the underlying information from the class
  definition.
}
\examples{

## the super- and subclasses of "standardGeneric"
## and "derivedDefaultMethod"
am <- classesToAM(list(class(show), class(getMethod(show))), TRUE)
am

\dontrun{
## the following function depends on the Bioconductor package Rgraphviz
plotInheritance <- function(classes, subclasses = FALSE, ...) {
    if(!require("Rgraphviz", quietly=TRUE))
      stop("Only implemented if Rgraphviz is available")
    mm <- classesToAM(classes, subclasses)
    classes <- rownames(mm); rownames(mm) <- colnames(mm)
    graph <-  new("graphAM", mm, "directed", ...)
    plot(graph)
    cat("Key:\n", paste(abbreviate(classes), " = ", classes, ", ",
        sep = ""),  sep = "", fill = TRUE)
    invisible(graph)
}

## The plot of the class inheritance of the package "graph"
require(graph)
plotInheritance(getClasses("package:graph"))

}
}

\keyword{classes}
\keyword{programming}
