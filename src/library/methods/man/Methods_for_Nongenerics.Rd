\name{Methods_for_Nongenerics}
\alias{Methods_for_Nongenerics}
\title{Methods for Non-Generic Functions in Other Packages}

\description{
In writing methods for an \R package, it's common for these methods to
apply to a function (in another package) that is not  generic in that
package; that is, there are no formal methods for the function in its
own package, although it may have S3 methods.
The programming in this case involves one extra step, to call
\code{\link{setGeneric}()} to declare that the function \emph{is}
generic in your package.

Calls to the function in your package will then use all methods
defined there or in any other loaded package that creates the same
generic function. Similarly, calls to the function in those packages
will use your methods.

The original version, however, remains non-generic.  Calls
in that package or in other packages that use that version will  not dispatch your methods
except for special circumstances:
\enumerate{
\item If the function is one of the primitive functions that accept
  methods, the internal C implementation will dispatch methods if one
  of the arguments is an S4 object, as should be the case.
\item If the other version of the function dispatches S3 methods
  \emph{and} your methods are also registered as S3 methods, the
  method will usually be dispatched as that S3 method.
\item Otherwise, you will need to ensure that all calls to the
  function come from a package in which the function is generic,
  perhaps by copying code to your package.
}
 Details and the underlying reasons are discussed in the following sections.
}

\section{Generic and Non-Generic Calls}{
Creating methods for a function (any function) in a package means that
calls to the function in that package will select methods according to
the actual arguments.
However, if the function was originally a non-generic in another
package, calls to the function from that package will \emph{not}
dispatch methods.
In addition, calls from any third package that imports the non-generic version
will also not dispatch methods.
This section considers the reason and how one might deal with the
consequences.

The reason is simply the \R namespace mechanism and its role in
evaluating function calls.
When a name (such as the name of a function) needs to be evaluated in
a call to a function from some package, the evaluator looks first in the frame of the call,
then in the namespace of the package and then in the imports to that
package.

Defining methods for a function in a package ensures that calls to the
function in that package will select the methods, because a generic
version of the function is created in the namespace.
Similarly, calls from another package that has or imports the generic
version will select methods.
Because the generic versions are identical, all methods will be
available in all these packages.

However, calls from any package that imports the old version or just
selects it from the search list will usually \emph{not} select methods.

A an example, consider the function
\code{\link{data.frame}()} in the base package.
This function takes any number of objects as arguments and attempts to combine
them as variables into a data frame object.
It does this by calling \code{\link{as.data.frame}()}, also in the
base package, for each of the objects.

A reasonable goal would be to extend the classes of objects that can
be included in a data frame by defining methods for
\code{\link{as.data.frame}()}.
But calls to \code{\link{data.frame}()},
will still use the version of that function in the base package, which
continues to call the non-generic \code{\link{as.data.frame}()} in
that package.

The details of what happens and options for dealing with it depend on
the form of the function:  a primitive function; a function that
dispatches S3 methods; or an ordinary \R function.

Primitive functions are not actual \R function objects.
They go directly to internal C code.
Some of them, however, have been implemented to recognize methods.
These functions dispatch both S4 and S3 methods from
the internal C code.
There is no explicit generic function, either S3 or S4.
The internal code looks for S4 methods if the first
argument, or either of the arguments in the case of a binary operator,
is an S4 object.
If no S4 method is found, a search is made for an S3 method.
So defining methods for these functions works as long as the relevant
classes have been defined, which should always be the case.

A function dispatches S3 methods by calling
\code{\link{UseMethod}()}, which does \emph{not} look for
formal methods regardless of whether the first argument is an S4
object or not.
This applies to the \code{\link{as.data.frame}()} example above.
To have methods called in this situation, your package must also define the
method as an S3 method, if possible. See section \sQuote{S3
  \dQuote{Generic} Functions}.

In the third possibility, the function is defined with no expectation
of methods.
For example, the base package has a number of functions that compute
numerical decompositions of matrix arguments.
Some, such as \code{\link{chol}()} and \code{\link{qr}()}
are implemented to dispatch S3 methods; others, such as
\code{\link{svd}()} are implemented directly as a specific
computation.
A generic version of the latter functions can be written and called
directly to define formal methods, but no code in another package that
does not import this generic version will dispatch such methods.

In this case, you need to have the generic version used in all the indirect calls to the
function supplying arguments that should dispatch methods.
This may require supplying new functions that dispatch methods and
then call the function they replace.
For example, if S3 methods did not work for
\code{\link{as.data.frame}()}, one could call a function that
applied the generic version to all its arguments and then called
\code{\link{data.frame}()} as a replacement for that function.
If all else fails, it might be necessary to copy over the relevant
functions so that they would find the generic versions.
}

\section{S3 \dQuote{Generic} Functions}{

S3 method dispatch looks at the class of the first
argument.
S3 methods are ordinary functions with the same arguments as the
generic function.
The \dQuote{signature} of an S3 method is identified  by the name to
which the method is assigned, composed of the name of the
generic function, followed by \code{"."}, followed by the name of the class.
For details, see \link{UseMethod}.

To implement a method for one of these functions corresponding to S4
classes, there are two possibilities: either an S4 method or an S3 method with the
S4 class name.
The S3 method is only possible if the intended signature has the
first argument and nothing else.
In this case,
the recommended approach is to define the S3 method and also supply the
identical function as the definition of the S4 method.
If the S3 generic function was \code{f3(x, ...)} and the S4 class for
the new method was
\code{"myClass"}:

 \code{f3.myClass <- function(x, ...) { ..... }}

 \code{setMethod("f3", "myClass", f3.myClass)}

Defining both methods usually ensures that all calls to the original
function will dispatch the intended method.
The S4 method alone would not be called from other packages using the
original version of the function.
On the other hand,
 an S3 method alone will not be called if there is \emph{any}
    eligible non-default S4 method.

S4 and S3 method selection are designed to follow compatible rules of
inheritance, as far as possible.
S3 classes can be used for any S4 method selection, provided that the
S3 classes have been registered by a call to
\code{\link{setOldClass}}, with that call specifying the correct S3
inheritance pattern.
S4 classes can be used for any S3 method selection; when an S4 object
is detected, S3 method selection uses the contents of
\code{\link{extends}(class(x))} as the equivalent of the S3
inheritance (the inheritance is cached after the first call).

An existing S3 method may not behave as desired for an S4 subclass, in
which case utilities such as \code{\link{asS3}} and
\code{\link{S3Part}} may be useful.  If the S3 method fails on the S4
object, \code{asS3(x)} may be passed instead; if the object returned
by the S3 method needs to be incorporated in the S4 object, the
replacement function for \code{S3Part} may be useful.}


\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}

\seealso{
  \link{Methods_for_S3} for suggested implementation of methods
  that work for both S3 and S4 dispatch.
}

\examples{
## A class that extends a registered S3 class inherits that class' S3
## methods.

setClass("myFrame", contains = "data.frame",
    representation(timestamps = "POSIXt"))

df1 <- data.frame(x = 1:10, y = rnorm(10), z = sample(letters,10))

mydf1 <- new("myFrame", df1, timestamps = Sys.time())

## "myFrame" objects inherit "data.frame" S3 methods; e.g., for `[`

mydf1[1:2, ] # a data frame object (with extra attributes)

## a method explicitly for "myFrame" class


setMethod("[",
    signature(x = "myFrame"),
    function (x, i, j, ..., drop = TRUE)
    {
        S3Part(x) <- callNextMethod()
        x@timestamps <- c(Sys.time(), as.POSIXct(x@timestamps))
        x
    }
)

\donttest{mydf1[1:2, ]}


setClass("myDateTime", contains = "POSIXt")

now <- Sys.time() # class(now) is c("POSIXct", "POSIXt")
nowLt <- as.POSIXlt(now)# class(nowLt) is c("POSIXlt", "POSIXt")

mCt <- new("myDateTime", now)
mLt <- new("myDateTime", nowLt)

## S3 methods for an S4 object will be selected using S4 inheritance
## Objects mCt and mLt have different S3Class() values, but this is
## not used.
f3 <- function(x)UseMethod("f3") # an S3 generic to illustrate inheritance

f3.POSIXct <- function(x) "The POSIXct result"
f3.POSIXlt <- function(x) "The POSIXlt result"
f3.POSIXt <- function(x) "The POSIXt result"

stopifnot(identical(f3(mCt), f3.POSIXt(mCt)))
stopifnot(identical(f3(mLt), f3.POSIXt(mLt)))



## An S4 object selects S3 methods according to its S4 "inheritance"


setClass("classA", contains = "numeric",
         representation(realData = "numeric"))

Math.classA <- function(x) {(getFunction(.Generic))(x@realData)}
setMethod("Math", "classA", Math.classA)


x <- new("classA", log(1:10), realData = 1:10)

stopifnot(identical(abs(x), 1:10))

setClass("classB", contains = "classA")

y <- new("classB", x)

stopifnot(identical(abs(y), abs(x))) # (version 2.9.0 or earlier fails here)

## an S3 generic: just for demonstration purposes
f3 <- function(x, ...) UseMethod("f3")

f3.default <- function(x, ...) "Default f3"

## S3 method (only) for classA
f3.classA <- function(x, ...) "Class classA for f3"

## S3 and S4 method for numeric
f3.numeric <- function(x, ...) "Class numeric for f3"
setMethod("f3", "numeric", f3.numeric)

## The S3 method for classA and the closest inherited S3 method for classB
## are not found.

f3(x); f3(y) # both choose "numeric" method

## to obtain the natural inheritance, set identical S3 and S4 methods
setMethod("f3", "classA", f3.classA)

f3(x); f3(y) # now both choose "classA" method

## Need to define an S3 as well as S4 method to use on an S3 object
## or if called from a package without the S4 generic

MathFun <- function(x) { # a smarter "data.frame" method for Math group
  for (i in seq(length = ncol(x))[sapply(x, is.numeric)])
    x[, i] <- (getFunction(.Generic))(x[, i])
  x
}
setMethod("Math", "data.frame", MathFun)

## S4 method works for an S4 class containing data.frame,
## but not for data.frame objects (not S4 objects)

try(logIris <- log(iris)) #gets an error from the old method

## Define an S3 method with the same computation

Math.data.frame <- MathFun

logIris <- log(iris)




\dontshow{
removeClass("classA"); removeClass("classB"); rm(x,y)
removeGeneric("f3")
removeClass("myDateTime")
removeMethod("Math", "data.frame"); rm(Math.data.frame, MathFun, logIris)
}

}
