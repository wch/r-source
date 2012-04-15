% File src/library/methods/man/Documentation.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{Documentation}
\alias{Documentation}
\alias{Documentation-class}
\alias{Documentation-methods}
\title{Using and Creating On-line Documentation for Classes and
  Methods}
\description{Special documentation can be supplied to describe the
  classes and methods that are created by the software in the methods
  package.  Techniques to access this documentation and to create it
  in R help files are described here.}
\section{Getting documentation on classes and methods}{
  You can ask for on-line help for class definitions, for specific
  methods for a generic function, and for general discussion of
  methods for a generic function.  These requests use the \code{?}
  operator (see \code{\link{help}} for a general description of
  the operator).  Of course, you are at the mercy of the implementer
  as to whether there \emph{is} any documentation on the corresponding
  topics.

  Documentation on a class uses the argument \code{class} on the left
  of the \code{?}, and the name of the class on the right; for
  example,

\code{class ? genericFunction}

to ask for documentation on the class \code{"genericFunction"}.

When you want documentation for the methods defined for a particular
function, you can ask either for a general discussion of the methods
or for documentation of a particular method (that is, the method that
would be selected for a particular set of actual arguments).

Overall methods documentation is requested by
calling the \code{?} operator with \code{methods} as the left-side
argument and the name of the function as the right-side argument. For
example,

\code{methods ? initialize}

asks for documentation on the methods for the \code{\link{initialize}}
function.

Asking for documentation on a particular method is done by giving a
function call expression as the right-hand argument to the \code{"?"}
operator.  There are two forms, depending on whether you prefer to
give the class names for the arguments or expressions that you intend
to use in the actual call.

If you planned to evaluate a function call, say \code{myFun(x, sqrt(wt))}
and wanted to find out something about the method that would be used
for this call, put the call on the right of the \code{"?"} operator:

\code{?myFun(x, sqrt(wt))}

A method will be selected, as it would be for the call itself, and
documentation for that method will be requested.  If \code{myFun} is
not a generic function, ordinary documentation for the function will
be requested.

If you know the actual classes for which you would like method
documentation, you can supply these explicitly in place of the
argument expressions.  In the example above, if you want method
documentation for the first argument having class \code{"maybeNumber"}
and the second \code{"logical"}, call the \code{"?"} operator, this
time with a left-side argument \code{method}, and with a function call
on the right using the class names as arguments:

\code{method ? myFun("maybeNumber", "logical")}

Once again, a method will be selected, this time corresponding to the
specified classes, and method documentation will be requested.  This
version only works with generic functions.

The two forms each have advantages.  The version with actual arguments
doesn't require you to figure out (or guess at) the classes of the
arguments.
On the other hand, evaluating the arguments may take some time,
depending on the example.
The version with class names does require you to pick classes, but
it's otherwise unambiguous.  It has a subtler advantage, in that the
classes supplied may be virtual classes, in which case no actual
argument will have specifically this class.  The class
\code{"maybeNumber"}, for example, might be a class union (see the
example for \code{\link{setClassUnion}}).

In either form, methods will be selected as they would be in actual
computation, including use of inheritance and group generic
functions.  See \code{\link{selectMethod}} for the details, since it is
the function used to find the appropriate method.

}
\section{Writing Documentation for Methods}{
The on-line documentation for methods and classes uses some extensions
to the R documentation format to implement the requests for class and
method documentation described above.  See the document \emph{Writing
  R Extensions} for the available markup commands (you should
have consulted this document already if you are at the stage of
documenting your software).

In addition to the specific markup commands to be described, you can
create an initial, overall file with a skeleton of documentation for
the methods defined for a particular generic function:

\code{promptMethods("myFun")}

will create a file, \file{myFun-methods.Rd} with a skeleton of
documentation for the methods defined for function \code{myFun}.
The output from \code{promptMethods} is suitable if you want to
describe all or most of the methods for the function in one file,
separate from the documentation of the generic function itself.
Once the file has been filled in and moved to the \file{man}
subdirectory of your source package, requests for methods
documentation will use that file, both for specific methods
documentation as described above, and for overall documentation
requested by

\code{methods ? myFun}

You are not required to use \code{promptMethods}, and if you do, you
may not want to use the entire file created:
\itemize{
\item If you want to document the methods in the file containing the
    documentation for the generic function itself, you can
    cut-and-paste to move the \verb{\alias} lines and the
    \code{Methods} section from the file created by
    \code{promptMethods} to the existing file.

\item On the other hand, if these are auxiliary methods, and you only
    want to document the added or modified software, you should strip
    out all but the relevant \verb{\alias} lines for the methods of
    interest, and remove all but the corresponding \verb{\item}
    entries in the \code{Methods} section. Note that in this case you
    will usually remove the first  \verb{\alias} line as well, since
    that is the marker for general methods documentation on this
    function (in the example, \samp{\alias\{myfun-methods\}}).
}

If you simply want to direct documentation for one or more methods to
a particular R documentation file, insert the appropriate alias.
}
\keyword{programming}
\keyword{methods}
\keyword{classes}
\keyword{documentation}
