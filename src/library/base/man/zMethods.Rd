\name{Methods}
\title{Internal and Group Methods and Generic Functions}
\usage{
Math.data.frame(x, \dots)
Math.factor(x, \dots)

Ops.data.frame(e1, e2 = NULL)
Ops.factor(e1, e2)
Ops.ordered(e1, e2)

Summary.data.frame(x, \dots)
Summary.factor(x, \dots)

.Method
.Generic
.Group
.Class
}
\alias{.Method}
\alias{.Generic}
\alias{.Group}
\alias{.Class}
\alias{Math}
\alias{Math.data.frame}
\alias{Math.factor}
\alias{Ops}
\alias{Ops.data.frame}
\alias{Ops.factor}
\alias{Ops.ordered}
\alias{Summary}
\alias{Summary.data.frame}
\alias{Summary.factor}

\description{Many \R-internal functions  are \emph{generic} and  allow
    methods to be written for.  \emph{Group} methods in particular are
    available for the \code{"Math"},  \code{"Ops"}, and
    \code{"Summary"} group.
}
\section{Group Dispatching}{
    %% --------------- grep -nw DispatchGroup src/*/*[ch]

    There are three \emph{groups} for which methods can be written, namely
    the \code{"Math"}, \code{"Ops"} and \code{"Summary"} groups.

    A function \emph{\code{f}} belonging to one of these groups must be
    \code{\link{.Internal}} or \code{\link{.Primitive}} and will
    automatically be using \emph{\code{<grp>.<class> (ob)}} when
    \emph{\code{f(<ob>)}} is called, \emph{\code{f}} belongs to group
    \emph{\code{<grp>}} and \emph{\code{<ob>}} is of \code{\link{class}}
    \emph{\code{<class>}}.
    
    \enumerate{
        \item Group \code{"Math"}:
        \itemize{
            \item \code{abs}, \code{sign}, \code{sqrt}, \cr
                  \code{floor}, \code{ceiling}, \code{trunc},\cr
                  \code{round}, \code{signif}

            
            \item \code{exp}, \code{log}, \cr
                  \code{cos}, \code{sin}, \code{tan},\cr
                  \code{acos}, \code{asin}, \code{atan}
            
                  \code{cosh}, \code{sinh}, \code{tanh},\cr
                  \code{acosh}, \code{asinh}, \code{atanh}
            
            \item \code{lgamma}, \code{gamma}, \code{gammaCody},\cr
                  \code{digamma}, \code{trigamma}, \code{tetragamma},
                  \code{pentagamma}
            %   do_math1() [arithmetic.c:794]: if (DispatchGroup("Math",...))
            % 
            %
            % "atan", "round", "log", "signif":
            % do_atan()  [arithmetic.c:958]: if (DispatchGroup("Math", ..))
            % do_round() [arithmetic.c:981]: if (DispatchGroup("Math", ..))
            % do_log()   [arithmetic.c:1011]:if (DispatchGroup("Math", ..))
            % do_signif()[arithmetic.c:1034]:if (DispatchGroup("Math", ..))
            
            \item \code{cumsum}, \code{cumprod}, \code{cummax}, \code{cummin}
            %   do_cum()   [cum.c:140]:    if (DispatchGroup("Math", ...))
        }
        
        \item Group \code{"Ops"}:
        \itemize{
            \item \code{"+"}, \code{"-"}, \code{"*"}, \code{"/"},
                  \code{"^"}, \code{"\%\%"}, \code{"\%/\%"}
            %   do_arith() [arithmetic.c:240]: if (DispatchGroup("Ops", ...))
            
            \item \code{"&"}, \code{"|"}, \code{"!"}
            %   do_logic() [logic.c:32]:   if (DispatchGroup("Ops",...))
            
            \item \code{"=="}, \code{"!="}, 
                  \code{"<"}, \code{"<="}, \code{">="}, \code{">"}
            %   do_relop() [relop.c:35]:   if (DispatchGroup("Ops", ...))
        }
        
        \item Group \code{"Summary"}:
        \itemize{
            \item \code{all}, \code{any}
            %   do_logic3()[logic.c:278]:  if(DispatchGroup("Summary", ...))
            
            \item \code{sum}, \code{prod}
            % /*NOMORE:\code{mean}, */
            \item \code{min}, \code{max}
            %   do_summary() [summary.c:322]: if(DispatchGroup("Summary",...))
        }
    }
}

\section{Simple Dispatching}{
    % -------------------- grep -nw DispatchOrEval src/*/*[ch]
    %                         --------------
    The following builtin functions are \emph{generic} as well, i.e., you
    can write \code{\link{methods}} for them:

    \code{\link{[}},            % do_subset()   [subset.c:464]
    \code{\link{[[}}            % do_subset2()  [subset.c:567]
    
    \code{\link{dimnames<-}},   % do_dimnamesgets()     [attrib.c:422]
    \code{\link{dimnames}},     % do_dimnames()         [attrib.c:489]
    \code{\link{dim<-}},        % do_dimgets()  [attrib.c:513]
    \code{\link{dim}}           % do_dim()      [attrib.c:501]
    
    \code{\link{c}}, %do_c()            [bind.c:524]
    \code{\link{unlist}}, %do_unlist()  [bind.c:648]
    \code{\link{as.vector}}, %do_asvector() [coerce.c:948]
    \code{\link{is.na}}, %do_isna()     [coerce.c:1244]
    \code{\link{is.nan}}%do_isnan()    [coerce.c:1332]
}
\references{
    Appendix A, \emph{Classes and Methods} of\cr
    Chambers, J. M.  and Hastie, T. J. eds (1992)
    \emph{Statistical Models in S.}
    Wadsworth & Brooks/Cole.
}
\seealso{
\code{\link{methods}} for methods of non-Internal generic functions.
}
\examples{
methods("Math")
methods("Ops")
methods("Summary")

d.fr <- data.frame(x=1:9, y=rnorm(9))
data.class(1 + d.fr) == "data.frame" ##-- add to d.f. ...
}
\keyword{methods}
