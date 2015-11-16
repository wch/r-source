% File src/library/methods/man/RMethodUtils.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{RMethodUtils}
\title{Method Utilities}
%% FIXME:  undocumented utilities (Sep. 23/02)
\alias{asMethodDefinition}
\alias{standardGeneric-class}
\alias{standardGenericWithTrace-class}
\alias{nonstandardGeneric-class}
\alias{nonstandardGenericFunction-class}
\alias{nonstandardGroupGenericFunction-class}
\alias{OptionalFunction-class}
\alias{PossibleMethod-class}
\alias{optionalMethod-class}
\alias{derivedDefaultMethod-class}
\alias{internalDispatchMethod-class}
%%
\alias{substituteFunctionArgs}
\alias{makeGeneric}
\alias{makeStandardGeneric}
\alias{generic.skeleton}
\alias{defaultDumpName}
\alias{doPrimitiveMethod}
\alias{conformMethod}
\alias{getGeneric}
\alias{getGroup}
\alias{getGroupMembers}
\alias{getMethodsMetaData}
\alias{assignMethodsMetaData}
\alias{matchSignature}
\alias{findUnique}
\alias{MethodAddCoerce}
\alias{.saveImage}
\alias{cacheMetaData}
\alias{cacheGenericsMetaData}
\alias{setPrimitiveMethods}
\alias{missingArg}
\alias{balanceMethodsList}
\alias{sigToEnv}
\alias{rematchDefinition}
\alias{unRematchDefinition}
\alias{addNextMethod,MethodDefinition-method}
\alias{addNextMethod,MethodWithNext-method}
\alias{addNextMethod}
\alias{.valueClassTest}
\alias{insertClassMethods}
%% Not for the user to know about!
\alias{.ShortPrimitiveSkeletons} %% used from C code
\alias{.EmptyPrimitiveSkeletons}
\description{
  Utility functions to support the definition and use of formal
  methods.  Most of these functions will not normally be called directly
  by the user.
}
\usage{
getGeneric(f, mustFind=FALSE, where, package)

getGroup(fdef, recursive, where)
getGroupMembers(group, recursive = FALSE, character = TRUE)

getMethodsMetaData(f, where)
assignMethodsMetaData (f, value, fdef, where)

makeGeneric(f, fdef, fdefault =, group=list(), valueClass=character(),
            package =, signature = NULL, genericFunction = NULL,
            simpleInheritanceOnly = NULL)

makeStandardGeneric(f, fdef)

generic.skeleton(name, fdef, fdefault)

defaultDumpName(generic, signature)

doPrimitiveMethod(name, def, call= sys.call(sys.parent()),
                  ev = sys.frame(sys.parent(2)))

conformMethod(signature, mnames, fnames, f= , fdef, method)

matchSignature(signature, fun, where)

findUnique(what, message, where)

MethodAddCoerce(method, argName, thisClass, methodClass)

cacheMetaData(where, attach = TRUE, searchWhere = as.environment(where),
              doCheck = TRUE)

cacheGenericsMetaData(f, fdef, attach = TRUE, where, package, methods)

setPrimitiveMethods(f, fdef, code, generic, mlist)

missingArg(symbol, envir = parent.frame(), eval)

sigToEnv(signature, generic)

rematchDefinition(definition, generic, mnames, fnames, signature)
unRematchDefinition(definition)

asMethodDefinition(def, signature, sealed = FALSE, fdef)

addNextMethod(method, f, mlist, optional, envir)

insertClassMethods(methods, Class, value, fieldNames, returnAll)

balanceMethodsList(mlist, args, check = TRUE) # <- deprecated since R 3.2.0
}
\section{Summary of Functions}{

  \describe{

    \item{\code{getGeneric}:}{
      returns the definition of the function named \code{f} as a generic.

      If no definition is found, throws an
      error or returns \code{NULL} according to the value of
      \code{mustFind}.  By default, searches in the top-level
      environment (normally the global environment, but adjusted to
      work correctly when package code is evaluated from the function
      \code{\link{library}}).

      Primitive functions are dealt with specially, since there is never
      a formal generic definition for them.  The value returned is the
      formal definition used for assigning methods to this primitive.
      Not all primitives can have methods; if this one can't, then
      \code{getGeneric} returns \code{NULL} or throws an error.
    }

    \item{\code{getGroup}:}{
      returns the groups to which this generic belongs, searching from
      environment \code{where} (the global environment normally by default).

      If \code{recursive=TRUE}, also all the group(s) of these groups.
    }

    \item{\code{getGroupMembers}:}{
      Return all the members of the group generic function named
      \code{group}.
      If \code{recursive} is \code{TRUE}, and some members are group
      generics, includes their members as well.
      If \code{character} is \code{TRUE}, returns just a character
      vector of the names; otherwise returns a list, whose elements may
      (or may not) include either names with a package attribute or
      actual generic functions.

      Note that members that are not defined as generic functions will
      \emph{not} be included in the returned value.  To see the raw data,
      use \code{getGeneric(group)@groupMembers}.
    }

    \item{\code{getMethodsMetaData},
      \code{assignMethodsMetaData},
      \code{mlistMetaName}:}{Utilities to get
      (\code{getMethodsMetaData}) and assign
      (\code{assignMethodsMetaData}) the metadata object recording the
      methods defined in a particular package, or to return the mangled
      name for that object  (\code{mlistMetaName}).

      The assign function should not be used directly.  The get
      function may be useful if you want explicitly only the outcome of
      the methods assigned in this package.  Otherwise, use
      \code{\link{getMethods}}.
    }

    \item{\code{matchSignature}:}{
      Matches the signature object (a partially or completely named
      subset of the signature arguments of the generic function object
      \code{fun}), and return a vector of all the classes in the order
      specified by \code{fun@signature}.  The classes not specified by
      \code{signature} will be \code{"ANY"} in the value, but extra
      trailing \code{"ANY"}'s are removed.  When the input signature is
      empty, the returned signature is a single \code{"ANY"} matching
      the first formal argument (so the returned value is always
      non-empty).

      Generates an error if any of the supplied signature names are
      not legal; that is, not in the signature slot of the generic
      function.

      If argument \code{where} is supplied, a warning will be issued
      if any of the classes does not have a formal definition visible
      from \code{where}.
    }

    \item{\code{MethodAddCoerce}:}{
      Possibly modify one or more methods to explicitly coerce this
      argument to \code{methodClass}, the class for which the method is
      explicitly defined.  Only modifies the method if an explicit
      coerce is required to coerce from \code{thisClass} to
      \code{methodClass}.
    }

    \item{\code{findUnique}:}{
      Return the list of environments (or equivalent) having an object named
      \code{what}, using environment \code{where} and its parent
      environments.  If more than one is found, a warning message is
      generated, using \code{message} to identify what was being
      searched for, unless \code{message} is the empty string.
    }

    \item{\code{cacheMetaData},
      \code{cacheGenericsMetaData},
      \code{setPrimitiveMethods}:}{
      Utilities for ensuring that the internal information about
      class and method definitions is up to date.  Should normally be
      called automatically whenever needed (for example, when a method
      or class definition changes, or when a package is attached or
      detached).  Required primarily because primitive functions are
      dispatched in C code, rather than by the official model.

      The \code{setPrimitiveMethods} function resets the caching
      information for a particular primitive function.  Don't call it
      directly.
    }

    \item{\code{missingArg}:}{
      Returns \code{TRUE} if the symbol supplied is missing \emph{from
        the call} corresponding to the environment supplied (by default,
      environment of the call to \code{missingArg}).  If \code{eval} is
      true, the argument is evaluated to get the name of the symbol to
      test.  Note that \code{missingArg} is closer to the
      \sQuote{Blue Book} sense of the \code{\link{missing}} function,
      not that of the current R base package implementation.  But beware
      that it works reliably only if no assignment has yet been made to
      the argument.  (For method dispatch this is fine, because
      computations are done at the beginning of the call.)
    }

    \item{\code{balanceMethodsList}:}{
      Used to be called from \code{setMethod()} and is \emph{deprecated}
      since \R version 3.2.0.
    %%   Called from \code{\link{setMethod}} to ensure that all nodes in
    %%   the list have the same depth (i.e., the same number of levels of
    %%   arguments).  Balance is needed to ensure that all necessary
    %%   arguments are examined when inherited methods are being found and
    %%   added to the \code{allMethods} slot.  No actual recomputation is
    %%   needed usually except when a new method uses a longer signature
    %%   than has appeared before.

    %%   Balance requires that \emph{all} methods be added to the generic
    %%   via \code{setMethod} (how else could you do it?) or by the initial
    %%   \code{setGeneric} call converting the ordinary function.
    }

    \item{\code{sigToEnv}:}{
      Turn the signature (a named vector of classes) into an environment
      with the classes assigned to the names.  The environment is then
      suitable for calling \code{\link{MethodsListSelect}}, with
      \code{evalArgs=FALSE}, to select a method corresponding to the
      signature.  Usually not called directly: see
      \code{\link{selectMethod}}.
    }

    \item{\code{.saveImage}:}{
      Flag, used in dynamically initializing the methods package from
      \code{.onLoad}.
    }

    \item{\code{rematchDefinition},
      \code{unRematchDefinition}:}{
      If the specified method in a call to \code{\link{setMethod}}
      specializes the argument list (by replacing \dots), then
      \code{rematchDefinition} constructs the actual method stored.
      Using knowledge of how \code{rematchDefinition} works,
      \code{unRematchDefinition} reverses the procedure; if given a
      function or method definition that does not correspond to this
      form, it just returns its argument.
    }

    \item{\code{asMethodDefinition}:}{
      Turn a function definition into an object of class
      \code{\linkS4class{MethodDefinition}}, corresponding to the
      given \code{signature} (by default generates a default method
      with empty signature).  The definition is sealed according to
      the \code{sealed} argument.
    }

    \item{\code{addNextMethod}:}{
      A generic function that finds the next method for
      the signature of the method definition \code{method}  and caches
      that method in the method definition (promoting the class to
      \code{"MethodWithNext"}.  Note that argument \code{mlist} is
      obsolete and not used.
    }

    \item{\code{makeGeneric}:}{
      Makes a generic function object corresponding to the given
      function name, optional definition and optional default method.
      Other arguments supply optional elements for the slots of class
      \code{\linkS4class{genericFunction}}.
    }

    \item{\code{makeStandardGeneric}:}{
      a utility function that makes a valid function calling
      \code{standardGeneric} for name \code{f}.  Works (more or less)
      even if the actual definition, \code{fdef}, is not a proper
      function, that is, it's a primitive or internal.
    }

    \item{\code{conformMethod}:}{
      If the formal arguments, \code{mnames}, are not identical to the
      formal arguments to the function, \code{fnames},
      \code{conformMethod} determines whether the signature and the two
      sets of arguments conform, and returns the signature, possibly
      extended.  The function name, \code{f} is supplied for error
      messages. The generic function, \code{fdef}, supplies the
      generic signature  for matching purposes.

      The method assignment conforms if method and generic function have
      identical formal argument lists.  It can also conform if the
      method omits some of the formal arguments of the function but: (1)
      the non-omitted arguments are a subset of the function arguments,
      appearing in the same order; (2) there are no arguments to the
      method that are not arguments to the function; and (3) the omitted
      formal arguments do not appear as explicit classes in the
      signature.  A future extension hopes to test also that the
      omitted arguments are not assumed by being used as locally assigned
      names or function names in the body of the method.
    }

    \item{\code{defaultDumpName}:}{
      the default name to be used for dumping a method.
    }


    \item{\code{doPrimitiveMethod}:}{
      do a primitive call to builtin function \code{name} the definition
      and call provided, and carried out in the environment \code{ev}.

      A call to \code{doPrimitiveMethod} is used when the actual method
      is a .Primitive.  (Because primitives don't behave correctly as
      ordinary functions, not having either formal arguments nor a
      function body).
    }

  }
}
\seealso{\code{\link{setGeneric}}, \code{\link{setClass}},
  \code{\link{showMethods}}.
}
\examples{
getGroup("exp")
getGroup("==", recursive = TRUE)

getGroupMembers("Arith")
getGroupMembers("Math")
getGroupMembers("Ops") # -> its sub groups
}
\keyword{internal}
