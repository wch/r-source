\name{RMethodUtils}
%% FIXME:  undocumented utilities (Sep. 23/02)
\alias{asMethodDefinition}
\alias{nonstandardGeneric-class}
\alias{nonstandardGenericFunction-class}
\alias{nonstandardGroupGenericFunction-class}
\alias{OptionalFunction-class}
\alias{PossibleMethod-class}
%%
\alias{substituteFunctionArgs}
\alias{makeGeneric}
\alias{makeStandardGeneric}
\alias{getAllMethods}
\alias{generic.skeleton}
\alias{defaultDumpName}
\alias{getAllMethods}
\alias{setAllMethodsSlot}
\alias{doPrimitiveMethod}
\alias{conformMethod}
\alias{getGeneric}
\alias{getGroup}
\alias{getMethodsMetaData}
\alias{assignMethodsMetaData}
\alias{matchSignature}
\alias{mlistMetaName}
\alias{assignToMethodMetaData}
\alias{getFromMethodMetaData}
\alias{removeMethodsObject}
\alias{findUnique}
\alias{removeFromMethodMetaData}
\alias{MethodAddCoerce}
\alias{is.primitive}
\alias{.saveImage}
\alias{cacheMetaData}
\alias{cacheGenericsMetaData}
\alias{setPrimitiveMethods}
\alias{printNoClass}
\alias{print.default}
\alias{missingArg}
\alias{balanceMethodsList}
\alias{sigToEnv}
\alias{rematchDefinition}
\alias{unRematchDefinition}
\allias{asMethodDefinition}
\alias{findNextMethod}
\alias{.valueClassTest}
%% Not for the user to know about!
\alias{.ShortPrimitiveSkeletons} %% used from C code
\alias{.EmptyPrimitiveSkeletons}
\alias{__MethodMetaData}
\title{RMethodUtils}
\description{
  Utility functions to support the definition and use of formal
  methods.  Most of these functions will not normally be called directly
  by the user.
}
\usage{
makeGeneric(f, fdef, fdefault, group=character(),
            valueClass=character(), package, signature, genericFunction)

makeStandardGeneric(f, fdef)

generic.skeleton(name, fdef, fdefault)

defaultDumpName(generic, signature)

getAllMethods(f, fdef, libs=search())

setAllMethodsSlot(mlist)

doPrimitiveMethod(name, def, call=sys.call(-1), ev=sys.frame(-2))

conformMethod(signature, mnames, fnames)

getGeneric(f, mustFind=FALSE)

getGroup(fdef, recursive=FALSE)

matchSignature(signature, fun)

## manage method metadata
getFromMethodMetaData(name)

assignToMethodMetaData(name, value)

removeFromMethodMetaData(name)

removeMethodsObject(f, where)

findUnique(what, doFind, message)

MethodAddCoerce(method, argName, thisClass, methodClass)

is.primitive(fdef)

cacheMetaData(where, attach = TRUE)

cacheGenericsMetaData(generics, attach = TRUE, where, package)

setPrimitiveMethods(f, fdef, code, generic, mlist)

printNoClass(x, digits, quote, na.print, print.gap, right, ...)

print.default(x, ...)

missingArg(symbol, envir = parent.frame(), eval)

balanceMethodsList(mlist, args, check = TRUE)

sigToEnv(signature)

rematchDefinition(definition, generic, mnames, fnames)
unRematchDefinition(definition)

asMethodDefinition(def, signature)

findNextMethod(method, f, mlist, optional = FALSE, envir)

}
\section{Summary of Functions}{
 
  \describe{
   
    \item{\code{makeGeneric}:}{ 
      Makes a generic function object corresponding to the given
      function name, optional definition and optional default method.  Other arguments supply optional
      elements for the slots of \code{\link{genericFunction-class}}.
    }

    \item{\code{makeStandardGeneric}:}{ 
      a utility function that makes a valid function calling
      standardGeneric for name f.  Works (more or less) even if the
      actual definition, fdef, is not a proper function, that is, it's a
      primitive or internal.
    }
    
    \item{\code{conformMethod}:}{
      If the formal arguments, \code{mnames}, are not identical to the
      formal arguments to the function, \code{fnames},
      \code{conformMethod} determines whether the signature and the two
      sets of arguments conform, and returns the signature, possibly
      extended.

      The method assignment conforms if either method and function have
      identical formal argument lists.  It can also conform if the
      method omits some of the formal arguments of the function but: (1)
      the non-omitted arguments are a subset of the function arguments,
      appearing in the same order; (2) there are no arguments to the
      method that are not arguments to the function; and (3) the omitted
      formal arguments do not appear as explicit classes in the
      signature.
    }

    \item{\code{defaultDumpName}:}{ 
      the default name to be used for dumping a method.
    }
    
    \item{\code{getAllMethods}:}{ 
      a generic function (with methods) representing the merge of all
      the versions of \code{f} on the specified packages (anything on
      the current search path by default).

      If the generic \code{f} has a group generic, methods for this
      group generic (and further generations of group generics, if any)
      are also merged.
  
      The merging rule is as follows: each generic is merged across
      packages, and the group generics are then merged, finally adding
      the directly defined methods of \code{f}.

      The effect of the merging rule is that any method directly defined
      for \code{f} on any included package overrides a method for the
      same signature defined for the group generic; similarly for the
      group generic and its group, if any, etc.

      For \code{f} or for a specific group generic, methods override in
      the order of the packages being searched.  A method for a
      particular signature on a particular package overrides any methods
      for the same signature on packages later on in the list of
      packages being searched.

      The slot "allMethods" of the merged methods list is set to a copy
      of the methods slot; this is the slot where inherited methods are
      stored.
    }
    
    \item{\code{doPrimitiveMethod}:}{ 
      do a primitive call to builtin function \code{name} the definition
      and call provided, and carried out in the environment \code{ev}.

      A call to \code{doPrimitiveMethod} is used when the actual method
      is a .Primitive.  (Because primitives don't behave correctly as
      ordinary functions, not having either formal arguments nor a
      function body).
    }

    \item{\code{getGeneric}:}{ 
      return the definition of the function named f as a generic.

      If there is no definition in the current search list, throws an
      error or returns NULL according to the value of mustFind.

      Primitive functions are dealt with specially, since there is never
      a formal generic definition for them.  The value returned is the
      formal definition used for assigning methods to this primitive.
      Not all primitives can have methods; if this one can't, then
      \code{getGeneric} returns \code{NULL} or throws an error.
    }

    \item{\code{getGroup}:}{ 
      return the groups to which this generic belongs.  

      If \code{recursive=TRUE}, also all the group(s) of these groups.
    }

    \item{\code{matchSignature}}{
      Matches the signature object (a partially or completely named
      subset of the signature arguments of the generic function object
      \code{fun}), and return a vector of all
      the classes in the order specified by \code{fun@signature}.  The classes
      not specified by `signature' will be \code{"ANY"} in the value,
      but extra trailing \code{"ANY"}'s are removed.  When the input
      signature is empty, the returned signature is a single
      \code{"ANY"} matching the first formal argument (so the returned
      value is always non-empty).
  
      Generates an error if any of the supplied signature names are
      not legal; that is, not in the signature slot of the generic function.
    }

    \item{\code{getMethodsMetaData}, \code{assignMethodsMetaData}, \code{mlistMetaName}:}{
      utilities to manage methods list objects in a particular environment.
      Not to be called directly.
    }
    
    \item{\code{getFromMethodMetaData}, \code{assignToMethodMetaData}, \code{removeFromMethodMetaData}}{
      Functions to manage the session metadata for methods.  Don't
      call these directly.
    }
    
    \item{\code{MethodAddCoerce}}{
      Possibly modify one or more methods to explicitly coerce this
      argument to \code{methodClass}, the class for which the method is
      explicitly defined.  Only modifies the method if an explicit
      coerce is required to coerce from \code{thisClass} to
      \code{methodClass}.
    }
    
    \item{\code{is.primitive}}{
      Is this object a primitive function (either a builtin or special)?
    }
    
    \item{\code{removeMethodsObject}:}{
      remove the metadata object containing methods for \code{f}.
    }
    
    \item{\code{findUnique}:}{
      Find the first position on the search list containing object
      \code{what}; if more than one is found, a warning message is
      generated, using \code{message} to identify what was being
      searched for.

      If \code{doFind} is supplied, it's the version of \code{find} used
      to do the search (e.g., \code{findFunction}.
    }
  
    \item{\code{cacheMetaData}, \code{cacheGenericsMetaData}, \code{setPrimitiveMethods}:}{
      Utilities for ensuring that the session-scope information about
      class and method definitions is up to date.  Should normally be
      called automatically whenever needed (for example, when a method
      or class definition changes, or when a package is attached or
      detached.

      The \code{environment} must be one of the environments on the
      current search list; note in particular that even on detaching
      (\code{attach=FALSE}), the environment will normally still be on
      the search list.

      The \code{setPrimitiveMethods} function resets the caching
      information for a particular primitive function.  Don't call it
      directly.
    }

    \item{\code{printNoClass},\code{print.default}: }{
      \code{printNoClass} is equivalent to the version of
      \code{print.default} in the base package.  The methods package
      overrides the latter function to provide meaningful printing for
      formally defined classes, and \code{printNoClass} is used to get
      the original default printing.
    }

    \item{\code{missingArg}: }{
      Returns \code{TRUE} if the symbol supplied is missing \emph{from
        the call} corresponding to the environment supplied (by default,
      environment of the call to \code{missingArg}).  If \code{eval} is
      true, the argument is evaluated to get the name of the symbol to
      test.  Note that \code{missingArg} is closer to the ``blue-book''
      sense of the \code{\link{missing}} function, not that of the
      current R base package implementation.  But beware that it works
      reliably only if no assignment has yet been made to the argument.
      (For method dispatch this is fine, because computations are done
      at the begining of the call.)
    }

    \item{\code{balanceMethodsList}: }{
      Called from \code{\link{setMethod}} to ensure that all nodes in
      the list have the same depth (i.e., the same number of levels of
      arguments).  Balance is needed to ensure that all necessary
      arguments are examined when inherited methods are being found and
      added to the \code{allMethods} slot.  No actual recomputation is
      needed usually except when a new method uses a longer signature
      than has appeared before.

      Balance requires that \emph{all} methods be added to the generic
      via \code{setMethod} (how else could you do it?) or by the initial
      \code{setGeneric} call converting the ordinary function.
    }

    \item{\code{sigToEnv}: }{
      Turn the signature (a named vector of classes) into an environment
      with the classes assigned to the names.  The environment is then
      suitable for calling \code{\link{MethodsListSelect}}, with
      \code{evalArgs=FALSE}, to select a method corresponding to the
      signature.  Usually not called directly: see
      \code{\link{selectMethod}}.
    }
      
}
\item{\code{.saveImage}:}{Flag, used in dynamically initializing the
    methods package from \code{.First.lib}}
}
\item{\code{rematchDefinition}, \code{unRematchDefinition}:}{
  If the specified method in a call to \code{\link{setMethod}}
  specializes the argument list (by replacing \dots), then
  \code{rematchDefinition} constructs the actual method stored.
  Using knowledge of how \code{rematchDefinition} works,
  \code{unRematchDefinition}
 reverses the procedure; if given a function or method definition that
 does not correspond to this form, it just returns its argument.
}
  \item{\code{asMethodDefinition}:}{ Turn a function definition into a
      \code{\link{MethodDefiniton-class}} object, corresponding to the
      given \code{signature} (by default generates a default method
      with empty signature).
      }
  \item{\code{findNextMethod}:}{ A generic function that finds the
      next method in \code{mlist} corresponding the method definition
      \code{method}. It uses methods defined suitably for ordinary
      methods and for methods with calls to \code{\link{callNextMethod}}.
      }
}
\keyword{internal}
