\name{RMethodUtils}
\alias{makeGeneric}
\alias{makeStandardGeneric}
\alias{generic.skeleton}
\alias{defaultDumpName}
\alias{mergeGenericFunctions}
\alias{setAllMethodsSlot}
\alias{doPrimitiveMethod}
\alias{conformMethodArgs}
\alias{getGeneric}
\alias{getGroup}
\alias{getMethodsMetaData}
\alias{assignMethodsMetaData}
\alias{mlistMetaName}
%% Not for the user to know about!
\alias{.ShortPrimitiveSkeletons} %% used from C code
\alias{.EmptyPrimitiveSkeletons}
\title{RMethodUtils}
\usage{
makeGeneric(f, fdef, keepMethods=TRUE, useAsDefault=NA, group=character(), valueClass=character())

makeStandardGeneric(f, fdef)

generic.skeleton(name, fdef, fdefault)

defaultDumpName(generic, signature)

mergeGenericFunctions(f, libs=search())

setAllMethodsSlot(mlist)

doPrimitiveMethod(name, def, call=sys.call(-1), ev=sys.frame(-2))

conformMethodArgs(def, fdef, envir)

getGeneric(f, mustFind=FALSE)

getGroup(fdef, recursive=FALSE)

}
\description{
 These are utility functions to support the definition and use of
 formal methods.  Most of them will not normally be called directly by
 the user.
 
\describe{
\item{\code{makeGeneric}:}{ 
  Makes a generic function object corresponding to the given function name.
and optional definition.

  
  }
\item{\code{makeStandardGeneric}:}{ 
  a utility function that makes a valid function calling standardGeneric for name f
Works (more or less) even if the actual definition, fdef, is not a proper function,
that is, it's a primitive or internal

  
  }
\item{\code{defaultDumpName}:}{ 
  the default name to be used for dumping a method.

  
  }
\item{\code{mergeGenericFunctions}:}{ 
  a generic function (with methods) representing the merge of all the versions
of \code{f} on the specified packages (anything on the current search path by default).

If the generic \code{f} has a group generic, methods for this group generic (and further
generations of group generics, if any) are also merged.  
  
  The merging rule is as follows:
each generic is merged across packages, and the group generics are then merged, finally
adding the directly defined methods of \code{f}.

The effect of the merging rule is that any method directly defined for \code{f} on any
included package overrides a method for the same signature defined for the group generic;
similarly for the group generic and its group, if any, etc.

For \code{f} or for a specific group generic, methods override in the order of the packages
being searched.  A method for a particular signature on a particular package overrides
any methods for the same signature on packages later on in the list of packages being
searched.

The slot "allMethods" of the merged methods list is set to a copy of the methods slot;
this is the slot where inherited methods are stored.

  }
\item{\code{doPrimitiveMethod}:}{ 
  do a primitive call to builtin function \code{name} the definition and call
provided, and carried out in the environment \code{ev}.

A call to \code{doPrimitiveMethod} is used when the actual method is a .Primitive.
(because primitives don't behave correctly as ordinary functions,
not having either formal arguments nor a function body).

  
  }
\item{\code{getGeneric}:}{ 
  return the definition of the function named f as a generic.

If there is no definition in the current search list, throws an error or returns
NULL according to the value of mustFind.

  
  }
\item{\code{getGroup}:}{ 
  return the groups to which this generic belongs.  

  If \code{recursive=TRUE}, also all the
group(s) of these groups.
}
\item{\code{getMethodsMetaData}, \code{assignMethodsMetaData},
  \code{mlistMetaName}:}{
  utilities to manage methods list objects in a particular database.
  Not to be called directly.
  }
}
}
\keyword{programming}
