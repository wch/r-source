\name{RClassUtils}
\alias{testVirtual}
\alias{makePrototypeFromClassDef}
\alias{newEmptyObject}
\alias{completeClassDefinition}
\alias{getFromClassDef}
\alias{setInClassDef}
\alias{synchronizeClassDef}
\alias{getProperties}
\alias{setProperties}
\alias{getExtends}
\alias{getAllSuperClasses}
\alias{setExtends}
\alias{getPrototype}
\alias{getVirtual}
\alias{isVirtualClass}
\alias{getSubclasses}
\alias{getClassName}
\alias{setClassName}
\alias{assignClassDef}
\alias{newClassEnvironment}
\alias{newBasic}
\alias{makeExtends}
\alias{tryNew}
\alias{showClass}
\alias{showExtends}
\alias{print.classRepresentation}
\alias{getFromClassMetaData}
\alias{assignToClassMetaData}
\alias{removeFromClassMetaData}
\alias{unsetClass}
\alias{extendsCoerce}
\alias{findExtends}
\alias{completeExtends}
\alias{representation}
\title{Utilities for Managing Class Definitions}
\usage{
testVirtual(properties, extends, prototype)

makePrototypeFromClassDef(properties, prototype)

newEmptyObject()

completeClassDefinition(Class)

getFromClassDef(ClassDef, what)

setInClassDef(ClassDef, what, value, synchronize=T)

synchronizeClassDef(ClassDef)

getProperties(ClassDef)

setProperties(ClassDef, value)

getExtends(ClassDef)

getAllSuperClasses(ClassDef)

setExtends(ClassDef, value)

getPrototype(ClassDef)

getVirtual(ClassDef)

isVirtualClass(Class)

getSubclasses(ClassDef)

getClassName(ClassDef)

setClassName(ClassDef, value)

assignClassDef(Class, def, where=-1)

newClassEnvironment(name, properties, extends, prototype, subclasses, virtual)

newBasic(Class, ..., force=F)

makeExtends(extends)

tryNew(Class)

showClass(Class, complete=T, propertiesAreCalled="Properties")

showExtends(ext)

print.classRepresentation(x, ...)

getFromClassMetaData(name)

assignToClassMetaData(name, value)

removeFromClassMetaData(name)

unsetClass(Class)

extendsCoerce(fromClass, Class)

findExtends(class1, class2)

completeExtends(ClassDef)

representation(...)

}
\description{
  These are utility functions for the class/method package.  Most of
  them should
  not be used directly, particularly if you want your code to be
  compatible with S-Plus.
}
\details{
  
  \code{testVirtual}: 
  Test for a Virtual Class.
Figures out, as well as possible, whether the class with these properties,
extension, and prototype is a virtual class.
Can be forced to be virtual by extending "VIRTUAL".  Otherwise, a class is
virtual only if it has no slots, extends no non-virtual classes, and has a
NULL Prototype

  
  \code{makePrototypeFromClassDef}: 
  Makes the prototype implied by
the class definition.

  
  \code{newEmptyObject}: 
  Utility function to create an empty object into which slots can be
set.  Currently just creates an empty list.

Later version should create a special object reference that marks an
object currently with no slots and no data.

  
  \code{completeClassDefinition}: 
  Completes the definition of Class, relative to the current session.

The completed definition is stored in the session's class metadata,
to be retrieved the next time that getClass is called on this class,
and is returned as the value of the call.

  
  \code{getFromClassDef}: 
  Extracts one of the intrinsically defined class definition properties
(".Poperties", etc.)  Strictly a utility function

  
  \code{setInClassDef}: 
  Set Property in Class Definition
set one of the intrinsically defined class definition properties
(".Poperties", etc.)  Strictly a utility function

  
  \code{synchronizeClassDef}: 
  Does whatever is needed to synchronize information in the class definition.

Basically computes derived information used to make object manipulations more efficient
but that need to be revised if information changes.

(Nothing at the moment)

  
  \code{getProperties}: 
  Extracts the class's Properties information from the class representation (only, not from
the name of the class).

If the second argument is TRUE, all the properties of the inherited classes are included
as well.

  
  \code{setProperties}: 
  Sets the class's Properties information given the class representation (only, not from
the name of the class)

  
  \code{getExtends}: 
  extract the class's Extends information from the class representation (only, not from
the name of the class)

Contrast with the \code{findExtends} and \code{is} functions, both of which use indirect
information as well.

  
  \code{getAllSuperClasses}: 
  Get the names of all the classes that this class definition extends.

A utility function used to complete a class definition.  It returns all the
superclasses reachable from this class, in depth-first order (which is the order
used for matching methods); that is, the first direct superclass followed by all its
superclasses, then the next, etc.  (The order is relevant only in the case that
some of the superclasses have multiple inheritance.)

The list of superclasses is stored in the extends property of the session metadata.
User code should not need to call getAllSuperClasses directly; instead, use getExtends(getClass())
(which will complete the definition if necessary).

  
  \code{setExtends}: 
  set the class's Extends information given the class representation (only, not from
the name of the class)

  
  \code{getPrototype}: 
  extract the class's Prototype information from the class representation (only, not from
the name of the class)

  
  \code{getVirtual}: 
  extract the class's Virtual information from the class representation (only, not from
the name of the class)

  
  \code{isVirtualClass}: 
  Is the named class a virtual class?  A class is virtual if explicitly declared to
be, and also if the class is not formally defined.

  
  \code{getSubclasses}: 
  extract the class's Subclasses information from the class representation (only, not from
the name of the class)

  
  \code{getClassName}: 
  The internal property in the class definition for the class name.

  
  \code{setClassName}: 
  set the name of the class inside the class definition

  
  \code{assignClassDef}: 
  assign the definition of the class to the specially named object

  
  \code{newBasic}: 
  the implementation of the function \code{new} for basic classes that don't have
a formal definition.  Any of these could have a formal definition, except for
Class="NULL" (disallowed because NULL can't have attributes).  For all cases except
"NULL", the class of the result will be set to Class.

  
  \code{makeExtends}: 
  convert the argument to a list defining the extension mechanism.

  
  \code{tryNew}: 
  Tries to generate a new element from this class, but if the attempt fails
(as, e.g., when the class is undefined or virtual) just returns NULL.

This is inefficient and also not a good idea when actually generating objects,
but is useful in the initial definition of classes.

  
  \code{showClass}: 
  print the information about a class definition.  If complete==TRUE, include the
indirect information about extensions.

  
  \code{showExtends}: 
  print the elements of the list of extensions.  Also used to print
extensions recorded in the opposite direction, via a subclass list.

\code{removeFromMetaData}:
Remove the object with this name from the session metadata for classes.

\code{unsetClass}:
remove this class name from the internal table for this session.

Rarely needed to call this directly.

  \code{extendsCoerce}: 
  the function to perform coercion based on the is relation
between two classes.  May be explicitly stored in the metadata or
inferred.  If the latter, the inferred result is stored in the session
metadata for fromClass, to save recomputation later.

  
  \code{findExtends}: 
  Find the information that says whether class1 extends class2,
directly or indirectly.  This can be either a logical value or
an object containing various functions to test and/or coerce the relationship.

  
  \code{completeExtends}: 
  complete the extends information in the class definition, by following
transitive chains.

Elements in the immediate extends list may be added and current elements may be
replaced, either by replacing a conditional relation with an unconditional
one, or by adding indirect relations.

The resulting extends list is presented in depth-first order; that is, the
first immediate superclass followed by all the indirect relations through it,
then the next immediate superclass, etc.  Depth first order is required for
consistent elaboration of inherited methods during dispatch, because the
method dispatcher stores the inherited method under the immediate class name.
Under rather obscure situations of multiple inheritance, the result could be
ambiguous (depending on the order in which signatures are seen by the dispatcher
for a particular generic function), unless searching is done depth first.

  
  \code{classMetaName}: 
  a name for the object storing this class's definition

  
  \code{metaName}: 
  a name mangling device to simulate the meta-data in S4

  \code{representation}:
  a wrapper function to enclose the names and classes of slots, plus the
  classes of superclasses, as an argument to \code{\link{setClass}}.  It
  currently does nothing more than check the arguments for being
  sensible by this definition (e.g., each argument should be a single
  character string).

}
\seealso{ \link{Classes} }

\author{
  John Chambers
}
\examples{
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
