\name{ClassUtils}
\alias{testVirtual}
\alias{SessionClassMetaData}
\alias{makePrototypeFromClassDef}
\alias{newEmptyObject}
\alias{completeClassDefinition}
\alias{getFromClassDef}
\alias{setInClassDef}
\alias{synchronizeClassDef}
\alias{getProperties}
\alias{setProperties}
\alias{getSlots}
\alias{getExtends}
\alias{getAllSuperClasses}
\alias{setExtends}
\alias{getPrototype}
\alias{setPrototype}
\alias{getVirtual}
\alias{isVirtualClass}
\alias{setVirtual}
\alias{getSubclasses}
\alias{setSubclasses}
\alias{getClassName}
\alias{setClassName}
\alias{assignClassDef}
\alias{newClassEnvironment}
\alias{newBasic}
\alias{makeExtends}
\alias{reconcilePropertiesAndPrototype}
\alias{tryNew}
\alias{empty.dump}
\alias{showClass}
\alias{showExtends}
\alias{print.classRepEnvironment}
\alias{getFromClassMetaData}
\alias{assignToClassMetaData}
\alias{removeFromClassMetaData}
\alias{unsetClass}
\alias{extendsCoerce}
\alias{findExtends}
\alias{completeExtends}
\alias{classMetaName}
\alias{methodsMetaName}
\alias{as.data.frame}
\alias{nullSymbol}
\alias{requireMethods}
%% not explicitly documented here
\alias{.BasicClasses}
\alias{.BasicVectorClasses}
\alias{classGetsInR}
\alias{classInR}
\alias{.InitBasicClasses}
\alias{.InitMethodsListClass}

\title{Utilities for Managing Class Definitions}
\usage{
testVirtual(properties, extends, prototype)

makePrototypeFromClassDef(properties, prototype, extends)

newEmptyObject()

completeClassDefinition(Class)

getFromClassDef(ClassDef, what)

setInClassDef(ClassDef, what, value, synchronize=TRUE)

synchronizeClassDef(ClassDef)

getProperties(ClassDef)

setProperties(ClassDef, value)

getSlots(x, complete = TRUE)

getExtends(ClassDef)

getAllSuperClasses(ClassDef)

setExtends(ClassDef, value)

getPrototype(ClassDef)

setPrototype(ClassDef, value)

getVirtual(ClassDef)

isVirtualClass(Class)

setVirtual(ClassDef, value)

getSubclasses(ClassDef)

setSubclasses(ClassDef, value)

getClassName(ClassDef)

setClassName(ClassDef, value)

assignClassDef(Class, def, where=.GlobalEnv)

newClassEnvironment(name, properties, extends, prototype, subclasses, virtual)

newBasic(Class, ..., .Force=FALSE)

makeExtends(extends)

reconcilePropertiesAndPrototype(name, properties, prototype)

tryNew(Class)

empty.dump()

showClass(Class, complete=TRUE, propertiesAreCalled="Properties")

showExtends(ext)

print.classRepEnvironment(x, ...)

getFromClassMetaData(name)

assignToClassMetaData(name, value)

removeFromClassMetaData(name)

unsetClass(Class)

extendsCoerce(fromClass, Class)

findExtends(class1, class2)

completeExtends(ClassDef)

classMetaName(name)

methodsMetaName(prefix, name)

as.data.frame(x, row.names=NULL, optional=FALSE)

nullSymbol()

requireMethods(functions, signature, message)

}
\description{

These are various functions to support the definition and use of
formal classes.  Most of them are rarely suitable to be called directly.
  
\describe{
\item{\code{testVirtual}:}{ 
  Test for a Virtual Class.
Figures out, as well as possible, whether the class with these properties,
extension, and prototype is a virtual class.
Can be forced to be virtual by extending "VIRTUAL".  
  
  Otherwise, a class is
virtual only if it has no slots, extends no non-virtual classes, and has a
NULL Prototype

  
  }
\item{\code{makePrototypeFromClassDef}:}{ 
  Makes the prototype implied by
the class definition.

The following three rules are applied in this order.

If the class has slots, then the prototype for each
slot is used by default, but a corresponding element in the explicitly supplied
prototype, if there is one, is used instead (but it must be coercible to the
class of the slot).

If there are no slots but a non-null prototype was specified, this is returned.

If there is a single non-virtual superclass (a class in the extends list),
then its prototype is used.

If all three of the above fail, the prototype is \code{NULL}.

  
  }
\item{\code{newEmptyObject}:}{ 
  Utility function to create an empty object into which slots can be
set.  
  
  Currently just creates an empty list with class "NULL"

Later version should create a special object reference that marks an
object currently with no slots and no data.

  }
\item{\code{completeClassDefinition}:}{ 
  Completes the definition of Class, relative to the current session.

The completed definition is stored in the session's class metadata,
to be retrieved the next time that getClass is called on this class,
and is returned as the value of the call.

  
  }
\item{\code{getFromClassDef}:}{ 
  Extracts one of the intrinsically defined class definition properties
(".Poperties", etc.)  Strictly a utility function

  
  }
\item{\code{setInClassDef}:}{ 
  Set Property in Class Definition
set one of the intrinsically defined class definition properties
(".Poperties", etc.)  Strictly a utility function

  
  }
\item{\code{synchronizeClassDef}:}{ 
  Does whatever is needed to synchronize information in the class definition.

Basically computes derived information used to make object manipulations more efficient
but that need to be revised if information changes.

(Nothing at the moment)

  
  }
\item{\code{getProperties}:}{ 
  Extracts the class's Properties information from the class representation (only, not from
the name of the class).

  
  }
\item{\code{setProperties}:}{ 
  Sets the class's Properties information given the class representation (only, not from
the name of the class)

  
  }
  \item{\code{getSlots}:}{
    Returns a named character vector.  The names are the names of the
    slots, the values are the classes of the corresponding slots.  If
    \code{complete} is \code{TRUE}, all slots from all superclasses will
    be included.  The argument \code{x} can either be the name of a
    class or an object having that class.
  }

  \item{\code{getExtends}:}{ 
  Extracts the class's Extends information from the class representation (only, not from
the name of the class)

Contrast with the \code{findExtends} and \code{is} functions, both of which use indirect
information as well.

  
  }
\item{\code{getAllSuperClasses}:}{ 
  Get the names of all the classes that this class definition extends.

A utility function used to complete a class definition.  
  It returns all the
superclasses reachable from this class, in depth-first order (which is the order
used for matching methods); that is, the first direct superclass followed by all its
superclasses, then the next, etc.  (The order is relevant only in the case that
some of the superclasses have multiple inheritance.)

  
  
  }
\item{\code{setExtends}:}{ 
  set the class's Extends information given the class representation (only, not from
the name of the class)

  
  }
\item{\code{getPrototype}:}{ 
  extract the class's Prototype information from the class representation (only, not from
the name of the class)

  
  }
\item{\code{setPrototype}:}{ 
  set the class's Prototype information given the class representation (only, not from
the name of the class)

  
  }
\item{\code{getVirtual}:}{ 
  extract the class's Virtual information from the class representation (only, not from
the name of the class)

  
  }
\item{\code{isVirtualClass}:}{ 
  Is the named class a virtual class?  
  
  A class is virtual if explicitly declared to
be, and also if the class is not formally defined.

   }
\item{\code{setVirtual}:}{ 
  set the class's Virtual information given the class representation (only, not from
the name of the class)

  
  }
\item{\code{getSubclasses}:}{ 
  extract the class's Subclasses information from the class representation (only, not from
the name of the class)

  
  }
\item{\code{setSubclasses}:}{ 
  set the class's Subclasses information given the class representation (only, not from
the name of the class)

  
  }
\item{\code{getClassName}:}{ 
  The internal property in the class definition for the class name.

  
  }
\item{\code{setClassName}:}{ 
  set the name of the class inside the class definition

  
  }
\item{\code{assignClassDef}:}{ 
  assign the definition of the class to the specially named object

  
  }
\item{\code{newBasic}:}{ 
  the implementation of the function \code{new} for basic classes that don't have
a formal definition.  
  
  Any of these could have a formal definition, except for
Class="NULL" (disallowed because NULL can't have attributes).  For all cases except
"NULL", the class of the result will be set to Class.

See \code{new} for the interpretation of the arguments.

   }
\item{\code{makeExtends}:}{ 
  convert the argument to a list defining the extension mechanism.

  
  }
\item{\code{reconcilePropertiesAndPrototype}:}{ 
  makes a list or a structure look like a prototype for the given class.

Specifically, returns a structure with attributes corresponding to the slot
names in properties and values taken from prototype if they exist there, from
\code{new(classi)} for the class, \code{classi} of the slot if that succeeds, and \code{NULL}
otherwise.

The prototype may imply slots not in the properties list, since properties does not
include inherited slots (these are left unresolved until the class is used in a
session).

  
  }
\item{\code{tryNew}:}{ 
  Tries to generate a new element from this class, but if the attempt fails
(as, e.g., when the class is undefined or virtual) just returns NULL.

This is inefficient and also not a good idea when actually generating objects,
but is useful in the initial definition of classes.

  
  }
\item{\code{showClass}:}{ 
  Print the information about a class definition.  
  
  If complete==TRUE, include the
indirect information about extensions.

  
  }
\item{\code{showExtends}:}{ 
  Print the elements of the list of extensions.  
  
  Also used to print
extensions recorded in the opposite direction, via a subclass list

  }
\item{\code{unsetClass}:}{ 
  remove this class name from the internal table for this session.

Rarely needed to call this directly.

  
  }
\item{\code{extendsCoerce}:}{ 
  the function to perform coercion based on the is relation
between two classes.  
  
  May be explicitly stored in the metadata or
inferred.  If the latter, the inferred result is stored in the session
metadata for fromClass, to save recomputation later.

  
  }
\item{\code{findExtends}:}{ 
  Find the information that says whether class1 extends class2,
directly or indirectly.  
  
  This can be either a logical value or
an object containing various functions to test and/or coerce the relationship.

  
   }
\item{\code{completeExtends}:}{ 
  complete the extends information in the class definition, by following
transitive chains.

Elements in the immediate extends list may be added and current elements may be
replaced, either by replacing a conditional relation with an unconditional
one, or by adding indirect relations.

The resulting extends list is presented in depth-first order; that is, the
first immediate superclass followed by all the indirect relations through it,
then the next immediate superclass, etc.  
  
  Depth first order is required for
consistent elaboration of inherited methods during dispatch, because the
method dispatcher stores the inherited method under the immediate class name.
Under rather obscure situations of multiple inheritance, the result could be
ambiguous (depending on the order in which signatures are seen by the dispatcher
for a particular generic function), unless searching is done depth first.

  }
\item{\code{classMetaName}:}{ 
  a name for the object storing this class's definition

  
  }
\item{\code{methodsMetaName}:}{ 
  a name mangling device to simulate the meta-data in S4

  
  }
\item{\code{nullSymbol}:}{ 
  Returns the special name which is the symbol used to represent NULL slots as (non-NULL)
attributes.

  
  }
\item{\code{requireMethods}:}{ 
  Require a subclass to implement methods for the generic functions, for this signature.

For each generic, \code{setMethod} will be called to define a method that throws an error,
with the supplied message.

The \code{requireMethods} function allows virtual classes to require actual classes that
extend them to implement methods for certain functions, in effect creating an API
for the virtual class.  

  Otherwise, default methods for the corresponding function would
be called, resulting in less helpful error messages or (worse still) silently incorrect
results.

}
\item{SessionClassMetaData:}{ Contains the name of the special table in which
    class information is cached during the session.}
}
}
\keyword{programming}
