\name{RClassUtils}
%% FIXME the following are undocumented utilities (Sep 23/02)
\alias{completeSubclasses}
\alias{getClassPackage}
\alias{newClassRepresentation}
\alias{print.classRepresentation}
\alias{setExtendsMetaData}
\alias{setSubclassMetaData}
\alias{subclassesMetaName}
\alias{extendsMetaName}
\alias{completeSubclasses}
\alias{classPrototypeDef-class}
\alias{trySilent}
%%
\alias{testVirtual}
\alias{SessionClassMetaData}
\alias{makePrototypeFromClassDef}
\alias{newEmptyObject}
\alias{completeClassDefinition}
\alias{getProperties}
\alias{getSlots}
\alias{getExtends}
\alias{getAccess}
\alias{getAllSuperClasses}
\alias{superClassDepth}
\alias{getPrototype}
\alias{getVirtual}
\alias{isVirtualClass}
\alias{getSubclasses}
\alias{getClassName}
\alias{assignClassDef}
\alias{newBasic}
\alias{makeExtends}
\alias{reconcilePropertiesAndPrototype}
\alias{tryNew}
\alias{empty.dump}
\alias{showClass}
\alias{showExtends}
\alias{getFromClassMetaData}
\alias{assignToClassMetaData}
\alias{removeFromClassMetaData}
\alias{possibleExtends}
\alias{completeExtends}
\alias{classMetaName}
\alias{methodsPackageMetaName}
\alias{metaNameUndo}
\alias{requireMethods}
\alias{checkSlotAssignment}
\alias{defaultPrototype}
\alias{isClassDef}
\alias{validSlotNames}
\alias{getDataPart}
\alias{setDataPart}
%% not explicitly documented here
\alias{.BasicClasses}
\alias{.BasicVectorClasses}
\alias{.InitBasicClasses}
\alias{.InitMethodsListClass}
\alias{.setCoerceGeneric}

\title{Utilities for Managing Class Definitions}
\description{
  These are various functions to support the definition and use of
  formal classes.  Most of them are rarely suitable to be called
  directly.
}

\usage{
testVirtual(properties, extends, prototype)

makePrototypeFromClassDef(properties, prototype, extends)

newEmptyObject()

completeClassDefinition(Class, ClassDef)

getProperties(ClassDef)

getSlots(x, complete = TRUE)

getExtends(ClassDef)

getAccess(ClassDef)

getAllSuperClasses(ClassDef)

superClassDepth(ClassDef, soFar)

getPrototype(ClassDef)

getVirtual(ClassDef)

isVirtualClass(Class)

getSubclasses(ClassDef)

getClassName(ClassDef)

assignClassDef(Class, def, where=.GlobalEnv)

newBasic(Class, ...)

makeExtends(Class, to, coerce, test, replace, by, package, slots,
                 classDef1, classDef2)

reconcilePropertiesAndPrototype(name, properties, prototype, superClasses)

tryNew(Class)

trySilent(expr)

empty.dump()

showClass(Class, complete=TRUE, propertiesAreCalled="Properties")

showExtends(ext, printTo = stdout())

getFromClassMetaData(name)

assignToClassMetaData(name, value)

removeFromClassMetaData(name)

possibleExtends(class1, class2)

completeExtends(ClassDef)

classMetaName(name)

methodsPackageMetaName(prefix, name)

metaNameUndo(strings, prefix = "M", searchForm = FALSE)

requireMethods(functions, signature, message)

checkSlotAssignment(obj, name, value)

defaultPrototype()

isClassDef(object)

validSlotNames(names)

getDataPart(object)
setDataPart(object, value)

}

\section{Summary of Functions}{
  \describe{
    \item{\code{testVirtual}:}{ 
      Test for a Virtual Class.
      Figures out, as well as possible, whether the class with these
      properties, extension, and prototype is a virtual class.
      Can be forced to be virtual by extending "VIRTUAL".  
  
      Otherwise, a class is virtual only if it has no slots, extends no
      non-virtual classes, and has a \code{NULL} Prototype.
    }
    
    \item{\code{makePrototypeFromClassDef}:}{ 
      Makes the prototype implied by the class definition.

      The following three rules are applied in this order.

      If the class has slots, then the prototype for each slot is used
      by default, but a corresponding element in the explicitly supplied
      prototype, if there is one, is used instead (but it must be
      coercible to the class of the slot).

      If there are no slots but a non-null prototype was specified, this
      is returned.

      If there is a single non-virtual superclass (a class in the
      extends list), then its prototype is used.

      If all three of the above fail, the prototype is \code{NULL}.
    }

    \item{\code{newEmptyObject}:}{
      Utility function to create an empty object into which slots can be
      set.
  
      Currently just creates an empty list with class \code{"NULL"}.

      Later version should create a special object reference that marks
      an object currently with no slots and no data.
    }

    \item{\code{completeClassDefinition}:}{ 
      Completes the definition of \code{Class}, relative to the current
      session.

      The completed definition is stored in the session's class metadata,
      to be retrieved the next time that getClass is called on this
      class, and is returned as the value of the call.

      If \code{ClassDef} is omitted, the initial definition is obtained
      from the first package having a meta-object for this class.
    }

    \item{\code{getFromClassDef}:}{ 
      Extracts one of the intrinsically defined class definition
      properties (".Properties", etc.)  Strictly a utility function.
    }



    \item{\code{getProperties}:}{ 
      Extracts the class's Properties information from the class
      representation (only, not from the name of the class).
    }
    

    \item{\code{getSlots}:}{
      Returns a named character vector.  The names are the names of the
      slots, the values are the classes of the corresponding slots.  If
      \code{complete} is \code{TRUE}, all slots from all superclasses
      will be included.  The argument \code{x} can either be the name of
      a class or an object having that class.
    }

    \item{\code{getExtends}:}{ 
      Extracts the class's Extends information from the class
      representation (only, not from the name of the class)

      Contrast with the \code{possibleExtends} and \code{is} functions, both
      of which use indirect information as well.
    }

    \item{\code{getAllSuperClasses}, \code{superClassDepth}:}{ 
      Get the names of all the classes that this class definition
      extends.

      \code{getAllSuperClasses} is a utility function used to complete a
      class definition.  It returns all the superclasses reachable from
      this class, in breadth-first order (which is the order used for
      matching methods); that is, the first direct superclass followed
      by all its superclasses, then the next, etc.  (The order is
      relevant only in the case that some of the superclasses have
      multiple inheritance.)

      \code{superClassDepth}, which is called from
      \code{getAllSuperClasses}, returns the same information, but as a
      list with components \code{label} and \code{depth}, the latter for
      the number of generations back each class is in the inheritance
      tree.  The argument \code{soFar} is used to avoid loops in the
      network of class relationships.
    }

    \item{\code{getPrototype}:}{ 
      extract the class's Prototype information from the class
      representation (only, not from the name of the class)
    }
    
    \item{\code{getAccess}:}{ 
      extract the class's Access information from the class
      representation (only, not from the name of the class)
    }

    \item{\code{getVirtual}:}{ 
      extract the class's Virtual information from the class
      representation (only, not from the name of the class)
    }
    
    \item{\code{isVirtualClass}:}{ 
      Is the named class a virtual class?  
  
      A class is virtual if explicitly declared to be, and also if the
      class is not formally defined.
    }

    
    \item{\code{getSubclasses}:}{ 
      extract the class's Subclasses information from the class
      representation (only, not from the name of the class)
    }
    

    \item{\code{getClassName}:}{ 
      The internal property in the class definition for the class name.
    }
    
    \item{\code{assignClassDef}:}{ 
      assign the definition of the class to the specially named object
    }
    
    \item{\code{newBasic}:}{
      the implementation of the function \code{new} for basic classes
      that don't have a formal definition.  
  
      Any of these could have a formal definition, except for
      \code{Class="NULL"} (disallowed because \code{NULL} can't have
      attributes).  For all cases except \code{"NULL"}, the class of the
      result will be set to \code{Class}.

      See \code{new} for the interpretation of the arguments.
    }

    \item{\code{makeExtends}:}{ 
      convert the argument to a list defining the extension mechanism.
    }
    
    \item{\code{reconcilePropertiesAndPrototype}:}{ 
      makes a list or a structure look like a prototype for the given
      class.
      
      Specifically, returns a structure with attributes corresponding to
      the slot names in properties and values taken from prototype if
      they exist there, from \code{new(classi)} for the class,
      \code{classi} of the slot if that succeeds, and \code{NULL}
      otherwise.
      
      The prototype may imply slots not in the properties list, since
      properties does not include inherited slots (these are left
      unresolved until the class is used in a session).
    }
    
    \item{\code{tryNew}:}{ 
      Tries to generate a new element from this class, but if the attempt
      fails (as, e.g., when the class is undefined or virtual) just
      returns \code{NULL}. 
      
      This is inefficient and also not a good idea when actually
      generating objects, but is useful in the initial definition of
      classes.
    }
    
    \item{\code{showClass}:}{ 
      Print the information about a class definition.  
      
      If \code{complete} is \code{TRUE}, include the indirect
      information about extensions.
    }
    
    \item{\code{showExtends}:}{ 
      Print the elements of the list of extensions.  
      
      (Used also by \code{\link{promptClass}} to get the list of what
      and how for the extensions.)
    }
    
    
    \item{\code{possibleExtends}:}{ 
      Find the information that says whether class1 extends class2,
      directly or indirectly.
      
      This can be either a logical value or an object of class
      \code{\link{SClassExtension-class}} containing various
      functions to test and/or coerce the relationship.
    }
    
    \item{\code{completeExtends}:}{ 
      complete the extends information in the class definition, by
      following transitive chains.
      
      Elements in the immediate extends list may be added and current
      elements may be replaced, either by replacing a conditional
      relation with an unconditional one, or by adding indirect
      relations.
      
    }
    
    \item{\code{classMetaName}:}{ 
      a name for the object storing this class's definition
    }
    
    \item{\code{methodsPackageMetaName}:}{ 
      a name mangling device to hide metadata defining method and
      class information.
    }
  \item{metaNameUndo}{ As its name implies, this function undoes the
      name-mangling used to produce meta-data object names, and
      returns a object of class \code{\link{ObjectsWithPackage-class}}.
      }
    
    \item{\code{requireMethods}:}{
      Require a subclass to implement methods for the generic functions,
      for this signature.
      
      For each generic, \code{setMethod} will be called to define a
      method that throws an error, with the supplied message.
      
      The \code{requireMethods} function allows virtual classes to
      require actual classes that extend them to implement methods for
      certain functions, in effect creating an API for the virtual class.
      
      Otherwise, default methods for the corresponding function would be
      called, resulting in less helpful error messages or (worse still)
      silently incorrect results.
    }
    
    \item{\code{checkSlotAssignment}:}{
      Check that the value provided is allowed for this slot, by
      consulting the definition of the class.  Called from the C code
      that assigns slots.
      
      For privileged slots (those that can only be set by accesor
      functions defined along with the class itself), the class designer
      may choose to improve efficiency by validating the value to be
      assigned in the accessor function and then calling \code{slot<-}
      with the argument \code{check=FALSE}, to prevent the call to
      \code{checkSlotAssignment}.
    }
    
    \item{\code{defaultPrototype}:}{
      The prototype for a class which will have slots, is not a virtual 
      class, and does not extend one of the basic classes.  In future
      releases, this will likely be a non-vector R object type, but none
      of the current types (as of release 1.4) is suitable.
    }
    
    \item{\code{SessionClassMetaData}:}{
      Contains the name of the special table in which class information
      is cached during the session.
    }
    
    \item{\code{.InitBasicClasses}, \code{.InitMethodsListClass}, \code{.setCoerceGeneric}:}{
      These functions perform part of the initialization of classes and
      methods, and are called (only!) from \code{.First.lib}.
    }
    
    \item{\code{isClassDef}:}{
      Is \code{object} a representation of a class?
    }
    
    \item{\code{validSlotNames}:}{
      Returns \code{names} unless one of the names is reserved, in which
      case there is an error.  (As of writing, \code{"class"} is the
      only reserved slot name.)
    }
  \item{\code{getDataPart}, \code{setDataPart}:}{ Utilities called from the base C code
      to implement \code{object@.Data}.
      }
    
  }
}
\keyword{internal}
