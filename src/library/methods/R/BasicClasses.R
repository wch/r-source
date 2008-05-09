#  File src/library/methods/R/BasicClasses.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

.InitBasicClasses <- function(envir)
{
    ## setClass won't allow redefining basic classes,
    ## so make the list of these empty for now.
    assign(".BasicClasses", character(), envir)
    ## hide some functions that would break because the basic
    ## classes are not yet defined
    real.reconcileP <- reconcilePropertiesAndPrototype
    assign("reconcilePropertiesAndPrototype",
           function(name, properties, prototype, extends, where) {
               list(properties=properties, prototype = prototype, extends = extends)
           }, envir)
    clList = character()
    setClass("VIRTUAL", where = envir); clList <- c(clList, "VIRTUAL")
    setClass("oldClass", where = envir) ## NOT A BASIC CLASS
    setClass("ANY", where = envir); clList <- c(clList, "ANY")
    setClass("vector", where = envir); clList <- c(clList, "vector")
    setClass("missing", where = envir); clList <- c(clList, "missing")
    ## "numeric" is the class returned by class() for double vectors
    vClasses <- c("logical", "numeric", "character",
                  "complex", "integer", "raw",
                  "expression", "list")
    ## now some pseudo-classes in base, marked specially for new()
    for(.class in vClasses) {
        .setBaseClass(.class, prototype = newBasic(.class), where = envir)
    }
    .setBaseClass("expression", prototype = expression(), where = envir)
    clList <- c(clList, vClasses)
    nullF <- function()NULL; environment(nullF) <- .GlobalEnv
    .setBaseClass("function", prototype = nullF, where = envir); clList <- c(clList, "function")

    setClass("language", where = envir); clList <- c(clList, "language")
    .setBaseClass("environment", prototype = new.env(), where = envir); clList <- c(clList, "environment")

    .setBaseClass("externalptr", prototype = .newExternalptr(), where = envir); clList <- c(clList, "externalptr")

    ## S4 is a basic class, but virtual:  new("S4") is an error; must be an actual S4 class
     tmp <- newClassRepresentation(className="S4", prototype = defaultPrototype(), virtual=TRUE, package = "methods")
    assignClassDef("S4", tmp, where = envir); clList <- c(clList, "S4")

    ## NULL is weird in that it has NULL as a prototype, but is not virtual
    tmp <- newClassRepresentation(className="NULL", prototype = NULL, virtual=FALSE, package = "methods")
    assignClassDef("NULL", tmp, where = envir); clList <- c(clList, "NULL")


    setClass("structure", where = envir); clList <- c(clList, "structure")
    stClasses <- c("matrix", "array") # classes that have attributes, but no class attr.
    for(.class in stClasses) {
        .setBaseClass(.class, prototype = newBasic(.class), where = envir)
    }
    ## "ts" will be defined below as an S3 class, but it is still
    ## included in .BasicClasses, to allow its coerce() method to use
    ## as.ts().  This decision may be revisited.
    clList <- c(clList, stClasses, "ts")
    assign(".BasicClasses", clList, envir)

    ## Now we can define the SClassExtension class and use it to instantiate some
    ## is() relations.
    .InitExtensions(envir)

    for(.class in vClasses)
        setIs(.class, "vector", where = envir)

    setIs("integer", "numeric", where = envir)

    setIs("structure", "vector", coerce = .gblEnv(function(object) as.vector(object)),
          replace = .gblEnv(function(from, to, value) {
              attributes(value) <- attributes(from)
              value
          }),
          where = envir)

    setIs("array", "structure", where = envir)
    setIs("matrix", "array", where = envir)
    setIs("array", "matrix", test = .gblEnv(function(object) length(dim(object)) == 2),
          replace = .gblEnv(function(from, to, value) {
              if(is(value, "matrix"))
                  value
              else
                  stop("replacement value is not a matrix")
          }),
          where = envir)

    ## Some class definitions extending "language", delayed to here so
    ## setIs will work.
    .setBaseClass("name", "language", prototype = as.name("<UNDEFINED>"), where = envir); clList <- c(clList, "name")
    .setBaseClass("call", "language", prototype = quote("<undef>"()), where = envir); clList <- c(clList, "call")
    .setBaseClass("{", "language", prototype = quote({}), where = envir); clList <- c(clList, "{")
    .setBaseClass("if", "language", prototype = quote(if(NA) TRUE else FALSE), where = envir); clList <- c(clList, "if")
    .setBaseClass("<-", "language", prototype = quote("<undef>"<-NULL), where = envir); clList <- c(clList, "<-")
    .setBaseClass("for", "language", prototype = quote(for(NAME in logical()) NULL), where = envir); clList <- c(clList, "for")
    .setBaseClass("while", "language", prototype = quote(while(FALSE) NULL), where = envir); clList <- c(clList, "while")
    .setBaseClass("repeat", "language", prototype = quote(repeat{break}), where = envir); clList <- c(clList, "repeat")
    .setBaseClass("(", "language", prototype = quote((NULL)), where = envir); clList <- c(clList, "(")

    ## a virtual class used to allow NULL as an indicator that a possible function
    ## is not supplied (used, e.g., for the validity slot in classRepresentation
    setClass("OptionalFunction", where = envir)
    setIs("function", "OptionalFunction", where = envir)
    setIs("NULL", "OptionalFunction")
    assign(".BasicClasses", clList, envir)
    assign(".SealedClasses", clList, envir)
    ## restore the true definition of the hidden functions
    assign("reconcilePropertiesAndPrototype", real.reconcileP, envir)
}

.InitS3Classes <- function(envir) {
      ## call setOldClass on some known old-style classes.  Ideally this would be done
    ## in the code that uses the classes, but that code doesn't know about the methods
    ## package.
    ## Two steps; first, those classes with a known prototype.  These
    ## can be non-Virtual
    clList <- get(".SealedClasses", envir = envir)
    for(i in seq_along(.OldClassesPrototypes)) {
      el <- .OldClassesPrototypes[[i]]
      if(is.list(el) && length(el) > 1)
        setOldClass(el[[1]], prototype = el[[2]],  where = envir)
       else
        warning("OOPS: something wrong with line ",i, " in .OldClassesPrototypes")
    }
  ## note re "ts".  An attempt was made, through R 2.7.0, to treat this as an S4 class.
    ## However, this has not proved to be very practical; for example,
    ## internal restrictions prevent assigning a slot "tsp" if it is
    ## temporarily inconsistent with the data part, and vice-versa.
    ## Also "mts" extends "ts" in an S3 sense but doesn't work as an
    ## S4 extension.  Beginning with 2.8, "ts" will be treated as a
    ## registered S3 class, but we try to keep its "structure"
    ## behavior
    setIs("ts", "structure", where = envir)
    setMethod("initialize", "ts",
              function(.Object,  ...) {
                  if(nargs() < 2) # guaranteed to be called with .Object from new
                     return( .Object)
                  args <- list(...)
                  argnames <- names(args)
                  if(is.null(argnames))
                    slotnames <- FALSE
                  else
                    slotnames <- nzchar(argnames) & is.na(match(argnames, .tsArgNames))
                  if(any(slotnames)) {
                      value = do.call(stats::ts, args[!slotnames])
                      .mergeAttrs(value, .Object, args[slotnames])
                  }
                  else
                    .mergeAttrs(stats::ts(...), .Object, list())
              })
    ## the following mimics settings for other basic classes ("ts" was
    ## not defined at the time these are done).
    setMethod("coerce", c("ANY", "ts"), function (from, to, strict = TRUE) 
              {
                  value <- as.ts(from)
                  if(strict) {
                      attrs <- attributes(value)
                      if(length(attrs)>2)
                        attributes(value) <- attrs[c("class", "tsp")]
                  }
                  value
              },
              where = envir)
    setMethod("show", "ts", function(object) {
        cl <- as.character(class(object))
        if(!identical(cl, "ts"))
          cat("Object of class \"", cl, "\"\n", sep = "")
        print(as(object, "ts"))
        otherSlots <- slotNames(cl)
        otherSlots <- otherSlots[is.na(match(otherSlots, c(".Data", "tsp")))]
        for(what in otherSlots) {
            cat('Slot "', what, '":\n', sep = "")
            show(slot(object, what))
            cat("\n")
        }
    })
        


    ## Next, miscellaneous S3 classes.
    for(cl in .OldClassesList)
        setOldClass(cl, where = envir)
    ## some S3 classes have inheritance on an instance basis, that breaks the S4 contains
    ## model.  To emulate their (unfortunate) behavior requires a setIs with a test.
    for(cl in .OldIsList)
        .setOldIs(cl, envir)
    assign(".SealedClasses", c(clList,unique(unlist(.OldClassesList))),  envir)
  }

### create a class definition for one of the pseudo-classes in base
### The class name does _not_ have a package attribute, which signals
### the C coded for new() to return an object w/o explicit class
### attribute, to be consistent with older R code
.setBaseClass <- function(cl, ..., where) {
    setClass(cl, ..., where = where)
    def <- getClassDef(cl, where)
    def@className <- as.character(def@className)
    assignClassDef(cl, def, where = where)
}


.tsArgNames <- names(formals(stats::ts))

### The following methods are not currently installed.  (Tradeoff between intuition
### of users that new("matrix", ...) should be like matrix(...) vs
### consistency of new().  Relevant when new class has basic class as its data part.
###
### To install the methods below, uncomment the last line of the function
### .InitMethodDefinitions in ./MethodsListClass.R
.InitBasicClassMethods <- function(where) {
    ## methods to initialize "informal" classes by using the
    ## functions of the same name.
    ##
    ## These methods are designed to be inherited or extended
    setMethod("initialize", "matrix",
              function(.Object, data =   NA, nrow = 1, ncol = 1,
                       byrow = FALSE, dimnames = NULL, ...) {
                  if(nargs() < 2) # guaranteed to be called with .Object from new
                      .Object
                  else if(is.matrix(data) && nargs() == 2 + length(list(...)))
                      .mergeAttrs(data, .Object, list(...))
                  else {
                      value <- matrix(data, nrow, ncol, byrow, dimnames)
                      .mergeAttrs(value, .Object, list(...))
                  }
              })
    setMethod("initialize", "array",
              function(.Object, data =   NA, dim = length(data),
                       dimnames = NULL, ...) {
                  if(nargs() < 2) # guaranteed to be called with .Object from new
                      .Object
                  else if(is.array(data) && nargs() == 2 + length(list(...)))
                      .mergeAttrs(data, .Object, list(...))
                  else {
                      value <- array(data, dim, dimnames)
                      .mergeAttrs(value, .Object, list(...))
                  }
              })
}

## .OldClassList is a purely heuristic list of known old-style classes, with emphasis
## on old-style class inheritiance.  Used in .InitBasicClasses to call setOldClass for
## each known class pattern.
## .OldClassesPrototypes is a list of S3 classes for which prototype
## objects are known & reasonable.  The classes will reappear in
## .OldClassesList, but will have been initialized first in
## .InitBasicClasses.  NB:  the methods package will NOT set up
## prototypes for S3 classes except those in package base and for "ts"
## (and would rather not do those either).  The package that owns the
## S3 class should have code to call setOldClass in its
## initialization.
.OldClassesPrototypes <-
  list(
       list("data.frame",  data.frame(), "data.frame"),
       list("factor",  factor()),
       list(c("ordered", "factor"), ordered(character())),
       list("table",  table(factor())),
       list("summary.table",  summary.table(table(factor()))),
       list("ts", stats::ts())
       )
.OldClassesList <-
    list(
         c("anova", "data.frame"),
         c("mlm", "lm"),
         c("aov", "lm"), # see also .OldIsList
         c("maov", "mlm", "lm"),
         "POSIXt", "POSIXct", "POSIXlt", # see .OldIsList
         "dump.frames",
         c("ordered", "factor"),
         c("glm.null", "glm", "lm"),
         c("anova.glm.null", "anova.glm"),
         "hsearch",
         "integrate",
         "packageInfo",
         "libraryIQR",
         "packageIQR",
         "mtable",
         "table",
         "summary.table",
         c("mts", "ts"),
         "recordedplot",
         "socket",
         "packageIQR",
         "density",
         "formula",
         "logLik",
         "rle"
)

# These relations sometimes hold, sometimes not:  have to look in the S3
# class attribute to test.
.OldIsList <- list(
                   c("POSIXt", "POSIXct"),
                   c("POSIXt", "POSIXlt"),
                   c("aov","mlm")
                   )
