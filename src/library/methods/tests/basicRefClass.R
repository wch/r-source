options(error = recover)
## simple call, only field names
fg <- setRefClass("foo", c("bar", "flag"))
f1<- new("foo")
f1$bar
f1 <- fg$new(flag = "testing")
f1$bar <- 1
stopifnot(identical(f1$bar, 1))
fg$methods(showAll = function() c(bar, flag))
stopifnot(all.equal(f1$showAll(), c(1, "testing")))

fg <- setRefClass("foo", list(bar = "numeric", flag = "character"),
            methods = list(
            addToBar = function(incr) {
                b = bar + incr
                bar <<- b
                b
            }
            ))
ff = new("foo", bar = 1.5)
stopifnot(identical(ff$bar, 1.5))
ff$bar <- pi
stopifnot(identical(ff$bar, pi))
## test against generator

f2 <- fg$new(bar = pi)
## identical does not return TRUE if *contents* of env are identical
stopifnot(identical(ff$bar, f2$bar), identical(ff$flag, f2$flag))

f2$flag <- "standard flag"
stopifnot(identical(f2$flag, "standard flag"))

## fg$lock("flag")
## tryCatch(f2$flag <- "other", error = function(e)e)


## add some accessor methods
fg$accessors("bar")

ff$setBar(1:3)
stopifnot(identical(ff$getBar(), 1:3))

ff$getBar()
stopifnot(all.equal(ff$addToBar(1), 2:4))


## Add a method
fg$methods(barTimes = function(x) {
    "This method multiples field bar by argument x
and this string is self-documentation"
    setBar(getBar() * x)})

ffbar <- ff$getBar()
ff$barTimes(10)
stopifnot(all.equal(ffbar * 10, ff$getBar()))
ff$barTimes(.1)

## inheritance.  redefines flag so should fail:
stopifnot(is(tryCatch(setRefClass("foo2", list(b2 = "numeric", flag = "complex"),
            contains = "foo",
            refMethods = list(addBoth = function(incr) {
                addToBar(incr) #uses inherited class method
                setB2(getB2() + incr)
                })),
          error = function(e)e), "error"))
## but with flag as a subclass of "character", should work
setClass("ratedChar", contains = "character", representation(score = "numeric"))
foo2 <- setRefClass("foo2", list(b2 = "numeric", flag = "ratedChar"),
            contains = "foo",
            methods = list(addBoth = function(incr) {
                addToBar(incr) #uses inherited class method
                b2<<- b2 + incr
                }))
## now lock the flag field; should still allow one write
foo2$lock("flag")
f2 <- foo2$new(bar = -3, flag = as("ANY", "ratedChar"), b2 = ff$bar)
## but not a second one
stopifnot(is(tryCatch(f2$flag <- "Try again",
         error = function(e)e), "error"))
f22 <- foo2$new(bar = f2$bar)
## same story if assignment follows the initialization
f22$flag <- f2$flag
stopifnot(is(tryCatch(f22$flag <- "Try again",
         error = function(e)e), "error"))
## Exporting superclass object
f22 <- fg$new(bar = f2$bar, flag = f2$flag)
f2e <- f2$export("foo")
stopifnot(identical(f2e$bar, f22$bar), identical(f2e$flag, f22$flag),
          identical(class(f2e), class(f22)))
stopifnot(identical(f2$flag,  as("ANY", "ratedChar")), identical(f2$bar, -3),
          all.equal(f2$b2, 2:4+0))
f2$addBoth(-1)
stopifnot(all.equal(f2$bar, -4), all.equal(f2$b2, 1:3+0))

## test callSuper()
setRefClass("foo3", fields = list(flag2 = "ratedChar"),
            contains = "foo2",
            methods = list(addBoth = function(incr) {
                callSuper(incr)
                flag2 <<- as(paste(flag, paste(incr, collapse = ", "), sep = "; "), "ratedChar")
                incr
            }))

f2 <- foo2$new(bar = -3, flag = as("ANY", "ratedChar"), b2 =  1:3)
f3 <- new("foo3")
f3$import(f2)
stopifnot(all.equal(f3$b2, f2$b2), all.equal(f3$bar, f2$bar), all.equal(f3$flag, f2$flag))
f3$addBoth(1)
stopifnot(all.equal(f3$bar, -2), all.equal(f3$b2, 2:4+0),
          all.equal(f3$flag2, as("ANY; 1", "ratedChar")))

## but the import should have used up the one write for $flag
stopifnot(is(tryCatch(f3$flag <- "Try again",
         error = function(e)e), "error"))

## a class with an initialize method, and an extra slot
setOldClass(c("simple.list", "list"))
fg4 <- setRefClass("foo4",
            contains = "foo2",
            methods = list(
              initialize = function(...) {
                  .self <- initFields(...)
                  .self@made = R.version
                  .self
              }),
            representation = list(made = "simple.list")
            )

f4 <- new("foo4", flag = "another test", bar = 1:3)
stopifnot(identical(f4@made, R.version))

## simple active binding test
abGen <- setRefClass("ab",
                  fields = list(a = "ANY",
                  b = function(x) if(missing(x)) a else {a <<- x; x}))

ab1 <- abGen$new(a = 1)

stopifnot(identical(ab1$a, 1), identical(ab1$b, 1))

ab1$b <- 2

stopifnot(identical(ab1$a, 2), identical(ab1$b, 2))

## a simple editor for matrix objects.  Method  $edit() changes some
## range of values; method $undo() undoes the last edit.
mEditor <- setRefClass("matrixEditor",
      fields = list( data = "matrix",
        edits = "list"),
      methods = list(
     edit = function(i, j, value) {
       ## the following string documents the edit method
       'Replaces the range [i, j] of the
        object by value.
        '
         backup <-
             list(i, j, data[i,j])
         data[i,j] <<- value
         edits <<- c(list(backup),
                     edits)
         invisible(value)
     },
     undo = function() {
       'Undoes the last edit() operation
        and update the edits field accordingly.
        '
         prev <- edits
         if(length(prev)) prev <- prev[[1]]
         else stop("No more edits to undo")
         edit(prev[[1]], prev[[2]], prev[[3]])
         ## trim the edits list
         length(edits) <<- length(edits) - 2
         invisible(prev)
     }
     ))
xMat <- matrix(1:12,4,3)
xx <- mEditor$new(data = xMat)
xx$edit(2, 2, 0)
xx$data
xx$undo()
mEditor$help("undo")
stopifnot(all.equal(xx$data, xMat))

## add a method to save the object
mEditor$methods(
     save = function(file) {
       'Save the current object on the file
        in R external object format.
       '
         base::save(.self, file = file)
     }
)

tf <- tempfile()
xx$save(tf) #$
load(tf)
unlink(tf)
stopifnot(identical(xx$data, .self$data))

markViewer <- ""
setMarkViewer <- function(what)
    markViewer <<- what

## Inheriting a reference class:  a matrix viewer
mv <- setRefClass("matrixViewer",
    fields = c("viewerDevice", "viewerFile"),
    contains = "matrixEditor",
    methods = list( view = function() {
        dd <- dev.cur(); dev.set(viewerDevice)
        devAskNewPage(FALSE)
        matplot(data, main = paste("After",length(edits),"edits"))
        dev.set(dd)},
        edit = # invoke previous method, then replot
          function(i, j, value) {
            callSuper(i, j, value)
            view()
          }))

## initialize and finalize methods
mv$methods( initialize = function(...) {
    viewerFile <<- tempfile("matrixView")
    pdf(viewerFile)
    viewerDevice <<- dev.cur()
    message("Plotting to ", viewerFile)
    dev.set(dev.prev())
    setMarkViewer("ON")
    initFields(...)
  },
  finalize = function() {
    dev.off(viewerDevice)
    setMarkViewer("OFF")
  })

ff = new("matrixViewer", data = xMat)
stopifnot(identical(markViewer, "ON")) # check initialize
ff$edit(2,2,0)
ff$data
ff$undo()
stopifnot(all.equal(ff$data, xMat))
rm(ff)
gc()
stopifnot(identical(markViewer, "OFF")) #check finalize
