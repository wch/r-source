options(error = recover)
setRefClass("foo", list(bar = "numeric", flag = "character"),
            fieldPrototypes = list(flag = "standard flag"),
            classMethods = list(
            addToBar = function(incr) {
                b = getBar() + incr
                setBar(b)
                b
            }
            ))
ff = new("foo", bar = 1.5)
stopifnot(identical(ff$bar, 1.5))
ff$bar <- pi
stopifnot(identical(ff$bar, pi))

stopifnot(identical(ff$flag, "standard flag"))

ff$setBar(1:3)
stopifnot(identical(ff$bar, 1:3))

ff$getBar()
stopifnot(all.equal(ff$addToBar(1), 2:4))

## inheritance.  redefines flag so should fail:
try(setRefClass("foo2", list(b2 = "numeric", flag = "complex"),
            contains = "foo",
            classMethods = list(addBoth = function(incr) {
                addToBar(incr) #uses inherited class method
                setB2(getB2() + incr)
                })))
## but with flag as a subclass of "character", should work
setClass("ratedChar", contains = "character", representation(score = "numeric"))
setRefClass("foo2", list(b2 = "numeric", flag = "ratedChar"),
            contains = "foo",
            classMethods = list(addBoth = function(incr) {
                addToBar(incr) #uses inherited class method
                setB2(getB2() + incr)
                }))
f2 <- new("foo2", bar = -3, flag = as("ANY", "ratedChar"), b2 = ff$bar)
f2$export("foo")
stopifnot(identical(f2$flag,  as("ANY", "ratedChar")), identical(f2$bar, -3),
          all.equal(f2$b2, 2:4+0))
f2$addBoth(-1)
stopifnot(all.equal(f2$bar, -4), all.equal(f2$b2, 1:3+0))

setRefClass("foo3", contains = "foo2",
            classMethods = list(addBoth = function(incr) {
                callSuper(incr)
                setFlag(as(paste(getFlag(), paste(incr, collapse = ", "), sep = "; "), "ratedChar"))
                incr
            }))

f3 <- new("foo3")
f3$import(f2)
stopifnot(all.equal(f3$b2, f2$b2), all.equal(f3$bar, f2$bar), all.equal(f3$flag, f2$flag))
f3$addBoth(1)
stopifnot(all.equal(f3$bar, -3), all.equal(f3$b2, 2:4+0),
          all.equal(f3$flag, as("ANY; 1", "ratedChar")))

