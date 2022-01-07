### Tests of structure() and deparsing.

## deparsing has always treated some attributes specially (apparently
## to allow inter-operability with S).
## The mapping is
## as printed: "dim", "dimnames", "names", "tsp", "levels"
## deparsed: ".Dim", ".Dimnames", ".Names", ".Tsp", ".Label"
## structure() remaps to the printed form.

## The remapping in deparse will be removed in R 4.2.0.

X <- matrix(1:4, 2, 2, dimnames=list(c("A", "B"), 1:2))
names(attributes(X))
cat(deparse(X), "\n")
Y <- structure(1:4, .Dim = c(2L, 2L),
               .Dimnames = list(c("A", "B"), c("1", "2")))
identical(X, Y)

z <- ts(1:10, frequency = 4, start = c(1959, 2))
attributes(z)
cat(deparse(z), "\n")
z2 <- structure(1:10, .Tsp = c(1959.25, 1961.5, 4), class = "ts")
identical(z, z2)

## levels <-> .Label is most relevant to factors, but is always remapped.
x <- 1:3
attr(x, "levels") <- letters[x]
cat(deparse(x), "\n")
y <- structure(1:3, .Label = c("a", "b", "c"))
identical(x, y)


## Factors were long deparsed with double (rather than integer codes).
## As from R 2.5.0 parsing such a deparse will given an error, so
## structure() coerces the codes to an integer vector.
## Example from an earlier version of Puromycin.R
state <- structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                     2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
                   .Label = c("treated", "untreated"), class = "factor")
typeof(state)
storage.mode(state)
attributes(state)
cat(deparse(state))

state2 <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                    levels = c("treated", "untreated"), class = "factor")
identical(state,state2)
