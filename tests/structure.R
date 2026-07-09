### Tests of structure() and deparsing.

## deparsing has always treated some attributes specially (apparently
## to allow inter-operability with S).
## The mapping is
## as printed: "dim", "dimnames", "names", "tsp", "levels"
## deparsed: ".Dim", ".Dimnames", ".Names", ".Tsp", ".Label"
## structure() remaps to the printed form.

## The remapping in deparse has been removed in R 4.2.0.
## The re-remapping (e.g. from ".Dim" to "dim") is deprecated for R 4.7.0

## Now __warn__ about old names use:
options(keep.parse.data = FALSE) -> op
getVaW <- function(expr) { # obj = TRUE - version
    W <- NULL
    withCallingHandlers(val <- expr,
                        warning = function(w) {
                            W <<- w
                            invokeRestart("muffleWarning") })
    structure(val %||% quote(._NULL_()), warning = W) # NULL cannot have attr.
}
noW <- function(x) {attr(x, "warning") <- NULL ; x }
chk_n_prt <- function(w) {
    w <- attr(w,"warning")
    cat(conditionMessage(w), "\n")
    inherits(w, "deprecatedWarning")
}

X <- matrix(1:4, 2, 2, dimnames=list(c("A", "B"), 1:2))
names(attributes(X))
cat(deparse(X), "\n")
w1 <- getVaW(structure(1:4, .Dim = c(2L, 2L),
                       .Dimnames = list(c("A", "B"), c("1", "2"))))
stopifnot(chk_n_prt(w1), identical(noW(w1), X))

z <- ts(1:10, frequency = 4, start = c(1959, 2))
attributes(z) # had '.Tsp' component, now 'tsp'
cat(deparse(z), "\n")
w2 <- getVaW(structure(1:10, .Tsp = c(1959.25, 1961.5, 4), class = "ts"))
stopifnot(chk_n_prt(w2), identical(noW(w2), z))

## levels <-> .Label is most relevant to factors, but is always remapped; also .Dim :
x <- array(1:3, 3)
attr(x, "levels") <- letters[x]
cat(deparse(x), "\n") # did show  ' .Label = ..'  but shows "levels = .." since 2021
w3 <- getVaW(structure(1:3, .Label = c("a", "b", "c"), .Dim = 3L))
stopifnot(chk_n_prt(w3), identical(noW(w3), x))


## factors were long deparsed with double (rather than integer codes).
## As from R 2.5.0 parsing such a deparse will given an error, so
## structure() coerces the codes to an integer vector.
## Example from an earlier version of Puromycin.R
state <- structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                     2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
                   levels = c("treated", "untreated"), class = "factor")
typeof(state)       # "integer" -- probably will become "double"
storage.mode(state) # ditto
attributes(state)
cat(deparse(state))

state2 <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L),
                    levels = c("treated", "untreated"), class = "factor")
identical(state,state2)

options(op) # in case this is source()d
