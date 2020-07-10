####  Tests for table() related  'stats' functionality


## Verify that the functions supplied to addmargins
## are evaluated in the correct environment
mB <- matrix(c(16, 26, 27, 20,
               24, 20, 19, 25,
               40, 46, 46, 45), 4L, 3L,
             dimnames =
                 list(Sea = c("Black", "Dead", "Red",  "White"),
                      Bee = c("Buzz", "Hum", "Total")))

local({
    sqsm <- function(x) sum(x)^2/100
    mB1 <- addmargins(mB, 1, list(list(Sum = sum, sqS = sqsm)))
    mB2 <- addmargins(mB, 1, list(list(Sum = sum, sqS = function(x) sum(x)^2/100)))
    stopifnot(identical(mB1[1:4,], mB),
              identical(mB1, mB2))
})
## mB1 .. gave Error in eval(.. ..) : object 'sqsm' not found
