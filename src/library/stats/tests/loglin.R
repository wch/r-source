### examples from investigating UBSAN reports in vcdExtra::seq_loglm

x <- margin.table(Titanic, 1)
## Next two had UBSAN reports in R 4.5.0 from using --x on a 0-length R-allocated  array.
loglin(x, NULL)
loglin(x, list())

loglin(x, list(1))

## failed in R 4.4.2 as R code assumed >= 2 parameters
loglin(x, NULL, param = TRUE)
loglin(x, list(1), param = TRUE)


## Versions of some MASS::loglm examples
# loglm(~ Type + Origin, xtabs(~ Type + Origin, Cars93))
if(require("MASS", quietly = TRUE))
   loglin(xtabs(~ Type + Origin, Cars93), list(Type=1, Origin=2))

# fm <- loglm(~ 1 + 2 + 3 + 4, minn38a)
mn <- structure(c(53L, 163L, 309L, 13L, 28L, 38L, 7L, 30L, 17L, 76L,
                  118L, 89L, 36L, 116L, 225L, 11L, 53L, 68L, 16L, 41L,
                  49L, 111L, 214L, 210L, 52L, 162L, 243L, 49L, 129L,
                  284L, 28L, 64L, 79L, 521L, 708L, 448L, 48L, 130L,
                  237L, 29L, 62L, 63L, 18L, 47L, 57L, 191L, 305L, 219L,
                  12L, 35L, 72L, 10L, 37L, 21L, 5L, 11L, 20L, 101L,
                  152L, 95L, 9L, 19L, 42L, 15L, 22L, 19L, 1L, 13L, 10L,
                  130L, 174L, 105L, 3L, 25L, 36L, 6L, 15L, 19L, 1L, 9L,
                  14L, 88L, 158L, 93L, 87L, 216L, 256L, 17L, 14L, 10L,
                  3L, 4L, 2L, 105L, 118L, 53L, 72L, 159L, 176L, 18L,
                  28L, 22L, 6L, 14L, 8L, 209L, 227L, 95L, 52L, 119L,
                  119L, 14L, 44L, 33L, 17L, 13L, 10L, 541L, 578L, 257L,
                  88L, 158L, 144L, 14L, 36L, 20L, 9L, 15L, 12L, 328L,
                  304L, 115L, 32L, 43L, 42L, 12L, 7L, 7L, 1L, 5L, 2L,
                  124L, 119L, 56L, 14L, 24L, 24L, 5L, 15L, 4L, 2L, 6L,
                  2L, 148L, 131L, 61L, 20L, 41L, 32L, 4L, 13L, 4L, 3L,
                  5L, 2L, 109L, 88L, 41L),
                dim = c(3L, 4L, 7L, 2L),
                dimnames = list(hs = c("L", "M", "U"),
                                phs = c("C", "E", "N", "O"),
                                fol = c("F1", "F2", "F3", "F4", "F5", "F6", "F7"),
                                sex = c("F", "M")))
loglin(mn, list(hs = 1, phs = 2, fol = 3, sex = 4), param = TRUE)
