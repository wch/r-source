set.seed(290875)
B <- 1e5

### note: psmirnov(, z = NULL) uses stats:::C_pSmirnov2x, z = something
### trigger Schröer-Trenkler algo
all.equal(psmirnov(1:999/1000, sizes = c(10, 20), z = 1:30), 
          psmirnov(1:999/1000, sizes = c(10, 20)))

### examples by Schröer & Trenkler (1995) without ties
all.equal(psmirnov(3 / 7, sizes = c(7, 5), z = 1:12), 
          psmirnov(3 / 7, sizes = c(7, 5)))

all.equal(psmirnov(sizes = c(m = 3, n = 4), q = 1/2, z = 1:7, lower.tail = FALSE),
          psmirnov(sizes = c(m = 3, n = 4), q = 1/2, lower.tail = FALSE))
psmirnov(sizes = c(m = 3, n = 4), q = 1/6, z = 1:7, lower.tail = FALSE, alternative = "greater")
psmirnov(sizes = c(m = 4, n = 3), q = 1/2, z = 1:7, lower.tail = FALSE, alternative = "greater")

all.equal(psmirnov(sizes = c(m = 5, n = 7), q = 3 / 7, z = 1:12, lower.tail = FALSE),
          psmirnov(sizes = c(m = 5, n = 7), q = 3 / 7, z = 1:12, lower.tail = FALSE))

all.equal(psmirnov(sizes = c(m = 300, n = 520), q = 1/8, z = 1:820, lower.tail = FALSE),
          psmirnov(sizes = c(m = 300, n = 520), q = 1/8, lower.tail = FALSE))
psmirnov(z = round(rnorm(820)), sizes = c(m = 300, n = 520), 
         q = 1/8, lower.tail = FALSE)

### tied example by Schröer & Trenkler (1995)
x <- c(1, 2, 2, 3, 3, 1, 2, 3, 3, 4, 5, 6)
psmirnov(z = x, sizes = c(m = 5, n = 7), q = 3 / 7, lower.tail = FALSE)
psmirnov(z = x, sizes = c(m = 5, n = 7), q = 3 / 7, lower.tail = FALSE, 
         exact = FALSE, simulate = TRUE, B = B)
psmirnov(z = x, sizes = c(m = 5, n = 7), q = 3 / 7, lower.tail = FALSE, alternative = "greater")
psmirnov(z = x, sizes = c(m = 5, n = 7), q = 3 / 7, lower.tail = FALSE, alternative = "greater",
         exact = FALSE, simulate = TRUE, B = B)
psmirnov(z = x, sizes = c(m = 5, n = 7), q = 3 / 7, lower.tail = TRUE, alternative = "greater")
psmirnov(z = x, sizes = c(m = 5, n = 7), q = 3 / 7, lower.tail = TRUE, alternative = "greater",
         exact = FALSE, simulate = TRUE, B = B)

### check quantiles
### Kim & Jennrich (1973) in Selected Tables in Mathematical Statistics, Vol 1
### (ed. Harter) Institute of Mathematical Statistics, page 129
all.equal(qsmirnov(1 - .05, sizes = c(m = 8, n = 6)) * 8 * 6, 34)
all.equal(qsmirnov(1 - .01, sizes = c(m = 8, n = 6)) * 8 * 6, 40)
all.equal(qsmirnov(1 - .001, sizes = c(m = 8, n = 6)) * 8 * 6, 48)
all.equal(qsmirnov(1 - .05, sizes = c(m = 14, n = 10)) * 140, 74)
all.equal(qsmirnov(1 - .01, sizes = c(m = 14, n = 10)) * 140, 90)
all.equal(qsmirnov(1 - .001, sizes = c(m = 14, n = 10)) * 140, 106)
### Schröer algo
all.equal(qsmirnov(1 - .05, sizes = c(m = 8, n = 6), z = 1:14) * 8 * 6, 34)
all.equal(qsmirnov(1 - .01, sizes = c(m = 8, n = 6), z = 1:14) * 8 * 6, 40)
all.equal(qsmirnov(1 - .001, sizes = c(m = 8, n = 6), z = 1:14) * 8 * 6, 48)
all.equal(qsmirnov(1 - .05, sizes = c(m = 14, n = 10), z = 1:24) * 140, 74)
all.equal(qsmirnov(1 - .01, sizes = c(m = 14, n = 10), z = 1:24) * 140, 90)
all.equal(qsmirnov(1 - .001, sizes = c(m = 14, n = 10), z = 1:24) * 140, 106)
### Monte Carlo
all.equal(qsmirnov(1 - .05, sizes = c(m = 8, n = 6), z = 1:14, exact = FALSE, simulate = TRUE, B = B) * 8 * 6, 34)
all.equal(qsmirnov(1 - .01, sizes = c(m = 8, n = 6), z = 1:14, exact = FALSE, simulate = TRUE, B = B) * 8 * 6, 40)
all.equal(qsmirnov(1 - .001, sizes = c(m = 8, n = 6), z = 1:14, exact = FALSE, simulate = TRUE, B = B) * 8 * 6, 48)
all.equal(qsmirnov(1 - .05, sizes = c(m = 14, n = 10), z = 1:24, exact = FALSE, simulate = TRUE, B = B) * 140, 74)
all.equal(qsmirnov(1 - .01, sizes = c(m = 14, n = 10), z = 1:24, exact = FALSE, simulate = TRUE, B = B) * 140, 90)
all.equal(qsmirnov(1 - .001, sizes = c(m = 14, n = 10), z = 1:24, exact = FALSE, simulate = TRUE, B = B) * 140, 106)

### without ties
q <- qsmirnov(1:9/10, sizes = c(5, 7))
p <- psmirnov(q, sizes = c(5, 7))
all.equal(qsmirnov(p, sizes = c(5, 7)), q)

### with ties
obs <- c(1, 2, 2, 3, 3, 1, 2, 3, 3, 4, 5, 6)
q <- qsmirnov(1:9/10, sizes = c(5, 7), z = obs)
p <- psmirnov(q, sizes = c(5, 7), z = obs)
all.equal(qsmirnov(p, sizes = c(5, 7), z = obs), q)

### without ties
q <- qsmirnov(1:9/10, sizes = c(5, 7), alternative = "greater")
p <- psmirnov(q, sizes = c(5, 7), alternative = "greater")
all.equal(qsmirnov(p, sizes = c(5, 7), alternative = "greater"), q)

### with ties
obs <- c(1, 2, 2, 3, 3, 1, 2, 3, 3, 4, 5, 6)
q <- qsmirnov(1:9/10, sizes = c(5, 7), z = obs, alternative = "greater")
p <- psmirnov(q, sizes = c(5, 7), z = obs, alternative = "greater")
all.equal(qsmirnov(p, sizes = c(5, 7), z = obs, alternative = "greater"), q)

## PR#18582
## <https://bugs.r-project.org/show_bug.cgi?id=18582>
## KS.Test with specific data contradicts itself
## See explanations in the PR: the permutation distributions of the
## one-sided statistics can be different in the case of ties.
## The C code for the exact and MC simulations only implements one-sided
## D_{xy}^+ ("greater"); the R code now knows that D_{xy}^- = D_{yx}^+
## ("less").

x <- scan(text="4.7 5.5 6.7 7.1")
y <- scan(text="2.3 3.4 4.0 4.3 4.3 4.3 4.5 5.0")
all.equal(ks.test(x, y, alternative = "less")$p.value,
          ks.test(y, x, alternative = "greater")$p.value)
all.equal(ks.test(y, x, alternative = "less")$p.value,
          ks.test(x, y, alternative = "greater")$p.value)
