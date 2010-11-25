
library(grid)

unitCheck <- function(u1, u2) {
    stopifnot(identical(as.character(u1), as.character(u2)))
}

# Mixture of rep() arguments based on example(rep) examples

# simple tests
simpleUnit <- unit(1:4, "npc")

unitCheck(rep(simpleUnit, 2),
          unit(rep(1:4, 2), "npc"))
unitCheck(rep(simpleUnit, each=2),
          unit(rep(1:4, each=2), "npc"))
unitCheck(rep(simpleUnit, c(2, 1, 2, 1)),
          unit(rep(1:4, c(2, 1, 2, 1)), "npc"))
unitCheck(rep(simpleUnit, each=2, len=4),
          unit(rep(1:4, each=2, len=4), "npc"))
unitCheck(rep(simpleUnit, each=2, len=10),
          unit(rep(1:4, each=2, len=10), "npc"))
unitCheck(rep(simpleUnit, each=2, times=3),
          unit(rep(1:4, each=2, times=3), "npc"))

simpleUnitMixed <- unit(1:4, c("npc", "cm"))

unitCheck(rep(simpleUnitMixed, 2),
          unit(rep(1:4, 2), c("npc", "cm")))
unitCheck(rep(simpleUnitMixed, each=2),
          unit(rep(1:4, each=2),
               rep(c("npc", "cm"), each=2)))
unitCheck(rep(simpleUnitMixed, c(2, 1, 2, 1)),
          unit(rep(1:4, c(2, 1, 2, 1)),
               rep(c("npc", "cm"), c(2, 1))))
unitCheck(rep(simpleUnitMixed, each=2, len=4),
          unit(rep(1:4, each=2, len=4),
               rep(c("npc", "cm"), each=2, len=4)))
unitCheck(rep(simpleUnitMixed, each=2, len=10),
          unit(rep(1:4, each=2, len=10),
               rep(c("npc", "cm"), each=2, len=10)))
unitCheck(rep(simpleUnitMixed, each=2, times=3),
          unit(rep(1:4, each=2, times=3),
               rep(c("npc", "cm"), each=2, times=3)))

units <- c("npc", "inch", "strwidth", "cm")
dataL <- list(NULL, NULL, "test", NULL)
unitWithData <- unit(1:4, units, data=dataL)
unitCheck(rep(unitWithData, 2),
          unit(rep(1:4, 2),
               rep(units, 2),
               rep(dataL, 2)))
unitCheck(rep(unitWithData, each=2),
          unit(rep(1:4, each=2),
               rep(units, each=2),
               rep(dataL, each=2)))
unitCheck(rep(unitWithData, c(2, 1, 2, 1)),
          unit(rep(1:4, c(2, 1, 2, 1)),
               rep(units, c(2, 1, 2, 1)),
               rep(dataL, c(2, 1, 2, 1))))
unitCheck(rep(unitWithData, each=2, len=4),
          unit(rep(1:4, each=2, len=4),
               rep(units, each=2, len=4),
               rep(dataL, each=2, len=4)))
unitCheck(rep(unitWithData, each=2, len=10),
          unit(rep(1:4, each=2, len=10),
               rep(units, each=2, len=10),
               rep(dataL, each=2, len=10)))
unitCheck(rep(unitWithData, each=2, times=3),
          unit(rep(1:4, each=2, times=3),
               rep(units, each=2, times=3),
               rep(dataL, each=2, times=3)))

# unit.arithmetic
unitArith <- unit(1, "npc") + unit(1:2, "inch")

unitCheck(rep(unitArith, 2),
          unit(1, "npc") + unit(rep(1:2, 2), "inch"))
unitCheck(rep(unitArith, each=2),
          unit(1, "npc") + unit(rep(1:2, each=2), "inch"))
unitCheck(rep(unitArith, c(2, 1)),
          unit(1, "npc") + unit(rep(1:2, c(2 ,1)), "inch"))
unitCheck(rep(unitArith, each=2, len=3),
          unit(1, "npc") + unit(rep(1:2, each=2, len=3), "inch"))
unitCheck(rep(unitArith, each=2, len=5),
          unit(1, "npc") + unit(rep(1:2, each=2, len=5), "inch"))
unitCheck(rep(unitArith, each=2, times=3),
          unit(1, "npc") + unit(rep(1:2, each=2, times=3), "inch"))

# unit.list
arg1 <- unit(1, "npc") + unit(1:2, "inch")
arg2 <- unit(3, "cm")
unitList <- unit.c(arg1, arg2)

unitCheck(rep(unitList, 2),
          unit.c(arg1, arg2, arg1, arg2))
unitCheck(rep(unitList, each=2),
          unit.c(rep(arg1, each=2), rep(arg2, 2)))
unitCheck(rep(unitList, c(2, 1, 2)),
          unit.c(rep(arg1, c(2, 1)), rep(arg2, 2)))
unitCheck(rep(unitList, each=2, len=4),
          rep(arg1, each=2))
unitCheck(rep(unitList, each=2, len=8),
          unit.c(rep(arg1, each=2), rep(arg2, 2), rep(arg1, each=2, len=2)))
unitCheck(rep(unitList, each=2, times=3),
          unit.c(rep(arg1, each=2), rep(arg2, 2),
                 rep(arg1, each=2), rep(arg2, 2),
                 rep(arg1, each=2), rep(arg2, 2)))

# Special test
# rep() unit.arithmetic where operands have different lengths
uaDiffLength <- unit(1:2, "npc") + unit(1:3, "npc")
uaSameLength <- unit(c(1, 2, 1), "npc") + unit(1:3, "npc")
unitCheck(rep(uaDiffLength, 2),
          rep(uaSameLength, 2))


# Bug report PR#14170 test

x <- unit.c(unit(5,"mm"),unit(3,"npc"))
rep(x, 2)
rep(x, each=2)
rep(x, c(2, 2))

x <- x - unit(1,"mm")
rep(x, 2)
rep(x, each=2)
rep(x, c(2, 2))


## grid::unit.pmax, PR#14443
x <- unit(1, "cm")
stopifnot(length(unit.pmax(x)) == 1L, length(unit.pmin(x)) == 1L)
## was 3 in R <= 2.12.0

