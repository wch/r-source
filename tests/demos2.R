#### Run all demos for which we do not wish to diff the output
.ptime <- proc.time()
.Random.seed <- c(0,rep(7654, 3))

demos <- c("Hershey", "Japanese", "nlm", "plotmath")

for(nam in  demos) demo(nam, character.only = TRUE)

cat("Time elapsed: ", proc.time() - .ptime, "\n")
