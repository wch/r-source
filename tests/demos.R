#### Run all demos that do not depend on tcl and other specials :
.ptime <- proc.time()
.Random.seed <- c(0,rep(7654, 3))

demos <- c(demo())
demos <- demos[substr(demos,1,2) != "tk"]
# Drop these, for strict testing :
demos <- demos[is.na(match(demos, c("nlm", "lm.glm")))]
for(nam in demos)
    demo(nam, character.only = TRUE)

cat("Time elapsed: ", proc.time() - .ptime,"\n")
