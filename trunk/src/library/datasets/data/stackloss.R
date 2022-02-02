stackloss <- data.frame(
 "Air Flow" = c(80,80,75,62,62,62,62,62,58,58,58,58,58,58,50,50,50,50,50,56,70),
"Water Temp"= c(27,27,25,24,22,23,24,24,23,18,18,17,18,19,18,18,19,19,20,20,20),
"Acid Conc."= c(89,88,90,87,87,87,93,93,87,80,89,88,82,93,89,86,72,79,80,82,91),
 stack.loss = c(42,37,37,28,18,18,19,20,15,14,14,13,11,12, 8, 7, 8, 8, 9,15,15)
)
##- These are for S compatibility:
stack.loss <- stackloss[["stack.loss"]]
stack.x    <- as.matrix(stackloss[-4])
