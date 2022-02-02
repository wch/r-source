UCBAdmissions <-
array(c(512, 313,
         89,  19,
        353, 207,
         17,   8,
        120, 205,
        202, 391,
        138, 279,
        131, 244,
         53, 138,
         94, 299,
         22, 351,
         24, 317),
      dim = c(2, 2, 6),
      dimnames =
      list(Admit = c("Admitted", "Rejected"),
           Gender = c("Male", "Female"),
           Dept = c("A", "B", "C", "D", "E", "F")))
class(UCBAdmissions) <- "table"
