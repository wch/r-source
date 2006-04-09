Titanic <-
array(c(  0,   0,  35,   0,
          0,   0,  17,   0,
        118, 154, 387, 670,
          4,  13,  89,   3,
          5,  11,  13,   0,
          1,  13,  14,   0,
         57,  14,  75, 192,
        140,  80,  76,  20),
      dim = c(4, 2, 2, 2),
      dimnames =
      list(Class = c("1st", "2nd", "3rd", "Crew"),
           Sex = c("Male", "Female"),
           Age = c("Child", "Adult"),
           Survived = c("No", "Yes")))
class(Titanic) <- "table"
