HairEyeColor <-
array(c(32, 53, 10, 3, 11, 50, 10, 30, 10, 25, 7, 5, 3, 15, 7, 8,
        36, 66, 16, 4,  9, 34,  7, 64,  5, 29, 7, 5, 2, 14, 7, 8,
        32, 53, 10, 3, 11, 50, 10, 30, 10, 25, 7, 5, 3, 15, 7, 8,
        36, 66, 16, 4,  9, 34,  7, 64,  5, 29, 7, 5, 2, 14, 7, 8),
      dim = c(4, 4, 2),
      dimnames =
      list(Hair = c("Black", "Brown", "Red", "Blond"),
           Eye = c("Brown", "Blue", "Hazel", "Green"),
           Sex = c("Male", "Female")))
           
class(HairEyeColor) <- "table"
