menu <- function(x, graphics = FALSE, title = "")
{
  xlen <- length(x)
  cat(title, "\n")
  for (i in 1:xlen) 
    cat(i, ":", x[i]," \n", sep = "")
  done <- 0
  repeat {
    cat("Selection: ")
    ind <- .Internal(menu(as.character(x)))
    if(ind <= xlen)
      return(ind)
    cat("Enter an item from the menu, or 0 to exit\n")
  }
}
