
grid.locator <- function(unit="native") {
  location <- c(grid.Call("L_locator"), 1)
  if (is.na(location[1]))
    invisible(NULL)
  else {
    transform <- solve(current.transform())
    location <- (location %*% transform)
    # The inverse viewport transform is from device coordinates into
    # inches relative to the current viewport
    location <- unit(location/location[3], "inches")
    list(x=convertX(location[1], unit),
         y=convertY(location[2], unit))
  }
}

