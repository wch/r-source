
grid.locator <- function(unit="native") {
  location <- c(grid.Call("L_locator"), 1)
  transform <- solve(viewport.transform(current.viewport()))
  location <- (location %*% transform)
  # The inverse viewport transform is from device coordinates into
  # inches relative to the current viewport
  location <- unit(location/location[3], "inches")
  list(x=grid.convertX(location[1], unit),
       y=grid.convertY(location[2], unit))
}

