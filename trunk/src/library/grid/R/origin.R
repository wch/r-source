valid.origin <- function(origin) {
  origin <- as.integer(match(origin,
                             c("bottom.left", "top.left",
                               "bottom.right", "top.right")) - 1)
  if (any(is.na(origin)))
    stop("Invalid 'origin'")
  origin
}

origin.left <- function(origin) {
  switch (origin,
          bottom.left = TRUE,
          bottom.right = FALSE,
          top.left = TRUE,
          top.right = FALSE)
}

origin.right <- function(origin) {
  switch (origin,
          bottom.left = FALSE,
          bottom.right = TRUE,
          top.left = FALSE,
          top.right = TRUE)
}

origin.bottom <- function(origin) {
  switch (origin,
          bottom.left = TRUE,
          bottom.right = TRUE,
          top.left = FALSE,
          top.right = FALSE)
}

origin.top <- function(origin) {
  switch (origin,
          bottom.left = FALSE,
          bottom.right = FALSE,
          top.left = TRUE,
          top.right = TRUE)
}

swap.origin.horizontal <- function(origin) {
  switch (origin,
          bottom.left = "bottom.right",
          bottom.right = "bottom.left",
          top.left = "top.right",
          top.right = "top.left")
}

swap.origin.vertical <- function(origin) {
  switch (origin,
          bottom.left = "top.left",
          bottom.right = "top.right",
          top.left = "bottom.left",
          top.right = "bottom.right")
}
