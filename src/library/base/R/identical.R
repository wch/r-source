identical <-
  ## test whether the two objects are exactly identical.
  function(x, y)
  .Call("do_identical", x, y)
