class <- dataClass

.BasicVectorClasses <- c("logical", "numeric", "character",
                         "complex", "integer", "single",
                         "expression", "list")
.BasicClasses <- c(.BasicVectorClasses,
                   "function", "environment", "named"," array",
                   "matrix", "name", "call", "NULL" )

"class<-" <-
  function(x, value)
  objWithClass(x, value)

