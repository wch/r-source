class <- dataClass

## first step of class<-  is temporary, pending fix of base package's primitive,
## which allows non-empty class attribute to stay if value is a basic class name

"class<-" <-
  function(x, value) {
      attr(x, "class") <- NULL
      objWithClass(x, value)
  }
