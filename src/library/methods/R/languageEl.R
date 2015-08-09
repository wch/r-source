#  File src/library/methods/R/languageEl.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

languageEl <-
  ## extract an element of a language object, consistently
  ## for different kinds of objects.
  ##
  ## The 1st., etc. elements of a function are the corresponding
  ## formal arguments, with the default expression if any as value.
  ##
  ## The first element of a call is the name or the function object being
  ## called.  The 2nd, 3rd, etc. elements are the 1st, 2nd, etc. arguments expressions.
  ## Note that the form of the extracted name is different for R and S-Plus.
  ## When the name (the first element) of a call is replaced, the languageEl replacement
  ## function coerces a character string to the internal form for each system.
  ##
  ## The 1st, 2nd, 3rd elements of an `if' expression are the test, first, and second branch.
  ##
  ## The 1st element of a `for' object is the name (symbol) being used in the loop,
  ## the second is the expression for the range of the loop, the third is the body of the loop.
  ##
  ## The first element of a `while' object is the loop test, and the second the body of
  ## the loop.
  function(object, which)
{
    data <- as.list(object)
    if(is.character(which))
        data[[which]]
    else if(typeof(object) == "language") {
        if(isGrammarSymbol(data[[1L]]))
            data[[which + 1]]
        else
            data[[which]]               ## other calls
    }
    else data[[which]]
}

"languageEl<-" <-
  ## replace an element of a language object, see "languageEl" for meaning.
  function(object, which, value)
{
    data <- as.list(object)
    n <- length(data)
    type <- typeof(object)
    if(type == "closure") {
        ev <- environment(object)
        if(is.character(which)) {
            if(is.na(match(which, names(data)))) {
                body <- data[[n]]
                data <- data[-n]
                data[[which]] <- value
                data[[n+1]] <- body
            }
            else
                data[[which]] <- value
        }
        else {
            if(which < 1 || which > n)
                stop("invalid index for function argument")
            ## we don't warn if this is used to replace the body (which == n)
            ## but maybe we should.
            data[[which]] <- value
        }
        object <- as.function(data)
        environment(object) <- ev
        object
    }
    else if(type == "language") {
        if(is.character(which))
            data[[which]] <- value
        else if(isGrammarSymbol(data[[1L]]))
            data[[which+1]] <- value
        else {
            if(identical(which, 1) && is.character(value))
                value <- as.symbol(value)
            data[[which]] <- value
        }
        as.call(data)
    }
    else {
        object[[which]] <- value
        object
    }
}


isGrammarSymbol <-
  function(symbol)
{
    if(typeof(symbol) != "symbol")
        FALSE
    else  switch(as.character(symbol),
                 ## the grammatical constructions
                 "{" =, "if" = , "for"= ,
                 "while" = , "repeat" = ,
                 "return" = , "next" = ,
                 "break" = , "<-" = , "<<-" = TRUE,
                 FALSE)
}
