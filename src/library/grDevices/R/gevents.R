#  File src/library/grDevices/R/gevents.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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
#  http://www.r-project.org/Licenses/

setGraphicsEventHandlers <- function(which=dev.cur(),
				     ...)
    setGraphicsEventEnv(which, as.environment(list(...)))

setGraphicsEventEnv <- function(which=dev.cur(), env) {
    result <- getGraphicsEventEnv(which)
    env$which <- which
    .External2(C_setGraphicsEventEnv, which, env)
    invisible(result)
}

getGraphicsEventEnv <- function(which=dev.cur())
    .External2(C_getGraphicsEventEnv, which)

getGraphicsEvent <- function(prompt = "Waiting for input",
                 onMouseDown = NULL, onMouseMove = NULL, onMouseUp = NULL,
                 onKeybd = NULL, consolePrompt = prompt) {
    if (!interactive()) return(NULL)
    if (!missing(prompt) || !missing(onMouseDown) || !missing(onMouseMove)
     || !missing(onMouseUp) || !missing(onKeybd)) {
        setGraphicsEventHandlers(prompt=prompt, onMouseDown=onMouseDown,
          onMouseMove=onMouseMove, onMouseUp=onMouseUp, onKeybd=onKeybd)
    }
    .External2(C_getGraphicsEvent, consolePrompt)
}
