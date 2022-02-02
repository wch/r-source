#  File src/library/grDevices/R/gevents.R
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

setGraphicsEventHandlers <- function(which=dev.cur(),
				     ...)
    setGraphicsEventEnv(which, as.environment(list(...)))

setGraphicsEventEnv <- function(which=dev.cur(), env) {
    which <- as.integer(which)
    stopifnot(length(which) == 1)
    result <- getGraphicsEventEnv(which)
    env$which <- which
    .External2(C_setGraphicsEventEnv, which, env)
    invisible(result)
}

getGraphicsEventEnv <- function(which=dev.cur()) {
    which <- as.integer(which)
    stopifnot(length(which) == 1)
    .External2(C_getGraphicsEventEnv, which)
}

getGraphicsEvent <- function(prompt = "Waiting for input",
                 onMouseDown = NULL, onMouseMove = NULL, onMouseUp = NULL,
                 onKeybd = NULL, onIdle = NULL, consolePrompt = prompt) {
    if (!interactive()) return(NULL)
    if (!missing(prompt) || !missing(onMouseDown) || !missing(onMouseMove)
     || !missing(onMouseUp) || !missing(onKeybd) || !missing(onIdle)) {
        setGraphicsEventHandlers(prompt=prompt, onMouseDown=onMouseDown,
          onMouseMove=onMouseMove, onMouseUp=onMouseUp, onKeybd=onKeybd,
          onIdle=onIdle)
    }
    .External2(C_getGraphicsEvent, consolePrompt)
}
