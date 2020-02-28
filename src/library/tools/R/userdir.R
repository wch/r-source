#  File src/library/tools/R/userdir.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2020 The R Core Team
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

R_user_dir <-
function(package, which = c("data", "config", "cache"))
{
    stopifnot(is.character(package), length(package) == 1L)

    which <- match.arg(which)

    home <- normalizePath("~")

    path <-
        switch(which,
               data = {
                   if(nzchar(p <- Sys.getenv("R_USER_DATA_DIR")))
                       p
                   else if(nzchar(p <- Sys.getenv("XDG_DATA_HOME")))
                       p
                   else if(.Platform$OS.type == "windows")
                       file.path(Sys.getenv("APPDATA"), "R", "data")
                   else if(Sys.info()["sysname"] == "Darwin")
                       file.path(home, "Library", "Application Support",
                                 "org.R-project.R") 
                   else
                       file.path(home, ".local", "share")
               },
               config = {
                   if(nzchar(p <- Sys.getenv("R_USER_CONFIG_DIR")))
                       p
                   else if(nzchar(p <- Sys.getenv("XDG_CONFIG_HOME")))
                       p
                   else if(.Platform$OS.type == "windows")
                       file.path(Sys.getenv("APPDATA"), "R", "config")
                   else if(Sys.info()["sysname"] == "Darwin")
                       file.path(home, "Library", "Preferences",
                                 "org.R-project.R")
                   else
                       file.path(home, ".config")
               },
               cache = {
                   if(nzchar(p <- Sys.getenv("R_USER_CACHE_DIR")))
                       p
                   else if(nzchar(p <- Sys.getenv("XDG_CACHE_HOME")))
                       p
                   else if(.Platform$OS.type == "windows")
                       file.path(Sys.getenv("LOCALAPPDATA"), "R", "cache")
                   else if(Sys.info()["sysname"] == "Darwin")
                       file.path(home, "Library", "Caches",
                                 "org.R-project.R")
                   else
                       file.path(home, ".cache")
               })
        
    file.path(path, "R", package)
}
