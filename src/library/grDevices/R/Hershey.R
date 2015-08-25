#  File src/library/grDevices/R/Hershey.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

Hershey <-
    list(typeface =
         c("serif", "sans serif", "script",
           "gothic english", "gothic german", "gothic italian",
           "serif symbol", "sans serif symbol"),
         fontindex =
         c("plain", "italic", "bold", "bold italic",
           "cyrillic", "oblique cyrillic", "EUC"),
## List of valid combinations : ../man/Hershey.Rd
## *checking* of allowed combinations is done in
## (via max{#}) in    FixupVFont() ../../../main/plot.c
## The basic "table" really is in  ../../../modules/vfonts/g_fontdb.c

         allowed = rbind(cbind(1L, 1L:7L), cbind(2L, 1L:4L), cbind(3L,1L:3L),
                         cbind(4L:6L, 1L), cbind(7L, 1L:4L), cbind(8L,1L:2L))
         )
