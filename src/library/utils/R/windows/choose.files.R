#  File src/library/utils/R/windows/choose.files.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

Filters <-
structure(c("R or S files (*.R,*.q,*.ssc,*.S)",
            "Enhanced metafiles (*.emf)",
            "Postscript files (*.ps)",
            "PDF files (*.pdf)",
            "Png files (*.png)",
            "Windows bitmap files (*.bmp)",
            "Jpeg files (*.jpeg,*.jpg)",
            "Text files (*.txt)",
            "R images (*.RData,*.rda)",
            "Zip files (*.zip)",
            "Package tarballs (*.tar.gz)",
            "All files (*.*)",

            "*.R;*.q;*.ssc;*.S", "*.emf", "*.ps", "*.pdf", "*.png", "*.bmp",
            "*.jpeg;*.jpg", "*.txt", "*.RData;*.rda", "*.zip", "*.tar.gz", "*.*"),
       .Dim = c(12L, 2L),
       .Dimnames = list(c("R", "emf", "ps","pdf", "png",
                          "bmp", "jpeg", "txt", "RData", "zip", "tarball", "All"),
                        NULL))

choose.files <- function(default = '', caption = 'Select files', multi = TRUE,
                         filters = Filters, index = nrow(Filters) )
    .Call(C_chooseFiles, default, caption, multi, filters, index)

choose.dir <- function(default = '', caption = 'Select folder')
    .Call(C_chooseDir, default, caption)
