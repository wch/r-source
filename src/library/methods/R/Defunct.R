#  File src/library/methods/R/Defunct.R
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

## old access functions that do nothing but get a slot
## likely to be made defunct & disappear.

## The above comment is there since May 2003, R 1.8.0
## similarly in the help file ../man/oldGet.Rd
## Finally (2008-02) made deprecated formally for 2.7.0, defunct in 2.8.0

getAccess <- function (ClassDef) .Defunct()

getClassName <- function (ClassDef) .Defunct()

getClassPackage <- function (ClassDef) .Defunct()

getExtends <- function (ClassDef) .Defunct()

getProperties <- function (ClassDef)  .Defunct()

getPrototype <- function (ClassDef) .Defunct()

getSubclasses <- function (ClassDef) .Defunct()

getVirtual <- function (ClassDef) .Defunct()

## Deprecated in 2.7.0, defunct in 2.8.0
getAllMethods <- function(f, fdef, where = topenv(parent.frame())) .Defunct()

mlistMetaName <- function(name = "", package = "") .Defunct()

removeMethodsObject <- function(f, where = topenv(parent.frame())) .Defunct()

seemsS4Object <- function(object) .Defunct("isS4")

## Deprecated in 2.8.0, defunct in 2.9.0

allGenerics <- function(...)
    ## this is used nowhere, and we already have too many functions
    .Defunct("getGenerics")

