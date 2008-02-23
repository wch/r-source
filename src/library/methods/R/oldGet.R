#  File src/library/methods/R/oldGet.R
#  Part of the R package, http://www.R-project.org
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
## Finally (2008-02) made deprecated formally for 2.7.0
"getAccess" <-
function (ClassDef) {
    .Deprecated()
    ClassDef@access
}

"getClassName" <-
function (ClassDef) {
    .Deprecated()
    ClassDef@className
}

"getClassPackage" <-
function (ClassDef) {
    .Deprecated()
    ClassDef@package
}

"getExtends" <-
function (ClassDef) {
    .Deprecated()
    ClassDef@contains
}

"getProperties" <-
function (ClassDef)  {
    .Deprecated()
    ClassDef@slots
}

"getPrototype" <-
function (ClassDef) {
    .Deprecated()
    ClassDef@prototype
}

"getSubclasses" <-
function (ClassDef) {
    .Deprecated()
    ClassDef@subclasses
}

## Hmm: this is explicitly mentioned as needed,
## .... in ../man/validObject.Rd
"getValidity" <-
function (ClassDef) {
    ## "needed" according to validObject.Rd, hence *not* .Deprecated()
    ClassDef@validity
}

"getVirtual" <-
function (ClassDef) {
    .Deprecated()
    ClassDef@virtual
}
