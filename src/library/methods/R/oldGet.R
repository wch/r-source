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
"getAccess" <-
function (ClassDef) 
ClassDef@access

"getClassName" <-
function (ClassDef) 
ClassDef@className

"getClassPackage" <-
function (ClassDef) 
ClassDef@package

"getExtends" <-
function (ClassDef) 
ClassDef@contains

"getProperties" <-
function (ClassDef) 
ClassDef@slots

"getPrototype" <-
function (ClassDef) 
ClassDef@prototype

"getSubclasses" <-
function (ClassDef) 
ClassDef@subclasses

"getValidity" <-
function (ClassDef) 
ClassDef@validity

"getVirtual" <-
function (ClassDef) 
ClassDef@virtual
