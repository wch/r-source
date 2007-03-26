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
