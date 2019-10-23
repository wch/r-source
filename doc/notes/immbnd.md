---
title: "Immediate Binding Values"
author: Luke Tierney
output: html_document
---

## Background

For scalar numerical code it can help to allow variable bindings to
hold scalar integer, logical, and double values as immediate values
rather than as allocated scalar vectors, or _boxed_ values. This
eliminates the overhead of checking whether they might be shared or
have attributes. It also makes inlining scalar computations for basic
arithmetic operations and element access in the byte code engine more
effective. The combined benefit can be as high as 20% for some
examples, including the convolution example from the extensions
manual. Having immediate bindings also allows some brittle
optimizations for updating scalar variable bindings and loop indices
to be removed.

This note reflects changed committed to R_devel in r77327.)

## Interface

Binding cells have a marker that is returned by `BNDCELL_TAG`. The
marker, or tag, is zero for standard bindings, and one of `REALSXP`,
`INTSXP`, or `LGLSXP` for immediate bindings.

`BINDING_VALUE`, used only in `eval.c` and `envir.c`, always returns
an allocated object as the value of a binding. For immediate bindings
it first converts to a standard binding by allocating and installing a
scalar vector of the appropriate type. This allows most code to be
unaware of the existence of typed bindings.  The allocation is done by
`R_expand_binding_value`.

Code that wants to take advantage of typed bindings can read and set
their values with

- `INTSXP`: `BNDCELL_IVAL(cell)`, `SET_BNDCELL_IVAL(cell, val)`
- `LGLSXP`: `BNDCELL_LVAL(cell)`, `SET_BNDCELL_LVAL(cell, val)`
- `REALSXP`:`BNDCELL_DVAL(cell)`, `SET_BNDCELL_DVAL(cell, val)`

These do not check or set the type tag. To create and initialize a new
immediate binding in a cell use
	  
- `INTSXP`: `NEW_BNDCELL_IVAL(cell, val)`
- `LGLSXP`: `NEW_BNDCELL_LVAL(cell, val)`
- `REALSXP`:`NEW_BNDCELL_DVAL(cell, val)`

The generic `CAR` accessor has been modified to signal an error if it
encounters a cell with an immediate `CAR` value. This ensures
immediate values are only used in the context of bindings. This makes
it easier to avoid inadvertent boxing and may help with a transition
to a different environment and binding representation.

The setters, such as `SETCAR`, clear an immediate binding marker
without signaling an error.


## Notes

  - For now, the `sxpinfo.extra` field is used to hold the binding
    tag.

  - Two implementations are provided for representing the immediate
    values. One replaces the `SEXP` `CAR` field by a union; he other
    allocates a boxed value. The union representation is conceptually
    more natural and a little more efficient. But it would require a
    change in memory layout on 32-bit platforms since the union
    requires 8 bytes for the `double` value while a pointer only
    requires 4 bytes. On 64-bit hardware the union approach should not
    change the memory layout.

    For now, the union approach is used on 64-bit platforms and the
    boxed approach on 32-bit ones. It would be best to use the union
    approach unconditionally, but this would require changing the
    binary version and rebuilding all packages with compiled code.
    This should probably be done before release.

  - The approach taken for now is to just allow immediate values in
    the `CAR` of binding cells. An alternative would be to allow
    immediate values in all `CONS` cells, or even more widely, such as
    in vector element. Allowing immediate values in all `CONS` cells
    would have been a little simpler. But it would have make it harder
    to detect unintended boxing, and might also have made it harder to
    transition to an alternate environment or binding representation
    should we wish to do that.

    If immediate values were to be supported more widely it would
    probably be necessary to suspend the GC when boxing values in
    `R_expand_binding_value`.
  
  - Serialization handles environment frames with standard pairlist
    code, so the code not checks for an immediate binding and boxes
    the value if necessary. An alternative would be to update the
    serialization format to support immediate bindings. But given how
    challenging it is to change the format it seemed best just to box.
  
  - Only unlocked standard environment bindings that can be cached can
    be turned into immediate bindings. Symbol bindings for the base
    environment are not cached, and bindings for user data bases are
    locked when returned by `findVarLoc` or findVarLocInFrame, so
    neither of these can become immediate bindings.

  - `BINDING_VALUE` is defined slightly differently in `eval.c` and
    `envir.c`. It would be good to unify these eventually.

<!--
Local Variables: 
mode: poly-markdown+R
mode: flyspell
End:
-->
