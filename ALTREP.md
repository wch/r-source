---
title: "ALTREP: Alternative Representations for R Objects"
author:
- Luke Tierney
- Gabe Becker
- Tomas Kalibera
output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: false
---

<!-- render with
Rscript -e 'rmarkdown::render(input="ALTREP.md", output_file="ALTREP.html")'
-->

# Introduction

The [`ALTREP`](https://svn.r-project.org/R/branches/ALTREP) branch of
the R svn repository provides an experimental framework for developing
alternate representations of basic `R` objects. Some examples of intended
uses would be to

- allow vector data to be in a memory-mapped file or distributed;
- allow compact representation of arithmetic sequences;
- support adding meta-data to objects;
- support alternative representations of environments.

The intent is that to the greatest extent possible existing `C` source
code for working with `R` objects should continue to work, though some
modifications of source code might help in taking advantage full 
advantage of the `ALTREP` framework.


# Basic Design

The basic design organizes alternate representation objects in a set
of abstract classes corresponding to the functionality of the
different basic `R` types. The most specific abstract classes
correspond to specific `R` data types, such as `INTSXP`. Concrete
classes specialize one of these specific abstract classes.  The
hierarchy is fixed as corresponding to the `R` internals; there is no
provision for adding new abstract classes or for a hierarchy of
concrete classes. This might be worth exploring in the future.

## Classes and Methods

### General Objects

The `ALTREP` class is the most general abstract class. It specifies
the methods `Duplicate`, `Coerce`, `Inspect`, and `Length`. The
`Length` method is included at this level since the `R` function
`length` is defined for all objects.

The `Duplicate` method accepts a second argument specifying whether
the copy should be deep or shallow. A duplicate method can return a
`C` `NULL` value to indicate that the object should be duplicated by
the default method for its `SEXP` type.

Similarly, a `Coerce` method can return a `C` `NULL` to indicate that
the object should be coerced using the standard protocol for its
`SEXP` type.

Adding an `Inspect` method is not essential but is useful for
debugging.


### General Vectors

The `ALTVEC` class specifies methods common to all concrete vector
types. These are `Dataptr`, `Dataptr_or_null`, and `Extract_subset`.
These method interfaces are designed to allow code using them to avoid
assuming that the full data of a vector is available in memory and is
writable.

`Extract_subset` allows efficient implementations of subsetting
operations. These methods can return a `C` `NULL` to defer to the
standard protocol.

`Dataptr` takes a second argument indicating
whether the pointer returned should allow modification of the data
pointed to.  For compatibility the `DATAPTR` function and the `REAL`,
`INTEGER`, etc, macros defined in terms of `DATAPTR` will request a
writable pointer from an `ALTVEC` object. `Dataptr_or_null` returns a
`const` pointer.

`Dataptr` methods might need to allocate memory; they are currently
invoked with GC suspended since `DATAPTR` previously would not
allocate and code will need additional `PROTECT` calls if GC is to be
allowed. `Dataptr_or_null` methods should not allocate. They should
return a valid pointer if this can be done cheaply and without
allocation; otherwise the should return a `C` `NULL`. This allows code
to test whether a pointer is available, use it if it is, and use
another approach, such as calling the `Elt` or `Get_region` methods if
they are not.


### Specific Vector Classes

Vector classes for specific element types, such as `ALTINTEGER`,
`ALTREAL`, or `ALTSTRING` will implement `Elt` methods. For atomic
types they will also implement `Get_region` methods for copying a
contiguous region of elements to a buffer. This is patterned after the
JNI specification. For non-atomic types they will implement `Set_elt`
methods to preserve the write barrier needed e.g. for generational
garbage collection. `DATAPTR` should almost never be used on non-atomic
vectors because of the risks to the write barrier.

It may make sense to also support `Set_elt` and `Set_region` methods
for atomic vectors. It may also be useful to have methods to get and
set non-contiguous subsets of values of atomic vector objects.


## Changes to Existing Functions

A number of functions have been modified to avoid using `DATAPTR` and
thus possibly allocating or duplicating a large object. The summary
functions `mean`, `min`, `max`, `sum`, and `prod` have been updated for
`INTSXP` and `REALSXP` vectors, but not yet for `CPLXSXP` or `LGLSXP`
vectors. Basic subsetting operations for `INTSXP` and `REALSXP` have
also been modified in this way, so calls to `head` and `sample`, among
others, do not force allocation. Many more functions could be modified
in this way.


## Serialization

A class that wants to handle serializing and unserializing its objects
should define `Serialized_state` and `Unserialize` methods.

The `Serialized_state` method should return an `SEXP` which is
serialized in the usual way, along with attributes, the name of the
`ALTREP` class, and the package the class is registered with.
Unserializing such an object will locate the corresponding class
object and call the `Unserialize` class method with this class object
and the serialized state as arguments.  The method should
return a new object without attributes. The serialized attributes are
then unserialized and attached.

The `Serialize` method can also return a `C` `NULL` pointer, in which
case the object is serialized in the standard way for its `SEXP` type.


## Package Support

The class creation and registration functions,
e.g. `R_make_altreal_class`, create a class object in an R session
corresponding to a specified class name and package name. Within the R
session the package C code can create objects using this class object.
The initialization function for the package's shared library should
create the classes the package supports and install their methods.

Serialization will record the class name and package name;
unserializing will load the package.

Ideally unloading a package shared library should cause methods in the
associated classes to be replaced by stubs that signal errors when
called. This would require a package to call a cleanup routine in its
library's unload function. This cleanup mechanism has not been
implemented yet, so for now packages using this mechanism should not
unload their DLLs.

As an illustration of this mechanism, the
[`simplemmap` package](https://github.com/ltierney/Rpkg-simplemmap)
implements the [memory-mapped vectors](#memory-mapped-vectors) example
described below as a package.


## Some Implementation Details

`ALTREP` objects are allocated as `CONS` objects and identified by
having the `altrep` bit set in the header. The GC checks for the bit
and scans the fields as for a `CONS` cell. The `TAG` field holds class
information; the `CAR` and `CDR` hold `SEXP` values that are used for
instance data; they are considered invisible outside of methods for
the specific class.

To allow efficient scalar identification there is also a scalar bit,
and the `IS_SCALAR macro is defined as

```C
#define IS_SCALAR(x, t) (((x)->sxpinfo.type == (t)) && (x)->sxpinfo.scalar)
```

The `altrep` bit should never be set when the `scalar` bit is set. It
would be possible to increase the size of the type field by one bit
and use the highest order bit. That would allow `IS_SCALAR` to be
defined as a single comparison. This doesn't seem to make a measurable
speed difference and complicates the code, but it might be worth
revisiting.

With the `ALTREP` changes operations like `DATAPTR`, `STRING_ELT`, and
`SET_STRING_ELT` now might cause allocation.  Eventually code should
be rewritten to allow for this. For now, GC is suspended in these
allocations.  Operations `REAL_ELT`, `REAL_GET_RANGE` and the like may
also allocate, but these are new operations and code written or
modified to use them should take this into account.

There is no specific support for versioning. If a class has to be
updated in an incompatible way it would be best to consider it a new
class with a new name. An unserialize method for the old class will
still need to be supported if serialized objects from the old class
are to be loaded.

The data structures associated with the base classes are not visible
outside core R, so if a new method is added to one of these classes
packages will get the default behavior until they add their own; there
is no risk of getting the wrong method or a null pointer exception
because of trying to access an incorrect or non-existent method as a
result of such a change.

With `STRSXP` or (not yet supported) `VECSXP` objects it is important
that the `DATAPTR` not be used in a way that can violate the write
barrier by creating untracked old-to-new references.

The capitalization of method names was chosen to avoid conflicts with
remapping for some common names, like `length`.


# Sample Implementations

## Compact Integer Vectors

Vectors created by `n1:n2` with `n1` and `n2` both integers, as well
as vectors created by `seq_along()` and `seq_len()`, can be
represented compactly in terms of their start and end values.  (The
type of the result will be `INTSXP` or `REALSXP`, depending on the
magnitudes of the end points.) The byte code interpreter in R 3.3.0
uses a representation of this form, but only on the stack; once such a
value is stored in a variable it is expanded.  The `ALTVEC` mechanism
allows this compact representation to be used more widely, and special
handling by the byte code interpreter has been dropped.

One implication is that a loop over a large range of integers that
stops early no longer needs to allocate and fill in the full vector:
In `R-devel` (NOTE: `R-devel` refers to a development version of R prior
to the release of R 3.4. R 3.5.0 already includes the optimizations
described here):

```R
> system.time(for (i in 1:1e9) break)
   user  system elapsed 
  0.258   1.141   1.400 
```

In the `ALTREP` branch on the other hand,
```R
> system.time(for (i in 1:1e9) break)
   user  system elapsed 
      0       0       0 
```

As another example, in R-devel on an Ubuntu laptop:
```R
> x <- 1:1e10
Error: cannot allocate vector of size 74.5 Gb
```
In the `ALTREP` branch:
```R
> x <- 1:1e10
```

The `.Internal(inspect())` function shows that a compact
representation has been used:

```R
> .Internal(inspect(x))
@1aab4c0 14 REALSXP g0c0 [NAM(2)]  1 : 10000000000 (compact)
```

The `mean` function has been modified to use the `Get_region` method
when the fill data is not available in memory, so after a call to
`mean` the value of `x` retains its compact representation:

```R
> system.time(print(mean(x)))
[1] 5e+09
   user  system elapsed 
 38.520   0.008  38.531 
> .Internal(inspect(x))
@1aab4c0 14 REALSXP g1c0 [MARK,NAM(2)]  1 : 10000000000 (compact)
```

Operations that want to use the data pointer will cause an allocation
attempt that might fail.  And on macOS, because of the memory
over-commit issue, this would probably get R killed.

The compact sequences are marked as not mutable; an assignment will
duplicate them as a full vector and modify the duplicate.  Serializing
a compact sequence serializes the compact representation, even if the
vector has been expanded.  Compact sequences know they are sorted, so
the `Is_sorted` property returns `TRUE`. A `No_NA` property is also
defined to return `TRUE`. These are two examples of attaching [meta
data](#meta-data) to an `ALTREP` object.

An alternative approach would be to not mark these vectors as
immutable but to treat them as standard vectors as soon as the data
pointer is requested and the vector is expanded.  If a writable data
pointer is requested then the expanded data could be modified by an
assignment.  The `Is_sorted` and `No_NA` properties could no longer be
assumed to be true, and the object would have to be serialized as a
full vector. For the integer case this is supported by uncommenting
the line

```C
//#define COMPACT_INTSEQ_MUTABLE
```


## Deferred String Conversions

Conversion of integers or reals to strings is an expensive operation.
One place where this happens and the result is rarely needed in its
entirety, if at all, is default row labels on design matrices. These
are effectively created as

```R
as.character(1 : nrow)
```

In the `ALTREP` branch the internal `coerce()` function has been
modified to return a deferred string coercion when asked to coerce an
`INTSXP` or a `REALSXP` to `STRSXP`.  Initially the resulting object
contains only a reference to the original numeric object, along with
the `scipen` option setting in effect at the time the deferred
coercion object is created. If elements are requested individually
these are converted on request and saved . If the data pointer is
requested, then the full vector is converted and the reference to the
original data is dropped.

Some simple examples:
```R
> x <- 1:1000
> y <- as.character(x)
> .Internal(inspect(y))
@2802830 16 STRSXP g0c0 [NAM(1)]   <deferred string conversion>
  @25114c0 13 INTSXP g0c0 [NAM(2)]  1 : 1000 (compact)
> head(x)
[1] 1 2 3 4 5 6
> y[1] <- "a"
> .Internal(inspect(y))
@2802830 16 STRSXP g0c0 [NAM(1)]   <expanded string conversion>
  @331a690 16 STRSXP g0c7 [] (len=1000, tl=0)
    @1d2d388 09 CHARSXP g0c1 [MARK,gp=0x61] [ASCII] [cached] "a"
    @2696ac8 09 CHARSXP g0c1 [MARK,gp=0x60] [ASCII] [cached] "2"
    @1d16038 09 CHARSXP g0c1 [MARK,gp=0x60] [ASCII] [cached] "3"
    @26a36e8 09 CHARSXP g0c1 [MARK,gp=0x60] [ASCII] [cached] "4"
    @2a57148 09 CHARSXP g0c1 [gp=0x60] [ASCII] [cached] "5"
    ...
```

A major benefit is a significant speedup in `lm()` for fitting a model
with many cases. In `R-devel`, in a fresh R session:
```R
> n <- 10000000
> x <- rnorm(n)
> y <- rnorm(n)
> system.time(lm(y ~ x))
   user  system elapsed 
 17.927   0.982  18.911 
> system.time(lm(y ~ x))
   user  system elapsed 
  9.225   0.703   9.929 
```

In the `ALTREP` branch:
```R
> n <- 10000000
> x <- rnorm(n)
> y <- rnorm(n)
> system.time(lm(y ~ x))
   user  system elapsed 
  1.989   0.601   2.590 
> system.time(lm(y ~ x))
   user  system elapsed 
  1.886   0.610   2.496 
```

The speedup is due entirely to not creating the row labels for the
design matrix.

Avoiding the cost of allocating default row labels in `glm()` requires
an additional modification.  The `glm()` computation takes subsets of
rows with positive weights, and with the changes described so far
these subsetting operations would trigger creating the row labels. To avoid
this, the `ALTREP` branch allows vector classes to provide their own
`ExtractSubset` methods. For deferred strings, this method creates a
new deferred string applied to the subsetted original data; the
conversion only takes place if the labels are used, which they are not
in `glm()`. In R-devel, in a fresh R session:
```R
> n <- 10000000
> x <- rnorm(n)
> y <- rnorm(n)
> system.time(glm(y ~ x))
   user  system elapsed 
 29.616   4.524  34.153 
> system.time(glm(y ~ x))
   user  system elapsed 
 17.464   2.536  20.012 
```

In the `ALTREP` branch:
```R
> n <- 10000000
> x <- rnorm(n)
> y <- rnorm(n)
> system.time(glm(y ~ x))
   user  system elapsed 
  7.796   3.340  11.139 
> system.time(glm(y ~ x))
   user  system elapsed 
  7.544   2.924  10.471 
```

As another example, in a recent `R-help` thread with subject `"Faster
Subsetting"` Martin Morgan showed that in `R-devel` reordering row names
on a data frame can affect performance of a `split` operation:

```R
> tmp <- data.frame(id = rep(1:20000, each = 10), foo = rnorm(200000))
> idList <- unique(tmp$id)
> system.time(split(tmp, tmp$id))
   user  system elapsed
  5.316   0.112   5.429 
> row.names(tmp) = rev(seq_len(nrow(tmp)))
> system.time(split(tmp, tmp$id))
   user  system elapsed
  1.012   0.008   1.020 
```

In the `ALTREP` branch:
```R
> tmp <- data.frame(id = rep(1:20000, each = 10), foo = rnorm(200000))
> idList <- unique(tmp$id)
> system.time(split(tmp, tmp$id))
   user  system elapsed 
  1.024   0.016   1.039 
> row.names(tmp) = rev(seq_len(nrow(tmp)))
> system.time(split(tmp, tmp$id))
   user  system elapsed 
  0.936   0.004   0.940 
```

Again the difference is due primarily to deferring the string coercions.

Serialization serializes the deferred string data if the object has
not been fully converted. This is safe since assignments currently
fully convert the object.  It would be possible to allow assignment to
operate without full expansion; serialization would then need to be
modified to also serialize the modified partially expanded data.

The argument to the deferred coercion function is marked as not
mutable to make sure it isn't changed after capture. This would not be
needed under reference counting.

The `Is_sorted` property is deferred to the original argument if the
deferred string has not been fully expanded. If it has been fully
expanded, then the data might have been modified, so `Is_sorted`
returns zero.


## Memory-Mapped Vectors

The `ALTREP` branch includes a simple implementation of integer and
double vectors with their data in memory mapped files.  The branch
includes two `.Internal` functions which are best accessed through
these simple wrappers:

```R
mmap <- function(filename, type = c("double", "integer", "int"),
                 ptrOK = TRUE, wrtOK = FALSE, serOK = TRUE) {
    type = match.arg(type)
    .Internal(mmap_file(filename, type, ptrOK, wrtOK, serOK))
}

munmap <- function(x)
    .Internal(munmap_file(x))
```

To illustrate the package support framework the memory mapped file
interface is also available in the
[`simplemmap` package](https://github.com/ltierney/Rpkg-simplemmap).

To set up an example, create a binary file with 1000 uniform random numbers:
```R
set.seed(1234)
x <- runif(1000)
writeBin(x, "foo.dat")
```

The `mmap` function requires one argument, a file name. The data type
can be specified by a second argument, which defaults to
`"double"`. For now the supported element types are `"integer"` and
`"double"`. Some examples:

```R
> y <- mmap("foo.dat")
> str(y)
 num [1:1000] 0.114 0.622 0.609 0.623 0.861 ...
> head(y)
[1] 0.1137034 0.6222994 0.6092747 0.6233794 0.8609154 0.6403106
> sample(y, 4)
[1] 0.5058416 0.6042504 0.4792225 0.1274334
> mean(y)
[1] 0.5072735
> head(y + 1)
[1] 1.113703 1.622299 1.609275 1.623379 1.860915 1.640311
```

Computing `y+1` produces a standard R vector the same length as
`y`. For a large memory-mapped file this may not be desirable.  The
optional argument `ptrOK` can be given as `FALSE` as a means of
preventing operations that directly access the pointer. Operations
that have been modified to avoid accessing a pointer to the full data
will succeed, but ones that need full data access will fail:

```R
> z <- mmap("foo.dat", ptrOK = FALSE)
> sample(z, 4)
[1] 0.2016572 0.7615122 0.7133016 0.2011326
> mean(z)
[1] 0.5072735
> z + 1
Error: cannot access data pointer for this mmaped vector
```

This is not a particularly good approach to the problem of
[unintended allocation](#unintended-allocations), but there are other
situations in which a pointer to the full data might not be available.

When the `wrtOK` argument is `FALSE`, the default, the file is opened
read-only and memory mapped region is marked as read-only. This is
enforced at the `R` level by marking the object returned by `mmap` as
not mutable. Thus an assignment will duplicate first, (which might
attempt to allocate a very large vector).  C code that attempts to
write in this memory will cause a segmentation fault.

If `wrtOK` is `TRUE` then the object returned by `mmap` is not marked
as immutable (though it might be later if, for example, it is assigned
to a second variable).  If the memory mapped object is not shared,
then an assignment will not duplicate the object and the assignment
will modify the file:

```R
> z <- mmap("foo.dat", wrtOK = TRUE)
> readBin("foo.dat", "double", 4)
[1] 0.1137034 0.6222994 0.6092747 0.6233794
> z[1] <- 0
> readBin("foo.dat", "double", 4)
[1] 0.0000000 0.6222994 0.6092747 0.6233794
```

If the `serOK` argument is `TRUE`, the default, then serializing a
memory mapped object will serialize the information passed to the
`mmap` function, including the file name as it was passed. When
unserializing, an attempt will be made to memory map a file by that
name, which might not be available. If `unserialize` cannot map the
file then it returns a zero-length vector of the same type. If `serOK`
is `FALSE` then the object will be serialized as a standard vector,
which might result in a very large serialization.


## Wrapper Objects

Vector wrapper objects provide a place to record meta-information such
as whether a vector is sorted or has no NAs, and can also hold
attributes and allow attributes to be changed without requiring
duplication of the vector data payload.

Wrappers would in principle also be useful for environments to allow
attributes on environments to behave in a way more consistent with
`R`'s conceptual pass by value semantics. But this would require more
extensive internal code changes because of the heavy use of pointer
identity in environment computations.

Wrappers for integer and real vectors are produced by a `.Internal`
function that is best accessed through the wrapper

```R
wrapper <- function(x, srt = 0, nna = 0) .Internal(wrap_meta(x, srt, nna))
```

A call to `shallow_duplicate` for a wrapper object will mark the
payload as immutable; the payload will be duplicated when a writable
data pointer is requested.

If the `srt` argument to `wrapper` is `0` or the `nna` argument is
`FALSE` then the default methods for accessing these values delegate
to the corresponding methods for the payload object.


### Attributes

For now the object to be wrapped must not contain any attributes; this
could easily be remedied by copying or possibly moving any attributes
to the wrapper object.

The expressions

```R
> x <- wrapper(c(1, 2, 3))
> y <- x
> attr(y, "foo") <- "stuff"
```

result in duplicating and modifying the wrapper object value of `y`,
but the payload is not duplicated and is shared by the values of `x`
and `y`:

```R
> .Internal(inspect(x))
@3299280 14 REALSXP g0c0 [NAM(2)]  wrapper [srt=0,no_na=0]
  @3288b98 14 REALSXP g0c3 [NAM(2)] (len=3, tl=0) 1,2,3
> .Internal(inspect(y))
@3298c60 14 REALSXP g0c0 [NAM(1),ATT]  wrapper [srt=0,no_na=0]
  @3288b98 14 REALSXP g0c3 [NAM(2)] (len=3, tl=0) 1,2,3
ATTRIB:
  @333c6e8 02 LISTSXP g0c0 [] 
    TAG: @2189008 01 SYMSXP g0c0 [MARK] "foo"
    @3289948 16 STRSXP g0c1 [NAM(2)] (len=1, tl=0)
      @32899f0 09 CHARSXP g0c1 [gp=0x60] [ASCII] [cached] "stuff"
```

An assignment to an element of `y` does result in duplicating the data
payload:

```R
> x <- c(1, 2, 3)
> y <- wrapper(x)
> y[1] <- 5
> y
[1] 5 2 3
> x
[1] 1 2 3
> .Internal(inspect(y))
@3298c60 14 REALSXP g0c0 [NAM(1),ATT]  wrapper [srt=0,no_na=0]
  @32889b8 14 REALSXP g0c3 [] (len=3, tl=0) 5,2,3
ATTRIB:
  @333c6e8 02 LISTSXP g0c0 [] 
    TAG: @2189008 01 SYMSXP g0c0 [MARK] "foo"
    @3289948 16 STRSXP g0c1 [NAM(2)] (len=1, tl=0)
      @32899f0 09 CHARSXP g0c1 [gp=0x60] [ASCII] [cached] "stuff"
```

Attribute wrapping can be used to define a version of `structure` that
does not copy payload:

```R
struct <- function(.Data, ...) structure(wrapper(.Data), ...)
```


### Meta Data

We can create an increasing sequence and use a wrapper marking it as
increasing and having no NAs:

```R
> x <- wrapper(c(1, 2, 3, 4, 5), 1, TRUE)
> .Internal(inspect(x))
@2c54158 14 REALSXP g0c0 [NAM(1)]  wrapper [srt=1,no_na=1]
  @351d128 14 REALSXP g0c4 [NAM(2)] (len=5, tl=0) 1,2,3,4,5
```

Operations that do not access a writable data pointer leave the meta
data intact. For example,

```R
> head(x)
[1] 1 2 3 4 5
> .Internal(inspect(x))
@2c54158 14 REALSXP g0c0 [NAM(2)]  wrapper [srt=1,no_na=1]
  @351d128 14 REALSXP g0c4 [NAM(2)] (len=5, tl=0) 1,2,3,4,5
```

The `wrapper` function is a closure and therefore its argument is
marked as referenced and the payload of the wrapper is then marked as
immutable. Since `print` still requests a writable pointer printing
the object results in duplicating the payload and clears the meta data:

```R
> x
[1] 1 2 3 4 5
> .Internal(inspect(x))
@2c54158 14 REALSXP g0c0 [NAM(2)]  wrapper [srt=0,no_na=0]
  @351c9b8 14 REALSXP g0c4 [] (len=5, tl=0) 1,2,3,4,5
```


# Alternatives and Variations

Not inlining `REAL_ELT` for compact sequences incurs a penalty of
about 1-10% in a simple loop. An alternative would be to not inline in
general but special-case handing of compact sequences in `for()` and
`STEPFOR`, as previously done in the byte code engine. This could be
added even with inlining.

One option would be to give _all_ objects a dispatch table and use
this for handling basic operations like `length`. This does work, but
the performance hit resulting from the additional function calls seems
a bit high. It does simplify code considerably though.



# Open Issues


## Unintended Allocations

A major goal of this mechanism is to allow R to deal cleanly with
subsets of large data objects. But some operations might attempt to
allocate large vectors and create problems. For example, if the value
of `x` is a sequence `1 : n` for a very large `n`, or a reference to a
memory-mapped file that allows access to the `DATAPTR`, then
computations like
```R
x + 1
log(x)
```
will attempt to allocate large result vectors, which may cause problems.

Not allowing `DATAPTR` access is one solution, but may be too drastic.
A better approach would be to have a threshold such that allocations
below the threshold succeed, while allocations beyond the threshold
raise some form of continuable error that can allows the opportunity
to adjust the threshold. The Common Lisp `cerror` mechanism, or
something similar, may be useful.

When the value of `x` is a large sequence, then evaluating `log(x)`
creates two separate issues: First, accessing the `DATAPTR` results in
an attempt to allocate the full sequence. Then a result of the same
length is allocated. Rewriting more code to not use the `DATAPTR`
unless it is available will alleviate the first, but not the second.
If the result of the second computation is used in a reduction, or if
only a subset is used, then deferring evaluation and pushing the
subset operation into the deferred evaluation can alleviate the second
issue.


## Memory Mapping Issues

The serialization issues associated with memory-mapped files have
already been mentioned. Another issue is that the file data may not be
in the form that matches the R internal types `double` or `int`. An
integer file might contain one, two, four, or eight byte integers; a
floating point file might contain `float` or `double` values. Only
`double` and `int` would be compatible with providing R with a
`DATAPTR`. The others could be handled by `Elt` and `Get_region`
methods for operations that do not require the `DATAPTR`.


## Length and Address Validity

For standard `R`vectors results returned by the `length` and `DATAPTR`
functions remain valid until the vector is reclaimed by the memory
manager. Should this be required of `ALTREP` objects as well?
It may have to be as code may be relying on this assumption.

But one option that would be useful is an array that can grow or
shrink, along the lines of Common Lisp arrays with fill pointers. Can
this be supported in a reasonably sane way?


## Deferred Evaluation Issues

One issue to keep in mind when deferring computations is that this may
also defer signaling of errors and warnings. Ideally it would be best
to check inputs before deferral and only defer when no warnings or
errors are possible. For deferred strings, the conversions as such
will not result in any warnings or errors. However, an out of memory
error could occur at the time the deferred computation is performed.
This seems something we would have to live with if computations are
deferred.

There might be some useful ideas in the Haskell literature on handing
errors in a non-strict language. The Haskell work on profiling may
also be relevant as it attempts to attribute costs of deferred
computations to the point they were requested.


## Meta Data Issues

Wrappers could be used to hold other meta data and possibly for memoizing
calls to summaries like `sum` or `max`.

It would be possible to have the vector allocation function always
return a wrapped vector for allocations above a specified size.

Should a vector being sorted imply no NA values? Are there other
relations among meta data that should be maintained?

One possibility would be to provide methods for setting meta
data. This might be useful in some situations but also seems very
dangerous.


## Other Notes

- Use the `ALTREP` mechanism to allow alternate implementations of
  environments. This will require a significant rewriting and
  refactoring of the code in `envir.c`, as well as a few other places
  that the internals of environments have leaked into, but it should
  be well worth while. Moving the user database code out of the main
  section should help make both that code and the base code more
  maintainable. Allowing alternate implementations will also give more
  options for compiled code.

- Add `SET_REAL_ELT` and similar functions to allow writes without
  requiring `DATAPTR` access.

- A more general deferred computation mechanism. The deferred string
  conversions could be a special case of such a more general
  mechanism.

- If we got rid of the `ALTVEC` layer (just `Dataptr`, and
  `Dataptr_or_null` and `Extract_subset` for now) then we could define
  all concrete methods for `R_altreal_t` and such to get some degree
  of static type checking.  This would be awkward if not impossible if
  the had to define some things for `R_altvec_t` since `C` has no
  sub-typing options.
  
- Attaching meta data at the `ALTVEC` level would make some things
  simpler. Some downsides:
    - Things like `Is_sorted` do not make sense for all vector types.
    - Adding static type checking would be more difficult if not impossible.
    - Dynamic type checking would be expensive unless we use a header
      bit for flagging objects as vectors.

<!--
# Random Notes ****
- Checks to run periodically:
    - run Tomas' PROTECT tool
    - check barrier, 32-bit version
    - check long vector stuff still works
    - check performance gain/loss, laptop and itasca

- Stuff for later
    - officially consider REAL, etc, as possibly allocating
    - better error message for no pointer access?
    - check on attribute_hidden for all non-static routines
        - these seem to need it for packages using USE_RINTERNALS since
	  they are used in Rinlinedfuns.h:
            - ALTVEC_DATAPTR
            - ALTVEC_DATAPTR_OR_NULL
            - ALTVEC_LENGTH
            - ALTVEC_SHORTLENGTH
            - ALTVEC_TRUELENGTH
    - cleanup function for unloading package shared libraries
    - summaries only done for integer, real (maybe logical)
    - support for a class hierarchy/virtual classes?
    - long vector loop support
    - inline compact sequences in do_for and compiled loops??
    - teach identical() about ALTREP?
    - mmap improvements
        - allow mmap to remap an unmapped vector?
	- add some test code?
        - add int16 to mmap?
            - to be writeable would need `SET_REAL_ELT`
    - look into ddr, sparklyr
    - make test code available
    - think about displaced arrays, contiguous sub-matrices
    - rename string class as `deferred_as_character` or some such, at least
      in the registration?
-->
