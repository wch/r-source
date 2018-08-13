---
title: "ALTREP: TODOs and Topics For Discussion"
author: Gabe Becker
output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: false
---

<!-- render with
Rscript -e 'rmarkdown::render(input="ALTREP_discussion_todo.md", output_file="ALTREP_discussion_todo.html")'
-->


# Direct use of metadata

## arithmetic summary functions (`sum`, `mean`, `prod`)

We can use `NO_NA` information to move NA checking outside of tight
loops. Initial testing suggests about a 1.7x speedup for integer
summing.

### Caveats and gotchas:

We can get the tight loop to be completely empty but need to ensure
updated (ie return value) is correct in all cases. if we take it out
of the loop.  This also, perhaps expectedly(?), seems to make a much
bigger difference in the integer case than the double case.

## `is.na`, `anyNA`, `na.omit`

We already use `NO_NA` to fastpass these, but in cases where we know
sortedness but don't know NA status (rare but technically possible) we
should be checking only the ends of the vector for `anyNA` and
leveraging sortedness for is.na. This can be either a binary search
for the first (na at end, or last (na at start) NA value, or a linear
walk that starts at the relevant end and walks until it sees a non-NA.

## `min`, `max`, `which.min`, `which.max`

`min`, `max` already dispatch to altrep methods, but core behavior should
use sortedness if present, after method dispatch (if the method
returns NULL)

`which.min`, `which.max` should use sortedness whether or not they
ultimately get methods as well.

## `match` (and thus `%in%`)

Test using sortedness in core match data. Binary search. rewrite
macros in `ALTREP` branch in more maintainable form and move out of
altrep methods.

# User-level R functions/operations that can always return ALTREPs with metadata

##  is.na

* Guaranteed no NAs
* Sorted if x is sorted
    + Will need logic to determine sortedness direction

## sample.int

Guaranteed No NAs

# `R` functions/ops which can sometimes return ALTREPs with metadata

"Propogation" refers here to infering guaranteed metadata about the
return value based on the metadata of one or more arguments passed in
("parents")



## Single propogation (one parent)

### Including sortedness

* `lag` 
* `head` (if not covered more generally by bracket)
* `tail` (if not covered more generally by bracket)

### Excluding sortedness (only `No_NA`)

* `cumsum`, `cumprod` (contingent on overflow check)
* `rep`
* `sample`

## Double propogation (two parents)

### Including sortedness

* `+ - * /` (contingent on overflow checking)
    + at least one scalar operand
* `* /`
    + one operand scalar
    +direction will reverse if scalar is < 0 
* `< <= > >=`
    + at least one operand scalar

### Excluding sortedness (only NO_NA)

* `+ - * / ^` (contingent on overflow checking)
    + both operands length > 1
*  `< > >= <= ==` 
    + both operands length > 1
* `& |`
* `[` (on a vector/1d array x, atomic vector indices i)
    + for logical indices `length(i) <= length(x)`
    + for numeric indices `max(i) <= length(x)`
* `rnorm` (mean, sd are two parents)
* `runif` (min, max are two parents)

## propogation mechanisms

Some propogation situations will be essentially one-off and require
custom logic, but I envision there being C level functions (or macros
if desired) representing the common metadata propagation
relationships, which can be used to wrap return values passed back up
to R from core C code.

Should only be called if the relevant "parent" objects are all
intact ALTREPs (ie non-expanded, with valid metadata to derive from)

```C
/* parent is an ALTREP with valid metadata, ie non-expanded */
  SEXP derive_nainfo1(SEXP ans, SEXP parent) {
      SEXP ret;
      switch(TYPEOF(parent)) {
      case INTSXP: 
        ret = int_derive_nainfo1(ans, parent);
        break;
      case REALSXP:
        ret = real_derive_nainfo1(ans, parent);
        break;
      /*etc*/
      default:
      /*need to be careful about protecting this in caller if we do it
        this way we could also always return a wrapper even if the all
        metadata is unknown/uninformative but that would be adding
        overhead for no reason...  

	On the other hand, we shouldn't even be calling this function unless 
        parent is an intact ALTREP so that shouldn't happen much. ~GB*/
        ret = ans; 
        break;
      }
      return ret;
  }

  SEXP int_derive_nainfo1(ans, parent) {
      SEXP ret;
      int nona = INTEGER_NO_NA(parent);
      if(nona == 1) {
          SEXP mdata = PROTECT(allocVector(VECSXP, 2));
          VECTOR_SET_ELT(mdata, 0, ScalarInteger(nona));
	  VECTOR_SET_ELT(mdata, 1, ScalarInteger(UNKNOWN_SORTEDNESS));
	  ret = make_wrapper(ans, mdata);
	  UNPROTECT(1);
      } else {
          ret = ans;
      }
      return ret
  }
```

# `ALTREP` Classes

## Internal

### Rles

Internal because we (may) want, e.g., scalar comparison against sorted
vectors to return logical RLE. E.g., when x is sorted, is.na, and
scalar comparisons of x can return rles.

## External

### HashTabled vectors

Vectors which carry around their hash-table once it is created, thus
making matching really fast.

### Indexed vectors

Vectors which carry around their order, making them fast to sort,
match against, etc.


# Other work

## formatting and printing

Currently operates directly on dataptr. Need version that operates on SEXP via `*_GET_REGION` and doesn't expand altreps.

This can be mostly simple wrapper functions around what's there now I think. A first pass that seems to work is in the branch.

## Handling attrbitues in more cases when wrapping
   
# For Discussion

## Changing No_NA metadata (possibly with name change)

from 0/1 indicating only knowledge of NA absence to

* `-1/0/1` or `-1/NA/1`
    + indicating known NA presence, unknown, known NA absence, or
* The known number of NAs present (NA or 0+ count)
    + Note `*_NO_NA` could still be supported here if desired

## What to do about scalars

A lot of functions return scalars where we can infer and attach
No_NA-ness metadata. Not sure we want to do this, though, because of

overhead.

### Possible solution

Change `*_NO_NA(x)` to just do the check if x is length 1.

upsides: no wrapping scalar SEXPs, saves on unneeded overhead

downsides: Breaks the assumption that *_NO_NA always returns 0 for non
`ALTREP` SEXPs. I'm not sure this is bad, but it is different.

Note: this won't be faster in the fast-passes case, but it will make the propogation logic much more unified, I think.

## Can we retain metadata as a side-effect in some cases?

e.g., is.na(x) doesn't find any NAs, it would be nice if x retained
that info even though it's not the return value. Same for is.unsorted(x)

Note: This is only relevant when NOT talking about return value, for
which creating a new wrapper `ALTREP` should be sufficient, especially
once wrappers of wrappers will collapse themselves at creation time
where appropriate.

Note: My initial conception included soemthing along the lines of
`Set_No_NA(x, val)` and `Set_Is_Sorted(x, val)` in the `ALTREP` methods
table as part of the API, but current thinking is to not expose
something like that, even to the R internals, much less package-space.

### Possible Solution 1: method for each op which should annotate metadata inplace

e.g., `Is_NA` method present in current `ALTREP` branch.

Upsides: Doesn't allow non-`ALTREP` code to penetrate the
abstraction. Doubles as a way to have custom mechanisms for the chosen
ops (e.g. is.na on RLEs)

Downsides: Each time we add fucntionality to do something like this
we'll have to add a whole new method to the `ALTREP` table.

### Possible Solution 2: Revisit metadata setter methods

Unlikely, here largely for completeness.

### Possible Solution3: Don't support that or heavily restrict what ops its supported in

We can just say yeah, that would be cool, but we don't allow it. only
return values can be annotated by any operation that isn't already a
method in the table

Upsides: Easiest.
Downsides: Feels like leaving something on the table.

## User-code level introspection of ALTREPs

Some package developers may want to. e.g., treat rle ALTREPs differently

Currently getting info about what type of `ALTREP` x is is restricted to
the semi-user-facing `.Internal(inspect(x))`, which can't be
programmed against, and some undocumented, non-API macros deep in
altrep.c

## methods/ops exclusive to specific ALTREP classes

Is there any reasonable way to support this? Do we want to?

### Example: a "windowing" ALTREP

`ALTREP` class which provides a no-copy window into another vector, code
will want to move the window around. For example when operating on
chunks/groups of data in the parent vector.

But a `Set_Window` (or whatever we call it) method doesn't make any
sense for any of the other vector types.

### Possible solution 1: Allow developers to create new ALTREP "types"

Which inherit from one of the basetypes. E.g. `ALTWINDOWEDINTEGER` which would have the `Set_Window` method. along with all the `ALTINTEGER` methods

Upsides: empowers developers to do some pretty powerful stuff within the `ALTREP` framework.
Downsides: Dramatically increases the complexity of the `ALTREP` machinery and, effectively, the C API

## Possible new methods for consideration for Integer/Real

We may not ultimately want to implement all of these, need to weigh
complexity/size of API vs expected benefits. This is intentionally an
anti-conserative list of possibilities for discussion.

### Sort_check

Implements custom logic to actually perform a sort-check on the data (as opposed to the metadata-based fastpass)

Example: only checking the "runs" vector for an RLE

### Sort

Implements custom sorting logic

Example: asking a columnstore to create a temporary column with the sorted data.

### Order

Implements custom logic for order. 

### Is_NA

Implements custom is.na logic for the `ALTREP` data representation

Example: Arrow uses a bitmask to represent missingness (they call it
null-ness). is.na needs to compute on that

### Which, which.min, which.max

Example: `ALTREP` pointing to Indexed columnstore memory where we want
the columnstore to do the min computation.

### Set_elt

The current decision is not to include this, but it would be useful,
particularly in non-read-only interoperability settings, to see if it
makes sense to include

Gotchas/downsies: very easy to accidentally create a vector that has
reference mechanics, which I think we really want to avoid.

### Match

### Unique

### Duplicated

