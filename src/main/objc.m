/* classes - the are used in the allocation code */
Class R_RObject;
Class R_RVector;
Class R_RList;
Class R_RPromise;
Class R_REnvironment;

#import <Foundation/Foundation.h>
#import <RObjC.h>

/* issues:
 * memory allocation:
   - alloc cannot be used for collective types (e.g. RObject or RVector) as the SEXP type is not known
   - alloc via ObjC does currently NOT protect the object and since we  have no ref counting in R, retain/release are ignored
   - proper use of matching protect/unprotect messages is not checked. they are a hack now and should NOT be used for new objects

   The current API is strictly experimental - it is likely to change a lot ...

*/

@implementation RObject
- (int) type { return TYPEOF(self); }

+ (id) alloc {
  NSLog(@"unspecified alloc called for class %@, returning NULL. Only specific subclasses can be allocated.", self);
  return R_NilValue;
}

+ (id) allocWithType: (int) type {
  return allocSExp(type);
}

- (void) release {
  NSLog(@"Release of %@ requested via ObjC, ignoring", self);
}

- (void) retain {
  NSLog(@"Retain of %@ requested via ObjC, ignoring", self);
}

- (void) protect {
  PROTECT(self);
}

- (void) unprotect {
  UNPROTECT(1);
}

- (void) autorelease {
  NSLog("Autorelease of %@ requested via ObjC, ignoring", self);
}
@end

@implementation RVector
+ (id) allocWithType: (int) type length: (R_len_t) len {
  return allocVector(type, len);
}
- (int) length { return LENGTH(self); }
- (void*) payload { return DATAPTR(self); }
@end

@implementation RList
- (RObject*) head { return CAR(self); }
- (RObject*) tail { return CDR(self); }
- (RObject*) tag { return TAG(self); }
@end

@implementation RPromise
@end

@implementation REnvironment
@end

static NSAutoreleasePool *pool = 0;

void R_init_objc() {
  pool = [[NSAutoreleasePool alloc] init];

  R_RObject = [RObject class];
  R_RVector = [RVector class];
  R_RList = [RList class];
  R_RPromise = [RPromise class];
  R_REnvironment = [REnvironment class];
}

SEXP objc_info(SEXP x) {
  Rprintf("sizeof(SEXPREC)=%d, sizeof(RObject)=%d\n", sizeof(SEXPREC), sizeof(RObject));
  /*{
    RObject *o = [[RObject alloc] init];
    }*/
  NSLog(@"type: %d\n", [((RObject*)x) type]);
  NSLog(@"length: %d\n", [((RObject*)x) length]);
  NSLog(@"object: %@\n", x);
  return [RVector allocWithType: INTSXP length:10];
}
