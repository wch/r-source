#ifndef SINT_MAX
# ifdef USING_R
typedef int Sint;
#  define SINT_MAX INT_MAX
#  define SINT_MIN INT_MIN
# else
typedef long Sint;
#  define SINT_MAX LONG_MAX
#  define SINT_MIN LONG_MIN
# endif
#endif

