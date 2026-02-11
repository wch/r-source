#ifndef __cplusplus
#error "This is not a C++ compiler"
# error "This is not a C++ compiler"
#elif __cplusplus < 201103L
# error "This is not a C++11 compiler"
#elif __cplusplus >= 201402L
# error "This is a compiler for C++14 or later"
#endif
