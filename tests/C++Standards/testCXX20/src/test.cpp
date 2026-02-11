#ifndef __cplusplus
#error "This is not a C++ compiler"
#elif __cplusplus < 202002L
#error "This is not a C++20 compiler"
#elif __cplusplus >= 202302L
# error "This is a C++23 or later compiler"
#endif
