#ifndef __cplusplus
#error "This is not a C++ compiler"
#elif __cplusplus < 201402L
# error "This is not a C++14 compiler"
#elif __cplusplus >= 201703L
# error "This is a C++17 or later compiler"
#endif
