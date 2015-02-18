/* Compatibility wrapper for R */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
int trio_printf(const char *format, ...);
int trio_vprintf(const char *format, va_list args);
int trio_fprintf(FILE *file, const char *format, ...);
int trio_sprintf(char *buffer, const char *format, ...);
int trio_vsprintf(char *buffer, const char *format, va_list args);
int trio_vfprintf(FILE *file, const char *format, va_list args);
int trio_snprintf(char *buffer, size_t max, const char *format, ...);
int trio_vsnprintf(char *buffer, size_t bufferSize, const char *format,
		   va_list args);
int trio_vasprintf(char **ret, const char *format, va_list args);


int printf(const char *format, ...)
{
    int res;
    va_list(ap);
    va_start(ap, format);
    res = trio_vprintf(format, ap);
    va_end(ap);
    return res;
}

int fprintf(FILE *file, const char *format, ...)
{
    int res;
    va_list(ap);
    va_start(ap, format);
    res = trio_vfprintf(file, format, ap);
    va_end(ap);
    return res;
}

int sprintf(char *buffer, const char *format, ...)
{
    int res;
    va_list(ap);
    va_start(ap, format);
    res = trio_vsprintf(buffer, format, ap);
    va_end(ap);
    return res;
}


int vprintf(const char *format, va_list args)
{
    return trio_vprintf(format, args);
}


int vsprintf(char *buffer, const char *format, va_list args)
{
    return trio_vsprintf(buffer, format, args);
}

int vfprintf(FILE *file, const char *format, va_list args)
{
    return trio_vfprintf(file, format, args);
}

#ifndef Win32
/* These are needed as MinGW's stdio.h has inline snprintf and vnsprintf.
   Include the trioremap.h header file to get the replacements */
int snprintf(char *buffer, size_t max, const char *format, ...)
{
    int res;
    va_list(ap);
    va_start(ap, format);
    res = trio_vsnprintf(buffer, max, format, ap);
    va_end(ap);
    return res;
}

int vsnprintf(char *buffer, size_t bufferSize, const char *format, va_list args)
{
    return trio_vsnprintf(buffer, bufferSize, format, args);
}

int _vsnprintf(char *buffer, size_t bufferSize, const char *format, va_list args)
{
    return trio_vsnprintf(buffer, bufferSize, format, args);
}
#endif

int vasprintf(char **ret, const char *format, va_list args)
{
    return trio_vasprintf(ret, format, args);
}
