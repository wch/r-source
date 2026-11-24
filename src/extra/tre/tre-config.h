/* tre-config.h -- R-maintained configuration for the bundled TRE.

   Upstream generates this file (and config.h) with its own configure;
   in R it is static, with HAVE_* macros otherwise supplied by R's
   config.h.  Note that this file must contain only the bare minimum of
   definitions without the TRE_ prefix to avoid conflicts between
   definitions here and definitions included from somewhere else. */

/* Define if you want to enable approximate matching functionality. */
#define TRE_APPROX 1

/* Define to enable multibyte character set support. */
#define TRE_MULTIBYTE 1

/* Define to a field in the regex_t struct where TRE should store a pointer to
   the internal tre_tnfa_t structure */
#define TRE_REGEX_T_FIELD value

/* Define if you want TRE to use alloca() instead of malloc() when allocating
   memory needed for regexec operations.  Deliberately not defined for R:
   the matchers allocate buffers proportional to the automaton size, which
   for large patterns can exceed the stack. */
/* #define TRE_USE_ALLOCA 1 */

/* Define to enable wide character (wchar_t) support. */
#define TRE_WCHAR 1

/* TRE version string.  The sources are post-0.9.0 upstream plus the pull
   requests listed in R_changes, so the commit they were taken from is
   included to keep extSoftVersion() unambiguous; update both together. */
#define TRE_VERSION "0.9.0-46577ec"
