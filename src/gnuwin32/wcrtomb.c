/* The version in mingw-runtime-3.9 only works in Latin-1 */

#include <wchar.h>
#include <stdlib.h>
#include <locale.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static inline
unsigned int get_cp_from_locale (void)
{
  char* cp_string;
  /*
    locale :: "lang[_country[.code_page]]"
               | ".code_page"

  */

  if ((cp_string = strchr(setlocale(LC_CTYPE, NULL), '.')))
    return  ((unsigned) atoi (cp_string + 1));
  return 0;
}

static int
__wcrtomb_cp (char *dst, wchar_t wc, const unsigned int cp,
	      const unsigned int mb_max)
{
    if (cp == 0) { /* C locale */
	if (wc > 255) {
	    errno = EILSEQ;
	    return -1;
	}
	*dst = (char) wc;
	return 1;
    } else {
	int invalid_char = 0;

	int size = WideCharToMultiByte(cp, 0 /* Is this correct flag? */,
				       &wc, 1, dst, mb_max,
				       NULL, &invalid_char);
	if (size == 0 || invalid_char) {
	    errno = EILSEQ;
	    return -1;
        }
	return size;
    }
}

size_t
wcrtomb (char *dst, wchar_t wc, mbstate_t *ps)
{
    char byte_bucket [MB_LEN_MAX];
    char* tmp_dst = dst ? dst : byte_bucket;
    return (size_t)__wcrtomb_cp (tmp_dst, wc, get_cp_from_locale (),
				 MB_CUR_MAX);
}
