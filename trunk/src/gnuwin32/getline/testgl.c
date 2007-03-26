#include <stdio.h>
#include "getline.h"


main()
/* 
 * just echo user input lines, letting user edit them and move through
 * history list
 */
{
    char *p;

    do {
	p = getline("PROMPT>>>> ");
	gl_histadd(p);
	fputs(p, stdout);
    } while (*p != 0);
}
