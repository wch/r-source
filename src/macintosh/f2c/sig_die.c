#include "stdio.h"



void sig_die(register char *s, int kill)
{
	/* print error message, then clear buffers */
	fprintf(stderr, "%s\n", s);

		exit(1);
}

