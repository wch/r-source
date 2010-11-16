#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char* argv[])
{
    int status = 0;
    char line[1000];

    printf("stdout 1\n"); fflush(stdout);
    fprintf(stderr, "stderr 1\n");
    fflush(stderr);

    if (argc > 1 && strcmp(argv[1], "1") == 0) {
	while(fgets(line, 1000, stdin)) printf("stdin: %s", line);
	fflush(stdout);
    }
    if (argc > 1 && strcmp(argv[1], "1")) {
	status = atof(argv[1]);
    }
    
    exit(status);
}
