#include <stdlib.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
    int status = 0;
    char line[1000];

    printf("stdout 1\n"); fflush(stdout);
    fprintf(stderr, "stderr 1\n");
    fflush(stderr);

    if (argc > 1) {
	while(fgets(line, 1000, stdin)) printf("stdin: %s", line);
	fflush(stdout);
    }
    
    exit(status);
}
