#define PARSE_NULL		0
#define PARSE_OK		1
#define PARSE_INCOMPLETE	2
#define PARSE_ERROR		3
#define PARSE_EOF		4

#define INPUT_CONSOLE		1
#define INPUT_TEXT		2
#define INPUT_FILE		3

	/* Parse A Single Expression */

SEXP R_Parse1File(FILE*, int, int*);
SEXP R_Parse1Buffer(IoBuffer*, int, int*);
SEXP R_Parse1Vector(TextBuffer*, int, int *);

	/* Parse Several Expressions */

SEXP R_ParseFile(FILE*, int, int*);
SEXP R_ParseBuffer(IoBuffer*, int, int*, SEXP);
SEXP R_ParseVector(SEXP, int, int *);
