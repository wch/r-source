typedef struct {
    GPar dp;		/* current device default parameters */
    GPar gp;		/* current device current parameters */
    GPar dpSaved;		/* saved device default parameters */
} baseSystemState;

void registerBase();

