#define RTLD_LAZY 1
#define RTLD_NOW  2
#define RTLD_GLOBAL 3

void *dlopen(const char *, int);
void *dlsym(void *, const char *);
int dlclose(void *);
char *dlerror(void);
