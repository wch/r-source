provider rtrace {
    probe eval__entry(char *, int);
    probe eval__dispatch(char *, char *);
    probe native__entry(char *, char *);
    probe native__exit(char *, char *);
    probe gc__start(unsigned long long, int);
    probe gc__end(int, int);
};
