typedef int (*blah1) (char *, char *, int, int);
typedef void (*blah2) (char *, int);
typedef void (*blah3) ();
typedef void (*blah4) (char *);
typedef int (*blah5) (char *);
typedef void (*blah6) (int);
#ifdef Win32
typedef enum {RGui, RTerm, LinkDLL} UImode;
#endif

typedef struct
{
    char *rhome;               /* R_HOME */
    char *home;                /* HOME  */
    blah1 ReadConsole;
    blah2 WriteConsole;
    blah3 CallBack;
    blah4 message;
    blah5 yesnocancel;
    blah6 busy;
    int R_Quiet;               /* > 0 to suppress messages */
    int R_Slave;               /* ?? */
    int R_Interactive;
    int R_Verbose;
    int RestoreAction;         /* Read HOME/.RData if > 0 */
    int SaveAction;
    int LoadSiteFile;
    int LoadInitFile;
    int DebugInitFile;
    int NoRenviron;
    int vsize;
    int nsize;    
#ifdef Win32
    UImode CharacterMode;
#endif
} structRstart;

typedef structRstart *Rstart;
