#ifndef GETLINE_H
#define GETLINE_H

/* unix systems can #define POSIX to use termios, otherwise 
 * the bsd or sysv interface will be used 
 */

#if __STDC__ > 0
#include <stddef.h>

typedef size_t (*gl_strwidth_proc)(char *);

/* read a line of input */
int             getline(char *prompt, char *buf, int maxlen);  

void            gl_setwidth(int);		/* specify width of screen */
void            gl_histadd(char *);		/* adds entries to hist */
void		gl_strwidth(gl_strwidth_proc);	/* to bind gl_strlen */
void		gl_loadhistory(char *);
void		gl_savehistory(char *, int size);
void            gl_hist_init(int, int);		/* set up history buffer */
char    	*gl_hist_next(void);	/* return ptr to next item */
char    	*gl_hist_prev(void);	/* return ptr to prev item */

extern int 	(*gl_in_hook)(char *);
extern int 	(*gl_out_hook)(char *);
extern int	(*gl_tab_hook)(char *, int, int *);

#ifdef Win32
extern void 	(*gl_events_hook)(void);
#endif


#else	/* not __STDC__ */

char           *getline();	
void            gl_setwidth();
void            gl_histadd();
void		gl_strwidth();
void		gl_loadhistory();
void		gl_savehistory();
void            gl_hist_init();
char    	*gl_hist_next();
char    	*gl_hist_prev();

extern int 	(*gl_in_hook)();
extern int 	(*gl_out_hook)();
extern int	(*gl_tab_hook)();

#endif /* __STDC__ */

#endif /* GETLINE_H */
