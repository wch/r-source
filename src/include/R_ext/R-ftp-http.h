void *R_HTTPOpen(const char *url);
int   R_HTTPRead(void *ctx, void *dest, int len);
void  R_HTTPClose(void *ctx);

void *R_FTPOpen(const char *url);
int   R_FTPRead(void *ctx, void *dest, int len);
void  R_FTPClose(void *ctx);

void *	RxmlNanoHTTPOpen(const char *URL, char **contentType);
int	RxmlNanoHTTPRead(void *ctx, void *dest, int len);
void	RxmlNanoHTTPClose(void *ctx);
int 	RxmlNanoHTTPReturnCode(void *ctx);
void	RxmlNanoHTTPTimeout(int delay);

void *	RxmlNanoFTPOpen(const char *URL);
int	RxmlNanoFTPRead(void *ctx, void *dest, int len);
int	RxmlNanoFTPClose(void *ctx);
void	RxmlNanoFTPTimeout(int delay);

/* not currently used */

void RxmlNanoFTPCleanup(void);
void RxmlNanoHTTPCleanup(void);
