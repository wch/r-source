void R_ShowMessage(char *s);
void Rstd_read_history(char *s);

void Rstd_Suicide(char *s);
void Rstd_ShowMessage(char *s);
int Rstd_ReadConsole(char *prompt, unsigned char *buf, int len, 
		     int addtohistory);
void Rstd_WriteConsole(char *buf, int len);
void Rstd_ResetConsole();
void Rstd_FlushConsole();
void Rstd_ClearerrConsole();
void Rstd_Busy(int which);
void Rstd_CleanUp(int saveact, int status, int runLast);
int Rstd_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
		   int del, char *pager);
int Rstd_ChooseFile(int new, char *buf, int len);

void Rgnome_Suicide(char *s);
void Rgnome_ShowMessage(char *s);
int Rgnome_ReadConsole(char *prompt, unsigned char *buf, int len, 
		     int addtohistory);
void Rgnome_WriteConsole(char *buf, int len);
void Rgnome_ResetConsole();
void Rgnome_FlushConsole();
void Rgnome_ClearerrConsole();
void Rgnome_Busy(int which);
void Rgnome_CleanUp(int saveact, int status, int runLast);
int Rgnome_ShowFiles(int nfile, char **file, char **headers, char *wtitle,
		   int del, char *pager);
int Rgnome_ChooseFile(int new, char *buf, int len);

