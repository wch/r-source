#ifndef WC_HISTORY_H
#define WC_HISTORY_H

void wgl_hist_init(int size, int beep);
void wgl_histadd(const wchar_t *buf);
wchar_t *wgl_hist_prev(void);
wchar_t *wgl_hist_next(void);
void wgl_savehistory(const char *file, int size);
void wgl_loadhistory(const char *file);
void wgl_savehistoryW(const wchar_t *file, int size);
void wgl_loadhistoryW(const wchar_t *file);


#endif /* WC_HISTORY_H */
