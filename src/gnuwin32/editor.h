
void menueditornew(control m);
void menueditoropen(control m);
int editorchecksave(editor c);
void editorsetfont(font f);
int Rgui_Edit(char *filename, int modal);

#define EDITORMAXTITLE 128
#define MAXNEDITORS 50

struct structEditorData {
    Rboolean file; /* is the editor associated with an existing file */
    char *filename; /* corresponding file */
    Rboolean stealconsole;  /* set when using fix() or edit(), so that no events are sent to console until this editor is closed */
    menuitem mcut, mcopy, mdelete, mfind, mreplace,
	mpopcut, mpopcopy, mpopdelete;
};
typedef struct structEditorData *EditorData;
