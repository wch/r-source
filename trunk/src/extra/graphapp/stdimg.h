#include <R_ext/libextern.h>
#undef LibExtern
#ifdef GA_DLL_BUILD
# define LibExtern LibExport
#else
# define LibExtern extern LibImport
#endif

LibExtern image cam_image;
LibExtern image color_image;
LibExtern image console_image;
LibExtern image console1_image;
LibExtern image copy_image;
LibExtern image copy1_image;
LibExtern image copypaste_image;
LibExtern image cut_image;
LibExtern image erase_image;
LibExtern image help_image;
LibExtern image open_image;
LibExtern image open1_image;
LibExtern image paste_image;
LibExtern image paste1_image;
LibExtern image print_image;
LibExtern image save_image;
LibExtern image stop_image;
#undef LibExtern
