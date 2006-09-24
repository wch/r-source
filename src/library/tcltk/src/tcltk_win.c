#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

#include <tk.h>
#include <tkPlatDecls.h>

/* Define this to activate the MDI support code */
#define TCLMDI

void tcltk_init();

extern __declspec(dllimport) void (* R_tcldo)();

static void _R_tcldo()
{
    Tcl_ServiceAll();
}

static void (* old_R_tcldo)();

#ifdef TCLMDI
static HWND MDIClientHandle = 0;
static int UpdateMDIMenu = 0;
/* This would make more sense in GWL_USERDATA, but changes there don't take effect fast enough */
static int nestedCall = 0;
#endif

void tcltk_start(int* handle)
{
    HWND active = GetForegroundWindow(); /* ActiveTCL steals the focus */
    tcltk_init(); /* won't return on error */
    old_R_tcldo = R_tcldo;
    R_tcldo = &_R_tcldo;
    _R_tcldo();  /* one call to trigger the focus stealing bug */
    SetForegroundWindow(active); /* and fix it */
 #ifdef TCLMDI   
    if (*handle) MDIClientHandle = GetParent((HWND)*handle);
 #endif
}

void tcltk_end()
{
    R_tcldo = old_R_tcldo;
}

#ifdef TCLMDI

#define TK_CLAIMFOCUS     (WM_USER)
#define TK_GEOMETRYREQ    (WM_USER+1)
#define TK_ATTACHWINDOW   (WM_USER+2)
#define TK_DETACHWINDOW   (WM_USER+3)
#define GWL_CONTAINEE	  (GWL_USERDATA)

static int adjwidth() {
    return 2 * GetSystemMetrics(SM_CXEDGE) + 3;
}

static int adjheight() {
    return GetSystemMetrics(SM_CYCAPTION) + 
			    2 * GetSystemMetrics(SM_CYEDGE) + 3;
}

LRESULT CALLBACK tcltk_windowProc(HWND hwnd, UINT message, WPARAM wParam, LPARAM lParam) {

  switch (message) {
    case WM_CREATE: 
    	UpdateMDIMenu = 1;
      	break;

    case WM_WINDOWPOSCHANGED:   {
	WINDOWPOS *pos = (WINDOWPOS *) lParam;
	HWND hContainee = (HWND) GetWindowLong(hwnd, GWL_CONTAINEE); 	
	Tk_Window winPtr = Tk_HWNDToWindow(hContainee);

	if (!winPtr) break;
	
	if (!(pos->flags & SWP_NOSIZE)) {
	    if (!nestedCall) {	    
		/*
		 * Update the shape of the contained window.
		 */	
	        RECT rect;
		int width, height;

		nestedCall = 1; 
		width = pos->cx - adjwidth();
		height = pos->cy - adjheight(); 

		GetWindowRect(hContainee, &rect);	    
		if (width  != rect.right-rect.left || height != rect.bottom-rect.top) {
		    XWindowChanges changes;
	            changes.width = width;
	            changes.height = height;
		    Tk_ConfigureWindow(winPtr, CWWidth | CWHeight, &changes);
		    Tcl_ServiceAll();
		}
		SetWindowPos(hwnd, NULL, 0, 0, pos->cx, pos->cy,
		    SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);
		nestedCall = 0; 
	    }
	    return 0;
	}
	break;
    }
    case WM_PAINT:
        if (UpdateMDIMenu) {
	   SendMessage(MDIClientHandle, WM_MDIREFRESHMENU, 0, 0);    
	   DrawMenuBar(GetParent(MDIClientHandle));
	   UpdateMDIMenu = 0;
	}
        break;
        
    case TK_CLAIMFOCUS:
	if (wParam || (GetFocus() != NULL)) {
	    HWND hContainee = (HWND) GetWindowLong(hwnd, GWL_CONTAINEE);
	    SetFocus(hContainee);
	}
	return 0;

    case TK_GEOMETRYREQ: {
        /*
         * Skip nested calls to avoid flashing
         */
    	if (nestedCall) return 0;  
	/*
	 * Adjust the request to include the frame; it will be handled by 
	 * the WM_WINDOWPOSCHANGED handler
	 */

	wParam += adjwidth();
	lParam += adjheight();

        SetWindowPos(hwnd, NULL, 0, 0, wParam, lParam,
		    SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);
	return 0;
    }
    case TK_ATTACHWINDOW:
        SetWindowLong(hwnd, GWL_CONTAINEE, (long) wParam);
	SetParent((HWND)wParam, hwnd);
	return 0;

    case TK_DETACHWINDOW:
        SetWindowLong(hwnd, GWL_CONTAINEE, 0);
	PostMessage(hwnd, WM_CLOSE, 0, 0);
	return 0;
  } 
  return DefMDIChildProc(hwnd, message, wParam, lParam); 
}

static ATOM registerTCLTKClass() {
  WNDCLASSEX wcex;
  ZeroMemory( &wcex, sizeof(WNDCLASSEX) );
  wcex.cbSize         = sizeof(WNDCLASSEX);
  wcex.cbWndExtra     = sizeof (long);
  wcex.lpfnWndProc    = (WNDPROC) tcltk_windowProc;
  wcex.hIcon          = LoadIcon(Tk_GetHINSTANCE(), "tk");
  wcex.hIconSm        = LoadIcon(Tk_GetHINSTANCE(), "tk"); 
  wcex.hCursor        = LoadCursor(NULL, IDC_ARROW);
  wcex.hbrBackground  = (HBRUSH)(COLOR_BTNFACE + 1);
  wcex.lpszClassName  = "RTkTopLevel";
  return RegisterClassEx(&wcex);
}

static ATOM RTCLTKclass = 0;

void tcltk_window(int * handle)
{
  if (*handle) PostMessage((HWND) *handle, WM_CLOSE, 0, 0);
  else {
    if (MDIClientHandle) {
      if (!RTCLTKclass) RTCLTKclass = registerTCLTKClass();

      *handle = (int) CreateMDIWindow(
	  MAKEINTATOM(RTCLTKclass)
	, "tcltk"
	, MDIS_ALLCHILDSTYLES | WS_OVERLAPPEDWINDOW | WS_CLIPCHILDREN
	, CW_USEDEFAULT, CW_USEDEFAULT
	, CW_USEDEFAULT, CW_USEDEFAULT
	, MDIClientHandle
	, GetModuleHandle(NULL)
	, 0
	);
	SetFocus((HWND)*handle);
      }
    }
  }
#else
void tcltk_window(int * handle)
{
    *handle = 0;
}
#endif /* TCLMDI */
