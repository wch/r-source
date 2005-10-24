/*******************************************************************************
 *  StatConn: Connector interface between application and interpreter language
 *  Copyright (C) 1999--2005 Thomas Baier
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 *  MA 02111-1307, USA
 *
 ******************************************************************************/

#ifndef _STATCONN_H_
#define _STATCONN_H_

#ifdef __cplusplus
extern "C" {
#endif

/* system specifics */
#include "SC_system.h"

#include "bdx.h"

/* forward definition */
struct _SC_Proxy_Object;
struct _SC_CharacterDevice;
struct _SC_GraphicsDevice;

/* error codes */

/* generic error and success codes */
#define SC_PROXY_OK                            0x00000000
#define SC_PROXY_ERR_UNKNOWN                   0x80000000

#define SC_PROXY_ERR_INVALIDARG                0x80000001
#define SC_PROXY_ERR_INVALIDFORMAT             0x80000002
#define SC_PROXY_ERR_NOTIMPL                   0x80000003

/* initialization and termination */
#define SC_PROXY_ERR_INITIALIZED               0x80000004
#define SC_PROXY_ERR_NOTINITIALIZED            0x80000005

/* evaluation, getting and setting symbols */
#define SC_PROXY_ERR_INVALIDSYMBOL             0x80000006
#define SC_PROXY_ERR_PARSE_INVALID             0x80000007
#define SC_PROXY_ERR_PARSE_INCOMPLETE          0x80000008
#define SC_PROXY_ERR_UNSUPPORTEDTYPE           0x80000009
#define SC_PROXY_ERR_EVALUATE_STOP             0x8000000a

/* version mismatch */
#define SC_PROXY_ERR_INVALIDINTERFACEVERSION   0x80000010
#define SC_PROXY_ERR_INVALIDINTERPRETERVERSION 0x80000011

/* type mask values */
#define SC_TM_SCALAR_BOOL   (BDX_BOOL)
#define SC_TM_SCALAR_INT    (BDX_INT)
#define SC_TM_SCALAR_DOUBLE (BDX_DOUBLE)
#define SC_TM_SCALAR_STRING (BDX_STRING)
#define SC_TM_SCALAR_ALL    (SC_TM_SCALAR_BOOL     \
                             | SC_TM_SCALAR_INT    \
                             | SC_TM_SCALAR_DOUBLE \
                             | SC_TM_SCALAR_STRING)

#define SC_TM_ARRAY_BOOL    (BDX_BOOL << 8)
#define SC_TM_ARRAY_INT     (BDX_INT << 8)
#define SC_TM_ARRAY_DOUBLE  (BDX_DOUBLE << 8)
#define SC_TM_ARRAY_STRING  (BDX_STRING << 8)
#define SC_TM_ARRAY_ALL     (SC_TM_ARRAY_BOOL     \
                             | SC_TM_ARRAY_INT    \
                             | SC_TM_ARRAY_DOUBLE \
                             | SC_TM_ARRAY_STRING)

#define SC_TM_VECTOR_BOOL   (BDX_BOOL << 12)
#define SC_TM_VECTOR_INT    (BDX_INT << 12)
#define SC_TM_VECTOR_DOUBLE (BDX_DOUBLE << 12)
#define SC_TM_VECTOR_STRING (BDX_STRING << 12)
#define SC_TM_VECTOR_ALL    (SC_TM_VECTOR_BOOL     \
                             | SC_TM_VECTOR_INT    \
                             | SC_TM_VECTOR_DOUBLE \
                             | SC_TM_VECTOR_STRING)

/* information main keys */
#define SC_INFO_MAIN_CONNECTOR     1
#define SC_INFO_MAIN_INTERPRETER   2

/* information sub keys */
#define SC_INFO_SUB_NAME                  1
#define SC_INFO_SUB_DESCRIPTION           2
#define SC_INFO_SUB_COPYRIGHT             3
#define SC_INFO_SUB_LICENSE               4
#define SC_INFO_SUB_MINORVERSION          5
#define SC_INFO_SUB_MAJORVERSION          6

/* function type-defs */
typedef int (SYSCALL *SC_PROXY_GET_VERSION) (struct _SC_Proxy_Object* object,
					     unsigned long* version);
typedef int (SYSCALL *SC_PROXY_INIT) (struct _SC_Proxy_Object* object,
				      char const* parameters);
typedef int (SYSCALL *SC_PROXY_TERMINATE) (struct _SC_Proxy_Object* object);
typedef int (SYSCALL *SC_PROXY_RETAIN) (struct _SC_Proxy_Object* object);
typedef int (SYSCALL *SC_PROXY_RELEASE) (struct _SC_Proxy_Object* object);

typedef int (SYSCALL *SC_PROXY_SET_SYMBOL) (struct _SC_Proxy_Object* object,
					    char const* symbol,
					    BDX_Data /* const */ * data);
typedef int (SYSCALL *SC_PROXY_GET_SYMBOL) (struct _SC_Proxy_Object* object,
					    char const* symbol,
					    BDX_Data** data);
typedef int (SYSCALL *SC_PROXY_EVALUATE) (struct _SC_Proxy_Object* object,
					  char const* command,
					  BDX_Data** data);
typedef int (SYSCALL *SC_PROXY_EVALUATE_NORETURN) (struct _SC_Proxy_Object* object,
						   char const* command);
typedef int (SYSCALL *SC_PROXY_FREE_DATA_BUFFER) (struct _SC_Proxy_Object* object,
						  BDX_Data* data);
typedef int (SYSCALL *SC_PROXY_QUERY_TYPES) (struct _SC_Proxy_Object* object,
					     long* type_mask);
typedef int (SYSCALL *SC_PROXY_QUERY_OPS) (struct _SC_Proxy_Object* object,
					   long* op_mask);
typedef int (SYSCALL *SC_PROXY_SET_CHARACTERDEVICE)
(
  struct _SC_Proxy_Object* object,
  struct _SC_CharacterDevice* device
);
typedef int (SYSCALL *SC_PROXY_QUERY_INFO) (struct _SC_Proxy_Object* object,
					    long main_key,
					    long sub_key,
					    const char** information);
typedef int (SYSCALL *SC_PROXY_SET_GRAPHICSDEVICE)
(
  struct _SC_Proxy_Object* object,
  struct _SC_GraphicsDevice* device
);

/* character device function typedefs */
typedef int (SYSCALL *SC_CHARACTERDEVICE_GET_VERSION)
(
  struct _SC_CharacterDevice* object,
  unsigned long* version
);

typedef int (SYSCALL *SC_CHARACTERDEVICE_RETAIN)
(
  struct _SC_CharacterDevice* object
);

typedef int (SYSCALL *SC_CHARACTERDEVICE_RELEASE)
(
  struct _SC_CharacterDevice* object
);

typedef int (SYSCALL *SC_CHARACTERDEVICE_WRITE_STRING)
(
  struct _SC_CharacterDevice* object,
  char const* string
);

typedef int (SYSCALL *SC_CHARACTERDEVICE_WRITE_STRING_LEVEL)
(
  struct _SC_CharacterDevice* object,
  char const* string,
  unsigned long level
);

/* graphics device function typedefs */
typedef int (SYSCALL *SC_GRAPHICSDEVICE_GET_VERSION)
(
  struct _SC_GraphicsDevice* object,
  unsigned long* version
);

typedef int (SYSCALL *SC_GRAPHICSDEVICE_RETAIN)
(
  struct _SC_GraphicsDevice* object
);

typedef int (SYSCALL *SC_GRAPHICSDEVICE_RELEASE)
(
  struct _SC_GraphicsDevice* object
);

/* really no data, should not be required */
typedef int (SYSCALL *SC_GRAPHICSDEVICE_OPEN)
(
  struct _SC_GraphicsDevice* object,
  char const* display,
  double w,
  double h,
  double gamma,
  int colormodel,
  int maxcupe
);

/* should not be required */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_CLOSE)
(
  struct _SC_GraphicsDevice* object
);

typedef void (SYSCALL *SC_GRAPHICSDEVICE_ACTIVATE)
(
  struct _SC_GraphicsDevice* object
);

typedef void (SYSCALL *SC_GRAPHICSDEVICE_DEACTIVATE)
(
  struct _SC_GraphicsDevice* object
);

/* 00-06-22 | baier | added color, line type and width */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_LINE)
(
  struct _SC_GraphicsDevice* object,
  double x1,
  double y1,
  double x2,
  double y2,
  int color,
  int line_type,
  double line_width
);

/* 00-06-22 | baier | added line type and width */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_CIRCLE)
(
  struct _SC_GraphicsDevice* object,
  double x,
  double y,
  double r,
  int col,    /* border color */
  int border, /* fill color */
  int line_type,
  double line_width
);

typedef void (SYSCALL *SC_GRAPHICSDEVICE_POLYGON)
(
  struct _SC_GraphicsDevice* object,
  int n,
  double* x,
  double* y,
  int bg,     /* border color */
  int fg      /* fill color */
);

/* 01-01-23 | baier | added "col" parameter */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_POLYLINE)
(
  struct _SC_GraphicsDevice* object,
  int n,
  double* x,
  double* y,
  int col    /* color */
);

/* 00-06-22 | baier | added line type and width */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_RECT)
(
  struct _SC_GraphicsDevice* object,
  double x0,
  double y0,
  double x1,
  double y1,
  int bg,     /* border color */
  int fg,     /* fill color */
  int line_type,
  double line_width
);

/* 00-06-22 | baier | added color, font and size */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_TEXT)
(
  struct _SC_GraphicsDevice* object,
  double x,
  double y,
  char const* string,
  double rot,          /* rotation in degrees */
  double hadj,         /* horizontal adjustment */
  int color,
  int font,            /* 0-31, one of the predefined fonts */
  int size             /* 8-64, point size */
);

typedef void (SYSCALL *SC_GRAPHICSDEVICE_CLIP)
(
  struct _SC_GraphicsDevice* object,
  double x0,
  double x1,
  double y0,
  double y1
);

/* to be removed. refresh of width and heigth should be done on "newpage" */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_RESIZE)
(
  struct _SC_GraphicsDevice* object
);

/* to be removed */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_HOLD)
(
  struct _SC_GraphicsDevice* object
);

/* should clear display AND refresh coordinates */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_NEWPAGE)
(
  struct _SC_GraphicsDevice* object
);

typedef int (SYSCALL *SC_GRAPHICSDEVICE_LOCATOR)
(
  struct _SC_GraphicsDevice* object,
  double* x,
  double* y
);

typedef void (SYSCALL *SC_GRAPHICSDEVICE_MODE)
(
  struct _SC_GraphicsDevice* object,
  int mode   /* 1 when starting drawing, 0 when stopping */
);

/* 00-06-22 | baier | added font and size parameters */
typedef double (SYSCALL *SC_GRAPHICSDEVICE_STRWIDTH)
(
  struct _SC_GraphicsDevice* object,
  char const* string,
  int font,            /* 0-31, one of the predefined fonts */
  int size             /* 8-64, point size */
);

/* 00-06-22 | baier | added font and size parameters */
typedef void (SYSCALL *SC_GRAPHICSDEVICE_METRICINFO)
(
  struct _SC_GraphicsDevice* object,
  int character,
  double* ascent,
  double* descent,
  double* width,
  int font,            /* 0-31, one of the predefined fonts */
  int size             /* 8-64, point size */
);


/*
 * interface version information:
 *
 * 1 ... obsolete first interface
 * 2 ... get_version, set_symbol, evaluate_noreturn, evaluate
 * 3 ... added character devices for output, errors and tracing, info strings
 * 4 ... pass parameter string in init() function
 * 5 ... simple graphics device interface
 */
#define SC_PROXY_INTERFACE_VERSION 5

/*
 * interface version information:
 *
 * 1 ... first version
 */
#define SC_CHARACTERDEVICE_VERSION 1

/*
 * interface version information:
 *
 * 1 ... first version
 */
#define SC_GRAPHICSDEVICE_VERSION 1

/* used for communication between the COM/CORBA server and R */
typedef struct _SC_Proxy_Object_Vtbl
{
  /* get interpreter version */
  SC_PROXY_GET_VERSION get_version;

  /* initialize the interpreter */
  SC_PROXY_INIT init;
  /* terminate the interpreter */
  SC_PROXY_TERMINATE terminate;

  /* increase the reference count to this interface object */
  SC_PROXY_RETAIN retain;
  /* decrease the reference count to this interface object, object will be */
  /* freed when count reaches 0 */
  SC_PROXY_RELEASE release;

  /* set a symbol in the interpreter's global namespace to the BDX data passed */
  SC_PROXY_SET_SYMBOL set_symbol;
  /* return a symbol's value from the interpreter's global namespace */
  SC_PROXY_GET_SYMBOL get_symbol;

  /* evaluate an expression, return result in a BDX data buffer (synchronously) */
  SC_PROXY_EVALUATE evaluate;
  /* evaluate an expression, do not return a result (synchronously) */
  SC_PROXY_EVALUATE_NORETURN evaluate_noreturn;

  /* return information about BDX data types supported by this interface and */
  /* the interpreter */
  SC_PROXY_QUERY_TYPES query_supported_types;
  /* return information about available functionality in this interface and */
  /* the interpreter */
  SC_PROXY_QUERY_OPS query_supported_operations;

  /* free a BDX data buffer allocated by one of this interface's functions */
  SC_PROXY_FREE_DATA_BUFFER free_data_buffer;

  /* set output, error and tracing devices */
  SC_PROXY_SET_CHARACTERDEVICE set_output_device;

  /* retrieve information about interface and interpreter */
  SC_PROXY_QUERY_INFO query_info;

  /* add and remove a graphics device */
  SC_PROXY_SET_GRAPHICSDEVICE set_graphics_device;
} SC_Proxy_Object_Vtbl;

/* character device used for output, (textual) error messages and traces */
typedef struct _SC_CharacterDevice_Vtbl
{
  /* get character device version */
  SC_CHARACTERDEVICE_GET_VERSION get_version;

  /* increase the reference count to this interface object */
  SC_CHARACTERDEVICE_RETAIN retain;
  /* decrease the reference count to this interface object, object will be */
  /* freed when count reaches 0 */
  SC_CHARACTERDEVICE_RELEASE release;

  /* write a string to the output device */
  SC_CHARACTERDEVICE_WRITE_STRING write_string;

  /* write a string to the output device. the string is echoed if the passed */
  /* level is less or equal to the current output level of the device (used */
  /* for conditional output) */
  SC_CHARACTERDEVICE_WRITE_STRING_LEVEL write_string_level;
} SC_CharacterDevice_Vtbl;

/* graphics device */
typedef struct _SC_GraphicsDevice_Vtbl
{
  /* get graphics device version */
  SC_GRAPHICSDEVICE_GET_VERSION get_version;

  /* increase the reference count to this interface object */
  SC_GRAPHICSDEVICE_RETAIN retain;
  /* decrease the reference count to this interface object, object will be */
  /* freed when count reaches 0 */
  SC_GRAPHICSDEVICE_RELEASE release;

  /* interface-specific functions following here! */
  SC_GRAPHICSDEVICE_OPEN open;
  SC_GRAPHICSDEVICE_CLOSE close;

  SC_GRAPHICSDEVICE_ACTIVATE activate;
  SC_GRAPHICSDEVICE_DEACTIVATE deactivate;

  SC_GRAPHICSDEVICE_LINE line;
  SC_GRAPHICSDEVICE_CIRCLE circle;
  SC_GRAPHICSDEVICE_POLYGON polygon;
  SC_GRAPHICSDEVICE_POLYLINE polyline;
  SC_GRAPHICSDEVICE_RECT rect;
  SC_GRAPHICSDEVICE_TEXT text;

  SC_GRAPHICSDEVICE_CLIP clip;
  SC_GRAPHICSDEVICE_RESIZE resize;

  SC_GRAPHICSDEVICE_HOLD hold;
  SC_GRAPHICSDEVICE_NEWPAGE newpage;

  SC_GRAPHICSDEVICE_LOCATOR locator;
  SC_GRAPHICSDEVICE_MODE mode;
  SC_GRAPHICSDEVICE_STRWIDTH strwidth;
  SC_GRAPHICSDEVICE_METRICINFO metricinfo;
} SC_GraphicsDevice_Vtbl;

/* abstract data type (implementation adds data) */
typedef struct _SC_Proxy_Object
{
  SC_Proxy_Object_Vtbl* vtbl;
} SC_Proxy_Object;

typedef struct _SC_CharacterDevice
{
  SC_CharacterDevice_Vtbl* vtbl;
} SC_CharacterDevice;

typedef struct _SC_GraphicsDevice
{
  SC_GraphicsDevice_Vtbl* vtbl;
} SC_GraphicsDevice;

/* entry point: retrieve a proxy object with a given version */
typedef int (SYSCALL* SC_PROXY_GET_OBJECT) (SC_Proxy_Object**,unsigned long);

#ifdef __cplusplus
}
#endif

#endif
