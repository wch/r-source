/*
 *	WEExtraHooks.h
 */


#include "WASTE.h"

enum
{
	kInvisiblesOldDrawTextProcTag	=	'icDT' ,
	kInvisiblesColorRedGreenTag 	=	'icRG' ,
	kInvisiblesColorBlueTag			=	'icB_'
} ;

enum
{
	kEOL							=	0x0D
} ;

#ifdef __cplusplus
inline Boolean IsColorPort ( GrafPtr port )
{
	return ( ( CGrafPtr ) port ) -> portVersion < 0 ;
}
#else
#define IsColorPort(port)	(((CGrafPtr) (port))->portVersion < 0)
#endif

#ifdef __cplusplus
extern "C" {
#endif

pascal StyledLineBreakCode _WENoWrapLineBreak ( Ptr, SInt32, SInt32, SInt32, Fixed *, SInt32 *, WEReference ) ;
pascal void _WEShowInvisiblesDrawText ( Ptr, SInt32, Fixed, JustStyleCode, WEReference ) ;

#ifdef __cplusplus
}
#endif
