/*
 *	WASTE Demo Project:
 *	Carbon Compatibility
 *
 *	Copyright © 1993-2000 Marco Piovanelli
 *	All Rights Reserved
 *
 *	C port by John C. Daub
 *
 *	<mailto:waste@merzwaren.com>
 *	<http://www.merzwaren.com/waste/>
 */

#include "CarbonCompatibility.h"

#if ! TARGET_API_MAC_CARBON

pascal Size AEGetDescDataSize (	const AEDesc *		inAEDesc )
{
	return ( inAEDesc->dataHandle == nil ) ? 0 : GetHandleSize ( inAEDesc->dataHandle ) ;
}

pascal OSErr AEGetDescData (	const AEDesc *		inAEDesc,
								void *				outData,
								Size				inMaximumSize )
{
	/*	do nothing if we were fed a null descriptor */
	if ( inAEDesc -> dataHandle != nil )
	{
		/*	get data size */
		Size dataSize = GetHandleSize ( inAEDesc -> dataHandle ) ;
		if ( inMaximumSize > dataSize )
		{
			inMaximumSize = dataSize ;
		}

		/*	copy it */
		BlockMoveData ( * inAEDesc -> dataHandle, outData, inMaximumSize ) ;
	}

	return noErr ;
}

#endif /* ! TARGET_API_MAC_CARBON */
