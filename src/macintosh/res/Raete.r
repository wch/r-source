/* This file is adapted from MacCVS code for AEvents.
*/
#define SystemSevenOrLater 1

#include "Types.r"
#include "BalloonTypes.r"
#include "AEUserTermTypes.r"
#include "AEObjects.r"

#define	kCMDEventClass	'DCMD'
#define	kCMDEvent    	'DCMD'
#define	kCMDEventClassEx	'xCMD'
#define	kCMDEventEx    	'xCMD'

resource 'aete' (0, "R AppleEvent Suites") {
	0x01, 0x00, english, roman,
	{
		"required Suite", "Events supported by all applications", 'reqd', 1, 1,
		{},
		{},
		{},
		{},
		"R Suite", "Custom events", '0FFF', 1, 1,
		{
			"Do Command", "Execute an R command", kCMDEventClassEx, kCMDEventEx,
			'null', "", replyOptional, singleItem, notEnumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, 
			'TEXT', "Command to execute", directParamRequired, listOfItems, notEnumerated,
			changesState, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, 
			{
				"Mode", 'MODE', 'MODE', "Mode (AE, File).", optional, singleItem, enumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, 
				"Environment", 'ENVT', 'TEXT', "Environment variables.", optional, listOfItems, notEnumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved,
				"Filename", 'FILE', 'TEXT', "Output file path.", optional, singleItem, notEnumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, 
				"Pathway", 'SPWD', 'TEXT', "Starting pathway.", optional, singleItem, notEnumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved,
				"NoLineBuffer", 'LBUF', 'bool', "if true, send each result line as separate AE.", optional, singleItem, notEnumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, 
			},
			"cmd", "Execute an R command", kCMDEventClass, kCMDEvent,
			'null', "", replyOptional, singleItem, notEnumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, 
			'TEXT', "Command to execute", directParamRequired, listOfItems, notEnumerated,
			changesState, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, 
			{
				"Environment", 'ENVT', 'TEXT', "Environment variables.", optional, listOfItems, notEnumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved,
				"Pathway", 'SPWD', 'TEXT', "Starting pathway.", optional, singleItem, notEnumerated, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved, reserved,
			}
		},
		{},
		{},
		{
			'MODE',
			{
				"AE", 'TOAE', "Redirect standard output to apple events",
				"File", 'FILE', "Redirect standard output to a file",
			}
		}
	}
};

