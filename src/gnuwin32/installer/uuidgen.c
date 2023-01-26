#define _OLEAUT32_

#include <stdio.h>
#include <stdlib.h>
#include <objbase.h>
#include <unknwn.h>

int main (int argc, char* argv[]) 
{
    GUID guid;
    LPOLESTR wstrGUID;
    char *strGUID;
    int count, i, res;

    if (argc != 2) {
	fprintf (stderr, "SYNTAX: UUIDGEN <number-of-GUIDs-to-generate>\n");
	return 1;
    }
    count = atoi (argv[1]);
    for (i = 0; i < count; i++) {
	CoCreateGuid (&guid);
	StringFromCLSID (&guid, &wstrGUID);
	res = WideCharToMultiByte (CP_ACP, 0, wstrGUID, -1, NULL, 0, NULL, NULL);
	if (res > 0) {
	    strGUID = (char *) malloc(res);
	    if (!strGUID) {
		fprintf(stderr, "Out of memory.");
		return 2;
	    }
	    WideCharToMultiByte (CP_ACP, 0, wstrGUID, -1, strGUID, res, NULL, NULL);
	    strGUID[strlen(strGUID)-1] = '\0'; /* erase } */
	    printf ("%s\n", strGUID+1);
	    free(strGUID);
	}
	CoTaskMemFree (wstrGUID);
    }
    return 0;
}
