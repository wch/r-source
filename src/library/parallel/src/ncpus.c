/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011   The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include <R.h>
#include "parallel.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>

/* Based on example at
   http://msdn.microsoft.com/en-us/library/ms683194%28v=VS.85%29.aspx
*/

#ifndef _W64
# include "glpi.h"
#endif

typedef BOOL 
(WINAPI *LPFN_GLPI)(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION, PDWORD);

// Helper function to count set bits in the processor mask.
static DWORD CountSetBits(ULONG_PTR bitMask)
{
    DWORD LSHIFT = sizeof(ULONG_PTR)*8 - 1;
    DWORD bitSetCount = 0;
    ULONG_PTR bitTest = (ULONG_PTR)1 << LSHIFT;    
    DWORD i;
    
    for (i = 0; i <= LSHIFT; ++i) {
        bitSetCount += ((bitMask & bitTest)?1:0);
        bitTest/=2;
    }
    return bitSetCount;
}

SEXP ncpus(SEXP virtual)
{
    // int virt = asLogical(virtual);

    SEXP ans = allocVector(INTSXP, 2);
    PROTECT(ans);
    int *ians = INTEGER(ans);
    for(int i = 1; i < 2; i++) ians[i] = NA_INTEGER;

    LPFN_GLPI glpi;
    BOOL done = FALSE;
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION buffer = NULL;
    PSYSTEM_LOGICAL_PROCESSOR_INFORMATION ptr = NULL;
    DWORD returnLength = 0;
    DWORD logicalProcessorCount = 0;
    DWORD numaNodeCount = 0;
    DWORD processorCoreCount = 0;
    DWORD byteOffset = 0;
    /* XP SP3 and later, but reports physical CPUs before Vista */
    glpi = (LPFN_GLPI) 
	GetProcAddress(GetModuleHandle(TEXT("kernel32")),
		       "GetLogicalProcessorInformation");
    if (NULL == glpi) {
	warning("GetLogicalProcessorInformation is not supported on this OS.");
        return ans;
    }

    while (!done) {
        DWORD rc = glpi(buffer, &returnLength);
        if (rc == FALSE) {
            if (GetLastError() == ERROR_INSUFFICIENT_BUFFER) {
                if (buffer) free(buffer);
                buffer = (PSYSTEM_LOGICAL_PROCESSOR_INFORMATION) malloc(returnLength);
                if (!buffer) error("allocation failure");
            } else error("in reading processor information, probable cause: %d", GetLastError());
        } else done = TRUE;
    }

    ptr = buffer;

    while (byteOffset + sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION) <= 
	   returnLength) {
        switch (ptr->Relationship) {
        case RelationNumaNode:
            // Non-NUMA systems report a single record of this type.
            numaNodeCount++;
            break;

        case RelationProcessorCore:
            processorCoreCount++;
            // A hyperthreaded core supplies more than one logical processor.
            logicalProcessorCount += CountSetBits(ptr->ProcessorMask);
            break;

        case RelationCache:
            // Cache data is in ptr->Cache, one CACHE_DESCRIPTOR structure for each cache. 
            break;

        default:
            break;
        }

        byteOffset += sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION);
        ptr++;
    }

    ians[0] = processorCoreCount;
    ians[1] = logicalProcessorCount;
    free(buffer);
    UNPROTECT(1);
    
    return ans;
}
