/*
 *  BDX: Binary Data eXchange format library
 *  Copyright (C) 1999 Thomas Baier
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
 *  $Id: bdx.c,v 1.2.4.1 1999/12/09 16:47:17 ripley Exp $
 */

#include <stdlib.h>
#include <assert.h>
#include "bdx.h"

void bdx_free (BDX_Data* data)
{
  if (data == NULL)
    {
      return;
    }

  // even for scalars, the dimensions are correctly initialized (just a 1
  // dimension structure with just one element)
  assert (data != NULL);
  assert (data->dimensions != NULL);
  assert (data->raw_data != NULL);

  // is it a character array/vector/scalar? if yes, release the data
  if ((data->type & BDX_SMASK) == BDX_STRING)
    {
      // treat the data as a 1-dimensional array with |dim1|*|dim2|*...
      // elements
      int total_size = 1;
      int i;

      for (i = 0;i < data->dim_count;i++)
	{
	  total_size *= data->dimensions[i];
	}

      for (i = 0;i < total_size;i++)
	{
	  if (data->raw_data[i].string_value != NULL)
	    {
	      free (data->raw_data[i].string_value);
	    }
	}
    }

  // free data pointers, dimension data and the data block itself
  free (data->raw_data);
  free (data->dimensions);
  free (data);
}
