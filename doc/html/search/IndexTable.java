/*============================================================================

  Project: Simple JAVA Search Engine for Keyword Search
  
  JAVA Source file for the class IndexTable
  
  COPYRIGHT (C), 1998-2000, Thomas Baier, R Core Development Team

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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

  
  
  $Rev$

  $LastChangedDate$
  
  $Author$

============================================================================*/


import java.util.Vector;
import java.util.Enumeration;


public class IndexTable extends Vector
{

  public Vector search (String key, boolean searchDesc,
			boolean searchKeywords, boolean searchAliases)
  {
    Vector returnValue = new Vector ();
    Enumeration cursor = elements ();
    
    while (cursor.hasMoreElements ()) {
      IndexEntry entry = (IndexEntry) cursor.nextElement ();
      
      if (entry.matches (key, searchDesc,
			 searchKeywords, searchAliases)){
	returnValue.addElement (entry);
      }
    }
    if (!returnValue.isEmpty ()) {
      return returnValue;
    }
    
    return null;
  }
}


// Local Variables:
// mode: Java
// mode: font-lock
// End:
