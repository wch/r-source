/*==============================================================================

  Project: Simple JAVA Search Engine for Keyword Search
  
  JAVA Source file for the class IndexTable
  
  COPYRIGHT (C), 1998, Thomas Baier

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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  
  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/IndexTable.java,v $
  
  $Revision: 1.4 $


  $Date: 1999/08/10 09:56:03 $
  
  $Author: ripley $

==============================================================================*/


/* -------------------------------- Imports --------------------------------- */

import java.util.Vector;
import java.util.Enumeration;



/*==============================================================================
                          Interface of class IndexTable
==============================================================================*/

/*------------------------------------------------------------------------------
  CLASS:    IndexTable
  SUPER:    Vector
  CONF. TO: 
  PURPOSE:  
  NOTES:    

  HISTORY:  98-04-26: created
            98-05-15: new static members for search-mode
------------------------------------------------------------------------------*/
public class IndexTable extends Vector
{
  /*============================================================================
                                Public methods
  ============================================================================*/

  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   build a vector of found entries
  	      
    NOTES:     convenience function to extend Vector super class
  
    PARAMS:    String key: the search string
    THROWS:    
    RETURNS:   Vector: a vector of found entries or null if no matches found
  
    HISTORY:   98-04-26: created
               98-05-15: new parameter for search mode
  ----------------------------------------------------------------------------*/
  public Vector search (String key,int mode)
  {
    Vector returnValue = new Vector ();
    Enumeration cursor = elements ();

    while (cursor.hasMoreElements ()) {
      IndexEntry entry = (IndexEntry) cursor.nextElement ();
      
      if (entry.matches (key,mode)) {
	returnValue.addElement (entry);
      }
    }
    if (!returnValue.isEmpty ()) {
      return returnValue;
    }
    
    return null;
  }


  /*============================================================================
                              Protected methods
  ============================================================================*/

  /*============================================================================
                               Private methods
  ============================================================================*/

  /*============================================================================
                             Instance Variables
  ============================================================================*/

  /*============================================================================
                                Static Data
  ============================================================================*/
  public static final int cSearchDescription = 0x00000001;
}


/*==============================================================================

  HISTORY:
  
  $Log: IndexTable.java,v $
  Revision 1.4  1999/08/10 09:56:03  ripley
  change FSF address in copyrights
  add some copyrights in src/gnome and elsewhere

  Revision 1.3  1999/03/05 19:18:38  pd
  branch update


  Revision 1.2  1999/03/04 17:15:18  leisch
  various bugfixes

  Revision 1.1.4.1  1999/03/02 15:19:56  leisch
  search used only kewords, no titles

  Revision 1.2  1998/05/15 22:09:15  baier
  support searching in description

  Revision 1.1  1998/04/26 21:46:51  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
