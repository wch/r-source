/*==============================================================================

  Project: Simple JAVA Search Engine for Keyword Search
  
  JAVA Source file for the class IndexTable
  
  COPYRIGHT (C), 1998, Thomas Baier
  ALL RIGHTS RESERVED.
  
  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/IndexTable.java,v $
  
  $Revision: 1.1 $

  $Date: 1998/05/15 10:38:10 $
  
  $Author: leisch $

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
  ----------------------------------------------------------------------------*/
  public Vector search (String key)
  {
    Vector returnValue = new Vector ();
    Enumeration cursor = elements ();

    while (cursor.hasMoreElements ()) {
      IndexEntry entry = (IndexEntry) cursor.nextElement ();
      
      if (entry.matches (key)) {
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
}


/*==============================================================================

  HISTORY:
  
  $Log: IndexTable.java,v $
  Revision 1.1  1998/05/15 10:38:10  leisch
  New: Search Engine

  Revision 1.1  1998/04/26 21:46:51  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
