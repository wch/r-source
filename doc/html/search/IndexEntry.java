/*==============================================================================

  Project: Simple JAVA Search Engine for Keyword Search
  
  JAVA Source file for the class IndexEntry
  
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
  
  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/IndexEntry.java,v $
  
  $Revision: 1.3 $

  $Date: 1999/08/10 09:56:03 $
  
  $Author: ripley $

==============================================================================*/


/* -------------------------------- Imports --------------------------------- */

import java.lang.Object;
import java.lang.String;
import java.applet.AppletContext;
import java.net.URL;
import java.net.MalformedURLException;



/*==============================================================================
                          Interface of class IndexEntry
==============================================================================*/

/*------------------------------------------------------------------------------
  CLASS:    IndexEntry
  SUPER:    Object
  CONF. TO: 
  PURPOSE:  
  NOTES:    

  HISTORY:  98-04-26: created
------------------------------------------------------------------------------*/
public class IndexEntry extends Object
{
  /*============================================================================
                                Public methods
  ============================================================================*/

  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   constructor
  	      
    NOTES:    
  
    PARAMS:   
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-04-26: created
               98-05-08: adapted for new index file format
	       98-05-10: added trace
  ----------------------------------------------------------------------------*/
  public IndexEntry (String title,String keywords,String desc,String url)
  {
    iTitle = title;
    iKey = keywords;
    iDescription = desc;
    iURL = url;

    // trace here
    Tracer.write ("Created IndexEntry. Title = \"" +
		  title + "\", Keywords = \"" + keywords + "\"\n");
    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   check for a matching search string
  	      
    NOTES:     case-insensitive compare
  
    PARAMS:    
    THROWS:    
    RETURNS:   boolean: true if matches
  
    HISTORY:   98-04-26: created
               98-05-15: new parameter mode, check description, too
  ----------------------------------------------------------------------------*/
  public boolean matches (String aString,int mode)
  {
    // case-insensitive substring!
    if ((mode & IndexTable.cSearchDescription) == IndexTable.cSearchDescription) {
      if ((iKey.toUpperCase ().indexOf (aString.toUpperCase ()) > -1)
	  || (iDescription.toUpperCase ().indexOf (aString.toUpperCase ()) > -1)) {
	return true;
      }
    } else {
      if (iKey.toUpperCase ().indexOf (aString.toUpperCase ()) > -1) {
	return true;
      }
    }
    return false;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   return the description text
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   String: the description
  
    HISTORY:   98-04-26: created
  ----------------------------------------------------------------------------*/
  public String getDescription ()
  {
    return iDescription;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   return the title string
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-08: created
  ----------------------------------------------------------------------------*/
  public String getTitle ()
  {
    return iTitle;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   return the URL as a string
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   String: the URL
  
    HISTORY:   98-04-26: created
  ----------------------------------------------------------------------------*/
  public String getURL ()
  {
    return iURL;
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

  private String iTitle;
  private String iKey;
  private String iDescription;
  private String iURL;


  /*============================================================================
                                Static Data
  ============================================================================*/
}


/*==============================================================================

  HISTORY:
  
  $Log: IndexEntry.java,v $
  Revision 1.3  1999/08/10 09:56:03  ripley
  change FSF address in copyrights
  add some copyrights in src/gnome and elsewhere

  Revision 1.2  1999/03/04 17:15:18  leisch
  various bugfixes

  Revision 1.1.4.1  1999/03/02 15:19:55  leisch
  search used only kewords, no titles

  Revision 1.4  1998/05/15 22:07:56  baier
  also allow search in description

  Revision 1.3  1998/05/10 02:43:24  baier
  now a simple data-class

  Revision 1.2  1998/04/26 22:35:21  baier
  some comments

  Revision 1.1  1998/04/26 21:43:09  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
