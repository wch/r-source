/*==============================================================================

  Project: Simple JAVA Search Engine for Keyword Search
  
  JAVA Source file for the class IndexEntry
  
  COPYRIGHT (C), 1998, Thomas Baier
  ALL RIGHTS RESERVED.
  
  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/IndexEntry.java,v $
  
  $Revision: 1.1 $

  $Date: 1998/05/15 10:38:09 $
  
  $Author: leisch $

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
  ----------------------------------------------------------------------------*/
  public boolean matches (String aString)
  {
    // case-insensitive substring!
    if (iKey.toUpperCase ().indexOf (aString.toUpperCase ()) > -1) {
      return true;
    } else {
      return false;
    }
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
  Revision 1.1  1998/05/15 10:38:09  leisch
  New: Search Engine

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
