/*==============================================================================

  Project: Simple JAVA Search Engine for Keyword Search
  
  JAVA Source file for the class SearchEngine
  
  COPYRIGHT (C), 1998, Thomas Baier
  ALL RIGHTS RESERVED.
  
  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/SearchEngine.java,v $
  
  $Revision: 1.1 $

  $Date: 1998/05/15 10:38:10 $
  
  $Author: leisch $

==============================================================================*/


/* -------------------------------- Imports --------------------------------- */

import java.applet.*;
import java.awt.*;
import java.net.*;
import java.io.*;
import java.util.*;


/*==============================================================================
                          Interface of class SearchEngine
==============================================================================*/

/*------------------------------------------------------------------------------
  CLASS:    SearchEngine
  SUPER:    Applet
  CONF. TO: 
  PURPOSE:  
  NOTES:    

  HISTORY:  98-04-26: created 
------------------------------------------------------------------------------*/
public class SearchEngine extends Applet
{
  /*============================================================================
                                Public methods
  ============================================================================*/

  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   default constructor
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-04-26: created
               98-05-03: now reads the index file
  ----------------------------------------------------------------------------*/
  public SearchEngine ()
  {
    iIndexTable = null;
    iSearchTerm = null;

    Tracer.write ("SearchEngine initializing\n");

    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   return general information about the applet
  	       
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   String: the information
  
    HISTORY:   98-04-26: created
  ----------------------------------------------------------------------------*/
  public String getAppletInfo ()
  {
    return "Name: SearchEngine\r\n" +
      "Author: Thomas Baier\r\n" +
      "(C) 1998 Thomas Baier, ALL RIGHTS RESERVED";
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   perform initialization
  	      
    NOTES:     creates the controls
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-04-26: created
               98-05-03: now a do-nothing
  ----------------------------------------------------------------------------*/
  public void init ()
  {
    resize(640, 240);

    // get the name of the index file
    String indexName = getParameter (cIndexKeyword);
    String searchTerm = getParameter (cSearchKeyword);

    Tracer.write ("Index file is \"" + indexName + "\"\n");
    Tracer.write ("Search term is \"" + searchTerm + "\"\n");

    // use a default index file if none specified
    if (indexName == null) {
      indexName = cIndexFile;
    }

    iSearchTerm = searchTerm;

    /*
     * examine the URL to get the search term...
     *
     * if the URL ends with ?SEARCHTERM=xxxxx we know, xxxxx is the search term
     */
    {
      URL url = getDocumentBase ();
      String urlString = url.toString ();
      int index = urlString.indexOf ("?" + cSearchKeyword + "=");

      Tracer.write ("URL is \"" + urlString + "\"\n");

      // if found, take the rest as the search string
      if (index >= 0) {
	iSearchTerm =
	  urlString.substring (index + 2 + cSearchKeyword.length ());
	Tracer.write ("found search term \"" + iSearchTerm + "\" in URL\n");
      }
    }

    readIndexFile (indexName);

    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   perform "destructor" code
  	      
    NOTES:     not required here
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-04-26: created
  ----------------------------------------------------------------------------*/
  public void destroy ()
  {
    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   perform painting of the applet
  	      
    NOTES:     not required for our applet, controls do everything
  
    PARAMS:    Graphics g: the graphics context to draw on
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-04-26: created
  ----------------------------------------------------------------------------*/
  public void paint (Graphics g)
  {
  
    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   perform startup code everytime visiting the applet
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-04-26: created
               98-05-10: start the tracer
  ----------------------------------------------------------------------------*/
  public void start ()
  {
    Tracer.start ();
    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   perform cleanup evertime the applet "loses" the focus
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-04-26: created
               98-05-10: stop the tracer
  ----------------------------------------------------------------------------*/
  public void stop ()
  {
    Tracer.stop ();
    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   perform the search and return the search results as a string
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-03: created
               98-05-08: new format for output
	       98-05-09: added trace
	       98-05-10: now a front-end for search()
  ----------------------------------------------------------------------------*/
  public String search (String key)
  {
    iSearchTerm = key;

    return internalSearch ();
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   perform the search (back-end)
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-10: created
  ----------------------------------------------------------------------------*/
  public String internalSearch ()
  {
    Tracer.write ("Search for \"" + iSearchTerm + "\" started");

    Vector foundItems = null;

    if (iSearchTerm != null) {
      foundItems = iIndexTable.search (iSearchTerm);
    } else {
      foundItems = null;
    }

    String result = null;

    // if nothing found, return a special string
    if (foundItems == null) {
      result = "No matches for <b>\"" + 
	iSearchTerm +
	"\"</b> have been found!<hr>";
    } else {
      Enumeration cursor = foundItems.elements ();

      result =
	"The search string was <b>\"" +
	iSearchTerm +
	"<b>\"" +
	"<hr>" +
	"<dl>";

      while (cursor.hasMoreElements ()) {
	IndexEntry entry = (IndexEntry) cursor.nextElement ();

	/*
	 * the format for every entry is
	 *
	 * title
	 *   description
	 */

	result +=
	  "<dt><a href=\"" +
	  entry.getURL () +
	  "\">" +
	  entry.getTitle () +
	  "</a></dt>\n";
	result += "<dd>" + entry.getDescription () + "</dd>\n";
      }

      result += "</dl>";
    }

    return result;
  }


  /*============================================================================
                              Protected methods
  ============================================================================*/

  /*============================================================================
                               Private methods
  ============================================================================*/

  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   read the index file
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-04-26: created
               98-05-08: now use an IndexStream
	       98-05-10: also use prefix and suffix, build URL from first key
  ----------------------------------------------------------------------------*/
  private void readIndexFile (String idxFile)
  {
    // create the index table
    iIndexTable = new IndexTable ();

    URL baseURL = getCodeBase ();

    // get the index file and parse its contents
    try {
      URL idxFileURL = new URL (baseURL,idxFile);

      // get an IndexStream object for ease of parsing
      IndexStream idxStream = new IndexStream (idxFileURL);

      // now start parsing...
      
      /*
       * An entry consists of a title, keywords, an URL and a description.
       * everything else is ignored. Every entry starts with the keyword
       * "Entry" (case is ignored)
       *
       * must-have entries are "Entry" and "Keywords"
       */
      String entry = null;
      String keywords = null;
      String url = null;
      String description = "";
      String prefix = "";
      String suffix = "";

      Value value = idxStream.popEntry ();

      while (value != null) {
	// parse the value now
	if (value.getKey ().equalsIgnoreCase ("entry")) {
	  // if a new entry is about to start, add the current one
	  addEntry (entry,keywords,description,url,prefix,suffix);

	  entry = value.getValue ();
	  keywords = null;
	  url = null;
	  description = "";
	} else if (value.getKey ().equalsIgnoreCase ("keywords")) {
	  keywords = value.getValue ();
	} else if (value.getKey ().equalsIgnoreCase ("url")) {
	  // use prefix and suffix
	  url = prefix + value.getValue () + suffix;
	} else if (value.getKey ().equalsIgnoreCase ("description")) {
	  description = value.getValue ();
	} else if (value.getKey ().equalsIgnoreCase ("prefix")) {
	  prefix = value.getValue ();
	  Tracer.write ("using new URL prefix \"" + prefix + "\"\n");
	} else if (value.getKey ().equalsIgnoreCase ("suffix")) {
	  suffix = value.getValue ();
	  Tracer.write ("using new URL suffix \"" + suffix + "\"\n");
	}
	value = idxStream.popEntry ();
      }

      // the final entry just read
      addEntry (entry,keywords,description,url,prefix,suffix);
    } catch (MalformedURLException exc) {
      // an error occured while reading...
    }

    return;
  }

  
  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-10: created
  ----------------------------------------------------------------------------*/
  private void addEntry (String entry,String keywords,
			 String description,String url,
			 String prefix,String suffix)
  {
    // the entry must be set
    if (entry == null) {
      return;
    }

    // the keywords must be set, else ignore it
    if (keywords != null) {
      if (url == null) {
	// if the URL is empty, construct one following the rule:
	// URL = prefix + first keyword + suffix
	int endOfFirstKeyword = keywords.indexOf (" ");
	
	// because we have trimmed the string, the first character must
	// not be a blank
	if (endOfFirstKeyword >= 0) {
	  url = keywords.substring (0,endOfFirstKeyword);
	  Tracer.write ("constructing URL, keywords=\"" +
			keywords + "\"" +
			"using \"" + 
			url +
			"\" (results in \"" +
			prefix + url + suffix + "\")\n");
	} else {
	  // just a single keyword
	  url = keywords;
	  Tracer.write ("constructing URL, using keyword \"" + 
			url +
			"\" (results in \"" +
			prefix + url + suffix + "\")\n");
	}
	// add prefix and suffix
	url = prefix + url + suffix;
      }
      IndexEntry idxEntry =
	new IndexEntry (entry,keywords,description,url);
      iIndexTable.addElement (idxEntry);
    }

    return;
  }


  /*============================================================================
                             Instance Variables
  ============================================================================*/
  private IndexTable iIndexTable;
  private String     iSearchTerm;


  /*============================================================================
                                Static Data
  ============================================================================*/

  private static final String cIndexFile = "index.txt";
  private static final String cIndexKeyword = "INDEXFILE";
  private static final String cSearchKeyword = "SEARCHTERM";
}

/*==============================================================================

  HISTORY:
  
  $Log: SearchEngine.java,v $
  Revision 1.1  1998/05/15 10:38:10  leisch
  New: Search Engine

  Revision 1.4  1998/05/10 22:56:53  baier
  internal search function, parameter expansion

  Revision 1.3  1998/05/10 02:44:32  baier
  traces, output in HTML via JavaScript, new index generation

  Revision 1.2  1998/04/26 22:36:34  baier
  documentation changes



  Revision 1.1  1998/04/26 21:32:54  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
