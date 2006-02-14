/*============================================================================

  Project: Simple JAVA Search Engine for Keyword Search
  
  JAVA Source file for the class SearchEngine
  
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


import java.applet.*;
import java.awt.*;
import java.net.*;
import java.io.*;
import java.util.*;


public class SearchEngine extends Applet
{
  public SearchEngine ()
  {
    iIndexTable = null;
    iSearchTerm = null;
    
    Tracer.write ("SearchEngine initializing\n");
    
    return;
  }
  
  
  public String getAppletInfo ()
  {
    return "Name: SearchEngine\r\n" +
      "Author: Thomas Baier\r\n" +
      "(C) 1998-2000 Thomas Baier, R Core Development Team";
  }
  
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


  public void destroy ()
  {
    return;
  }


  public void paint (Graphics g)
  {
  
    return;
  }


  public void start ()
  {
    Tracer.start ();
    return;
  }
  
  
  public void stop ()
  {
    Tracer.stop ();
    return;
  }
  
  /* perform the search and return result as string */
     
  public String search (String key, boolean searchDesc,
			boolean searchKeywords, boolean searchAliases)
  {
    iSearchTerm = key;
    
    Tracer.write ("Search for \"" + iSearchTerm + "\" started\n");

    if(searchDesc){
      Tracer.write("Searching in Descriptions\n");
    }

    if(searchKeywords){
      Tracer.write("Searching in Keywords\n");
    }

    if(searchAliases){
      Tracer.write("Searching in Aliases\n");
    }

    Vector foundItems = null;

    if (iSearchTerm != null) {
      foundItems = iIndexTable.search (iSearchTerm, searchDesc,
				       searchKeywords, searchAliases);
    }
    else {
      foundItems = null;
    }

    String result = null;

    // if nothing found, return a special string
    if (foundItems == null) {
      result = "No matches for <b>\"" + 
	iSearchTerm +
	"\"</b> have been found!<hr>";
    }
    else {
      
      Enumeration cursor = foundItems.elements ();

      result =
	"The search string was <b>\"" +
	iSearchTerm +
	"</b>\"" +
	"<hr>" +
	"<dl>";

      while (cursor.hasMoreElements ()) {
	IndexEntry entry = (IndexEntry) cursor.nextElement ();

	result +=
	  "<dt><a href=\"" +
	  entry.getURL () +
	  "\">" +
	  entry.getEntry () +
	  "</a></dt>\n";
	result += "<dd>" + entry.getDescription () + "</dd>\n";
      }

      result += "</dl>";
    }

    return result;
  }

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
       * An entry consists of a title, keywords, aliases, an URL and
       * a description, everything else is ignored.
       * Every entry starts with the keyword
       * "Entry" (case is ignored)
       *
       * must-have entries are "Entry" and "Keywords"
       *
       * 98-06-01: bugfix: don't null the variables
       */
      String entry = "";
      String keywords = "";
      String aliases = "";
      String url = "";
      String description = "";
      String prefix = "";
      String suffix = "";

      Value value = idxStream.popEntry ();

      while (value != null) {
	// parse the value now
	if (value.getKey ().equalsIgnoreCase ("entry")) {
	  // if a new entry is about to start, add the current one
	  addEntry (entry,keywords,aliases,description,url,prefix,suffix);

	  entry = value.getValue ();
	  aliases = entry;
	  keywords = "";
	  url = "";
	  description = "";
	} else if (value.getKey ().equalsIgnoreCase ("keywords")) {
	  keywords += value.getValue ();
	} else if (value.getKey ().equalsIgnoreCase ("aliases")) {
	  aliases += value.getValue ();
	} else if (value.getKey ().equalsIgnoreCase ("url")) {
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
      addEntry (entry,keywords,aliases,description,url,prefix,suffix);
    } catch (MalformedURLException exc) {
      // an error occured while reading...
    }

    return;
  }
  
  private void addEntry (String entry,String keywords,
			 String aliases,
			 String description,String url,
			 String prefix,String suffix)
  {
    // the entry must be set
    if (entry.length () == 0) {
      return;
    }

    // if the URL is empty, construct one following the rule:
    // URL = prefix + entry + suffix
    if (url.length () == 0) {
      url = prefix + entry + suffix;
    }
    IndexEntry idxEntry =
      new IndexEntry (entry,keywords,aliases,description,url);
    iIndexTable.addElement (idxEntry);

    return;
  }


  /* Instance Variables */

  private IndexTable iIndexTable;
  private String     iSearchTerm;

  
  /* Static Data */

  private static final String cIndexFile = "index.txt";
  private static final String cIndexKeyword = "INDEXFILE";
  private static final String cSearchKeyword = "SEARCHTERM";
}


// Local Variables:
// mode: Java
// mode: font-lock
// End:
