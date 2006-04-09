/*==============================================================================

  Project: 
  
  JAVA Source file for the class IndexStream
  
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

  $Rev$

  $LastChangedDate$
  
  $Author$

==============================================================================*/


/* -------------------------------- Imports --------------------------------- */

import java.util.*;
import java.applet.*;
import java.awt.*;
import java.net.*;
import java.io.*;



/*==============================================================================
                          Interface of class IndexStream
==============================================================================*/

/*------------------------------------------------------------------------------
  CLASS:    IndexStream
  SUPER:    Object
  CONF. TO: 
  PURPOSE:  reading and parsing a stream of multiple index entries, also handles
            some keywords, e.g. include of other streams, provides a context
            for environment space
  NOTES:    

  HISTORY:  98-05-08: created
------------------------------------------------------------------------------*/
public class IndexStream extends Object
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
  
    HISTORY:   98-05-08: created
               98-05-15: new static member for include directive
  ----------------------------------------------------------------------------*/
  public IndexStream (URL inputURL)
  {
    // init the instance data
    iInputURL = inputURL;
    iIndex = 0;
    iData = new Vector ();

    // read the index data from the stream
    readStream ();
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-08: created
  ----------------------------------------------------------------------------*/
  public Value popEntry ()
  {
    if (iIndex < 0) {
      iIndex = 0;
    }

    if (iIndex >= iData.size ()) {
      return null;
    }

    iIndex++;

    return (Value) iData.elementAt (iIndex - 1);
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-08: created
  ----------------------------------------------------------------------------*/
  public void pushEntry (Value anEntry)
  {
    if (iIndex < 0) {
      iIndex = 0;
    }

    if (iIndex >= iData.size ()) {
      iIndex = iData.size ();
    }

    iData.insertElementAt (anEntry,iIndex);

    return;
  }


  /*============================================================================
                              Protected methods
  ============================================================================*/

  /*============================================================================
                               Private methods
  ============================================================================*/

  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-08: created
               98-05-15: catch all exception (e.g. security violation)
               98-06-07: correctly trace exceptions
               98-11-14: replaced deprecated DataInputStream.readLine()
  ----------------------------------------------------------------------------*/
  public void readStream ()
  {
    try {
      InputStream stream = iInputURL.openStream ();
      // according to JDK 1.1.3 docs, now use BufferedReader. Here the docs:
      //
      // As of JDK 1.1, the preferred way to read lines of text is via the
      // BufferedReader.readLine() method. Programs that use the
      // DataInputStream class to read lines can be converted to use the
      // BufferedReader class by replacing code of the form 
      //   DataInputStream d = new DataInputStream(in); 
      // with 
      //   BufferedReader d = new BufferedReader(new InputStreamReader(in)); 
      BufferedReader is = new BufferedReader (new InputStreamReader (stream));

      // now read and parse all lines of the file.
      String line = is.readLine ();

      while (line != null) {
	parseLine (line);
	line = is.readLine ();
      }

      is.close ();
      stream.close ();
    } catch (IOException exc) {
      // there's some error, ignore it!
    } catch (Exception exc) {
      Tracer.write ("exction opening " + iInputURL.toString () + "\n");
      Tracer.write ("string: " + exc.toString () + "\n");
      if (exc.getMessage () != null) {
	Tracer.write ("info: " + exc.getMessage () + "\n");
      }
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
  
    HISTORY:   98-05-08: created
  ----------------------------------------------------------------------------*/
  public void parseLine (String line)
  {
    // if the line is empty, we'll ignore it
    if (line.length () == 0) {
      return;
    }

    // if the line starts with #, its a comment
    if (line.startsWith ("#")) {
      return;
    }

    // if the line starts with whitespace, its a continuation
    if (line.startsWith ("\t")
	|| line.startsWith (" ")) {
      parseContinuation (line);
    }

    // else, it must be a key/value pair
    parseKeyValue (line);

    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-08: created
  ----------------------------------------------------------------------------*/
  public void parseContinuation (String line)
  {
    // if there's no key/value pair in the list, ignore it
    if (iData.size () < 1) {
      return;
    }

    // we'll trim whitespace
    String value = line.trim ();

    // get the last object
    Value lastObject = (Value) iData.lastElement ();

    // modify it
    lastObject.addToValue (value);

    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   
  	      
    NOTES:     for safety, we'll ignore everything not ok
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-08: created
               98-05-15: handle include directive
  ----------------------------------------------------------------------------*/
  public void parseKeyValue (String line)
  {
    int index = line.indexOf (":");

    // not found or : as first character
    if (index < 1) {
      return;
    }

    // everything from the start to the : is a key
    String key = line.substring (0,index);

    String value = null;

    // value starts after :, trim whitespace
    try {
      value = line.substring (index + 1).trim ();
    } catch (IndexOutOfBoundsException exc) {
      line = "";
    }

    // 98-05-15: handle include directive
    if (key.equalsIgnoreCase (cIncludeDirective)) {
      // the value is assumed to be the URL of the included file
      try {
	URL includedURL = new URL (iInputURL,value);

	Tracer.write ("URL to be included expands to " +
		      includedURL.toString () + "\n");

	IndexStream idxStream = new IndexStream (includedURL);

	if (idxStream != null) {
	  Value copyValue = idxStream.popEntry ();
	  while (copyValue != null) {
	    iData.addElement (copyValue);
	    copyValue = idxStream.popEntry ();
	  }
	}
	idxStream = null;
      } catch (MalformedURLException exc) {
	// ignore errors
	Tracer.write ("error parsing include URL " + value + "\n");
	return;
      }
    } else {
      // create the value and add it to the vector
      Value newValue = new Value (key,value);

      iData.addElement (newValue);
    }

    return;
  }


  /*============================================================================
                             Instance Variables
  ============================================================================*/

  private URL    iInputURL;
  private int    iIndex;
  private Vector iData;


  /*============================================================================
                                Static Data
  ============================================================================*/
  private static final String cIncludeDirective = "include";
}


/*==============================================================================

  HISTORY:
  
  $Log: IndexStream.java,v $
  Revision 1.4  2002/05/05 22:34:52  pd
  .subset/.subset2, perfomace tweak in [[.data.frame

  Revision 1.3  1999/08/10 09:56:03  ripley
  change FSF address in copyrights
  add some copyrights in src/gnome and elsewhere

  Revision 1.2  1999/03/04 17:15:18  leisch
  various bugfixes

  Revision 1.1.4.1  1999/03/02 15:19:55  leisch
  search used only kewords, no titles

  Revision 1.2  1998/05/15 22:08:57  baier
  handle include

  Revision 1.1  1998/05/10 02:43:37  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
