/*==============================================================================

  Project: 
  
  JAVA Source file for the class IndexStream
  
  COPYRIGHT (C), 1998, Thomas Baier
  ALL RIGHTS RESERVED.
  
  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/IndexStream.java,v $
  
  $Revision: 1.1 $

  $Date: 1998/05/15 10:38:09 $
  
  $Author: leisch $

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
  ----------------------------------------------------------------------------*/
  public void readStream ()
  {
    try {
      InputStream stream = iInputURL.openStream ();
      DataInputStream is = new DataInputStream (stream);

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

    // create the value and add it to the vector
    Value newValue = new Value (key,value);

    iData.addElement (newValue);

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
}


/*==============================================================================

  HISTORY:
  
  $Log: IndexStream.java,v $
  Revision 1.1  1998/05/15 10:38:09  leisch
  New: Search Engine

  Revision 1.1  1998/05/10 02:43:37  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
