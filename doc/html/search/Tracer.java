/*==============================================================================

  Project: 
  
  JAVA Source file for the class Tracer
  
  COPYRIGHT (C), 1998, Thomas Baier
  ALL RIGHTS RESERVED.
  
  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/Tracer.java,v $
  
  $Revision: 1.1 $

  $Date: 1998/05/15 10:38:10 $
  
  $Author: leisch $

==============================================================================*/


/* -------------------------------- Imports --------------------------------- */

import java.applet.*;
import java.awt.*;
import java.net.*;
import java.io.*;



/*==============================================================================
                          Interface of class Tracer
==============================================================================*/

/*------------------------------------------------------------------------------
  CLASS:    Tracer
  SUPER:    Frame
  CONF. TO: 
  PURPOSE:  provide a tracing interface for every browser
  NOTES:    

  HISTORY:  98-05-10: created
------------------------------------------------------------------------------*/
public class Tracer extends Frame
{
  /*============================================================================
                                Public methods
  ============================================================================*/

  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   application front-end, create tracer if required, trace string
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-10: created
  ----------------------------------------------------------------------------*/
  public static void write (String string)
  {
    // create a trace window
    //    if (cTracer == null) {
    //      cTracer = new Tracer ();
    //    }

    //    cTracer._write (string);

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
  public static void start ()
  {
    if (cTracer != null) {
      cTracer.show ();
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
  public static void stop ()
  {
    if (cTracer != null) {
      cTracer.hide ();
    }

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
    PURPOSE:   default constructor
  	      
    NOTES:    
  
    PARAMS:   
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-10: created
  ----------------------------------------------------------------------------*/
  private Tracer ()
  {
    super ("Java Applet Tracer");

    // construct a layout manager
    GridBagLayout gridbag = new GridBagLayout ();
    GridBagConstraints constraints = new GridBagConstraints ();
    setLayout (gridbag);
    constraints.fill = GridBagConstraints.BOTH;
    constraints.weighty = 1.0;
    constraints.weightx = 1.0;
    constraints.gridwidth = GridBagConstraints.REMAINDER;
    constraints.gridheight = GridBagConstraints.REMAINDER;

    iOutput = new TextArea ();

    gridbag.setConstraints (iOutput,constraints);
    add (iOutput);

    show ();
    pack ();

    return;
  }


  /*----------------------------------------------------------------------------
    INTERFACE: 
    PURPOSE:   write a string to the tracer
  	      
    NOTES:     
  
    PARAMS:    
    THROWS:    
    RETURNS:   void
  
    HISTORY:   98-05-10: created
  ----------------------------------------------------------------------------*/
  private void _write (String string)
  {
    iOutput.appendText (string);

    return;
  }


  /*============================================================================
                             Instance Variables
  ============================================================================*/
  private TextArea iOutput;

  /*============================================================================
                                Static Data
  ============================================================================*/
  private static Tracer cTracer = null;

}


/*==============================================================================

  HISTORY:
  
  $Log: Tracer.java,v $
  Revision 1.1  1998/05/15 10:38:10  leisch
  New: Search Engine

  Revision 1.2  1998/05/10 22:57:14  baier
  tracer window disabled

  Revision 1.1  1998/05/10 02:44:44  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
