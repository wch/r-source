/*==============================================================================

  Project: 
  
  JAVA Source file for the class Tracer
  
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

  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/Tracer.java,v $
  
  $Revision: 1.3 $

  $Date: 1999/08/10 09:56:03 $
  
  $Author: ripley $

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
    //create a trace window
//       if (cTracer == null) {
 //        cTracer = new Tracer ();
 //      }
    
 //      cTracer._write (string);

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
  //  if (cTracer != null) {
  //    cTracer.show ();
 //   }

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
               98-11-14: replaced deprecated hide() with setVisible()
  ----------------------------------------------------------------------------*/
  public static void stop ()
  {
  //  if (cTracer != null) {
   //   cTracer.setVisible (false);
    //}

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
               98-11-14: replaced deprecated appendText() with append()
  ----------------------------------------------------------------------------*/
  private void _write (String string)
  {
    iOutput.append (string);

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
  Revision 1.3  1999/08/10 09:56:03  ripley
  change FSF address in copyrights
  add some copyrights in src/gnome and elsewhere

  Revision 1.2  1999/03/04 17:15:19  leisch
  various bugfixes

  Revision 1.1.4.1  1999/03/02 15:19:58  leisch
  search used only kewords, no titles

  Revision 1.3  1998/11/14 23:33:25  baier
  adjusted to JDK 1.1

  Revision 1.2  1998/05/10 22:57:14  baier
  tracer window disabled

  Revision 1.1  1998/05/10 02:44:44  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
