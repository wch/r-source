/*==============================================================================

  Project: 
  
  JAVA Source file for the class Value
  
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
  
  
  $Source: /scratch/CVS-ARCHIVE/R/doc/html/search/Value.java,v $
  
  $Revision: 1.2 $

  $Date: 1999/03/04 17:15:19 $
  
  $Author: leisch $

==============================================================================*/


/* -------------------------------- Imports --------------------------------- */


/*==============================================================================
                          Interface of class Value
==============================================================================*/

/*------------------------------------------------------------------------------
  CLASS:    Value
  SUPER:    Object
  CONF. TO: 
  PURPOSE:  
  NOTES:    

  HISTORY:  98-05-08: created
------------------------------------------------------------------------------*/
public class Value extends Object
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
  public Value (String key,String value)
  {
    iKey = key;
    iValue = value;
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
  public void addToValue (String addition)
  {
    iValue += addition;

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
  public String getKey ()
  {
    return iKey;
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
  public String getValue ()
  {
    return iValue;
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

  private String iKey;
  private String iValue;


  /*============================================================================
                                Static Data
  ============================================================================*/
}


/*==============================================================================

  HISTORY:
  
  $Log: Value.java,v $
  Revision 1.2  1999/03/04 17:15:19  leisch
  various bugfixes

  Revision 1.1.4.1  1999/03/02 15:19:59  leisch
  search used only kewords, no titles

  Revision 1.1  1998/05/10 02:44:50  baier
  Initial revision


==============================================================================*/


// Local Variables:
// mode: Java
// mode: font-lock
// End:
