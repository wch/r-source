/*============================================================================

  Project: Simple JAVA Search Engine for Keyword Search
  
  JAVA Source file for the class IndexEntry
  
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
  
  $Revision: 1.5 $

  $Date: 2000/02/16 12:54:16 $
  
  $Author: leisch $

============================================================================*/


import java.lang.Object;
import java.lang.String;
import java.applet.AppletContext;
import java.net.URL;
import java.net.MalformedURLException;



public class IndexEntry extends Object
{
  public IndexEntry (String entry, String keywords, String aliases,
		     String desc, String url)
  {
    iEntry = entry;
    iKey = " " + keywords + " ";
    iAliases = " " + entry + " " + aliases + " ";
    iDescription = desc;
    iURL = url;
    
    // trace here
    Tracer.write ("Created IndexEntry. Entry = \"" +
		  entry + "\", Keywords = \"" + keywords + "\"\n");
    return;
  }

  
  public boolean matches (String aString, boolean searchDesc,
			  boolean searchKeywords, boolean searchAliases)
  {
    if (searchAliases &&
	(iAliases.toUpperCase ().indexOf (aString.toUpperCase ()) > -1))
      {
	return true;
      }
    
    if (searchDesc &&
	(iDescription.toUpperCase ().indexOf (aString.toUpperCase ()) > -1))
      {
	return true;
      }

    if (searchKeywords &&
	(iKey.toUpperCase ().indexOf (aString.toUpperCase ()) > -1))
      {
	return true;
      }
    
    return false;
  }


  public String getDescription ()
  {
    return iDescription;
  }

  public String getEntry ()
  {
    return iEntry;
  }

  public String getAliases ()
  {
    return iAliases;
  }

  public String getURL ()
  {
    return iURL;
  }


  private String iEntry;
  private String iKey;
  private String iAliases;
  private String iDescription;
  private String iURL;


}

// Local Variables:
// mode: Java
// mode: font-lock
// End:
