### $Id: zzz.R,v 1.2 2000/05/30 16:53:19 ripley Exp $
###
###                     Splines classes for R
###
### Copyright 1999-1999 Douglas M. Bates <bates@stat.wisc.edu>,
###                     William N. Venables <Bill.Venables@cmis.csiro.au>,
###                     Saikat DebRoy <saikat@stat.wisc.edu>
###
### This file is part of the splines library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
###
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
###
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA

.First.lib <- function(lib, pkg) library.dynam( "splines", pkg, lib )
