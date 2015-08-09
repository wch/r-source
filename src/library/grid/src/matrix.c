/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-3 Paul Murrell
 *                2003 The R Core Team
 *
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#include "grid.h" 

/* Code for matrices, matrix multiplication, etc for performing
 *  2D affine transformations:  translations, scaling, and rotations.
 */

double locationX(LLocation l) {
    return l[0];
}

double locationY(LLocation l) {
    return l[1];
}

void copyTransform(LTransform t1, LTransform t2)
{
    int i, j;
    for (i=0; i<3; i++) 
	for (j=0; j<3; j++)
	    t2[i][j] = t1[i][j];
}

void invTransform(LTransform t, LTransform invt)
{
    double det = t[0][0]*(t[2][2]*t[1][1] - t[2][1]*t[1][2]) - 
	t[1][0]*(t[2][2]*t[0][1] - t[2][1]*t[0][2]) +
	t[2][0]*(t[1][2]*t[0][1] - t[1][1]*t[0][2]);
    if (det == 0)
	error(_("singular transformation matrix"));
    invt[0][0] = 1/det*(t[2][2]*t[1][1] - t[2][1]*t[1][2]);
    invt[0][1] = -1/det*(t[2][2]*t[0][1] - t[2][1]*t[0][2]);
    invt[0][2] = 1/det*(t[1][2]*t[0][1] - t[1][1]*t[0][2]);
    invt[1][0] = -1/det*(t[2][2]*t[1][0] - t[2][0]*t[1][2]);
    invt[1][1] = 1/det*(t[2][2]*t[0][0] - t[2][0]*t[0][2]);
    invt[1][2] = -1/det*(t[1][2]*t[0][0] - t[1][0]*t[0][2]);
    invt[2][0] = 1/det*(t[2][1]*t[1][0] - t[2][0]*t[1][1]);
    invt[2][1] = -1/det*(t[2][1]*t[0][0] - t[2][0]*t[0][1]);
    invt[2][2] = 1/det*(t[1][1]*t[0][0] - t[1][0]*t[0][1]);
}

void identity(LTransform m) 
{
    int i, j;
    for (i=0; i<3; i++) 
	for (j=0; j<3; j++)
	    if (i == j)
		m[i][j] = 1;
	    else
		m[i][j] = 0;
}

void translation(double tx, double ty, LTransform m)
{
    identity(m);
    m[2][0] = tx;
    m[2][1] = ty;
}

void scaling(double sx, double sy, LTransform m)
{
    identity(m);
    m[0][0] = sx;
    m[1][1] = sy;
}

void rotation(double theta, LTransform m)
{
    double thetarad = theta/180*M_PI;
    double costheta = cos(thetarad);
    double sintheta = sin(thetarad);
    identity(m);
    m[0][0] = costheta;
    m[0][1] = sintheta;
    m[1][0] = -sintheta;
    m[1][1] = costheta;
}

void multiply(LTransform m1, LTransform m2, LTransform m)
{
    m[0][0] = m1[0][0]*m2[0][0] + m1[0][1]*m2[1][0] + m1[0][2]*m2[2][0];
    m[0][1] = m1[0][0]*m2[0][1] + m1[0][1]*m2[1][1] + m1[0][2]*m2[2][1];
    m[0][2] = m1[0][0]*m2[0][2] + m1[0][1]*m2[1][2] + m1[0][2]*m2[2][2];
    m[1][0] = m1[1][0]*m2[0][0] + m1[1][1]*m2[1][0] + m1[1][2]*m2[2][0];
    m[1][1] = m1[1][0]*m2[0][1] + m1[1][1]*m2[1][1] + m1[1][2]*m2[2][1];
    m[1][2] = m1[1][0]*m2[0][2] + m1[1][1]*m2[1][2] + m1[1][2]*m2[2][2];
    m[2][0] = m1[2][0]*m2[0][0] + m1[2][1]*m2[1][0] + m1[2][2]*m2[2][0];
    m[2][1] = m1[2][0]*m2[0][1] + m1[2][1]*m2[1][1] + m1[2][2]*m2[2][1];
    m[2][2] = m1[2][0]*m2[0][2] + m1[2][1]*m2[1][2] + m1[2][2]*m2[2][2];
}

void location(double x, double y, LLocation v)
{
    v[0] = x;
    v[1] = y;
    v[2] = 1;
}

void trans(LLocation vin, LTransform m, LLocation vout)
{
    vout[0] = vin[0]*m[0][0] + vin[1]*m[1][0] + vin[2]*m[2][0];
    vout[1] = vin[0]*m[0][1] + vin[1]*m[1][1] + vin[2]*m[2][1];
    vout[2] = vin[0]*m[0][2] + vin[1]*m[1][2] + vin[2]*m[2][2];
}

/* Testing code
 * Need to undocument main() below and add #include <math.h> at top of file
 * Correct answers are "2.67 2.00 1.00" for m4=identity
 * and "0.00 2.00 1.00" for m4=rotation
 */

/*
  main() 
  {
  LLocation v1, v2; 
  LTransform m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11;
  location(9, 10, v1);
  translation(-5, -6, m1);
  scaling(1/7.0, 1/8.0, m2);
  scaling(7, 8, m3);
  identity(m4);  
  rotation(3.141592 / 2, m4); 
  translation(4, 4, m5);
  scaling(1/3.0, 1/4.0, m6);
  multiply(m1, m2, m7);
  multiply(m7, m3, m8);
  multiply(m8, m4, m9);
  multiply(m9, m5, m10);
  multiply(m10, m6, m11);
  transform(v1, m11, v2);
  printf("%1.2f %1.2f %1.2f\n", v2[0], v2[1], v2[2]);	  
  }
*/

