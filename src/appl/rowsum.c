/*
**  SCCS @(#)rowsum.c	4.2 06/30/93
**
** Add up data along rows
**
** Input
**      dim:   integer vector, the #rows and #columns of the matrix
**      na_x:  the value that marks NA's in the X matrix
**      x  :   matrix of data (remember, S uses column major order!)
**      group: the group to which each row belongs
**
** Output:
**      dd[0]: the number of unique groups found
**      x    : rows 1 to dd[0] contain the sums.
*/

void rowsum(dim, na_x, x, group)
int     *dim;
double  *na_x;
double  *x,
	*group;
    {
    register int i,j, k;
    int     nrow,
	    ncol;
    int     newrow;
    int     isna;
    double  tgrp,
	    sum;
    double  dummy;
    double  na;

    nrow = dim[0];
    ncol = dim[1];
    na   = *na_x;

    dummy =0;
    for (i=0; i<nrow; i++) if (group[i] < dummy) dummy = group[i];
    dummy = (dummy/2) -1;    /*no group uses this number */

    newrow =0;
    for (i=0; i<nrow; i++) {
	if (group[i] > dummy) {
	    tgrp = group[i];
	    for (j=0; j<ncol; j++) {
		sum =0;
		isna=0;
		for (k=i; k<nrow; k++)
		    if (group[k] == tgrp) {
			if (x[k + j*nrow] == na)  isna=1;
			else                      sum += x[k + j*nrow];
			}
		if (isna==1) x[newrow + j*nrow] = na;
		else         x[newrow + j*nrow] = sum;
		}
	    for (k=i; k<nrow; k++)
		if (group[k] == tgrp) group[k] = dummy;
	    newrow++;
	    }
	}
    dim[0] = newrow;
    }
