/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--1999  Marcus G. Daniels <mgd@swarm.org>
 *  Copyright (C) 1998--1999  The R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Mathlib.h"
#include "Fileio.h"

#ifdef HAVE_HDF5
#include <hdf5.h>

#define STRING2REF_CONV "string->ref"
#define REF2STRING_CONV "ref->string"
/* FIXME: should really use SEXP R_RowNamesSymbol : */
#define ROWNAMES "row.names"

static herr_t
ref_string (hid_t sid, hid_t did, H5T_cdata_t *cdata,
	    size_t count, size_t stride,
	    void *buf, void *bkg,
	    hid_t dset_xfer_plid)
{
  if (cdata->command == H5T_CONV_CONV)
    {
      SEXPREC *srcbuf[count];
      char *destbuf = buf;
      SEXPREC **recptr = srcbuf;
      size_t i;
      size_t maxlen = H5Tget_size (did);

      memcpy (srcbuf, buf, sizeof (srcbuf));

      for (i = 0; i < count; i++)
	{
	  strncpy (destbuf, CHAR (*recptr), maxlen);
	  recptr++;
	  destbuf += maxlen;
	}
    }
  return 0;
}

static herr_t
string_ref (hid_t sid, hid_t did, H5T_cdata_t *cdata,
	    size_t count, size_t stride,
	    void *buf, void *bkg,
	    hid_t dset_xfer_plid)
{
  if (cdata->command == H5T_CONV_CONV)
    {
      size_t size = H5Tget_size (sid);
      unsigned char srcbuf[size * count], *srcptr = srcbuf;
      size_t i;

      memcpy (srcbuf, buf, sizeof (srcbuf));
      for (i = 0; i < count;i ++)
	{
	  ((SEXPREC **)buf)[i] = mkChar (srcptr);
	  srcptr += size;
	}
    }
  return 0;
}

struct permute_info {
  SEXP call;
  int writeflag;
  SEXPTYPE type;
  unsigned rank;
  hssize_t *dims;
  hssize_t *coord;
  hid_t dataset;
  hid_t memtid;
  hid_t space;
  hid_t mspace;
  void *buf;
};

static void
permute (struct permute_info *pinfo, unsigned dimnum)
{
  hssize_t i;

  if (dimnum < pinfo->rank)
    {
      for (i = 0; i < pinfo->dims[dimnum]; i++)
	{
	  pinfo->coord[dimnum] = i;
	  permute (pinfo, dimnum + 1);
	}
    }
  else
    {
      unsigned offset, mult;

      if (H5Sselect_elements (pinfo->space, H5S_SELECT_SET, 1,
			      (const hssize_t **) pinfo->coord) < 0)
	errorcall (pinfo->call, "Unable to select file elements");

      offset = pinfo->coord[0];
      mult = 1;
      for (i = 1; i < pinfo->rank; i++)
	{
	  mult *= pinfo->dims[i - 1];
	  offset += pinfo->coord[i] * mult;
	}

      {
	void *pointaddr;

	switch (pinfo->type)
	  {
	  case STRSXP: case VECSXP:
	    pointaddr = &((SEXPREC **)pinfo->buf)[offset];
	    break;
	  case REALSXP:
	    pointaddr = &((double *)pinfo->buf)[offset];
	    break;
	  case INTSXP: case LGLSXP:
	    pointaddr = &((int *)pinfo->buf)[offset];
	    break;
	  default:
	    errorcall (pinfo->call, "No support for R type: %d", pinfo->type);
	    pointaddr = &offset;/* unreached; -Wall */
	  }

	if (pinfo->writeflag)
	  {
	    if (H5Dwrite (pinfo->dataset,
			  pinfo->memtid,
			  pinfo->mspace,
			  pinfo->space,
			  H5P_DEFAULT,
			  pointaddr) < 0)
	      errorcall (pinfo->call, "Unable to write dataset");
	  }
	else
	  {
	    if (H5Dread (pinfo->dataset,
			 pinfo->memtid,
			 pinfo->mspace,
			 pinfo->space,
			 H5P_DEFAULT,
			 pointaddr) < 0)
	      errorcall (pinfo->call, "Unable to read dataset");
	  }
      }
    }
}

static hid_t
make_sexp_ref_type (SEXP call)
{
  hid_t memtid;

  if ((memtid = H5Tcopy (H5T_STD_REF_OBJ)) < 0)
    errorcall (call, "Unable to copy H5T_STD_REF_OBJ");
  if (H5Tset_size (memtid, sizeof (SEXPREC *)) < 0)
    errorcall (call, "unable to set size of reference type");
  return memtid;
}

static hid_t
get_string_type (SEXP call, SEXP vec)
{
  hid_t stid;
  unsigned vecpos;
  size_t maxstrlen = 0;

  for (vecpos = 0; vecpos < LENGTH (vec); vecpos++)
    {
      SEXP stritem = STRING (vec)[vecpos];

      if (LENGTH (stritem) > maxstrlen)
	maxstrlen = LENGTH (stritem);
    }

  if ((stid = H5Tcopy (H5T_C_S1)) < 0)
    errorcall (call, "Cannot copy string type");

  if (H5Tset_size (stid, maxstrlen + 1) < 0)
    errorcall (call, "Cannot set size of string type");

  return stid;
}


static void
vector_io (SEXP call, int writeflag, hid_t dataset, hid_t space, SEXP obj)
{
  int rank = H5Sget_simple_extent_ndims (space);
  hsize_t mdims[1] = {1};
  hsize_t dims[rank], maxdims[rank];
  SEXPTYPE type = TYPEOF (obj);
  hid_t memtid, tid, mspace;
  void *buf;

  if ((tid = H5Dget_type (dataset)) < 0)
    errorcall (call, "Unable to get type for dataset");

  switch (type) {

  case STRSXP:
      memtid = make_sexp_ref_type (call);
      buf = STRING (obj);
      break;
  case REALSXP:
      memtid = H5T_NATIVE_DOUBLE;
      buf = REAL (obj);
      break;
  case INTSXP:
      memtid = H5T_NATIVE_INT;
      buf = INTEGER (obj);
      break;
  case LGLSXP:
      memtid = H5T_NATIVE_UINT;
      buf = INTEGER (obj);
      break;
  default:
      errorcall (call, "Can't get type for R type: %d (IO)", type);
      /* unreached (-Wall): */ memtid = tid;  buf = &tid;
  }

  if (H5Sget_simple_extent_dims (space, dims, maxdims) < 0)
    errorcall (call, "Unable to get dimensions of space");

  if ((mspace = H5Screate_simple (1, mdims, NULL)) < 0)
    errorcall (call, "Unable to create point space");

  {
    struct permute_info pinfo;
    hssize_t coord[rank];

    pinfo.call = call;
    pinfo.writeflag = writeflag;
    pinfo.type = type;
    pinfo.rank = rank;
    pinfo.coord = coord;
    pinfo.dims = dims;
    pinfo.dataset = dataset;
    pinfo.memtid = memtid;
    pinfo.space = space;
    pinfo.mspace = mspace;
    pinfo.buf = buf;

    permute (&pinfo, 0);
  }

  if (H5Sclose (mspace) < 0)
    errorcall (call, "Unable to close point space");

  if (type == STRSXP) {
      if (H5Tclose (memtid) < 0)
	errorcall (call, "Unable to close string reference type");
    }
}

static void
hdf5_save_attributes (SEXP call, hid_t loc_id, SEXP val)
{
  SEXP l;

  for (l = ATTRIB (val); l != R_NilValue; l = CDR (l))
    {
      SEXP attr = CAR (l);
      SEXPTYPE type = TYPEOF (attr);
      const char *name = CHAR (PRINTNAME (TAG (l)));
      void *buf;
      hid_t tid, memtid;
      hid_t sid, aid;
      unsigned count = LENGTH (attr);
      /*SEXPREC *stringptrs[count];*/

      if (TAG (l) == R_RowNamesSymbol
	  || TAG (l) == R_ClassSymbol
	  || TAG (l) == R_NamesSymbol
	  || TAG (l) == R_DimNamesSymbol)
	continue;
      {
	hsize_t dims[1];

	dims[0] = count;

	if ((sid = H5Screate_simple (1, dims, NULL)) < 0)
	  errorcall (call,
		     "unable to create vector space for attribute `%s'", name);
      }

      if (type == STRSXP)
	{
	  memtid = make_sexp_ref_type (call);
	  tid = get_string_type (call, attr);
	  buf = STRING (attr);
	}
      else if (type == LGLSXP)
	{
	  memtid = H5T_NATIVE_INT;
	  tid = H5Tcopy (H5T_NATIVE_UINT);
	  H5Tset_precision (tid, 1);
	  H5Tset_size (tid, 1);
	  buf = INTEGER (attr);
	}
     else if (type == INTSXP)
	{
	  memtid = H5T_NATIVE_INT;
	  tid = H5T_NATIVE_INT;
	  buf = INTEGER (attr);
	}
      else if (type == REALSXP)
	{
	  memtid = H5T_NATIVE_DOUBLE;
	  tid = H5T_NATIVE_DOUBLE;
	  buf = REAL (attr);
	}
      else
	abort ();

      if ((aid = H5Acreate (loc_id, name, tid, sid, H5P_DEFAULT)) < 0)
	errorcall (call, "unable to create attribute `%s'", name);

      if (H5Awrite (aid, memtid, buf) < 0)
	errorcall (call, "unable to write attribute `%s'", name);

      if (H5Aclose (aid) < 0)
	errorcall (call, "unable to close attribute `%s'", name);

      if (type == STRSXP)
	{
	  if (H5Tclose (memtid) < 0)
	    errorcall (call,
		       "unable to close string reference type `%s'",
		       name);
	}
      if (type == LGLSXP || type == STRSXP)
	{
	  if (H5Tclose (tid) < 0)
	    errorcall (call, "unable to close output type `%s'", name);
	}
      if (H5Sclose (sid) < 0)
	errorcall (call, "unable to close space for attribute `%s'", name);
    }
}

static void
hdf5_write_vector (SEXP call, hid_t id, const char *symname, SEXP val)
{
  unsigned i, rank;
  SEXP dimvec;
  hid_t space, dataset;
  SEXPTYPE type = TYPEOF (val);
  hid_t tid;

  dimvec = getAttrib (val, R_DimSymbol);
  rank = (dimvec == R_NilValue) ? 1 : LENGTH (dimvec);

  {
    hsize_t dims[rank];

    if (rank > 1)
      for (i = 0; i < rank; i++)
	dims[i] = INTEGER (dimvec)[i];
    else
      dims[0] = length(val);

    if ((space = H5Screate_simple (rank, dims, NULL)) < 0)
      errorcall (call, "Unable to create file dataspace");

    if (type == STRSXP)
      tid = get_string_type (call, val);
    else if (type == LGLSXP)
      {
	tid = H5Tcopy (H5T_NATIVE_UINT);
	H5Tset_precision (tid, 1);
	H5Tset_size (tid, 1);
      }
    else if (type == INTSXP)
      tid = H5T_NATIVE_INT;
    else if (type == REALSXP)
      tid = H5T_NATIVE_DOUBLE;
    else {
      errorcall (call, "Can't get type for R type: %d (Creating)", type);
      tid = H5T_NATIVE_INT;/*unreached; -Wall*/
    }

    if ((dataset = H5Dcreate (id,
			      symname,
			      tid,
			      space,
			      H5P_DEFAULT)) < 0)
      errorcall (call, "Unable to create dataset");

    vector_io (call, LTRUE, dataset, space, val);
    hdf5_save_attributes (call, dataset, val);

    if (type == LGLSXP || type == STRSXP)
      if (H5Tclose (tid) < 0)
	errorcall (call, "Unable to close type");

    if (H5Dclose (dataset) < 0)
      errorcall (call, "Unable to close dataset");
    if (H5Sclose (space) < 0)
      errorcall (call, "Unable to close space");
  }
}

static void
hdf5_write_string (SEXP call, hid_t fid, const char *symname, const char *str)
{
  hid_t stringtype;
  hid_t dataset;
  hid_t dataspace;

  dataspace = H5Screate (H5S_SCALAR);

  stringtype = H5Tcopy (H5T_C_S1);
  H5Tset_size (stringtype, strlen (str) + 1);

  if ((dataset = H5Dcreate (fid,
			    symname,
			    stringtype,
			    dataspace,
			    H5P_DEFAULT)) < 0)
    errorcall (call, "Unable to create dataset");

  if (H5Dwrite (dataset,
		stringtype,
		H5S_ALL,
		H5S_ALL,
		H5P_DEFAULT,
		str) < 0)
    errorcall (call, "Unable to write dataset");

  H5Dclose (dataset);
  H5Sclose (dataspace);
  H5Tclose (stringtype);
}

static unsigned
align (unsigned offset, unsigned alignto)
{
#if 0
  unsigned mask = alignto - 1;

  if ((offset & mask) == 0)
    return offset;
  else
    return (offset + alignto) & ~mask;
#else
  return offset;
#endif
}

static void
create_rownames_dataset_attribute (SEXP call, hid_t dataset, SEXP rownames)
{
  hid_t stringtid = get_string_type (call, rownames);
  hid_t rtid = make_sexp_ref_type (call);
  hid_t rnattrib, rndataspace;
  unsigned rowcount = LENGTH (rownames);
  hsize_t dims[1];

  dims[0] = rowcount;

  if ((rndataspace = H5Screate_simple (1, dims, NULL)) < 0)
    errorcall (call, "Unable to create row names vector space");

  if ((rnattrib = H5Acreate (dataset, ROWNAMES,
			     stringtid, rndataspace, H5P_DEFAULT)) < 0)
    errorcall (call, "unable to create row names dataset");

  if (H5Awrite (rnattrib, rtid, STRING (rownames)) < 0)
    errorcall (call, "unable to write row names dataset");

  if (H5Aclose (rnattrib) < 0)
    errorcall (call, "unable to close row names dataset");

  if (H5Sclose (rndataspace) < 0)
    errorcall (call, "unable to close row names dataspace");

  if (H5Tclose (stringtid) < 0)
    errorcall (call, "unable to close row names string type");

  if (H5Tclose (rtid) < 0)
    errorcall (call, "unable to close reference type");
}

static void
hdf5_save_object (SEXP call, hid_t fid, const char *symname, SEXP val)
{
  if (isFrame (val))
    {
      unsigned colcount = length (val), pos;
      size_t offsets[colcount];
      hid_t hdftypes[colcount];
      hid_t hdfbooltype;
      size_t size = 0;

      if ((hdfbooltype = H5Tcopy (H5T_NATIVE_UINT)) < 0)
	errorcall (call, "Cannot copy unsigned integer type");
      if (H5Tset_precision (hdfbooltype, 1) < 0)
	errorcall (call, "Cannot set precision of boolean type");
      if (H5Tset_size (hdfbooltype, 1) < 0)
	errorcall (call, "Cannot set size of boolean type");

      for (pos = 0; pos < colcount; pos++)
	{
	  SEXPTYPE type = TYPEOF (VECTOR (val)[pos]);

	  switch (type)
	    {
	    case REALSXP:
	      hdftypes[pos] = H5T_NATIVE_DOUBLE;
	      break;
	    case INTSXP:
	      hdftypes[pos] = H5T_NATIVE_INT;
	      break;
	    case STRSXP:
	      hdftypes[pos] = get_string_type (call, VECTOR (val)[pos]);
	      break;
	    case LGLSXP:
	      hdftypes[pos] = hdfbooltype;
	      break;
	    default:
	      errorcall (call,
			 "No support for converting R type: %d to HDF5",
			 type);
	      break;
	    }
	  if (H5Tget_class (hdftypes[pos]) == H5T_STRING)
	    size = align (size, 8);
	  else
	    size = align (size, H5Tget_size (hdftypes[pos]));
	  offsets[pos] = size;
	  size += H5Tget_size (hdftypes[pos]);
	}
      size = align (size, H5Tget_size (hdftypes[0]));
      {
	hid_t ctid;
	hid_t dataset;
	hid_t dataspace;
	unsigned rowcount = length (VECTOR (val)[0]);
	{
	  hsize_t dims[1];

	  dims[0] = rowcount;
	  if ((dataspace = H5Screate_simple (1, dims, NULL)) < 0)
	    errorcall (call, "Unable to create dataframe vector space");
	}

	{
	  SEXP colnames = getAttrib (val, R_NamesSymbol);

	  if ((ctid = H5Tcreate (H5T_COMPOUND, size)) < 0)
	    errorcall (call, "unable to create compound type");

	  for (pos = 0; pos < colcount; pos++)
	    if (H5Tinsert (ctid,
			   CHAR (STRING (colnames)[pos]),
			   offsets[pos],
			   hdftypes[pos]) < 0)
	      errorcall (call, "unable to insert type into compound type");
	}
	if (H5Tpack (ctid) < 0)
	  errorcall (call, "Unable to pack type");

	if (H5Tlock (ctid) < 0)
	  errorcall (call, "Unable to lock type");

	if ((dataset = H5Dcreate (fid, symname,
				  ctid, dataspace, H5P_DEFAULT)) < 0)
	  errorcall (call, "unable to create dataframe dataset");

	{
	  unsigned ri;
	  unsigned char buf[rowcount][size];

	  for (ri = 0; ri < rowcount; ri++)
	    for (pos = 0; pos < colcount; pos++)
	      {
		SEXP item = VECTOR (val)[pos];
		SEXPTYPE type = TYPEOF (item);
		void *ptr = &buf[ri][offsets[pos]];

		switch (type)
		  {
		  case REALSXP:
		    memcpy (ptr, &REAL (item)[ri], sizeof (double));
		    break;
		  case INTSXP:
		    memcpy (ptr, &INTEGER (item)[ri], sizeof (int));
		    break;
		  case STRSXP:
		    {
		      SEXP stritem = STRING (item)[ri];
		      /*size_t len = LENGTH (stritem);*/

		      memset (ptr, 0, H5Tget_size (hdftypes[pos]));
		      strcpy ((char *)ptr, CHAR (stritem));
		    }
		    break;
		  case LGLSXP:
		    *(unsigned char *)ptr = INTEGER (item)[ri];
		    break;
		  default:
		    abort ();
		  }
	      }
	  if (H5Dwrite (dataset,
			ctid,
			dataspace,
			dataspace,
			H5P_DEFAULT,
			buf) < 0)
	    errorcall (call, "Unable to write dataframe");
	}
	hdf5_save_attributes (call, dataset, val);
	{
	  SEXP rownames = getAttrib (val, R_RowNamesSymbol);

	  if (rownames != R_NilValue)
	    create_rownames_dataset_attribute (call, dataset, rownames);
	}
	if (H5Dclose (dataset) < 0)
	  errorcall (call, "Cannot close dataset");
	if (H5Sclose (dataspace) < 0)
	  errorcall (call, "Cannot close dataspace");
      }

      for (pos = 0; pos < colcount; pos++)
	if (H5Tget_class (hdftypes[pos]) == H5T_STRING)
	  if (H5Tclose (hdftypes[pos]) < 0)
	    errorcall (call, "Cannot close string type");
      if (H5Tclose (hdfbooltype) < 0)
	errorcall (call, "Cannot close boolean type");

    }
  else if (isNull (val))
    {
    }
  else
    {
      SEXPTYPE type = TYPEOF (val);

      switch (type)
	{
	case LGLSXP: case INTSXP: case REALSXP: case STRSXP:
	  hdf5_write_vector (call, fid, symname, val);
	  break;
	case LISTSXP: case VECSXP:
	  {
	    unsigned len = length (val);
	    hid_t gid;
	    unsigned pos;
	    char buf[(sizeof (pos) * 8 / 3 + 1) + 1];

	    if ((gid = H5Gcreate (fid, symname, len * 8)) < 0)
	      errorcall (call, "unable to create group");

	    if (type == LISTSXP)
	      {
		SEXP l;

		for (l = val, pos = 0; l != R_NilValue; l = CDR (l), pos++)
		  {
		    SEXP s = CAR (l);

		    if (!isNull (TAG (l)))
		      hdf5_save_object (call, gid,
				       CHAR (PRINTNAME (TAG (l))), s);
		    else
		      {
			sprintf (buf, "%u", pos);
			hdf5_save_object (call, gid, buf, s);
		      }
		  }
	      }
	    else
	      {
		for (pos = 0; pos < len; pos++)
		  {
		    SEXP s = VECTOR (val)[pos];
		    SEXP names = getAttrib (val, R_NamesSymbol);

		    if (!isNull (names))
		      hdf5_save_object (call, gid,
				       CHAR (STRING (names) [pos]),
				       s);
		    else
		      {
			sprintf (buf, "%u", pos);
			hdf5_save_object (call, gid, buf, s);
		      }
		  }
	      }
	    hdf5_save_attributes (call, gid, val);

	    if (H5Gclose (gid) < 0)
	      errorcall (call, "unable to close group");
	  }
	  break;
	case SYMSXP:
	  {
	    const char *pn = CHAR (PRINTNAME (val));

	    hdf5_write_string (call, fid, symname, pn);
	  }
	  break;
	default:
	  errorcall (call, "unhandled type: %d", type);
	  break;
	}
    }
}


static void
hdf5_save_symbol (SEXP call, hid_t fid, SEXP sym, SEXP env)
{
  SEXP val;

  val = findVar (sym, env);

  hdf5_save_object (call, fid, CHAR (PRINTNAME (sym)), val);
}

SEXP do_hdf5save (SEXP call, SEXP op, SEXP args, SEXP env)
{
  const char *path;
  hid_t fid;
  SEXP s;

  checkArity (op, args);

  if (length (args) < 2)
    errorcall (call, "Two arguments are required: HDF-path and an object");

  if (TYPEOF (CAR (args)) != STRSXP)
    errorcall (call, "first argument must be a pathname");

  path = CHAR (STRING (CAR (args))[0]);

  H5dont_atexit ();

  if (H5Tregister (H5T_PERS_SOFT,
		   REF2STRING_CONV,
		   H5T_STD_REF_OBJ,
		   H5T_C_S1, ref_string) < 0)
    errorcall (call, "Unable to register ref->string converter");

  if ((fid = H5Fcreate (path, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
    errorcall (call, "unable to create HDF file: %s", path);

  for (s = CDR (args); s != R_NilValue; s = CDR (s))
    hdf5_save_symbol (call, fid, CAR (s), env);

  if (H5Fclose (fid) < 0)
    errorcall (call, "unable to close HDF file: %s", path);

  if (H5Tunregister (H5T_PERS_SOFT, REF2STRING_CONV, -1, -1, ref_string) < 0)
    errorcall (call, "Unable to unregister ref->string converter");

  return R_NilValue;
}

struct hdf5_iterate_info {
  SEXP call;
  void (*add) (struct hdf5_iterate_info *, const char *, SEXP);
  SEXP env;
  SEXP ret;
};

static void
add_to_list (struct hdf5_iterate_info *iinfo, const char *name, SEXP obj)
{
  PROTECT (iinfo->ret);
  iinfo->ret = CONS (obj, iinfo->ret);
  TAG (iinfo->ret) = install ((char *) name);
  UNPROTECT (1);
}


static SEXP
collect (SEXP call, hid_t id, H5G_iterate_t iterate_func, SEXP env)
{
  struct hdf5_iterate_info iinfo;

  iinfo.call = call;
  iinfo.add = add_to_list;
  iinfo.ret = R_NilValue;
  iinfo.env = env;

  if (H5Giterate (id, ".", NULL, iterate_func, &iinfo) < 0)
    errorcall (call, "unable to collect HDF group");

  {
    SEXP nl = R_NilValue, l;
    SEXP rl = iinfo.ret;

    PROTECT (rl);
    l = rl;
    while (l != R_NilValue)
      {
	PROTECT (nl);
	nl = CONS (CAR (l), nl);
	TAG (nl) = TAG (l);
	UNPROTECT (1);
	l = CDR (l);
      }
    UNPROTECT (1);
    return nl;
  }
}

static void
load_rownames_dataset_attribute (SEXP call, hid_t dataset, SEXP vec)
{
  hid_t rnattrib, rnspace, rntid;
  unsigned rowcount;
  SEXP rownames;
  H5E_auto_t errfunc;
  void *client_data;

  H5Eset_auto (NULL, NULL);
  rnattrib = H5Aopen_name (dataset, ROWNAMES);
  H5Eget_auto (&errfunc, &client_data);
  H5Eset_auto (errfunc, client_data);

  if (rnattrib < 0)
    return;

  if ((rnspace = H5Aget_space (rnattrib)) < 0)
    errorcall (call, "could not get space for rownames attribute");

  if ((rntid = H5Aget_type (rnattrib)) < 0)
    errorcall (call, "could not get element type of rownames attribute");

  if (H5Sget_simple_extent_ndims (rnspace) != 1)
    errorcall (call, "rownames space should be of rank 1");

  {
    hsize_t dims[1], maxdims[1];

    if (H5Sget_simple_extent_dims (rnspace, dims, maxdims)< 0)
      errorcall (call, "can't get attribute space dims");
    rowcount = dims[0];
  }
  PROTECT (rownames = allocVector (STRSXP, rowcount));
  {
    SEXPREC *strptrs[rowcount];
    unsigned ri;
    hid_t rtid = make_sexp_ref_type (call);

    for (ri = 0; ri < rowcount; ri++)
      strptrs[ri] = STRING (rownames)[ri];

    if (H5Aread (rnattrib, rtid, strptrs) < 0)
      errorcall (call, "can't read rownames");

    for (ri = 0; ri < rowcount; ri++)
      STRING (rownames)[ri] = strptrs [ri];
  }
  setAttrib (vec, R_RowNamesSymbol, rownames);
  UNPROTECT (1);
}

struct hdf5_attribute_info {
  SEXP call;
  SEXP obj;
  const char *name;
};

static herr_t
hdf5_process_attribute (hid_t loc_id, const char *attrName, void *data)
{
  struct hdf5_attribute_info *ainfo = data;
  hid_t aid, sid, tid;
  H5T_class_t class;
  size_t tid_size;

  if ((aid = H5Aopen_name (loc_id, attrName)) < 0)
    errorcall (ainfo->call, "could not open attribute `%s'", attrName);

  if ((sid = H5Aget_space (aid)) < 0)
    errorcall (ainfo->call, "could not open space of attribute `%s'",
	       attrName);

  if ((tid = H5Aget_type (aid)) < 0)
    errorcall (ainfo->call, "could not get type of attribute `%s'", attrName);

  if ((tid_size = H5Tget_size (tid)) < 0)
    errorcall (ainfo->call, "could not get size of attribute `%s' tid",
	       attrName);

  if ((class = H5Tget_class (tid)) < 0)
    errorcall (ainfo->call, "could not get type class of attribute `%s'",
	       attrName);

  {
    int rank;

    if ((rank = H5Sget_simple_extent_ndims (sid)) < 0)
      errorcall (ainfo->call,
		 "could not get rank of attribute space `%s'",
		 attrName);
    {
      hsize_t dims[rank];

      if (H5Sget_simple_extent_dims (sid, dims, NULL) < 0)
	errorcall (ainfo->call,
		   "could not get extent of attribute space `%s'",
		   attrName);

      if (rank == 1)
	{
	  unsigned count = dims[0];
	  SEXPTYPE type;
	  hid_t memtid;
	  SEXP vec;
	  void *buf;

	  switch (class)
	    {
	    case H5T_INTEGER:
	      type = (tid_size == 1) ? LGLSXP : INTSXP;
	      memtid = H5Tcopy (H5T_NATIVE_INT);
	      break;
	    case H5T_FLOAT:
	      type = REALSXP;
	      memtid = H5Tcopy (H5T_NATIVE_DOUBLE);
	      break;
	    case H5T_STRING:
	      type = STRSXP;
	      memtid = make_sexp_ref_type (ainfo->call);
	      break;
	    default:
	      warningcall (ainfo->call, "skipping attribute `%s' due to type",
			   attrName);
	      goto done;
	    }
	  PROTECT (vec = allocVector (type, count));
	  switch (class)
	    {
	    case H5T_INTEGER:
	      buf = INTEGER (vec);
	      break;
	    case H5T_FLOAT:
	      buf = REAL (vec);
	      break;
	    case H5T_STRING:
	      buf = STRING (vec);
	      break;
	    default:
	      abort ();
	    }
	  if (H5Aread (aid, memtid, buf) < 0)
	    errorcall (ainfo->call, "unable to read attribute `%s'", attrName);

	  if (!isNull (ainfo->obj))
	    setAttrib (ainfo->obj, install ((char *) attrName), vec);

	  UNPROTECT (1);

	  if (H5Tclose (memtid) < 0)
	    errorcall (ainfo->call,
		       "unable to close reference type in attribute `%s'",
		       attrName);
	}
      else
	warningcall (ainfo->call, "skipping attribute `%s' due to rank",
		     attrName);
    }
  }
 done:
  if (H5Sclose (sid) < 0)
    errorcall (ainfo->call, "unable to close attribute `%s' space", attrName);
  if (H5Tclose (tid) < 0)
    errorcall (ainfo->call, "unable to close attribute `%s' type", attrName);
  if (H5Aclose (aid) < 0)
    errorcall (ainfo->call, "unable to close attribute `%s'", attrName);
  return 0;
}

static void
hdf5_load_attributes (SEXP call, hid_t id, SEXP obj, const char *name)
{
  unsigned idx = 0;
  struct hdf5_attribute_info ainfo;

  ainfo.call = call;
  ainfo.obj = obj;
  ainfo.name = name;

  if (H5Aiterate (id, &idx, hdf5_process_attribute, &ainfo) < 0)
    errorcall (call, "unable to iterate over attributes");
}

static herr_t
hdf5_process_object (hid_t id, const char *name, void *client_data)
{
  struct hdf5_iterate_info *iinfo = client_data;

  H5G_stat_t statbuf;

  if (H5Gget_objinfo (id, name, 1, &statbuf) < 0)
    errorcall (iinfo->call, "Cannot query object `%s'", name);

  if (statbuf.type == H5G_GROUP)
    {
      SEXP l;
      hid_t gid = H5Gopen (id, name);

      if (gid < 0)
	errorcall (iinfo->call, "unable to open group `%s'", name);

      PROTECT (l = collect (iinfo->call, gid, hdf5_process_object, iinfo->env));
      iinfo->add (iinfo, name, l);

      hdf5_load_attributes (iinfo->call, gid, l, name);
      UNPROTECT (1);

      if (H5Gclose (gid) < 0)
	errorcall (iinfo->call, "unable to close group");
    }
  else if (statbuf.type == H5G_DATASET)
    {
      hid_t dataset, space, tid;
      int rank;
      SEXPTYPE type = NILSXP;
      /*H5T_class_t class;*/

      if ((dataset = H5Dopen (id, name)) < 0)
	errorcall (iinfo->call, "unable to load dataset `%s'", name);

      if ((tid = H5Dget_type (dataset)) < 0)
	errorcall (iinfo->call, "unable to get dataset type");

      switch (H5Tget_class (tid))
	{
	case H5T_INTEGER:
	  if (H5Tget_precision (tid) == 1)
	    type = LGLSXP;
	  else
	    type = INTSXP;
	  break;
	case H5T_FLOAT:
	  type = REALSXP;
	  break;
	case H5T_STRING:
	  type = STRSXP;
	  break;
	case H5T_COMPOUND:
	  type = VECSXP;
	  break;
	default:
	  errorcall (iinfo->call, "can't handle hdf type %d", tid);
	  break;
	}

      if ((space = H5Dget_space (dataset)) < 0)
	errorcall (iinfo->call, "unable to get dataset space");

      if (H5Sis_simple (space) != LTRUE)
	errorcall (iinfo->call, "space not simple");

      if ((rank = H5Sget_simple_extent_ndims (space)) < 0)
	errorcall (iinfo->call, "unable to get space rank");

      {
	hsize_t dims[rank];
	hsize_t maxdims[rank];

	if (H5Sget_simple_extent_dims (space, dims, maxdims) < 0)
	  errorcall (iinfo->call, "unable to get space extent");

	if (type == VECSXP && rank == 1)
	  {
	    unsigned colcount = H5Tget_nmembers (tid), ci;
	    SEXP vec;
	    size_t size = H5Tget_size (tid);
	    unsigned ri, rowcount = dims[0];
	    char buf[rowcount][size];
	    hid_t rtid = make_sexp_ref_type (iinfo->call);
	    SEXP names;

	    if (H5Dread (dataset, tid, space, space, H5P_DEFAULT,
			 buf) < 0)
	      errorcall (iinfo->call, "can't read compound data vector");

	    PROTECT (vec = allocVector (VECSXP, colcount));
	    PROTECT (names = allocVector (STRSXP, colcount));

	    for (ci = 0; ci < colcount; ci++)
	      {
		hid_t ctid = H5Tget_member_type (tid, ci);
		H5T_class_t class = H5Tget_class (ctid);
		size_t csize = H5Tget_size (ctid);
		size_t coffset = H5Tget_member_offset (tid, ci);
		SEXPREC **rowptr = &VECTOR (vec)[ci];
		unsigned char itembuf[size]; /* for overrun */

#define VECLOOP(vectype, vecref, dtid) \
  { \
    size_t dsize = H5Tget_size (dtid); \
    for (ri = 0; ri < rowcount; ri++) \
      { \
	memcpy (itembuf, &buf[ri][coffset], csize); \
	if (H5Tconvert (ctid, dtid, 1, itembuf, NULL, H5P_DEFAULT) < 0) \
	  errorcall (iinfo->call, "type conversion failed"); \
	memcpy (&vecref (*rowptr)[ri], itembuf, dsize); \
      } \
  }

		{
		  char *colname = H5Tget_member_name (tid, ci);

		  if (colname)
		    STRING (names)[ci] = mkChar (colname);
		}
		switch (class) {
		  case H5T_INTEGER:
		    {
		      SEXPTYPE type = (csize == 1) ? LGLSXP : INTSXP;

		      *rowptr = allocVector (type, rowcount);
		      VECLOOP (type, INTEGER, H5T_NATIVE_INT);
		    }
		    break;
		  case H5T_FLOAT:
		    *rowptr = allocVector (REALSXP, rowcount);
		    VECLOOP (REALSXP, REAL, H5T_NATIVE_DOUBLE);
		    break;
		  case H5T_STRING:
		    *rowptr = allocVector (STRSXP, rowcount);
		    VECLOOP (STRSXP, STRING, rtid);
		    break;
		  default:
		    errorcall (iinfo->call, "can't handle hdf class %d",
			       class);
		  }
	      }
	    load_rownames_dataset_attribute (iinfo->call, dataset, vec);
	    setAttrib (vec, R_NamesSymbol, names);
	    UNPROTECT (1);
	    setAttrib (vec, R_ClassSymbol, mkString ("data.frame"));
	    iinfo->add (iinfo, name, vec);
	    hdf5_load_attributes (iinfo->call, dataset, vec, name);
	    UNPROTECT (1);
	    if (H5Tclose (rtid) < 0)
	      errorcall (iinfo->call, "could not close reference type");
	  }
	else
	  {
	    SEXP obj;

	    PROTECT (obj = ((rank == 1)
			    ? allocVector (type, dims[0])
			    : allocMatrix (type, dims[0], dims[1])));
	    vector_io (iinfo->call, LFALSE, dataset, space, obj);
	    iinfo->add (iinfo, name, obj);
	    hdf5_load_attributes (iinfo->call, dataset, obj, name);
	    UNPROTECT (1);
	  }
      }
      if (H5Sclose (space) < 0)
	errorcall (iinfo->call, "unable to close dataspace");
      if (H5Tclose (tid) < 0)
	errorcall (iinfo->call, "unable to close datatype");
      if (H5Dclose (dataset) < 0)
	errorcall (iinfo->call, "unable to close dataset");
    }
  else
    errorcall (iinfo->call, "no support for HDF object type: %d",
	       statbuf.type);
  return 0;
}

static void
add_to_symbol_table (struct hdf5_iterate_info *iinfo,
		     const char *name,
		     SEXP obj)
{
  setVar (install ((char *)name), obj, iinfo->env);
}

static void
add_to_return_list (struct hdf5_iterate_info *iinfo,
		    const char *name,
		    SEXP obj)
{
  PROTECT (iinfo->ret);
  iinfo->ret = CONS (obj, iinfo->ret);
  TAG (iinfo->ret) = install ((char *) name);
  UNPROTECT (1);
}

SEXP do_hdf5load (SEXP call, SEXP op, SEXP args, SEXP env)
{
  const char *path;
  hid_t fid;
  int restore_syms;
  struct hdf5_iterate_info iinfo;

  checkArity (op, args);

  if (!isValidString(CAR(args)))
    errorcall (call, "first argument must be a pathname\n");

  if (TYPEOF (CADR (args)) != LGLSXP)
    errorcall (call, "second argument must be a logical vector");

  path = CHAR(STRING (CAR(args))[0]);
  restore_syms = INTEGER (CADR(args))[0];

  H5dont_atexit ();

  if ((fid = H5Fopen (path, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
    errorcall (call, "unable to open HDF file: %s", path);

  if (H5Tregister (H5T_PERS_SOFT,
		   STRING2REF_CONV,
		   H5T_C_S1,
		   H5T_STD_REF_OBJ, string_ref) < 0)
    errorcall (call, "Unable to register string->ref converter");

  iinfo.call = call;
  iinfo.add = restore_syms ? add_to_symbol_table : add_to_return_list;
  iinfo.env = env;
  iinfo.ret = R_NilValue;

  if (H5Giterate (fid, "/", NULL, hdf5_process_object, &iinfo) < 0)
    errorcall (call, "unable to iterate over HDF file: %s", path);

  if (H5Tunregister (H5T_PERS_SOFT, STRING2REF_CONV, -1, -1, string_ref) < 0)
    errorcall (call, "Unable to unregister string->ref converter");

  if (H5Fclose (fid) < 0)
    errorcall (call, "unable to close HDF file");

  return iinfo.ret;
}

#else

SEXP do_hdf5save (SEXP call, SEXP op, SEXP args, SEXP env)
{
    errorcall(call, "HDF5 support unavailable");
    return(R_NilValue);/* -Wall */
}
SEXP do_hdf5load (SEXP call, SEXP op, SEXP args, SEXP env)
{
    errorcall(call, "HDF5 support unavailable");
    return(R_NilValue);/* -Wall */
}
#endif
