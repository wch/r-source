/*
 *  AUTHORS
 *	Catherine Loader, catherine@research.bell-labs.com, October 23, 2000. [ bd0() ]
 *	Morten Welinder, see Bugzilla PR#15628, 2014                          [ebd0() ]
 *
 *  Merge in to R (and much more):
 *
 *	Copyright (C) 2000-2025 The R Core Team
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
 *
 *
 *  DESCRIPTION
 *	Evaluates the "deviance part"
 *	bd0(x,M) :=  M * D0(x/M) = M*[ x/M * log(x/M) + 1 - (x/M) ] =
 *		  =  x * log(x/M) + M - x
 *	where M = E[X] = n*p (or = lambda), for	  x, M > 0
 *
 *	in a manner that should be stable (with small relative error)
 *	for all x and M=np. In particular for x/np close to 1, direct
 *	evaluation fails, and evaluation is based on the Taylor series
 *	of log((1+v)/(1-v)) with v = (x-M)/(x+M) = (x-np)/(x+np).
 *
 * Martyn Plummer had the nice idea to use log1p() and Martin Maechler
 * emphasized the extra need to control cancellation.
 *
 * MP:   t := (x-M)/M  ( <==> 1+t = x/M  ==>
 *
 * bd0 = M*[ x/M * log(x/M) + 1 - (x/M) ] = M*[ (1+t)*log1p(t) + 1 - (1+t) ]
 *     = M*[ (1+t)*log1p(t) - t ] =: M * p1log1pm(t) =: M * p1l1(t)
 * MM: The above is very nice, as the "simple" p1l1() function would be useful
 *    to have available in a fast numerical stable way more generally.
 */
#include "nmath.h"

attribute_hidden double bd0(double x, double np)
{
    if(!R_FINITE(x) || !R_FINITE(np) || np == 0.0) ML_WARN_return_NAN;

    if (fabs(x-np) < 0.1*(x+np)) {
    	double d = x - np,
	    v = d/(x+np);
	if((d != 0.0)  && (v == 0.0)) {  // v has underflown to 0 (as  x+np = inf)
	    double
		x_ = ldexp(x, -2),
		n_ = ldexp(np,-2);
	    v = (x_ - n_)/(x_ + n_);
	}
	double s = ldexp(d, -1) * v; // was d * v
	if(fabs(ldexp(s, 1)) < DBL_MIN) return ldexp(s, 1);
	double ej = x * v; // as 2*x*v could overflow:  v > 1/2  <==> ej = 2xv > x
	v *= v; // "v = v^2"
	for (int j = 1; j < 1000; j++) { /* Taylor series; 1000: no infinite loop
					    as |v| < .1,  v^2000 is "zero" */
	    ej *= v;// = x v^(2j+1)
	    double s_ = s;
	    s += ej/((j<<1)+1);
	    if (s == s_) { /* last term was effectively 0 */
#ifdef DEBUG_bd0
		REprintf("bd0(%g, %g): T.series w/ %d terms -> bd0=%g\n", x, np, j, ldexp(s, 1));
#endif
		return ldexp(s, 1); // 2*s ; as we dropped '2 *' above
	    }
	}
	/* ---- the following should _never_ happen ------------ */
	MATHLIB_WARNING4("bd0(%g, %g): T.series failed to converge in 1000 it.; s=%g, ej/(2j+1)=%g\n",
			 x, np, s, ej/((1000<<1)+1));
    }
    /* else:  | x - np |  is not too small */
    /* NB: x/np |--> inf (overflow)  doesn't happen when called from rpois_raw() */
#define lg_x_n (R_FINITE(x/np) ? log(x/np) : (log(x) - log(np)))
    return (x > np) ? x*(lg_x_n -1.) + np
	            : x* lg_x_n + np -x;
#undef lg_x_n
}


// ebd0(): R Bugzilla PR#15628 -- proposed accuracy improvement by Morten Welinder

/*
 * A table of logs for scaling purposes.  Each value has four parts with
 * 23 bits in each.  That means each part can be multiplied by a double
 * with at most 30 bits set and not have any rounding error.  Note, that
 * the first entry is log(2).
 *
 * Entry i is associated with the value r = 0.5 + i / 256.0.  The
 * argument to log is p/q where q=1024 and p=floor(q / r + 0.5).
 * Thus r*p/q is close to 1.
 */
static const float bd0_scale[128 + 1][4] = {
	{ +0x1.62e430p-1, -0x1.05c610p-29, -0x1.950d88p-54, +0x1.d9cc02p-79 }, /* 128: log(2048/1024.) */
	{ +0x1.5ee02cp-1, -0x1.6dbe98p-25, -0x1.51e540p-50, +0x1.2bfa48p-74 }, /* 129: log(2032/1024.) */
	{ +0x1.5ad404p-1, +0x1.86b3e4p-26, +0x1.9f6534p-50, +0x1.54be04p-74 }, /* 130: log(2016/1024.) */
	{ +0x1.570124p-1, -0x1.9ed750p-25, -0x1.f37dd0p-51, +0x1.10b770p-77 }, /* 131: log(2001/1024.) */
	{ +0x1.5326e4p-1, -0x1.9b9874p-25, -0x1.378194p-49, +0x1.56feb2p-74 }, /* 132: log(1986/1024.) */
	{ +0x1.4f4528p-1, +0x1.aca70cp-28, +0x1.103e74p-53, +0x1.9c410ap-81 }, /* 133: log(1971/1024.) */
	{ +0x1.4b5bd8p-1, -0x1.6a91d8p-25, -0x1.8e43d0p-50, -0x1.afba9ep-77 }, /* 134: log(1956/1024.) */
	{ +0x1.47ae54p-1, -0x1.abb51cp-25, +0x1.19b798p-51, +0x1.45e09cp-76 }, /* 135: log(1942/1024.) */
	{ +0x1.43fa00p-1, -0x1.d06318p-25, -0x1.8858d8p-49, -0x1.1927c4p-75 }, /* 136: log(1928/1024.) */
	{ +0x1.3ffa40p-1, +0x1.1a427cp-25, +0x1.151640p-53, -0x1.4f5606p-77 }, /* 137: log(1913/1024.) */
	{ +0x1.3c7c80p-1, -0x1.19bf48p-34, +0x1.05fc94p-58, -0x1.c096fcp-82 }, /* 138: log(1900/1024.) */
	{ +0x1.38b320p-1, +0x1.6b5778p-25, +0x1.be38d0p-50, -0x1.075e96p-74 }, /* 139: log(1886/1024.) */
	{ +0x1.34e288p-1, +0x1.d9ce1cp-25, +0x1.316eb8p-49, +0x1.2d885cp-73 }, /* 140: log(1872/1024.) */
	{ +0x1.315124p-1, +0x1.c2fc60p-29, -0x1.4396fcp-53, +0x1.acf376p-78 }, /* 141: log(1859/1024.) */
	{ +0x1.2db954p-1, +0x1.720de4p-25, -0x1.d39b04p-49, -0x1.f11176p-76 }, /* 142: log(1846/1024.) */
	{ +0x1.2a1b08p-1, -0x1.562494p-25, +0x1.a7863cp-49, +0x1.85dd64p-73 }, /* 143: log(1833/1024.) */
	{ +0x1.267620p-1, +0x1.3430e0p-29, -0x1.96a958p-56, +0x1.f8e636p-82 }, /* 144: log(1820/1024.) */
	{ +0x1.23130cp-1, +0x1.7bebf4p-25, +0x1.416f1cp-52, -0x1.78dd36p-77 }, /* 145: log(1808/1024.) */
	{ +0x1.1faa34p-1, +0x1.70e128p-26, +0x1.81817cp-50, -0x1.c2179cp-76 }, /* 146: log(1796/1024.) */
	{ +0x1.1bf204p-1, +0x1.3a9620p-28, +0x1.2f94c0p-52, +0x1.9096c0p-76 }, /* 147: log(1783/1024.) */
	{ +0x1.187ce4p-1, -0x1.077870p-27, +0x1.655a80p-51, +0x1.eaafd6p-78 }, /* 148: log(1771/1024.) */
	{ +0x1.1501c0p-1, -0x1.406cacp-25, -0x1.e72290p-49, +0x1.5dd800p-73 }, /* 149: log(1759/1024.) */
	{ +0x1.11cb80p-1, +0x1.787cd0p-25, -0x1.efdc78p-51, -0x1.5380cep-77 }, /* 150: log(1748/1024.) */
	{ +0x1.0e4498p-1, +0x1.747324p-27, -0x1.024548p-51, +0x1.77a5a6p-75 }, /* 151: log(1736/1024.) */
	{ +0x1.0b036cp-1, +0x1.690c74p-25, +0x1.5d0cc4p-50, -0x1.c0e23cp-76 }, /* 152: log(1725/1024.) */
	{ +0x1.077070p-1, -0x1.a769bcp-27, +0x1.452234p-52, +0x1.6ba668p-76 }, /* 153: log(1713/1024.) */
	{ +0x1.04240cp-1, -0x1.a686acp-27, -0x1.ef46b0p-52, -0x1.5ce10cp-76 }, /* 154: log(1702/1024.) */
	{ +0x1.00d22cp-1, +0x1.fc0e10p-25, +0x1.6ee034p-50, -0x1.19a2ccp-74 }, /* 155: log(1691/1024.) */
	{ +0x1.faf588p-2, +0x1.ef1e64p-27, -0x1.26504cp-54, -0x1.b15792p-82 }, /* 156: log(1680/1024.) */
	{ +0x1.f4d87cp-2, +0x1.d7b980p-26, -0x1.a114d8p-50, +0x1.9758c6p-75 }, /* 157: log(1670/1024.) */
	{ +0x1.ee1414p-2, +0x1.2ec060p-26, +0x1.dc00fcp-52, +0x1.f8833cp-76 }, /* 158: log(1659/1024.) */
	{ +0x1.e7e32cp-2, -0x1.ac796cp-27, -0x1.a68818p-54, +0x1.235d02p-78 }, /* 159: log(1649/1024.) */
	{ +0x1.e108a0p-2, -0x1.768ba4p-28, -0x1.f050a8p-52, +0x1.00d632p-82 }, /* 160: log(1638/1024.) */
	{ +0x1.dac354p-2, -0x1.d3a6acp-30, +0x1.18734cp-57, -0x1.f97902p-83 }, /* 161: log(1628/1024.) */
	{ +0x1.d47424p-2, +0x1.7dbbacp-31, -0x1.d5ada4p-56, +0x1.56fcaap-81 }, /* 162: log(1618/1024.) */
	{ +0x1.ce1af0p-2, +0x1.70be7cp-27, +0x1.6f6fa4p-51, +0x1.7955a2p-75 }, /* 163: log(1608/1024.) */
	{ +0x1.c7b798p-2, +0x1.ec36ecp-26, -0x1.07e294p-50, -0x1.ca183cp-75 }, /* 164: log(1598/1024.) */
	{ +0x1.c1ef04p-2, +0x1.c1dfd4p-26, +0x1.888eecp-50, -0x1.fd6b86p-75 }, /* 165: log(1589/1024.) */
	{ +0x1.bb7810p-2, +0x1.478bfcp-26, +0x1.245b8cp-50, +0x1.ea9d52p-74 }, /* 166: log(1579/1024.) */
	{ +0x1.b59da0p-2, -0x1.882b08p-27, +0x1.31573cp-53, -0x1.8c249ap-77 }, /* 167: log(1570/1024.) */
	{ +0x1.af1294p-2, -0x1.b710f4p-27, +0x1.622670p-51, +0x1.128578p-76 }, /* 168: log(1560/1024.) */
	{ +0x1.a925d4p-2, -0x1.0ae750p-27, +0x1.574ed4p-51, +0x1.084996p-75 }, /* 169: log(1551/1024.) */
	{ +0x1.a33040p-2, +0x1.027d30p-29, +0x1.b9a550p-53, -0x1.b2e38ap-78 }, /* 170: log(1542/1024.) */
	{ +0x1.9d31c0p-2, -0x1.5ec12cp-26, -0x1.5245e0p-52, +0x1.2522d0p-79 }, /* 171: log(1533/1024.) */
	{ +0x1.972a34p-2, +0x1.135158p-30, +0x1.a5c09cp-56, +0x1.24b70ep-80 }, /* 172: log(1524/1024.) */
	{ +0x1.911984p-2, +0x1.0995d4p-26, +0x1.3bfb5cp-50, +0x1.2c9dd6p-75 }, /* 173: log(1515/1024.) */
	{ +0x1.8bad98p-2, -0x1.1d6144p-29, +0x1.5b9208p-53, +0x1.1ec158p-77 }, /* 174: log(1507/1024.) */
	{ +0x1.858b58p-2, -0x1.1b4678p-27, +0x1.56cab4p-53, -0x1.2fdc0cp-78 }, /* 175: log(1498/1024.) */
	{ +0x1.7f5fa0p-2, +0x1.3aaf48p-27, +0x1.461964p-51, +0x1.4ae476p-75 }, /* 176: log(1489/1024.) */
	{ +0x1.79db68p-2, -0x1.7e5054p-26, +0x1.673750p-51, -0x1.a11f7ap-76 }, /* 177: log(1481/1024.) */
	{ +0x1.744f88p-2, -0x1.cc0e18p-26, -0x1.1e9d18p-50, -0x1.6c06bcp-78 }, /* 178: log(1473/1024.) */
	{ +0x1.6e08ecp-2, -0x1.5d45e0p-26, -0x1.c73ec8p-50, +0x1.318d72p-74 }, /* 179: log(1464/1024.) */
	{ +0x1.686c80p-2, +0x1.e9b14cp-26, -0x1.13bbd4p-50, -0x1.efeb1cp-78 }, /* 180: log(1456/1024.) */
	{ +0x1.62c830p-2, -0x1.a8c70cp-27, -0x1.5a1214p-51, -0x1.bab3fcp-79 }, /* 181: log(1448/1024.) */
	{ +0x1.5d1bdcp-2, -0x1.4fec6cp-31, +0x1.423638p-56, +0x1.ee3feep-83 }, /* 182: log(1440/1024.) */
	{ +0x1.576770p-2, +0x1.7455a8p-26, -0x1.3ab654p-50, -0x1.26be4cp-75 }, /* 183: log(1432/1024.) */
	{ +0x1.5262e0p-2, -0x1.146778p-26, -0x1.b9f708p-52, -0x1.294018p-77 }, /* 184: log(1425/1024.) */
	{ +0x1.4c9f08p-2, +0x1.e152c4p-26, -0x1.dde710p-53, +0x1.fd2208p-77 }, /* 185: log(1417/1024.) */
	{ +0x1.46d2d8p-2, +0x1.c28058p-26, -0x1.936284p-50, +0x1.9fdd68p-74 }, /* 186: log(1409/1024.) */
	{ +0x1.41b940p-2, +0x1.cce0c0p-26, -0x1.1a4050p-50, +0x1.bc0376p-76 }, /* 187: log(1402/1024.) */
	{ +0x1.3bdd24p-2, +0x1.d6296cp-27, +0x1.425b48p-51, -0x1.cddb2cp-77 }, /* 188: log(1394/1024.) */
	{ +0x1.36b578p-2, -0x1.287ddcp-27, -0x1.2d0f4cp-51, +0x1.38447ep-75 }, /* 189: log(1387/1024.) */
	{ +0x1.31871cp-2, +0x1.2a8830p-27, +0x1.3eae54p-52, -0x1.898136p-77 }, /* 190: log(1380/1024.) */
	{ +0x1.2b9304p-2, -0x1.51d8b8p-28, +0x1.27694cp-52, -0x1.fd852ap-76 }, /* 191: log(1372/1024.) */
	{ +0x1.265620p-2, -0x1.d98f3cp-27, +0x1.a44338p-51, -0x1.56e85ep-78 }, /* 192: log(1365/1024.) */
	{ +0x1.211254p-2, +0x1.986160p-26, +0x1.73c5d0p-51, +0x1.4a861ep-75 }, /* 193: log(1358/1024.) */
	{ +0x1.1bc794p-2, +0x1.fa3918p-27, +0x1.879c5cp-51, +0x1.16107cp-78 }, /* 194: log(1351/1024.) */
	{ +0x1.1675ccp-2, -0x1.4545a0p-26, +0x1.c07398p-51, +0x1.f55c42p-76 }, /* 195: log(1344/1024.) */
	{ +0x1.111ce4p-2, +0x1.f72670p-37, -0x1.b84b5cp-61, +0x1.a4a4dcp-85 }, /* 196: log(1337/1024.) */
	{ +0x1.0c81d4p-2, +0x1.0c150cp-27, +0x1.218600p-51, -0x1.d17312p-76 }, /* 197: log(1331/1024.) */
	{ +0x1.071b84p-2, +0x1.fcd590p-26, +0x1.a3a2e0p-51, +0x1.fe5ef8p-76 }, /* 198: log(1324/1024.) */
	{ +0x1.01ade4p-2, -0x1.bb1844p-28, +0x1.db3cccp-52, +0x1.1f56fcp-77 }, /* 199: log(1317/1024.) */
	{ +0x1.fa01c4p-3, -0x1.12a0d0p-29, -0x1.f71fb0p-54, +0x1.e287a4p-78 }, /* 200: log(1311/1024.) */
	{ +0x1.ef0adcp-3, +0x1.7b8b28p-28, -0x1.35bce4p-52, -0x1.abc8f8p-79 }, /* 201: log(1304/1024.) */
	{ +0x1.e598ecp-3, +0x1.5a87e4p-27, -0x1.134bd0p-51, +0x1.c2cebep-76 }, /* 202: log(1298/1024.) */
	{ +0x1.da85d8p-3, -0x1.df31b0p-27, +0x1.94c16cp-57, +0x1.8fd7eap-82 }, /* 203: log(1291/1024.) */
	{ +0x1.d0fb80p-3, -0x1.bb5434p-28, -0x1.ea5640p-52, -0x1.8ceca4p-77 }, /* 204: log(1285/1024.) */
	{ +0x1.c765b8p-3, +0x1.e4d68cp-27, +0x1.5b59b4p-51, +0x1.76f6c4p-76 }, /* 205: log(1279/1024.) */
	{ +0x1.bdc46cp-3, -0x1.1cbb50p-27, +0x1.2da010p-51, +0x1.eb282cp-75 }, /* 206: log(1273/1024.) */
	{ +0x1.b27980p-3, -0x1.1b9ce0p-27, +0x1.7756f8p-52, +0x1.2ff572p-76 }, /* 207: log(1266/1024.) */
	{ +0x1.a8bed0p-3, -0x1.bbe874p-30, +0x1.85cf20p-56, +0x1.b9cf18p-80 }, /* 208: log(1260/1024.) */
	{ +0x1.9ef83cp-3, +0x1.2769a4p-27, -0x1.85bda0p-52, +0x1.8c8018p-79 }, /* 209: log(1254/1024.) */
	{ +0x1.9525a8p-3, +0x1.cf456cp-27, -0x1.7137d8p-52, -0x1.f158e8p-76 }, /* 210: log(1248/1024.) */
	{ +0x1.8b46f8p-3, +0x1.11b12cp-30, +0x1.9f2104p-54, -0x1.22836ep-78 }, /* 211: log(1242/1024.) */
	{ +0x1.83040cp-3, +0x1.2379e4p-28, +0x1.b71c70p-52, -0x1.990cdep-76 }, /* 212: log(1237/1024.) */
	{ +0x1.790ed4p-3, +0x1.dc4c68p-28, -0x1.910ac8p-52, +0x1.dd1bd6p-76 }, /* 213: log(1231/1024.) */
	{ +0x1.6f0d28p-3, +0x1.5cad68p-28, +0x1.737c94p-52, -0x1.9184bap-77 }, /* 214: log(1225/1024.) */
	{ +0x1.64fee8p-3, +0x1.04bf88p-28, +0x1.6fca28p-52, +0x1.8884a8p-76 }, /* 215: log(1219/1024.) */
	{ +0x1.5c9400p-3, +0x1.d65cb0p-29, -0x1.b2919cp-53, +0x1.b99bcep-77 }, /* 216: log(1214/1024.) */
	{ +0x1.526e60p-3, -0x1.c5e4bcp-27, -0x1.0ba380p-52, +0x1.d6e3ccp-79 }, /* 217: log(1208/1024.) */
	{ +0x1.483bccp-3, +0x1.9cdc7cp-28, -0x1.5ad8dcp-54, -0x1.392d3cp-83 }, /* 218: log(1202/1024.) */
	{ +0x1.3fb25cp-3, -0x1.a6ad74p-27, +0x1.5be6b4p-52, -0x1.4e0114p-77 }, /* 219: log(1197/1024.) */
	{ +0x1.371fc4p-3, -0x1.fe1708p-27, -0x1.78864cp-52, -0x1.27543ap-76 }, /* 220: log(1192/1024.) */
	{ +0x1.2cca10p-3, -0x1.4141b4p-28, -0x1.ef191cp-52, +0x1.00ee08p-76 }, /* 221: log(1186/1024.) */
	{ +0x1.242310p-3, +0x1.3ba510p-27, -0x1.d003c8p-51, +0x1.162640p-76 }, /* 222: log(1181/1024.) */
	{ +0x1.1b72acp-3, +0x1.52f67cp-27, -0x1.fd6fa0p-51, +0x1.1a3966p-77 }, /* 223: log(1176/1024.) */
	{ +0x1.10f8e4p-3, +0x1.129cd8p-30, +0x1.31ef30p-55, +0x1.a73e38p-79 }, /* 224: log(1170/1024.) */
	{ +0x1.08338cp-3, -0x1.005d7cp-27, -0x1.661a9cp-51, +0x1.1f138ap-79 }, /* 225: log(1165/1024.) */
	{ +0x1.fec914p-4, -0x1.c482a8p-29, -0x1.55746cp-54, +0x1.99f932p-80 }, /* 226: log(1160/1024.) */
	{ +0x1.ed1794p-4, +0x1.d06f00p-29, +0x1.75e45cp-53, -0x1.d0483ep-78 }, /* 227: log(1155/1024.) */
	{ +0x1.db5270p-4, +0x1.87d928p-32, -0x1.0f52a4p-57, +0x1.81f4a6p-84 }, /* 228: log(1150/1024.) */
	{ +0x1.c97978p-4, +0x1.af1d24p-29, -0x1.0977d0p-60, -0x1.8839d0p-84 }, /* 229: log(1145/1024.) */
	{ +0x1.b78c84p-4, -0x1.44f124p-28, -0x1.ef7bc4p-52, +0x1.9e0650p-78 }, /* 230: log(1140/1024.) */
	{ +0x1.a58b60p-4, +0x1.856464p-29, +0x1.c651d0p-55, +0x1.b06b0cp-79 }, /* 231: log(1135/1024.) */
	{ +0x1.9375e4p-4, +0x1.5595ecp-28, +0x1.dc3738p-52, +0x1.86c89ap-81 }, /* 232: log(1130/1024.) */
	{ +0x1.814be4p-4, -0x1.c073fcp-28, -0x1.371f88p-53, -0x1.5f4080p-77 }, /* 233: log(1125/1024.) */
	{ +0x1.6f0d28p-4, +0x1.5cad68p-29, +0x1.737c94p-53, -0x1.9184bap-78 }, /* 234: log(1120/1024.) */
	{ +0x1.60658cp-4, -0x1.6c8af4p-28, +0x1.d8ef74p-55, +0x1.c4f792p-80 }, /* 235: log(1116/1024.) */
	{ +0x1.4e0110p-4, +0x1.146b5cp-29, +0x1.73f7ccp-54, -0x1.d28db8p-79 }, /* 236: log(1111/1024.) */
	{ +0x1.3b8758p-4, +0x1.8b1b70p-28, -0x1.20aca4p-52, -0x1.651894p-76 }, /* 237: log(1106/1024.) */
	{ +0x1.28f834p-4, +0x1.43b6a4p-30, -0x1.452af8p-55, +0x1.976892p-80 }, /* 238: log(1101/1024.) */
	{ +0x1.1a0fbcp-4, -0x1.e4075cp-28, +0x1.1fe618p-52, +0x1.9d6dc2p-77 }, /* 239: log(1097/1024.) */
	{ +0x1.075984p-4, -0x1.4ce370p-29, -0x1.d9fc98p-53, +0x1.4ccf12p-77 }, /* 240: log(1092/1024.) */
	{ +0x1.f0a30cp-5, +0x1.162a68p-37, -0x1.e83368p-61, -0x1.d222a6p-86 }, /* 241: log(1088/1024.) */
	{ +0x1.cae730p-5, -0x1.1a8f7cp-31, -0x1.5f9014p-55, +0x1.2720c0p-79 }, /* 242: log(1083/1024.) */
	{ +0x1.ac9724p-5, -0x1.e8ee08p-29, +0x1.a7de04p-54, -0x1.9bba74p-78 }, /* 243: log(1079/1024.) */
	{ +0x1.868a84p-5, -0x1.ef8128p-30, +0x1.dc5eccp-54, -0x1.58d250p-79 }, /* 244: log(1074/1024.) */
	{ +0x1.67f950p-5, -0x1.ed684cp-30, -0x1.f060c0p-55, -0x1.b1294cp-80 }, /* 245: log(1070/1024.) */
	{ +0x1.494accp-5, +0x1.a6c890p-32, -0x1.c3ad48p-56, -0x1.6dc66cp-84 }, /* 246: log(1066/1024.) */
	{ +0x1.22c71cp-5, -0x1.8abe2cp-32, -0x1.7e7078p-56, -0x1.ddc3dcp-86 }, /* 247: log(1061/1024.) */
	{ +0x1.03d5d8p-5, +0x1.79cfbcp-31, -0x1.da7c4cp-58, +0x1.4e7582p-83 }, /* 248: log(1057/1024.) */
	{ +0x1.c98d18p-6, +0x1.a01904p-31, -0x1.854164p-55, +0x1.883c36p-79 }, /* 249: log(1053/1024.) */
	{ +0x1.8b31fcp-6, -0x1.356500p-30, +0x1.c3ab48p-55, +0x1.b69bdap-80 }, /* 250: log(1049/1024.) */
	{ +0x1.3cea44p-6, +0x1.a352bcp-33, -0x1.8865acp-57, -0x1.48159cp-81 }, /* 251: log(1044/1024.) */
	{ +0x1.fc0a8cp-7, -0x1.e07f84p-32, +0x1.e7cf6cp-58, +0x1.3a69c0p-82 }, /* 252: log(1040/1024.) */
	{ +0x1.7dc474p-7, +0x1.f810a8p-31, -0x1.245b5cp-56, -0x1.a1f4f8p-80 }, /* 253: log(1036/1024.) */
	{ +0x1.fe02a8p-8, -0x1.4ef988p-32, +0x1.1f86ecp-57, +0x1.20723cp-81 }, /* 254: log(1032/1024.) */
	{ +0x1.ff00acp-9, -0x1.d4ef44p-33, +0x1.2821acp-63, +0x1.5a6d32p-87 }, /* 255: log(1028/1024.) */
	{ 0, 0, 0, 0 } /* log(1024/1024) = log(1) = 0 */
};


/*
 * Compute x * log (x / M) + (M - x)
 * aka -x * log1pmx ((M - x) / x)
 *
 * Deliver the result back in two parts, *yh and *yl.
 */
attribute_hidden void ebd0(double x, double M, double *yh, double *yl)
{
	const int Sb = 10;
	const double S = 1u << Sb; // = 2^10 = 1024
	const int N = 128; // == ? == G_N_ELEMENTS(bd0_scale) - 1; <<<< FIXME:

	*yl = *yh = 0;

	if (x == M) return;
	if (x == 0) { *yh = M;         return; }
	if (M == 0) { *yh = ML_POSINF; return; }

	if (M/x == ML_POSINF) { *yh = M; return; }//  as when (x == 0)

	int e;
	// NB: M/x overflow handled above; underflow should be handled by fg = Inf
	double r = frexp (M / x, &e); // => r in  [0.5, 1) and 'e' (int) such that  M/x = r * 2^e

	// prevent later overflow
	if (M_LN2 * ((double) -e)  > 1. + DBL_MAX / x) { *yh = ML_POSINF; return; }

	int i = (int) floor ((r - 0.5) * (2 * N) + 0.5);
	// now,  0 <= i <= N
	double f = floor (S / (0.5 + i / (2.0 * N)) + 0.5);
	double fg = ldexp (f, -(e + Sb)); // ldexp(f, E) := f * 2^E
#ifdef DEBUG_bd0
	REprintf("ebd0(x=%g, M=%g): M/x = (r=%.15g) * 2^(e=%d); i=%d,\n  f=%g, fg=f*2^-(e+%d)=%g\n",
		 x, M, r,e, i, f, Sb, fg);
	if (fg == ML_POSINF) {
	    REprintf(" --> fg = +Inf --> return( +Inf )\n");
	    *yh = fg; return;
	}
	REprintf("     bd0_sc[0][0..3]= ("); for(int j=0; j < 4; j++) REprintf("%g ", bd0_scale[0][j]); REprintf(")\n");
	REprintf("i -> bd0_sc[i][0..3]= ("); for(int j=0; j < 4; j++) REprintf("%g ", bd0_scale[i][j]); REprintf(")\n");
	REprintf( "  small(?)  (M*fg-x)/x = (M*fg)/x - 1 = %.16g\n", (M*fg-x)/x);
#else
	if (fg == ML_POSINF) {
	    *yh = fg; return;
	}
#endif
	/* We now have (M * fg / x) close to 1.  */

	/*
	 * We need to compute this:
	 * (x/M)^x * exp(M-x) =
	 * (M/x)^-x * exp(M-x) =
	 * (M*fg/x)^-x * (fg)^x * exp(M-x) =
	 * (M*fg/x)^-x * (fg)^x * exp(M*fg-x) * exp(M-M*fg)
	 *
	 * In log terms:
	 * log((x/M)^x * exp(M-x)) =
	 * log((M*fg/x)^-x * (fg)^x * exp(M*fg-x) * exp(M-M*fg)) =
	 * log((M*fg/x)^-x * exp(M*fg-x)) + x*log(fg) + (M-M*fg) =
	 * -x*log1pmx((M*fg-x)/x) + x*log(fg) + M - M*fg =
	 *
	 * Note, that fg has at most 10 bits.  If M and x are suitably
	 * "nice" -- such as being integers or half-integers -- then
	 * we can compute M*fg as well as x * bd0_scale[.][.] without
	 * rounding errors.
	 */

#define ADD1(d_) do {				\
   volatile double d = (d_);			\
	    double d1 = floor (d + 0.5);	\
	    double d2 = d - d1;/* in [-.5,.5) */ \
	    *yh += d1;				\
	    *yl += d2;				\
	} while(0)

#ifdef DEBUG_bd0
	{
	    double log1__ = log1pmx((M * fg - x) / x),
		xl = -x * log1__;
	    REprintf(" 1a. before adding  -x * log1pmx(.) = -x * %g = %g\n", log1__, xl);
	    ADD1(xl);
	    REprintf(" 1. after A.(-x*l..):       yl,yh = (%13g, %13g); yl+yh= %g\n",
		     *yl, *yh, (*yl)+(*yh));
	}
        if(fg == 1) {
            REprintf("___ fg = 1 ___ skipping further steps\n");
            return;
        }
	// else  [ fg != 1 ]
	REprintf(" 2:  A(x*b[i,j]) and A(-x*e*b[0,j]), j=1:4:\n");
	for (int j = 0; j < 4; j++) {
 	    ADD1( x * bd0_scale[i][j]);     // handles  x*log(fg*2^e)
	    REprintf(" j=%d: (%13g, %13g);", j, *yl, *yh);
	    ADD1(-x * bd0_scale[0][j] * e); // handles  x*log(1/ 2^e)
	    REprintf(" (%13g, %13g); yl+yh= %g\n", *yl, *yh, (*yl)+(*yh));
            if(!R_FINITE(*yh)) {
                REprintf(" non-finite yh --> return((yh=Inf, yl=0))\n");
		*yh = ML_POSINF; *yl = 0; return;
            }
	}
#else
	ADD1(-x * log1pmx ((M * fg - x) / x));
        if(fg == 1) return;
	// else (fg != 1) :
	for (int j = 0; j < 4; j++) {
	    ADD1( x * bd0_scale[i][j]);     // handles  x*log(fg*2^e)
	    ADD1(-x * bd0_scale[0][j] * e); // handles  x*log(1/ 2^e)
	    //                        ^^^ at end prevents overflow in  ebd0(1e307, 1e300)
            if(!R_FINITE(*yh)) { *yh = ML_POSINF; *yl = 0; return; }
	}
#endif

	ADD1(M);
#ifdef DEBUG_bd0
	REprintf(" 3. after ADD1(M):            yl,yh = (%13g, %13g); yl+yh= %g\n", *yl, *yh, (*yl)+(*yh));
#endif
	ADD1(-M * fg);
#ifdef DEBUG_bd0
	REprintf(" 4. after ADD1(- M*fg):       yl,yh = (%13g, %13g); yl+yh= %g\n\n", *yl, *yh, (*yl)+(*yh));
#endif
}

#undef ADD1
