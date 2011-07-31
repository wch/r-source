/* From
   http://www.iro.umontreal.ca/~lecuyer/myftp/streams00/c2010/

   which contains the manual.
 */

/***********************************************************************\
 *
 * File:           RngStream.c for multiple streams of Random Numbers
 * Language:       ANSI C
 * Copyright:      Pierre L'Ecuyer, Université de Montréal
 * Notice:         This code can be used freely for personal, academic,
 *                 or non-commercial purposes. For commercial purposes, 
 *                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca
 *
 *                 Professor L'Ecuyer has agreed to its use under the
 *                 GPL licence.
 * Version:        5 January 2010
 *
\***********************************************************************/

/* This 2010 version uses int not long, return values not exit notably in 
   RngStream_SetPackageSeed, and a pointer arg in RngStream_DeleteStream.
 */

#include "RngStream.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*---------------------------------------------------------------------*/
/* Private part.                                                       */
/*---------------------------------------------------------------------*/


#define norm  2.328306549295727688e-10
#define m1    4294967087.0
#define m2    4294944443.0
#define a12     1403580.0
#define a13n     810728.0
#define a21      527612.0
#define a23n    1370589.0

#define two17   131072.0
#define two53   9007199254740992.0
#define fact  5.9604644775390625e-8    /* 1 / 2^24 */




/* Default initial seed of the package. Will be updated to become
   the seed of the next created stream. */
static double nextSeed[6] = { 12345, 12345, 12345, 12345, 12345, 12345 };


/* The following are the transition matrices of the two MRG components */
/* (in matrix form), raised to the powers -1, 1, 2^76, and 2^127, resp.*/
static double InvA1[3][3] = {          /* Inverse of A1p0 */
          { 184888585.0,   0.0,  1945170933.0 },
          {         1.0,   0.0,           0.0 },
          {         0.0,   1.0,           0.0 }
          };

static double InvA2[3][3] = {          /* Inverse of A2p0 */
          {      0.0,  360363334.0,  4225571728.0 },
          {      1.0,          0.0,           0.0 },
          {      0.0,          1.0,           0.0 }
          };

static double A1p0[3][3] = {
          {       0.0,        1.0,       0.0 },
          {       0.0,        0.0,       1.0 },
          { -810728.0,  1403580.0,       0.0 }
          };

static double A2p0[3][3] = {
          {        0.0,        1.0,       0.0 },
          {        0.0,        0.0,       1.0 },
          { -1370589.0,        0.0,  527612.0 }
          };

static double A1p76[3][3] = {
          {      82758667.0, 1871391091.0, 4127413238.0 }, 
          {    3672831523.0,   69195019.0, 1871391091.0 }, 
          {    3672091415.0, 3528743235.0,   69195019.0 }
          };

static double A2p76[3][3] = {
          {    1511326704.0, 3759209742.0, 1610795712.0 }, 
          {    4292754251.0, 1511326704.0, 3889917532.0 }, 
          {    3859662829.0, 4292754251.0, 3708466080.0 }
          };

static double A1p127[3][3] = {
          {    2427906178.0, 3580155704.0,  949770784.0 }, 
          {     226153695.0, 1230515664.0, 3580155704.0 },
          {    1988835001.0,  986791581.0, 1230515664.0 }
          };

static double A2p127[3][3] = {
          {    1464411153.0,  277697599.0, 1610723613.0 },
          {      32183930.0, 1464411153.0, 1022607788.0 },
          {    2824425944.0,   32183930.0, 2093834863.0 }
          };





/*-------------------------------------------------------------------------*/

static double MultModM (double a, double s, double c, double m)
   /* Compute (a*s + c) % m. m must be < 2^35.  Works also for s, c < 0 */
{
   double v;
   long a1;
   v = a * s + c;
   if ((v >= two53) || (v <= -two53)) {
      a1 = (long) (a / two17);
      a -= a1 * two17;
      v = a1 * s;
      a1 = (long) (v / m);
      v -= a1 * m;
      v = v * two17 + a * s + c;
   }
   a1 = (long) (v / m);
   if ((v -= a1 * m) < 0.0)
      return v += m;
   else
      return v;
}


/*-------------------------------------------------------------------------*/

static void MatVecModM (double A[3][3], double s[3], double v[3], double m)
   /* Returns v = A*s % m.  Assumes that -m < s[i] < m. */
   /* Works even if v = s. */
{
   int i;
   double x[3];
   for (i = 0; i < 3; ++i) {
      x[i] = MultModM (A[i][0], s[0], 0.0, m);
      x[i] = MultModM (A[i][1], s[1], x[i], m);
      x[i] = MultModM (A[i][2], s[2], x[i], m);
   }
   for (i = 0; i < 3; ++i)
      v[i] = x[i];
}


/*-------------------------------------------------------------------------*/

static void MatMatModM (double A[3][3], double B[3][3], double C[3][3],
                        double m)
   /* Returns C = A*B % m. Work even if A = C or B = C or A = B = C. */
{
   int i, j;
   double V[3], W[3][3];
   for (i = 0; i < 3; ++i) {
      for (j = 0; j < 3; ++j)
         V[j] = B[j][i];
      MatVecModM (A, V, V, m);
      for (j = 0; j < 3; ++j)
         W[j][i] = V[j];
   }
   for (i = 0; i < 3; ++i) {
      for (j = 0; j < 3; ++j)
         C[i][j] = W[i][j];
   }
}


/*-------------------------------------------------------------------------*/

static void MatTwoPowModM (double A[3][3], double B[3][3], double m, long e)
  /* Compute matrix B = (A^(2^e) % m);  works even if A = B */
{
   int i, j;

   /* initialize: B = A */
   if (A != B) {
      for (i = 0; i < 3; i++) {
         for (j = 0; j < 3; ++j)
            B[i][j] = A[i][j];
      }
   }
   /* Compute B = A^{2^e} */
   for (i = 0; i < e; i++)
      MatMatModM (B, B, B, m);
}


/*-------------------------------------------------------------------------*/

static void MatPowModM (double A[3][3], double B[3][3], double m, long n)
   /* Compute matrix B = A^n % m ;  works even if A = B */
{
   int i, j;
   double W[3][3];

   /* initialize: W = A; B = I */
   for (i = 0; i < 3; i++) {
      for (j = 0; j < 3; ++j) {
         W[i][j] = A[i][j];
         B[i][j] = 0.0;
      }
   }
   for (j = 0; j < 3; ++j)
      B[j][j] = 1.0;

   /* Compute B = A^n % m using the binary decomposition of n */
   while (n > 0) {
      if (n % 2)
         MatMatModM (W, B, B, m);
      MatMatModM (W, W, W, m);
      n /= 2;
   }
}


/*-------------------------------------------------------------------------*/

static double U01 (RngStream g)
{
   long k;
   double p1, p2, u;

   /* Component 1 */
   p1 = a12 * g->Cg[1] - a13n * g->Cg[0];
   k = p1 / m1;
   p1 -= k * m1;
   if (p1 < 0.0)
      p1 += m1;
   g->Cg[0] = g->Cg[1];
   g->Cg[1] = g->Cg[2];
   g->Cg[2] = p1;

   /* Component 2 */
   p2 = a21 * g->Cg[5] - a23n * g->Cg[3];
   k = p2 / m2;
   p2 -= k * m2;
   if (p2 < 0.0)
      p2 += m2;
   g->Cg[3] = g->Cg[4];
   g->Cg[4] = g->Cg[5];
   g->Cg[5] = p2;

   /* Combination */
   u = ((p1 > p2) ? (p1 - p2) * norm : (p1 - p2 + m1) * norm);
   return (g->Anti) ? (1 - u) : u;
}


/*-------------------------------------------------------------------------*/

static double U01d (RngStream g)
{
   double u;
   u = U01(g);
   if (g->Anti == 0) {
      u += U01(g) * fact;
      return (u < 1.0) ? u : (u - 1.0);
   } else {
      /* Don't forget that U01() returns 1 - u in the antithetic case */
      u += (U01(g) - 1.0) * fact;
      return (u < 0.0) ? u + 1.0 : u;
   }
}


/*-------------------------------------------------------------------------*/

static int CheckSeed (unsigned long seed[6])
{
   /* Check that the seeds are legitimate values. Returns 0 if legal seeds,
     -1 otherwise */
   int i;

   for (i = 0; i < 3; ++i) {
      if (seed[i] >= m1) {
	 fprintf (stderr, "****************************************\n"
		 "ERROR: Seed[%1d] >= m1, Seed is not set.\n"
		 "****************************************\n\n", i);
	 return (-1);
       }
   }
   for (i = 3; i < 6; ++i) {
      if (seed[i] >= m2) {
	 fprintf (stderr, "****************************************\n"
		 "ERROR: Seed[%1d] >= m1, Seed is not set.\n"
		 "****************************************\n\n", i);
	 return (-1);
       }
   }
   if (seed[0] == 0 && seed[1] == 0 && seed[2] == 0) {
      fprintf (stderr, "****************************\n"
	      "ERROR: First 3 seeds = 0.\n"
	      "****************************\n\n");
      return (-1);
   }
   if (seed[3] == 0 && seed[4] == 0 && seed[5] == 0) {
      fprintf (stderr, "****************************\n"
	      "ERROR: Last 3 seeds = 0.\n"
	      "****************************\n\n");
      return (-1);
   }

   return 0;
}


/*---------------------------------------------------------------------*/
/* Public part.                                                        */
/*---------------------------------------------------------------------*/


RngStream RngStream_CreateStream (const char name[])
{
   int i;
   RngStream g;
   size_t len;

   g = (RngStream) malloc (sizeof (struct RngStream_InfoState));
   if (g == NULL) {
      printf ("RngStream_CreateStream: No more memory\n\n");
      exit (EXIT_FAILURE);
   }
   if (name) {
      len = strlen (name);
      g->name = (char *) malloc ((len + 1) * sizeof (char));
      strncpy (g->name, name, len + 1);
   } else
      g->name = 0;
   g->Anti = 0;
   g->IncPrec = 0;

   for (i = 0; i < 6; ++i) {
      g->Bg[i] = g->Cg[i] = g->Ig[i] = nextSeed[i];
   }
   MatVecModM (A1p127, nextSeed, nextSeed, m1);
   MatVecModM (A2p127, &nextSeed[3], &nextSeed[3], m2);
   return g;
}

/*-------------------------------------------------------------------------*/

void RngStream_DeleteStream (RngStream g)
{
   if (g == NULL)
      return;
   if (g->name != NULL)
      free (g->name);
   free (g);
}

/*-------------------------------------------------------------------------*/

void RngStream_ResetStartStream (RngStream g)
{
   int i;
   for (i = 0; i < 6; ++i)
      g->Cg[i] = g->Bg[i] = g->Ig[i];
}

/*-------------------------------------------------------------------------*/

void RngStream_ResetNextSubstream (RngStream g)
{
   int i;
   MatVecModM (A1p76, g->Bg, g->Bg, m1);
   MatVecModM (A2p76, &g->Bg[3], &g->Bg[3], m2);
   for (i = 0; i < 6; ++i)
      g->Cg[i] = g->Bg[i];
}

/*-------------------------------------------------------------------------*/

void RngStream_ResetStartSubstream (RngStream g)
{
   int i;
   for (i = 0; i < 6; ++i)
      g->Cg[i] = g->Bg[i];
}

/*-------------------------------------------------------------------------*/

int RngStream_SetPackageSeed (unsigned long seed[6])
{
   int i;
   if (CheckSeed (seed))
      return -1;                    /* FAILURE */
   for (i = 0; i < 6; ++i)
      nextSeed[i] = seed[i];
   return 0;                       /* SUCCESS */
}

/*-------------------------------------------------------------------------*/

int RngStream_SetSeed (RngStream g, unsigned long seed[6])
{
   int i;
   if (CheckSeed (seed))
      return -1;                    /* FAILURE */
   for (i = 0; i < 6; ++i)
      g->Cg[i] = g->Bg[i] = g->Ig[i] = seed[i];
   return 0;                       /* SUCCESS */ 
}

/*-------------------------------------------------------------------------*/

void RngStream_AdvanceState (RngStream g, long e, long c)
{
   double B1[3][3], C1[3][3], B2[3][3], C2[3][3];

   if (e > 0) {
      MatTwoPowModM (A1p0, B1, m1, e);
      MatTwoPowModM (A2p0, B2, m2, e);
   } else if (e < 0) {
      MatTwoPowModM (InvA1, B1, m1, -e);
      MatTwoPowModM (InvA2, B2, m2, -e);
   }

   if (c >= 0) {
      MatPowModM (A1p0, C1, m1, c);
      MatPowModM (A2p0, C2, m2, c);
   } else {
      MatPowModM (InvA1, C1, m1, -c);
      MatPowModM (InvA2, C2, m2, -c);
   }

   if (e) {
      MatMatModM (B1, C1, C1, m1);
      MatMatModM (B2, C2, C2, m2);
   }

   MatVecModM (C1, g->Cg, g->Cg, m1);
   MatVecModM (C2, &g->Cg[3], &g->Cg[3], m2);
}

/*-------------------------------------------------------------------------*/

void RngStream_GetState (RngStream g, unsigned long seed[6])
{
   int i;
   for (i = 0; i < 6; ++i)
      seed[i] = g->Cg[i];
}

/*-------------------------------------------------------------------------*/

void RngStream_WriteState (RngStream g)
{
   int i;
   if (g == NULL)
      return;
   printf ("The current state of the Rngstream");
   if (g->name && (strlen (g->name) > 0))
      printf (" %s", g->name);
   printf (":\n   Cg = { ");

   for (i = 0; i < 5; i++) {
      printf ("%lu, ", (unsigned long) g->Cg[i]);
   }
   printf ("%lu }\n\n", (unsigned long) g->Cg[5]);
}

/*-------------------------------------------------------------------------*/

void RngStream_WriteStateFull (RngStream g)
{
   int i;
   if (g == NULL)
      return;
   printf ("The RngStream");
   if (g->name && (strlen (g->name) > 0))
      printf (" %s", g->name);
   printf (":\n   Anti = %s\n", (g->Anti ? "true" : "false"));
   printf ("   IncPrec = %s\n", (g->IncPrec ? "true" : "false"));

   printf ("   Ig = { ");
   for (i = 0; i < 5; i++) {
      printf ("%lu, ", (unsigned long) (g->Ig[i]));
   }
   printf ("%lu }\n", (unsigned long) g->Ig[5]);

   printf ("   Bg = { ");
   for (i = 0; i < 5; i++) {
      printf ("%lu, ", (unsigned long) (g->Bg[i]));
   }
   printf ("%lu }\n", (unsigned long) g->Bg[5]);

   printf ("   Cg = { ");
   for (i = 0; i < 5; i++) {
      printf ("%lu, ", (unsigned long) (g->Cg[i]));
   }
   printf ("%lu }\n\n", (unsigned long) g->Cg[5]);
}

/*-------------------------------------------------------------------------*/

void RngStream_IncreasedPrecis (RngStream g, int incp)
{
   g->IncPrec = incp;
}

/*-------------------------------------------------------------------------*/

void RngStream_SetAntithetic (RngStream g, int a)
{
   g->Anti = a;
}

/*-------------------------------------------------------------------------*/

double RngStream_RandU01 (RngStream g)
{
   if (g->IncPrec)
      return U01d (g);
   else
      return U01 (g);
}

/*-------------------------------------------------------------------------*/

int RngStream_RandInt (RngStream g, int i, int j)
{
   return i + (int) ((j - i + 1.0) * RngStream_RandU01 (g));
}
