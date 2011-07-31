/* 
   RngStream.h for ANSI C
   Version: 5 january 2010 

   See the header of RngStream.c
*/

#ifndef RNGSTREAM_H
#define RNGSTREAM_H
 
#ifdef __cplusplus
extern "C"{
#endif


typedef struct RngStream_InfoState * RngStream;

struct RngStream_InfoState {
   double Cg[6], Bg[6], Ig[6];
   int Anti;
   int IncPrec;
   char *name;
};


int RngStream_SetPackageSeed (unsigned long seed[6]);
RngStream RngStream_CreateStream (const char name[]);
void RngStream_DeleteStream (RngStream g);
void RngStream_ResetStartStream (RngStream g);
void RngStream_ResetStartSubstream (RngStream g);
void RngStream_ResetNextSubstream (RngStream g);
void RngStream_SetAntithetic (RngStream g, int a);
void RngStream_IncreasedPrecis (RngStream g, int incp);
int RngStream_SetSeed (RngStream g, unsigned long seed[6]);
void RngStream_AdvanceState (RngStream g, long e, long c);
void RngStream_GetState (RngStream g, unsigned long seed[6]);
void RngStream_WriteState (RngStream g);
void RngStream_WriteStateFull (RngStream g);
double RngStream_RandU01 (RngStream g);
int RngStream_RandInt (RngStream g, int i, int j);
 
        
#ifdef __cplusplus
}
#endif
        
#endif

 

