      SUBROUTINE SWILK (INIT, X, N, N1, N2, A, W, PW, IFAULT)
C
C        ALGORITHM AS R94 APPL. STATIST. (1995) VOL.44, NO.4
C
C        Calculates the Shapiro-Wilk W test and its significance level
C
      INTEGER N, N1, N2, IFAULT
      REAL X(*), A(*), PW, W
      REAL C1(6), C2(6), C3(4), C4(4), C5(4), C6(3), C7(2)
      REAL C8(2), C9(2), G(2)
      REAL Z90, Z95, Z99, ZM, ZSS, BF1, XX90, XX95, ZERO, ONE, TWO
      REAL THREE, SQRTH, QTR, TH, SMALL, PI6, STQR
      REAL SUMM2, SSUMM2, FAC, RSN, AN, AN25, A1, A2, DELTA, RANGE
      REAL SA, SX, SSX, SSA, SAX, ASA, XSX, SSASSX, W1, Y, XX, XI
      REAL GAMMA, M, S, LD, BF, Z90F, Z95F, Z99F, ZFM, ZSD, ZBAR
C      
C        Auxiliary routines
C
      REAL POLY
C      
      INTEGER NCENS, NN2, I, I1, J
      LOGICAL INIT, UPPER
C
      DATA C1 /0.0E0, 0.221157E0, -0.147981E0, -0.207119E1,
     *     0.4434685E1, -0.2706056E1/
      DATA C2 /0.0E0, 0.42981E-1, -0.293762E0, -0.1752461E1,
     *     0.5682633E1, -0.3582633E1/
      DATA C3 /0.5440E0, -0.39978E0, 0.25054E-1, -0.6714E-3/
      DATA C4 /0.13822E1, -0.77857E0, 0.62767E-1, -0.20322E-2/
      DATA C5 /-0.15861E1, -0.31082E0, -0.83751E-1, 0.38915E-2/
      DATA C6 /-0.4803E0, -0.82676E-1, 0.30302E-2/
      DATA C7 /0.164E0, 0.533E0/
      DATA C8 /0.1736E0, 0.315E0/
      DATA C9 /0.256E0, -0.635E-2/
      DATA G  /-0.2273E1, 0.459E0/
      DATA Z90, Z95, Z99 /0.12816E1, 0.16449E1, 0.23263E1/
      DATA ZM, ZSS /0.17509E1, 0.56268E0/
      DATA BF1 /0.8378E0/, XX90, XX95 /0.556E0, 0.622E0/
      DATA ZERO /0.0E0/, ONE/1.0E0/, TWO/2.0E0/, THREE/3.0E0/
      DATA SQRTH /0.70711E0/, QTR/0.25E0/, TH/0.375E0/, SMALL/1E-19/
      DATA PI6 /0.1909859E1/, STQR/0.1047198E1/, UPPER/.TRUE./
C
      PW  =  ONE
      IF (W .GE. ZERO) W = ONE
      AN = N
      IFAULT = 3
      NN2 = N/2
      IF (N2 .LT. NN2) RETURN
      IFAULT = 1
      IF (N .LT. 3) RETURN
C
C        If INIT is false, calculates coefficients for the test
C
      IF (.NOT. INIT) THEN
	 IF (N .EQ. 3) THEN
	    A(1) = SQRTH
	 ELSE
	    AN25 = AN + QTR
	    SUMM2 = ZERO
	    DO 30 I = 1, N2
	       A(I) = real(PPND(dble((I - TH)/AN25), IER))
	       SUMM2 = SUMM2 + A(I) ** 2
30          CONTINUE                
	    SUMM2 = SUMM2 * TWO
	    SSUMM2 = SQRT(SUMM2)
	    RSN = ONE / SQRT(AN)
	    A1 = POLY(C1, 6, RSN) - A(1) / SSUMM2
C
C        Normalize coefficients
C
	    IF (N .GT. 5) THEN
	       I1 = 3
	       A2 = -A(2)/SSUMM2 + POLY(C2,6,RSN)
	       FAC = SQRT((SUMM2 - TWO * A(1) ** 2 - TWO *
     *               A(2) ** 2)/(ONE - TWO * A1 ** 2 - TWO * A2 ** 2))
	       A(1) = A1
	       A(2) = A2
	    ELSE
	       I1 = 2
	       FAC = SQRT((SUMM2 - TWO * A(1) ** 2)/
     *                   (ONE - TWO * A1 ** 2))
	       A(1) = A1
	    END IF
	    DO 40 I = I1, NN2
	       A(I) = -A(I)/FAC
   40       CONTINUE
	 END IF
	 INIT = .TRUE.
      END IF
      IF (N1 .LT. 3) RETURN
      NCENS = N - N1
      IFAULT = 4
      IF (NCENS .LT. 0 .OR. (NCENS .GT. 0 .AND. N .LT. 20)) RETURN
      IFAULT = 5
      DELTA = FLOAT(NCENS)/AN
      IF (DELTA .GT. 0.8) RETURN
C
C        If W input as negative, calculate significance level of -W
C
      IF (W .LT. ZERO) THEN
	 W1 = ONE + W
	 IFAULT = 0
	 GOTO 70
      END IF
C
C        Check for zero range
C
      IFAULT = 6
      RANGE = X(N1) - X(1)
      IF (RANGE .LT. SMALL) RETURN
C
C        Check for correct sort order on range - scaled X
C
      IFAULT = 7
      XX = X(1)/RANGE
      SX = XX
      SA = -A(1)
      J = N - 1
      DO 50 I = 2, N1
	 XI = X(I)/RANGE
	 IF (XX-XI .GT. SMALL) PRINT *,' ANYTHING'
	 SX = SX + XI
	 IF (I .NE. J) SA = SA + SIGN(1, I - J) * A(MIN(I, J))
	 XX = XI
	 J = J - 1
50    CONTINUE
      IFAULT = 0
      IF (N .GT. 5000) IFAULT = 2
C
C        Calculate W statistic as squared correlation
C        between data and coefficients
C
      SA = SA/N1
      SX = SX/N1
      SSA = ZERO
      SSX = ZERO
      SAX = ZERO
      J = N
      DO 60 I = 1, N1
	 IF (I .NE. J) THEN
	    ASA = SIGN(1, I - J) * A(MIN(I, J)) - SA
	 ELSE
	    ASA = -SA
	 END IF
	 XSX = X(I)/RANGE - SX
	 SSA = SSA + ASA * ASA
	 SSX = SSX + XSX * XSX
	 SAX = SAX + ASA * XSX
	 J = J - 1
   60 CONTINUE
C
C        W1 equals (1-W) claculated to avoid excessive rounding error
C        for W very near 1 (a potential problem in very large samples)
C
      SSASSX = SQRT(SSA * SSX)
      W1 = (SSASSX - SAX) * (SSASSX + SAX)/(SSA * SSX)
   70 W = ONE - W1
C
C        Calculate significance level for W (exact for N=3)
C
      IF (N .EQ. 3) THEN
	  PW = PI6 * (ASIN(SQRT(W)) - STQR)
	  RETURN
      END IF
      Y = LOG(W1)
      XX = LOG(AN)
      M = ZERO
      S = ONE
      IF (N .LE. 11) THEN
	 GAMMA = POLY(G, 2, AN)
	 IF (Y .GE. GAMMA) THEN
	    PW = SMALL
	    RETURN
	 END IF
	 Y = -LOG(GAMMA - Y)
	 M = POLY(C3, 4, AN)
	 S = EXP(POLY(C4, 4, AN))
      ELSE
	 M = POLY(C5, 4, XX)
	 S = EXP(POLY(C6, 3, XX))
      END IF
      IF (NCENS .GT. 0) THEN
C
C        Censoring by proportion NCENS/N.  Calculate mean and sd
C        of normal equivalent deviate of W.
C
	 LD = -LOG(DELTA)
	 BF = ONE + XX * BF1
	 Z90F = Z90 + BF * POLY(C7, 2, XX90 ** XX) ** LD
	 Z95F = Z95 + BF * POLY(C8, 2, XX95 ** XX) ** LD
	 Z99F = Z99 + BF * POLY(C9, 2, XX) ** LD
C
C        Regress Z90F,...,Z99F on normal deviates Z90,...,Z99 to get
C        pseudo-mean and pseudo-sd of z as the slope and intercept
C
	 ZFM = (Z90F + Z95F + Z99F)/THREE
	 ZSD = (Z90*(Z90F-ZFM)+Z95*(Z95F-ZFM)+Z99*(Z99F-ZFM))/ZSS
	 ZBAR = ZFM - ZSD * ZM
	 M = M + ZBAR * S
	 S = S * ZSD
      END IF
      PW = real(ALNORM(dble((Y - M)/S), UPPER))
C
      RETURN
      END

C additional routines needed
      function poly(c, nord, x)
c
c
c        Algorithm AS 181.2   Appl. Statist.  (1982) Vol. 31, No. 2
c
c        Calculates the algebraic polynomial of order nored-1 with
c        array of coefficients c.  Zero order coefficient is c(1)
c
      real c(nord)
      poly = c(1)
      if(nord.eq.1) return
      p = x*c(nord)
      if(nord.eq.2) goto 20
      n2 = nord-2
      j = n2+1
      do 10 i = 1,n2
      p = (p+c(j))*x
      j = j-1
   10 continue
   20 poly = poly+p
      return
      end
