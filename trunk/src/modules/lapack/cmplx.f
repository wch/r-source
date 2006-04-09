      SUBROUTINE ZBDSQR( UPLO, N, NCVT, NRU, NCC, D, E, VT, LDVT, U,
     $                   LDU, C, LDC, RWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), RWORK( * )
      COMPLEX*16         C( LDC, * ), U( LDU, * ), VT( LDVT, * )
*     ..
*
*  Purpose
*  =======
*
*  ZBDSQR computes the singular value decomposition (SVD) of a real
*  N-by-N (upper or lower) bidiagonal matrix B:  B = Q * S * P' (P'
*  denotes the transpose of P), where S is a diagonal matrix with
*  non-negative diagonal elements (the singular values of B), and Q
*  and P are orthogonal matrices.
*
*  The routine computes S, and optionally computes U * Q, P' * VT,
*  or Q' * C, for given complex input matrices U, VT, and C.
*
*  See "Computing  Small Singular Values of Bidiagonal Matrices With
*  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
*  LAPACK Working Note #3 (or SIAM J. Sci. Statist. Comput. vol. 11,
*  no. 5, pp. 873-912, Sept 1990) and
*  "Accurate singular values and differential qd algorithms," by
*  B. Parlett and V. Fernando, Technical Report CPAM-554, Mathematics
*  Department, University of California at Berkeley, July 1992
*  for a detailed description of the algorithm.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  B is upper bidiagonal;
*          = 'L':  B is lower bidiagonal.
*
*  N       (input) INTEGER
*          The order of the matrix B.  N >= 0.
*
*  NCVT    (input) INTEGER
*          The number of columns of the matrix VT. NCVT >= 0.
*
*  NRU     (input) INTEGER
*          The number of rows of the matrix U. NRU >= 0.
*
*  NCC     (input) INTEGER
*          The number of columns of the matrix C. NCC >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the bidiagonal matrix B.
*          On exit, if INFO=0, the singular values of B in decreasing
*          order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the elements of E contain the
*          offdiagonal elements of of the bidiagonal matrix whose SVD
*          is desired. On normal exit (INFO = 0), E is destroyed.
*          If the algorithm does not converge (INFO > 0), D and E
*          will contain the diagonal and superdiagonal elements of a
*          bidiagonal matrix orthogonally equivalent to the one given
*          as input. E(N) is used for workspace.
*
*  VT      (input/output) COMPLEX*16 array, dimension (LDVT, NCVT)
*          On entry, an N-by-NCVT matrix VT.
*          On exit, VT is overwritten by P' * VT.
*          VT is not referenced if NCVT = 0.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.
*          LDVT >= max(1,N) if NCVT > 0; LDVT >= 1 if NCVT = 0.
*
*  U       (input/output) COMPLEX*16 array, dimension (LDU, N)
*          On entry, an NRU-by-N matrix U.
*          On exit, U is overwritten by U * Q.
*          U is not referenced if NRU = 0.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= max(1,NRU).
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC, NCC)
*          On entry, an N-by-NCC matrix C.
*          On exit, C is overwritten by Q' * C.
*          C is not referenced if NCC = 0.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C.
*          LDC >= max(1,N) if NCC > 0; LDC >=1 if NCC = 0.
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (4*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  If INFO = -i, the i-th argument had an illegal value
*          > 0:  the algorithm did not converge; D and E contain the
*                elements of a bidiagonal matrix which is orthogonally
*                similar to the input matrix B;  if INFO = i, i
*                elements of E have not converged to zero.
*
*  Internal Parameters
*  ===================
*
*  TOLMUL  DOUBLE PRECISION, default = max(10,min(100,EPS**(-1/8)))
*          TOLMUL controls the convergence criterion of the QR loop.
*          If it is positive, TOLMUL*EPS is the desired relative
*             precision in the computed singular values.
*          If it is negative, abs(TOLMUL*EPS*sigma_max) is the
*             desired absolute accuracy in the computed singular
*             values (corresponds to relative accuracy
*             abs(TOLMUL*EPS) in the largest singular value.
*          abs(TOLMUL) should be between 1 and 1/EPS, and preferably
*             between 10 (for fast convergence) and .1/EPS
*             (for there to be some accuracy in the results).
*          Default is to lose at either one eighth or 2 of the
*             available decimal digits in each computed singular value
*             (whichever is smaller).
*
*  MAXITR  INTEGER, default = 6
*          MAXITR controls the maximum number of passes of the
*          algorithm through its inner loop. The algorithms stops
*          (and so fails to converge) if the number of passes
*          through the inner loop exceeds MAXITR*N**2.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   NEGONE
      PARAMETER          ( NEGONE = -1.0D0 )
      DOUBLE PRECISION   HNDRTH
      PARAMETER          ( HNDRTH = 0.01D0 )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 10.0D0 )
      DOUBLE PRECISION   HNDRD
      PARAMETER          ( HNDRD = 100.0D0 )
      DOUBLE PRECISION   MEIGTH
      PARAMETER          ( MEIGTH = -0.125D0 )
      INTEGER            MAXITR
      PARAMETER          ( MAXITR = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LOWER, ROTATE
      INTEGER            I, IDIR, ISUB, ITER, J, LL, LLL, M, MAXIT, NM1,
     $                   NM12, NM13, OLDLL, OLDM
      DOUBLE PRECISION   ABSE, ABSS, COSL, COSR, CS, EPS, F, G, H, MU,
     $                   OLDCS, OLDSN, R, SHIFT, SIGMN, SIGMX, SINL,
     $                   SINR, SLL, SMAX, SMIN, SMINL, SMINLO, SMINOA,
     $                   SN, THRESH, TOL, TOLMUL, UNFL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARTG, DLAS2, DLASQ1, DLASV2, XERBLA, ZDROT,
     $                   ZDSCAL, ZLASR, ZSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      LOWER = LSAME( UPLO, 'L' )
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LOWER ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NCVT.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRU.LT.0 ) THEN
         INFO = -4
      ELSE IF( NCC.LT.0 ) THEN
         INFO = -5
      ELSE IF( ( NCVT.EQ.0 .AND. LDVT.LT.1 ) .OR.
     $         ( NCVT.GT.0 .AND. LDVT.LT.MAX( 1, N ) ) ) THEN
         INFO = -9
      ELSE IF( LDU.LT.MAX( 1, NRU ) ) THEN
         INFO = -11
      ELSE IF( ( NCC.EQ.0 .AND. LDC.LT.1 ) .OR.
     $         ( NCC.GT.0 .AND. LDC.LT.MAX( 1, N ) ) ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZBDSQR', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 )
     $   GO TO 160
*
*     ROTATE is true if any singular vectors desired, false otherwise
*
      ROTATE = ( NCVT.GT.0 ) .OR. ( NRU.GT.0 ) .OR. ( NCC.GT.0 )
*
*     If no singular vectors desired, use qd algorithm
*
      IF( .NOT.ROTATE ) THEN
         CALL DLASQ1( N, D, E, RWORK, INFO )
         RETURN
      END IF
*
      NM1 = N - 1
      NM12 = NM1 + NM1
      NM13 = NM12 + NM1
      IDIR = 0
*
*     Get machine constants
*
      EPS = DLAMCH( 'Epsilon' )
      UNFL = DLAMCH( 'Safe minimum' )
*
*     If matrix lower bidiagonal, rotate to be upper bidiagonal
*     by applying Givens rotations on the left
*
      IF( LOWER ) THEN
         DO 10 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            RWORK( I ) = CS
            RWORK( NM1+I ) = SN
   10    CONTINUE
*
*        Update singular vectors if desired
*
         IF( NRU.GT.0 )
     $      CALL ZLASR( 'R', 'V', 'F', NRU, N, RWORK( 1 ), RWORK( N ),
     $                  U, LDU )
         IF( NCC.GT.0 )
     $      CALL ZLASR( 'L', 'V', 'F', N, NCC, RWORK( 1 ), RWORK( N ),
     $                  C, LDC )
      END IF
*
*     Compute singular values to relative accuracy TOL
*     (By setting TOL to be negative, algorithm will compute
*     singular values to absolute accuracy ABS(TOL)*norm(input matrix))
*
      TOLMUL = MAX( TEN, MIN( HNDRD, EPS**MEIGTH ) )
      TOL = TOLMUL*EPS
*
*     Compute approximate maximum, minimum singular values
*
      SMAX = ZERO
      DO 20 I = 1, N
         SMAX = MAX( SMAX, ABS( D( I ) ) )
   20 CONTINUE
      DO 30 I = 1, N - 1
         SMAX = MAX( SMAX, ABS( E( I ) ) )
   30 CONTINUE
      SMINL = ZERO
      IF( TOL.GE.ZERO ) THEN
*
*        Relative accuracy desired
*
         SMINOA = ABS( D( 1 ) )
         IF( SMINOA.EQ.ZERO )
     $      GO TO 50
         MU = SMINOA
         DO 40 I = 2, N
            MU = ABS( D( I ) )*( MU / ( MU+ABS( E( I-1 ) ) ) )
            SMINOA = MIN( SMINOA, MU )
            IF( SMINOA.EQ.ZERO )
     $         GO TO 50
   40    CONTINUE
   50    CONTINUE
         SMINOA = SMINOA / SQRT( DBLE( N ) )
         THRESH = MAX( TOL*SMINOA, MAXITR*N*N*UNFL )
      ELSE
*
*        Absolute accuracy desired
*
         THRESH = MAX( ABS( TOL )*SMAX, MAXITR*N*N*UNFL )
      END IF
*
*     Prepare for main iteration loop for the singular values
*     (MAXIT is the maximum number of passes through the inner
*     loop permitted before nonconvergence signalled.)
*
      MAXIT = MAXITR*N*N
      ITER = 0
      OLDLL = -1
      OLDM = -1
*
*     M points to last element of unconverged part of matrix
*
      M = N
*
*     Begin main iteration loop
*
   60 CONTINUE
*
*     Check for convergence or exceeding iteration count
*
      IF( M.LE.1 )
     $   GO TO 160
      IF( ITER.GT.MAXIT )
     $   GO TO 200
*
*     Find diagonal block of matrix to work on
*
      IF( TOL.LT.ZERO .AND. ABS( D( M ) ).LE.THRESH )
     $   D( M ) = ZERO
      SMAX = ABS( D( M ) )
      SMIN = SMAX
      DO 70 LLL = 1, M - 1
         LL = M - LLL
         ABSS = ABS( D( LL ) )
         ABSE = ABS( E( LL ) )
         IF( TOL.LT.ZERO .AND. ABSS.LE.THRESH )
     $      D( LL ) = ZERO
         IF( ABSE.LE.THRESH )
     $      GO TO 80
         SMIN = MIN( SMIN, ABSS )
         SMAX = MAX( SMAX, ABSS, ABSE )
   70 CONTINUE
      LL = 0
      GO TO 90
   80 CONTINUE
      E( LL ) = ZERO
*
*     Matrix splits since E(LL) = 0
*
      IF( LL.EQ.M-1 ) THEN
*
*        Convergence of bottom singular value, return to top of loop
*
         M = M - 1
         GO TO 60
      END IF
   90 CONTINUE
      LL = LL + 1
*
*     E(LL) through E(M-1) are nonzero, E(LL-1) is zero
*
      IF( LL.EQ.M-1 ) THEN
*
*        2 by 2 block, handle separately
*
         CALL DLASV2( D( M-1 ), E( M-1 ), D( M ), SIGMN, SIGMX, SINR,
     $                COSR, SINL, COSL )
         D( M-1 ) = SIGMX
         E( M-1 ) = ZERO
         D( M ) = SIGMN
*
*        Compute singular vectors, if desired
*
         IF( NCVT.GT.0 )
     $      CALL ZDROT( NCVT, VT( M-1, 1 ), LDVT, VT( M, 1 ), LDVT,
     $                  COSR, SINR )
         IF( NRU.GT.0 )
     $      CALL ZDROT( NRU, U( 1, M-1 ), 1, U( 1, M ), 1, COSL, SINL )
         IF( NCC.GT.0 )
     $      CALL ZDROT( NCC, C( M-1, 1 ), LDC, C( M, 1 ), LDC, COSL,
     $                  SINL )
         M = M - 2
         GO TO 60
      END IF
*
*     If working on new submatrix, choose shift direction
*     (from larger end diagonal element towards smaller)
*
      IF( LL.GT.OLDM .OR. M.LT.OLDLL ) THEN
         IF( ABS( D( LL ) ).GE.ABS( D( M ) ) ) THEN
*
*           Chase bulge from top (big end) to bottom (small end)
*
            IDIR = 1
         ELSE
*
*           Chase bulge from bottom (big end) to top (small end)
*
            IDIR = 2
         END IF
      END IF
*
*     Apply convergence tests
*
      IF( IDIR.EQ.1 ) THEN
*
*        Run convergence test in forward direction
*        First apply standard test to bottom of matrix
*
         IF( ABS( E( M-1 ) ).LE.ABS( TOL )*ABS( D( M ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( M-1 ) ).LE.THRESH ) ) THEN
            E( M-1 ) = ZERO
            GO TO 60
         END IF
*
         IF( TOL.GE.ZERO ) THEN
*
*           If relative accuracy desired,
*           apply convergence criterion forward
*
            MU = ABS( D( LL ) )
            SMINL = MU
            DO 100 LLL = LL, M - 1
               IF( ABS( E( LLL ) ).LE.TOL*MU ) THEN
                  E( LLL ) = ZERO
                  GO TO 60
               END IF
               SMINLO = SMINL
               MU = ABS( D( LLL+1 ) )*( MU / ( MU+ABS( E( LLL ) ) ) )
               SMINL = MIN( SMINL, MU )
  100       CONTINUE
         END IF
*
      ELSE
*
*        Run convergence test in backward direction
*        First apply standard test to top of matrix
*
         IF( ABS( E( LL ) ).LE.ABS( TOL )*ABS( D( LL ) ) .OR.
     $       ( TOL.LT.ZERO .AND. ABS( E( LL ) ).LE.THRESH ) ) THEN
            E( LL ) = ZERO
            GO TO 60
         END IF
*
         IF( TOL.GE.ZERO ) THEN
*
*           If relative accuracy desired,
*           apply convergence criterion backward
*
            MU = ABS( D( M ) )
            SMINL = MU
            DO 110 LLL = M - 1, LL, -1
               IF( ABS( E( LLL ) ).LE.TOL*MU ) THEN
                  E( LLL ) = ZERO
                  GO TO 60
               END IF
               SMINLO = SMINL
               MU = ABS( D( LLL ) )*( MU / ( MU+ABS( E( LLL ) ) ) )
               SMINL = MIN( SMINL, MU )
  110       CONTINUE
         END IF
      END IF
      OLDLL = LL
      OLDM = M
*
*     Compute shift.  First, test if shifting would ruin relative
*     accuracy, and if so set the shift to zero.
*
      IF( TOL.GE.ZERO .AND. N*TOL*( SMINL / SMAX ).LE.
     $    MAX( EPS, HNDRTH*TOL ) ) THEN
*
*        Use a zero shift to avoid loss of relative accuracy
*
         SHIFT = ZERO
      ELSE
*
*        Compute the shift from 2-by-2 block at end of matrix
*
         IF( IDIR.EQ.1 ) THEN
            SLL = ABS( D( LL ) )
            CALL DLAS2( D( M-1 ), E( M-1 ), D( M ), SHIFT, R )
         ELSE
            SLL = ABS( D( M ) )
            CALL DLAS2( D( LL ), E( LL ), D( LL+1 ), SHIFT, R )
         END IF
*
*        Test if shift negligible, and if so set to zero
*
         IF( SLL.GT.ZERO ) THEN
            IF( ( SHIFT / SLL )**2.LT.EPS )
     $         SHIFT = ZERO
         END IF
      END IF
*
*     Increment iteration count
*
      ITER = ITER + M - LL
*
*     If SHIFT = 0, do simplified QR iteration
*
      IF( SHIFT.EQ.ZERO ) THEN
         IF( IDIR.EQ.1 ) THEN
*
*           Chase bulge from top to bottom
*           Save cosines and sines for later singular vector updates
*
            CS = ONE
            OLDCS = ONE
            DO 120 I = LL, M - 1
               CALL DLARTG( D( I )*CS, E( I ), CS, SN, R )
               IF( I.GT.LL )
     $            E( I-1 ) = OLDSN*R
               CALL DLARTG( OLDCS*R, D( I+1 )*SN, OLDCS, OLDSN, D( I ) )
               RWORK( I-LL+1 ) = CS
               RWORK( I-LL+1+NM1 ) = SN
               RWORK( I-LL+1+NM12 ) = OLDCS
               RWORK( I-LL+1+NM13 ) = OLDSN
  120       CONTINUE
            H = D( M )*CS
            D( M ) = H*OLDCS
            E( M-1 ) = H*OLDSN
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL ZLASR( 'L', 'V', 'F', M-LL+1, NCVT, RWORK( 1 ),
     $                     RWORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL ZLASR( 'R', 'V', 'F', NRU, M-LL+1, RWORK( NM12+1 ),
     $                     RWORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL ZLASR( 'L', 'V', 'F', M-LL+1, NCC, RWORK( NM12+1 ),
     $                     RWORK( NM13+1 ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
*
         ELSE
*
*           Chase bulge from bottom to top
*           Save cosines and sines for later singular vector updates
*
            CS = ONE
            OLDCS = ONE
            DO 130 I = M, LL + 1, -1
               CALL DLARTG( D( I )*CS, E( I-1 ), CS, SN, R )
               IF( I.LT.M )
     $            E( I ) = OLDSN*R
               CALL DLARTG( OLDCS*R, D( I-1 )*SN, OLDCS, OLDSN, D( I ) )
               RWORK( I-LL ) = CS
               RWORK( I-LL+NM1 ) = -SN
               RWORK( I-LL+NM12 ) = OLDCS
               RWORK( I-LL+NM13 ) = -OLDSN
  130       CONTINUE
            H = D( LL )*CS
            D( LL ) = H*OLDCS
            E( LL ) = H*OLDSN
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL ZLASR( 'L', 'V', 'B', M-LL+1, NCVT, RWORK( NM12+1 ),
     $                     RWORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL ZLASR( 'R', 'V', 'B', NRU, M-LL+1, RWORK( 1 ),
     $                     RWORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL ZLASR( 'L', 'V', 'B', M-LL+1, NCC, RWORK( 1 ),
     $                     RWORK( N ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
         END IF
      ELSE
*
*        Use nonzero shift
*
         IF( IDIR.EQ.1 ) THEN
*
*           Chase bulge from top to bottom
*           Save cosines and sines for later singular vector updates
*
            F = ( ABS( D( LL ) )-SHIFT )*
     $          ( SIGN( ONE, D( LL ) )+SHIFT / D( LL ) )
            G = E( LL )
            DO 140 I = LL, M - 1
               CALL DLARTG( F, G, COSR, SINR, R )
               IF( I.GT.LL )
     $            E( I-1 ) = R
               F = COSR*D( I ) + SINR*E( I )
               E( I ) = COSR*E( I ) - SINR*D( I )
               G = SINR*D( I+1 )
               D( I+1 ) = COSR*D( I+1 )
               CALL DLARTG( F, G, COSL, SINL, R )
               D( I ) = R
               F = COSL*E( I ) + SINL*D( I+1 )
               D( I+1 ) = COSL*D( I+1 ) - SINL*E( I )
               IF( I.LT.M-1 ) THEN
                  G = SINL*E( I+1 )
                  E( I+1 ) = COSL*E( I+1 )
               END IF
               RWORK( I-LL+1 ) = COSR
               RWORK( I-LL+1+NM1 ) = SINR
               RWORK( I-LL+1+NM12 ) = COSL
               RWORK( I-LL+1+NM13 ) = SINL
  140       CONTINUE
            E( M-1 ) = F
*
*           Update singular vectors
*
            IF( NCVT.GT.0 )
     $         CALL ZLASR( 'L', 'V', 'F', M-LL+1, NCVT, RWORK( 1 ),
     $                     RWORK( N ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL ZLASR( 'R', 'V', 'F', NRU, M-LL+1, RWORK( NM12+1 ),
     $                     RWORK( NM13+1 ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL ZLASR( 'L', 'V', 'F', M-LL+1, NCC, RWORK( NM12+1 ),
     $                     RWORK( NM13+1 ), C( LL, 1 ), LDC )
*
*           Test convergence
*
            IF( ABS( E( M-1 ) ).LE.THRESH )
     $         E( M-1 ) = ZERO
*
         ELSE
*
*           Chase bulge from bottom to top
*           Save cosines and sines for later singular vector updates
*
            F = ( ABS( D( M ) )-SHIFT )*( SIGN( ONE, D( M ) )+SHIFT /
     $          D( M ) )
            G = E( M-1 )
            DO 150 I = M, LL + 1, -1
               CALL DLARTG( F, G, COSR, SINR, R )
               IF( I.LT.M )
     $            E( I ) = R
               F = COSR*D( I ) + SINR*E( I-1 )
               E( I-1 ) = COSR*E( I-1 ) - SINR*D( I )
               G = SINR*D( I-1 )
               D( I-1 ) = COSR*D( I-1 )
               CALL DLARTG( F, G, COSL, SINL, R )
               D( I ) = R
               F = COSL*E( I-1 ) + SINL*D( I-1 )
               D( I-1 ) = COSL*D( I-1 ) - SINL*E( I-1 )
               IF( I.GT.LL+1 ) THEN
                  G = SINL*E( I-2 )
                  E( I-2 ) = COSL*E( I-2 )
               END IF
               RWORK( I-LL ) = COSR
               RWORK( I-LL+NM1 ) = -SINR
               RWORK( I-LL+NM12 ) = COSL
               RWORK( I-LL+NM13 ) = -SINL
  150       CONTINUE
            E( LL ) = F
*
*           Test convergence
*
            IF( ABS( E( LL ) ).LE.THRESH )
     $         E( LL ) = ZERO
*
*           Update singular vectors if desired
*
            IF( NCVT.GT.0 )
     $         CALL ZLASR( 'L', 'V', 'B', M-LL+1, NCVT, RWORK( NM12+1 ),
     $                     RWORK( NM13+1 ), VT( LL, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL ZLASR( 'R', 'V', 'B', NRU, M-LL+1, RWORK( 1 ),
     $                     RWORK( N ), U( 1, LL ), LDU )
            IF( NCC.GT.0 )
     $         CALL ZLASR( 'L', 'V', 'B', M-LL+1, NCC, RWORK( 1 ),
     $                     RWORK( N ), C( LL, 1 ), LDC )
         END IF
      END IF
*
*     QR iteration finished, go back and check convergence
*
      GO TO 60
*
*     All singular values converged, so make them positive
*
  160 CONTINUE
      DO 170 I = 1, N
         IF( D( I ).LT.ZERO ) THEN
            D( I ) = -D( I )
*
*           Change sign of singular vectors, if desired
*
            IF( NCVT.GT.0 )
     $         CALL ZDSCAL( NCVT, NEGONE, VT( I, 1 ), LDVT )
         END IF
  170 CONTINUE
*
*     Sort the singular values into decreasing order (insertion sort on
*     singular values, but only one transposition per singular vector)
*
      DO 190 I = 1, N - 1
*
*        Scan for smallest D(I)
*
         ISUB = 1
         SMIN = D( 1 )
         DO 180 J = 2, N + 1 - I
            IF( D( J ).LE.SMIN ) THEN
               ISUB = J
               SMIN = D( J )
            END IF
  180    CONTINUE
         IF( ISUB.NE.N+1-I ) THEN
*
*           Swap singular values and vectors
*
            D( ISUB ) = D( N+1-I )
            D( N+1-I ) = SMIN
            IF( NCVT.GT.0 )
     $         CALL ZSWAP( NCVT, VT( ISUB, 1 ), LDVT, VT( N+1-I, 1 ),
     $                     LDVT )
            IF( NRU.GT.0 )
     $         CALL ZSWAP( NRU, U( 1, ISUB ), 1, U( 1, N+1-I ), 1 )
            IF( NCC.GT.0 )
     $         CALL ZSWAP( NCC, C( ISUB, 1 ), LDC, C( N+1-I, 1 ), LDC )
         END IF
  190 CONTINUE
      GO TO 220
*
*     Maximum number of iterations exceeded, failure to converge
*
  200 CONTINUE
      INFO = 0
      DO 210 I = 1, N - 1
         IF( E( I ).NE.ZERO )
     $      INFO = INFO + 1
  210 CONTINUE
  220 CONTINUE
      RETURN
*
*     End of ZBDSQR
*
      END
      SUBROUTINE ZGEBAK( JOB, SIDE, N, ILO, IHI, SCALE, M, V, LDV,
     $                   INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          JOB, SIDE
      INTEGER            IHI, ILO, INFO, LDV, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   SCALE( * )
      COMPLEX*16         V( LDV, * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEBAK forms the right or left eigenvectors of a complex general
*  matrix by backward transformation on the computed eigenvectors of the
*  balanced matrix output by ZGEBAL.
*
*  Arguments
*  =========
*
*  JOB     (input) CHARACTER*1
*          Specifies the type of backward transformation required:
*          = 'N', do nothing, return immediately;
*          = 'P', do backward transformation for permutation only;
*          = 'S', do backward transformation for scaling only;
*          = 'B', do backward transformations for both permutation and
*                 scaling.
*          JOB must be the same as the argument JOB supplied to ZGEBAL.
*
*  SIDE    (input) CHARACTER*1
*          = 'R':  V contains right eigenvectors;
*          = 'L':  V contains left eigenvectors.
*
*  N       (input) INTEGER
*          The number of rows of the matrix V.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          The integers ILO and IHI determined by ZGEBAL.
*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
*
*  SCALE   (input) DOUBLE PRECISION array, dimension (N)
*          Details of the permutation and scaling factors, as returned
*          by ZGEBAL.
*
*  M       (input) INTEGER
*          The number of columns of the matrix V.  M >= 0.
*
*  V       (input/output) COMPLEX*16 array, dimension (LDV,M)
*          On entry, the matrix of right or left eigenvectors to be
*          transformed, as returned by ZHSEIN or ZTREVC.
*          On exit, V is overwritten by the transformed eigenvectors.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V. LDV >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFTV, RIGHTV
      INTEGER            I, II, K
      DOUBLE PRECISION   S
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZDSCAL, ZSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Decode and Test the input parameters
*
      RIGHTV = LSAME( SIDE, 'R' )
      LEFTV = LSAME( SIDE, 'L' )
*
      INFO = 0
      IF( .NOT.LSAME( JOB, 'N' ) .AND. .NOT.LSAME( JOB, 'P' ) .AND.
     $    .NOT.LSAME( JOB, 'S' ) .AND. .NOT.LSAME( JOB, 'B' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.RIGHTV .AND. .NOT.LEFTV ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN
         INFO = -5
      ELSE IF( M.LT.0 ) THEN
         INFO = -7
      ELSE IF( LDV.LT.MAX( 1, N ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEBAK', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
      IF( M.EQ.0 )
     $   RETURN
      IF( LSAME( JOB, 'N' ) )
     $   RETURN
*
      IF( ILO.EQ.IHI )
     $   GO TO 30
*
*     Backward balance
*
      IF( LSAME( JOB, 'S' ) .OR. LSAME( JOB, 'B' ) ) THEN
*
         IF( RIGHTV ) THEN
            DO 10 I = ILO, IHI
               S = SCALE( I )
               CALL ZDSCAL( M, S, V( I, 1 ), LDV )
   10       CONTINUE
         END IF
*
         IF( LEFTV ) THEN
            DO 20 I = ILO, IHI
               S = ONE / SCALE( I )
               CALL ZDSCAL( M, S, V( I, 1 ), LDV )
   20       CONTINUE
         END IF
*
      END IF
*
*     Backward permutation
*
*     For  I = ILO-1 step -1 until 1,
*              IHI+1 step 1 until N do --
*
   30 CONTINUE
      IF( LSAME( JOB, 'P' ) .OR. LSAME( JOB, 'B' ) ) THEN
         IF( RIGHTV ) THEN
            DO 40 II = 1, N
               I = II
               IF( I.GE.ILO .AND. I.LE.IHI )
     $            GO TO 40
               IF( I.LT.ILO )
     $            I = ILO - II
               K = SCALE( I )
               IF( K.EQ.I )
     $            GO TO 40
               CALL ZSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV )
   40       CONTINUE
         END IF
*
         IF( LEFTV ) THEN
            DO 50 II = 1, N
               I = II
               IF( I.GE.ILO .AND. I.LE.IHI )
     $            GO TO 50
               IF( I.LT.ILO )
     $            I = ILO - II
               K = SCALE( I )
               IF( K.EQ.I )
     $            GO TO 50
               CALL ZSWAP( M, V( I, 1 ), LDV, V( K, 1 ), LDV )
   50       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of ZGEBAK
*
      END
      SUBROUTINE ZGEBAL( JOB, N, A, LDA, ILO, IHI, SCALE, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          JOB
      INTEGER            IHI, ILO, INFO, LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   SCALE( * )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEBAL balances a general complex matrix A.  This involves, first,
*  permuting A by a similarity transformation to isolate eigenvalues
*  in the first 1 to ILO-1 and last IHI+1 to N elements on the
*  diagonal; and second, applying a diagonal similarity transformation
*  to rows and columns ILO to IHI to make the rows and columns as
*  close in norm as possible.  Both steps are optional.
*
*  Balancing may reduce the 1-norm of the matrix, and improve the
*  accuracy of the computed eigenvalues and/or eigenvectors.
*
*  Arguments
*  =========
*
*  JOB     (input) CHARACTER*1
*          Specifies the operations to be performed on A:
*          = 'N':  none:  simply set ILO = 1, IHI = N, SCALE(I) = 1.0
*                  for i = 1,...,N;
*          = 'P':  permute only;
*          = 'S':  scale only;
*          = 'B':  both permute and scale.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the input matrix A.
*          On exit,  A is overwritten by the balanced matrix.
*          If JOB = 'N', A is not referenced.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  ILO     (output) INTEGER
*  IHI     (output) INTEGER
*          ILO and IHI are set to integers such that on exit
*          A(i,j) = 0 if i > j and j = 1,...,ILO-1 or I = IHI+1,...,N.
*          If JOB = 'N' or 'S', ILO = 1 and IHI = N.
*
*  SCALE   (output) DOUBLE PRECISION array, dimension (N)
*          Details of the permutations and scaling factors applied to
*          A.  If P(j) is the index of the row and column interchanged
*          with row and column j and D(j) is the scaling factor
*          applied to row and column j, then
*          SCALE(j) = P(j)    for j = 1,...,ILO-1
*                   = D(j)    for j = ILO,...,IHI
*                   = P(j)    for j = IHI+1,...,N.
*          The order in which the interchanges are made is N to IHI+1,
*          then 1 to ILO-1.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The permutations consist of row and column interchanges which put
*  the matrix in the form
*
*             ( T1   X   Y  )
*     P A P = (  0   B   Z  )
*             (  0   0   T2 )
*
*  where T1 and T2 are upper triangular matrices whose eigenvalues lie
*  along the diagonal.  The column indices ILO and IHI mark the starting
*  and ending columns of the submatrix B. Balancing consists of applying
*  a diagonal similarity transformation inv(D) * B * D to make the
*  1-norms of each row of B and its corresponding column nearly equal.
*  The output matrix is
*
*     ( T1     X*D          Y    )
*     (  0  inv(D)*B*D  inv(D)*Z ).
*     (  0      0           T2   )
*
*  Information about the permutations P and the diagonal matrix D is
*  returned in the vector SCALE.
*
*  This subroutine is based on the EISPACK routine CBAL.
*
*  Modified by Tzu-Yi Chen, Computer Science Division, University of
*    California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   SCLFAC
      PARAMETER          ( SCLFAC = 0.8D+1 )
      DOUBLE PRECISION   FACTOR
      PARAMETER          ( FACTOR = 0.95D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOCONV
      INTEGER            I, ICA, IEXC, IRA, J, K, L, M
      DOUBLE PRECISION   C, CA, F, G, R, RA, S, SFMAX1, SFMAX2, SFMIN1,
     $                   SFMIN2
      COMPLEX*16         CDUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IZAMAX
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, IZAMAX, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZDSCAL, ZSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG, MAX, MIN
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( .NOT.LSAME( JOB, 'N' ) .AND. .NOT.LSAME( JOB, 'P' ) .AND.
     $    .NOT.LSAME( JOB, 'S' ) .AND. .NOT.LSAME( JOB, 'B' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEBAL', -INFO )
         RETURN
      END IF
*
      K = 1
      L = N
*
      IF( N.EQ.0 )
     $   GO TO 210
*
      IF( LSAME( JOB, 'N' ) ) THEN
         DO 10 I = 1, N
            SCALE( I ) = ONE
   10    CONTINUE
         GO TO 210
      END IF
*
      IF( LSAME( JOB, 'S' ) )
     $   GO TO 120
*
*     Permutation to isolate eigenvalues if possible
*
      GO TO 50
*
*     Row and column exchange.
*
   20 CONTINUE
      SCALE( M ) = J
      IF( J.EQ.M )
     $   GO TO 30
*
      CALL ZSWAP( L, A( 1, J ), 1, A( 1, M ), 1 )
      CALL ZSWAP( N-K+1, A( J, K ), LDA, A( M, K ), LDA )
*
   30 CONTINUE
      GO TO ( 40, 80 )IEXC
*
*     Search for rows isolating an eigenvalue and push them down.
*
   40 CONTINUE
      IF( L.EQ.1 )
     $   GO TO 210
      L = L - 1
*
   50 CONTINUE
      DO 70 J = L, 1, -1
*
         DO 60 I = 1, L
            IF( I.EQ.J )
     $         GO TO 60
            IF( DBLE( A( J, I ) ).NE.ZERO .OR. DIMAG( A( J, I ) ).NE.
     $          ZERO )GO TO 70
   60    CONTINUE
*
         M = L
         IEXC = 1
         GO TO 20
   70 CONTINUE
*
      GO TO 90
*
*     Search for columns isolating an eigenvalue and push them left.
*
   80 CONTINUE
      K = K + 1
*
   90 CONTINUE
      DO 110 J = K, L
*
         DO 100 I = K, L
            IF( I.EQ.J )
     $         GO TO 100
            IF( DBLE( A( I, J ) ).NE.ZERO .OR. DIMAG( A( I, J ) ).NE.
     $          ZERO )GO TO 110
  100    CONTINUE
*
         M = K
         IEXC = 2
         GO TO 20
  110 CONTINUE
*
  120 CONTINUE
      DO 130 I = K, L
         SCALE( I ) = ONE
  130 CONTINUE
*
      IF( LSAME( JOB, 'P' ) )
     $   GO TO 210
*
*     Balance the submatrix in rows K to L.
*
*     Iterative loop for norm reduction
*
      SFMIN1 = DLAMCH( 'S' ) / DLAMCH( 'P' )
      SFMAX1 = ONE / SFMIN1
      SFMIN2 = SFMIN1*SCLFAC
      SFMAX2 = ONE / SFMIN2
  140 CONTINUE
      NOCONV = .FALSE.
*
      DO 200 I = K, L
         C = ZERO
         R = ZERO
*
         DO 150 J = K, L
            IF( J.EQ.I )
     $         GO TO 150
            C = C + CABS1( A( J, I ) )
            R = R + CABS1( A( I, J ) )
  150    CONTINUE
         ICA = IZAMAX( L, A( 1, I ), 1 )
         CA = ABS( A( ICA, I ) )
         IRA = IZAMAX( N-K+1, A( I, K ), LDA )
         RA = ABS( A( I, IRA+K-1 ) )
*
*        Guard against zero C or R due to underflow.
*
         IF( C.EQ.ZERO .OR. R.EQ.ZERO )
     $      GO TO 200
         G = R / SCLFAC
         F = ONE
         S = C + R
  160    CONTINUE
         IF( C.GE.G .OR. MAX( F, C, CA ).GE.SFMAX2 .OR.
     $       MIN( R, G, RA ).LE.SFMIN2 )GO TO 170
         F = F*SCLFAC
         C = C*SCLFAC
         CA = CA*SCLFAC
         R = R / SCLFAC
         G = G / SCLFAC
         RA = RA / SCLFAC
         GO TO 160
*
  170    CONTINUE
         G = C / SCLFAC
  180    CONTINUE
         IF( G.LT.R .OR. MAX( R, RA ).GE.SFMAX2 .OR.
     $       MIN( F, C, G, CA ).LE.SFMIN2 )GO TO 190
         F = F / SCLFAC
         C = C / SCLFAC
         G = G / SCLFAC
         CA = CA / SCLFAC
         R = R*SCLFAC
         RA = RA*SCLFAC
         GO TO 180
*
*        Now balance.
*
  190    CONTINUE
         IF( ( C+R ).GE.FACTOR*S )
     $      GO TO 200
         IF( F.LT.ONE .AND. SCALE( I ).LT.ONE ) THEN
            IF( F*SCALE( I ).LE.SFMIN1 )
     $         GO TO 200
         END IF
         IF( F.GT.ONE .AND. SCALE( I ).GT.ONE ) THEN
            IF( SCALE( I ).GE.SFMAX1 / F )
     $         GO TO 200
         END IF
         G = ONE / F
         SCALE( I ) = SCALE( I )*F
         NOCONV = .TRUE.
*
         CALL ZDSCAL( N-K+1, G, A( I, K ), LDA )
         CALL ZDSCAL( L, F, A( 1, I ), 1 )
*
  200 CONTINUE
*
      IF( NOCONV )
     $   GO TO 140
*
  210 CONTINUE
      ILO = K
      IHI = L
*
      RETURN
*
*     End of ZGEBAL
*
      END
      SUBROUTINE ZGEBD2( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
      COMPLEX*16         A( LDA, * ), TAUP( * ), TAUQ( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEBD2 reduces a complex general m by n matrix A to upper or lower
*  real bidiagonal form B by a unitary transformation: Q' * A * P = B.
*
*  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit,
*          if m >= n, the diagonal and the first superdiagonal are
*            overwritten with the upper bidiagonal matrix B; the
*            elements below the diagonal, with the array TAUQ, represent
*            the unitary matrix Q as a product of elementary
*            reflectors, and the elements above the first superdiagonal,
*            with the array TAUP, represent the unitary matrix P as
*            a product of elementary reflectors;
*          if m < n, the diagonal and the first subdiagonal are
*            overwritten with the lower bidiagonal matrix B; the
*            elements below the first subdiagonal, with the array TAUQ,
*            represent the unitary matrix Q as a product of
*            elementary reflectors, and the elements above the diagonal,
*            with the array TAUP, represent the unitary matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the bidiagonal matrix B:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
*          The off-diagonal elements of the bidiagonal matrix B:
*          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
*          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
*
*  TAUQ    (output) COMPLEX*16 array dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the unitary matrix Q. See Further Details.
*
*  TAUP    (output) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the unitary matrix P. See Further Details.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (max(M,N))
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*  If m >= n,
*
*     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are complex scalars, and v and u are complex
*  vectors; v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in
*  A(i+1:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in
*  A(i,i+2:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n,
*
*     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are complex scalars, v and u are complex vectors;
*  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
*  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
*  tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The contents of A on exit are illustrated by the following examples:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
*    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
*    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
*    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
*    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
*    (  v1  v2  v3  v4  v5 )
*
*  where d and e denote diagonal and off-diagonal elements of B, vi
*  denotes an element of the vector defining H(i), and ui an element of
*  the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      COMPLEX*16         ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLACGV, ZLARF, ZLARFG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'ZGEBD2', -INFO )
         RETURN
      END IF
*
      IF( M.GE.N ) THEN
*
*        Reduce to upper bidiagonal form
*
         DO 10 I = 1, N
*
*           Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
            ALPHA = A( I, I )
            CALL ZLARFG( M-I+1, ALPHA, A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = ALPHA
            A( I, I ) = ONE
*
*           Apply H(i)' to A(i:m,i+1:n) from the left
*
            CALL ZLARF( 'Left', M-I+1, N-I, A( I, I ), 1,
     $                  DCONJG( TAUQ( I ) ), A( I, I+1 ), LDA, WORK )
            A( I, I ) = D( I )
*
            IF( I.LT.N ) THEN
*
*              Generate elementary reflector G(i) to annihilate
*              A(i,i+2:n)
*
               CALL ZLACGV( N-I, A( I, I+1 ), LDA )
               ALPHA = A( I, I+1 )
               CALL ZLARFG( N-I, ALPHA, A( I, MIN( I+2, N ) ), LDA,
     $                      TAUP( I ) )
               E( I ) = ALPHA
               A( I, I+1 ) = ONE
*
*              Apply G(i) to A(i+1:m,i+1:n) from the right
*
               CALL ZLARF( 'Right', M-I, N-I, A( I, I+1 ), LDA,
     $                     TAUP( I ), A( I+1, I+1 ), LDA, WORK )
               CALL ZLACGV( N-I, A( I, I+1 ), LDA )
               A( I, I+1 ) = E( I )
            ELSE
               TAUP( I ) = ZERO
            END IF
   10    CONTINUE
      ELSE
*
*        Reduce to lower bidiagonal form
*
         DO 20 I = 1, M
*
*           Generate elementary reflector G(i) to annihilate A(i,i+1:n)
*
            CALL ZLACGV( N-I+1, A( I, I ), LDA )
            ALPHA = A( I, I )
            CALL ZLARFG( N-I+1, ALPHA, A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = ALPHA
            A( I, I ) = ONE
*
*           Apply G(i) to A(i+1:m,i:n) from the right
*
            CALL ZLARF( 'Right', M-I, N-I+1, A( I, I ), LDA, TAUP( I ),
     $                  A( MIN( I+1, M ), I ), LDA, WORK )
            CALL ZLACGV( N-I+1, A( I, I ), LDA )
            A( I, I ) = D( I )
*
            IF( I.LT.M ) THEN
*
*              Generate elementary reflector H(i) to annihilate
*              A(i+2:m,i)
*
               ALPHA = A( I+1, I )
               CALL ZLARFG( M-I, ALPHA, A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = ALPHA
               A( I+1, I ) = ONE
*
*              Apply H(i)' to A(i+1:m,i+1:n) from the left
*
               CALL ZLARF( 'Left', M-I, N-I, A( I+1, I ), 1,
     $                     DCONJG( TAUQ( I ) ), A( I+1, I+1 ), LDA,
     $                     WORK )
               A( I+1, I ) = E( I )
            ELSE
               TAUQ( I ) = ZERO
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     End of ZGEBD2
*
      END
      SUBROUTINE ZGEBRD( M, N, A, LDA, D, E, TAUQ, TAUP, WORK, LWORK,
     $                   INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
      COMPLEX*16         A( LDA, * ), TAUP( * ), TAUQ( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEBRD reduces a general complex M-by-N matrix A to upper or lower
*  bidiagonal form B by a unitary transformation: Q**H * A * P = B.
*
*  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the M-by-N general matrix to be reduced.
*          On exit,
*          if m >= n, the diagonal and the first superdiagonal are
*            overwritten with the upper bidiagonal matrix B; the
*            elements below the diagonal, with the array TAUQ, represent
*            the unitary matrix Q as a product of elementary
*            reflectors, and the elements above the first superdiagonal,
*            with the array TAUP, represent the unitary matrix P as
*            a product of elementary reflectors;
*          if m < n, the diagonal and the first subdiagonal are
*            overwritten with the lower bidiagonal matrix B; the
*            elements below the first subdiagonal, with the array TAUQ,
*            represent the unitary matrix Q as a product of
*            elementary reflectors, and the elements above the diagonal,
*            with the array TAUP, represent the unitary matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The diagonal elements of the bidiagonal matrix B:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
*          The off-diagonal elements of the bidiagonal matrix B:
*          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
*          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
*
*  TAUQ    (output) COMPLEX*16 array dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the unitary matrix Q. See Further Details.
*
*  TAUP    (output) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors which
*          represent the unitary matrix P. See Further Details.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= max(1,M,N).
*          For optimum performance LWORK >= (M+N)*NB, where NB
*          is the optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*  If m >= n,
*
*     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are complex scalars, and v and u are complex
*  vectors; v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in
*  A(i+1:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in
*  A(i,i+2:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n,
*
*     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are complex scalars, and v and u are complex
*  vectors; v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in
*  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in
*  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The contents of A on exit are illustrated by the following examples:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
*    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
*    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
*    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
*    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
*    (  v1  v2  v3  v4  v5 )
*
*  where d and e denote diagonal and off-diagonal elements of B, vi
*  denotes an element of the vector defining H(i), and ui an element of
*  the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IINFO, J, LDWRKX, LDWRKY, LWKOPT, MINMN, NB,
     $                   NBMIN, NX
      DOUBLE PRECISION   WS
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEBD2, ZGEMM, ZLABRD
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      NB = MAX( 1, ILAENV( 1, 'ZGEBRD', ' ', M, N, -1, -1 ) )
      LWKOPT = ( M+N )*NB
      WORK( 1 ) = DBLE( LWKOPT )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, M, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -10
      END IF
      IF( INFO.LT.0 ) THEN
         CALL XERBLA( 'ZGEBRD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      MINMN = MIN( M, N )
      IF( MINMN.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      WS = MAX( M, N )
      LDWRKX = M
      LDWRKY = N
*
      IF( NB.GT.1 .AND. NB.LT.MINMN ) THEN
*
*        Set the crossover point NX.
*
         NX = MAX( NB, ILAENV( 3, 'ZGEBRD', ' ', M, N, -1, -1 ) )
*
*        Determine when to switch from blocked to unblocked code.
*
         IF( NX.LT.MINMN ) THEN
            WS = ( M+N )*NB
            IF( LWORK.LT.WS ) THEN
*
*              Not enough work space for the optimal NB, consider using
*              a smaller block size.
*
               NBMIN = ILAENV( 2, 'ZGEBRD', ' ', M, N, -1, -1 )
               IF( LWORK.GE.( M+N )*NBMIN ) THEN
                  NB = LWORK / ( M+N )
               ELSE
                  NB = 1
                  NX = MINMN
               END IF
            END IF
         END IF
      ELSE
         NX = MINMN
      END IF
*
      DO 30 I = 1, MINMN - NX, NB
*
*        Reduce rows and columns i:i+ib-1 to bidiagonal form and return
*        the matrices X and Y which are needed to update the unreduced
*        part of the matrix
*
         CALL ZLABRD( M-I+1, N-I+1, NB, A( I, I ), LDA, D( I ), E( I ),
     $                TAUQ( I ), TAUP( I ), WORK, LDWRKX,
     $                WORK( LDWRKX*NB+1 ), LDWRKY )
*
*        Update the trailing submatrix A(i+ib:m,i+ib:n), using
*        an update of the form  A := A - V*Y' - X*U'
*
         CALL ZGEMM( 'No transpose', 'Conjugate transpose', M-I-NB+1,
     $               N-I-NB+1, NB, -ONE, A( I+NB, I ), LDA,
     $               WORK( LDWRKX*NB+NB+1 ), LDWRKY, ONE,
     $               A( I+NB, I+NB ), LDA )
         CALL ZGEMM( 'No transpose', 'No transpose', M-I-NB+1, N-I-NB+1,
     $               NB, -ONE, WORK( NB+1 ), LDWRKX, A( I, I+NB ), LDA,
     $               ONE, A( I+NB, I+NB ), LDA )
*
*        Copy diagonal and off-diagonal elements of B back into A
*
         IF( M.GE.N ) THEN
            DO 10 J = I, I + NB - 1
               A( J, J ) = D( J )
               A( J, J+1 ) = E( J )
   10       CONTINUE
         ELSE
            DO 20 J = I, I + NB - 1
               A( J, J ) = D( J )
               A( J+1, J ) = E( J )
   20       CONTINUE
         END IF
   30 CONTINUE
*
*     Use unblocked code to reduce the remainder of the matrix
*
      CALL ZGEBD2( M-I+1, N-I+1, A( I, I ), LDA, D( I ), E( I ),
     $             TAUQ( I ), TAUP( I ), WORK, IINFO )
      WORK( 1 ) = WS
      RETURN
*
*     End of ZGEBRD
*
      END
      SUBROUTINE ZGEEV( JOBVL, JOBVR, N, A, LDA, W, VL, LDVL, VR, LDVR,
     $                  WORK, LWORK, RWORK, INFO )
*
*  -- LAPACK driver routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVL, JOBVR
      INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   W( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEEV computes for an N-by-N complex nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*
*  The right eigenvector v(j) of A satisfies
*                   A * v(j) = lambda(j) * v(j)
*  where lambda(j) is its eigenvalue.
*  The left eigenvector u(j) of A satisfies
*                u(j)**H * A = lambda(j) * u(j)**H
*  where u(j)**H denotes the conjugate transpose of u(j).
*
*  The computed eigenvectors are normalized to have Euclidean norm
*  equal to 1 and largest component real.
*
*  Arguments
*  =========
*
*  JOBVL   (input) CHARACTER*1
*          = 'N': left eigenvectors of A are not computed;
*          = 'V': left eigenvectors of are computed.
*
*  JOBVR   (input) CHARACTER*1
*          = 'N': right eigenvectors of A are not computed;
*          = 'V': right eigenvectors of A are computed.
*
*  N       (input) INTEGER
*          The order of the matrix A. N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the N-by-N matrix A.
*          On exit, A has been overwritten.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  W       (output) COMPLEX*16 array, dimension (N)
*          W contains the computed eigenvalues.
*
*  VL      (output) COMPLEX*16 array, dimension (LDVL,N)
*          If JOBVL = 'V', the left eigenvectors u(j) are stored one
*          after another in the columns of VL, in the same order
*          as their eigenvalues.
*          If JOBVL = 'N', VL is not referenced.
*          u(j) = VL(:,j), the j-th column of VL.
*
*  LDVL    (input) INTEGER
*          The leading dimension of the array VL.  LDVL >= 1; if
*          JOBVL = 'V', LDVL >= N.
*
*  VR      (output) COMPLEX*16 array, dimension (LDVR,N)
*          If JOBVR = 'V', the right eigenvectors v(j) are stored one
*          after another in the columns of VR, in the same order
*          as their eigenvalues.
*          If JOBVR = 'N', VR is not referenced.
*          v(j) = VR(:,j), the j-th column of VR.
*
*  LDVR    (input) INTEGER
*          The leading dimension of the array VR.  LDVR >= 1; if
*          JOBVR = 'V', LDVR >= N.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,2*N).
*          For good performance, LWORK must generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = i, the QR algorithm failed to compute all the
*                eigenvalues, and no eigenvectors have been computed;
*                elements and i+1:N of W contain eigenvalues which have
*                converged.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, SCALEA, WANTVL, WANTVR
      CHARACTER          SIDE
      INTEGER            HSWORK, I, IBAL, IERR, IHI, ILO, IRWORK, ITAU,
     $                   IWRK, K, MAXB, MAXWRK, MINWRK, NOUT
      DOUBLE PRECISION   ANRM, BIGNUM, CSCALE, EPS, SCL, SMLNUM
      COMPLEX*16         TMP
*     ..
*     .. Local Arrays ..
      LOGICAL            SELECT( 1 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZDSCAL, ZGEBAK, ZGEBAL, ZGEHRD, ZHSEQR,
     $                   ZLACPY, ZLASCL, ZSCAL, ZTREVC, ZUNGHR
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX, ILAENV
      DOUBLE PRECISION   DLAMCH, DZNRM2, ZLANGE
      EXTERNAL           LSAME, IDAMAX, ILAENV, DLAMCH, DZNRM2, ZLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, DCONJG, DIMAG, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      WANTVL = LSAME( JOBVL, 'V' )
      WANTVR = LSAME( JOBVR, 'V' )
      IF( ( .NOT.WANTVL ) .AND. ( .NOT.LSAME( JOBVL, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( ( .NOT.WANTVR ) .AND. ( .NOT.LSAME( JOBVR, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDVL.LT.1 .OR. ( WANTVL .AND. LDVL.LT.N ) ) THEN
         INFO = -8
      ELSE IF( LDVR.LT.1 .OR. ( WANTVR .AND. LDVR.LT.N ) ) THEN
         INFO = -10
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       CWorkspace refers to complex workspace, and RWorkspace to real
*       workspace. NB refers to the optimal block size for the
*       immediately following subroutine, as returned by ILAENV.
*       HSWORK refers to the workspace preferred by ZHSEQR, as
*       calculated below. HSWORK is computed assuming ILO=1 and IHI=N,
*       the worst case.)
*
      MINWRK = 1
      IF( INFO.EQ.0 .AND. ( LWORK.GE.1 .OR. LQUERY ) ) THEN
         MAXWRK = N + N*ILAENV( 1, 'ZGEHRD', ' ', N, 1, N, 0 )
         IF( ( .NOT.WANTVL ) .AND. ( .NOT.WANTVR ) ) THEN
            MINWRK = MAX( 1, 2*N )
            MAXB = MAX( ILAENV( 8, 'ZHSEQR', 'EN', N, 1, N, -1 ), 2 )
            K = MIN( MAXB, N, MAX( 2, ILAENV( 4, 'ZHSEQR', 'EN', N, 1,
     $          N, -1 ) ) )
            HSWORK = MAX( K*( K+2 ), 2*N )
            MAXWRK = MAX( MAXWRK, HSWORK )
         ELSE
            MINWRK = MAX( 1, 2*N )
            MAXWRK = MAX( MAXWRK, N+( N-1 )*
     $               ILAENV( 1, 'ZUNGHR', ' ', N, 1, N, -1 ) )
            MAXB = MAX( ILAENV( 8, 'ZHSEQR', 'SV', N, 1, N, -1 ), 2 )
            K = MIN( MAXB, N, MAX( 2, ILAENV( 4, 'ZHSEQR', 'SV', N, 1,
     $          N, -1 ) ) )
            HSWORK = MAX( K*( K+2 ), 2*N )
            MAXWRK = MAX( MAXWRK, HSWORK, 2*N )
         END IF
         WORK( 1 ) = MAXWRK
      END IF
      IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEEV ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Get machine constants
*
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SQRT( SMLNUM ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = ZLANGE( 'M', N, N, A, LDA, DUM )
      SCALEA = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = SMLNUM
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = BIGNUM
      END IF
      IF( SCALEA )
     $   CALL ZLASCL( 'G', 0, 0, ANRM, CSCALE, N, N, A, LDA, IERR )
*
*     Balance the matrix
*     (CWorkspace: none)
*     (RWorkspace: need N)
*
      IBAL = 1
      CALL ZGEBAL( 'B', N, A, LDA, ILO, IHI, RWORK( IBAL ), IERR )
*
*     Reduce to upper Hessenberg form
*     (CWorkspace: need 2*N, prefer N+N*NB)
*     (RWorkspace: none)
*
      ITAU = 1
      IWRK = ITAU + N
      CALL ZGEHRD( N, ILO, IHI, A, LDA, WORK( ITAU ), WORK( IWRK ),
     $             LWORK-IWRK+1, IERR )
*
      IF( WANTVL ) THEN
*
*        Want left eigenvectors
*        Copy Householder vectors to VL
*
         SIDE = 'L'
         CALL ZLACPY( 'L', N, N, A, LDA, VL, LDVL )
*
*        Generate unitary matrix in VL
*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB)
*        (RWorkspace: none)
*
         CALL ZUNGHR( N, ILO, IHI, VL, LDVL, WORK( ITAU ), WORK( IWRK ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VL
*        (CWorkspace: need 1, prefer HSWORK (see comments) )
*        (RWorkspace: none)
*
         IWRK = ITAU
         CALL ZHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, W, VL, LDVL,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
         IF( WANTVR ) THEN
*
*           Want left and right eigenvectors
*           Copy Schur vectors to VR
*
            SIDE = 'B'
            CALL ZLACPY( 'F', N, N, VL, LDVL, VR, LDVR )
         END IF
*
      ELSE IF( WANTVR ) THEN
*
*        Want right eigenvectors
*        Copy Householder vectors to VR
*
         SIDE = 'R'
         CALL ZLACPY( 'L', N, N, A, LDA, VR, LDVR )
*
*        Generate unitary matrix in VR
*        (CWorkspace: need 2*N-1, prefer N+(N-1)*NB)
*        (RWorkspace: none)
*
         CALL ZUNGHR( N, ILO, IHI, VR, LDVR, WORK( ITAU ), WORK( IWRK ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VR
*        (CWorkspace: need 1, prefer HSWORK (see comments) )
*        (RWorkspace: none)
*
         IWRK = ITAU
         CALL ZHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, W, VR, LDVR,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
      ELSE
*
*        Compute eigenvalues only
*        (CWorkspace: need 1, prefer HSWORK (see comments) )
*        (RWorkspace: none)
*
         IWRK = ITAU
         CALL ZHSEQR( 'E', 'N', N, ILO, IHI, A, LDA, W, VR, LDVR,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
      END IF
*
*     If INFO > 0 from ZHSEQR, then quit
*
      IF( INFO.GT.0 )
     $   GO TO 50
*
      IF( WANTVL .OR. WANTVR ) THEN
*
*        Compute left and/or right eigenvectors
*        (CWorkspace: need 2*N)
*        (RWorkspace: need 2*N)
*
         IRWORK = IBAL + N
         CALL ZTREVC( SIDE, 'B', SELECT, N, A, LDA, VL, LDVL, VR, LDVR,
     $                N, NOUT, WORK( IWRK ), RWORK( IRWORK ), IERR )
      END IF
*
      IF( WANTVL ) THEN
*
*        Undo balancing of left eigenvectors
*        (CWorkspace: none)
*        (RWorkspace: need N)
*
         CALL ZGEBAK( 'B', 'L', N, ILO, IHI, RWORK( IBAL ), N, VL, LDVL,
     $                IERR )
*
*        Normalize left eigenvectors and make largest component real
*
         DO 20 I = 1, N
            SCL = ONE / DZNRM2( N, VL( 1, I ), 1 )
            CALL ZDSCAL( N, SCL, VL( 1, I ), 1 )
            DO 10 K = 1, N
               RWORK( IRWORK+K-1 ) = DBLE( VL( K, I ) )**2 +
     $                               DIMAG( VL( K, I ) )**2
   10       CONTINUE
            K = IDAMAX( N, RWORK( IRWORK ), 1 )
            TMP = DCONJG( VL( K, I ) ) / SQRT( RWORK( IRWORK+K-1 ) )
            CALL ZSCAL( N, TMP, VL( 1, I ), 1 )
            VL( K, I ) = DCMPLX( DBLE( VL( K, I ) ), ZERO )
   20    CONTINUE
      END IF
*
      IF( WANTVR ) THEN
*
*        Undo balancing of right eigenvectors
*        (CWorkspace: none)
*        (RWorkspace: need N)
*
         CALL ZGEBAK( 'B', 'R', N, ILO, IHI, RWORK( IBAL ), N, VR, LDVR,
     $                IERR )
*
*        Normalize right eigenvectors and make largest component real
*
         DO 40 I = 1, N
            SCL = ONE / DZNRM2( N, VR( 1, I ), 1 )
            CALL ZDSCAL( N, SCL, VR( 1, I ), 1 )
            DO 30 K = 1, N
               RWORK( IRWORK+K-1 ) = DBLE( VR( K, I ) )**2 +
     $                               DIMAG( VR( K, I ) )**2
   30       CONTINUE
            K = IDAMAX( N, RWORK( IRWORK ), 1 )
            TMP = DCONJG( VR( K, I ) ) / SQRT( RWORK( IRWORK+K-1 ) )
            CALL ZSCAL( N, TMP, VR( 1, I ), 1 )
            VR( K, I ) = DCMPLX( DBLE( VR( K, I ) ), ZERO )
   40    CONTINUE
      END IF
*
*     Undo scaling if necessary
*
   50 CONTINUE
      IF( SCALEA ) THEN
         CALL ZLASCL( 'G', 0, 0, CSCALE, ANRM, N-INFO, 1, W( INFO+1 ),
     $                MAX( N-INFO, 1 ), IERR )
         IF( INFO.GT.0 ) THEN
            CALL ZLASCL( 'G', 0, 0, CSCALE, ANRM, ILO-1, 1, W, N, IERR )
         END IF
      END IF
*
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of ZGEEV
*
      END
      SUBROUTINE ZGEHD2( N, ILO, IHI, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            IHI, ILO, INFO, LDA, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEHD2 reduces a complex general matrix A to upper Hessenberg form H
*  by a unitary similarity transformation:  Q' * A * Q = H .
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          It is assumed that A is already upper triangular in rows
*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
*          set by a previous call to ZGEBAL; otherwise they should be
*          set to 1 and N respectively. See Further Details.
*          1 <= ILO <= IHI <= max(1,N).
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the n by n general matrix to be reduced.
*          On exit, the upper triangle and the first subdiagonal of A
*          are overwritten with the upper Hessenberg matrix H, and the
*          elements below the first subdiagonal, with the array TAU,
*          represent the unitary matrix Q as a product of elementary
*          reflectors. See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  TAU     (output) COMPLEX*16 array, dimension (N-1)
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) COMPLEX*16 array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of (ihi-ilo) elementary
*  reflectors
*
*     Q = H(ilo) H(ilo+1) . . . H(ihi-1).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on
*  exit in A(i+2:ihi,i), and tau in TAU(i).
*
*  The contents of A are illustrated by the following example, with
*  n = 7, ilo = 2 and ihi = 6:
*
*  on entry,                        on exit,
*
*  ( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a )
*  (     a   a   a   a   a   a )    (      a   h   h   h   h   a )
*  (     a   a   a   a   a   a )    (      h   h   h   h   h   h )
*  (     a   a   a   a   a   a )    (      v2  h   h   h   h   h )
*  (     a   a   a   a   a   a )    (      v2  v3  h   h   h   h )
*  (     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h )
*  (                         a )    (                          a )
*
*  where a denotes an element of the original matrix A, h denotes a
*  modified element of the upper Hessenberg matrix H, and vi denotes an
*  element of the vector defining H(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      COMPLEX*16         ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARF, ZLARFG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN
         INFO = -2
      ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEHD2', -INFO )
         RETURN
      END IF
*
      DO 10 I = ILO, IHI - 1
*
*        Compute elementary reflector H(i) to annihilate A(i+2:ihi,i)
*
         ALPHA = A( I+1, I )
         CALL ZLARFG( IHI-I, ALPHA, A( MIN( I+2, N ), I ), 1, TAU( I ) )
         A( I+1, I ) = ONE
*
*        Apply H(i) to A(1:ihi,i+1:ihi) from the right
*
         CALL ZLARF( 'Right', IHI, IHI-I, A( I+1, I ), 1, TAU( I ),
     $               A( 1, I+1 ), LDA, WORK )
*
*        Apply H(i)' to A(i+1:ihi,i+1:n) from the left
*
         CALL ZLARF( 'Left', IHI-I, N-I, A( I+1, I ), 1,
     $               DCONJG( TAU( I ) ), A( I+1, I+1 ), LDA, WORK )
*
         A( I+1, I ) = ALPHA
   10 CONTINUE
*
      RETURN
*
*     End of ZGEHD2
*
      END
      SUBROUTINE ZGEHRD( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            IHI, ILO, INFO, LDA, LWORK, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEHRD reduces a complex general matrix A to upper Hessenberg form H
*  by a unitary similarity transformation:  Q' * A * Q = H .
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          It is assumed that A is already upper triangular in rows
*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
*          set by a previous call to ZGEBAL; otherwise they should be
*          set to 1 and N respectively. See Further Details.
*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the N-by-N general matrix to be reduced.
*          On exit, the upper triangle and the first subdiagonal of A
*          are overwritten with the upper Hessenberg matrix H, and the
*          elements below the first subdiagonal, with the array TAU,
*          represent the unitary matrix Q as a product of elementary
*          reflectors. See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  TAU     (output) COMPLEX*16 array, dimension (N-1)
*          The scalar factors of the elementary reflectors (see Further
*          Details). Elements 1:ILO-1 and IHI:N-1 of TAU are set to
*          zero.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is the
*          optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of (ihi-ilo) elementary
*  reflectors
*
*     Q = H(ilo) H(ilo+1) . . . H(ihi-1).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i) = 0, v(i+1) = 1 and v(ihi+1:n) = 0; v(i+2:ihi) is stored on
*  exit in A(i+2:ihi,i), and tau in TAU(i).
*
*  The contents of A are illustrated by the following example, with
*  n = 7, ilo = 2 and ihi = 6:
*
*  on entry,                        on exit,
*
*  ( a   a   a   a   a   a   a )    (  a   a   h   h   h   h   a )
*  (     a   a   a   a   a   a )    (      a   h   h   h   h   a )
*  (     a   a   a   a   a   a )    (      h   h   h   h   h   h )
*  (     a   a   a   a   a   a )    (      v2  h   h   h   h   h )
*  (     a   a   a   a   a   a )    (      v2  v3  h   h   h   h )
*  (     a   a   a   a   a   a )    (      v2  v3  v4  h   h   h )
*  (                         a )    (                          a )
*
*  where a denotes an element of the original matrix A, h denotes a
*  modified element of the upper Hessenberg matrix H, and vi denotes an
*  element of the vector defining H(i).
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, LDWORK, LWKOPT, NB, NBMIN,
     $                   NH, NX
      COMPLEX*16         EI
*     ..
*     .. Local Arrays ..
      COMPLEX*16         T( LDT, NBMAX )
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEHD2, ZGEMM, ZLAHRD, ZLARFB
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      NB = MIN( NBMAX, ILAENV( 1, 'ZGEHRD', ' ', N, ILO, IHI, -1 ) )
      LWKOPT = N*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN
         INFO = -2
      ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEHRD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Set elements 1:ILO-1 and IHI:N-1 of TAU to zero
*
      DO 10 I = 1, ILO - 1
         TAU( I ) = ZERO
   10 CONTINUE
      DO 20 I = MAX( 1, IHI ), N - 1
         TAU( I ) = ZERO
   20 CONTINUE
*
*     Quick return if possible
*
      NH = IHI - ILO + 1
      IF( NH.LE.1 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      IWS = 1
      IF( NB.GT.1 .AND. NB.LT.NH ) THEN
*
*        Determine when to cross over from blocked to unblocked code
*        (last block is always handled by unblocked code).
*
         NX = MAX( NB, ILAENV( 3, 'ZGEHRD', ' ', N, ILO, IHI, -1 ) )
         IF( NX.LT.NH ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            IWS = N*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  determine the
*              minimum value of NB, and reduce NB or force use of
*              unblocked code.
*
               NBMIN = MAX( 2, ILAENV( 2, 'ZGEHRD', ' ', N, ILO, IHI,
     $                 -1 ) )
               IF( LWORK.GE.N*NBMIN ) THEN
                  NB = LWORK / N
               ELSE
                  NB = 1
               END IF
            END IF
         END IF
      END IF
      LDWORK = N
*
      IF( NB.LT.NBMIN .OR. NB.GE.NH ) THEN
*
*        Use unblocked code below
*
         I = ILO
*
      ELSE
*
*        Use blocked code
*
         DO 30 I = ILO, IHI - 1 - NX, NB
            IB = MIN( NB, IHI-I )
*
*           Reduce columns i:i+ib-1 to Hessenberg form, returning the
*           matrices V and T of the block reflector H = I - V*T*V'
*           which performs the reduction, and also the matrix Y = A*V*T
*
            CALL ZLAHRD( IHI, I, IB, A( 1, I ), LDA, TAU( I ), T, LDT,
     $                   WORK, LDWORK )
*
*           Apply the block reflector H to A(1:ihi,i+ib:ihi) from the
*           right, computing  A := A - Y * V'. V(i+ib,ib-1) must be set
*           to 1.
*
            EI = A( I+IB, I+IB-1 )
            A( I+IB, I+IB-1 ) = ONE
            CALL ZGEMM( 'No transpose', 'Conjugate transpose', IHI,
     $                  IHI-I-IB+1, IB, -ONE, WORK, LDWORK,
     $                  A( I+IB, I ), LDA, ONE, A( 1, I+IB ), LDA )
            A( I+IB, I+IB-1 ) = EI
*
*           Apply the block reflector H to A(i+1:ihi,i+ib:n) from the
*           left
*
            CALL ZLARFB( 'Left', 'Conjugate transpose', 'Forward',
     $                   'Columnwise', IHI-I, N-I-IB+1, IB, A( I+1, I ),
     $                   LDA, T, LDT, A( I+1, I+IB ), LDA, WORK,
     $                   LDWORK )
   30    CONTINUE
      END IF
*
*     Use unblocked code to reduce the rest of the matrix
*
      CALL ZGEHD2( N, I, IHI, A, LDA, TAU, WORK, IINFO )
      WORK( 1 ) = IWS
*
      RETURN
*
*     End of ZGEHRD
*
      END
      SUBROUTINE ZGELQ2( M, N, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGELQ2 computes an LQ factorization of a complex m by n matrix A:
*  A = L * Q.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, the elements on and below the diagonal of the array
*          contain the m by min(m,n) lower trapezoidal matrix L (L is
*          lower triangular if m <= n); the elements above the diagonal,
*          with the array TAU, represent the unitary matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) COMPLEX*16 array, dimension (M)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(k)' . . . H(2)' H(1)', where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i-1) = 0 and v(i) = 1; conjg(v(i+1:n)) is stored on exit in
*  A(i,i+1:n), and tau in TAU(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K
      COMPLEX*16         ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLACGV, ZLARF, ZLARFG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGELQ2', -INFO )
         RETURN
      END IF
*
      K = MIN( M, N )
*
      DO 10 I = 1, K
*
*        Generate elementary reflector H(i) to annihilate A(i,i+1:n)
*
         CALL ZLACGV( N-I+1, A( I, I ), LDA )
         ALPHA = A( I, I )
         CALL ZLARFG( N-I+1, ALPHA, A( I, MIN( I+1, N ) ), LDA,
     $                TAU( I ) )
         IF( I.LT.M ) THEN
*
*           Apply H(i) to A(i+1:m,i:n) from the right
*
            A( I, I ) = ONE
            CALL ZLARF( 'Right', M-I, N-I+1, A( I, I ), LDA, TAU( I ),
     $                  A( I+1, I ), LDA, WORK )
         END IF
         A( I, I ) = ALPHA
         CALL ZLACGV( N-I+1, A( I, I ), LDA )
   10 CONTINUE
      RETURN
*
*     End of ZGELQ2
*
      END
      SUBROUTINE ZGELQF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGELQF computes an LQ factorization of a complex M-by-N matrix A:
*  A = L * Q.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the elements on and below the diagonal of the array
*          contain the m-by-min(m,n) lower trapezoidal matrix L (L is
*          lower triangular if m <= n); the elements above the diagonal,
*          with the array TAU, represent the unitary matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,M).
*          For optimum performance LWORK >= M*NB, where NB is the
*          optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(k)' . . . H(2)' H(1)', where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i-1) = 0 and v(i) = 1; conjg(v(i+1:n)) is stored on exit in
*  A(i,i+1:n), and tau in TAU(i).
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, LWKOPT, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGELQ2, ZLARFB, ZLARFT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      NB = ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
      LWKOPT = M*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, M ) .AND. .NOT.LQUERY ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGELQF', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'ZGELQF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'ZGELQF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code initially
*
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
*
*           Compute the LQ factorization of the current block
*           A(i:i+ib-1,i:n)
*
            CALL ZGELQ2( IB, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.M ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL ZLARFT( 'Forward', 'Rowwise', N-I+1, IB, A( I, I ),
     $                      LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(i+ib:m,i:n) from the right
*
               CALL ZLARFB( 'Right', 'No transpose', 'Forward',
     $                      'Rowwise', M-I-IB+1, N-I+1, IB, A( I, I ),
     $                      LDA, WORK, LDWORK, A( I+IB, I ), LDA,
     $                      WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
*
*     Use unblocked code to factor the last or only block.
*
      IF( I.LE.K )
     $   CALL ZGELQ2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of ZGELQF
*
      END
      SUBROUTINE ZGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, RWORK,
     $                   INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEQP3 computes a QR factorization with column pivoting of a
*  matrix A:  A*P = Q*R  using Level 3 BLAS.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the upper triangle of the array contains the
*          min(M,N)-by-N upper trapezoidal matrix R; the elements below
*          the diagonal, together with the array TAU, represent the
*          unitary matrix Q as a product of min(M,N) elementary
*          reflectors.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(J).ne.0, the J-th column of A is permuted
*          to the front of A*P (a leading column); if JPVT(J)=0,
*          the J-th column of A is a free column.
*          On exit, if JPVT(J)=K, then the J-th column of A*P was the
*          the K-th column of A.
*
*  TAU     (output) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO=0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= N+1.
*          For optimal performance LWORK >= ( N+1 )*NB, where NB
*          is the optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit.
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a real/complex scalar, and v is a real/complex vector
*  with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
*  A(i+1:m,i), and tau in TAU(i).
*
*  Based on contributions by
*    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*    X. Sun, Computer Science Dept., Duke University, USA
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            INB, INBMIN, IXOVER
      PARAMETER          ( INB = 1, INBMIN = 2, IXOVER = 3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            FJB, IWS, J, JB, LWKOPT, MINMN, MINWS, NA, NB,
     $                   NBMIN, NFXD, NX, SM, SMINMN, SN, TOPBMN
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEQRF, ZLAQP2, ZLAQPS, ZSWAP, ZUNMQR
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DZNRM2
      EXTERNAL           ILAENV, DZNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      IWS = N + 1
      MINMN = MIN( M, N )
*
*     Test input arguments
*     ====================
*
      INFO = 0
      NB = ILAENV( INB, 'ZGEQRF', ' ', M, N, -1, -1 )
      LWKOPT = ( N+1 )*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( ( LWORK.LT.IWS ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEQP3', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( MINMN.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Move initial columns up front.
*
      NFXD = 1
      DO 10 J = 1, N
         IF( JPVT( J ).NE.0 ) THEN
            IF( J.NE.NFXD ) THEN
               CALL ZSWAP( M, A( 1, J ), 1, A( 1, NFXD ), 1 )
               JPVT( J ) = JPVT( NFXD )
               JPVT( NFXD ) = J
            ELSE
               JPVT( J ) = J
            END IF
            NFXD = NFXD + 1
         ELSE
            JPVT( J ) = J
         END IF
   10 CONTINUE
      NFXD = NFXD - 1
*
*     Factorize fixed columns
*     =======================
*
*     Compute the QR factorization of fixed columns and update
*     remaining columns.
*
      IF( NFXD.GT.0 ) THEN
         NA = MIN( M, NFXD )
*CC      CALL ZGEQR2( M, NA, A, LDA, TAU, WORK, INFO )
         CALL ZGEQRF( M, NA, A, LDA, TAU, WORK, LWORK, INFO )
         IWS = MAX( IWS, INT( WORK( 1 ) ) )
         IF( NA.LT.N ) THEN
*CC         CALL ZUNM2R( 'Left', 'Conjugate Transpose', M, N-NA,
*CC  $                   NA, A, LDA, TAU, A( 1, NA+1 ), LDA, WORK,
*CC  $                   INFO )
            CALL ZUNMQR( 'Left', 'Conjugate Transpose', M, N-NA, NA, A,
     $                   LDA, TAU, A( 1, NA+1 ), LDA, WORK, LWORK,
     $                   INFO )
            IWS = MAX( IWS, INT( WORK( 1 ) ) )
         END IF
      END IF
*
*     Factorize free columns
*     ======================
*
      IF( NFXD.LT.MINMN ) THEN
*
         SM = M - NFXD
         SN = N - NFXD
         SMINMN = MINMN - NFXD
*
*        Determine the block size.
*
         NB = ILAENV( INB, 'ZGEQRF', ' ', SM, SN, -1, -1 )
         NBMIN = 2
         NX = 0
*
         IF( ( NB.GT.1 ) .AND. ( NB.LT.SMINMN ) ) THEN
*
*           Determine when to cross over from blocked to unblocked code.
*
            NX = MAX( 0, ILAENV( IXOVER, 'ZGEQRF', ' ', SM, SN, -1,
     $           -1 ) )
*
*
            IF( NX.LT.SMINMN ) THEN
*
*              Determine if workspace is large enough for blocked code.
*
               MINWS = ( SN+1 )*NB
               IWS = MAX( IWS, MINWS )
               IF( LWORK.LT.MINWS ) THEN
*
*                 Not enough workspace to use optimal NB: Reduce NB and
*                 determine the minimum value of NB.
*
                  NB = LWORK / ( SN+1 )
                  NBMIN = MAX( 2, ILAENV( INBMIN, 'ZGEQRF', ' ', SM, SN,
     $                    -1, -1 ) )
*
*
               END IF
            END IF
         END IF
*
*        Initialize partial column norms. The first N elements of work
*        store the exact column norms.
*
         DO 20 J = NFXD + 1, N
            RWORK( J ) = DZNRM2( SM, A( NFXD+1, J ), 1 )
            RWORK( N+J ) = RWORK( J )
   20    CONTINUE
*
         IF( ( NB.GE.NBMIN ) .AND. ( NB.LT.SMINMN ) .AND.
     $       ( NX.LT.SMINMN ) ) THEN
*
*           Use blocked code initially.
*
            J = NFXD + 1
*
*           Compute factorization: while loop.
*
*
            TOPBMN = MINMN - NX
   30       CONTINUE
            IF( J.LE.TOPBMN ) THEN
               JB = MIN( NB, TOPBMN-J+1 )
*
*              Factorize JB columns among columns J:N.
*
               CALL ZLAQPS( M, N-J+1, J-1, JB, FJB, A( 1, J ), LDA,
     $                      JPVT( J ), TAU( J ), RWORK( J ),
     $                      RWORK( N+J ), WORK( 1 ), WORK( JB+1 ),
     $                      N-J+1 )
*
               J = J + FJB
               GO TO 30
            END IF
         ELSE
            J = NFXD + 1
         END IF
*
*        Use unblocked code to factor the last or only block.
*
*
         IF( J.LE.MINMN )
     $      CALL ZLAQP2( M, N-J+1, J-1, A( 1, J ), LDA, JPVT( J ),
     $                   TAU( J ), RWORK( J ), RWORK( N+J ), WORK( 1 ) )
*
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of ZGEQP3
*
      END
      SUBROUTINE ZGEQR2( M, N, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEQR2 computes a QR factorization of a complex m by n matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(m,n) by n upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the unitary matrix Q as a
*          product of elementary reflectors (see Further Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace) COMPLEX*16 array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, K
      COMPLEX*16         ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARF, ZLARFG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEQR2', -INFO )
         RETURN
      END IF
*
      K = MIN( M, N )
*
      DO 10 I = 1, K
*
*        Generate elementary reflector H(i) to annihilate A(i+1:m,i)
*
         CALL ZLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                TAU( I ) )
         IF( I.LT.N ) THEN
*
*           Apply H(i)' to A(i:m,i+1:n) from the left
*
            ALPHA = A( I, I )
            A( I, I ) = ONE
            CALL ZLARF( 'Left', M-I+1, N-I, A( I, I ), 1,
     $                  DCONJG( TAU( I ) ), A( I, I+1 ), LDA, WORK )
            A( I, I ) = ALPHA
         END IF
   10 CONTINUE
      RETURN
*
*     End of ZGEQR2
*
      END
      SUBROUTINE ZGEQRF( M, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGEQRF computes a QR factorization of a complex M-by-N matrix A:
*  A = Q * R.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(M,N)-by-N upper trapezoidal matrix R (R is
*          upper triangular if m >= n); the elements below the diagonal,
*          with the array TAU, represent the unitary matrix Q as a
*          product of min(m,n) elementary reflectors (see Further
*          Details).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  TAU     (output) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is
*          the optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of elementary reflectors
*
*     Q = H(1) H(2) . . . H(k), where k = min(m,n).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
*  and tau in TAU(i).
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, K, LDWORK, LWKOPT, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEQR2, ZLARFB, ZLARFT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      NB = ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
      LWKOPT = N*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGEQRF', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      K = MIN( M, N )
      IF( K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'ZGEQRF', ' ', M, N, -1, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'ZGEQRF', ' ', M, N, -1,
     $                 -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code initially
*
         DO 10 I = 1, K - NX, NB
            IB = MIN( K-I+1, NB )
*
*           Compute the QR factorization of the current block
*           A(i:m,i:i+ib-1)
*
            CALL ZGEQR2( M-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
            IF( I+IB.LE.N ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL ZLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H' to A(i:m,i+ib:n) from the left
*
               CALL ZLARFB( 'Left', 'Conjugate transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
   10    CONTINUE
      ELSE
         I = 1
      END IF
*
*     Use unblocked code to factor the last or only block.
*
      IF( I.LE.K )
     $   CALL ZGEQR2( M-I+1, N-I+1, A( I, I ), LDA, TAU( I ), WORK,
     $                IINFO )
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of ZGEQRF
*
      END
      SUBROUTINE ZGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
*
*  -- LAPACK driver routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  ZGESV computes the solution to a complex system of linear equations
*     A * X = B,
*  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
*
*  The LU decomposition with partial pivoting and row interchanges is
*  used to factor A as
*     A = P * L * U,
*  where P is a permutation matrix, L is unit lower triangular, and U is
*  upper triangular.  The factored form of A is then used to solve the
*  system of equations A * X = B.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the N-by-N coefficient matrix A.
*          On exit, the factors L and U from the factorization
*          A = P*L*U; the unit diagonal elements of L are not stored.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  IPIV    (output) INTEGER array, dimension (N)
*          The pivot indices that define the permutation matrix P;
*          row i of the matrix was interchanged with row IPIV(i).
*
*  B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS matrix of right hand side matrix B.
*          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
*                has been completed, but the factor U is exactly
*                singular, so the solution could not be computed.
*
*  =====================================================================
*
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGETRF, ZGETRS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGESV ', -INFO )
         RETURN
      END IF
*
*     Compute the LU factorization of A.
*
      CALL ZGETRF( N, N, A, LDA, IPIV, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL ZGETRS( 'No transpose', N, NRHS, A, LDA, IPIV, B, LDB,
     $                INFO )
      END IF
      RETURN
*
*     End of ZGESV
*
      END
      SUBROUTINE ZGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
     $                   WORK, LWORK, RWORK, INFO )
*
*  -- LAPACK driver routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          JOBU, JOBVT
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   RWORK( * ), S( * )
      COMPLEX*16         A( LDA, * ), U( LDU, * ), VT( LDVT, * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZGESVD computes the singular value decomposition (SVD) of a complex
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*
*       A = U * SIGMA * conjugate-transpose(V)
*
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
*  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*
*  Note that the routine returns V**H, not V.
*
*  Arguments
*  =========
*
*  JOBU    (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix U:
*          = 'A':  all M columns of U are returned in array U:
*          = 'S':  the first min(m,n) columns of U (the left singular
*                  vectors) are returned in the array U;
*          = 'O':  the first min(m,n) columns of U (the left singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no columns of U (no left singular vectors) are
*                  computed.
*
*  JOBVT   (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix
*          V**H:
*          = 'A':  all N rows of V**H are returned in the array VT;
*          = 'S':  the first min(m,n) rows of V**H (the right singular
*                  vectors) are returned in the array VT;
*          = 'O':  the first min(m,n) rows of V**H (the right singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no rows of V**H (no right singular vectors) are
*                  computed.
*
*          JOBVT and JOBU cannot both be 'O'.
*
*  M       (input) INTEGER
*          The number of rows of the input matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the input matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*          if JOBU = 'O',  A is overwritten with the first min(m,n)
*                          columns of U (the left singular vectors,
*                          stored columnwise);
*          if JOBVT = 'O', A is overwritten with the first min(m,n)
*                          rows of V**H (the right singular vectors,
*                          stored rowwise);
*          if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
*                          are destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A, sorted so that S(i) >= S(i+1).
*
*  U       (output) COMPLEX*16 array, dimension (LDU,UCOL)
*          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
*          If JOBU = 'A', U contains the M-by-M unitary matrix U;
*          if JOBU = 'S', U contains the first min(m,n) columns of U
*          (the left singular vectors, stored columnwise);
*          if JOBU = 'N' or 'O', U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= 1; if
*          JOBU = 'S' or 'A', LDU >= M.
*
*  VT      (output) COMPLEX*16 array, dimension (LDVT,N)
*          If JOBVT = 'A', VT contains the N-by-N unitary matrix
*          V**H;
*          if JOBVT = 'S', VT contains the first min(m,n) rows of
*          V**H (the right singular vectors, stored rowwise);
*          if JOBVT = 'N' or 'O', VT is not referenced.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= 1; if
*          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 1.
*          LWORK >=  2*MIN(M,N)+MAX(M,N).
*          For good performance, LWORK should generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (5*min(M,N))
*          On exit, if INFO > 0, RWORK(1:MIN(M,N)-1) contains the
*          unconverged superdiagonal elements of an upper bidiagonal
*          matrix B whose diagonal is in S (not necessarily sorted).
*          B satisfies A = U * B * VT, so it has the same singular
*          values as A, and singular vectors related by U and VT.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if ZBDSQR did not converge, INFO specifies how many
*                superdiagonals of an intermediate bidiagonal form B
*                did not converge to zero. See the description of RWORK
*                above for details.
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D0, 0.0D0 ),
     $                   CONE = ( 1.0D0, 0.0D0 ) )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, WNTUA, WNTUAS, WNTUN, WNTUO, WNTUS,
     $                   WNTVA, WNTVAS, WNTVN, WNTVO, WNTVS
      INTEGER            BLK, CHUNK, I, IE, IERR, IR, IRWORK, ISCL,
     $                   ITAU, ITAUP, ITAUQ, IU, IWORK, LDWRKR, LDWRKU,
     $                   MAXWRK, MINMN, MINWRK, MNTHR, NCU, NCVT, NRU,
     $                   NRVT, WRKBL
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, SMLNUM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 1 )
      COMPLEX*16         CDUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASCL, XERBLA, ZBDSQR, ZGEBRD, ZGELQF, ZGEMM,
     $                   ZGEQRF, ZLACPY, ZLASCL, ZLASET, ZUNGBR, ZUNGLQ,
     $                   ZUNGQR, ZUNMBR
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, ZLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, ZLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      MINMN = MIN( M, N )
      MNTHR = ILAENV( 6, 'ZGESVD', JOBU // JOBVT, M, N, 0, 0 )
      WNTUA = LSAME( JOBU, 'A' )
      WNTUS = LSAME( JOBU, 'S' )
      WNTUAS = WNTUA .OR. WNTUS
      WNTUO = LSAME( JOBU, 'O' )
      WNTUN = LSAME( JOBU, 'N' )
      WNTVA = LSAME( JOBVT, 'A' )
      WNTVS = LSAME( JOBVT, 'S' )
      WNTVAS = WNTVA .OR. WNTVS
      WNTVO = LSAME( JOBVT, 'O' )
      WNTVN = LSAME( JOBVT, 'N' )
      MINWRK = 1
      LQUERY = ( LWORK.EQ.-1 )
*
      IF( .NOT.( WNTUA .OR. WNTUS .OR. WNTUO .OR. WNTUN ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( WNTVA .OR. WNTVS .OR. WNTVO .OR. WNTVN ) .OR.
     $         ( WNTVO .AND. WNTUO ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LDU.LT.1 .OR. ( WNTUAS .AND. LDU.LT.M ) ) THEN
         INFO = -9
      ELSE IF( LDVT.LT.1 .OR. ( WNTVA .AND. LDVT.LT.N ) .OR.
     $         ( WNTVS .AND. LDVT.LT.MINMN ) ) THEN
         INFO = -11
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       CWorkspace refers to complex workspace, and RWorkspace to
*       real workspace. NB refers to the optimal block size for the
*       immediately following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 .AND. ( LWORK.GE.1 .OR. LQUERY ) .AND. M.GT.0 .AND.
     $    N.GT.0 ) THEN
         IF( M.GE.N ) THEN
*
*           Space needed for ZBDSQR is BDSPAC = 5*N
*
            IF( M.GE.MNTHR ) THEN
               IF( WNTUN ) THEN
*
*                 Path 1 (M much larger than N, JOBU='N')
*
                  MAXWRK = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1,
     $                     -1 )
                  MAXWRK = MAX( MAXWRK, 2*N+2*N*
     $                     ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  IF( WNTVO .OR. WNTVAS )
     $               MAXWRK = MAX( MAXWRK, 2*N+( N-1 )*
     $                        ILAENV( 1, 'ZUNGBR', 'P', N, N, N, -1 ) )
                  MINWRK = 3*N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTUO .AND. WNTVN ) THEN
*
*                 Path 2 (M much larger than N, JOBU='O', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'ZUNGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+2*N*
     $                    ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+N*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', N, N, N, -1 ) )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N )
                  MINWRK = 2*N + M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTUO .AND. WNTVAS ) THEN
*
*                 Path 3 (M much larger than N, JOBU='O', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'ZUNGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+2*N*
     $                    ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+N*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+( N-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N )
                  MINWRK = 2*N + M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTUS .AND. WNTVN ) THEN
*
*                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'ZUNGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+2*N*
     $                    ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+N*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', N, N, N, -1 ) )
                  MAXWRK = N*N + WRKBL
                  MINWRK = 2*N + M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTUS .AND. WNTVO ) THEN
*
*                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
*
                  WRKBL = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'ZUNGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+2*N*
     $                    ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+N*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+( N-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = 2*N + M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTUS .AND. WNTVAS ) THEN
*
*                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'ZUNGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+2*N*
     $                    ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+N*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+( N-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = N*N + WRKBL
                  MINWRK = 2*N + M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTUA .AND. WNTVN ) THEN
*
*                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'ZUNGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+2*N*
     $                    ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+N*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', N, N, N, -1 ) )
                  MAXWRK = N*N + WRKBL
                  MINWRK = 2*N + M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTUA .AND. WNTVO ) THEN
*
*                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
*
                  WRKBL = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'ZUNGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+2*N*
     $                    ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+N*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+( N-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = 2*N + M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTUA .AND. WNTVAS ) THEN
*
*                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'ZGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'ZUNGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+2*N*
     $                    ILAENV( 1, 'ZGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+N*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 2*N+( N-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = N*N + WRKBL
                  MINWRK = 2*N + M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               END IF
            ELSE
*
*              Path 10 (M at least N, but not much larger)
*
               MAXWRK = 2*N + ( M+N )*ILAENV( 1, 'ZGEBRD', ' ', M, N,
     $                  -1, -1 )
               IF( WNTUS .OR. WNTUO )
     $            MAXWRK = MAX( MAXWRK, 2*N+N*
     $                     ILAENV( 1, 'ZUNGBR', 'Q', M, N, N, -1 ) )
               IF( WNTUA )
     $            MAXWRK = MAX( MAXWRK, 2*N+M*
     $                     ILAENV( 1, 'ZUNGBR', 'Q', M, M, N, -1 ) )
               IF( .NOT.WNTVN )
     $            MAXWRK = MAX( MAXWRK, 2*N+( N-1 )*
     $                     ILAENV( 1, 'ZUNGBR', 'P', N, N, N, -1 ) )
               MINWRK = 2*N + M
               MAXWRK = MAX( MINWRK, MAXWRK )
            END IF
         ELSE
*
*           Space needed for ZBDSQR is BDSPAC = 5*M
*
            IF( N.GE.MNTHR ) THEN
               IF( WNTVN ) THEN
*
*                 Path 1t(N much larger than M, JOBVT='N')
*
                  MAXWRK = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1,
     $                     -1 )
                  MAXWRK = MAX( MAXWRK, 2*M+2*M*
     $                     ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  IF( WNTUO .OR. WNTUAS )
     $               MAXWRK = MAX( MAXWRK, 2*M+M*
     $                        ILAENV( 1, 'ZUNGBR', 'Q', M, M, M, -1 ) )
                  MINWRK = 3*M
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTVO .AND. WNTUN ) THEN
*
*                 Path 2t(N much larger than M, JOBU='N', JOBVT='O')
*
                  WRKBL = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'ZUNGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+2*M*
     $                    ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+( M-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', M, M, M, -1 ) )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N )
                  MINWRK = 2*M + N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTVO .AND. WNTUAS ) THEN
*
*                 Path 3t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='O')
*
                  WRKBL = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'ZUNGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+2*M*
     $                    ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+( M-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+M*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N )
                  MINWRK = 2*M + N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTVS .AND. WNTUN ) THEN
*
*                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'ZUNGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+2*M*
     $                    ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+( M-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', M, M, M, -1 ) )
                  MAXWRK = M*M + WRKBL
                  MINWRK = 2*M + N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTVS .AND. WNTUO ) THEN
*
*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'ZUNGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+2*M*
     $                    ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+( M-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+M*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = 2*M + N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTVS .AND. WNTUAS ) THEN
*
*                 Path 6t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'ZUNGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+2*M*
     $                    ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+( M-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+M*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = M*M + WRKBL
                  MINWRK = 2*M + N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTVA .AND. WNTUN ) THEN
*
*                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'ZUNGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+2*M*
     $                    ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+( M-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', M, M, M, -1 ) )
                  MAXWRK = M*M + WRKBL
                  MINWRK = 2*M + N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTVA .AND. WNTUO ) THEN
*
*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'ZUNGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+2*M*
     $                    ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+( M-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+M*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = 2*M + N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               ELSE IF( WNTVA .AND. WNTUAS ) THEN
*
*                 Path 9t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'ZGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'ZUNGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+2*M*
     $                    ILAENV( 1, 'ZGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+( M-1 )*
     $                    ILAENV( 1, 'ZUNGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 2*M+M*
     $                    ILAENV( 1, 'ZUNGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = M*M + WRKBL
                  MINWRK = 2*M + N
                  MAXWRK = MAX( MINWRK, MAXWRK )
               END IF
            ELSE
*
*              Path 10t(N greater than M, but not much larger)
*
               MAXWRK = 2*M + ( M+N )*ILAENV( 1, 'ZGEBRD', ' ', M, N,
     $                  -1, -1 )
               IF( WNTVS .OR. WNTVO )
     $            MAXWRK = MAX( MAXWRK, 2*M+M*
     $                     ILAENV( 1, 'ZUNGBR', 'P', M, N, M, -1 ) )
               IF( WNTVA )
     $            MAXWRK = MAX( MAXWRK, 2*M+N*
     $                     ILAENV( 1, 'ZUNGBR', 'P', N, N, M, -1 ) )
               IF( .NOT.WNTUN )
     $            MAXWRK = MAX( MAXWRK, 2*M+( M-1 )*
     $                     ILAENV( 1, 'ZUNGBR', 'Q', M, M, M, -1 ) )
               MINWRK = 2*M + N
               MAXWRK = MAX( MINWRK, MAXWRK )
            END IF
         END IF
         WORK( 1 ) = MAXWRK
      END IF
*
      IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
         INFO = -13
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGESVD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         IF( LWORK.GE.1 )
     $      WORK( 1 ) = ONE
         RETURN
      END IF
*
*     Get machine constants
*
      EPS = DLAMCH( 'P' )
      SMLNUM = SQRT( DLAMCH( 'S' ) ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = ZLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ISCL = 1
         CALL ZLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ISCL = 1
         CALL ZLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
      END IF
*
      IF( M.GE.N ) THEN
*
*        A has at least as many rows as columns. If A has sufficiently
*        more rows than columns, first reduce using the QR
*        decomposition (if sufficient workspace available)
*
         IF( M.GE.MNTHR ) THEN
*
            IF( WNTUN ) THEN
*
*              Path 1 (M much larger than N, JOBU='N')
*              No left singular vectors to be computed
*
               ITAU = 1
               IWORK = ITAU + N
*
*              Compute A=Q*R
*              (CWorkspace: need 2*N, prefer N+N*NB)
*              (RWorkspace: need 0)
*
               CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
*
*              Zero out below R
*
               CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO, A( 2, 1 ),
     $                      LDA )
               IE = 1
               ITAUQ = 1
               ITAUP = ITAUQ + N
               IWORK = ITAUP + N
*
*              Bidiagonalize R in A
*              (CWorkspace: need 3*N, prefer 2*N+2*N*NB)
*              (RWorkspace: need N)
*
               CALL ZGEBRD( N, N, A, LDA, S, RWORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               NCVT = 0
               IF( WNTVO .OR. WNTVAS ) THEN
*
*                 If right singular vectors desired, generate P'.
*                 (CWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  NCVT = N
               END IF
               IRWORK = IE + N
*
*              Perform bidiagonal QR iteration, computing right
*              singular vectors of A in A if desired
*              (CWorkspace: 0)
*              (RWorkspace: need BDSPAC)
*
               CALL ZBDSQR( 'U', N, NCVT, 0, 0, S, RWORK( IE ), A, LDA,
     $                      CDUM, 1, CDUM, 1, RWORK( IRWORK ), INFO )
*
*              If right singular vectors desired in VT, copy them there
*
               IF( WNTVAS )
     $            CALL ZLACPY( 'F', N, N, A, LDA, VT, LDVT )
*
            ELSE IF( WNTUO .AND. WNTVN ) THEN
*
*              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
*              N left singular vectors to be overwritten on A and
*              no right singular vectors to be computed
*
               IF( LWORK.GE.N*N+3*N ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N )+LDA*N ) THEN
*
*                    WORK(IU) is LDA by N, WORK(IR) is LDA by N
*
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N )+N*N ) THEN
*
*                    WORK(IU) is LDA by N, WORK(IR) is N by N
*
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
*
*                    WORK(IU) is LDWRKU by N, WORK(IR) is N by N
*
                     LDWRKU = ( LWORK-N*N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to WORK(IR) and zero out below it
*
                  CALL ZLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
                  CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                         WORK( IR+1 ), LDWRKR )
*
*                 Generate Q in A
*                 (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = 1
                  ITAUQ = ITAU
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in WORK(IR)
*                 (CWorkspace: need N*N+3*N, prefer N*N+2*N+2*N*NB)
*                 (RWorkspace: need N)
*
                  CALL ZGEBRD( N, N, WORK( IR ), LDWRKR, S, RWORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing R
*                 (CWorkspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*                 (RWorkspace: need 0)
*
                  CALL ZUNGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IRWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of R in WORK(IR)
*                 (CWorkspace: need N*N)
*                 (RWorkspace: need BDSPAC)
*
                  CALL ZBDSQR( 'U', N, 0, N, 0, S, RWORK( IE ), CDUM, 1,
     $                         WORK( IR ), LDWRKR, CDUM, 1,
     $                         RWORK( IRWORK ), INFO )
                  IU = ITAUQ
*
*                 Multiply Q in A by left singular vectors of R in
*                 WORK(IR), storing result in WORK(IU) and copying to A
*                 (CWorkspace: need N*N+N, prefer N*N+M*N)
*                 (RWorkspace: 0)
*
                  DO 10 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL ZGEMM( 'N', 'N', CHUNK, N, N, CONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, CZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL ZLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   10             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  IE = 1
                  ITAUQ = 1
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize A
*                 (CWorkspace: need 2*N+M, prefer 2*N+(M+N)*NB)
*                 (RWorkspace: N)
*
                  CALL ZGEBRD( M, N, A, LDA, S, RWORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing A
*                 (CWorkspace: need 3*N, prefer 2*N+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IRWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in A
*                 (CWorkspace: need 0)
*                 (RWorkspace: need BDSPAC)
*
                  CALL ZBDSQR( 'U', N, 0, M, 0, S, RWORK( IE ), CDUM, 1,
     $                         A, LDA, CDUM, 1, RWORK( IRWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTUO .AND. WNTVAS ) THEN
*
*              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
*              N left singular vectors to be overwritten on A and
*              N right singular vectors to be computed in VT
*
               IF( LWORK.GE.N*N+3*N ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N )+LDA*N ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N )+N*N ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is N by N
*
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
*
*                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
*
                     LDWRKU = ( LWORK-N*N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to VT, zeroing out below it
*
                  CALL ZLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO, VT( 2, 1 ),
     $                         LDVT )
*
*                 Generate Q in A
*                 (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = 1
                  ITAUQ = ITAU
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in VT, copying result to WORK(IR)
*                 (CWorkspace: need N*N+3*N, prefer N*N+2*N+2*N*NB)
*                 (RWorkspace: need N)
*
                  CALL ZGEBRD( N, N, VT, LDVT, S, RWORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL ZLACPY( 'L', N, N, VT, LDVT, WORK( IR ), LDWRKR )
*
*                 Generate left vectors bidiagonalizing R in WORK(IR)
*                 (CWorkspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing R in VT
*                 (CWorkspace: need N*N+3*N-1, prefer N*N+2*N+(N-1)*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IRWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of R in WORK(IR) and computing right
*                 singular vectors of R in VT
*                 (CWorkspace: need N*N)
*                 (RWorkspace: need BDSPAC)
*
                  CALL ZBDSQR( 'U', N, N, N, 0, S, RWORK( IE ), VT,
     $                         LDVT, WORK( IR ), LDWRKR, CDUM, 1,
     $                         RWORK( IRWORK ), INFO )
                  IU = ITAUQ
*
*                 Multiply Q in A by left singular vectors of R in
*                 WORK(IR), storing result in WORK(IU) and copying to A
*                 (CWorkspace: need N*N+N, prefer N*N+M*N)
*                 (RWorkspace: 0)
*
                  DO 20 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL ZGEMM( 'N', 'N', CHUNK, N, N, CONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, CZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL ZLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   20             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  ITAU = 1
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (CWorkspace: need 2*N, prefer N+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to VT, zeroing out below it
*
                  CALL ZLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO, VT( 2, 1 ),
     $                         LDVT )
*
*                 Generate Q in A
*                 (CWorkspace: need 2*N, prefer N+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = 1
                  ITAUQ = ITAU
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in VT
*                 (CWorkspace: need 3*N, prefer 2*N+2*N*NB)
*                 (RWorkspace: N)
*
                  CALL ZGEBRD( N, N, VT, LDVT, S, RWORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Multiply Q in A by left vectors bidiagonalizing R
*                 (CWorkspace: need 2*N+M, prefer 2*N+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                         WORK( ITAUQ ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing R in VT
*                 (CWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IRWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in A and computing right
*                 singular vectors of A in VT
*                 (CWorkspace: 0)
*                 (RWorkspace: need BDSPAC)
*
                  CALL ZBDSQR( 'U', N, N, M, 0, S, RWORK( IE ), VT,
     $                         LDVT, A, LDA, CDUM, 1, RWORK( IRWORK ),
     $                         INFO )
*
               END IF
*
            ELSE IF( WNTUS ) THEN
*
               IF( WNTVN ) THEN
*
*                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
*                 N left singular vectors to be computed in U and
*                 no right singular vectors to be computed
*
                  IF( LWORK.GE.N*N+3*N ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IR) is LDA by N
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is N by N
*
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IR), zeroing out below it
*
                     CALL ZLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            WORK( IR+1 ), LDWRKR )
*
*                    Generate Q in A
*                    (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IR)
*                    (CWorkspace: need N*N+3*N, prefer N*N+2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left vectors bidiagonalizing R in WORK(IR)
*                    (CWorkspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IR)
*                    (CWorkspace: need N*N)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, 0, N, 0, S, RWORK( IE ), CDUM,
     $                            1, WORK( IR ), LDWRKR, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IR), storing result in U
*                    (CWorkspace: need N*N)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, N, CONE, A, LDA,
     $                           WORK( IR ), LDWRKR, CZERO, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            A( 2, 1 ), LDA )
*
*                    Bidiagonalize R in A
*                    (CWorkspace: need 3*N, prefer 2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, A, LDA, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left vectors bidiagonalizing R
*                    (CWorkspace: need 2*N+M, prefer 2*N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, 0, M, 0, S, RWORK( IE ), CDUM,
     $                            1, U, LDU, CDUM, 1, RWORK( IRWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVO ) THEN
*
*                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
*                 N left singular vectors to be computed in U and
*                 N right singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*N*N+3*N ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is N by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
*
*                       WORK(IU) is N by N and WORK(IR) is N by N
*
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (CWorkspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL ZLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            WORK( IU+1 ), LDWRKU )
*
*                    Generate Q in A
*                    (CWorkspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to
*                    WORK(IR)
*                    (CWorkspace: need   2*N*N+3*N,
*                                 prefer 2*N*N+2*N+2*N*NB)
*                    (RWorkspace: need   N)
*
                     CALL ZGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (CWorkspace: need 2*N*N+3*N, prefer 2*N*N+2*N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (CWorkspace: need   2*N*N+3*N-1,
*                                 prefer 2*N*N+2*N+(N-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in WORK(IR)
*                    (CWorkspace: need 2*N*N)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, N, N, 0, S, RWORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, CDUM, 1, RWORK( IRWORK ),
     $                            INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IU), storing result in U
*                    (CWorkspace: need N*N)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, N, CONE, A, LDA,
     $                           WORK( IU ), LDWRKU, CZERO, U, LDU )
*
*                    Copy right singular vectors of R to A
*                    (CWorkspace: need N*N)
*                    (RWorkspace: 0)
*
                     CALL ZLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            A( 2, 1 ), LDA )
*
*                    Bidiagonalize R in A
*                    (CWorkspace: need 3*N, prefer 2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, A, LDA, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left vectors bidiagonalizing R
*                    (CWorkspace: need 2*N+M, prefer 2*N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right vectors bidiagonalizing R in A
*                    (CWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in A
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, N, M, 0, S, RWORK( IE ), A,
     $                            LDA, U, LDU, CDUM, 1, RWORK( IRWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVAS ) THEN
*
*                 Path 6 (M much larger than N, JOBU='S', JOBVT='S'
*                         or 'A')
*                 N left singular vectors to be computed in U and
*                 N right singular vectors to be computed in VT
*
                  IF( LWORK.GE.N*N+3*N ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is N by N
*
                        LDWRKU = N
                     END IF
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R
*                    (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL ZLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            WORK( IU+1 ), LDWRKU )
*
*                    Generate Q in A
*                    (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to VT
*                    (CWorkspace: need N*N+3*N, prefer N*N+2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (CWorkspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (CWorkspace: need   N*N+3*N-1,
*                                 prefer N*N+2*N+(N-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in VT
*                    (CWorkspace: need N*N)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, N, N, 0, S, RWORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IU), storing result in U
*                    (CWorkspace: need N*N)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, N, CONE, A, LDA,
     $                           WORK( IU ), LDWRKU, CZERO, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to VT, zeroing out below it
*
                     CALL ZLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            VT( 2, 1 ), LDVT )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in VT
*                    (CWorkspace: need 3*N, prefer 2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, VT, LDVT, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in VT
*                    (CWorkspace: need 2*N+M, prefer 2*N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (CWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, N, M, 0, S, RWORK( IE ), VT,
     $                            LDVT, U, LDU, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
                  END IF
*
               END IF
*
            ELSE IF( WNTUA ) THEN
*
               IF( WNTVN ) THEN
*
*                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
*                 M left singular vectors to be computed in U and
*                 no right singular vectors to be computed
*
                  IF( LWORK.GE.N*N+MAX( N+M, 3*N ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IR) is LDA by N
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is N by N
*
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Copy R to WORK(IR), zeroing out below it
*
                     CALL ZLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            WORK( IR+1 ), LDWRKR )
*
*                    Generate Q in U
*                    (CWorkspace: need N*N+N+M, prefer N*N+N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IR)
*                    (CWorkspace: need N*N+3*N, prefer N*N+2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (CWorkspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IR)
*                    (CWorkspace: need N*N)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, 0, N, 0, S, RWORK( IE ), CDUM,
     $                            1, WORK( IR ), LDWRKR, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IR), storing result in A
*                    (CWorkspace: need N*N)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, N, CONE, U, LDU,
     $                           WORK( IR ), LDWRKR, CZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL ZLACPY( 'F', M, N, A, LDA, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (CWorkspace: need N+M, prefer N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            A( 2, 1 ), LDA )
*
*                    Bidiagonalize R in A
*                    (CWorkspace: need 3*N, prefer 2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, A, LDA, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in A
*                    (CWorkspace: need 2*N+M, prefer 2*N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, 0, M, 0, S, RWORK( IE ), CDUM,
     $                            1, U, LDU, CDUM, 1, RWORK( IRWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVO ) THEN
*
*                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
*                 M left singular vectors to be computed in U and
*                 N right singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*N*N+MAX( N+M, 3*N ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+N )*N ) THEN
*
*                       WORK(IU) is LDA by N and WORK(IR) is N by N
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     ELSE
*
*                       WORK(IU) is N by N and WORK(IR) is N by N
*
                        LDWRKU = N
                        IR = IU + LDWRKU*N
                        LDWRKR = N
                     END IF
                     ITAU = IR + LDWRKR*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (CWorkspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL ZLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to
*                    WORK(IR)
*                    (CWorkspace: need   2*N*N+3*N,
*                                 prefer 2*N*N+2*N+2*N*NB)
*                    (RWorkspace: need   N)
*
                     CALL ZGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (CWorkspace: need 2*N*N+3*N, prefer 2*N*N+2*N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (CWorkspace: need   2*N*N+3*N-1,
*                                 prefer 2*N*N+2*N+(N-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in WORK(IR)
*                    (CWorkspace: need 2*N*N)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, N, N, 0, S, RWORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, CDUM, 1, RWORK( IRWORK ),
     $                            INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IU), storing result in A
*                    (CWorkspace: need N*N)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, N, CONE, U, LDU,
     $                           WORK( IU ), LDWRKU, CZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL ZLACPY( 'F', M, N, A, LDA, U, LDU )
*
*                    Copy right singular vectors of R from WORK(IR) to A
*
                     CALL ZLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (CWorkspace: need N+M, prefer N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            A( 2, 1 ), LDA )
*
*                    Bidiagonalize R in A
*                    (CWorkspace: need 3*N, prefer 2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, A, LDA, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in A
*                    (CWorkspace: need 2*N+M, prefer 2*N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in A
*                    (CWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in A
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, N, M, 0, S, RWORK( IE ), A,
     $                            LDA, U, LDU, CDUM, 1, RWORK( IRWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTVAS ) THEN
*
*                 Path 9 (M much larger than N, JOBU='A', JOBVT='S'
*                         or 'A')
*                 M left singular vectors to be computed in U and
*                 N right singular vectors to be computed in VT
*
                  IF( LWORK.GE.N*N+MAX( N+M, 3*N ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*N ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is N by N
*
                        LDWRKU = N
                     END IF
                     ITAU = IU + LDWRKU*N
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need N*N+2*N, prefer N*N+N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (CWorkspace: need N*N+N+M, prefer N*N+N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL ZLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to VT
*                    (CWorkspace: need N*N+3*N, prefer N*N+2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (CWorkspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (CWorkspace: need   N*N+3*N-1,
*                                 prefer N*N+2*N+(N-1)*NB)
*                    (RWorkspace: need   0)
*
                     CALL ZUNGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in VT
*                    (CWorkspace: need N*N)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, N, N, 0, S, RWORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IU), storing result in A
*                    (CWorkspace: need N*N)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, N, CONE, U, LDU,
     $                           WORK( IU ), LDWRKU, CZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL ZLACPY( 'F', M, N, A, LDA, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (CWorkspace: need 2*N, prefer N+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (CWorkspace: need N+M, prefer N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R from A to VT, zeroing out below it
*
                     CALL ZLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     CALL ZLASET( 'L', N-1, N-1, CZERO, CZERO,
     $                            VT( 2, 1 ), LDVT )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in VT
*                    (CWorkspace: need 3*N, prefer 2*N+2*N*NB)
*                    (RWorkspace: need N)
*
                     CALL ZGEBRD( N, N, VT, LDVT, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in VT
*                    (CWorkspace: need 2*N+M, prefer 2*N+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (CWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', N, N, M, 0, S, RWORK( IE ), VT,
     $                            LDVT, U, LDU, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
                  END IF
*
               END IF
*
            END IF
*
         ELSE
*
*           M .LT. MNTHR
*
*           Path 10 (M at least N, but not much larger)
*           Reduce to bidiagonal form without QR decomposition
*
            IE = 1
            ITAUQ = 1
            ITAUP = ITAUQ + N
            IWORK = ITAUP + N
*
*           Bidiagonalize A
*           (CWorkspace: need 2*N+M, prefer 2*N+(M+N)*NB)
*           (RWorkspace: need N)
*
            CALL ZGEBRD( M, N, A, LDA, S, RWORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
*
*              If left singular vectors desired in U, copy result to U
*              and generate left bidiagonalizing vectors in U
*              (CWorkspace: need 2*N+NCU, prefer 2*N+NCU*NB)
*              (RWorkspace: 0)
*
               CALL ZLACPY( 'L', M, N, A, LDA, U, LDU )
               IF( WNTUS )
     $            NCU = N
               IF( WNTUA )
     $            NCU = M
               CALL ZUNGBR( 'Q', M, NCU, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
*
*              If right singular vectors desired in VT, copy result to
*              VT and generate right bidiagonalizing vectors in VT
*              (CWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*              (RWorkspace: 0)
*
               CALL ZLACPY( 'U', N, N, A, LDA, VT, LDVT )
               CALL ZUNGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
*
*              If left singular vectors desired in A, generate left
*              bidiagonalizing vectors in A
*              (CWorkspace: need 3*N, prefer 2*N+N*NB)
*              (RWorkspace: 0)
*
               CALL ZUNGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
*
*              If right singular vectors desired in A, generate right
*              bidiagonalizing vectors in A
*              (CWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*              (RWorkspace: 0)
*
               CALL ZUNGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IRWORK = IE + N
            IF( WNTUAS .OR. WNTUO )
     $         NRU = M
            IF( WNTUN )
     $         NRU = 0
            IF( WNTVAS .OR. WNTVO )
     $         NCVT = N
            IF( WNTVN )
     $         NCVT = 0
            IF( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in VT
*              (CWorkspace: 0)
*              (RWorkspace: need BDSPAC)
*
               CALL ZBDSQR( 'U', N, NCVT, NRU, 0, S, RWORK( IE ), VT,
     $                      LDVT, U, LDU, CDUM, 1, RWORK( IRWORK ),
     $                      INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in A
*              (CWorkspace: 0)
*              (RWorkspace: need BDSPAC)
*
               CALL ZBDSQR( 'U', N, NCVT, NRU, 0, S, RWORK( IE ), A,
     $                      LDA, U, LDU, CDUM, 1, RWORK( IRWORK ),
     $                      INFO )
            ELSE
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in A and computing right singular
*              vectors in VT
*              (CWorkspace: 0)
*              (RWorkspace: need BDSPAC)
*
               CALL ZBDSQR( 'U', N, NCVT, NRU, 0, S, RWORK( IE ), VT,
     $                      LDVT, A, LDA, CDUM, 1, RWORK( IRWORK ),
     $                      INFO )
            END IF
*
         END IF
*
      ELSE
*
*        A has more columns than rows. If A has sufficiently more
*        columns than rows, first reduce using the LQ decomposition (if
*        sufficient workspace available)
*
         IF( N.GE.MNTHR ) THEN
*
            IF( WNTVN ) THEN
*
*              Path 1t(N much larger than M, JOBVT='N')
*              No right singular vectors to be computed
*
               ITAU = 1
               IWORK = ITAU + M
*
*              Compute A=L*Q
*              (CWorkspace: need 2*M, prefer M+M*NB)
*              (RWorkspace: 0)
*
               CALL ZGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
*
*              Zero out above L
*
               CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO, A( 1, 2 ),
     $                      LDA )
               IE = 1
               ITAUQ = 1
               ITAUP = ITAUQ + M
               IWORK = ITAUP + M
*
*              Bidiagonalize L in A
*              (CWorkspace: need 3*M, prefer 2*M+2*M*NB)
*              (RWorkspace: need M)
*
               CALL ZGEBRD( M, M, A, LDA, S, RWORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               IF( WNTUO .OR. WNTUAS ) THEN
*
*                 If left singular vectors desired, generate Q
*                 (CWorkspace: need 3*M, prefer 2*M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
               END IF
               IRWORK = IE + M
               NRU = 0
               IF( WNTUO .OR. WNTUAS )
     $            NRU = M
*
*              Perform bidiagonal QR iteration, computing left singular
*              vectors of A in A if desired
*              (CWorkspace: 0)
*              (RWorkspace: need BDSPAC)
*
               CALL ZBDSQR( 'U', M, 0, NRU, 0, S, RWORK( IE ), CDUM, 1,
     $                      A, LDA, CDUM, 1, RWORK( IRWORK ), INFO )
*
*              If left singular vectors desired in U, copy them there
*
               IF( WNTUAS )
     $            CALL ZLACPY( 'F', M, M, A, LDA, U, LDU )
*
            ELSE IF( WNTVO .AND. WNTUN ) THEN
*
*              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
*              M right singular vectors to be overwritten on A and
*              no left singular vectors to be computed
*
               IF( LWORK.GE.M*M+3*M ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N )+LDA*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N )+M*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is M by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
*
*                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
*
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to WORK(IR) and zero out above it
*
                  CALL ZLACPY( 'L', M, M, A, LDA, WORK( IR ), LDWRKR )
                  CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                         WORK( IR+LDWRKR ), LDWRKR )
*
*                 Generate Q in A
*                 (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = 1
                  ITAUQ = ITAU
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in WORK(IR)
*                 (CWorkspace: need M*M+3*M, prefer M*M+2*M+2*M*NB)
*                 (RWorkspace: need M)
*
                  CALL ZGEBRD( M, M, WORK( IR ), LDWRKR, S, RWORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing L
*                 (CWorkspace: need M*M+3*M-1, prefer M*M+2*M+(M-1)*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IRWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing right
*                 singular vectors of L in WORK(IR)
*                 (CWorkspace: need M*M)
*                 (RWorkspace: need BDSPAC)
*
                  CALL ZBDSQR( 'U', M, M, 0, 0, S, RWORK( IE ),
     $                         WORK( IR ), LDWRKR, CDUM, 1, CDUM, 1,
     $                         RWORK( IRWORK ), INFO )
                  IU = ITAUQ
*
*                 Multiply right singular vectors of L in WORK(IR) by Q
*                 in A, storing result in WORK(IU) and copying to A
*                 (CWorkspace: need M*M+M, prefer M*M+M*N)
*                 (RWorkspace: 0)
*
                  DO 30 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL ZGEMM( 'N', 'N', M, BLK, M, CONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, CZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL ZLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   30             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  IE = 1
                  ITAUQ = 1
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize A
*                 (CWorkspace: need 2*M+N, prefer 2*M+(M+N)*NB)
*                 (RWorkspace: need M)
*
                  CALL ZGEBRD( M, N, A, LDA, S, RWORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing A
*                 (CWorkspace: need 3*M, prefer 2*M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IRWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing right
*                 singular vectors of A in A
*                 (CWorkspace: 0)
*                 (RWorkspace: need BDSPAC)
*
                  CALL ZBDSQR( 'L', M, N, 0, 0, S, RWORK( IE ), A, LDA,
     $                         CDUM, 1, CDUM, 1, RWORK( IRWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTVO .AND. WNTUAS ) THEN
*
*              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
*              M right singular vectors to be overwritten on A and
*              M left singular vectors to be computed in U
*
               IF( LWORK.GE.M*M+3*M ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N )+LDA*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N )+M*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is M by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = M
                  ELSE
*
*                    WORK(IU) is M by CHUNK and WORK(IR) is M by M
*
                     LDWRKU = M
                     CHUNK = ( LWORK-M*M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to U, zeroing about above it
*
                  CALL ZLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO, U( 1, 2 ),
     $                         LDU )
*
*                 Generate Q in A
*                 (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = 1
                  ITAUQ = ITAU
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in U, copying result to WORK(IR)
*                 (CWorkspace: need M*M+3*M, prefer M*M+2*M+2*M*NB)
*                 (RWorkspace: need M)
*
                  CALL ZGEBRD( M, M, U, LDU, S, RWORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL ZLACPY( 'U', M, M, U, LDU, WORK( IR ), LDWRKR )
*
*                 Generate right vectors bidiagonalizing L in WORK(IR)
*                 (CWorkspace: need M*M+3*M-1, prefer M*M+2*M+(M-1)*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing L in U
*                 (CWorkspace: need M*M+3*M, prefer M*M+2*M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IRWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of L in U, and computing right
*                 singular vectors of L in WORK(IR)
*                 (CWorkspace: need M*M)
*                 (RWorkspace: need BDSPAC)
*
                  CALL ZBDSQR( 'U', M, M, M, 0, S, RWORK( IE ),
     $                         WORK( IR ), LDWRKR, U, LDU, CDUM, 1,
     $                         RWORK( IRWORK ), INFO )
                  IU = ITAUQ
*
*                 Multiply right singular vectors of L in WORK(IR) by Q
*                 in A, storing result in WORK(IU) and copying to A
*                 (CWorkspace: need M*M+M, prefer M*M+M*N))
*                 (RWorkspace: 0)
*
                  DO 40 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL ZGEMM( 'N', 'N', M, BLK, M, CONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, CZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL ZLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   40             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  ITAU = 1
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (CWorkspace: need 2*M, prefer M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to U, zeroing out above it
*
                  CALL ZLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO, U( 1, 2 ),
     $                         LDU )
*
*                 Generate Q in A
*                 (CWorkspace: need 2*M, prefer M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = 1
                  ITAUQ = ITAU
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in U
*                 (CWorkspace: need 3*M, prefer 2*M+2*M*NB)
*                 (RWorkspace: need M)
*
                  CALL ZGEBRD( M, M, U, LDU, S, RWORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Multiply right vectors bidiagonalizing L by Q in A
*                 (CWorkspace: need 2*M+N, prefer 2*M+N*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNMBR( 'P', 'L', 'C', M, N, M, U, LDU,
     $                         WORK( ITAUP ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing L in U
*                 (CWorkspace: need 3*M, prefer 2*M+M*NB)
*                 (RWorkspace: 0)
*
                  CALL ZUNGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IRWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in U and computing right
*                 singular vectors of A in A
*                 (CWorkspace: 0)
*                 (RWorkspace: need BDSPAC)
*
                  CALL ZBDSQR( 'U', M, N, M, 0, S, RWORK( IE ), A, LDA,
     $                         U, LDU, CDUM, 1, RWORK( IRWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTVS ) THEN
*
               IF( WNTUN ) THEN
*
*                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 no left singular vectors to be computed
*
                  IF( LWORK.GE.M*M+3*M ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IR) is LDA by M
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is M by M
*
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IR), zeroing out above it
*
                     CALL ZLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
*
*                    Generate Q in A
*                    (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IR)
*                    (CWorkspace: need M*M+3*M, prefer M*M+2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right vectors bidiagonalizing L in
*                    WORK(IR)
*                    (CWorkspace: need M*M+3*M, prefer M*M+2*M+(M-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of L in WORK(IR)
*                    (CWorkspace: need M*M)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, M, 0, 0, S, RWORK( IE ),
     $                            WORK( IR ), LDWRKR, CDUM, 1, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IR) by
*                    Q in A, storing result in VT
*                    (CWorkspace: need M*M)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, M, CONE, WORK( IR ),
     $                           LDWRKR, A, LDA, CZERO, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy result to VT
*
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            A( 1, 2 ), LDA )
*
*                    Bidiagonalize L in A
*                    (CWorkspace: need 3*M, prefer 2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, A, LDA, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right vectors bidiagonalizing L by Q in VT
*                    (CWorkspace: need 2*M+N, prefer 2*M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'P', 'L', 'C', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of A in VT
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, N, 0, 0, S, RWORK( IE ), VT,
     $                            LDVT, CDUM, 1, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
                  END IF
*
               ELSE IF( WNTUO ) THEN
*
*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 M left singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*M*M+3*M ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is M by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
*
*                       WORK(IU) is M by M and WORK(IR) is M by M
*
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (CWorkspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out below it
*
                     CALL ZLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
*
*                    Generate Q in A
*                    (CWorkspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to
*                    WORK(IR)
*                    (CWorkspace: need   2*M*M+3*M,
*                                 prefer 2*M*M+2*M+2*M*NB)
*                    (RWorkspace: need   M)
*
                     CALL ZGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (CWorkspace: need   2*M*M+3*M-1,
*                                 prefer 2*M*M+2*M+(M-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (CWorkspace: need 2*M*M+3*M, prefer 2*M*M+2*M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in WORK(IR) and computing
*                    right singular vectors of L in WORK(IU)
*                    (CWorkspace: need 2*M*M)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, M, M, 0, S, RWORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, CDUM, 1, RWORK( IRWORK ),
     $                            INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in A, storing result in VT
*                    (CWorkspace: need M*M)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, M, CONE, WORK( IU ),
     $                           LDWRKU, A, LDA, CZERO, VT, LDVT )
*
*                    Copy left singular vectors of L to A
*                    (CWorkspace: need M*M)
*                    (RWorkspace: 0)
*
                     CALL ZLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            A( 1, 2 ), LDA )
*
*                    Bidiagonalize L in A
*                    (CWorkspace: need 3*M, prefer 2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, A, LDA, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right vectors bidiagonalizing L by Q in VT
*                    (CWorkspace: need 2*M+N, prefer 2*M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'P', 'L', 'C', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors of L in A
*                    (CWorkspace: need 3*M, prefer 2*M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in A and computing right
*                    singular vectors of A in VT
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, N, M, 0, S, RWORK( IE ), VT,
     $                            LDVT, A, LDA, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
                  END IF
*
               ELSE IF( WNTUAS ) THEN
*
*                 Path 6t(N much larger than M, JOBU='S' or 'A',
*                         JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 M left singular vectors to be computed in U
*
                  IF( LWORK.GE.M*M+3*M ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IU) is LDA by N
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is LDA by M
*
                        LDWRKU = M
                     END IF
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL ZLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
*
*                    Generate Q in A
*                    (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to U
*                    (CWorkspace: need M*M+3*M, prefer M*M+2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (CWorkspace: need   M*M+3*M-1,
*                                 prefer M*M+2*M+(M-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (CWorkspace: need M*M+3*M, prefer M*M+2*M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in U and computing right
*                    singular vectors of L in WORK(IU)
*                    (CWorkspace: need M*M)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, M, M, 0, S, RWORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in A, storing result in VT
*                    (CWorkspace: need M*M)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, M, CONE, WORK( IU ),
     $                           LDWRKU, A, LDA, CZERO, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to U, zeroing out above it
*
                     CALL ZLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            U( 1, 2 ), LDU )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in U
*                    (CWorkspace: need 3*M, prefer 2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, U, LDU, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in U by Q
*                    in VT
*                    (CWorkspace: need 2*M+N, prefer 2*M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'P', 'L', 'C', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (CWorkspace: need 3*M, prefer 2*M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, N, M, 0, S, RWORK( IE ), VT,
     $                            LDVT, U, LDU, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
                  END IF
*
               END IF
*
            ELSE IF( WNTVA ) THEN
*
               IF( WNTUN ) THEN
*
*                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 no left singular vectors to be computed
*
                  IF( LWORK.GE.M*M+MAX( N+M, 3*M ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IR = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IR) is LDA by M
*
                        LDWRKR = LDA
                     ELSE
*
*                       WORK(IR) is M by M
*
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Copy L to WORK(IR), zeroing out above it
*
                     CALL ZLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
*
*                    Generate Q in VT
*                    (CWorkspace: need M*M+M+N, prefer M*M+M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IR)
*                    (CWorkspace: need M*M+3*M, prefer M*M+2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (CWorkspace: need   M*M+3*M-1,
*                                 prefer M*M+2*M+(M-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of L in WORK(IR)
*                    (CWorkspace: need M*M)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, M, 0, 0, S, RWORK( IE ),
     $                            WORK( IR ), LDWRKR, CDUM, 1, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IR) by
*                    Q in VT, storing result in A
*                    (CWorkspace: need M*M)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, M, CONE, WORK( IR ),
     $                           LDWRKR, VT, LDVT, CZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL ZLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (CWorkspace: need M+N, prefer M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            A( 1, 2 ), LDA )
*
*                    Bidiagonalize L in A
*                    (CWorkspace: need 3*M, prefer 2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, A, LDA, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in A by Q
*                    in VT
*                    (CWorkspace: need 2*M+N, prefer 2*M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'P', 'L', 'C', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of A in VT
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, N, 0, 0, S, RWORK( IE ), VT,
     $                            LDVT, CDUM, 1, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
                  END IF
*
               ELSE IF( WNTUO ) THEN
*
*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 M left singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*M*M+MAX( N+M, 3*M ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+2*LDA*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is LDA by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = LDA
                     ELSE IF( LWORK.GE.WRKBL+( LDA+M )*M ) THEN
*
*                       WORK(IU) is LDA by M and WORK(IR) is M by M
*
                        LDWRKU = LDA
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     ELSE
*
*                       WORK(IU) is M by M and WORK(IR) is M by M
*
                        LDWRKU = M
                        IR = IU + LDWRKU*M
                        LDWRKR = M
                     END IF
                     ITAU = IR + LDWRKR*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (CWorkspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (CWorkspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL ZLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to
*                    WORK(IR)
*                    (CWorkspace: need   2*M*M+3*M,
*                                 prefer 2*M*M+2*M+2*M*NB)
*                    (RWorkspace: need   M)
*
                     CALL ZGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (CWorkspace: need   2*M*M+3*M-1,
*                                 prefer 2*M*M+2*M+(M-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (CWorkspace: need 2*M*M+3*M, prefer 2*M*M+2*M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in WORK(IR) and computing
*                    right singular vectors of L in WORK(IU)
*                    (CWorkspace: need 2*M*M)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, M, M, 0, S, RWORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, CDUM, 1, RWORK( IRWORK ),
     $                            INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in VT, storing result in A
*                    (CWorkspace: need M*M)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, M, CONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, CZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL ZLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
*                    Copy left singular vectors of A from WORK(IR) to A
*
                     CALL ZLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
     $                            LDA )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (CWorkspace: need M+N, prefer M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            A( 1, 2 ), LDA )
*
*                    Bidiagonalize L in A
*                    (CWorkspace: need 3*M, prefer 2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, A, LDA, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in A by Q
*                    in VT
*                    (CWorkspace: need 2*M+N, prefer 2*M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'P', 'L', 'C', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in A
*                    (CWorkspace: need 3*M, prefer 2*M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in A and computing right
*                    singular vectors of A in VT
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, N, M, 0, S, RWORK( IE ), VT,
     $                            LDVT, A, LDA, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
                  END IF
*
               ELSE IF( WNTUAS ) THEN
*
*                 Path 9t(N much larger than M, JOBU='S' or 'A',
*                         JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 M left singular vectors to be computed in U
*
                  IF( LWORK.GE.M*M+MAX( N+M, 3*M ) ) THEN
*
*                    Sufficient workspace for a fast algorithm
*
                     IU = 1
                     IF( LWORK.GE.WRKBL+LDA*M ) THEN
*
*                       WORK(IU) is LDA by M
*
                        LDWRKU = LDA
                     ELSE
*
*                       WORK(IU) is M by M
*
                        LDWRKU = M
                     END IF
                     ITAU = IU + LDWRKU*M
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (CWorkspace: need M*M+2*M, prefer M*M+M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (CWorkspace: need M*M+M+N, prefer M*M+M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL ZLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to U
*                    (CWorkspace: need M*M+3*M, prefer M*M+2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            RWORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (CWorkspace: need M*M+3*M, prefer M*M+2*M+(M-1)*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (CWorkspace: need M*M+3*M, prefer M*M+2*M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in U and computing right
*                    singular vectors of L in WORK(IU)
*                    (CWorkspace: need M*M)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, M, M, 0, S, RWORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in VT, storing result in A
*                    (CWorkspace: need M*M)
*                    (RWorkspace: 0)
*
                     CALL ZGEMM( 'N', 'N', M, N, M, CONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, CZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL ZLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (CWorkspace: need 2*M, prefer M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (CWorkspace: need M+N, prefer M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to U, zeroing out above it
*
                     CALL ZLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL ZLASET( 'U', M-1, M-1, CZERO, CZERO,
     $                            U( 1, 2 ), LDU )
                     IE = 1
                     ITAUQ = ITAU
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in U
*                    (CWorkspace: need 3*M, prefer 2*M+2*M*NB)
*                    (RWorkspace: need M)
*
                     CALL ZGEBRD( M, M, U, LDU, S, RWORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in U by Q
*                    in VT
*                    (CWorkspace: need 2*M+N, prefer 2*M+N*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNMBR( 'P', 'L', 'C', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (CWorkspace: need 3*M, prefer 2*M+M*NB)
*                    (RWorkspace: 0)
*
                     CALL ZUNGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IRWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (CWorkspace: 0)
*                    (RWorkspace: need BDSPAC)
*
                     CALL ZBDSQR( 'U', M, N, M, 0, S, RWORK( IE ), VT,
     $                            LDVT, U, LDU, CDUM, 1,
     $                            RWORK( IRWORK ), INFO )
*
                  END IF
*
               END IF
*
            END IF
*
         ELSE
*
*           N .LT. MNTHR
*
*           Path 10t(N greater than M, but not much larger)
*           Reduce to bidiagonal form without LQ decomposition
*
            IE = 1
            ITAUQ = 1
            ITAUP = ITAUQ + M
            IWORK = ITAUP + M
*
*           Bidiagonalize A
*           (CWorkspace: need 2*M+N, prefer 2*M+(M+N)*NB)
*           (RWorkspace: M)
*
            CALL ZGEBRD( M, N, A, LDA, S, RWORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
*
*              If left singular vectors desired in U, copy result to U
*              and generate left bidiagonalizing vectors in U
*              (CWorkspace: need 3*M-1, prefer 2*M+(M-1)*NB)
*              (RWorkspace: 0)
*
               CALL ZLACPY( 'L', M, M, A, LDA, U, LDU )
               CALL ZUNGBR( 'Q', M, M, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
*
*              If right singular vectors desired in VT, copy result to
*              VT and generate right bidiagonalizing vectors in VT
*              (CWorkspace: need 2*M+NRVT, prefer 2*M+NRVT*NB)
*              (RWorkspace: 0)
*
               CALL ZLACPY( 'U', M, N, A, LDA, VT, LDVT )
               IF( WNTVA )
     $            NRVT = N
               IF( WNTVS )
     $            NRVT = M
               CALL ZUNGBR( 'P', NRVT, N, M, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
*
*              If left singular vectors desired in A, generate left
*              bidiagonalizing vectors in A
*              (CWorkspace: need 3*M-1, prefer 2*M+(M-1)*NB)
*              (RWorkspace: 0)
*
               CALL ZUNGBR( 'Q', M, M, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
*
*              If right singular vectors desired in A, generate right
*              bidiagonalizing vectors in A
*              (CWorkspace: need 3*M, prefer 2*M+M*NB)
*              (RWorkspace: 0)
*
               CALL ZUNGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IRWORK = IE + M
            IF( WNTUAS .OR. WNTUO )
     $         NRU = M
            IF( WNTUN )
     $         NRU = 0
            IF( WNTVAS .OR. WNTVO )
     $         NCVT = N
            IF( WNTVN )
     $         NCVT = 0
            IF( ( .NOT.WNTUO ) .AND. ( .NOT.WNTVO ) ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in VT
*              (CWorkspace: 0)
*              (RWorkspace: need BDSPAC)
*
               CALL ZBDSQR( 'L', M, NCVT, NRU, 0, S, RWORK( IE ), VT,
     $                      LDVT, U, LDU, CDUM, 1, RWORK( IRWORK ),
     $                      INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in A
*              (CWorkspace: 0)
*              (RWorkspace: need BDSPAC)
*
               CALL ZBDSQR( 'L', M, NCVT, NRU, 0, S, RWORK( IE ), A,
     $                      LDA, U, LDU, CDUM, 1, RWORK( IRWORK ),
     $                      INFO )
            ELSE
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in A and computing right singular
*              vectors in VT
*              (CWorkspace: 0)
*              (RWorkspace: need BDSPAC)
*
               CALL ZBDSQR( 'L', M, NCVT, NRU, 0, S, RWORK( IE ), VT,
     $                      LDVT, A, LDA, CDUM, 1, RWORK( IRWORK ),
     $                      INFO )
            END IF
*
         END IF
*
      END IF
*
*     Undo scaling if necessary
*
      IF( ISCL.EQ.1 ) THEN
         IF( ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1,
     $                   RWORK( IE ), MINMN, IERR )
         IF( ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1,
     $                   RWORK( IE ), MINMN, IERR )
      END IF
*
*     Return optimal workspace in WORK(1)
*
      WORK( 1 ) = MAXWRK
*
      RETURN
*
*     End of ZGESVD
*
      END
      SUBROUTINE ZGETF2( M, N, A, LDA, IPIV, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZGETF2 computes an LU factorization of a general m-by-n matrix A
*  using partial pivoting with row interchanges.
*
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*
*  This is the right-looking Level 2 BLAS version of the algorithm.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the m by n matrix to be factored.
*          On exit, the factors L and U from the factorization
*          A = P*L*U; the unit diagonal elements of L are not stored.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  IPIV    (output) INTEGER array, dimension (min(M,N))
*          The pivot indices; for 1 <= i <= min(M,N), row i of the
*          matrix was interchanged with row IPIV(i).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
*               has been completed, but the factor U is exactly
*               singular, and division by zero will occur if it is used
*               to solve a system of equations.
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            J, JP
*     ..
*     .. External Functions ..
      INTEGER            IZAMAX
      EXTERNAL           IZAMAX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGERU, ZSCAL, ZSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGETF2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
      DO 10 J = 1, MIN( M, N )
*
*        Find pivot and test for singularity.
*
         JP = J - 1 + IZAMAX( M-J+1, A( J, J ), 1 )
         IPIV( J ) = JP
         IF( A( JP, J ).NE.ZERO ) THEN
*
*           Apply the interchange to columns 1:N.
*
            IF( JP.NE.J )
     $         CALL ZSWAP( N, A( J, 1 ), LDA, A( JP, 1 ), LDA )
*
*           Compute elements J+1:M of J-th column.
*
            IF( J.LT.M )
     $         CALL ZSCAL( M-J, ONE / A( J, J ), A( J+1, J ), 1 )
*
         ELSE IF( INFO.EQ.0 ) THEN
*
            INFO = J
         END IF
*
         IF( J.LT.MIN( M, N ) ) THEN
*
*           Update trailing submatrix.
*
            CALL ZGERU( M-J, N-J, -ONE, A( J+1, J ), 1, A( J, J+1 ),
     $                  LDA, A( J+1, J+1 ), LDA )
         END IF
   10 CONTINUE
      RETURN
*
*     End of ZGETF2
*
      END
      SUBROUTINE ZGETRF( M, N, A, LDA, IPIV, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZGETRF computes an LU factorization of a general M-by-N matrix A
*  using partial pivoting with row interchanges.
*
*  The factorization has the form
*     A = P * L * U
*  where P is a permutation matrix, L is lower triangular with unit
*  diagonal elements (lower trapezoidal if m > n), and U is upper
*  triangular (upper trapezoidal if m < n).
*
*  This is the right-looking Level 3 BLAS version of the algorithm.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the M-by-N matrix to be factored.
*          On exit, the factors L and U from the factorization
*          A = P*L*U; the unit diagonal elements of L are not stored.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  IPIV    (output) INTEGER array, dimension (min(M,N))
*          The pivot indices; for 1 <= i <= min(M,N), row i of the
*          matrix was interchanged with row IPIV(i).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
*                has been completed, but the factor U is exactly
*                singular, and division by zero will occur if it is used
*                to solve a system of equations.
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IINFO, J, JB, NB
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZGEMM, ZGETF2, ZLASWP, ZTRSM
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGETRF', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
*     Determine the block size for this environment.
*
      NB = ILAENV( 1, 'ZGETRF', ' ', M, N, -1, -1 )
      IF( NB.LE.1 .OR. NB.GE.MIN( M, N ) ) THEN
*
*        Use unblocked code.
*
         CALL ZGETF2( M, N, A, LDA, IPIV, INFO )
      ELSE
*
*        Use blocked code.
*
         DO 20 J = 1, MIN( M, N ), NB
            JB = MIN( MIN( M, N )-J+1, NB )
*
*           Factor diagonal and subdiagonal blocks and test for exact
*           singularity.
*
            CALL ZGETF2( M-J+1, JB, A( J, J ), LDA, IPIV( J ), IINFO )
*
*           Adjust INFO and the pivot indices.
*
            IF( INFO.EQ.0 .AND. IINFO.GT.0 )
     $         INFO = IINFO + J - 1
            DO 10 I = J, MIN( M, J+JB-1 )
               IPIV( I ) = J - 1 + IPIV( I )
   10       CONTINUE
*
*           Apply interchanges to columns 1:J-1.
*
            CALL ZLASWP( J-1, A, LDA, J, J+JB-1, IPIV, 1 )
*
            IF( J+JB.LE.N ) THEN
*
*              Apply interchanges to columns J+JB:N.
*
               CALL ZLASWP( N-J-JB+1, A( 1, J+JB ), LDA, J, J+JB-1,
     $                      IPIV, 1 )
*
*              Compute block row of U.
*
               CALL ZTRSM( 'Left', 'Lower', 'No transpose', 'Unit', JB,
     $                     N-J-JB+1, ONE, A( J, J ), LDA, A( J, J+JB ),
     $                     LDA )
               IF( J+JB.LE.M ) THEN
*
*                 Update trailing submatrix.
*
                  CALL ZGEMM( 'No transpose', 'No transpose', M-J-JB+1,
     $                        N-J-JB+1, JB, -ONE, A( J+JB, J ), LDA,
     $                        A( J, J+JB ), LDA, ONE, A( J+JB, J+JB ),
     $                        LDA )
               END IF
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     End of ZGETRF
*
      END
      SUBROUTINE ZGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            INFO, LDA, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  ZGETRS solves a system of linear equations
*     A * X = B,  A**T * X = B,  or  A**H * X = B
*  with a general N-by-N matrix A using the LU factorization computed
*  by ZGETRF.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations:
*          = 'N':  A * X = B     (No transpose)
*          = 'T':  A**T * X = B  (Transpose)
*          = 'C':  A**H * X = B  (Conjugate transpose)
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The factors L and U from the factorization A = P*L*U
*          as computed by ZGETRF.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  IPIV    (input) INTEGER array, dimension (N)
*          The pivot indices from ZGETRF; for 1<=i<=N, row i of the
*          matrix was interchanged with row IPIV(i).
*
*  B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, the solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLASWP, ZTRSM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $    LSAME( TRANS, 'C' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZGETRS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
      IF( NOTRAN ) THEN
*
*        Solve A * X = B.
*
*        Apply row interchanges to the right hand sides.
*
         CALL ZLASWP( NRHS, B, LDB, 1, N, IPIV, 1 )
*
*        Solve L*X = B, overwriting B with X.
*
         CALL ZTRSM( 'Left', 'Lower', 'No transpose', 'Unit', N, NRHS,
     $               ONE, A, LDA, B, LDB )
*
*        Solve U*X = B, overwriting B with X.
*
         CALL ZTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', N,
     $               NRHS, ONE, A, LDA, B, LDB )
      ELSE
*
*        Solve A**T * X = B  or A**H * X = B.
*
*        Solve U'*X = B, overwriting B with X.
*
         CALL ZTRSM( 'Left', 'Upper', TRANS, 'Non-unit', N, NRHS, ONE,
     $               A, LDA, B, LDB )
*
*        Solve L'*X = B, overwriting B with X.
*
         CALL ZTRSM( 'Left', 'Lower', TRANS, 'Unit', N, NRHS, ONE, A,
     $               LDA, B, LDB )
*
*        Apply row interchanges to the solution vectors.
*
         CALL ZLASWP( NRHS, B, LDB, 1, N, IPIV, -1 )
      END IF
*
      RETURN
*
*     End of ZGETRS
*
      END
      SUBROUTINE ZHEEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK,
     $                  INFO )
*
*  -- LAPACK driver routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, LDA, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   RWORK( * ), W( * )
      COMPLEX*16         A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZHEEV computes all eigenvalues and, optionally, eigenvectors of a
*  complex Hermitian matrix A.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA, N)
*          On entry, the Hermitian matrix A.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of A contains the
*          upper triangular part of the matrix A.  If UPLO = 'L',
*          the leading N-by-N lower triangular part of A contains
*          the lower triangular part of the matrix A.
*          On exit, if JOBZ = 'V', then if INFO = 0, A contains the
*          orthonormal eigenvectors of the matrix A.
*          If JOBZ = 'N', then on exit the lower triangle (if UPLO='L')
*          or the upper triangle (if UPLO='U') of A, including the
*          diagonal, is destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= max(1,2*N-1).
*          For optimal efficiency, LWORK >= (NB+1)*N,
*          where NB is the blocksize for ZHETRD returned by ILAENV.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (max(1, 3*N-2))
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm failed to converge; i
*                off-diagonal elements of an intermediate tridiagonal
*                form did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
      COMPLEX*16         CONE
      PARAMETER          ( CONE = ( 1.0D0, 0.0D0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LOWER, LQUERY, WANTZ
      INTEGER            IINFO, IMAX, INDE, INDTAU, INDWRK, ISCALE,
     $                   LLWORK, LOPT, LWKOPT, NB
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, ZLANHE
      EXTERNAL           LSAME, ILAENV, DLAMCH, ZLANHE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSCAL, DSTERF, XERBLA, ZHETRD, ZLASCL, ZSTEQR,
     $                   ZUNGTR
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      LOWER = LSAME( UPLO, 'L' )
      LQUERY = ( LWORK.EQ.-1 )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, 2*N-1 ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
*
      IF( INFO.EQ.0 ) THEN
         NB = ILAENV( 1, 'ZHETRD', UPLO, N, -1, -1, -1 )
         LWKOPT = MAX( 1, ( NB+1 )*N )
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZHEEV ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      IF( N.EQ.1 ) THEN
         W( 1 ) = A( 1, 1 )
         WORK( 1 ) = 3
         IF( WANTZ )
     $      A( 1, 1 ) = CONE
         RETURN
      END IF
*
*     Get machine constants.
*
      SAFMIN = DLAMCH( 'Safe minimum' )
      EPS = DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN = SQRT( SMLNUM )
      RMAX = SQRT( BIGNUM )
*
*     Scale matrix to allowable range, if necessary.
*
      ANRM = ZLANHE( 'M', UPLO, N, A, LDA, RWORK )
      ISCALE = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 )
     $   CALL ZLASCL( UPLO, 0, 0, ONE, SIGMA, N, N, A, LDA, INFO )
*
*     Call ZHETRD to reduce Hermitian matrix to tridiagonal form.
*
      INDE = 1
      INDTAU = 1
      INDWRK = INDTAU + N
      LLWORK = LWORK - INDWRK + 1
      CALL ZHETRD( UPLO, N, A, LDA, W, RWORK( INDE ), WORK( INDTAU ),
     $             WORK( INDWRK ), LLWORK, IINFO )
      LOPT = N + WORK( INDWRK )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, first call
*     ZUNGTR to generate the unitary matrix, then call ZSTEQR.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, RWORK( INDE ), INFO )
      ELSE
         CALL ZUNGTR( UPLO, N, A, LDA, WORK( INDTAU ), WORK( INDWRK ),
     $                LLWORK, IINFO )
         INDWRK = INDE + N
         CALL ZSTEQR( JOBZ, N, W, RWORK( INDE ), A, LDA,
     $                RWORK( INDWRK ), INFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = N
         ELSE
            IMAX = INFO - 1
         END IF
         CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     Set WORK(1) to optimal complex workspace size.
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of ZHEEV
*
      END
      SUBROUTINE ZHETD2( UPLO, N, A, LDA, D, E, TAU, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
      COMPLEX*16         A( LDA, * ), TAU( * )
*     ..
*
*  Purpose
*  =======
*
*  ZHETD2 reduces a complex Hermitian matrix A to real symmetric
*  tridiagonal form T by a unitary similarity transformation:
*  Q' * A * Q = T.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          Hermitian matrix A is stored:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the Hermitian matrix A.  If UPLO = 'U', the leading
*          n-by-n upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading n-by-n lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*          On exit, if UPLO = 'U', the diagonal and first superdiagonal
*          of A are overwritten by the corresponding elements of the
*          tridiagonal matrix T, and the elements above the first
*          superdiagonal, with the array TAU, represent the unitary
*          matrix Q as a product of elementary reflectors; if UPLO
*          = 'L', the diagonal and first subdiagonal of A are over-
*          written by the corresponding elements of the tridiagonal
*          matrix T, and the elements below the first subdiagonal, with
*          the array TAU, represent the unitary matrix Q as a product
*          of elementary reflectors. See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  D       (output) DOUBLE PRECISION array, dimension (N)
*          The diagonal elements of the tridiagonal matrix T:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (N-1)
*          The off-diagonal elements of the tridiagonal matrix T:
*          E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
*
*  TAU     (output) COMPLEX*16 array, dimension (N-1)
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  If UPLO = 'U', the matrix Q is represented as a product of elementary
*  reflectors
*
*     Q = H(n-1) . . . H(2) H(1).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
*  A(1:i-1,i+1), and tau in TAU(i).
*
*  If UPLO = 'L', the matrix Q is represented as a product of elementary
*  reflectors
*
*     Q = H(1) H(2) . . . H(n-1).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
*  and tau in TAU(i).
*
*  The contents of A on exit are illustrated by the following examples
*  with n = 5:
*
*  if UPLO = 'U':                       if UPLO = 'L':
*
*    (  d   e   v2  v3  v4 )              (  d                  )
*    (      d   e   v3  v4 )              (  e   d              )
*    (          d   e   v4 )              (  v1  e   d          )
*    (              d   e  )              (  v1  v2  e   d      )
*    (                  d  )              (  v1  v2  v3  e   d  )
*
*  where d and e denote diagonal and off-diagonal elements of T, and vi
*  denotes an element of the vector defining H(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO, HALF
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   HALF = ( 0.5D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I
      COMPLEX*16         ALPHA, TAUI
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZAXPY, ZHEMV, ZHER2, ZLARFG
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      COMPLEX*16         ZDOTC
      EXTERNAL           LSAME, ZDOTC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZHETD2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Reduce the upper triangle of A
*
         A( N, N ) = DBLE( A( N, N ) )
         DO 10 I = N - 1, 1, -1
*
*           Generate elementary reflector H(i) = I - tau * v * v'
*           to annihilate A(1:i-1,i+1)
*
            ALPHA = A( I, I+1 )
            CALL ZLARFG( I, ALPHA, A( 1, I+1 ), 1, TAUI )
            E( I ) = ALPHA
*
            IF( TAUI.NE.ZERO ) THEN
*
*              Apply H(i) from both sides to A(1:i,1:i)
*
               A( I, I+1 ) = ONE
*
*              Compute  x := tau * A * v  storing x in TAU(1:i)
*
               CALL ZHEMV( UPLO, I, TAUI, A, LDA, A( 1, I+1 ), 1, ZERO,
     $                     TAU, 1 )
*
*              Compute  w := x - 1/2 * tau * (x'*v) * v
*
               ALPHA = -HALF*TAUI*ZDOTC( I, TAU, 1, A( 1, I+1 ), 1 )
               CALL ZAXPY( I, ALPHA, A( 1, I+1 ), 1, TAU, 1 )
*
*              Apply the transformation as a rank-2 update:
*                 A := A - v * w' - w * v'
*
               CALL ZHER2( UPLO, I, -ONE, A( 1, I+1 ), 1, TAU, 1, A,
     $                     LDA )
*
            ELSE
               A( I, I ) = DBLE( A( I, I ) )
            END IF
            A( I, I+1 ) = E( I )
            D( I+1 ) = A( I+1, I+1 )
            TAU( I ) = TAUI
   10    CONTINUE
         D( 1 ) = A( 1, 1 )
      ELSE
*
*        Reduce the lower triangle of A
*
         A( 1, 1 ) = DBLE( A( 1, 1 ) )
         DO 20 I = 1, N - 1
*
*           Generate elementary reflector H(i) = I - tau * v * v'
*           to annihilate A(i+2:n,i)
*
            ALPHA = A( I+1, I )
            CALL ZLARFG( N-I, ALPHA, A( MIN( I+2, N ), I ), 1, TAUI )
            E( I ) = ALPHA
*
            IF( TAUI.NE.ZERO ) THEN
*
*              Apply H(i) from both sides to A(i+1:n,i+1:n)
*
               A( I+1, I ) = ONE
*
*              Compute  x := tau * A * v  storing y in TAU(i:n-1)
*
               CALL ZHEMV( UPLO, N-I, TAUI, A( I+1, I+1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, TAU( I ), 1 )
*
*              Compute  w := x - 1/2 * tau * (x'*v) * v
*
               ALPHA = -HALF*TAUI*ZDOTC( N-I, TAU( I ), 1, A( I+1, I ),
     $                 1 )
               CALL ZAXPY( N-I, ALPHA, A( I+1, I ), 1, TAU( I ), 1 )
*
*              Apply the transformation as a rank-2 update:
*                 A := A - v * w' - w * v'
*
               CALL ZHER2( UPLO, N-I, -ONE, A( I+1, I ), 1, TAU( I ), 1,
     $                     A( I+1, I+1 ), LDA )
*
            ELSE
               A( I+1, I+1 ) = DBLE( A( I+1, I+1 ) )
            END IF
            A( I+1, I ) = E( I )
            D( I ) = A( I, I )
            TAU( I ) = TAUI
   20    CONTINUE
         D( N ) = A( N, N )
      END IF
*
      RETURN
*
*     End of ZHETD2
*
      END
      SUBROUTINE ZHETRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZHETRD reduces a complex Hermitian matrix A to real symmetric
*  tridiagonal form T by a unitary similarity transformation:
*  Q**H * A * Q = T.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the Hermitian matrix A.  If UPLO = 'U', the leading
*          N-by-N upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading N-by-N lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*          On exit, if UPLO = 'U', the diagonal and first superdiagonal
*          of A are overwritten by the corresponding elements of the
*          tridiagonal matrix T, and the elements above the first
*          superdiagonal, with the array TAU, represent the unitary
*          matrix Q as a product of elementary reflectors; if UPLO
*          = 'L', the diagonal and first subdiagonal of A are over-
*          written by the corresponding elements of the tridiagonal
*          matrix T, and the elements below the first subdiagonal, with
*          the array TAU, represent the unitary matrix Q as a product
*          of elementary reflectors. See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  D       (output) DOUBLE PRECISION array, dimension (N)
*          The diagonal elements of the tridiagonal matrix T:
*          D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (N-1)
*          The off-diagonal elements of the tridiagonal matrix T:
*          E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
*
*  TAU     (output) COMPLEX*16 array, dimension (N-1)
*          The scalar factors of the elementary reflectors (see Further
*          Details).
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= 1.
*          For optimum performance LWORK >= N*NB, where NB is the
*          optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  If UPLO = 'U', the matrix Q is represented as a product of elementary
*  reflectors
*
*     Q = H(n-1) . . . H(2) H(1).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
*  A(1:i-1,i+1), and tau in TAU(i).
*
*  If UPLO = 'L', the matrix Q is represented as a product of elementary
*  reflectors
*
*     Q = H(1) H(2) . . . H(n-1).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
*  and tau in TAU(i).
*
*  The contents of A on exit are illustrated by the following examples
*  with n = 5:
*
*  if UPLO = 'U':                       if UPLO = 'L':
*
*    (  d   e   v2  v3  v4 )              (  d                  )
*    (      d   e   v3  v4 )              (  e   d              )
*    (          d   e   v4 )              (  v1  e   d          )
*    (              d   e  )              (  v1  v2  e   d      )
*    (                  d  )              (  v1  v2  v3  e   d  )
*
*  where d and e denote diagonal and off-diagonal elements of T, and vi
*  denotes an element of the vector defining H(i).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
      COMPLEX*16         CONE
      PARAMETER          ( CONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER
      INTEGER            I, IINFO, IWS, J, KK, LDWORK, LWKOPT, NB,
     $                   NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZHER2K, ZHETD2, ZLATRD
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.1 .AND. .NOT.LQUERY ) THEN
         INFO = -9
      END IF
*
      IF( INFO.EQ.0 ) THEN
*
*        Determine the block size.
*
         NB = ILAENV( 1, 'ZHETRD', UPLO, N, -1, -1, -1 )
         LWKOPT = N*NB
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZHETRD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NX = N
      IWS = 1
      IF( NB.GT.1 .AND. NB.LT.N ) THEN
*
*        Determine when to cross over from blocked to unblocked code
*        (last block is always handled by unblocked code).
*
         NX = MAX( NB, ILAENV( 3, 'ZHETRD', UPLO, N, -1, -1, -1 ) )
         IF( NX.LT.N ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  determine the
*              minimum value of NB, and reduce NB or force use of
*              unblocked code by setting NX = N.
*
               NB = MAX( LWORK / LDWORK, 1 )
               NBMIN = ILAENV( 2, 'ZHETRD', UPLO, N, -1, -1, -1 )
               IF( NB.LT.NBMIN )
     $            NX = N
            END IF
         ELSE
            NX = N
         END IF
      ELSE
         NB = 1
      END IF
*
      IF( UPPER ) THEN
*
*        Reduce the upper triangle of A.
*        Columns 1:kk are handled by the unblocked method.
*
         KK = N - ( ( N-NX+NB-1 ) / NB )*NB
         DO 20 I = N - NB + 1, KK + 1, -NB
*
*           Reduce columns i:i+nb-1 to tridiagonal form and form the
*           matrix W which is needed to update the unreduced part of
*           the matrix
*
            CALL ZLATRD( UPLO, I+NB-1, NB, A, LDA, E, TAU, WORK,
     $                   LDWORK )
*
*           Update the unreduced submatrix A(1:i-1,1:i-1), using an
*           update of the form:  A := A - V*W' - W*V'
*
            CALL ZHER2K( UPLO, 'No transpose', I-1, NB, -CONE,
     $                   A( 1, I ), LDA, WORK, LDWORK, ONE, A, LDA )
*
*           Copy superdiagonal elements back into A, and diagonal
*           elements into D
*
            DO 10 J = I, I + NB - 1
               A( J-1, J ) = E( J-1 )
               D( J ) = A( J, J )
   10       CONTINUE
   20    CONTINUE
*
*        Use unblocked code to reduce the last or only block
*
         CALL ZHETD2( UPLO, KK, A, LDA, D, E, TAU, IINFO )
      ELSE
*
*        Reduce the lower triangle of A
*
         DO 40 I = 1, N - NX, NB
*
*           Reduce columns i:i+nb-1 to tridiagonal form and form the
*           matrix W which is needed to update the unreduced part of
*           the matrix
*
            CALL ZLATRD( UPLO, N-I+1, NB, A( I, I ), LDA, E( I ),
     $                   TAU( I ), WORK, LDWORK )
*
*           Update the unreduced submatrix A(i+nb:n,i+nb:n), using
*           an update of the form:  A := A - V*W' - W*V'
*
            CALL ZHER2K( UPLO, 'No transpose', N-I-NB+1, NB, -CONE,
     $                   A( I+NB, I ), LDA, WORK( NB+1 ), LDWORK, ONE,
     $                   A( I+NB, I+NB ), LDA )
*
*           Copy subdiagonal elements back into A, and diagonal
*           elements into D
*
            DO 30 J = I, I + NB - 1
               A( J+1, J ) = E( J )
               D( J ) = A( J, J )
   30       CONTINUE
   40    CONTINUE
*
*        Use unblocked code to reduce the last or only block
*
         CALL ZHETD2( UPLO, N-I+1, A( I, I ), LDA, D( I ), E( I ),
     $                TAU( I ), IINFO )
      END IF
*
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of ZHETRD
*
      END
      SUBROUTINE ZHSEQR( JOB, COMPZ, N, ILO, IHI, H, LDH, W, Z, LDZ,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ, JOB
      INTEGER            IHI, ILO, INFO, LDH, LDZ, LWORK, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         H( LDH, * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  ZHSEQR computes the eigenvalues of a complex upper Hessenberg
*  matrix H, and, optionally, the matrices T and Z from the Schur
*  decomposition H = Z T Z**H, where T is an upper triangular matrix
*  (the Schur form), and Z is the unitary matrix of Schur vectors.
*
*  Optionally Z may be postmultiplied into an input unitary matrix Q,
*  so that this routine can give the Schur factorization of a matrix A
*  which has been reduced to the Hessenberg form H by the unitary
*  matrix Q:  A = Q*H*Q**H = (QZ)*T*(QZ)**H.
*
*  Arguments
*  =========
*
*  JOB     (input) CHARACTER*1
*          = 'E': compute eigenvalues only;
*          = 'S': compute eigenvalues and the Schur form T.
*
*  COMPZ   (input) CHARACTER*1
*          = 'N': no Schur vectors are computed;
*          = 'I': Z is initialized to the unit matrix and the matrix Z
*                 of Schur vectors of H is returned;
*          = 'V': Z must contain an unitary matrix Q on entry, and
*                 the product Q*Z is returned.
*
*  N       (input) INTEGER
*          The order of the matrix H.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          It is assumed that H is already upper triangular in rows
*          and columns 1:ILO-1 and IHI+1:N. ILO and IHI are normally
*          set by a previous call to ZGEBAL, and then passed to CGEHRD
*          when the matrix output by ZGEBAL is reduced to Hessenberg
*          form. Otherwise ILO and IHI should be set to 1 and N
*          respectively.
*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
*
*  H       (input/output) COMPLEX*16 array, dimension (LDH,N)
*          On entry, the upper Hessenberg matrix H.
*          On exit, if JOB = 'S', H contains the upper triangular matrix
*          T from the Schur decomposition (the Schur form). If
*          JOB = 'E', the contents of H are unspecified on exit.
*
*  LDH     (input) INTEGER
*          The leading dimension of the array H. LDH >= max(1,N).
*
*  W       (output) COMPLEX*16 array, dimension (N)
*          The computed eigenvalues. If JOB = 'S', the eigenvalues are
*          stored in the same order as on the diagonal of the Schur form
*          returned in H, with W(i) = H(i,i).
*
*  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)
*          If COMPZ = 'N': Z is not referenced.
*          If COMPZ = 'I': on entry, Z need not be set, and on exit, Z
*          contains the unitary matrix Z of the Schur vectors of H.
*          If COMPZ = 'V': on entry Z must contain an N-by-N matrix Q,
*          which is assumed to be equal to the unit matrix except for
*          the submatrix Z(ILO:IHI,ILO:IHI); on exit Z contains Q*Z.
*          Normally Q is the unitary matrix generated by ZUNGHR after
*          the call to ZGEHRD which formed the Hessenberg matrix H.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.
*          LDZ >= max(1,N) if COMPZ = 'I' or 'V'; LDZ >= 1 otherwise.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,N).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, ZHSEQR failed to compute all the
*                eigenvalues in a total of 30*(IHI-ILO+1) iterations;
*                elements 1:ilo-1 and i+1:n of W contain those
*                eigenvalues which have been successfully computed.
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
      DOUBLE PRECISION   RZERO, RONE, CONST
      PARAMETER          ( RZERO = 0.0D+0, RONE = 1.0D+0,
     $                   CONST = 1.5D+0 )
      INTEGER            NSMAX, LDS
      PARAMETER          ( NSMAX = 15, LDS = NSMAX )
*     ..
*     .. Local Scalars ..
      LOGICAL            INITZ, LQUERY, WANTT, WANTZ
      INTEGER            I, I1, I2, IERR, II, ITEMP, ITN, ITS, J, K, L,
     $                   MAXB, NH, NR, NS, NV
      DOUBLE PRECISION   OVFL, RTEMP, SMLNUM, TST1, ULP, UNFL
      COMPLEX*16         CDUM, TAU, TEMP
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   RWORK( 1 )
      COMPLEX*16         S( LDS, NSMAX ), V( NSMAX+1 ), VV( NSMAX+1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV, IZAMAX
      DOUBLE PRECISION   DLAMCH, DLAPY2, ZLANHS
      EXTERNAL           LSAME, ILAENV, IZAMAX, DLAMCH, DLAPY2, ZLANHS
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZCOPY, ZDSCAL, ZGEMV, ZLACPY, ZLAHQR,
     $                   ZLARFG, ZLARFX, ZLASET, ZSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCONJG, DIMAG, MAX, MIN
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
*     ..
*     .. Executable Statements ..
*
*     Decode and test the input parameters
*
      WANTT = LSAME( JOB, 'S' )
      INITZ = LSAME( COMPZ, 'I' )
      WANTZ = INITZ .OR. LSAME( COMPZ, 'V' )
*
      INFO = 0
      WORK( 1 ) = MAX( 1, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.LSAME( JOB, 'E' ) .AND. .NOT.WANTT ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( COMPZ, 'N' ) .AND. .NOT.WANTZ ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN
         INFO = -5
      ELSE IF( LDH.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDZ.LT.1 .OR. WANTZ .AND. LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZHSEQR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Initialize Z, if necessary
*
      IF( INITZ )
     $   CALL ZLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
*
*     Store the eigenvalues isolated by ZGEBAL.
*
      DO 10 I = 1, ILO - 1
         W( I ) = H( I, I )
   10 CONTINUE
      DO 20 I = IHI + 1, N
         W( I ) = H( I, I )
   20 CONTINUE
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
      IF( ILO.EQ.IHI ) THEN
         W( ILO ) = H( ILO, ILO )
         RETURN
      END IF
*
*     Set rows and columns ILO to IHI to zero below the first
*     subdiagonal.
*
      DO 40 J = ILO, IHI - 2
         DO 30 I = J + 2, N
            H( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
      NH = IHI - ILO + 1
*
*     I1 and I2 are the indices of the first row and last column of H
*     to which transformations must be applied. If eigenvalues only are
*     being computed, I1 and I2 are re-set inside the main loop.
*
      IF( WANTT ) THEN
         I1 = 1
         I2 = N
      ELSE
         I1 = ILO
         I2 = IHI
      END IF
*
*     Ensure that the subdiagonal elements are real.
*
      DO 50 I = ILO + 1, IHI
         TEMP = H( I, I-1 )
         IF( DIMAG( TEMP ).NE.RZERO ) THEN
            RTEMP = DLAPY2( DBLE( TEMP ), DIMAG( TEMP ) )
            H( I, I-1 ) = RTEMP
            TEMP = TEMP / RTEMP
            IF( I2.GT.I )
     $         CALL ZSCAL( I2-I, DCONJG( TEMP ), H( I, I+1 ), LDH )
            CALL ZSCAL( I-I1, TEMP, H( I1, I ), 1 )
            IF( I.LT.IHI )
     $         H( I+1, I ) = TEMP*H( I+1, I )
            IF( WANTZ )
     $         CALL ZSCAL( NH, TEMP, Z( ILO, I ), 1 )
         END IF
   50 CONTINUE
*
*     Determine the order of the multi-shift QR algorithm to be used.
*
      NS = ILAENV( 4, 'ZHSEQR', JOB // COMPZ, N, ILO, IHI, -1 )
      MAXB = ILAENV( 8, 'ZHSEQR', JOB // COMPZ, N, ILO, IHI, -1 )
      IF( NS.LE.1 .OR. NS.GT.NH .OR. MAXB.GE.NH ) THEN
*
*        Use the standard double-shift algorithm
*
         CALL ZLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILO, IHI, Z,
     $                LDZ, INFO )
         RETURN
      END IF
      MAXB = MAX( 2, MAXB )
      NS = MIN( NS, MAXB, NSMAX )
*
*     Now 1 < NS <= MAXB < NH.
*
*     Set machine-dependent constants for the stopping criterion.
*     If norm(H) <= sqrt(OVFL), overflow should not occur.
*
      UNFL = DLAMCH( 'Safe minimum' )
      OVFL = RONE / UNFL
      CALL DLABAD( UNFL, OVFL )
      ULP = DLAMCH( 'Precision' )
      SMLNUM = UNFL*( NH / ULP )
*
*     ITN is the total number of multiple-shift QR iterations allowed.
*
      ITN = 30*NH
*
*     The main loop begins here. I is the loop index and decreases from
*     IHI to ILO in steps of at most MAXB. Each iteration of the loop
*     works with the active submatrix in rows and columns L to I.
*     Eigenvalues I+1 to IHI have already converged. Either L = ILO, or
*     H(L,L-1) is negligible so that the matrix splits.
*
      I = IHI
   60 CONTINUE
      IF( I.LT.ILO )
     $   GO TO 180
*
*     Perform multiple-shift QR iterations on rows and columns ILO to I
*     until a submatrix of order at most MAXB splits off at the bottom
*     because a subdiagonal element has become negligible.
*
      L = ILO
      DO 160 ITS = 0, ITN
*
*        Look for a single small subdiagonal element.
*
         DO 70 K = I, L + 1, -1
            TST1 = CABS1( H( K-1, K-1 ) ) + CABS1( H( K, K ) )
            IF( TST1.EQ.RZERO )
     $         TST1 = ZLANHS( '1', I-L+1, H( L, L ), LDH, RWORK )
            IF( ABS( DBLE( H( K, K-1 ) ) ).LE.MAX( ULP*TST1, SMLNUM ) )
     $         GO TO 80
   70    CONTINUE
   80    CONTINUE
         L = K
         IF( L.GT.ILO ) THEN
*
*           H(L,L-1) is negligible.
*
            H( L, L-1 ) = ZERO
         END IF
*
*        Exit from loop if a submatrix of order <= MAXB has split off.
*
         IF( L.GE.I-MAXB+1 )
     $      GO TO 170
*
*        Now the active submatrix is in rows and columns L to I. If
*        eigenvalues only are being computed, only the active submatrix
*        need be transformed.
*
         IF( .NOT.WANTT ) THEN
            I1 = L
            I2 = I
         END IF
*
         IF( ITS.EQ.20 .OR. ITS.EQ.30 ) THEN
*
*           Exceptional shifts.
*
            DO 90 II = I - NS + 1, I
               W( II ) = CONST*( ABS( DBLE( H( II, II-1 ) ) )+
     $                   ABS( DBLE( H( II, II ) ) ) )
   90       CONTINUE
         ELSE
*
*           Use eigenvalues of trailing submatrix of order NS as shifts.
*
            CALL ZLACPY( 'Full', NS, NS, H( I-NS+1, I-NS+1 ), LDH, S,
     $                   LDS )
            CALL ZLAHQR( .FALSE., .FALSE., NS, 1, NS, S, LDS,
     $                   W( I-NS+1 ), 1, NS, Z, LDZ, IERR )
            IF( IERR.GT.0 ) THEN
*
*              If ZLAHQR failed to compute all NS eigenvalues, use the
*              unconverged diagonal elements as the remaining shifts.
*
               DO 100 II = 1, IERR
                  W( I-NS+II ) = S( II, II )
  100          CONTINUE
            END IF
         END IF
*
*        Form the first column of (G-w(1)) (G-w(2)) . . . (G-w(ns))
*        where G is the Hessenberg submatrix H(L:I,L:I) and w is
*        the vector of shifts (stored in W). The result is
*        stored in the local array V.
*
         V( 1 ) = ONE
         DO 110 II = 2, NS + 1
            V( II ) = ZERO
  110    CONTINUE
         NV = 1
         DO 130 J = I - NS + 1, I
            CALL ZCOPY( NV+1, V, 1, VV, 1 )
            CALL ZGEMV( 'No transpose', NV+1, NV, ONE, H( L, L ), LDH,
     $                  VV, 1, -W( J ), V, 1 )
            NV = NV + 1
*
*           Scale V(1:NV) so that max(abs(V(i))) = 1. If V is zero,
*           reset it to the unit vector.
*
            ITEMP = IZAMAX( NV, V, 1 )
            RTEMP = CABS1( V( ITEMP ) )
            IF( RTEMP.EQ.RZERO ) THEN
               V( 1 ) = ONE
               DO 120 II = 2, NV
                  V( II ) = ZERO
  120          CONTINUE
            ELSE
               RTEMP = MAX( RTEMP, SMLNUM )
               CALL ZDSCAL( NV, RONE / RTEMP, V, 1 )
            END IF
  130    CONTINUE
*
*        Multiple-shift QR step
*
         DO 150 K = L, I - 1
*
*           The first iteration of this loop determines a reflection G
*           from the vector V and applies it from left and right to H,
*           thus creating a nonzero bulge below the subdiagonal.
*
*           Each subsequent iteration determines a reflection G to
*           restore the Hessenberg form in the (K-1)th column, and thus
*           chases the bulge one step toward the bottom of the active
*           submatrix. NR is the order of G.
*
            NR = MIN( NS+1, I-K+1 )
            IF( K.GT.L )
     $         CALL ZCOPY( NR, H( K, K-1 ), 1, V, 1 )
            CALL ZLARFG( NR, V( 1 ), V( 2 ), 1, TAU )
            IF( K.GT.L ) THEN
               H( K, K-1 ) = V( 1 )
               DO 140 II = K + 1, I
                  H( II, K-1 ) = ZERO
  140          CONTINUE
            END IF
            V( 1 ) = ONE
*
*           Apply G' from the left to transform the rows of the matrix
*           in columns K to I2.
*
            CALL ZLARFX( 'Left', NR, I2-K+1, V, DCONJG( TAU ),
     $                   H( K, K ), LDH, WORK )
*
*           Apply G from the right to transform the columns of the
*           matrix in rows I1 to min(K+NR,I).
*
            CALL ZLARFX( 'Right', MIN( K+NR, I )-I1+1, NR, V, TAU,
     $                   H( I1, K ), LDH, WORK )
*
            IF( WANTZ ) THEN
*
*              Accumulate transformations in the matrix Z
*
               CALL ZLARFX( 'Right', NH, NR, V, TAU, Z( ILO, K ), LDZ,
     $                      WORK )
            END IF
  150    CONTINUE
*
*        Ensure that H(I,I-1) is real.
*
         TEMP = H( I, I-1 )
         IF( DIMAG( TEMP ).NE.RZERO ) THEN
            RTEMP = DLAPY2( DBLE( TEMP ), DIMAG( TEMP ) )
            H( I, I-1 ) = RTEMP
            TEMP = TEMP / RTEMP
            IF( I2.GT.I )
     $         CALL ZSCAL( I2-I, DCONJG( TEMP ), H( I, I+1 ), LDH )
            CALL ZSCAL( I-I1, TEMP, H( I1, I ), 1 )
            IF( WANTZ ) THEN
               CALL ZSCAL( NH, TEMP, Z( ILO, I ), 1 )
            END IF
         END IF
*
  160 CONTINUE
*
*     Failure to converge in remaining number of iterations
*
      INFO = I
      RETURN
*
  170 CONTINUE
*
*     A submatrix of order <= MAXB in rows and columns L to I has split
*     off. Use the double-shift QR algorithm to handle it.
*
      CALL ZLAHQR( WANTT, WANTZ, N, L, I, H, LDH, W, ILO, IHI, Z, LDZ,
     $             INFO )
      IF( INFO.GT.0 )
     $   RETURN
*
*     Decrement number of remaining iterations, and return to start of
*     the main loop with a new value of I.
*
      ITN = ITN - ITS
      I = L - 1
      GO TO 60
*
  180 CONTINUE
      WORK( 1 ) = MAX( 1, N )
      RETURN
*
*     End of ZHSEQR
*
      END
      SUBROUTINE ZLABRD( M, N, NB, A, LDA, D, E, TAUQ, TAUP, X, LDX, Y,
     $                   LDY )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDX, LDY, M, N, NB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
      COMPLEX*16         A( LDA, * ), TAUP( * ), TAUQ( * ), X( LDX, * ),
     $                   Y( LDY, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLABRD reduces the first NB rows and columns of a complex general
*  m by n matrix A to upper or lower real bidiagonal form by a unitary
*  transformation Q' * A * P, and returns the matrices X and Y which
*  are needed to apply the transformation to the unreduced part of A.
*
*  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
*  bidiagonal form.
*
*  This is an auxiliary routine called by ZGEBRD
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows in the matrix A.
*
*  N       (input) INTEGER
*          The number of columns in the matrix A.
*
*  NB      (input) INTEGER
*          The number of leading rows and columns of A to be reduced.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit, the first NB rows and columns of the matrix are
*          overwritten; the rest of the array is unchanged.
*          If m >= n, elements on and below the diagonal in the first NB
*            columns, with the array TAUQ, represent the unitary
*            matrix Q as a product of elementary reflectors; and
*            elements above the diagonal in the first NB rows, with the
*            array TAUP, represent the unitary matrix P as a product
*            of elementary reflectors.
*          If m < n, elements below the diagonal in the first NB
*            columns, with the array TAUQ, represent the unitary
*            matrix Q as a product of elementary reflectors, and
*            elements on and above the diagonal in the first NB rows,
*            with the array TAUP, represent the unitary matrix P as
*            a product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  D       (output) DOUBLE PRECISION array, dimension (NB)
*          The diagonal elements of the first NB rows and columns of
*          the reduced matrix.  D(i) = A(i,i).
*
*  E       (output) DOUBLE PRECISION array, dimension (NB)
*          The off-diagonal elements of the first NB rows and columns of
*          the reduced matrix.
*
*  TAUQ    (output) COMPLEX*16 array dimension (NB)
*          The scalar factors of the elementary reflectors which
*          represent the unitary matrix Q. See Further Details.
*
*  TAUP    (output) COMPLEX*16 array, dimension (NB)
*          The scalar factors of the elementary reflectors which
*          represent the unitary matrix P. See Further Details.
*
*  X       (output) COMPLEX*16 array, dimension (LDX,NB)
*          The m-by-nb matrix X required to update the unreduced part
*          of A.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X. LDX >= max(1,M).
*
*  Y       (output) COMPLEX*16 array, dimension (LDY,NB)
*          The n-by-nb matrix Y required to update the unreduced part
*          of A.
*
*  LDY     (output) INTEGER
*          The leading dimension of the array Y. LDY >= max(1,N).
*
*  Further Details
*  ===============
*
*  The matrices Q and P are represented as products of elementary
*  reflectors:
*
*     Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)
*
*  Each H(i) and G(i) has the form:
*
*     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
*
*  where tauq and taup are complex scalars, and v and u are complex
*  vectors.
*
*  If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
*  A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
*  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
*  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
*  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
*
*  The elements of the vectors v and u together form the m-by-nb matrix
*  V and the nb-by-n matrix U' which are needed, with X and Y, to apply
*  the transformation to the unreduced part of the matrix, using a block
*  update of the form:  A := A - V*Y' - X*U'.
*
*  The contents of A on exit are illustrated by the following examples
*  with nb = 2:
*
*  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
*
*    (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
*    (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
*    (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
*    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
*    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
*    (  v1  v2  a   a   a  )
*
*  where a denotes an element of the original matrix which is unchanged,
*  vi denotes an element of the vector defining H(i), and ui an element
*  of the vector defining G(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      COMPLEX*16         ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZGEMV, ZLACGV, ZLARFG, ZSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      IF( M.GE.N ) THEN
*
*        Reduce to upper bidiagonal form
*
         DO 10 I = 1, NB
*
*           Update A(i:m,i)
*
            CALL ZLACGV( I-1, Y( I, 1 ), LDY )
            CALL ZGEMV( 'No transpose', M-I+1, I-1, -ONE, A( I, 1 ),
     $                  LDA, Y( I, 1 ), LDY, ONE, A( I, I ), 1 )
            CALL ZLACGV( I-1, Y( I, 1 ), LDY )
            CALL ZGEMV( 'No transpose', M-I+1, I-1, -ONE, X( I, 1 ),
     $                  LDX, A( 1, I ), 1, ONE, A( I, I ), 1 )
*
*           Generate reflection Q(i) to annihilate A(i+1:m,i)
*
            ALPHA = A( I, I )
            CALL ZLARFG( M-I+1, ALPHA, A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = ALPHA
            IF( I.LT.N ) THEN
               A( I, I ) = ONE
*
*              Compute Y(i+1:n,i)
*
               CALL ZGEMV( 'Conjugate transpose', M-I+1, N-I, ONE,
     $                     A( I, I+1 ), LDA, A( I, I ), 1, ZERO,
     $                     Y( I+1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', M-I+1, I-1, ONE,
     $                     A( I, 1 ), LDA, A( I, I ), 1, ZERO,
     $                     Y( 1, I ), 1 )
               CALL ZGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', M-I+1, I-1, ONE,
     $                     X( I, 1 ), LDX, A( I, I ), 1, ZERO,
     $                     Y( 1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', I-1, N-I, -ONE,
     $                     A( 1, I+1 ), LDA, Y( 1, I ), 1, ONE,
     $                     Y( I+1, I ), 1 )
               CALL ZSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
*
*              Update A(i,i+1:n)
*
               CALL ZLACGV( N-I, A( I, I+1 ), LDA )
               CALL ZLACGV( I, A( I, 1 ), LDA )
               CALL ZGEMV( 'No transpose', N-I, I, -ONE, Y( I+1, 1 ),
     $                     LDY, A( I, 1 ), LDA, ONE, A( I, I+1 ), LDA )
               CALL ZLACGV( I, A( I, 1 ), LDA )
               CALL ZLACGV( I-1, X( I, 1 ), LDX )
               CALL ZGEMV( 'Conjugate transpose', I-1, N-I, -ONE,
     $                     A( 1, I+1 ), LDA, X( I, 1 ), LDX, ONE,
     $                     A( I, I+1 ), LDA )
               CALL ZLACGV( I-1, X( I, 1 ), LDX )
*
*              Generate reflection P(i) to annihilate A(i,i+2:n)
*
               ALPHA = A( I, I+1 )
               CALL ZLARFG( N-I, ALPHA, A( I, MIN( I+2, N ) ), LDA,
     $                      TAUP( I ) )
               E( I ) = ALPHA
               A( I, I+1 ) = ONE
*
*              Compute X(i+1:m,i)
*
               CALL ZGEMV( 'No transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( I+1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', N-I, I, ONE,
     $                     Y( I+1, 1 ), LDY, A( I, I+1 ), LDA, ZERO,
     $                     X( 1, I ), 1 )
               CALL ZGEMV( 'No transpose', M-I, I, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL ZGEMV( 'No transpose', I-1, N-I, ONE, A( 1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL ZGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL ZSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
               CALL ZLACGV( N-I, A( I, I+1 ), LDA )
            END IF
   10    CONTINUE
      ELSE
*
*        Reduce to lower bidiagonal form
*
         DO 20 I = 1, NB
*
*           Update A(i,i:n)
*
            CALL ZLACGV( N-I+1, A( I, I ), LDA )
            CALL ZLACGV( I-1, A( I, 1 ), LDA )
            CALL ZGEMV( 'No transpose', N-I+1, I-1, -ONE, Y( I, 1 ),
     $                  LDY, A( I, 1 ), LDA, ONE, A( I, I ), LDA )
            CALL ZLACGV( I-1, A( I, 1 ), LDA )
            CALL ZLACGV( I-1, X( I, 1 ), LDX )
            CALL ZGEMV( 'Conjugate transpose', I-1, N-I+1, -ONE,
     $                  A( 1, I ), LDA, X( I, 1 ), LDX, ONE, A( I, I ),
     $                  LDA )
            CALL ZLACGV( I-1, X( I, 1 ), LDX )
*
*           Generate reflection P(i) to annihilate A(i,i+1:n)
*
            ALPHA = A( I, I )
            CALL ZLARFG( N-I+1, ALPHA, A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = ALPHA
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
*
*              Compute X(i+1:m,i)
*
               CALL ZGEMV( 'No transpose', M-I, N-I+1, ONE, A( I+1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( I+1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', N-I+1, I-1, ONE,
     $                     Y( I, 1 ), LDY, A( I, I ), LDA, ZERO,
     $                     X( 1, I ), 1 )
               CALL ZGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL ZGEMV( 'No transpose', I-1, N-I+1, ONE, A( 1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL ZGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL ZSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
               CALL ZLACGV( N-I+1, A( I, I ), LDA )
*
*              Update A(i+1:m,i)
*
               CALL ZLACGV( I-1, Y( I, 1 ), LDY )
               CALL ZGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, Y( I, 1 ), LDY, ONE, A( I+1, I ), 1 )
               CALL ZLACGV( I-1, Y( I, 1 ), LDY )
               CALL ZGEMV( 'No transpose', M-I, I, -ONE, X( I+1, 1 ),
     $                     LDX, A( 1, I ), 1, ONE, A( I+1, I ), 1 )
*
*              Generate reflection Q(i) to annihilate A(i+2:m,i)
*
               ALPHA = A( I+1, I )
               CALL ZLARFG( M-I, ALPHA, A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = ALPHA
               A( I+1, I ) = ONE
*
*              Compute Y(i+1:n,i)
*
               CALL ZGEMV( 'Conjugate transpose', M-I, N-I, ONE,
     $                     A( I+1, I+1 ), LDA, A( I+1, I ), 1, ZERO,
     $                     Y( I+1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', M-I, I-1, ONE,
     $                     A( I+1, 1 ), LDA, A( I+1, I ), 1, ZERO,
     $                     Y( 1, I ), 1 )
               CALL ZGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', M-I, I, ONE,
     $                     X( I+1, 1 ), LDX, A( I+1, I ), 1, ZERO,
     $                     Y( 1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', I, N-I, -ONE,
     $                     A( 1, I+1 ), LDA, Y( 1, I ), 1, ONE,
     $                     Y( I+1, I ), 1 )
               CALL ZSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
            ELSE
               CALL ZLACGV( N-I+1, A( I, I ), LDA )
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     End of ZLABRD
*
      END
      SUBROUTINE ZLACGV( N, X, INCX )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         X( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLACGV conjugates a complex vector of length N.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The length of the vector X.  N >= 0.
*
*  X       (input/output) COMPLEX*16 array, dimension
*                         (1+(N-1)*abs(INCX))
*          On entry, the vector of length N to be conjugated.
*          On exit, X is overwritten with conjg(X).
*
*  INCX    (input) INTEGER
*          The spacing between successive elements of X.
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IOFF
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
*     ..
*     .. Executable Statements ..
*
      IF( INCX.EQ.1 ) THEN
         DO 10 I = 1, N
            X( I ) = DCONJG( X( I ) )
   10    CONTINUE
      ELSE
         IOFF = 1
         IF( INCX.LT.0 )
     $      IOFF = 1 - ( N-1 )*INCX
         DO 20 I = 1, N
            X( IOFF ) = DCONJG( X( IOFF ) )
            IOFF = IOFF + INCX
   20    CONTINUE
      END IF
      RETURN
*
*     End of ZLACGV
*
      END
      SUBROUTINE ZLACPY( UPLO, M, N, A, LDA, B, LDB )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDB, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLACPY copies all or part of a two-dimensional matrix A to another
*  matrix B.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies the part of the matrix A to be copied to B.
*          = 'U':      Upper triangular part
*          = 'L':      Lower triangular part
*          Otherwise:  All of the matrix A
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The m by n matrix A.  If UPLO = 'U', only the upper trapezium
*          is accessed; if UPLO = 'L', only the lower trapezium is
*          accessed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (output) COMPLEX*16 array, dimension (LDB,N)
*          On exit, B = A in the locations specified by UPLO.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,M).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 20 J = 1, N
            DO 10 I = 1, MIN( J, M )
               B( I, J ) = A( I, J )
   10       CONTINUE
   20    CONTINUE
*
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
         DO 40 J = 1, N
            DO 30 I = J, M
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
*
      ELSE
         DO 60 J = 1, N
            DO 50 I = 1, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
*
      RETURN
*
*     End of ZLACPY
*
      END
      DOUBLE COMPLEX   FUNCTION ZLADIV( X, Y )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      COMPLEX*16         X, Y
*     ..
*
*  Purpose
*  =======
*
*  ZLADIV := X / Y, where X and Y are complex.  The computation of X / Y
*  will not overflow on an intermediary step unless the results
*  overflows.
*
*  Arguments
*  =========
*
*  X       (input) COMPLEX*16
*  Y       (input) COMPLEX*16
*          The complex scalars X and Y.
*
*  =====================================================================
*
*     .. Local Scalars ..
      DOUBLE PRECISION   ZI, ZR
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLADIV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, DCMPLX, DIMAG
*     ..
*     .. Executable Statements ..
*
      CALL DLADIV( DBLE( X ), DIMAG( X ), DBLE( Y ), DIMAG( Y ), ZR,
     $             ZI )
      ZLADIV = DCMPLX( ZR, ZI )
*
      RETURN
*
*     End of ZLADIV
*
      END
      SUBROUTINE ZLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, W, ILOZ,
     $                   IHIZ, Z, LDZ, INFO )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      LOGICAL            WANTT, WANTZ
      INTEGER            IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         H( LDH, * ), W( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLAHQR is an auxiliary routine called by ZHSEQR to update the
*  eigenvalues and Schur decomposition already computed by ZHSEQR, by
*  dealing with the Hessenberg submatrix in rows and columns ILO to IHI.
*
*  Arguments
*  =========
*
*  WANTT   (input) LOGICAL
*          = .TRUE. : the full Schur form T is required;
*          = .FALSE.: only eigenvalues are required.
*
*  WANTZ   (input) LOGICAL
*          = .TRUE. : the matrix of Schur vectors Z is required;
*          = .FALSE.: Schur vectors are not required.
*
*  N       (input) INTEGER
*          The order of the matrix H.  N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          It is assumed that H is already upper triangular in rows and
*          columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless ILO = 1).
*          ZLAHQR works primarily with the Hessenberg submatrix in rows
*          and columns ILO to IHI, but applies transformations to all of
*          H if WANTT is .TRUE..
*          1 <= ILO <= max(1,IHI); IHI <= N.
*
*  H       (input/output) COMPLEX*16 array, dimension (LDH,N)
*          On entry, the upper Hessenberg matrix H.
*          On exit, if WANTT is .TRUE., H is upper triangular in rows
*          and columns ILO:IHI, with any 2-by-2 diagonal blocks in
*          standard form. If WANTT is .FALSE., the contents of H are
*          unspecified on exit.
*
*  LDH     (input) INTEGER
*          The leading dimension of the array H. LDH >= max(1,N).
*
*  W       (output) COMPLEX*16 array, dimension (N)
*          The computed eigenvalues ILO to IHI are stored in the
*          corresponding elements of W. If WANTT is .TRUE., the
*          eigenvalues are stored in the same order as on the diagonal
*          of the Schur form returned in H, with W(i) = H(i,i).
*
*  ILOZ    (input) INTEGER
*  IHIZ    (input) INTEGER
*          Specify the rows of Z to which transformations must be
*          applied if WANTZ is .TRUE..
*          1 <= ILOZ <= ILO; IHI <= IHIZ <= N.
*
*  Z       (input/output) COMPLEX*16 array, dimension (LDZ,N)
*          If WANTZ is .TRUE., on entry Z must contain the current
*          matrix Z of transformations accumulated by ZHSEQR, and on
*          exit Z has been updated; transformations are applied only to
*          the submatrix Z(ILOZ:IHIZ,ILO:IHI).
*          If WANTZ is .FALSE., Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z. LDZ >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          > 0: if INFO = i, ZLAHQR failed to compute all the
*               eigenvalues ILO to IHI in a total of 30*(IHI-ILO+1)
*               iterations; elements i+1:ihi of W contain those
*               eigenvalues which have been successfully computed.
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
      DOUBLE PRECISION   RZERO, HALF
      PARAMETER          ( RZERO = 0.0D+0, HALF = 0.5D+0 )
      DOUBLE PRECISION   DAT1
      PARAMETER          ( DAT1 = 0.75D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, I1, I2, ITN, ITS, J, K, L, M, NH, NZ
      DOUBLE PRECISION   H10, H21, RTEMP, S, SMLNUM, T2, TST1, ULP
      COMPLEX*16         CDUM, H11, H11S, H22, SUM, T, T1, TEMP, U, V2,
     $                   X, Y
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   RWORK( 1 )
      COMPLEX*16         V( 2 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, ZLANHS
      COMPLEX*16         ZLADIV
      EXTERNAL           DLAMCH, ZLANHS, ZLADIV
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZCOPY, ZLARFG, ZSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCONJG, DIMAG, MAX, MIN, SQRT
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
      IF( ILO.EQ.IHI ) THEN
         W( ILO ) = H( ILO, ILO )
         RETURN
      END IF
*
      NH = IHI - ILO + 1
      NZ = IHIZ - ILOZ + 1
*
*     Set machine-dependent constants for the stopping criterion.
*     If norm(H) <= sqrt(OVFL), overflow should not occur.
*
      ULP = DLAMCH( 'Precision' )
      SMLNUM = DLAMCH( 'Safe minimum' ) / ULP
*
*     I1 and I2 are the indices of the first row and last column of H
*     to which transformations must be applied. If eigenvalues only are
*     being computed, I1 and I2 are set inside the main loop.
*
      IF( WANTT ) THEN
         I1 = 1
         I2 = N
      END IF
*
*     ITN is the total number of QR iterations allowed.
*
      ITN = 30*NH
*
*     The main loop begins here. I is the loop index and decreases from
*     IHI to ILO in steps of 1. Each iteration of the loop works
*     with the active submatrix in rows and columns L to I.
*     Eigenvalues I+1 to IHI have already converged. Either L = ILO, or
*     H(L,L-1) is negligible so that the matrix splits.
*
      I = IHI
   10 CONTINUE
      IF( I.LT.ILO )
     $   GO TO 130
*
*     Perform QR iterations on rows and columns ILO to I until a
*     submatrix of order 1 splits off at the bottom because a
*     subdiagonal element has become negligible.
*
      L = ILO
      DO 110 ITS = 0, ITN
*
*        Look for a single small subdiagonal element.
*
         DO 20 K = I, L + 1, -1
            TST1 = CABS1( H( K-1, K-1 ) ) + CABS1( H( K, K ) )
            IF( TST1.EQ.RZERO )
     $         TST1 = ZLANHS( '1', I-L+1, H( L, L ), LDH, RWORK )
            IF( ABS( DBLE( H( K, K-1 ) ) ).LE.MAX( ULP*TST1, SMLNUM ) )
     $         GO TO 30
   20    CONTINUE
   30    CONTINUE
         L = K
         IF( L.GT.ILO ) THEN
*
*           H(L,L-1) is negligible
*
            H( L, L-1 ) = ZERO
         END IF
*
*        Exit from loop if a submatrix of order 1 has split off.
*
         IF( L.GE.I )
     $      GO TO 120
*
*        Now the active submatrix is in rows and columns L to I. If
*        eigenvalues only are being computed, only the active submatrix
*        need be transformed.
*
         IF( .NOT.WANTT ) THEN
            I1 = L
            I2 = I
         END IF
*
         IF( ITS.EQ.10 .OR. ITS.EQ.20 ) THEN
*
*           Exceptional shift.
*
            S = DAT1*ABS( DBLE( H( I, I-1 ) ) )
            T = S + H( I, I )
         ELSE
*
*           Wilkinson's shift.
*
            T = H( I, I )
            U = H( I-1, I )*DBLE( H( I, I-1 ) )
            IF( U.NE.ZERO ) THEN
               X = HALF*( H( I-1, I-1 )-T )
               Y = SQRT( X*X+U )
               IF( DBLE( X )*DBLE( Y )+DIMAG( X )*DIMAG( Y ).LT.RZERO )
     $            Y = -Y
               T = T - ZLADIV( U, ( X+Y ) )
            END IF
         END IF
*
*        Look for two consecutive small subdiagonal elements.
*
         DO 40 M = I - 1, L + 1, -1
*
*           Determine the effect of starting the single-shift QR
*           iteration at row M, and see if this would make H(M,M-1)
*           negligible.
*
            H11 = H( M, M )
            H22 = H( M+1, M+1 )
            H11S = H11 - T
            H21 = H( M+1, M )
            S = CABS1( H11S ) + ABS( H21 )
            H11S = H11S / S
            H21 = H21 / S
            V( 1 ) = H11S
            V( 2 ) = H21
            H10 = H( M, M-1 )
            TST1 = CABS1( H11S )*( CABS1( H11 )+CABS1( H22 ) )
            IF( ABS( H10*H21 ).LE.ULP*TST1 )
     $         GO TO 50
   40    CONTINUE
         H11 = H( L, L )
         H22 = H( L+1, L+1 )
         H11S = H11 - T
         H21 = H( L+1, L )
         S = CABS1( H11S ) + ABS( H21 )
         H11S = H11S / S
         H21 = H21 / S
         V( 1 ) = H11S
         V( 2 ) = H21
   50    CONTINUE
*
*        Single-shift QR step
*
         DO 100 K = M, I - 1
*
*           The first iteration of this loop determines a reflection G
*           from the vector V and applies it from left and right to H,
*           thus creating a nonzero bulge below the subdiagonal.
*
*           Each subsequent iteration determines a reflection G to
*           restore the Hessenberg form in the (K-1)th column, and thus
*           chases the bulge one step toward the bottom of the active
*           submatrix.
*
*           V(2) is always real before the call to ZLARFG, and hence
*           after the call T2 ( = T1*V(2) ) is also real.
*
            IF( K.GT.M )
     $         CALL ZCOPY( 2, H( K, K-1 ), 1, V, 1 )
            CALL ZLARFG( 2, V( 1 ), V( 2 ), 1, T1 )
            IF( K.GT.M ) THEN
               H( K, K-1 ) = V( 1 )
               H( K+1, K-1 ) = ZERO
            END IF
            V2 = V( 2 )
            T2 = DBLE( T1*V2 )
*
*           Apply G from the left to transform the rows of the matrix
*           in columns K to I2.
*
            DO 60 J = K, I2
               SUM = DCONJG( T1 )*H( K, J ) + T2*H( K+1, J )
               H( K, J ) = H( K, J ) - SUM
               H( K+1, J ) = H( K+1, J ) - SUM*V2
   60       CONTINUE
*
*           Apply G from the right to transform the columns of the
*           matrix in rows I1 to min(K+2,I).
*
            DO 70 J = I1, MIN( K+2, I )
               SUM = T1*H( J, K ) + T2*H( J, K+1 )
               H( J, K ) = H( J, K ) - SUM
               H( J, K+1 ) = H( J, K+1 ) - SUM*DCONJG( V2 )
   70       CONTINUE
*
            IF( WANTZ ) THEN
*
*              Accumulate transformations in the matrix Z
*
               DO 80 J = ILOZ, IHIZ
                  SUM = T1*Z( J, K ) + T2*Z( J, K+1 )
                  Z( J, K ) = Z( J, K ) - SUM
                  Z( J, K+1 ) = Z( J, K+1 ) - SUM*DCONJG( V2 )
   80          CONTINUE
            END IF
*
            IF( K.EQ.M .AND. M.GT.L ) THEN
*
*              If the QR step was started at row M > L because two
*              consecutive small subdiagonals were found, then extra
*              scaling must be performed to ensure that H(M,M-1) remains
*              real.
*
               TEMP = ONE - T1
               TEMP = TEMP / ABS( TEMP )
               H( M+1, M ) = H( M+1, M )*DCONJG( TEMP )
               IF( M+2.LE.I )
     $            H( M+2, M+1 ) = H( M+2, M+1 )*TEMP
               DO 90 J = M, I
                  IF( J.NE.M+1 ) THEN
                     IF( I2.GT.J )
     $                  CALL ZSCAL( I2-J, TEMP, H( J, J+1 ), LDH )
                     CALL ZSCAL( J-I1, DCONJG( TEMP ), H( I1, J ), 1 )
                     IF( WANTZ ) THEN
                        CALL ZSCAL( NZ, DCONJG( TEMP ), Z( ILOZ, J ),
     $                              1 )
                     END IF
                  END IF
   90          CONTINUE
            END IF
  100    CONTINUE
*
*        Ensure that H(I,I-1) is real.
*
         TEMP = H( I, I-1 )
         IF( DIMAG( TEMP ).NE.RZERO ) THEN
            RTEMP = ABS( TEMP )
            H( I, I-1 ) = RTEMP
            TEMP = TEMP / RTEMP
            IF( I2.GT.I )
     $         CALL ZSCAL( I2-I, DCONJG( TEMP ), H( I, I+1 ), LDH )
            CALL ZSCAL( I-I1, TEMP, H( I1, I ), 1 )
            IF( WANTZ ) THEN
               CALL ZSCAL( NZ, TEMP, Z( ILOZ, I ), 1 )
            END IF
         END IF
*
  110 CONTINUE
*
*     Failure to converge in remaining number of iterations
*
      INFO = I
      RETURN
*
  120 CONTINUE
*
*     H(I,I-1) is negligible: one eigenvalue has converged.
*
      W( I ) = H( I, I )
*
*     Decrement number of remaining iterations, and return to start of
*     the main loop with new value of I.
*
      ITN = ITN - ITS
      I = L - 1
      GO TO 10
*
  130 CONTINUE
      RETURN
*
*     End of ZLAHQR
*
      END
      SUBROUTINE ZLAHRD( N, K, NB, A, LDA, TAU, T, LDT, Y, LDY )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            K, LDA, LDT, LDY, N, NB
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), T( LDT, NB ), TAU( NB ),
     $                   Y( LDY, NB )
*     ..
*
*  Purpose
*  =======
*
*  ZLAHRD reduces the first NB columns of a complex general n-by-(n-k+1)
*  matrix A so that elements below the k-th subdiagonal are zero. The
*  reduction is performed by a unitary similarity transformation
*  Q' * A * Q. The routine returns the matrices V and T which determine
*  Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T.
*
*  This is an auxiliary routine called by ZGEHRD.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix A.
*
*  K       (input) INTEGER
*          The offset for the reduction. Elements below the k-th
*          subdiagonal in the first NB columns are reduced to zero.
*
*  NB      (input) INTEGER
*          The number of columns to be reduced.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N-K+1)
*          On entry, the n-by-(n-k+1) general matrix A.
*          On exit, the elements on and above the k-th subdiagonal in
*          the first NB columns are overwritten with the corresponding
*          elements of the reduced matrix; the elements below the k-th
*          subdiagonal, with the array TAU, represent the matrix Q as a
*          product of elementary reflectors. The other columns of A are
*          unchanged. See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  TAU     (output) COMPLEX*16 array, dimension (NB)
*          The scalar factors of the elementary reflectors. See Further
*          Details.
*
*  T       (output) COMPLEX*16 array, dimension (LDT,NB)
*          The upper triangular matrix T.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T.  LDT >= NB.
*
*  Y       (output) COMPLEX*16 array, dimension (LDY,NB)
*          The n-by-nb matrix Y.
*
*  LDY     (input) INTEGER
*          The leading dimension of the array Y. LDY >= max(1,N).
*
*  Further Details
*  ===============
*
*  The matrix Q is represented as a product of nb elementary reflectors
*
*     Q = H(1) H(2) . . . H(nb).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i+k-1) = 0, v(i+k) = 1; v(i+k+1:n) is stored on exit in
*  A(i+k+1:n,i), and tau in TAU(i).
*
*  The elements of the vectors v together form the (n-k+1)-by-nb matrix
*  V which is needed, with T and Y, to apply the transformation to the
*  unreduced part of the matrix, using an update of the form:
*  A := (I - V*T*V') * (A - Y*V').
*
*  The contents of A on exit are illustrated by the following example
*  with n = 7, k = 3 and nb = 2:
*
*     ( a   h   a   a   a )
*     ( a   h   a   a   a )
*     ( a   h   a   a   a )
*     ( h   h   a   a   a )
*     ( v1  h   a   a   a )
*     ( v1  v2  a   a   a )
*     ( v1  v2  a   a   a )
*
*  where a denotes an element of the original matrix A, h denotes a
*  modified element of the upper Hessenberg matrix H, and vi denotes an
*  element of the vector defining H(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      COMPLEX*16         EI
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZAXPY, ZCOPY, ZGEMV, ZLACGV, ZLARFG, ZSCAL,
     $                   ZTRMV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.1 )
     $   RETURN
*
      DO 10 I = 1, NB
         IF( I.GT.1 ) THEN
*
*           Update A(1:n,i)
*
*           Compute i-th column of A - Y * V'
*
            CALL ZLACGV( I-1, A( K+I-1, 1 ), LDA )
            CALL ZGEMV( 'No transpose', N, I-1, -ONE, Y, LDY,
     $                  A( K+I-1, 1 ), LDA, ONE, A( 1, I ), 1 )
            CALL ZLACGV( I-1, A( K+I-1, 1 ), LDA )
*
*           Apply I - V * T' * V' to this column (call it b) from the
*           left, using the last column of T as workspace
*
*           Let  V = ( V1 )   and   b = ( b1 )   (first I-1 rows)
*                    ( V2 )             ( b2 )
*
*           where V1 is unit lower triangular
*
*           w := V1' * b1
*
            CALL ZCOPY( I-1, A( K+1, I ), 1, T( 1, NB ), 1 )
            CALL ZTRMV( 'Lower', 'Conjugate transpose', 'Unit', I-1,
     $                  A( K+1, 1 ), LDA, T( 1, NB ), 1 )
*
*           w := w + V2'*b2
*
            CALL ZGEMV( 'Conjugate transpose', N-K-I+1, I-1, ONE,
     $                  A( K+I, 1 ), LDA, A( K+I, I ), 1, ONE,
     $                  T( 1, NB ), 1 )
*
*           w := T'*w
*
            CALL ZTRMV( 'Upper', 'Conjugate transpose', 'Non-unit', I-1,
     $                  T, LDT, T( 1, NB ), 1 )
*
*           b2 := b2 - V2*w
*
            CALL ZGEMV( 'No transpose', N-K-I+1, I-1, -ONE, A( K+I, 1 ),
     $                  LDA, T( 1, NB ), 1, ONE, A( K+I, I ), 1 )
*
*           b1 := b1 - V1*w
*
            CALL ZTRMV( 'Lower', 'No transpose', 'Unit', I-1,
     $                  A( K+1, 1 ), LDA, T( 1, NB ), 1 )
            CALL ZAXPY( I-1, -ONE, T( 1, NB ), 1, A( K+1, I ), 1 )
*
            A( K+I-1, I-1 ) = EI
         END IF
*
*        Generate the elementary reflector H(i) to annihilate
*        A(k+i+1:n,i)
*
         EI = A( K+I, I )
         CALL ZLARFG( N-K-I+1, EI, A( MIN( K+I+1, N ), I ), 1,
     $                TAU( I ) )
         A( K+I, I ) = ONE
*
*        Compute  Y(1:n,i)
*
         CALL ZGEMV( 'No transpose', N, N-K-I+1, ONE, A( 1, I+1 ), LDA,
     $               A( K+I, I ), 1, ZERO, Y( 1, I ), 1 )
         CALL ZGEMV( 'Conjugate transpose', N-K-I+1, I-1, ONE,
     $               A( K+I, 1 ), LDA, A( K+I, I ), 1, ZERO, T( 1, I ),
     $               1 )
         CALL ZGEMV( 'No transpose', N, I-1, -ONE, Y, LDY, T( 1, I ), 1,
     $               ONE, Y( 1, I ), 1 )
         CALL ZSCAL( N, TAU( I ), Y( 1, I ), 1 )
*
*        Compute T(1:i,i)
*
         CALL ZSCAL( I-1, -TAU( I ), T( 1, I ), 1 )
         CALL ZTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T, LDT,
     $               T( 1, I ), 1 )
         T( I, I ) = TAU( I )
*
   10 CONTINUE
      A( K+NB, NB ) = EI
*
      RETURN
*
*     End of ZLAHRD
*
      END
      DOUBLE PRECISION FUNCTION ZLANGE( NORM, M, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   WORK( * )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex matrix A.
*
*  Description
*  ===========
*
*  ZLANGE returns the value
*
*     ZLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
*              (
*              ( norm1(A),         NORM = '1', 'O' or 'o'
*              (
*              ( normI(A),         NORM = 'I' or 'i'
*              (
*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
*
*  where  norm1  denotes the  one norm of a matrix (maximum column sum),
*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
*  normF  denotes the  Frobenius norm of a matrix (square root of sum of
*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in ZLANGE as described
*          above.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.  When M = 0,
*          ZLANGE is set to zero.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.  When N = 0,
*          ZLANGE is set to zero.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The m by n matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(M,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
*          where LWORK >= M when NORM = 'I'; otherwise, WORK is not
*          referenced.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   SCALE, SUM, VALUE
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZLASSQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( MIN( M, N ).EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, M
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, M
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         DO 50 I = 1, M
            WORK( I ) = ZERO
   50    CONTINUE
         DO 70 J = 1, N
            DO 60 I = 1, M
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, M
            VALUE = MAX( VALUE, WORK( I ) )
   80    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL ZLASSQ( M, A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      ZLANGE = VALUE
      RETURN
*
*     End of ZLANGE
*
      END
      DOUBLE PRECISION FUNCTION ZLANHE( NORM, UPLO, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          NORM, UPLO
      INTEGER            LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   WORK( * )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLANHE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  complex hermitian matrix A.
*
*  Description
*  ===========
*
*  ZLANHE returns the value
*
*     ZLANHE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
*              (
*              ( norm1(A),         NORM = '1', 'O' or 'o'
*              (
*              ( normI(A),         NORM = 'I' or 'i'
*              (
*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
*
*  where  norm1  denotes the  one norm of a matrix (maximum column sum),
*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
*  normF  denotes the  Frobenius norm of a matrix (square root of sum of
*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in ZLANHE as described
*          above.
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          hermitian matrix A is to be referenced.
*          = 'U':  Upper triangular part of A is referenced
*          = 'L':  Lower triangular part of A is referenced
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, ZLANHE is
*          set to zero.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The hermitian matrix A.  If UPLO = 'U', the leading n by n
*          upper triangular part of A contains the upper triangular part
*          of the matrix A, and the strictly lower triangular part of A
*          is not referenced.  If UPLO = 'L', the leading n by n lower
*          triangular part of A contains the lower triangular part of
*          the matrix A, and the strictly upper triangular part of A is
*          not referenced. Note that the imaginary parts of the diagonal
*          elements need not be set and are assumed to be zero.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(N,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
*          where LWORK >= N when NORM = 'I' or '1' or 'O'; otherwise,
*          WORK is not referenced.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   ABSA, SCALE, SUM, VALUE
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZLASSQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( N.EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 20 J = 1, N
               DO 10 I = 1, J - 1
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10          CONTINUE
               VALUE = MAX( VALUE, ABS( DBLE( A( J, J ) ) ) )
   20       CONTINUE
         ELSE
            DO 40 J = 1, N
               VALUE = MAX( VALUE, ABS( DBLE( A( J, J ) ) ) )
               DO 30 I = J + 1, N
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   30          CONTINUE
   40       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'I' ) ) .OR. ( LSAME( NORM, 'O' ) ) .OR.
     $         ( NORM.EQ.'1' ) ) THEN
*
*        Find normI(A) ( = norm1(A), since A is hermitian).
*
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 60 J = 1, N
               SUM = ZERO
               DO 50 I = 1, J - 1
                  ABSA = ABS( A( I, J ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
   50          CONTINUE
               WORK( J ) = SUM + ABS( DBLE( A( J, J ) ) )
   60       CONTINUE
            DO 70 I = 1, N
               VALUE = MAX( VALUE, WORK( I ) )
   70       CONTINUE
         ELSE
            DO 80 I = 1, N
               WORK( I ) = ZERO
   80       CONTINUE
            DO 100 J = 1, N
               SUM = WORK( J ) + ABS( DBLE( A( J, J ) ) )
               DO 90 I = J + 1, N
                  ABSA = ABS( A( I, J ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
   90          CONTINUE
               VALUE = MAX( VALUE, SUM )
  100       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 110 J = 2, N
               CALL ZLASSQ( J-1, A( 1, J ), 1, SCALE, SUM )
  110       CONTINUE
         ELSE
            DO 120 J = 1, N - 1
               CALL ZLASSQ( N-J, A( J+1, J ), 1, SCALE, SUM )
  120       CONTINUE
         END IF
         SUM = 2*SUM
         DO 130 I = 1, N
            IF( DBLE( A( I, I ) ).NE.ZERO ) THEN
               ABSA = ABS( DBLE( A( I, I ) ) )
               IF( SCALE.LT.ABSA ) THEN
                  SUM = ONE + SUM*( SCALE / ABSA )**2
                  SCALE = ABSA
               ELSE
                  SUM = SUM + ( ABSA / SCALE )**2
               END IF
            END IF
  130    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      ZLANHE = VALUE
      RETURN
*
*     End of ZLANHE
*
      END
      DOUBLE PRECISION FUNCTION ZLANHS( NORM, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   WORK( * )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLANHS  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  Hessenberg matrix A.
*
*  Description
*  ===========
*
*  ZLANHS returns the value
*
*     ZLANHS = ( max(abs(A(i,j))), NORM = 'M' or 'm'
*              (
*              ( norm1(A),         NORM = '1', 'O' or 'o'
*              (
*              ( normI(A),         NORM = 'I' or 'i'
*              (
*              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
*
*  where  norm1  denotes the  one norm of a matrix (maximum column sum),
*  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
*  normF  denotes the  Frobenius norm of a matrix (square root of sum of
*  squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in ZLANHS as described
*          above.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, ZLANHS is
*          set to zero.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The n by n upper Hessenberg matrix A; the part of A below the
*          first sub-diagonal is not referenced.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(N,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK),
*          where LWORK >= N when NORM = 'I'; otherwise, WORK is not
*          referenced.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   SCALE, SUM, VALUE
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZLASSQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( N.EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         VALUE = ZERO
         DO 20 J = 1, N
            DO 10 I = 1, MIN( N, J+1 )
               VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = 1, MIN( N, J+1 )
               SUM = SUM + ABS( A( I, J ) )
   30       CONTINUE
            VALUE = MAX( VALUE, SUM )
   40    CONTINUE
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         DO 50 I = 1, N
            WORK( I ) = ZERO
   50    CONTINUE
         DO 70 J = 1, N
            DO 60 I = 1, MIN( N, J+1 )
               WORK( I ) = WORK( I ) + ABS( A( I, J ) )
   60       CONTINUE
   70    CONTINUE
         VALUE = ZERO
         DO 80 I = 1, N
            VALUE = MAX( VALUE, WORK( I ) )
   80    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         DO 90 J = 1, N
            CALL ZLASSQ( MIN( N, J+1 ), A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      ZLANHS = VALUE
      RETURN
*
*     End of ZLANHS
*
      END
      SUBROUTINE ZLAQP2( M, N, OFFSET, A, LDA, JPVT, TAU, VN1, VN2,
     $                   WORK )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            LDA, M, N, OFFSET
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   VN1( * ), VN2( * )
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLAQP2 computes a QR factorization with column pivoting of
*  the block A(OFFSET+1:M,1:N).
*  The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A. N >= 0.
*
*  OFFSET  (input) INTEGER
*          The number of rows of the matrix A that must be pivoted
*          but no factorized. OFFSET >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the upper triangle of block A(OFFSET+1:M,1:N) is
*          the triangular factor obtained; the elements in block
*          A(OFFSET+1:M,1:N) below the diagonal, together with the
*          array TAU, represent the orthogonal matrix Q as a product of
*          elementary reflectors. Block A(1:OFFSET,1:N) has been
*          accordingly pivoted, but no factorized.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
*          to the front of A*P (a leading column); if JPVT(i) = 0,
*          the i-th column of A is a free column.
*          On exit, if JPVT(i) = k, then the i-th column of A*P
*          was the k-th column of A.
*
*  TAU     (output) COMPLEX*16 array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors.
*
*  VN1     (input/output) DOUBLE PRECISION array, dimension (N)
*          The vector with the partial column norms.
*
*  VN2     (input/output) DOUBLE PRECISION array, dimension (N)
*          The vector with the exact column norms.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (N)
*
*  Further Details
*  ===============
*
*  Based on contributions by
*    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*    X. Sun, Computer Science Dept., Duke University, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      COMPLEX*16         CONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0,
     $                   CONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ITEMP, J, MN, OFFPI, PVT
      DOUBLE PRECISION   TEMP, TEMP2
      COMPLEX*16         AII
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZLARF, ZLARFG, ZSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DCONJG, MAX, MIN, SQRT
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DZNRM2
      EXTERNAL           IDAMAX, DZNRM2
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M-OFFSET, N )
*
*     Compute factorization.
*
      DO 20 I = 1, MN
*
         OFFPI = OFFSET + I
*
*        Determine ith pivot column and swap if necessary.
*
         PVT = ( I-1 ) + IDAMAX( N-I+1, VN1( I ), 1 )
*
         IF( PVT.NE.I ) THEN
            CALL ZSWAP( M, A( 1, PVT ), 1, A( 1, I ), 1 )
            ITEMP = JPVT( PVT )
            JPVT( PVT ) = JPVT( I )
            JPVT( I ) = ITEMP
            VN1( PVT ) = VN1( I )
            VN2( PVT ) = VN2( I )
         END IF
*
*        Generate elementary reflector H(i).
*
         IF( OFFPI.LT.M ) THEN
            CALL ZLARFG( M-OFFPI+1, A( OFFPI, I ), A( OFFPI+1, I ), 1,
     $                   TAU( I ) )
         ELSE
            CALL ZLARFG( 1, A( M, I ), A( M, I ), 1, TAU( I ) )
         END IF
*
         IF( I.LT.N ) THEN
*
*           Apply H(i)' to A(offset+i:m,i+1:n) from the left.
*
            AII = A( OFFPI, I )
            A( OFFPI, I ) = CONE
            CALL ZLARF( 'Left', M-OFFPI+1, N-I, A( OFFPI, I ), 1,
     $                  DCONJG( TAU( I ) ), A( OFFPI, I+1 ), LDA,
     $                  WORK( 1 ) )
            A( OFFPI, I ) = AII
         END IF
*
*        Update partial column norms.
*
         DO 10 J = I + 1, N
            IF( VN1( J ).NE.ZERO ) THEN
               TEMP = ONE - ( ABS( A( OFFPI, J ) ) / VN1( J ) )**2
               TEMP = MAX( TEMP, ZERO )
               TEMP2 = ONE + 0.05D0*TEMP*( VN1( J ) / VN2( J ) )**2
               IF( TEMP2.EQ.ONE ) THEN
                  IF( OFFPI.LT.M ) THEN
                     VN1( J ) = DZNRM2( M-OFFPI, A( OFFPI+1, J ), 1 )
                     VN2( J ) = VN1( J )
                  ELSE
                     VN1( J ) = ZERO
                     VN2( J ) = ZERO
                  END IF
               ELSE
                  VN1( J ) = VN1( J )*SQRT( TEMP )
               END IF
            END IF
   10    CONTINUE
*
   20 CONTINUE
*
      RETURN
*
*     End of ZLAQP2
*
      END
      SUBROUTINE ZLAQPS( M, N, OFFSET, NB, KB, A, LDA, JPVT, TAU, VN1,
     $                   VN2, AUXV, F, LDF )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            KB, LDA, LDF, M, N, NB, OFFSET
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   VN1( * ), VN2( * )
      COMPLEX*16         A( LDA, * ), AUXV( * ), F( LDF, * ), TAU( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLAQPS computes a step of QR factorization with column pivoting
*  of a complex M-by-N matrix A by using Blas-3.  It tries to factorize
*  NB columns from A starting from the row OFFSET+1, and updates all
*  of the matrix with Blas-3 xGEMM.
*
*  In some cases, due to catastrophic cancellations, it cannot
*  factorize NB columns.  Hence, the actual number of factorized
*  columns is returned in KB.
*
*  Block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A. N >= 0
*
*  OFFSET  (input) INTEGER
*          The number of rows of A that have been factorized in
*          previous steps.
*
*  NB      (input) INTEGER
*          The number of columns to factorize.
*
*  KB      (output) INTEGER
*          The number of columns actually factorized.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, block A(OFFSET+1:M,1:KB) is the triangular
*          factor obtained and block A(1:OFFSET,1:N) has been
*          accordingly pivoted, but no factorized.
*          The rest of the matrix, block A(OFFSET+1:M,KB+1:N) has
*          been updated.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          JPVT(I) = K <==> Column K of the full matrix A has been
*          permuted into position I in AP.
*
*  TAU     (output) COMPLEX*16 array, dimension (KB)
*          The scalar factors of the elementary reflectors.
*
*  VN1     (input/output) DOUBLE PRECISION array, dimension (N)
*          The vector with the partial column norms.
*
*  VN2     (input/output) DOUBLE PRECISION array, dimension (N)
*          The vector with the exact column norms.
*
*  AUXV    (input/output) COMPLEX*16 array, dimension (NB)
*          Auxiliar vector.
*
*  F       (input/output) COMPLEX*16 array, dimension (LDF,NB)
*          Matrix F' = L*Y'*A.
*
*  LDF     (input) INTEGER
*          The leading dimension of the array F. LDF >= max(1,N).
*
*  Further Details
*  ===============
*
*  Based on contributions by
*    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*    X. Sun, Computer Science Dept., Duke University, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0,
     $                   CZERO = ( 0.0D+0, 0.0D+0 ),
     $                   CONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            ITEMP, J, K, LASTRK, LSTICC, PVT, RK
      DOUBLE PRECISION   TEMP, TEMP2
      COMPLEX*16         AKK
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZGEMM, ZGEMV, ZLARFG, ZSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCONJG, MAX, MIN, NINT, SQRT
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DZNRM2
      EXTERNAL           IDAMAX, DZNRM2
*     ..
*     .. Executable Statements ..
*
      LASTRK = MIN( M, N+OFFSET )
      LSTICC = 0
      K = 0
*
*     Beginning of while loop.
*
   10 CONTINUE
      IF( ( K.LT.NB ) .AND. ( LSTICC.EQ.0 ) ) THEN
         K = K + 1
         RK = OFFSET + K
*
*        Determine ith pivot column and swap if necessary
*
         PVT = ( K-1 ) + IDAMAX( N-K+1, VN1( K ), 1 )
         IF( PVT.NE.K ) THEN
            CALL ZSWAP( M, A( 1, PVT ), 1, A( 1, K ), 1 )
            CALL ZSWAP( K-1, F( PVT, 1 ), LDF, F( K, 1 ), LDF )
            ITEMP = JPVT( PVT )
            JPVT( PVT ) = JPVT( K )
            JPVT( K ) = ITEMP
            VN1( PVT ) = VN1( K )
            VN2( PVT ) = VN2( K )
         END IF
*
*        Apply previous Householder reflectors to column K:
*        A(RK:M,K) := A(RK:M,K) - A(RK:M,1:K-1)*F(K,1:K-1)'.
*
         IF( K.GT.1 ) THEN
*CC            CALL ZGEMM( 'No transpose', 'Conjugate transpose',
*CC     $                  M-RK+1, 1, K-1, -CONE, A( RK, 1 ), LDA,
*CC     $                  F( K, 1 ), LDF, CONE, A( RK, K ), LDA )
            DO 20 J = 1, K - 1
               F( K, J ) = DCONJG( F( K, J ) )
   20       CONTINUE
            CALL ZGEMV( 'No transpose', M-RK+1, K-1, -CONE, A( RK, 1 ),
     $                  LDA, F( K, 1 ), LDF, CONE, A( RK, K ), 1 )
            DO 30 J = 1, K - 1
               F( K, J ) = DCONJG( F( K, J ) )
   30       CONTINUE
         END IF
*
*        Generate elementary reflector H(k).
*
         IF( RK.LT.M ) THEN
            CALL ZLARFG( M-RK+1, A( RK, K ), A( RK+1, K ), 1, TAU( K ) )
         ELSE
            CALL ZLARFG( 1, A( RK, K ), A( RK, K ), 1, TAU( K ) )
         END IF
*
         AKK = A( RK, K )
         A( RK, K ) = CONE
*
*        Compute Kth column of F:
*
*        Compute  F(K+1:N,K) := tau(K)*A(RK:M,K+1:N)'*A(RK:M,K).
*
         IF( K.LT.N ) THEN
            CALL ZGEMV( 'Conjugate transpose', M-RK+1, N-K, TAU( K ),
     $                  A( RK, K+1 ), LDA, A( RK, K ), 1, CZERO,
     $                  F( K+1, K ), 1 )
         END IF
*
*        Padding F(1:K,K) with zeros.
*
         DO 40 J = 1, K
            F( J, K ) = CZERO
   40    CONTINUE
*
*        Incremental updating of F:
*        F(1:N,K) := F(1:N,K) - tau(K)*F(1:N,1:K-1)*A(RK:M,1:K-1)'
*                    *A(RK:M,K).
*
         IF( K.GT.1 ) THEN
            CALL ZGEMV( 'Conjugate transpose', M-RK+1, K-1, -TAU( K ),
     $                  A( RK, 1 ), LDA, A( RK, K ), 1, CZERO,
     $                  AUXV( 1 ), 1 )
*
            CALL ZGEMV( 'No transpose', N, K-1, CONE, F( 1, 1 ), LDF,
     $                  AUXV( 1 ), 1, CONE, F( 1, K ), 1 )
         END IF
*
*        Update the current row of A:
*        A(RK,K+1:N) := A(RK,K+1:N) - A(RK,1:K)*F(K+1:N,1:K)'.
*
         IF( K.LT.N ) THEN
            CALL ZGEMM( 'No transpose', 'Conjugate transpose', 1, N-K,
     $                  K, -CONE, A( RK, 1 ), LDA, F( K+1, 1 ), LDF,
     $                  CONE, A( RK, K+1 ), LDA )
         END IF
*
*        Update partial column norms.
*
         IF( RK.LT.LASTRK ) THEN
            DO 50 J = K + 1, N
               IF( VN1( J ).NE.ZERO ) THEN
                  TEMP = ABS( A( RK, J ) ) / VN1( J )
                  TEMP = MAX( ZERO, ( ONE+TEMP )*( ONE-TEMP ) )
                  TEMP2 = ONE + 0.05D0*TEMP*( VN1( J ) / VN2( J ) )**2
                  IF( TEMP2.EQ.ONE ) THEN
                     VN2( J ) = DBLE( LSTICC )
                     LSTICC = J
                  ELSE
                     VN1( J ) = VN1( J )*SQRT( TEMP )
                  END IF
               END IF
   50       CONTINUE
         END IF
*
         A( RK, K ) = AKK
*
*        End of while loop.
*
         GO TO 10
      END IF
      KB = K
      RK = OFFSET + KB
*
*     Apply the block reflector to the rest of the matrix:
*     A(OFFSET+KB+1:M,KB+1:N) := A(OFFSET+KB+1:M,KB+1:N) -
*                         A(OFFSET+KB+1:M,1:KB)*F(KB+1:N,1:KB)'.
*
      IF( KB.LT.MIN( N, M-OFFSET ) ) THEN
         CALL ZGEMM( 'No transpose', 'Conjugate transpose', M-RK, N-KB,
     $               KB, -CONE, A( RK+1, 1 ), LDA, F( KB+1, 1 ), LDF,
     $               CONE, A( RK+1, KB+1 ), LDA )
      END IF
*
*     Recomputation of difficult columns.
*
   60 CONTINUE
      IF( LSTICC.GT.0 ) THEN
         ITEMP = NINT( VN2( LSTICC ) )
         VN1( LSTICC ) = DZNRM2( M-RK, A( RK+1, LSTICC ), 1 )
         VN2( LSTICC ) = VN1( LSTICC )
         LSTICC = ITEMP
         GO TO 60
      END IF
*
      RETURN
*
*     End of ZLAQPS
*
      END
      SUBROUTINE ZLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      COMPLEX*16         TAU
*     ..
*     .. Array Arguments ..
      COMPLEX*16         C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLARF applies a complex elementary reflector H to a complex M-by-N
*  matrix C, from either the left or the right. H is represented in the
*  form
*
*        H = I - tau * v * v'
*
*  where tau is a complex scalar and v is a complex vector.
*
*  If tau = 0, then H is taken to be the unit matrix.
*
*  To apply H' (the conjugate transpose of H), supply conjg(tau) instead
*  tau.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': form  H * C
*          = 'R': form  C * H
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  V       (input) COMPLEX*16 array, dimension
*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
*          The vector v in the representation of H. V is not used if
*          TAU = 0.
*
*  INCV    (input) INTEGER
*          The increment between elements of v. INCV <> 0.
*
*  TAU     (input) COMPLEX*16
*          The value tau in the representation of H.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*          or C * H if SIDE = 'R'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) COMPLEX*16 array, dimension
*                         (N) if SIDE = 'L'
*                      or (M) if SIDE = 'R'
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZGEMV, ZGERC
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  H * C
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C' * v
*
            CALL ZGEMV( 'Conjugate transpose', M, N, ONE, C, LDC, V,
     $                  INCV, ZERO, WORK, 1 )
*
*           C := C - v * w'
*
            CALL ZGERC( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
*
*        Form  C * H
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C * v
*
            CALL ZGEMV( 'No transpose', M, N, ONE, C, LDC, V, INCV,
     $                  ZERO, WORK, 1 )
*
*           C := C - w * v'
*
            CALL ZGERC( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
      RETURN
*
*     End of ZLARF
*
      END
      SUBROUTINE ZLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
     $                   T, LDT, C, LDC, WORK, LDWORK )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLARFB applies a complex block reflector H or its transpose H' to a
*  complex M-by-N matrix C, from either the left or the right.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply H or H' from the Left
*          = 'R': apply H or H' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply H (No transpose)
*          = 'C': apply H' (Conjugate transpose)
*
*  DIRECT  (input) CHARACTER*1
*          Indicates how H is formed from a product of elementary
*          reflectors
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Indicates how the vectors which define the elementary
*          reflectors are stored:
*          = 'C': Columnwise
*          = 'R': Rowwise
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  K       (input) INTEGER
*          The order of the matrix T (= the number of elementary
*          reflectors whose product defines the block reflector).
*
*  V       (input) COMPLEX*16 array, dimension
*                                (LDV,K) if STOREV = 'C'
*                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
*                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V.
*          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
*          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
*          if STOREV = 'R', LDV >= K.
*
*  T       (input) COMPLEX*16 array, dimension (LDT,K)
*          The triangular K-by-K matrix T in the representation of the
*          block reflector.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= K.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) COMPLEX*16 array, dimension (LDWORK,K)
*
*  LDWORK  (input) INTEGER
*          The leading dimension of the array WORK.
*          If SIDE = 'L', LDWORK >= max(1,N);
*          if SIDE = 'R', LDWORK >= max(1,M).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANST
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZCOPY, ZGEMM, ZLACGV, ZTRMM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'C'
      ELSE
         TRANST = 'N'
      END IF
*
      IF( LSAME( STOREV, 'C' ) ) THEN
*
         IF( LSAME( DIRECT, 'F' ) ) THEN
*
*           Let  V =  ( V1 )    (first K rows)
*                     ( V2 )
*           where  V1  is unit lower triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
*
*              W := C1'
*
               DO 10 J = 1, K
                  CALL ZCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
                  CALL ZLACGV( N, WORK( 1, J ), 1 )
   10          CONTINUE
*
*              W := W * V1
*
               CALL ZTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C2'*V2
*
                  CALL ZGEMM( 'Conjugate transpose', 'No transpose', N,
     $                        K, M-K, ONE, C( K+1, 1 ), LDC,
     $                        V( K+1, 1 ), LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL ZTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               IF( M.GT.K ) THEN
*
*                 C2 := C2 - V2 * W'
*
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose',
     $                        M-K, N, K, -ONE, V( K+1, 1 ), LDV, WORK,
     $                        LDWORK, ONE, C( K+1, 1 ), LDC )
               END IF
*
*              W := W * V1'
*
               CALL ZTRMM( 'Right', 'Lower', 'Conjugate transpose',
     $                     'Unit', N, K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 30 J = 1, K
                  DO 20 I = 1, N
                     C( J, I ) = C( J, I ) - DCONJG( WORK( I, J ) )
   20             CONTINUE
   30          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
*
*              W := C1
*
               DO 40 J = 1, K
                  CALL ZCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          CONTINUE
*
*              W := W * V1
*
               CALL ZTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C2 * V2
*
                  CALL ZGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL ZTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               IF( N.GT.K ) THEN
*
*                 C2 := C2 - W * V2'
*
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose', M,
     $                        N-K, K, -ONE, WORK, LDWORK, V( K+1, 1 ),
     $                        LDV, ONE, C( 1, K+1 ), LDC )
               END IF
*
*              W := W * V1'
*
               CALL ZTRMM( 'Right', 'Lower', 'Conjugate transpose',
     $                     'Unit', M, K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 60 J = 1, K
                  DO 50 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
   50             CONTINUE
   60          CONTINUE
            END IF
*
         ELSE
*
*           Let  V =  ( V1 )
*                     ( V2 )    (last K rows)
*           where  V2  is unit upper triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
*
*              W := C2'
*
               DO 70 J = 1, K
                  CALL ZCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
                  CALL ZLACGV( N, WORK( 1, J ), 1 )
   70          CONTINUE
*
*              W := W * V2
*
               CALL ZTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C1'*V1
*
                  CALL ZGEMM( 'Conjugate transpose', 'No transpose', N,
     $                        K, M-K, ONE, C, LDC, V, LDV, ONE, WORK,
     $                        LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL ZTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               IF( M.GT.K ) THEN
*
*                 C1 := C1 - V1 * W'
*
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose',
     $                        M-K, N, K, -ONE, V, LDV, WORK, LDWORK,
     $                        ONE, C, LDC )
               END IF
*
*              W := W * V2'
*
               CALL ZTRMM( 'Right', 'Upper', 'Conjugate transpose',
     $                     'Unit', N, K, ONE, V( M-K+1, 1 ), LDV, WORK,
     $                     LDWORK )
*
*              C2 := C2 - W'
*
               DO 90 J = 1, K
                  DO 80 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) -
     $                               DCONJG( WORK( I, J ) )
   80             CONTINUE
   90          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
*
*              W := C2
*
               DO 100 J = 1, K
                  CALL ZCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  100          CONTINUE
*
*              W := W * V2
*
               CALL ZTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C1 * V1
*
                  CALL ZGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL ZTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               IF( N.GT.K ) THEN
*
*                 C1 := C1 - W * V1'
*
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose', M,
     $                        N-K, K, -ONE, WORK, LDWORK, V, LDV, ONE,
     $                        C, LDC )
               END IF
*
*              W := W * V2'
*
               CALL ZTRMM( 'Right', 'Upper', 'Conjugate transpose',
     $                     'Unit', M, K, ONE, V( N-K+1, 1 ), LDV, WORK,
     $                     LDWORK )
*
*              C2 := C2 - W
*
               DO 120 J = 1, K
                  DO 110 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  110             CONTINUE
  120          CONTINUE
            END IF
         END IF
*
      ELSE IF( LSAME( STOREV, 'R' ) ) THEN
*
         IF( LSAME( DIRECT, 'F' ) ) THEN
*
*           Let  V =  ( V1  V2 )    (V1: first K columns)
*           where  V1  is unit upper triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
*
*              W := C1'
*
               DO 130 J = 1, K
                  CALL ZCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
                  CALL ZLACGV( N, WORK( 1, J ), 1 )
  130          CONTINUE
*
*              W := W * V1'
*
               CALL ZTRMM( 'Right', 'Upper', 'Conjugate transpose',
     $                     'Unit', N, K, ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C2'*V2'
*
                  CALL ZGEMM( 'Conjugate transpose',
     $                        'Conjugate transpose', N, K, M-K, ONE,
     $                        C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE,
     $                        WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL ZTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               IF( M.GT.K ) THEN
*
*                 C2 := C2 - V2' * W'
*
                  CALL ZGEMM( 'Conjugate transpose',
     $                        'Conjugate transpose', M-K, N, K, -ONE,
     $                        V( 1, K+1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
*
*              W := W * V1
*
               CALL ZTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 150 J = 1, K
                  DO 140 I = 1, N
                     C( J, I ) = C( J, I ) - DCONJG( WORK( I, J ) )
  140             CONTINUE
  150          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
*
*              W := C1
*
               DO 160 J = 1, K
                  CALL ZCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          CONTINUE
*
*              W := W * V1'
*
               CALL ZTRMM( 'Right', 'Upper', 'Conjugate transpose',
     $                     'Unit', M, K, ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C2 * V2'
*
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose', M,
     $                        K, N-K, ONE, C( 1, K+1 ), LDC,
     $                        V( 1, K+1 ), LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL ZTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               IF( N.GT.K ) THEN
*
*                 C2 := C2 - W * V2
*
                  CALL ZGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
*
*              W := W * V1
*
               CALL ZTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 180 J = 1, K
                  DO 170 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
  170             CONTINUE
  180          CONTINUE
*
            END IF
*
         ELSE
*
*           Let  V =  ( V1  V2 )    (V2: last K columns)
*           where  V2  is unit lower triangular.
*
            IF( LSAME( SIDE, 'L' ) ) THEN
*
*              Form  H * C  or  H' * C  where  C = ( C1 )
*                                                  ( C2 )
*
*              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
*
*              W := C2'
*
               DO 190 J = 1, K
                  CALL ZCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
                  CALL ZLACGV( N, WORK( 1, J ), 1 )
  190          CONTINUE
*
*              W := W * V2'
*
               CALL ZTRMM( 'Right', 'Lower', 'Conjugate transpose',
     $                     'Unit', N, K, ONE, V( 1, M-K+1 ), LDV, WORK,
     $                     LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C1'*V1'
*
                  CALL ZGEMM( 'Conjugate transpose',
     $                        'Conjugate transpose', N, K, M-K, ONE, C,
     $                        LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL ZTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               IF( M.GT.K ) THEN
*
*                 C1 := C1 - V1' * W'
*
                  CALL ZGEMM( 'Conjugate transpose',
     $                        'Conjugate transpose', M-K, N, K, -ONE, V,
     $                        LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
*
*              W := W * V2
*
               CALL ZTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 210 J = 1, K
                  DO 200 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) -
     $                               DCONJG( WORK( I, J ) )
  200             CONTINUE
  210          CONTINUE
*
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*              Form  C * H  or  C * H'  where  C = ( C1  C2 )
*
*              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
*
*              W := C2
*
               DO 220 J = 1, K
                  CALL ZCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  220          CONTINUE
*
*              W := W * V2'
*
               CALL ZTRMM( 'Right', 'Lower', 'Conjugate transpose',
     $                     'Unit', M, K, ONE, V( 1, N-K+1 ), LDV, WORK,
     $                     LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C1 * V1'
*
                  CALL ZGEMM( 'No transpose', 'Conjugate transpose', M,
     $                        K, N-K, ONE, C, LDC, V, LDV, ONE, WORK,
     $                        LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL ZTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               IF( N.GT.K ) THEN
*
*                 C1 := C1 - W * V1
*
                  CALL ZGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
*
*              W := W * V2
*
               CALL ZTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
*
*              C1 := C1 - W
*
               DO 240 J = 1, K
                  DO 230 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  230             CONTINUE
  240          CONTINUE
*
            END IF
*
         END IF
      END IF
*
      RETURN
*
*     End of ZLARFB
*
      END
      SUBROUTINE ZLARFG( N, ALPHA, X, INCX, TAU )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      COMPLEX*16         ALPHA, TAU
*     ..
*     .. Array Arguments ..
      COMPLEX*16         X( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLARFG generates a complex elementary reflector H of order n, such
*  that
*
*        H' * ( alpha ) = ( beta ),   H' * H = I.
*             (   x   )   (   0  )
*
*  where alpha and beta are scalars, with beta real, and x is an
*  (n-1)-element complex vector. H is represented in the form
*
*        H = I - tau * ( 1 ) * ( 1 v' ) ,
*                      ( v )
*
*  where tau is a complex scalar and v is a complex (n-1)-element
*  vector. Note that H is not hermitian.
*
*  If the elements of x are all zero and alpha is real, then tau = 0
*  and H is taken to be the unit matrix.
*
*  Otherwise  1 <= real(tau) <= 2  and  abs(tau-1) <= 1 .
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the elementary reflector.
*
*  ALPHA   (input/output) COMPLEX*16
*          On entry, the value alpha.
*          On exit, it is overwritten with the value beta.
*
*  X       (input/output) COMPLEX*16 array, dimension
*                         (1+(N-2)*abs(INCX))
*          On entry, the vector x.
*          On exit, it is overwritten with the vector v.
*
*  INCX    (input) INTEGER
*          The increment between elements of X. INCX > 0.
*
*  TAU     (output) COMPLEX*16
*          The value tau.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J, KNT
      DOUBLE PRECISION   ALPHI, ALPHR, BETA, RSAFMN, SAFMIN, XNORM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY3, DZNRM2
      COMPLEX*16         ZLADIV
      EXTERNAL           DLAMCH, DLAPY3, DZNRM2, ZLADIV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DIMAG, SIGN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZDSCAL, ZSCAL
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.0 ) THEN
         TAU = ZERO
         RETURN
      END IF
*
      XNORM = DZNRM2( N-1, X, INCX )
      ALPHR = DBLE( ALPHA )
      ALPHI = DIMAG( ALPHA )
*
      IF( XNORM.EQ.ZERO .AND. ALPHI.EQ.ZERO ) THEN
*
*        H  =  I
*
         TAU = ZERO
      ELSE
*
*        general case
*
         BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
         RSAFMN = ONE / SAFMIN
*
         IF( ABS( BETA ).LT.SAFMIN ) THEN
*
*           XNORM, BETA may be inaccurate; scale X and recompute them
*
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL ZDSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHI = ALPHI*RSAFMN
            ALPHR = ALPHR*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     $         GO TO 10
*
*           New BETA is at most 1, at least SAFMIN
*
            XNORM = DZNRM2( N-1, X, INCX )
            ALPHA = DCMPLX( ALPHR, ALPHI )
            BETA = -SIGN( DLAPY3( ALPHR, ALPHI, XNORM ), ALPHR )
            TAU = DCMPLX( ( BETA-ALPHR ) / BETA, -ALPHI / BETA )
            ALPHA = ZLADIV( DCMPLX( ONE ), ALPHA-BETA )
            CALL ZSCAL( N-1, ALPHA, X, INCX )
*
*           If ALPHA is subnormal, it may lose relative accuracy
*
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = DCMPLX( ( BETA-ALPHR ) / BETA, -ALPHI / BETA )
            ALPHA = ZLADIV( DCMPLX( ONE ), ALPHA-BETA )
            CALL ZSCAL( N-1, ALPHA, X, INCX )
            ALPHA = BETA
         END IF
      END IF
*
      RETURN
*
*     End of ZLARFG
*
      END
      SUBROUTINE ZLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         T( LDT, * ), TAU( * ), V( LDV, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLARFT forms the triangular factor T of a complex block reflector H
*  of order n, which is defined as a product of k elementary reflectors.
*
*  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
*
*  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
*
*  If STOREV = 'C', the vector which defines the elementary reflector
*  H(i) is stored in the i-th column of the array V, and
*
*     H  =  I - V * T * V'
*
*  If STOREV = 'R', the vector which defines the elementary reflector
*  H(i) is stored in the i-th row of the array V, and
*
*     H  =  I - V' * T * V
*
*  Arguments
*  =========
*
*  DIRECT  (input) CHARACTER*1
*          Specifies the order in which the elementary reflectors are
*          multiplied to form the block reflector:
*          = 'F': H = H(1) H(2) . . . H(k) (Forward)
*          = 'B': H = H(k) . . . H(2) H(1) (Backward)
*
*  STOREV  (input) CHARACTER*1
*          Specifies how the vectors which define the elementary
*          reflectors are stored (see also Further Details):
*          = 'C': columnwise
*          = 'R': rowwise
*
*  N       (input) INTEGER
*          The order of the block reflector H. N >= 0.
*
*  K       (input) INTEGER
*          The order of the triangular factor T (= the number of
*          elementary reflectors). K >= 1.
*
*  V       (input/output) COMPLEX*16 array, dimension
*                               (LDV,K) if STOREV = 'C'
*                               (LDV,N) if STOREV = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V.
*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i).
*
*  T       (output) COMPLEX*16 array, dimension (LDT,K)
*          The k by k triangular factor T of the block reflector.
*          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
*          lower triangular. The rest of the array is not used.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= K.
*
*  Further Details
*  ===============
*
*  The shape of the matrix V and the storage of the vectors which define
*  the H(i) is best illustrated by the following example with n = 5 and
*  k = 3. The elements equal to 1 are not stored; the corresponding
*  array elements are modified but restored on exit. The rest of the
*  array is not used.
*
*  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
*
*               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
*                   ( v1  1    )                     (     1 v2 v2 v2 )
*                   ( v1 v2  1 )                     (        1 v3 v3 )
*                   ( v1 v2 v3 )
*                   ( v1 v2 v3 )
*
*  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
*
*               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
*                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
*                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
*                   (     1 v3 )
*                   (        1 )
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      COMPLEX*16         VII
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZGEMV, ZLACGV, ZTRMV
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( LSAME( DIRECT, 'F' ) ) THEN
         DO 20 I = 1, K
            IF( TAU( I ).EQ.ZERO ) THEN
*
*              H(i)  =  I
*
               DO 10 J = 1, I
                  T( J, I ) = ZERO
   10          CONTINUE
            ELSE
*
*              general case
*
               VII = V( I, I )
               V( I, I ) = ONE
               IF( LSAME( STOREV, 'C' ) ) THEN
*
*                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i)
*
                  CALL ZGEMV( 'Conjugate transpose', N-I+1, I-1,
     $                        -TAU( I ), V( I, 1 ), LDV, V( I, I ), 1,
     $                        ZERO, T( 1, I ), 1 )
               ELSE
*
*                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'
*
                  IF( I.LT.N )
     $               CALL ZLACGV( N-I, V( I, I+1 ), LDV )
                  CALL ZGEMV( 'No transpose', I-1, N-I+1, -TAU( I ),
     $                        V( 1, I ), LDV, V( I, I ), LDV, ZERO,
     $                        T( 1, I ), 1 )
                  IF( I.LT.N )
     $               CALL ZLACGV( N-I, V( I, I+1 ), LDV )
               END IF
               V( I, I ) = VII
*
*              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
*
               CALL ZTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T,
     $                     LDT, T( 1, I ), 1 )
               T( I, I ) = TAU( I )
            END IF
   20    CONTINUE
      ELSE
         DO 40 I = K, 1, -1
            IF( TAU( I ).EQ.ZERO ) THEN
*
*              H(i)  =  I
*
               DO 30 J = I, K
                  T( J, I ) = ZERO
   30          CONTINUE
            ELSE
*
*              general case
*
               IF( I.LT.K ) THEN
                  IF( LSAME( STOREV, 'C' ) ) THEN
                     VII = V( N-K+I, I )
                     V( N-K+I, I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i)
*
                     CALL ZGEMV( 'Conjugate transpose', N-K+I, K-I,
     $                           -TAU( I ), V( 1, I+1 ), LDV, V( 1, I ),
     $                           1, ZERO, T( I+1, I ), 1 )
                     V( N-K+I, I ) = VII
                  ELSE
                     VII = V( I, N-K+I )
                     V( I, N-K+I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'
*
                     CALL ZLACGV( N-K+I-1, V( I, 1 ), LDV )
                     CALL ZGEMV( 'No transpose', K-I, N-K+I, -TAU( I ),
     $                           V( I+1, 1 ), LDV, V( I, 1 ), LDV, ZERO,
     $                           T( I+1, I ), 1 )
                     CALL ZLACGV( N-K+I-1, V( I, 1 ), LDV )
                     V( I, N-K+I ) = VII
                  END IF
*
*                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
*
                  CALL ZTRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                        T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
               END IF
               T( I, I ) = TAU( I )
            END IF
   40    CONTINUE
      END IF
      RETURN
*
*     End of ZLARFT
*
      END
      SUBROUTINE ZLARFX( SIDE, M, N, V, TAU, C, LDC, WORK )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            LDC, M, N
      COMPLEX*16         TAU
*     ..
*     .. Array Arguments ..
      COMPLEX*16         C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLARFX applies a complex elementary reflector H to a complex m by n
*  matrix C, from either the left or the right. H is represented in the
*  form
*
*        H = I - tau * v * v'
*
*  where tau is a complex scalar and v is a complex vector.
*
*  If tau = 0, then H is taken to be the unit matrix
*
*  This version uses inline code if H has order < 11.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': form  H * C
*          = 'R': form  C * H
*
*  M       (input) INTEGER
*          The number of rows of the matrix C.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C.
*
*  V       (input) COMPLEX*16 array, dimension (M) if SIDE = 'L'
*                                        or (N) if SIDE = 'R'
*          The vector v in the representation of H.
*
*  TAU     (input) COMPLEX*16
*          The value tau in the representation of H.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*          or C * H if SIDE = 'R'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDA >= max(1,M).
*
*  WORK    (workspace) COMPLEX*16 array, dimension (N) if SIDE = 'L'
*                                            or (M) if SIDE = 'R'
*          WORK is not referenced if H has order < 11.
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            J
      COMPLEX*16         SUM, T1, T10, T2, T3, T4, T5, T6, T7, T8, T9,
     $                   V1, V10, V2, V3, V4, V5, V6, V7, V8, V9
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZGEMV, ZGERC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG
*     ..
*     .. Executable Statements ..
*
      IF( TAU.EQ.ZERO )
     $   RETURN
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  H * C, where H has order m.
*
         GO TO ( 10, 30, 50, 70, 90, 110, 130, 150,
     $           170, 190 )M
*
*        Code for general M
*
*        w := C'*v
*
         CALL ZGEMV( 'Conjugate transpose', M, N, ONE, C, LDC, V, 1,
     $               ZERO, WORK, 1 )
*
*        C := C - tau * v * w'
*
         CALL ZGERC( M, N, -TAU, V, 1, WORK, 1, C, LDC )
         GO TO 410
   10    CONTINUE
*
*        Special code for 1 x 1 Householder
*
         T1 = ONE - TAU*V( 1 )*DCONJG( V( 1 ) )
         DO 20 J = 1, N
            C( 1, J ) = T1*C( 1, J )
   20    CONTINUE
         GO TO 410
   30    CONTINUE
*
*        Special code for 2 x 2 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         DO 40 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
   40    CONTINUE
         GO TO 410
   50    CONTINUE
*
*        Special code for 3 x 3 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         V3 = DCONJG( V( 3 ) )
         T3 = TAU*DCONJG( V3 )
         DO 60 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
            C( 3, J ) = C( 3, J ) - SUM*T3
   60    CONTINUE
         GO TO 410
   70    CONTINUE
*
*        Special code for 4 x 4 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         V3 = DCONJG( V( 3 ) )
         T3 = TAU*DCONJG( V3 )
         V4 = DCONJG( V( 4 ) )
         T4 = TAU*DCONJG( V4 )
         DO 80 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J ) +
     $            V4*C( 4, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
            C( 3, J ) = C( 3, J ) - SUM*T3
            C( 4, J ) = C( 4, J ) - SUM*T4
   80    CONTINUE
         GO TO 410
   90    CONTINUE
*
*        Special code for 5 x 5 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         V3 = DCONJG( V( 3 ) )
         T3 = TAU*DCONJG( V3 )
         V4 = DCONJG( V( 4 ) )
         T4 = TAU*DCONJG( V4 )
         V5 = DCONJG( V( 5 ) )
         T5 = TAU*DCONJG( V5 )
         DO 100 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J ) +
     $            V4*C( 4, J ) + V5*C( 5, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
            C( 3, J ) = C( 3, J ) - SUM*T3
            C( 4, J ) = C( 4, J ) - SUM*T4
            C( 5, J ) = C( 5, J ) - SUM*T5
  100    CONTINUE
         GO TO 410
  110    CONTINUE
*
*        Special code for 6 x 6 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         V3 = DCONJG( V( 3 ) )
         T3 = TAU*DCONJG( V3 )
         V4 = DCONJG( V( 4 ) )
         T4 = TAU*DCONJG( V4 )
         V5 = DCONJG( V( 5 ) )
         T5 = TAU*DCONJG( V5 )
         V6 = DCONJG( V( 6 ) )
         T6 = TAU*DCONJG( V6 )
         DO 120 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J ) +
     $            V4*C( 4, J ) + V5*C( 5, J ) + V6*C( 6, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
            C( 3, J ) = C( 3, J ) - SUM*T3
            C( 4, J ) = C( 4, J ) - SUM*T4
            C( 5, J ) = C( 5, J ) - SUM*T5
            C( 6, J ) = C( 6, J ) - SUM*T6
  120    CONTINUE
         GO TO 410
  130    CONTINUE
*
*        Special code for 7 x 7 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         V3 = DCONJG( V( 3 ) )
         T3 = TAU*DCONJG( V3 )
         V4 = DCONJG( V( 4 ) )
         T4 = TAU*DCONJG( V4 )
         V5 = DCONJG( V( 5 ) )
         T5 = TAU*DCONJG( V5 )
         V6 = DCONJG( V( 6 ) )
         T6 = TAU*DCONJG( V6 )
         V7 = DCONJG( V( 7 ) )
         T7 = TAU*DCONJG( V7 )
         DO 140 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J ) +
     $            V4*C( 4, J ) + V5*C( 5, J ) + V6*C( 6, J ) +
     $            V7*C( 7, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
            C( 3, J ) = C( 3, J ) - SUM*T3
            C( 4, J ) = C( 4, J ) - SUM*T4
            C( 5, J ) = C( 5, J ) - SUM*T5
            C( 6, J ) = C( 6, J ) - SUM*T6
            C( 7, J ) = C( 7, J ) - SUM*T7
  140    CONTINUE
         GO TO 410
  150    CONTINUE
*
*        Special code for 8 x 8 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         V3 = DCONJG( V( 3 ) )
         T3 = TAU*DCONJG( V3 )
         V4 = DCONJG( V( 4 ) )
         T4 = TAU*DCONJG( V4 )
         V5 = DCONJG( V( 5 ) )
         T5 = TAU*DCONJG( V5 )
         V6 = DCONJG( V( 6 ) )
         T6 = TAU*DCONJG( V6 )
         V7 = DCONJG( V( 7 ) )
         T7 = TAU*DCONJG( V7 )
         V8 = DCONJG( V( 8 ) )
         T8 = TAU*DCONJG( V8 )
         DO 160 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J ) +
     $            V4*C( 4, J ) + V5*C( 5, J ) + V6*C( 6, J ) +
     $            V7*C( 7, J ) + V8*C( 8, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
            C( 3, J ) = C( 3, J ) - SUM*T3
            C( 4, J ) = C( 4, J ) - SUM*T4
            C( 5, J ) = C( 5, J ) - SUM*T5
            C( 6, J ) = C( 6, J ) - SUM*T6
            C( 7, J ) = C( 7, J ) - SUM*T7
            C( 8, J ) = C( 8, J ) - SUM*T8
  160    CONTINUE
         GO TO 410
  170    CONTINUE
*
*        Special code for 9 x 9 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         V3 = DCONJG( V( 3 ) )
         T3 = TAU*DCONJG( V3 )
         V4 = DCONJG( V( 4 ) )
         T4 = TAU*DCONJG( V4 )
         V5 = DCONJG( V( 5 ) )
         T5 = TAU*DCONJG( V5 )
         V6 = DCONJG( V( 6 ) )
         T6 = TAU*DCONJG( V6 )
         V7 = DCONJG( V( 7 ) )
         T7 = TAU*DCONJG( V7 )
         V8 = DCONJG( V( 8 ) )
         T8 = TAU*DCONJG( V8 )
         V9 = DCONJG( V( 9 ) )
         T9 = TAU*DCONJG( V9 )
         DO 180 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J ) +
     $            V4*C( 4, J ) + V5*C( 5, J ) + V6*C( 6, J ) +
     $            V7*C( 7, J ) + V8*C( 8, J ) + V9*C( 9, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
            C( 3, J ) = C( 3, J ) - SUM*T3
            C( 4, J ) = C( 4, J ) - SUM*T4
            C( 5, J ) = C( 5, J ) - SUM*T5
            C( 6, J ) = C( 6, J ) - SUM*T6
            C( 7, J ) = C( 7, J ) - SUM*T7
            C( 8, J ) = C( 8, J ) - SUM*T8
            C( 9, J ) = C( 9, J ) - SUM*T9
  180    CONTINUE
         GO TO 410
  190    CONTINUE
*
*        Special code for 10 x 10 Householder
*
         V1 = DCONJG( V( 1 ) )
         T1 = TAU*DCONJG( V1 )
         V2 = DCONJG( V( 2 ) )
         T2 = TAU*DCONJG( V2 )
         V3 = DCONJG( V( 3 ) )
         T3 = TAU*DCONJG( V3 )
         V4 = DCONJG( V( 4 ) )
         T4 = TAU*DCONJG( V4 )
         V5 = DCONJG( V( 5 ) )
         T5 = TAU*DCONJG( V5 )
         V6 = DCONJG( V( 6 ) )
         T6 = TAU*DCONJG( V6 )
         V7 = DCONJG( V( 7 ) )
         T7 = TAU*DCONJG( V7 )
         V8 = DCONJG( V( 8 ) )
         T8 = TAU*DCONJG( V8 )
         V9 = DCONJG( V( 9 ) )
         T9 = TAU*DCONJG( V9 )
         V10 = DCONJG( V( 10 ) )
         T10 = TAU*DCONJG( V10 )
         DO 200 J = 1, N
            SUM = V1*C( 1, J ) + V2*C( 2, J ) + V3*C( 3, J ) +
     $            V4*C( 4, J ) + V5*C( 5, J ) + V6*C( 6, J ) +
     $            V7*C( 7, J ) + V8*C( 8, J ) + V9*C( 9, J ) +
     $            V10*C( 10, J )
            C( 1, J ) = C( 1, J ) - SUM*T1
            C( 2, J ) = C( 2, J ) - SUM*T2
            C( 3, J ) = C( 3, J ) - SUM*T3
            C( 4, J ) = C( 4, J ) - SUM*T4
            C( 5, J ) = C( 5, J ) - SUM*T5
            C( 6, J ) = C( 6, J ) - SUM*T6
            C( 7, J ) = C( 7, J ) - SUM*T7
            C( 8, J ) = C( 8, J ) - SUM*T8
            C( 9, J ) = C( 9, J ) - SUM*T9
            C( 10, J ) = C( 10, J ) - SUM*T10
  200    CONTINUE
         GO TO 410
      ELSE
*
*        Form  C * H, where H has order n.
*
         GO TO ( 210, 230, 250, 270, 290, 310, 330, 350,
     $           370, 390 )N
*
*        Code for general N
*
*        w := C * v
*
         CALL ZGEMV( 'No transpose', M, N, ONE, C, LDC, V, 1, ZERO,
     $               WORK, 1 )
*
*        C := C - tau * w * v'
*
         CALL ZGERC( M, N, -TAU, WORK, 1, V, 1, C, LDC )
         GO TO 410
  210    CONTINUE
*
*        Special code for 1 x 1 Householder
*
         T1 = ONE - TAU*V( 1 )*DCONJG( V( 1 ) )
         DO 220 J = 1, M
            C( J, 1 ) = T1*C( J, 1 )
  220    CONTINUE
         GO TO 410
  230    CONTINUE
*
*        Special code for 2 x 2 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         DO 240 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
  240    CONTINUE
         GO TO 410
  250    CONTINUE
*
*        Special code for 3 x 3 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         V3 = V( 3 )
         T3 = TAU*DCONJG( V3 )
         DO 260 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
            C( J, 3 ) = C( J, 3 ) - SUM*T3
  260    CONTINUE
         GO TO 410
  270    CONTINUE
*
*        Special code for 4 x 4 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         V3 = V( 3 )
         T3 = TAU*DCONJG( V3 )
         V4 = V( 4 )
         T4 = TAU*DCONJG( V4 )
         DO 280 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 ) +
     $            V4*C( J, 4 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
            C( J, 3 ) = C( J, 3 ) - SUM*T3
            C( J, 4 ) = C( J, 4 ) - SUM*T4
  280    CONTINUE
         GO TO 410
  290    CONTINUE
*
*        Special code for 5 x 5 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         V3 = V( 3 )
         T3 = TAU*DCONJG( V3 )
         V4 = V( 4 )
         T4 = TAU*DCONJG( V4 )
         V5 = V( 5 )
         T5 = TAU*DCONJG( V5 )
         DO 300 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 ) +
     $            V4*C( J, 4 ) + V5*C( J, 5 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
            C( J, 3 ) = C( J, 3 ) - SUM*T3
            C( J, 4 ) = C( J, 4 ) - SUM*T4
            C( J, 5 ) = C( J, 5 ) - SUM*T5
  300    CONTINUE
         GO TO 410
  310    CONTINUE
*
*        Special code for 6 x 6 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         V3 = V( 3 )
         T3 = TAU*DCONJG( V3 )
         V4 = V( 4 )
         T4 = TAU*DCONJG( V4 )
         V5 = V( 5 )
         T5 = TAU*DCONJG( V5 )
         V6 = V( 6 )
         T6 = TAU*DCONJG( V6 )
         DO 320 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 ) +
     $            V4*C( J, 4 ) + V5*C( J, 5 ) + V6*C( J, 6 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
            C( J, 3 ) = C( J, 3 ) - SUM*T3
            C( J, 4 ) = C( J, 4 ) - SUM*T4
            C( J, 5 ) = C( J, 5 ) - SUM*T5
            C( J, 6 ) = C( J, 6 ) - SUM*T6
  320    CONTINUE
         GO TO 410
  330    CONTINUE
*
*        Special code for 7 x 7 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         V3 = V( 3 )
         T3 = TAU*DCONJG( V3 )
         V4 = V( 4 )
         T4 = TAU*DCONJG( V4 )
         V5 = V( 5 )
         T5 = TAU*DCONJG( V5 )
         V6 = V( 6 )
         T6 = TAU*DCONJG( V6 )
         V7 = V( 7 )
         T7 = TAU*DCONJG( V7 )
         DO 340 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 ) +
     $            V4*C( J, 4 ) + V5*C( J, 5 ) + V6*C( J, 6 ) +
     $            V7*C( J, 7 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
            C( J, 3 ) = C( J, 3 ) - SUM*T3
            C( J, 4 ) = C( J, 4 ) - SUM*T4
            C( J, 5 ) = C( J, 5 ) - SUM*T5
            C( J, 6 ) = C( J, 6 ) - SUM*T6
            C( J, 7 ) = C( J, 7 ) - SUM*T7
  340    CONTINUE
         GO TO 410
  350    CONTINUE
*
*        Special code for 8 x 8 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         V3 = V( 3 )
         T3 = TAU*DCONJG( V3 )
         V4 = V( 4 )
         T4 = TAU*DCONJG( V4 )
         V5 = V( 5 )
         T5 = TAU*DCONJG( V5 )
         V6 = V( 6 )
         T6 = TAU*DCONJG( V6 )
         V7 = V( 7 )
         T7 = TAU*DCONJG( V7 )
         V8 = V( 8 )
         T8 = TAU*DCONJG( V8 )
         DO 360 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 ) +
     $            V4*C( J, 4 ) + V5*C( J, 5 ) + V6*C( J, 6 ) +
     $            V7*C( J, 7 ) + V8*C( J, 8 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
            C( J, 3 ) = C( J, 3 ) - SUM*T3
            C( J, 4 ) = C( J, 4 ) - SUM*T4
            C( J, 5 ) = C( J, 5 ) - SUM*T5
            C( J, 6 ) = C( J, 6 ) - SUM*T6
            C( J, 7 ) = C( J, 7 ) - SUM*T7
            C( J, 8 ) = C( J, 8 ) - SUM*T8
  360    CONTINUE
         GO TO 410
  370    CONTINUE
*
*        Special code for 9 x 9 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         V3 = V( 3 )
         T3 = TAU*DCONJG( V3 )
         V4 = V( 4 )
         T4 = TAU*DCONJG( V4 )
         V5 = V( 5 )
         T5 = TAU*DCONJG( V5 )
         V6 = V( 6 )
         T6 = TAU*DCONJG( V6 )
         V7 = V( 7 )
         T7 = TAU*DCONJG( V7 )
         V8 = V( 8 )
         T8 = TAU*DCONJG( V8 )
         V9 = V( 9 )
         T9 = TAU*DCONJG( V9 )
         DO 380 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 ) +
     $            V4*C( J, 4 ) + V5*C( J, 5 ) + V6*C( J, 6 ) +
     $            V7*C( J, 7 ) + V8*C( J, 8 ) + V9*C( J, 9 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
            C( J, 3 ) = C( J, 3 ) - SUM*T3
            C( J, 4 ) = C( J, 4 ) - SUM*T4
            C( J, 5 ) = C( J, 5 ) - SUM*T5
            C( J, 6 ) = C( J, 6 ) - SUM*T6
            C( J, 7 ) = C( J, 7 ) - SUM*T7
            C( J, 8 ) = C( J, 8 ) - SUM*T8
            C( J, 9 ) = C( J, 9 ) - SUM*T9
  380    CONTINUE
         GO TO 410
  390    CONTINUE
*
*        Special code for 10 x 10 Householder
*
         V1 = V( 1 )
         T1 = TAU*DCONJG( V1 )
         V2 = V( 2 )
         T2 = TAU*DCONJG( V2 )
         V3 = V( 3 )
         T3 = TAU*DCONJG( V3 )
         V4 = V( 4 )
         T4 = TAU*DCONJG( V4 )
         V5 = V( 5 )
         T5 = TAU*DCONJG( V5 )
         V6 = V( 6 )
         T6 = TAU*DCONJG( V6 )
         V7 = V( 7 )
         T7 = TAU*DCONJG( V7 )
         V8 = V( 8 )
         T8 = TAU*DCONJG( V8 )
         V9 = V( 9 )
         T9 = TAU*DCONJG( V9 )
         V10 = V( 10 )
         T10 = TAU*DCONJG( V10 )
         DO 400 J = 1, M
            SUM = V1*C( J, 1 ) + V2*C( J, 2 ) + V3*C( J, 3 ) +
     $            V4*C( J, 4 ) + V5*C( J, 5 ) + V6*C( J, 6 ) +
     $            V7*C( J, 7 ) + V8*C( J, 8 ) + V9*C( J, 9 ) +
     $            V10*C( J, 10 )
            C( J, 1 ) = C( J, 1 ) - SUM*T1
            C( J, 2 ) = C( J, 2 ) - SUM*T2
            C( J, 3 ) = C( J, 3 ) - SUM*T3
            C( J, 4 ) = C( J, 4 ) - SUM*T4
            C( J, 5 ) = C( J, 5 ) - SUM*T5
            C( J, 6 ) = C( J, 6 ) - SUM*T6
            C( J, 7 ) = C( J, 7 ) - SUM*T7
            C( J, 8 ) = C( J, 8 ) - SUM*T8
            C( J, 9 ) = C( J, 9 ) - SUM*T9
            C( J, 10 ) = C( J, 10 ) - SUM*T10
  400    CONTINUE
         GO TO 410
      END IF
  410 CONTINUE
      RETURN
*
*     End of ZLARFX
*
      END
      SUBROUTINE ZLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      DOUBLE PRECISION   CFROM, CTO
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLASCL multiplies the M by N complex matrix A by the real scalar
*  CTO/CFROM.  This is done without over/underflow as long as the final
*  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
*  A may be full, upper triangular, lower triangular, upper Hessenberg,
*  or banded.
*
*  Arguments
*  =========
*
*  TYPE    (input) CHARACTER*1
*          TYPE indices the storage type of the input matrix.
*          = 'G':  A is a full matrix.
*          = 'L':  A is a lower triangular matrix.
*          = 'U':  A is an upper triangular matrix.
*          = 'H':  A is an upper Hessenberg matrix.
*          = 'B':  A is a symmetric band matrix with lower bandwidth KL
*                  and upper bandwidth KU and with the only the lower
*                  half stored.
*          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
*                  and upper bandwidth KU and with the only the upper
*                  half stored.
*          = 'Z':  A is a band matrix with lower bandwidth KL and upper
*                  bandwidth KU.
*
*  KL      (input) INTEGER
*          The lower bandwidth of A.  Referenced only if TYPE = 'B',
*          'Q' or 'Z'.
*
*  KU      (input) INTEGER
*          The upper bandwidth of A.  Referenced only if TYPE = 'B',
*          'Q' or 'Z'.
*
*  CFROM   (input) DOUBLE PRECISION
*  CTO     (input) DOUBLE PRECISION
*          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
*          without over/underflow if the final result CTO*A(I,J)/CFROM
*          can be represented without over/underflow.  CFROM must be
*          nonzero.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,M)
*          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
*          storage type.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  INFO    (output) INTEGER
*          0  - successful exit
*          <0 - if INFO = -i, the i-th argument had an illegal value.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      DOUBLE PRECISION   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
*
      IF( LSAME( TYPE, 'G' ) ) THEN
         ITYPE = 0
      ELSE IF( LSAME( TYPE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LSAME( TYPE, 'U' ) ) THEN
         ITYPE = 2
      ELSE IF( LSAME( TYPE, 'H' ) ) THEN
         ITYPE = 3
      ELSE IF( LSAME( TYPE, 'B' ) ) THEN
         ITYPE = 4
      ELSE IF( LSAME( TYPE, 'Q' ) ) THEN
         ITYPE = 5
      ELSE IF( LSAME( TYPE, 'Z' ) ) THEN
         ITYPE = 6
      ELSE
         ITYPE = -1
      END IF
*
      IF( ITYPE.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( CFROM.EQ.ZERO ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR.
     $         ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR.
     $            ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) )
     $             THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR.
     $            ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR.
     $            ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZLASCL', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. M.EQ.0 )
     $   RETURN
*
*     Get machine parameters
*
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
*
      CFROMC = CFROM
      CTOC = CTO
*
   10 CONTINUE
      CFROM1 = CFROMC*SMLNUM
      CTO1 = CTOC / BIGNUM
      IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
         MUL = SMLNUM
         DONE = .FALSE.
         CFROMC = CFROM1
      ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
         MUL = BIGNUM
         DONE = .FALSE.
         CTOC = CTO1
      ELSE
         MUL = CTOC / CFROMC
         DONE = .TRUE.
      END IF
*
      IF( ITYPE.EQ.0 ) THEN
*
*        Full matrix
*
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
*
      ELSE IF( ITYPE.EQ.1 ) THEN
*
*        Lower triangular matrix
*
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
*
      ELSE IF( ITYPE.EQ.2 ) THEN
*
*        Upper triangular matrix
*
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
*
      ELSE IF( ITYPE.EQ.3 ) THEN
*
*        Upper Hessenberg matrix
*
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
*
      ELSE IF( ITYPE.EQ.4 ) THEN
*
*        Lower half of a symmetric band matrix
*
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
*
      ELSE IF( ITYPE.EQ.5 ) THEN
*
*        Upper half of a symmetric band matrix
*
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
*
      ELSE IF( ITYPE.EQ.6 ) THEN
*
*        Band matrix
*
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
*
      END IF
*
      IF( .NOT.DONE )
     $   GO TO 10
*
      RETURN
*
*     End of ZLASCL
*
      END
      SUBROUTINE ZLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      COMPLEX*16         ALPHA, BETA
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLASET initializes a 2-D array A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies the part of the matrix A to be set.
*          = 'U':      Upper triangular part is set. The lower triangle
*                      is unchanged.
*          = 'L':      Lower triangular part is set. The upper triangle
*                      is unchanged.
*          Otherwise:  All of the matrix A is set.
*
*  M       (input) INTEGER
*          On entry, M specifies the number of rows of A.
*
*  N       (input) INTEGER
*          On entry, N specifies the number of columns of A.
*
*  ALPHA   (input) COMPLEX*16
*          All the offdiagonal array elements are set to ALPHA.
*
*  BETA    (input) COMPLEX*16
*          All the diagonal array elements are set to BETA.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the m by n matrix A.
*          On exit, A(i,j) = ALPHA, 1 <= i <= m, 1 <= j <= n, i.ne.j;
*                   A(i,i) = BETA , 1 <= i <= min(m,n)
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( LSAME( UPLO, 'U' ) ) THEN
*
*        Set the diagonal to BETA and the strictly upper triangular
*        part of the array to ALPHA.
*
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
         DO 30 I = 1, MIN( N, M )
            A( I, I ) = BETA
   30    CONTINUE
*
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
*
*        Set the diagonal to BETA and the strictly lower triangular
*        part of the array to ALPHA.
*
         DO 50 J = 1, MIN( M, N )
            DO 40 I = J + 1, M
               A( I, J ) = ALPHA
   40       CONTINUE
   50    CONTINUE
         DO 60 I = 1, MIN( N, M )
            A( I, I ) = BETA
   60    CONTINUE
*
      ELSE
*
*        Set the array to BETA on the diagonal and ALPHA on the
*        offdiagonal.
*
         DO 80 J = 1, N
            DO 70 I = 1, M
               A( I, J ) = ALPHA
   70       CONTINUE
   80    CONTINUE
         DO 90 I = 1, MIN( M, N )
            A( I, I ) = BETA
   90    CONTINUE
      END IF
*
      RETURN
*
*     End of ZLASET
*
      END
      SUBROUTINE ZLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, PIVOT, SIDE
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( * ), S( * )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLASR   performs the transformation
*
*     A := P*A,   when SIDE = 'L' or 'l'  (  Left-hand side )
*
*     A := A*P',  when SIDE = 'R' or 'r'  ( Right-hand side )
*
*  where A is an m by n complex matrix and P is an orthogonal matrix,
*  consisting of a sequence of plane rotations determined by the
*  parameters PIVOT and DIRECT as follows ( z = m when SIDE = 'L' or 'l'
*  and z = n when SIDE = 'R' or 'r' ):
*
*  When  DIRECT = 'F' or 'f'  ( Forward sequence ) then
*
*     P = P( z - 1 )*...*P( 2 )*P( 1 ),
*
*  and when DIRECT = 'B' or 'b'  ( Backward sequence ) then
*
*     P = P( 1 )*P( 2 )*...*P( z - 1 ),
*
*  where  P( k ) is a plane rotation matrix for the following planes:
*
*     when  PIVOT = 'V' or 'v'  ( Variable pivot ),
*        the plane ( k, k + 1 )
*
*     when  PIVOT = 'T' or 't'  ( Top pivot ),
*        the plane ( 1, k + 1 )
*
*     when  PIVOT = 'B' or 'b'  ( Bottom pivot ),
*        the plane ( k, z )
*
*  c( k ) and s( k )  must contain the  cosine and sine that define the
*  matrix  P( k ).  The two by two plane rotation part of the matrix
*  P( k ), R( k ), is assumed to be of the form
*
*     R( k ) = (  c( k )  s( k ) ).
*              ( -s( k )  c( k ) )
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          Specifies whether the plane rotation matrix P is applied to
*          A on the left or the right.
*          = 'L':  Left, compute A := P*A
*          = 'R':  Right, compute A:= A*P'
*
*  DIRECT  (input) CHARACTER*1
*          Specifies whether P is a forward or backward sequence of
*          plane rotations.
*          = 'F':  Forward, P = P( z - 1 )*...*P( 2 )*P( 1 )
*          = 'B':  Backward, P = P( 1 )*P( 2 )*...*P( z - 1 )
*
*  PIVOT   (input) CHARACTER*1
*          Specifies the plane for which P(k) is a plane rotation
*          matrix.
*          = 'V':  Variable pivot, the plane (k,k+1)
*          = 'T':  Top pivot, the plane (1,k+1)
*          = 'B':  Bottom pivot, the plane (k,z)
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  If m <= 1, an immediate
*          return is effected.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  If n <= 1, an
*          immediate return is effected.
*
*  C, S    (input) DOUBLE PRECISION arrays, dimension
*                  (M-1) if SIDE = 'L'
*                  (N-1) if SIDE = 'R'
*          c(k) and s(k) contain the cosine and sine that define the
*          matrix P(k).  The two by two plane rotation part of the
*          matrix P(k), R(k), is assumed to be of the form
*          R( k ) = (  c( k )  s( k ) ).
*                   ( -s( k )  c( k ) )
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          The m by n matrix A.  On exit, A is overwritten by P*A if
*          SIDE = 'R' or by A*P' if SIDE = 'L'.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
      DOUBLE PRECISION   CTEMP, STEMP
      COMPLEX*16         TEMP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      IF( .NOT.( LSAME( SIDE, 'L' ) .OR. LSAME( SIDE, 'R' ) ) ) THEN
         INFO = 1
      ELSE IF( .NOT.( LSAME( PIVOT, 'V' ) .OR. LSAME( PIVOT,
     $         'T' ) .OR. LSAME( PIVOT, 'B' ) ) ) THEN
         INFO = 2
      ELSE IF( .NOT.( LSAME( DIRECT, 'F' ) .OR. LSAME( DIRECT, 'B' ) ) )
     $          THEN
         INFO = 3
      ELSE IF( M.LT.0 ) THEN
         INFO = 4
      ELSE IF( N.LT.0 ) THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = 9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZLASR ', INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( ( M.EQ.0 ) .OR. ( N.EQ.0 ) )
     $   RETURN
      IF( LSAME( SIDE, 'L' ) ) THEN
*
*        Form  P * A
*
         IF( LSAME( PIVOT, 'V' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 20 J = 1, M - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 10 I = 1, N
                        TEMP = A( J+1, I )
                        A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
                        A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
   10                CONTINUE
                  END IF
   20          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 40 J = M - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 30 I = 1, N
                        TEMP = A( J+1, I )
                        A( J+1, I ) = CTEMP*TEMP - STEMP*A( J, I )
                        A( J, I ) = STEMP*TEMP + CTEMP*A( J, I )
   30                CONTINUE
                  END IF
   40          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'T' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 60 J = 2, M
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 50 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
                        A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
   50                CONTINUE
                  END IF
   60          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 80 J = M, 2, -1
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 70 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = CTEMP*TEMP - STEMP*A( 1, I )
                        A( 1, I ) = STEMP*TEMP + CTEMP*A( 1, I )
   70                CONTINUE
                  END IF
   80          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'B' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 100 J = 1, M - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 90 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
                        A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
   90                CONTINUE
                  END IF
  100          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 120 J = M - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 110 I = 1, N
                        TEMP = A( J, I )
                        A( J, I ) = STEMP*A( M, I ) + CTEMP*TEMP
                        A( M, I ) = CTEMP*A( M, I ) - STEMP*TEMP
  110                CONTINUE
                  END IF
  120          CONTINUE
            END IF
         END IF
      ELSE IF( LSAME( SIDE, 'R' ) ) THEN
*
*        Form A * P'
*
         IF( LSAME( PIVOT, 'V' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 140 J = 1, N - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 130 I = 1, M
                        TEMP = A( I, J+1 )
                        A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  130                CONTINUE
                  END IF
  140          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 160 J = N - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 150 I = 1, M
                        TEMP = A( I, J+1 )
                        A( I, J+1 ) = CTEMP*TEMP - STEMP*A( I, J )
                        A( I, J ) = STEMP*TEMP + CTEMP*A( I, J )
  150                CONTINUE
                  END IF
  160          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'T' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 180 J = 2, N
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 170 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
                        A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
  170                CONTINUE
                  END IF
  180          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 200 J = N, 2, -1
                  CTEMP = C( J-1 )
                  STEMP = S( J-1 )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 190 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = CTEMP*TEMP - STEMP*A( I, 1 )
                        A( I, 1 ) = STEMP*TEMP + CTEMP*A( I, 1 )
  190                CONTINUE
                  END IF
  200          CONTINUE
            END IF
         ELSE IF( LSAME( PIVOT, 'B' ) ) THEN
            IF( LSAME( DIRECT, 'F' ) ) THEN
               DO 220 J = 1, N - 1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 210 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
                        A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
  210                CONTINUE
                  END IF
  220          CONTINUE
            ELSE IF( LSAME( DIRECT, 'B' ) ) THEN
               DO 240 J = N - 1, 1, -1
                  CTEMP = C( J )
                  STEMP = S( J )
                  IF( ( CTEMP.NE.ONE ) .OR. ( STEMP.NE.ZERO ) ) THEN
                     DO 230 I = 1, M
                        TEMP = A( I, J )
                        A( I, J ) = STEMP*A( I, N ) + CTEMP*TEMP
                        A( I, N ) = CTEMP*A( I, N ) - STEMP*TEMP
  230                CONTINUE
                  END IF
  240          CONTINUE
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of ZLASR
*
      END
      SUBROUTINE ZLASSQ( N, X, INCX, SCALE, SUMSQ )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SCALE, SUMSQ
*     ..
*     .. Array Arguments ..
      COMPLEX*16         X( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLASSQ returns the values scl and ssq such that
*
*     ( scl**2 )*ssq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
*
*  where x( i ) = abs( X( 1 + ( i - 1 )*INCX ) ). The value of sumsq is
*  assumed to be at least unity and the value of ssq will then satisfy
*
*     1.0 .le. ssq .le. ( sumsq + 2*n ).
*
*  scale is assumed to be non-negative and scl returns the value
*
*     scl = max( scale, abs( real( x( i ) ) ), abs( aimag( x( i ) ) ) ),
*            i
*
*  scale and sumsq must be supplied in SCALE and SUMSQ respectively.
*  SCALE and SUMSQ are overwritten by scl and ssq respectively.
*
*  The routine makes only one pass through the vector X.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of elements to be used from the vector X.
*
*  X       (input) COMPLEX*16 array, dimension (N)
*          The vector x as described above.
*             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
*
*  INCX    (input) INTEGER
*          The increment between successive values of the vector X.
*          INCX > 0.
*
*  SCALE   (input/output) DOUBLE PRECISION
*          On entry, the value  scale  in the equation above.
*          On exit, SCALE is overwritten with the value  scl .
*
*  SUMSQ   (input/output) DOUBLE PRECISION
*          On entry, the value  sumsq  in the equation above.
*          On exit, SUMSQ is overwritten with the value  ssq .
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            IX
      DOUBLE PRECISION   TEMP1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG
*     ..
*     .. Executable Statements ..
*
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            IF( DBLE( X( IX ) ).NE.ZERO ) THEN
               TEMP1 = ABS( DBLE( X( IX ) ) )
               IF( SCALE.LT.TEMP1 ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2
                  SCALE = TEMP1
               ELSE
                  SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2
               END IF
            END IF
            IF( DIMAG( X( IX ) ).NE.ZERO ) THEN
               TEMP1 = ABS( DIMAG( X( IX ) ) )
               IF( SCALE.LT.TEMP1 ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / TEMP1 )**2
                  SCALE = TEMP1
               ELSE
                  SUMSQ = SUMSQ + ( TEMP1 / SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
*
      RETURN
*
*     End of ZLASSQ
*
      END
      SUBROUTINE ZLASWP( N, A, LDA, K1, K2, IPIV, INCX )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INCX, K1, K2, LDA, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      COMPLEX*16         A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLASWP performs a series of row interchanges on the matrix A.
*  One row interchange is initiated for each of rows K1 through K2 of A.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the matrix of column dimension N to which the row
*          interchanges will be applied.
*          On exit, the permuted matrix.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*
*  K1      (input) INTEGER
*          The first element of IPIV for which a row interchange will
*          be done.
*
*  K2      (input) INTEGER
*          The last element of IPIV for which a row interchange will
*          be done.
*
*  IPIV    (input) INTEGER array, dimension (M*abs(INCX))
*          The vector of pivot indices.  Only the elements in positions
*          K1 through K2 of IPIV are accessed.
*          IPIV(K) = L implies rows K and L are to be interchanged.
*
*  INCX    (input) INTEGER
*          The increment between successive values of IPIV.  If IPIV
*          is negative, the pivots are applied in reverse order.
*
*  Further Details
*  ===============
*
*  Modified by
*   R. C. Whaley, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*
* =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, I1, I2, INC, IP, IX, IX0, J, K, N32
      COMPLEX*16         TEMP
*     ..
*     .. Executable Statements ..
*
*     Interchange row I with row IPIV(I) for each of rows K1 through K2.
*
      IF( INCX.GT.0 ) THEN
         IX0 = K1
         I1 = K1
         I2 = K2
         INC = 1
      ELSE IF( INCX.LT.0 ) THEN
         IX0 = 1 + ( 1-K2 )*INCX
         I1 = K2
         I2 = K1
         INC = -1
      ELSE
         RETURN
      END IF
*
      N32 = ( N / 32 )*32
      IF( N32.NE.0 ) THEN
         DO 30 J = 1, N32, 32
            IX = IX0
            DO 20 I = I1, I2, INC
               IP = IPIV( IX )
               IF( IP.NE.I ) THEN
                  DO 10 K = J, J + 31
                     TEMP = A( I, K )
                     A( I, K ) = A( IP, K )
                     A( IP, K ) = TEMP
   10             CONTINUE
               END IF
               IX = IX + INCX
   20       CONTINUE
   30    CONTINUE
      END IF
      IF( N32.NE.N ) THEN
         N32 = N32 + 1
         IX = IX0
         DO 50 I = I1, I2, INC
            IP = IPIV( IX )
            IF( IP.NE.I ) THEN
               DO 40 K = N32, N
                  TEMP = A( I, K )
                  A( I, K ) = A( IP, K )
                  A( IP, K ) = TEMP
   40          CONTINUE
            END IF
            IX = IX + INCX
   50    CONTINUE
      END IF
*
      RETURN
*
*     End of ZLASWP
*
      END
      SUBROUTINE ZLATRD( UPLO, N, NB, A, LDA, E, TAU, W, LDW )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDW, N, NB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   E( * )
      COMPLEX*16         A( LDA, * ), TAU( * ), W( LDW, * )
*     ..
*
*  Purpose
*  =======
*
*  ZLATRD reduces NB rows and columns of a complex Hermitian matrix A to
*  Hermitian tridiagonal form by a unitary similarity
*  transformation Q' * A * Q, and returns the matrices V and W which are
*  needed to apply the transformation to the unreduced part of A.
*
*  If UPLO = 'U', ZLATRD reduces the last NB rows and columns of a
*  matrix, of which the upper triangle is supplied;
*  if UPLO = 'L', ZLATRD reduces the first NB rows and columns of a
*  matrix, of which the lower triangle is supplied.
*
*  This is an auxiliary routine called by ZHETRD.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER
*          Specifies whether the upper or lower triangular part of the
*          Hermitian matrix A is stored:
*          = 'U': Upper triangular
*          = 'L': Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.
*
*  NB      (input) INTEGER
*          The number of rows and columns to be reduced.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the Hermitian matrix A.  If UPLO = 'U', the leading
*          n-by-n upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading n-by-n lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*          On exit:
*          if UPLO = 'U', the last NB columns have been reduced to
*            tridiagonal form, with the diagonal elements overwriting
*            the diagonal elements of A; the elements above the diagonal
*            with the array TAU, represent the unitary matrix Q as a
*            product of elementary reflectors;
*          if UPLO = 'L', the first NB columns have been reduced to
*            tridiagonal form, with the diagonal elements overwriting
*            the diagonal elements of A; the elements below the diagonal
*            with the array TAU, represent the  unitary matrix Q as a
*            product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  E       (output) DOUBLE PRECISION array, dimension (N-1)
*          If UPLO = 'U', E(n-nb:n-1) contains the superdiagonal
*          elements of the last NB columns of the reduced matrix;
*          if UPLO = 'L', E(1:nb) contains the subdiagonal elements of
*          the first NB columns of the reduced matrix.
*
*  TAU     (output) COMPLEX*16 array, dimension (N-1)
*          The scalar factors of the elementary reflectors, stored in
*          TAU(n-nb:n-1) if UPLO = 'U', and in TAU(1:nb) if UPLO = 'L'.
*          See Further Details.
*
*  W       (output) COMPLEX*16 array, dimension (LDW,NB)
*          The n-by-nb matrix W required to update the unreduced part
*          of A.
*
*  LDW     (input) INTEGER
*          The leading dimension of the array W. LDW >= max(1,N).
*
*  Further Details
*  ===============
*
*  If UPLO = 'U', the matrix Q is represented as a product of elementary
*  reflectors
*
*     Q = H(n) H(n-1) . . . H(n-nb+1).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(i:n) = 0 and v(i-1) = 1; v(1:i-1) is stored on exit in A(1:i-1,i),
*  and tau in TAU(i-1).
*
*  If UPLO = 'L', the matrix Q is represented as a product of elementary
*  reflectors
*
*     Q = H(1) H(2) . . . H(nb).
*
*  Each H(i) has the form
*
*     H(i) = I - tau * v * v'
*
*  where tau is a complex scalar, and v is a complex vector with
*  v(1:i) = 0 and v(i+1) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
*  and tau in TAU(i).
*
*  The elements of the vectors v together form the n-by-nb matrix V
*  which is needed, with W, to apply the transformation to the unreduced
*  part of the matrix, using a Hermitian rank-2k update of the form:
*  A := A - V*W' - W*V'.
*
*  The contents of A on exit are illustrated by the following examples
*  with n = 5 and nb = 2:
*
*  if UPLO = 'U':                       if UPLO = 'L':
*
*    (  a   a   a   v4  v5 )              (  d                  )
*    (      a   a   v4  v5 )              (  1   d              )
*    (          a   1   v5 )              (  v1  1   a          )
*    (              d   1  )              (  v1  v2  a   a      )
*    (                  d  )              (  v1  v2  a   a   a  )
*
*  where d denotes a diagonal element of the reduced matrix, a denotes
*  an element of the original matrix that is unchanged, and vi denotes
*  an element of the vector defining H(i).
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE, HALF
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ),
     $                   HALF = ( 0.5D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IW
      COMPLEX*16         ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZAXPY, ZGEMV, ZHEMV, ZLACGV, ZLARFG, ZSCAL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      COMPLEX*16         ZDOTC
      EXTERNAL           LSAME, ZDOTC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.0 )
     $   RETURN
*
      IF( LSAME( UPLO, 'U' ) ) THEN
*
*        Reduce last NB columns of upper triangle
*
         DO 10 I = N, N - NB + 1, -1
            IW = I - N + NB
            IF( I.LT.N ) THEN
*
*              Update A(1:i,i)
*
               A( I, I ) = DBLE( A( I, I ) )
               CALL ZLACGV( N-I, W( I, IW+1 ), LDW )
               CALL ZGEMV( 'No transpose', I, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, W( I, IW+1 ), LDW, ONE, A( 1, I ), 1 )
               CALL ZLACGV( N-I, W( I, IW+1 ), LDW )
               CALL ZLACGV( N-I, A( I, I+1 ), LDA )
               CALL ZGEMV( 'No transpose', I, N-I, -ONE, W( 1, IW+1 ),
     $                     LDW, A( I, I+1 ), LDA, ONE, A( 1, I ), 1 )
               CALL ZLACGV( N-I, A( I, I+1 ), LDA )
               A( I, I ) = DBLE( A( I, I ) )
            END IF
            IF( I.GT.1 ) THEN
*
*              Generate elementary reflector H(i) to annihilate
*              A(1:i-2,i)
*
               ALPHA = A( I-1, I )
               CALL ZLARFG( I-1, ALPHA, A( 1, I ), 1, TAU( I-1 ) )
               E( I-1 ) = ALPHA
               A( I-1, I ) = ONE
*
*              Compute W(1:i-1,i)
*
               CALL ZHEMV( 'Upper', I-1, ONE, A, LDA, A( 1, I ), 1,
     $                     ZERO, W( 1, IW ), 1 )
               IF( I.LT.N ) THEN
                  CALL ZGEMV( 'Conjugate transpose', I-1, N-I, ONE,
     $                        W( 1, IW+1 ), LDW, A( 1, I ), 1, ZERO,
     $                        W( I+1, IW ), 1 )
                  CALL ZGEMV( 'No transpose', I-1, N-I, -ONE,
     $                        A( 1, I+1 ), LDA, W( I+1, IW ), 1, ONE,
     $                        W( 1, IW ), 1 )
                  CALL ZGEMV( 'Conjugate transpose', I-1, N-I, ONE,
     $                        A( 1, I+1 ), LDA, A( 1, I ), 1, ZERO,
     $                        W( I+1, IW ), 1 )
                  CALL ZGEMV( 'No transpose', I-1, N-I, -ONE,
     $                        W( 1, IW+1 ), LDW, W( I+1, IW ), 1, ONE,
     $                        W( 1, IW ), 1 )
               END IF
               CALL ZSCAL( I-1, TAU( I-1 ), W( 1, IW ), 1 )
               ALPHA = -HALF*TAU( I-1 )*ZDOTC( I-1, W( 1, IW ), 1,
     $                 A( 1, I ), 1 )
               CALL ZAXPY( I-1, ALPHA, A( 1, I ), 1, W( 1, IW ), 1 )
            END IF
*
   10    CONTINUE
      ELSE
*
*        Reduce first NB columns of lower triangle
*
         DO 20 I = 1, NB
*
*           Update A(i:n,i)
*
            A( I, I ) = DBLE( A( I, I ) )
            CALL ZLACGV( I-1, W( I, 1 ), LDW )
            CALL ZGEMV( 'No transpose', N-I+1, I-1, -ONE, A( I, 1 ),
     $                  LDA, W( I, 1 ), LDW, ONE, A( I, I ), 1 )
            CALL ZLACGV( I-1, W( I, 1 ), LDW )
            CALL ZLACGV( I-1, A( I, 1 ), LDA )
            CALL ZGEMV( 'No transpose', N-I+1, I-1, -ONE, W( I, 1 ),
     $                  LDW, A( I, 1 ), LDA, ONE, A( I, I ), 1 )
            CALL ZLACGV( I-1, A( I, 1 ), LDA )
            A( I, I ) = DBLE( A( I, I ) )
            IF( I.LT.N ) THEN
*
*              Generate elementary reflector H(i) to annihilate
*              A(i+2:n,i)
*
               ALPHA = A( I+1, I )
               CALL ZLARFG( N-I, ALPHA, A( MIN( I+2, N ), I ), 1,
     $                      TAU( I ) )
               E( I ) = ALPHA
               A( I+1, I ) = ONE
*
*              Compute W(i+1:n,i)
*
               CALL ZHEMV( 'Lower', N-I, ONE, A( I+1, I+1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, W( I+1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', N-I, I-1, ONE,
     $                     W( I+1, 1 ), LDW, A( I+1, I ), 1, ZERO,
     $                     W( 1, I ), 1 )
               CALL ZGEMV( 'No transpose', N-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
               CALL ZGEMV( 'Conjugate transpose', N-I, I-1, ONE,
     $                     A( I+1, 1 ), LDA, A( I+1, I ), 1, ZERO,
     $                     W( 1, I ), 1 )
               CALL ZGEMV( 'No transpose', N-I, I-1, -ONE, W( I+1, 1 ),
     $                     LDW, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
               CALL ZSCAL( N-I, TAU( I ), W( I+1, I ), 1 )
               ALPHA = -HALF*TAU( I )*ZDOTC( N-I, W( I+1, I ), 1,
     $                 A( I+1, I ), 1 )
               CALL ZAXPY( N-I, ALPHA, A( I+1, I ), 1, W( I+1, I ), 1 )
            END IF
*
   20    CONTINUE
      END IF
*
      RETURN
*
*     End of ZLATRD
*
      END
      SUBROUTINE ZLATRS( UPLO, TRANS, DIAG, NORMIN, N, A, LDA, X, SCALE,
     $                   CNORM, INFO )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, NORMIN, TRANS, UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   SCALE
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   CNORM( * )
      COMPLEX*16         A( LDA, * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLATRS solves one of the triangular systems
*
*     A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b,
*
*  with scaling to prevent overflow.  Here A is an upper or lower
*  triangular matrix, A**T denotes the transpose of A, A**H denotes the
*  conjugate transpose of A, x and b are n-element vectors, and s is a
*  scaling factor, usually less than or equal to 1, chosen so that the
*  components of x will be less than the overflow threshold.  If the
*  unscaled problem will not cause overflow, the Level 2 BLAS routine
*  ZTRSV is called. If the matrix A is singular (A(j,j) = 0 for some j),
*  then s is set to 0 and a non-trivial solution to A*x = 0 is returned.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the matrix A is upper or lower triangular.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  TRANS   (input) CHARACTER*1
*          Specifies the operation applied to A.
*          = 'N':  Solve A * x = s*b     (No transpose)
*          = 'T':  Solve A**T * x = s*b  (Transpose)
*          = 'C':  Solve A**H * x = s*b  (Conjugate transpose)
*
*  DIAG    (input) CHARACTER*1
*          Specifies whether or not the matrix A is unit triangular.
*          = 'N':  Non-unit triangular
*          = 'U':  Unit triangular
*
*  NORMIN  (input) CHARACTER*1
*          Specifies whether CNORM has been set or not.
*          = 'Y':  CNORM contains the column norms on entry
*          = 'N':  CNORM is not set on entry.  On exit, the norms will
*                  be computed and stored in CNORM.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The triangular matrix A.  If UPLO = 'U', the leading n by n
*          upper triangular part of the array A contains the upper
*          triangular matrix, and the strictly lower triangular part of
*          A is not referenced.  If UPLO = 'L', the leading n by n lower
*          triangular part of the array A contains the lower triangular
*          matrix, and the strictly upper triangular part of A is not
*          referenced.  If DIAG = 'U', the diagonal elements of A are
*          also not referenced and are assumed to be 1.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max (1,N).
*
*  X       (input/output) COMPLEX*16 array, dimension (N)
*          On entry, the right hand side b of the triangular system.
*          On exit, X is overwritten by the solution vector x.
*
*  SCALE   (output) DOUBLE PRECISION
*          The scaling factor s for the triangular system
*             A * x = s*b,  A**T * x = s*b,  or  A**H * x = s*b.
*          If SCALE = 0, the matrix A is singular or badly scaled, and
*          the vector x is an exact or approximate solution to A*x = 0.
*
*  CNORM   (input or output) DOUBLE PRECISION array, dimension (N)
*
*          If NORMIN = 'Y', CNORM is an input argument and CNORM(j)
*          contains the norm of the off-diagonal part of the j-th column
*          of A.  If TRANS = 'N', CNORM(j) must be greater than or equal
*          to the infinity-norm, and if TRANS = 'T' or 'C', CNORM(j)
*          must be greater than or equal to the 1-norm.
*
*          If NORMIN = 'N', CNORM is an output argument and CNORM(j)
*          returns the 1-norm of the offdiagonal part of the j-th column
*          of A.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -k, the k-th argument had an illegal value
*
*  Further Details
*  ======= =======
*
*  A rough bound on x is computed; if that is less than overflow, ZTRSV
*  is called, otherwise, specific code is used which checks for possible
*  overflow or divide-by-zero at every operation.
*
*  A columnwise scheme is used for solving A*x = b.  The basic algorithm
*  if A is lower triangular is
*
*       x[1:n] := b[1:n]
*       for j = 1, ..., n
*            x(j) := x(j) / A(j,j)
*            x[j+1:n] := x[j+1:n] - x(j) * A[j+1:n,j]
*       end
*
*  Define bounds on the components of x after j iterations of the loop:
*     M(j) = bound on x[1:j]
*     G(j) = bound on x[j+1:n]
*  Initially, let M(0) = 0 and G(0) = max{x(i), i=1,...,n}.
*
*  Then for iteration j+1 we have
*     M(j+1) <= G(j) / | A(j+1,j+1) |
*     G(j+1) <= G(j) + M(j+1) * | A[j+2:n,j+1] |
*            <= G(j) ( 1 + CNORM(j+1) / | A(j+1,j+1) | )
*
*  where CNORM(j+1) is greater than or equal to the infinity-norm of
*  column j+1 of A, not counting the diagonal.  Hence
*
*     G(j) <= G(0) product ( 1 + CNORM(i) / | A(i,i) | )
*                  1<=i<=j
*  and
*
*     |x(j)| <= ( G(0) / |A(j,j)| ) product ( 1 + CNORM(i) / |A(i,i)| )
*                                   1<=i< j
*
*  Since |x(j)| <= M(j), we use the Level 2 BLAS routine ZTRSV if the
*  reciprocal of the largest M(j), j=1,..,n, is larger than
*  max(underflow, 1/overflow).
*
*  The bound on x(j) is also used to determine when a step in the
*  columnwise method can be performed without fear of overflow.  If
*  the computed bound is greater than a large constant, x is scaled to
*  prevent overflow, but if the bound overflows, x is set to 0, x(j) to
*  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.
*
*  Similarly, a row-wise scheme is used to solve A**T *x = b  or
*  A**H *x = b.  The basic algorithm for A upper triangular is
*
*       for j = 1, ..., n
*            x(j) := ( b(j) - A[1:j-1,j]' * x[1:j-1] ) / A(j,j)
*       end
*
*  We simultaneously compute two bounds
*       G(j) = bound on ( b(i) - A[1:i-1,i]' * x[1:i-1] ), 1<=i<=j
*       M(j) = bound on x(i), 1<=i<=j
*
*  The initial values are G(0) = 0, M(0) = max{b(i), i=1,..,n}, and we
*  add the constraint G(j) >= G(j-1) and M(j) >= M(j-1) for j >= 1.
*  Then the bound on x(j) is
*
*       M(j) <= M(j-1) * ( 1 + CNORM(j) ) / | A(j,j) |
*
*            <= M(0) * product ( ( 1 + CNORM(i) ) / |A(i,i)| )
*                      1<=i<=j
*
*  and we can safely call ZTRSV if 1/M(n) and 1/G(n) are both greater
*  than max(underflow, 1/overflow).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0,
     $                   TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN, NOUNIT, UPPER
      INTEGER            I, IMAX, J, JFIRST, JINC, JLAST
      DOUBLE PRECISION   BIGNUM, GROW, REC, SMLNUM, TJJ, TMAX, TSCAL,
     $                   XBND, XJ, XMAX
      COMPLEX*16         CSUMJ, TJJS, USCAL, ZDUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX, IZAMAX
      DOUBLE PRECISION   DLAMCH, DZASUM
      COMPLEX*16         ZDOTC, ZDOTU, ZLADIV
      EXTERNAL           LSAME, IDAMAX, IZAMAX, DLAMCH, DZASUM, ZDOTC,
     $                   ZDOTU, ZLADIV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSCAL, XERBLA, ZAXPY, ZDSCAL, ZTRSV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, MAX, MIN
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION   CABS1, CABS2
*     ..
*     .. Statement Function definitions ..
      CABS1( ZDUM ) = ABS( DBLE( ZDUM ) ) + ABS( DIMAG( ZDUM ) )
      CABS2( ZDUM ) = ABS( DBLE( ZDUM ) / 2.D0 ) +
     $                ABS( DIMAG( ZDUM ) / 2.D0 )
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      NOTRAN = LSAME( TRANS, 'N' )
      NOUNIT = LSAME( DIAG, 'N' )
*
*     Test the input parameters.
*
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $         LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -3
      ELSE IF( .NOT.LSAME( NORMIN, 'Y' ) .AND. .NOT.
     $         LSAME( NORMIN, 'N' ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZLATRS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Determine machine dependent parameters to control overflow.
*
      SMLNUM = DLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
      SMLNUM = SMLNUM / DLAMCH( 'Precision' )
      BIGNUM = ONE / SMLNUM
      SCALE = ONE
*
      IF( LSAME( NORMIN, 'N' ) ) THEN
*
*        Compute the 1-norm of each column, not including the diagonal.
*
         IF( UPPER ) THEN
*
*           A is upper triangular.
*
            DO 10 J = 1, N
               CNORM( J ) = DZASUM( J-1, A( 1, J ), 1 )
   10       CONTINUE
         ELSE
*
*           A is lower triangular.
*
            DO 20 J = 1, N - 1
               CNORM( J ) = DZASUM( N-J, A( J+1, J ), 1 )
   20       CONTINUE
            CNORM( N ) = ZERO
         END IF
      END IF
*
*     Scale the column norms by TSCAL if the maximum element in CNORM is
*     greater than BIGNUM/2.
*
      IMAX = IDAMAX( N, CNORM, 1 )
      TMAX = CNORM( IMAX )
      IF( TMAX.LE.BIGNUM*HALF ) THEN
         TSCAL = ONE
      ELSE
         TSCAL = HALF / ( SMLNUM*TMAX )
         CALL DSCAL( N, TSCAL, CNORM, 1 )
      END IF
*
*     Compute a bound on the computed solution vector to see if the
*     Level 2 BLAS routine ZTRSV can be used.
*
      XMAX = ZERO
      DO 30 J = 1, N
         XMAX = MAX( XMAX, CABS2( X( J ) ) )
   30 CONTINUE
      XBND = XMAX
*
      IF( NOTRAN ) THEN
*
*        Compute the growth in A * x = b.
*
         IF( UPPER ) THEN
            JFIRST = N
            JLAST = 1
            JINC = -1
         ELSE
            JFIRST = 1
            JLAST = N
            JINC = 1
         END IF
*
         IF( TSCAL.NE.ONE ) THEN
            GROW = ZERO
            GO TO 60
         END IF
*
         IF( NOUNIT ) THEN
*
*           A is non-unit triangular.
*
*           Compute GROW = 1/G(j) and XBND = 1/M(j).
*           Initially, G(0) = max{x(i), i=1,...,n}.
*
            GROW = HALF / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 40 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 60
*
               TJJS = A( J, J )
               TJJ = CABS1( TJJS )
*
               IF( TJJ.GE.SMLNUM ) THEN
*
*                 M(j) = G(j-1) / abs(A(j,j))
*
                  XBND = MIN( XBND, MIN( ONE, TJJ )*GROW )
               ELSE
*
*                 M(j) could overflow, set XBND to 0.
*
                  XBND = ZERO
               END IF
*
               IF( TJJ+CNORM( J ).GE.SMLNUM ) THEN
*
*                 G(j) = G(j-1)*( 1 + CNORM(j) / abs(A(j,j)) )
*
                  GROW = GROW*( TJJ / ( TJJ+CNORM( J ) ) )
               ELSE
*
*                 G(j) could overflow, set GROW to 0.
*
                  GROW = ZERO
               END IF
   40       CONTINUE
            GROW = XBND
         ELSE
*
*           A is unit triangular.
*
*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
*
            GROW = MIN( ONE, HALF / MAX( XBND, SMLNUM ) )
            DO 50 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 60
*
*              G(j) = G(j-1)*( 1 + CNORM(j) )
*
               GROW = GROW*( ONE / ( ONE+CNORM( J ) ) )
   50       CONTINUE
         END IF
   60    CONTINUE
*
      ELSE
*
*        Compute the growth in A**T * x = b  or  A**H * x = b.
*
         IF( UPPER ) THEN
            JFIRST = 1
            JLAST = N
            JINC = 1
         ELSE
            JFIRST = N
            JLAST = 1
            JINC = -1
         END IF
*
         IF( TSCAL.NE.ONE ) THEN
            GROW = ZERO
            GO TO 90
         END IF
*
         IF( NOUNIT ) THEN
*
*           A is non-unit triangular.
*
*           Compute GROW = 1/G(j) and XBND = 1/M(j).
*           Initially, M(0) = max{x(i), i=1,...,n}.
*
            GROW = HALF / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 70 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 90
*
*              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) )
*
               XJ = ONE + CNORM( J )
               GROW = MIN( GROW, XBND / XJ )
*
               TJJS = A( J, J )
               TJJ = CABS1( TJJS )
*
               IF( TJJ.GE.SMLNUM ) THEN
*
*                 M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j))
*
                  IF( XJ.GT.TJJ )
     $               XBND = XBND*( TJJ / XJ )
               ELSE
*
*                 M(j) could overflow, set XBND to 0.
*
                  XBND = ZERO
               END IF
   70       CONTINUE
            GROW = MIN( GROW, XBND )
         ELSE
*
*           A is unit triangular.
*
*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
*
            GROW = MIN( ONE, HALF / MAX( XBND, SMLNUM ) )
            DO 80 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 90
*
*              G(j) = ( 1 + CNORM(j) )*G(j-1)
*
               XJ = ONE + CNORM( J )
               GROW = GROW / XJ
   80       CONTINUE
         END IF
   90    CONTINUE
      END IF
*
      IF( ( GROW*TSCAL ).GT.SMLNUM ) THEN
*
*        Use the Level 2 BLAS solve if the reciprocal of the bound on
*        elements of X is not too small.
*
         CALL ZTRSV( UPLO, TRANS, DIAG, N, A, LDA, X, 1 )
      ELSE
*
*        Use a Level 1 BLAS solve, scaling intermediate results.
*
         IF( XMAX.GT.BIGNUM*HALF ) THEN
*
*           Scale X so that its components are less than or equal to
*           BIGNUM in absolute value.
*
            SCALE = ( BIGNUM*HALF ) / XMAX
            CALL ZDSCAL( N, SCALE, X, 1 )
            XMAX = BIGNUM
         ELSE
            XMAX = XMAX*TWO
         END IF
*
         IF( NOTRAN ) THEN
*
*           Solve A * x = b
*
            DO 120 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) / A(j,j), scaling x if necessary.
*
               XJ = CABS1( X( J ) )
               IF( NOUNIT ) THEN
                  TJJS = A( J, J )*TSCAL
               ELSE
                  TJJS = TSCAL
                  IF( TSCAL.EQ.ONE )
     $               GO TO 110
               END IF
               TJJ = CABS1( TJJS )
               IF( TJJ.GT.SMLNUM ) THEN
*
*                    abs(A(j,j)) > SMLNUM:
*
                  IF( TJJ.LT.ONE ) THEN
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                          Scale x by 1/b(j).
*
                        REC = ONE / XJ
                        CALL ZDSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  X( J ) = ZLADIV( X( J ), TJJS )
                  XJ = CABS1( X( J ) )
               ELSE IF( TJJ.GT.ZERO ) THEN
*
*                    0 < abs(A(j,j)) <= SMLNUM:
*
                  IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                       Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM
*                       to avoid overflow when dividing by A(j,j).
*
                     REC = ( TJJ*BIGNUM ) / XJ
                     IF( CNORM( J ).GT.ONE ) THEN
*
*                          Scale by 1/CNORM(j) to avoid overflow when
*                          multiplying x(j) times column j.
*
                        REC = REC / CNORM( J )
                     END IF
                     CALL ZDSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
                  X( J ) = ZLADIV( X( J ), TJJS )
                  XJ = CABS1( X( J ) )
               ELSE
*
*                    A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                    scale = 0, and compute a solution to A*x = 0.
*
                  DO 100 I = 1, N
                     X( I ) = ZERO
  100             CONTINUE
                  X( J ) = ONE
                  XJ = ONE
                  SCALE = ZERO
                  XMAX = ZERO
               END IF
  110          CONTINUE
*
*              Scale x if necessary to avoid overflow when adding a
*              multiple of column j of A.
*
               IF( XJ.GT.ONE ) THEN
                  REC = ONE / XJ
                  IF( CNORM( J ).GT.( BIGNUM-XMAX )*REC ) THEN
*
*                    Scale x by 1/(2*abs(x(j))).
*
                     REC = REC*HALF
                     CALL ZDSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                  END IF
               ELSE IF( XJ*CNORM( J ).GT.( BIGNUM-XMAX ) ) THEN
*
*                 Scale x by 1/2.
*
                  CALL ZDSCAL( N, HALF, X, 1 )
                  SCALE = SCALE*HALF
               END IF
*
               IF( UPPER ) THEN
                  IF( J.GT.1 ) THEN
*
*                    Compute the update
*                       x(1:j-1) := x(1:j-1) - x(j) * A(1:j-1,j)
*
                     CALL ZAXPY( J-1, -X( J )*TSCAL, A( 1, J ), 1, X,
     $                           1 )
                     I = IZAMAX( J-1, X, 1 )
                     XMAX = CABS1( X( I ) )
                  END IF
               ELSE
                  IF( J.LT.N ) THEN
*
*                    Compute the update
*                       x(j+1:n) := x(j+1:n) - x(j) * A(j+1:n,j)
*
                     CALL ZAXPY( N-J, -X( J )*TSCAL, A( J+1, J ), 1,
     $                           X( J+1 ), 1 )
                     I = J + IZAMAX( N-J, X( J+1 ), 1 )
                     XMAX = CABS1( X( I ) )
                  END IF
               END IF
  120       CONTINUE
*
         ELSE IF( LSAME( TRANS, 'T' ) ) THEN
*
*           Solve A**T * x = b
*
            DO 170 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) - sum A(k,j)*x(k).
*                                    k<>j
*
               XJ = CABS1( X( J ) )
               USCAL = TSCAL
               REC = ONE / MAX( XMAX, ONE )
               IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN
*
*                 If x(j) could overflow, scale x by 1/(2*XMAX).
*
                  REC = REC*HALF
                  IF( NOUNIT ) THEN
                     TJJS = A( J, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                  END IF
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.ONE ) THEN
*
*                       Divide by A(j,j) when scaling x if A(j,j) > 1.
*
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = ZLADIV( USCAL, TJJS )
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL ZDSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
*
               CSUMJ = ZERO
               IF( USCAL.EQ.DCMPLX( ONE ) ) THEN
*
*                 If the scaling needed for A in the dot product is 1,
*                 call ZDOTU to perform the dot product.
*
                  IF( UPPER ) THEN
                     CSUMJ = ZDOTU( J-1, A( 1, J ), 1, X, 1 )
                  ELSE IF( J.LT.N ) THEN
                     CSUMJ = ZDOTU( N-J, A( J+1, J ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
*
*                 Otherwise, use in-line code for the dot product.
*
                  IF( UPPER ) THEN
                     DO 130 I = 1, J - 1
                        CSUMJ = CSUMJ + ( A( I, J )*USCAL )*X( I )
  130                CONTINUE
                  ELSE IF( J.LT.N ) THEN
                     DO 140 I = J + 1, N
                        CSUMJ = CSUMJ + ( A( I, J )*USCAL )*X( I )
  140                CONTINUE
                  END IF
               END IF
*
               IF( USCAL.EQ.DCMPLX( TSCAL ) ) THEN
*
*                 Compute x(j) := ( x(j) - CSUMJ ) / A(j,j) if 1/A(j,j)
*                 was not used to scale the dotproduct.
*
                  X( J ) = X( J ) - CSUMJ
                  XJ = CABS1( X( J ) )
                  IF( NOUNIT ) THEN
                     TJJS = A( J, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     $                  GO TO 160
                  END IF
*
*                    Compute x(j) = x(j) / A(j,j), scaling if necessary.
*
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.SMLNUM ) THEN
*
*                       abs(A(j,j)) > SMLNUM:
*
                     IF( TJJ.LT.ONE ) THEN
                        IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                             Scale X by 1/abs(x(j)).
*
                           REC = ONE / XJ
                           CALL ZDSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = ZLADIV( X( J ), TJJS )
                  ELSE IF( TJJ.GT.ZERO ) THEN
*
*                       0 < abs(A(j,j)) <= SMLNUM:
*
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM.
*
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL ZDSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = ZLADIV( X( J ), TJJS )
                  ELSE
*
*                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                       scale = 0 and compute a solution to A**T *x = 0.
*
                     DO 150 I = 1, N
                        X( I ) = ZERO
  150                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  160             CONTINUE
               ELSE
*
*                 Compute x(j) := x(j) / A(j,j) - CSUMJ if the dot
*                 product has already been divided by 1/A(j,j).
*
                  X( J ) = ZLADIV( X( J ), TJJS ) - CSUMJ
               END IF
               XMAX = MAX( XMAX, CABS1( X( J ) ) )
  170       CONTINUE
*
         ELSE
*
*           Solve A**H * x = b
*
            DO 220 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) - sum A(k,j)*x(k).
*                                    k<>j
*
               XJ = CABS1( X( J ) )
               USCAL = TSCAL
               REC = ONE / MAX( XMAX, ONE )
               IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN
*
*                 If x(j) could overflow, scale x by 1/(2*XMAX).
*
                  REC = REC*HALF
                  IF( NOUNIT ) THEN
                     TJJS = DCONJG( A( J, J ) )*TSCAL
                  ELSE
                     TJJS = TSCAL
                  END IF
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.ONE ) THEN
*
*                       Divide by A(j,j) when scaling x if A(j,j) > 1.
*
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = ZLADIV( USCAL, TJJS )
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL ZDSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
*
               CSUMJ = ZERO
               IF( USCAL.EQ.DCMPLX( ONE ) ) THEN
*
*                 If the scaling needed for A in the dot product is 1,
*                 call ZDOTC to perform the dot product.
*
                  IF( UPPER ) THEN
                     CSUMJ = ZDOTC( J-1, A( 1, J ), 1, X, 1 )
                  ELSE IF( J.LT.N ) THEN
                     CSUMJ = ZDOTC( N-J, A( J+1, J ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
*
*                 Otherwise, use in-line code for the dot product.
*
                  IF( UPPER ) THEN
                     DO 180 I = 1, J - 1
                        CSUMJ = CSUMJ + ( DCONJG( A( I, J ) )*USCAL )*
     $                          X( I )
  180                CONTINUE
                  ELSE IF( J.LT.N ) THEN
                     DO 190 I = J + 1, N
                        CSUMJ = CSUMJ + ( DCONJG( A( I, J ) )*USCAL )*
     $                          X( I )
  190                CONTINUE
                  END IF
               END IF
*
               IF( USCAL.EQ.DCMPLX( TSCAL ) ) THEN
*
*                 Compute x(j) := ( x(j) - CSUMJ ) / A(j,j) if 1/A(j,j)
*                 was not used to scale the dotproduct.
*
                  X( J ) = X( J ) - CSUMJ
                  XJ = CABS1( X( J ) )
                  IF( NOUNIT ) THEN
                     TJJS = DCONJG( A( J, J ) )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     $                  GO TO 210
                  END IF
*
*                    Compute x(j) = x(j) / A(j,j), scaling if necessary.
*
                  TJJ = CABS1( TJJS )
                  IF( TJJ.GT.SMLNUM ) THEN
*
*                       abs(A(j,j)) > SMLNUM:
*
                     IF( TJJ.LT.ONE ) THEN
                        IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                             Scale X by 1/abs(x(j)).
*
                           REC = ONE / XJ
                           CALL ZDSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = ZLADIV( X( J ), TJJS )
                  ELSE IF( TJJ.GT.ZERO ) THEN
*
*                       0 < abs(A(j,j)) <= SMLNUM:
*
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM.
*
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL ZDSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = ZLADIV( X( J ), TJJS )
                  ELSE
*
*                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                       scale = 0 and compute a solution to A**H *x = 0.
*
                     DO 200 I = 1, N
                        X( I ) = ZERO
  200                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  210             CONTINUE
               ELSE
*
*                 Compute x(j) := x(j) / A(j,j) - CSUMJ if the dot
*                 product has already been divided by 1/A(j,j).
*
                  X( J ) = ZLADIV( X( J ), TJJS ) - CSUMJ
               END IF
               XMAX = MAX( XMAX, CABS1( X( J ) ) )
  220       CONTINUE
         END IF
         SCALE = SCALE / TSCAL
      END IF
*
*     Scale the column norms by 1/TSCAL for return.
*
      IF( TSCAL.NE.ONE ) THEN
         CALL DSCAL( N, ONE / TSCAL, CNORM, 1 )
      END IF
*
      RETURN
*
*     End of ZLATRS
*
      END
      SUBROUTINE ZSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * )
      COMPLEX*16         Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  ZSTEQR computes all eigenvalues and, optionally, eigenvectors of a
*  symmetric tridiagonal matrix using the implicit QL or QR method.
*  The eigenvectors of a full or band complex Hermitian matrix can also
*  be found if ZHETRD or ZHPTRD or ZHBTRD has been used to reduce this
*  matrix to tridiagonal form.
*
*  Arguments
*  =========
*
*  COMPZ   (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only.
*          = 'V':  Compute eigenvalues and eigenvectors of the original
*                  Hermitian matrix.  On entry, Z must contain the
*                  unitary matrix used to reduce the original matrix
*                  to tridiagonal form.
*          = 'I':  Compute eigenvalues and eigenvectors of the
*                  tridiagonal matrix.  Z is initialized to the identity
*                  matrix.
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the diagonal elements of the tridiagonal matrix.
*          On exit, if INFO = 0, the eigenvalues in ascending order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix.
*          On exit, E has been destroyed.
*
*  Z       (input/output) COMPLEX*16 array, dimension (LDZ, N)
*          On entry, if  COMPZ = 'V', then Z contains the unitary
*          matrix used in the reduction to tridiagonal form.
*          On exit, if INFO = 0, then if COMPZ = 'V', Z contains the
*          orthonormal eigenvectors of the original Hermitian matrix,
*          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
*          of the symmetric tridiagonal matrix.
*          If COMPZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          eigenvectors are desired, then  LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,2*N-2))
*          If COMPZ = 'N', then WORK is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  the algorithm has failed to find all the eigenvalues in
*                a total of 30*N iterations; if INFO = i, then i
*                elements of E have not converged to zero; on exit, D
*                and E contain the elements of a symmetric tridiagonal
*                matrix which is unitarily similar to the original
*                matrix.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   THREE = 3.0D0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D0, 0.0D0 ),
     $                   CONE = ( 1.0D0, 0.0D0 ) )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 30 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ICOMPZ, II, ISCALE, J, JTOT, K, L, L1, LEND,
     $                   LENDM1, LENDP1, LENDSV, LM1, LSV, M, MM, MM1,
     $                   NM1, NMAXIT
      DOUBLE PRECISION   ANORM, B, C, EPS, EPS2, F, G, P, R, RT1, RT2,
     $                   S, SAFMAX, SAFMIN, SSFMAX, SSFMIN, TST
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST, DLAPY2
      EXTERNAL           LSAME, DLAMCH, DLANST, DLAPY2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAE2, DLAEV2, DLARTG, DLASCL, DLASRT, XERBLA,
     $                   ZLASET, ZLASR, ZSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( LSAME( COMPZ, 'N' ) ) THEN
         ICOMPZ = 0
      ELSE IF( LSAME( COMPZ, 'V' ) ) THEN
         ICOMPZ = 1
      ELSE IF( LSAME( COMPZ, 'I' ) ) THEN
         ICOMPZ = 2
      ELSE
         ICOMPZ = -1
      END IF
      IF( ICOMPZ.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( ( LDZ.LT.1 ) .OR. ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1,
     $         N ) ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZSTEQR', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.EQ.2 )
     $      Z( 1, 1 ) = CONE
         RETURN
      END IF
*
*     Determine the unit roundoff and over/underflow thresholds.
*
      EPS = DLAMCH( 'E' )
      EPS2 = EPS**2
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      SSFMAX = SQRT( SAFMAX ) / THREE
      SSFMIN = SQRT( SAFMIN ) / EPS2
*
*     Compute the eigenvalues and eigenvectors of the tridiagonal
*     matrix.
*
      IF( ICOMPZ.EQ.2 )
     $   CALL ZLASET( 'Full', N, N, CZERO, CONE, Z, LDZ )
*
      NMAXIT = N*MAXIT
      JTOT = 0
*
*     Determine where the matrix splits and choose QL or QR iteration
*     for each block, according to whether top or bottom diagonal
*     element is smaller.
*
      L1 = 1
      NM1 = N - 1
*
   10 CONTINUE
      IF( L1.GT.N )
     $   GO TO 160
      IF( L1.GT.1 )
     $   E( L1-1 ) = ZERO
      IF( L1.LE.NM1 ) THEN
         DO 20 M = L1, NM1
            TST = ABS( E( M ) )
            IF( TST.EQ.ZERO )
     $         GO TO 30
            IF( TST.LE.( SQRT( ABS( D( M ) ) )*SQRT( ABS( D( M+
     $          1 ) ) ) )*EPS ) THEN
               E( M ) = ZERO
               GO TO 30
            END IF
   20    CONTINUE
      END IF
      M = N
*
   30 CONTINUE
      L = L1
      LSV = L
      LEND = M
      LENDSV = LEND
      L1 = M + 1
      IF( LEND.EQ.L )
     $   GO TO 10
*
*     Scale submatrix in rows and columns L to LEND
*
      ANORM = DLANST( 'I', LEND-L+1, D( L ), E( L ) )
      ISCALE = 0
      IF( ANORM.EQ.ZERO )
     $   GO TO 10
      IF( ANORM.GT.SSFMAX ) THEN
         ISCALE = 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L, 1, E( L ), N,
     $                INFO )
      ELSE IF( ANORM.LT.SSFMIN ) THEN
         ISCALE = 2
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L+1, 1, D( L ), N,
     $                INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L, 1, E( L ), N,
     $                INFO )
      END IF
*
*     Choose between QL and QR iteration
*
      IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN
         LEND = LSV
         L = LENDSV
      END IF
*
      IF( LEND.GT.L ) THEN
*
*        QL Iteration
*
*        Look for small subdiagonal element.
*
   40    CONTINUE
         IF( L.NE.LEND ) THEN
            LENDM1 = LEND - 1
            DO 50 M = L, LENDM1
               TST = ABS( E( M ) )**2
               IF( TST.LE.( EPS2*ABS( D( M ) ) )*ABS( D( M+1 ) )+
     $             SAFMIN )GO TO 60
   50       CONTINUE
         END IF
*
         M = LEND
*
   60    CONTINUE
         IF( M.LT.LEND )
     $      E( M ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 80
*
*        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
*        to compute its eigensystem.
*
         IF( M.EQ.L+1 ) THEN
            IF( ICOMPZ.GT.0 ) THEN
               CALL DLAEV2( D( L ), E( L ), D( L+1 ), RT1, RT2, C, S )
               WORK( L ) = C
               WORK( N-1+L ) = S
               CALL ZLASR( 'R', 'V', 'B', N, 2, WORK( L ),
     $                     WORK( N-1+L ), Z( 1, L ), LDZ )
            ELSE
               CALL DLAE2( D( L ), E( L ), D( L+1 ), RT1, RT2 )
            END IF
            D( L ) = RT1
            D( L+1 ) = RT2
            E( L ) = ZERO
            L = L + 2
            IF( L.LE.LEND )
     $         GO TO 40
            GO TO 140
         END IF
*
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 140
         JTOT = JTOT + 1
*
*        Form shift.
*
         G = ( D( L+1 )-P ) / ( TWO*E( L ) )
         R = DLAPY2( G, ONE )
         G = D( M ) - P + ( E( L ) / ( G+SIGN( R, G ) ) )
*
         S = ONE
         C = ONE
         P = ZERO
*
*        Inner loop
*
         MM1 = M - 1
         DO 70 I = MM1, L, -1
            F = S*E( I )
            B = C*E( I )
            CALL DLARTG( G, F, C, S, R )
            IF( I.NE.M-1 )
     $         E( I+1 ) = R
            G = D( I+1 ) - P
            R = ( D( I )-G )*S + TWO*C*B
            P = S*R
            D( I+1 ) = G + P
            G = C*R - B
*
*           If eigenvectors are desired, then save rotations.
*
            IF( ICOMPZ.GT.0 ) THEN
               WORK( I ) = C
               WORK( N-1+I ) = -S
            END IF
*
   70    CONTINUE
*
*        If eigenvectors are desired, then apply saved rotations.
*
         IF( ICOMPZ.GT.0 ) THEN
            MM = M - L + 1
            CALL ZLASR( 'R', 'V', 'B', N, MM, WORK( L ), WORK( N-1+L ),
     $                  Z( 1, L ), LDZ )
         END IF
*
         D( L ) = D( L ) - P
         E( L ) = G
         GO TO 40
*
*        Eigenvalue found.
*
   80    CONTINUE
         D( L ) = P
*
         L = L + 1
         IF( L.LE.LEND )
     $      GO TO 40
         GO TO 140
*
      ELSE
*
*        QR Iteration
*
*        Look for small superdiagonal element.
*
   90    CONTINUE
         IF( L.NE.LEND ) THEN
            LENDP1 = LEND + 1
            DO 100 M = L, LENDP1, -1
               TST = ABS( E( M-1 ) )**2
               IF( TST.LE.( EPS2*ABS( D( M ) ) )*ABS( D( M-1 ) )+
     $             SAFMIN )GO TO 110
  100       CONTINUE
         END IF
*
         M = LEND
*
  110    CONTINUE
         IF( M.GT.LEND )
     $      E( M-1 ) = ZERO
         P = D( L )
         IF( M.EQ.L )
     $      GO TO 130
*
*        If remaining matrix is 2-by-2, use DLAE2 or SLAEV2
*        to compute its eigensystem.
*
         IF( M.EQ.L-1 ) THEN
            IF( ICOMPZ.GT.0 ) THEN
               CALL DLAEV2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2, C, S )
               WORK( M ) = C
               WORK( N-1+M ) = S
               CALL ZLASR( 'R', 'V', 'F', N, 2, WORK( M ),
     $                     WORK( N-1+M ), Z( 1, L-1 ), LDZ )
            ELSE
               CALL DLAE2( D( L-1 ), E( L-1 ), D( L ), RT1, RT2 )
            END IF
            D( L-1 ) = RT1
            D( L ) = RT2
            E( L-1 ) = ZERO
            L = L - 2
            IF( L.GE.LEND )
     $         GO TO 90
            GO TO 140
         END IF
*
         IF( JTOT.EQ.NMAXIT )
     $      GO TO 140
         JTOT = JTOT + 1
*
*        Form shift.
*
         G = ( D( L-1 )-P ) / ( TWO*E( L-1 ) )
         R = DLAPY2( G, ONE )
         G = D( M ) - P + ( E( L-1 ) / ( G+SIGN( R, G ) ) )
*
         S = ONE
         C = ONE
         P = ZERO
*
*        Inner loop
*
         LM1 = L - 1
         DO 120 I = M, LM1
            F = S*E( I )
            B = C*E( I )
            CALL DLARTG( G, F, C, S, R )
            IF( I.NE.M )
     $         E( I-1 ) = R
            G = D( I ) - P
            R = ( D( I+1 )-G )*S + TWO*C*B
            P = S*R
            D( I ) = G + P
            G = C*R - B
*
*           If eigenvectors are desired, then save rotations.
*
            IF( ICOMPZ.GT.0 ) THEN
               WORK( I ) = C
               WORK( N-1+I ) = S
            END IF
*
  120    CONTINUE
*
*        If eigenvectors are desired, then apply saved rotations.
*
         IF( ICOMPZ.GT.0 ) THEN
            MM = L - M + 1
            CALL ZLASR( 'R', 'V', 'F', N, MM, WORK( M ), WORK( N-1+M ),
     $                  Z( 1, M ), LDZ )
         END IF
*
         D( L ) = D( L ) - P
         E( LM1 ) = G
         GO TO 90
*
*        Eigenvalue found.
*
  130    CONTINUE
         D( L ) = P
*
         L = L - 1
         IF( L.GE.LEND )
     $      GO TO 90
         GO TO 140
*
      END IF
*
*     Undo scaling if necessary
*
  140 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV, 1, E( LSV ),
     $                N, INFO )
      ELSE IF( ISCALE.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV+1, 1,
     $                D( LSV ), N, INFO )
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV, 1, E( LSV ),
     $                N, INFO )
      END IF
*
*     Check for no convergence to an eigenvalue after a total
*     of N*MAXIT iterations.
*
      IF( JTOT.EQ.NMAXIT ) THEN
         DO 150 I = 1, N - 1
            IF( E( I ).NE.ZERO )
     $         INFO = INFO + 1
  150    CONTINUE
         RETURN
      END IF
      GO TO 10
*
*     Order eigenvalues and eigenvectors.
*
  160 CONTINUE
      IF( ICOMPZ.EQ.0 ) THEN
*
*        Use Quick Sort
*
         CALL DLASRT( 'I', N, D, INFO )
*
      ELSE
*
*        Use Selection Sort to minimize swaps of eigenvectors
*
         DO 180 II = 2, N
            I = II - 1
            K = I
            P = D( I )
            DO 170 J = II, N
               IF( D( J ).LT.P ) THEN
                  K = J
                  P = D( J )
               END IF
  170       CONTINUE
            IF( K.NE.I ) THEN
               D( K ) = D( I )
               D( I ) = P
               CALL ZSWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 )
            END IF
  180    CONTINUE
      END IF
      RETURN
*
*     End of ZSTEQR
*
      END
      SUBROUTINE ZTREVC( SIDE, HOWMNY, SELECT, N, T, LDT, VL, LDVL, VR,
     $                   LDVR, MM, M, WORK, RWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          HOWMNY, SIDE
      INTEGER            INFO, LDT, LDVL, LDVR, M, MM, N
*     ..
*     .. Array Arguments ..
      LOGICAL            SELECT( * )
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         T( LDT, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZTREVC computes some or all of the right and/or left eigenvectors of
*  a complex upper triangular matrix T.
*
*  The right eigenvector x and the left eigenvector y of T corresponding
*  to an eigenvalue w are defined by:
*
*               T*x = w*x,     y'*T = w*y'
*
*  where y' denotes the conjugate transpose of the vector y.
*
*  If all eigenvectors are requested, the routine may either return the
*  matrices X and/or Y of right or left eigenvectors of T, or the
*  products Q*X and/or Q*Y, where Q is an input unitary
*  matrix. If T was obtained from the Schur factorization of an
*  original matrix A = Q*T*Q', then Q*X and Q*Y are the matrices of
*  right or left eigenvectors of A.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'R':  compute right eigenvectors only;
*          = 'L':  compute left eigenvectors only;
*          = 'B':  compute both right and left eigenvectors.
*
*  HOWMNY  (input) CHARACTER*1
*          = 'A':  compute all right and/or left eigenvectors;
*          = 'B':  compute all right and/or left eigenvectors,
*                  and backtransform them using the input matrices
*                  supplied in VR and/or VL;
*          = 'S':  compute selected right and/or left eigenvectors,
*                  specified by the logical array SELECT.
*
*  SELECT  (input) LOGICAL array, dimension (N)
*          If HOWMNY = 'S', SELECT specifies the eigenvectors to be
*          computed.
*          If HOWMNY = 'A' or 'B', SELECT is not referenced.
*          To select the eigenvector corresponding to the j-th
*          eigenvalue, SELECT(j) must be set to .TRUE..
*
*  N       (input) INTEGER
*          The order of the matrix T. N >= 0.
*
*  T       (input/output) COMPLEX*16 array, dimension (LDT,N)
*          The upper triangular matrix T.  T is modified, but restored
*          on exit.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= max(1,N).
*
*  VL      (input/output) COMPLEX*16 array, dimension (LDVL,MM)
*          On entry, if SIDE = 'L' or 'B' and HOWMNY = 'B', VL must
*          contain an N-by-N matrix Q (usually the unitary matrix Q of
*          Schur vectors returned by ZHSEQR).
*          On exit, if SIDE = 'L' or 'B', VL contains:
*          if HOWMNY = 'A', the matrix Y of left eigenvectors of T;
*                           VL is lower triangular. The i-th column
*                           VL(i) of VL is the eigenvector corresponding
*                           to T(i,i).
*          if HOWMNY = 'B', the matrix Q*Y;
*          if HOWMNY = 'S', the left eigenvectors of T specified by
*                           SELECT, stored consecutively in the columns
*                           of VL, in the same order as their
*                           eigenvalues.
*          If SIDE = 'R', VL is not referenced.
*
*  LDVL    (input) INTEGER
*          The leading dimension of the array VL.  LDVL >= max(1,N) if
*          SIDE = 'L' or 'B'; LDVL >= 1 otherwise.
*
*  VR      (input/output) COMPLEX*16 array, dimension (LDVR,MM)
*          On entry, if SIDE = 'R' or 'B' and HOWMNY = 'B', VR must
*          contain an N-by-N matrix Q (usually the unitary matrix Q of
*          Schur vectors returned by ZHSEQR).
*          On exit, if SIDE = 'R' or 'B', VR contains:
*          if HOWMNY = 'A', the matrix X of right eigenvectors of T;
*                           VR is upper triangular. The i-th column
*                           VR(i) of VR is the eigenvector corresponding
*                           to T(i,i).
*          if HOWMNY = 'B', the matrix Q*X;
*          if HOWMNY = 'S', the right eigenvectors of T specified by
*                           SELECT, stored consecutively in the columns
*                           of VR, in the same order as their
*                           eigenvalues.
*          If SIDE = 'L', VR is not referenced.
*
*  LDVR    (input) INTEGER
*          The leading dimension of the array VR.  LDVR >= max(1,N) if
*           SIDE = 'R' or 'B'; LDVR >= 1 otherwise.
*
*  MM      (input) INTEGER
*          The number of columns in the arrays VL and/or VR. MM >= M.
*
*  M       (output) INTEGER
*          The number of columns in the arrays VL and/or VR actually
*          used to store the eigenvectors.  If HOWMNY = 'A' or 'B', M
*          is set to N.  Each selected eigenvector occupies one
*          column.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (2*N)
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  Further Details
*  ===============
*
*  The algorithm used in this program is basically backward (forward)
*  substitution, with scaling to make the the code robust against
*  possible overflow.
*
*  Each eigenvector is normalized so that the element of largest
*  magnitude has magnitude 1; here the magnitude of a complex number
*  (x,y) is taken to be |x| + |y|.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CMZERO, CMONE
      PARAMETER          ( CMZERO = ( 0.0D+0, 0.0D+0 ),
     $                   CMONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLV, BOTHV, LEFTV, OVER, RIGHTV, SOMEV
      INTEGER            I, II, IS, J, K, KI
      DOUBLE PRECISION   OVFL, REMAX, SCALE, SMIN, SMLNUM, ULP, UNFL
      COMPLEX*16         CDUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IZAMAX
      DOUBLE PRECISION   DLAMCH, DZASUM
      EXTERNAL           LSAME, IZAMAX, DLAMCH, DZASUM
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZCOPY, ZDSCAL, ZGEMV, ZLATRS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DCMPLX, DCONJG, DIMAG, MAX
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
*     ..
*     .. Executable Statements ..
*
*     Decode and test the input parameters
*
      BOTHV = LSAME( SIDE, 'B' )
      RIGHTV = LSAME( SIDE, 'R' ) .OR. BOTHV
      LEFTV = LSAME( SIDE, 'L' ) .OR. BOTHV
*
      ALLV = LSAME( HOWMNY, 'A' )
      OVER = LSAME( HOWMNY, 'B' )
      SOMEV = LSAME( HOWMNY, 'S' )
*
*     Set M to the number of columns required to store the selected
*     eigenvectors.
*
      IF( SOMEV ) THEN
         M = 0
         DO 10 J = 1, N
            IF( SELECT( J ) )
     $         M = M + 1
   10    CONTINUE
      ELSE
         M = N
      END IF
*
      INFO = 0
      IF( .NOT.RIGHTV .AND. .NOT.LEFTV ) THEN
         INFO = -1
      ELSE IF( .NOT.ALLV .AND. .NOT.OVER .AND. .NOT.SOMEV ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDT.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDVL.LT.1 .OR. ( LEFTV .AND. LDVL.LT.N ) ) THEN
         INFO = -8
      ELSE IF( LDVR.LT.1 .OR. ( RIGHTV .AND. LDVR.LT.N ) ) THEN
         INFO = -10
      ELSE IF( MM.LT.M ) THEN
         INFO = -11
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTREVC', -INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Set the constants to control overflow.
*
      UNFL = DLAMCH( 'Safe minimum' )
      OVFL = ONE / UNFL
      CALL DLABAD( UNFL, OVFL )
      ULP = DLAMCH( 'Precision' )
      SMLNUM = UNFL*( N / ULP )
*
*     Store the diagonal elements of T in working array WORK.
*
      DO 20 I = 1, N
         WORK( I+N ) = T( I, I )
   20 CONTINUE
*
*     Compute 1-norm of each column of strictly upper triangular
*     part of T to control overflow in triangular solver.
*
      RWORK( 1 ) = ZERO
      DO 30 J = 2, N
         RWORK( J ) = DZASUM( J-1, T( 1, J ), 1 )
   30 CONTINUE
*
      IF( RIGHTV ) THEN
*
*        Compute right eigenvectors.
*
         IS = M
         DO 80 KI = N, 1, -1
*
            IF( SOMEV ) THEN
               IF( .NOT.SELECT( KI ) )
     $            GO TO 80
            END IF
            SMIN = MAX( ULP*( CABS1( T( KI, KI ) ) ), SMLNUM )
*
            WORK( 1 ) = CMONE
*
*           Form right-hand side.
*
            DO 40 K = 1, KI - 1
               WORK( K ) = -T( K, KI )
   40       CONTINUE
*
*           Solve the triangular system:
*              (T(1:KI-1,1:KI-1) - T(KI,KI))*X = SCALE*WORK.
*
            DO 50 K = 1, KI - 1
               T( K, K ) = T( K, K ) - T( KI, KI )
               IF( CABS1( T( K, K ) ).LT.SMIN )
     $            T( K, K ) = SMIN
   50       CONTINUE
*
            IF( KI.GT.1 ) THEN
               CALL ZLATRS( 'Upper', 'No transpose', 'Non-unit', 'Y',
     $                      KI-1, T, LDT, WORK( 1 ), SCALE, RWORK,
     $                      INFO )
               WORK( KI ) = SCALE
            END IF
*
*           Copy the vector x or Q*x to VR and normalize.
*
            IF( .NOT.OVER ) THEN
               CALL ZCOPY( KI, WORK( 1 ), 1, VR( 1, IS ), 1 )
*
               II = IZAMAX( KI, VR( 1, IS ), 1 )
               REMAX = ONE / CABS1( VR( II, IS ) )
               CALL ZDSCAL( KI, REMAX, VR( 1, IS ), 1 )
*
               DO 60 K = KI + 1, N
                  VR( K, IS ) = CMZERO
   60          CONTINUE
            ELSE
               IF( KI.GT.1 )
     $            CALL ZGEMV( 'N', N, KI-1, CMONE, VR, LDVR, WORK( 1 ),
     $                        1, DCMPLX( SCALE ), VR( 1, KI ), 1 )
*
               II = IZAMAX( N, VR( 1, KI ), 1 )
               REMAX = ONE / CABS1( VR( II, KI ) )
               CALL ZDSCAL( N, REMAX, VR( 1, KI ), 1 )
            END IF
*
*           Set back the original diagonal elements of T.
*
            DO 70 K = 1, KI - 1
               T( K, K ) = WORK( K+N )
   70       CONTINUE
*
            IS = IS - 1
   80    CONTINUE
      END IF
*
      IF( LEFTV ) THEN
*
*        Compute left eigenvectors.
*
         IS = 1
         DO 130 KI = 1, N
*
            IF( SOMEV ) THEN
               IF( .NOT.SELECT( KI ) )
     $            GO TO 130
            END IF
            SMIN = MAX( ULP*( CABS1( T( KI, KI ) ) ), SMLNUM )
*
            WORK( N ) = CMONE
*
*           Form right-hand side.
*
            DO 90 K = KI + 1, N
               WORK( K ) = -DCONJG( T( KI, K ) )
   90       CONTINUE
*
*           Solve the triangular system:
*              (T(KI+1:N,KI+1:N) - T(KI,KI))'*X = SCALE*WORK.
*
            DO 100 K = KI + 1, N
               T( K, K ) = T( K, K ) - T( KI, KI )
               IF( CABS1( T( K, K ) ).LT.SMIN )
     $            T( K, K ) = SMIN
  100       CONTINUE
*
            IF( KI.LT.N ) THEN
               CALL ZLATRS( 'Upper', 'Conjugate transpose', 'Non-unit',
     $                      'Y', N-KI, T( KI+1, KI+1 ), LDT,
     $                      WORK( KI+1 ), SCALE, RWORK, INFO )
               WORK( KI ) = SCALE
            END IF
*
*           Copy the vector x or Q*x to VL and normalize.
*
            IF( .NOT.OVER ) THEN
               CALL ZCOPY( N-KI+1, WORK( KI ), 1, VL( KI, IS ), 1 )
*
               II = IZAMAX( N-KI+1, VL( KI, IS ), 1 ) + KI - 1
               REMAX = ONE / CABS1( VL( II, IS ) )
               CALL ZDSCAL( N-KI+1, REMAX, VL( KI, IS ), 1 )
*
               DO 110 K = 1, KI - 1
                  VL( K, IS ) = CMZERO
  110          CONTINUE
            ELSE
               IF( KI.LT.N )
     $            CALL ZGEMV( 'N', N, N-KI, CMONE, VL( 1, KI+1 ), LDVL,
     $                        WORK( KI+1 ), 1, DCMPLX( SCALE ),
     $                        VL( 1, KI ), 1 )
*
               II = IZAMAX( N, VL( 1, KI ), 1 )
               REMAX = ONE / CABS1( VL( II, KI ) )
               CALL ZDSCAL( N, REMAX, VL( 1, KI ), 1 )
            END IF
*
*           Set back the original diagonal elements of T.
*
            DO 120 K = KI + 1, N
               T( K, K ) = WORK( K+N )
  120       CONTINUE
*
            IS = IS + 1
  130    CONTINUE
      END IF
*
      RETURN
*
*     End of ZTREVC
*
      END
      SUBROUTINE ZTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA, B, LDB,
     $                   INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, TRANS, UPLO
      INTEGER            INFO, LDA, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  ZTRTRS solves a triangular system of the form
*
*     A * X = B,  A**T * X = B,  or  A**H * X = B,
*
*  where A is a triangular matrix of order N, and B is an N-by-NRHS
*  matrix.  A check is made to verify that A is nonsingular.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  A is upper triangular;
*          = 'L':  A is lower triangular.
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations:
*          = 'N':  A * X = B     (No transpose)
*          = 'T':  A**T * X = B  (Transpose)
*          = 'C':  A**H * X = B  (Conjugate transpose)
*
*  DIAG    (input) CHARACTER*1
*          = 'N':  A is non-unit triangular;
*          = 'U':  A is unit triangular.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The triangular matrix A.  If UPLO = 'U', the leading N-by-N
*          upper triangular part of the array A contains the upper
*          triangular matrix, and the strictly lower triangular part of
*          A is not referenced.  If UPLO = 'L', the leading N-by-N lower
*          triangular part of the array A contains the lower triangular
*          matrix, and the strictly upper triangular part of A is not
*          referenced.  If DIAG = 'U', the diagonal elements of A are
*          also not referenced and are assumed to be 1.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit, if INFO = 0, the solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          > 0: if INFO = i, the i-th diagonal element of A is zero,
*               indicating that the matrix is singular and the solutions
*               X have not been computed.
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOUNIT
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZTRSM
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      NOUNIT = LSAME( DIAG, 'N' )
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( TRANS, 'N' ) .AND. .NOT.
     $         LSAME( TRANS, 'T' ) .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOUNIT .AND. .NOT.LSAME( DIAG, 'U' ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZTRTRS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Check for singularity.
*
      IF( NOUNIT ) THEN
         DO 10 INFO = 1, N
            IF( A( INFO, INFO ).EQ.ZERO )
     $         RETURN
   10    CONTINUE
      END IF
      INFO = 0
*
*     Solve A * x = b,  A**T * x = b,  or  A**H * x = b.
*
      CALL ZTRSM( 'Left', UPLO, TRANS, DIAG, N, NRHS, ONE, A, LDA, B,
     $            LDB )
*
      RETURN
*
*     End of ZTRTRS
*
      END
      SUBROUTINE ZUNG2L( M, N, K, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNG2L generates an m by n complex matrix Q with orthonormal columns,
*  which is defined as the last n columns of a product of k elementary
*  reflectors of order m
*
*        Q  =  H(k) . . . H(2) H(1)
*
*  as returned by ZGEQLF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the (n-k+i)-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by ZGEQLF in the last k columns of its array
*          argument A.
*          On exit, the m-by-n matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGEQLF.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, II, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARF, ZSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNG2L', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 )
     $   RETURN
*
*     Initialise columns 1:n-k to columns of the unit matrix
*
      DO 20 J = 1, N - K
         DO 10 L = 1, M
            A( L, J ) = ZERO
   10    CONTINUE
         A( M-N+J, J ) = ONE
   20 CONTINUE
*
      DO 40 I = 1, K
         II = N - K + I
*
*        Apply H(i) to A(1:m-k+i,1:n-k+i) from the left
*
         A( M-N+II, II ) = ONE
         CALL ZLARF( 'Left', M-N+II, II-1, A( 1, II ), 1, TAU( I ), A,
     $               LDA, WORK )
         CALL ZSCAL( M-N+II-1, -TAU( I ), A( 1, II ), 1 )
         A( M-N+II, II ) = ONE - TAU( I )
*
*        Set A(m-k+i+1:m,n-k+i) to zero
*
         DO 30 L = M - N + II + 1, M
            A( L, II ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
*
*     End of ZUNG2L
*
      END
      SUBROUTINE ZUNG2R( M, N, K, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNG2R generates an m by n complex matrix Q with orthonormal columns,
*  which is defined as the first n columns of a product of k elementary
*  reflectors of order m
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by ZGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by ZGEQRF in the first k columns of its array
*          argument A.
*          On exit, the m by n matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGEQRF.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARF, ZSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNG2R', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 )
     $   RETURN
*
*     Initialise columns k+1:n to columns of the unit matrix
*
      DO 20 J = K + 1, N
         DO 10 L = 1, M
            A( L, J ) = ZERO
   10    CONTINUE
         A( J, J ) = ONE
   20 CONTINUE
*
      DO 40 I = K, 1, -1
*
*        Apply H(i) to A(i:m,i:n) from the left
*
         IF( I.LT.N ) THEN
            A( I, I ) = ONE
            CALL ZLARF( 'Left', M-I+1, N-I, A( I, I ), 1, TAU( I ),
     $                  A( I, I+1 ), LDA, WORK )
         END IF
         IF( I.LT.M )
     $      CALL ZSCAL( M-I, -TAU( I ), A( I+1, I ), 1 )
         A( I, I ) = ONE - TAU( I )
*
*        Set A(1:i-1,i) to zero
*
         DO 30 L = 1, I - 1
            A( L, I ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
*
*     End of ZUNG2R
*
      END
      SUBROUTINE ZUNGBR( VECT, M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          VECT
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGBR generates one of the complex unitary matrices Q or P**H
*  determined by ZGEBRD when reducing a complex matrix A to bidiagonal
*  form: A = Q * B * P**H.  Q and P**H are defined as products of
*  elementary reflectors H(i) or G(i) respectively.
*
*  If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
*  is of order M:
*  if m >= k, Q = H(1) H(2) . . . H(k) and ZUNGBR returns the first n
*  columns of Q, where m >= n >= k;
*  if m < k, Q = H(1) H(2) . . . H(m-1) and ZUNGBR returns Q as an
*  M-by-M matrix.
*
*  If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**H
*  is of order N:
*  if k < n, P**H = G(k) . . . G(2) G(1) and ZUNGBR returns the first m
*  rows of P**H, where n >= m >= k;
*  if k >= n, P**H = G(n-1) . . . G(2) G(1) and ZUNGBR returns P**H as
*  an N-by-N matrix.
*
*  Arguments
*  =========
*
*  VECT    (input) CHARACTER*1
*          Specifies whether the matrix Q or the matrix P**H is
*          required, as defined in the transformation applied by ZGEBRD:
*          = 'Q':  generate Q;
*          = 'P':  generate P**H.
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q or P**H to be returned.
*          M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q or P**H to be returned.
*          N >= 0.
*          If VECT = 'Q', M >= N >= min(M,K);
*          if VECT = 'P', N >= M >= min(N,K).
*
*  K       (input) INTEGER
*          If VECT = 'Q', the number of columns in the original M-by-K
*          matrix reduced by ZGEBRD.
*          If VECT = 'P', the number of rows in the original K-by-N
*          matrix reduced by ZGEBRD.
*          K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the vectors which define the elementary reflectors,
*          as returned by ZGEBRD.
*          On exit, the M-by-N matrix Q or P**H.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= M.
*
*  TAU     (input) COMPLEX*16 array, dimension
*                                (min(M,K)) if VECT = 'Q'
*                                (min(N,K)) if VECT = 'P'
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i) or G(i), which determines Q or P**H, as
*          returned by ZGEBRD in its array argument TAUQ or TAUP.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,min(M,N)).
*          For optimum performance LWORK >= min(M,N)*NB, where NB
*          is the optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, WANTQ
      INTEGER            I, IINFO, J, LWKOPT, MN, NB
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZUNGLQ, ZUNGQR
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      WANTQ = LSAME( VECT, 'Q' )
      MN = MIN( M, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.WANTQ .AND. .NOT.LSAME( VECT, 'P' ) ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 .OR. ( WANTQ .AND. ( N.GT.M .OR. N.LT.MIN( M,
     $         K ) ) ) .OR. ( .NOT.WANTQ .AND. ( M.GT.N .OR. M.LT.
     $         MIN( N, K ) ) ) ) THEN
         INFO = -3
      ELSE IF( K.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LWORK.LT.MAX( 1, MN ) .AND. .NOT.LQUERY ) THEN
         INFO = -9
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( WANTQ ) THEN
            NB = ILAENV( 1, 'ZUNGQR', ' ', M, N, K, -1 )
         ELSE
            NB = ILAENV( 1, 'ZUNGLQ', ' ', M, N, K, -1 )
         END IF
         LWKOPT = MAX( 1, MN )*NB
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGBR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      IF( WANTQ ) THEN
*
*        Form Q, determined by a call to ZGEBRD to reduce an m-by-k
*        matrix
*
         IF( M.GE.K ) THEN
*
*           If m >= k, assume m >= n >= k
*
            CALL ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, IINFO )
*
         ELSE
*
*           If m < k, assume m = n
*
*           Shift the vectors which define the elementary reflectors one
*           column to the right, and set the first row and column of Q
*           to those of the unit matrix
*
            DO 20 J = M, 2, -1
               A( 1, J ) = ZERO
               DO 10 I = J + 1, M
                  A( I, J ) = A( I, J-1 )
   10          CONTINUE
   20       CONTINUE
            A( 1, 1 ) = ONE
            DO 30 I = 2, M
               A( I, 1 ) = ZERO
   30       CONTINUE
            IF( M.GT.1 ) THEN
*
*              Form Q(2:m,2:m)
*
               CALL ZUNGQR( M-1, M-1, M-1, A( 2, 2 ), LDA, TAU, WORK,
     $                      LWORK, IINFO )
            END IF
         END IF
      ELSE
*
*        Form P', determined by a call to ZGEBRD to reduce a k-by-n
*        matrix
*
         IF( K.LT.N ) THEN
*
*           If k < n, assume k <= m <= n
*
            CALL ZUNGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, IINFO )
*
         ELSE
*
*           If k >= n, assume m = n
*
*           Shift the vectors which define the elementary reflectors one
*           row downward, and set the first row and column of P' to
*           those of the unit matrix
*
            A( 1, 1 ) = ONE
            DO 40 I = 2, N
               A( I, 1 ) = ZERO
   40       CONTINUE
            DO 60 J = 2, N
               DO 50 I = J - 1, 2, -1
                  A( I, J ) = A( I-1, J )
   50          CONTINUE
               A( 1, J ) = ZERO
   60       CONTINUE
            IF( N.GT.1 ) THEN
*
*              Form P'(2:n,2:n)
*
               CALL ZUNGLQ( N-1, N-1, N-1, A( 2, 2 ), LDA, TAU, WORK,
     $                      LWORK, IINFO )
            END IF
         END IF
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of ZUNGBR
*
      END
      SUBROUTINE ZUNGHR( N, ILO, IHI, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            IHI, ILO, INFO, LDA, LWORK, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGHR generates a complex unitary matrix Q which is defined as the
*  product of IHI-ILO elementary reflectors of order N, as returned by
*  ZGEHRD:
*
*  Q = H(ilo) H(ilo+1) . . . H(ihi-1).
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix Q. N >= 0.
*
*  ILO     (input) INTEGER
*  IHI     (input) INTEGER
*          ILO and IHI must have the same values as in the previous call
*          of ZGEHRD. Q is equal to the unit matrix except in the
*          submatrix Q(ilo+1:ihi,ilo+1:ihi).
*          1 <= ILO <= IHI <= N, if N > 0; ILO=1 and IHI=0, if N=0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the vectors which define the elementary reflectors,
*          as returned by ZGEHRD.
*          On exit, the N-by-N unitary matrix Q.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,N).
*
*  TAU     (input) COMPLEX*16 array, dimension (N-1)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGEHRD.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= IHI-ILO.
*          For optimum performance LWORK >= (IHI-ILO)*NB, where NB is
*          the optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IINFO, J, LWKOPT, NB, NH
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZUNGQR
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      NH = IHI - ILO
      LQUERY = ( LWORK.EQ.-1 )
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( ILO.LT.1 .OR. ILO.GT.MAX( 1, N ) ) THEN
         INFO = -2
      ELSE IF( IHI.LT.MIN( ILO, N ) .OR. IHI.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, NH ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
*
      IF( INFO.EQ.0 ) THEN
         NB = ILAENV( 1, 'ZUNGQR', ' ', NH, NH, NH, -1 )
         LWKOPT = MAX( 1, NH )*NB
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGHR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
*     Shift the vectors which define the elementary reflectors one
*     column to the right, and set the first ilo and the last n-ihi
*     rows and columns to those of the unit matrix
*
      DO 40 J = IHI, ILO + 1, -1
         DO 10 I = 1, J - 1
            A( I, J ) = ZERO
   10    CONTINUE
         DO 20 I = J + 1, IHI
            A( I, J ) = A( I, J-1 )
   20    CONTINUE
         DO 30 I = IHI + 1, N
            A( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
      DO 60 J = 1, ILO
         DO 50 I = 1, N
            A( I, J ) = ZERO
   50    CONTINUE
         A( J, J ) = ONE
   60 CONTINUE
      DO 80 J = IHI + 1, N
         DO 70 I = 1, N
            A( I, J ) = ZERO
   70    CONTINUE
         A( J, J ) = ONE
   80 CONTINUE
*
      IF( NH.GT.0 ) THEN
*
*        Generate Q(ilo+1:ihi,ilo+1:ihi)
*
         CALL ZUNGQR( NH, NH, NH, A( ILO+1, ILO+1 ), LDA, TAU( ILO ),
     $                WORK, LWORK, IINFO )
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of ZUNGHR
*
      END
      SUBROUTINE ZUNGL2( M, N, K, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGL2 generates an m-by-n complex matrix Q with orthonormal rows,
*  which is defined as the first m rows of a product of k elementary
*  reflectors of order n
*
*        Q  =  H(k)' . . . H(2)' H(1)'
*
*  as returned by ZGELQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. N >= M.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. M >= K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the i-th row must contain the vector which defines
*          the elementary reflector H(i), for i = 1,2,...,k, as returned
*          by ZGELQF in the first k rows of its array argument A.
*          On exit, the m by n matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGELQF.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (M)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLACGV, ZLARF, ZSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGL2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 )
     $   RETURN
*
      IF( K.LT.M ) THEN
*
*        Initialise rows k+1:m to rows of the unit matrix
*
         DO 20 J = 1, N
            DO 10 L = K + 1, M
               A( L, J ) = ZERO
   10       CONTINUE
            IF( J.GT.K .AND. J.LE.M )
     $         A( J, J ) = ONE
   20    CONTINUE
      END IF
*
      DO 40 I = K, 1, -1
*
*        Apply H(i)' to A(i:m,i:n) from the right
*
         IF( I.LT.N ) THEN
            CALL ZLACGV( N-I, A( I, I+1 ), LDA )
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
               CALL ZLARF( 'Right', M-I, N-I+1, A( I, I ), LDA,
     $                     DCONJG( TAU( I ) ), A( I+1, I ), LDA, WORK )
            END IF
            CALL ZSCAL( N-I, -TAU( I ), A( I, I+1 ), LDA )
            CALL ZLACGV( N-I, A( I, I+1 ), LDA )
         END IF
         A( I, I ) = ONE - DCONJG( TAU( I ) )
*
*        Set A(i,1:i-1) to zero
*
         DO 30 L = 1, I - 1
            A( I, L ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
*
*     End of ZUNGL2
*
      END
      SUBROUTINE ZUNGLQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGLQ generates an M-by-N complex matrix Q with orthonormal rows,
*  which is defined as the first M rows of a product of K elementary
*  reflectors of order N
*
*        Q  =  H(k)' . . . H(2)' H(1)'
*
*  as returned by ZGELQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. N >= M.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. M >= K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the i-th row must contain the vector which defines
*          the elementary reflector H(i), for i = 1,2,...,k, as returned
*          by ZGELQF in the first k rows of its array argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGELQF.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,M).
*          For optimum performance LWORK >= M*NB, where NB is
*          the optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit;
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK,
     $                   LWKOPT, NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNGL2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      NB = ILAENV( 1, 'ZUNGLQ', ' ', M, N, K, -1 )
      LWKOPT = MAX( 1, M )*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, M ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGLQ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'ZUNGLQ', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'ZUNGLQ', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code after the last block.
*        The first kk rows are handled by the block method.
*
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
*
*        Set A(kk+1:m,1:kk) to zero.
*
         DO 20 J = 1, KK
            DO 10 I = KK + 1, M
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
*
*     Use unblocked code for the last or only block.
*
      IF( KK.LT.M )
     $   CALL ZUNGL2( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
*
      IF( KK.GT.0 ) THEN
*
*        Use blocked code
*
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.M ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL ZLARFT( 'Forward', 'Rowwise', N-I+1, IB, A( I, I ),
     $                      LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H' to A(i+ib:m,i:n) from the right
*
               CALL ZLARFB( 'Right', 'Conjugate transpose', 'Forward',
     $                      'Rowwise', M-I-IB+1, N-I+1, IB, A( I, I ),
     $                      LDA, WORK, LDWORK, A( I+IB, I ), LDA,
     $                      WORK( IB+1 ), LDWORK )
            END IF
*
*           Apply H' to columns i:n of current block
*
            CALL ZUNGL2( IB, N-I+1, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
*
*           Set columns 1:i-1 of current block to zero
*
            DO 40 J = 1, I - 1
               DO 30 L = I, I + IB - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of ZUNGLQ
*
      END
      SUBROUTINE ZUNGQL( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGQL generates an M-by-N complex matrix Q with orthonormal columns,
*  which is defined as the last N columns of a product of K elementary
*  reflectors of order M
*
*        Q  =  H(k) . . . H(2) H(1)
*
*  as returned by ZGEQLF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the (n-k+i)-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by ZGEQLF in the last k columns of its array
*          argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGEQLF.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is the
*          optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, J, KK, L, LDWORK, LWKOPT,
     $                   NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNG2L
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      NB = ILAENV( 1, 'ZUNGQL', ' ', M, N, K, -1 )
      LWKOPT = MAX( 1, N )*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGQL', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'ZUNGQL', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'ZUNGQL', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code after the first block.
*        The last kk columns are handled by the block method.
*
         KK = MIN( K, ( ( K-NX+NB-1 ) / NB )*NB )
*
*        Set A(m-kk+1:m,1:n-kk) to zero.
*
         DO 20 J = 1, N - KK
            DO 10 I = M - KK + 1, M
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
*
*     Use unblocked code for the first or only block.
*
      CALL ZUNG2L( M-KK, N-KK, K-KK, A, LDA, TAU, WORK, IINFO )
*
      IF( KK.GT.0 ) THEN
*
*        Use blocked code
*
         DO 50 I = K - KK + 1, K, NB
            IB = MIN( NB, K-I+1 )
            IF( N-K+I.GT.1 ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i+ib-1) . . . H(i+1) H(i)
*
               CALL ZLARFT( 'Backward', 'Columnwise', M-K+I+IB-1, IB,
     $                      A( 1, N-K+I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(1:m-k+i+ib-1,1:n-k+i-1) from the left
*
               CALL ZLARFB( 'Left', 'No transpose', 'Backward',
     $                      'Columnwise', M-K+I+IB-1, N-K+I-1, IB,
     $                      A( 1, N-K+I ), LDA, WORK, LDWORK, A, LDA,
     $                      WORK( IB+1 ), LDWORK )
            END IF
*
*           Apply H to rows 1:m-k+i+ib-1 of current block
*
            CALL ZUNG2L( M-K+I+IB-1, IB, IB, A( 1, N-K+I ), LDA,
     $                   TAU( I ), WORK, IINFO )
*
*           Set rows m-k+i+ib:m of current block to zero
*
            DO 40 J = N - K + I, N - K + I + IB - 1
               DO 30 L = M - K + I + IB, M
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of ZUNGQL
*
      END
      SUBROUTINE ZUNGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
*  which is defined as the first N columns of a product of K elementary
*  reflectors of order M
*
*        Q  =  H(1) H(2) . . . H(k)
*
*  as returned by ZGEQRF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. M >= N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. N >= K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the i-th column must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by ZGEQRF in the first k columns of its array
*          argument A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGEQRF.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,N).
*          For optimum performance LWORK >= N*NB, where NB is the
*          optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, IINFO, IWS, J, KI, KK, L, LDWORK,
     $                   LWKOPT, NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNG2R
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      NB = ILAENV( 1, 'ZUNGQR', ' ', M, N, K, -1 )
      LWKOPT = MAX( 1, N )*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.N ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, N ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGQR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      NX = 0
      IWS = N
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'ZUNGQR', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'ZUNGQR', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code after the last block.
*        The first kk columns are handled by the block method.
*
         KI = ( ( K-NX-1 ) / NB )*NB
         KK = MIN( K, KI+NB )
*
*        Set A(1:kk,kk+1:n) to zero.
*
         DO 20 J = KK + 1, N
            DO 10 I = 1, KK
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
*
*     Use unblocked code for the last or only block.
*
      IF( KK.LT.N )
     $   CALL ZUNG2R( M-KK, N-KK, K-KK, A( KK+1, KK+1 ), LDA,
     $                TAU( KK+1 ), WORK, IINFO )
*
      IF( KK.GT.0 ) THEN
*
*        Use blocked code
*
         DO 50 I = KI + 1, 1, -NB
            IB = MIN( NB, K-I+1 )
            IF( I+IB.LE.N ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i) H(i+1) . . . H(i+ib-1)
*
               CALL ZLARFT( 'Forward', 'Columnwise', M-I+1, IB,
     $                      A( I, I ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H to A(i:m,i+ib:n) from the left
*
               CALL ZLARFB( 'Left', 'No transpose', 'Forward',
     $                      'Columnwise', M-I+1, N-I-IB+1, IB,
     $                      A( I, I ), LDA, WORK, LDWORK, A( I, I+IB ),
     $                      LDA, WORK( IB+1 ), LDWORK )
            END IF
*
*           Apply H to rows i:m of current block
*
            CALL ZUNG2R( M-I+1, IB, IB, A( I, I ), LDA, TAU( I ), WORK,
     $                   IINFO )
*
*           Set rows 1:i-1 of current block to zero
*
            DO 40 J = I, I + IB - 1
               DO 30 L = 1, I - 1
                  A( L, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of ZUNGQR
*
      END
      SUBROUTINE ZUNGR2( M, N, K, A, LDA, TAU, WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGR2 generates an m by n complex matrix Q with orthonormal rows,
*  which is defined as the last m rows of a product of k elementary
*  reflectors of order n
*
*        Q  =  H(1)' H(2)' . . . H(k)'
*
*  as returned by ZGERQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. N >= M.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. M >= K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the (m-k+i)-th row must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by ZGERQF in the last k rows of its array argument
*          A.
*          On exit, the m-by-n matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGERQF.
*
*  WORK    (workspace) COMPLEX*16 array, dimension (M)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, II, J, L
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLACGV, ZLARF, ZSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGR2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 )
     $   RETURN
*
      IF( K.LT.M ) THEN
*
*        Initialise rows 1:m-k to rows of the unit matrix
*
         DO 20 J = 1, N
            DO 10 L = 1, M - K
               A( L, J ) = ZERO
   10       CONTINUE
            IF( J.GT.N-M .AND. J.LE.N-K )
     $         A( M-N+J, J ) = ONE
   20    CONTINUE
      END IF
*
      DO 40 I = 1, K
         II = M - K + I
*
*        Apply H(i)' to A(1:m-k+i,1:n-k+i) from the right
*
         CALL ZLACGV( N-M+II-1, A( II, 1 ), LDA )
         A( II, N-M+II ) = ONE
         CALL ZLARF( 'Right', II-1, N-M+II, A( II, 1 ), LDA,
     $               DCONJG( TAU( I ) ), A, LDA, WORK )
         CALL ZSCAL( N-M+II-1, -TAU( I ), A( II, 1 ), LDA )
         CALL ZLACGV( N-M+II-1, A( II, 1 ), LDA )
         A( II, N-M+II ) = ONE - DCONJG( TAU( I ) )
*
*        Set A(m-k+i,n-k+i+1:n) to zero
*
         DO 30 L = N - M + II + 1, N
            A( II, L ) = ZERO
   30    CONTINUE
   40 CONTINUE
      RETURN
*
*     End of ZUNGR2
*
      END
      SUBROUTINE ZUNGRQ( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGRQ generates an M-by-N complex matrix Q with orthonormal rows,
*  which is defined as the last M rows of a product of K elementary
*  reflectors of order N
*
*        Q  =  H(1)' H(2)' . . . H(k)'
*
*  as returned by ZGERQF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix Q. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix Q. N >= M.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines the
*          matrix Q. M >= K >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the (m-k+i)-th row must contain the vector which
*          defines the elementary reflector H(i), for i = 1,2,...,k, as
*          returned by ZGERQF in the last k rows of its array argument
*          A.
*          On exit, the M-by-N matrix Q.
*
*  LDA     (input) INTEGER
*          The first dimension of the array A. LDA >= max(1,M).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGERQF.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,M).
*          For optimum performance LWORK >= M*NB, where NB is the
*          optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument has an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IB, II, IINFO, IWS, J, KK, L, LDWORK,
     $                   LWKOPT, NB, NBMIN, NX
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNGR2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      NB = ILAENV( 1, 'ZUNGRQ', ' ', M, N, K, -1 )
      LWKOPT = MAX( 1, M )*NB
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.M ) THEN
         INFO = -2
      ELSE IF( K.LT.0 .OR. K.GT.M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LWORK.LT.MAX( 1, M ) .AND. .NOT.LQUERY ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGRQ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.LE.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      NX = 0
      IWS = M
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
*
*        Determine when to cross over from blocked to unblocked code.
*
         NX = MAX( 0, ILAENV( 3, 'ZUNGRQ', ' ', M, N, K, -1 ) )
         IF( NX.LT.K ) THEN
*
*           Determine if workspace is large enough for blocked code.
*
            LDWORK = M
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
*
*              Not enough workspace to use optimal NB:  reduce NB and
*              determine the minimum value of NB.
*
               NB = LWORK / LDWORK
               NBMIN = MAX( 2, ILAENV( 2, 'ZUNGRQ', ' ', M, N, K, -1 ) )
            END IF
         END IF
      END IF
*
      IF( NB.GE.NBMIN .AND. NB.LT.K .AND. NX.LT.K ) THEN
*
*        Use blocked code after the first block.
*        The last kk rows are handled by the block method.
*
         KK = MIN( K, ( ( K-NX+NB-1 ) / NB )*NB )
*
*        Set A(1:m-kk,n-kk+1:n) to zero.
*
         DO 20 J = N - KK + 1, N
            DO 10 I = 1, M - KK
               A( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE
         KK = 0
      END IF
*
*     Use unblocked code for the first or only block.
*
      CALL ZUNGR2( M-KK, N-KK, K-KK, A, LDA, TAU, WORK, IINFO )
*
      IF( KK.GT.0 ) THEN
*
*        Use blocked code
*
         DO 50 I = K - KK + 1, K, NB
            IB = MIN( NB, K-I+1 )
            II = M - K + I
            IF( II.GT.1 ) THEN
*
*              Form the triangular factor of the block reflector
*              H = H(i+ib-1) . . . H(i+1) H(i)
*
               CALL ZLARFT( 'Backward', 'Rowwise', N-K+I+IB-1, IB,
     $                      A( II, 1 ), LDA, TAU( I ), WORK, LDWORK )
*
*              Apply H' to A(1:m-k+i-1,1:n-k+i+ib-1) from the right
*
               CALL ZLARFB( 'Right', 'Conjugate transpose', 'Backward',
     $                      'Rowwise', II-1, N-K+I+IB-1, IB, A( II, 1 ),
     $                      LDA, WORK, LDWORK, A, LDA, WORK( IB+1 ),
     $                      LDWORK )
            END IF
*
*           Apply H' to columns 1:n-k+i+ib-1 of current block
*
            CALL ZUNGR2( IB, N-K+I+IB-1, IB, A( II, 1 ), LDA, TAU( I ),
     $                   WORK, IINFO )
*
*           Set columns n-k+i+ib:n of current block to zero
*
            DO 40 L = N - K + I + IB, N
               DO 30 J = II, II + IB - 1
                  A( J, L ) = ZERO
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
      END IF
*
      WORK( 1 ) = IWS
      RETURN
*
*     End of ZUNGRQ
*
      END
      SUBROUTINE ZUNGTR( UPLO, N, A, LDA, TAU, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LWORK, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNGTR generates a complex unitary matrix Q which is defined as the
*  product of n-1 elementary reflectors of order N, as returned by
*  ZHETRD:
*
*  if UPLO = 'U', Q = H(n-1) . . . H(2) H(1),
*
*  if UPLO = 'L', Q = H(1) H(2) . . . H(n-1).
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U': Upper triangle of A contains elementary reflectors
*                 from ZHETRD;
*          = 'L': Lower triangle of A contains elementary reflectors
*                 from ZHETRD.
*
*  N       (input) INTEGER
*          The order of the matrix Q. N >= 0.
*
*  A       (input/output) COMPLEX*16 array, dimension (LDA,N)
*          On entry, the vectors which define the elementary reflectors,
*          as returned by ZHETRD.
*          On exit, the N-by-N unitary matrix Q.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= N.
*
*  TAU     (input) COMPLEX*16 array, dimension (N-1)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZHETRD.
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= N-1.
*          For optimum performance LWORK >= (N-1)*NB, where NB is
*          the optimal blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ZERO, ONE
      PARAMETER          ( ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER
      INTEGER            I, IINFO, J, LWKOPT, NB
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZUNGQL, ZUNGQR
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.MAX( 1, N-1 ) .AND. .NOT.LQUERY ) THEN
         INFO = -7
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( UPPER ) THEN
            NB = ILAENV( 1, 'ZUNGQL', ' ', N-1, N-1, N-1, -1 )
         ELSE
            NB = ILAENV( 1, 'ZUNGQR', ' ', N-1, N-1, N-1, -1 )
         END IF
         LWKOPT = MAX( 1, N-1 )*NB
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNGTR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      IF( UPPER ) THEN
*
*        Q was determined by a call to ZHETRD with UPLO = 'U'
*
*        Shift the vectors which define the elementary reflectors one
*        column to the left, and set the last row and column of Q to
*        those of the unit matrix
*
         DO 20 J = 1, N - 1
            DO 10 I = 1, J - 1
               A( I, J ) = A( I, J+1 )
   10       CONTINUE
            A( N, J ) = ZERO
   20    CONTINUE
         DO 30 I = 1, N - 1
            A( I, N ) = ZERO
   30    CONTINUE
         A( N, N ) = ONE
*
*        Generate Q(1:n-1,1:n-1)
*
         CALL ZUNGQL( N-1, N-1, N-1, A, LDA, TAU, WORK, LWORK, IINFO )
*
      ELSE
*
*        Q was determined by a call to ZHETRD with UPLO = 'L'.
*
*        Shift the vectors which define the elementary reflectors one
*        column to the right, and set the first row and column of Q to
*        those of the unit matrix
*
         DO 50 J = N, 2, -1
            A( 1, J ) = ZERO
            DO 40 I = J + 1, N
               A( I, J ) = A( I, J-1 )
   40       CONTINUE
   50    CONTINUE
         A( 1, 1 ) = ONE
         DO 60 I = 2, N
            A( I, 1 ) = ZERO
   60    CONTINUE
         IF( N.GT.1 ) THEN
*
*           Generate Q(2:n,2:n)
*
            CALL ZUNGQR( N-1, N-1, N-1, A( 2, 2 ), LDA, TAU, WORK,
     $                   LWORK, IINFO )
         END IF
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of ZUNGTR
*
      END
      SUBROUTINE ZUNM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNM2R overwrites the general complex m-by-n matrix C with
*
*        Q * C  if SIDE = 'L' and TRANS = 'N', or
*
*        Q'* C  if SIDE = 'L' and TRANS = 'C', or
*
*        C * Q  if SIDE = 'R' and TRANS = 'N', or
*
*        C * Q' if SIDE = 'R' and TRANS = 'C',
*
*  where Q is a complex unitary matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(1) H(2) . . . H(k)
*
*  as returned by ZGEQRF. Q is of order m if SIDE = 'L' and of order n
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q' from the Left
*          = 'R': apply Q or Q' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply Q  (No transpose)
*          = 'C': apply Q' (Conjugate transpose)
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,K)
*          The i-th column must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          ZGEQRF in the first k columns of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If SIDE = 'L', LDA >= max(1,M);
*          if SIDE = 'R', LDA >= max(1,N).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGEQRF.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
*          On entry, the m-by-n matrix C.
*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) COMPLEX*16 array, dimension
*                                   (N) if SIDE = 'L',
*                                   (M) if SIDE = 'R'
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      COMPLEX*16         AII, TAUI
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARF
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q
*
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNM2R', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
*
      IF( ( LEFT .AND. .NOT.NOTRAN .OR. .NOT.LEFT .AND. NOTRAN ) ) THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
*
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
*
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
*
*           H(i) or H(i)' is applied to C(i:m,1:n)
*
            MI = M - I + 1
            IC = I
         ELSE
*
*           H(i) or H(i)' is applied to C(1:m,i:n)
*
            NI = N - I + 1
            JC = I
         END IF
*
*        Apply H(i) or H(i)'
*
         IF( NOTRAN ) THEN
            TAUI = TAU( I )
         ELSE
            TAUI = DCONJG( TAU( I ) )
         END IF
         AII = A( I, I )
         A( I, I ) = ONE
         CALL ZLARF( SIDE, MI, NI, A( I, I ), 1, TAUI, C( IC, JC ), LDC,
     $               WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
*
*     End of ZUNM2R
*
      END
      SUBROUTINE ZUNMBR( VECT, SIDE, TRANS, M, N, K, A, LDA, TAU, C,
     $                   LDC, WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS, VECT
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  If VECT = 'Q', ZUNMBR overwrites the general complex M-by-N matrix C
*  with
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'C':      Q**H * C       C * Q**H
*
*  If VECT = 'P', ZUNMBR overwrites the general complex M-by-N matrix C
*  with
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      P * C          C * P
*  TRANS = 'C':      P**H * C       C * P**H
*
*  Here Q and P**H are the unitary matrices determined by ZGEBRD when
*  reducing a complex matrix A to bidiagonal form: A = Q * B * P**H. Q
*  and P**H are defined as products of elementary reflectors H(i) and
*  G(i) respectively.
*
*  Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
*  order of the unitary matrix Q or P**H that is applied.
*
*  If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
*  if nq >= k, Q = H(1) H(2) . . . H(k);
*  if nq < k, Q = H(1) H(2) . . . H(nq-1).
*
*  If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
*  if k < nq, P = G(1) G(2) . . . G(k);
*  if k >= nq, P = G(1) G(2) . . . G(nq-1).
*
*  Arguments
*  =========
*
*  VECT    (input) CHARACTER*1
*          = 'Q': apply Q or Q**H;
*          = 'P': apply P or P**H.
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q, Q**H, P or P**H from the Left;
*          = 'R': apply Q, Q**H, P or P**H from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q or P;
*          = 'C':  Conjugate transpose, apply Q**H or P**H.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          If VECT = 'Q', the number of columns in the original
*          matrix reduced by ZGEBRD.
*          If VECT = 'P', the number of rows in the original
*          matrix reduced by ZGEBRD.
*          K >= 0.
*
*  A       (input) COMPLEX*16 array, dimension
*                                (LDA,min(nq,K)) if VECT = 'Q'
*                                (LDA,nq)        if VECT = 'P'
*          The vectors which define the elementary reflectors H(i) and
*          G(i), whose products determine the matrices Q and P, as
*          returned by ZGEBRD.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If VECT = 'Q', LDA >= max(1,nq);
*          if VECT = 'P', LDA >= max(1,min(nq,K)).
*
*  TAU     (input) COMPLEX*16 array, dimension (min(nq,K))
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i) or G(i) which determines Q or P, as returned
*          by ZGEBRD in the array argument TAUQ or TAUP.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q
*          or P*C or P**H*C or C*P or C*P**H.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            APPLYQ, LEFT, LQUERY, NOTRAN
      CHARACTER          TRANST
      INTEGER            I1, I2, IINFO, LWKOPT, MI, NB, NI, NQ, NW
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZUNMLQ, ZUNMQR
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      APPLYQ = LSAME( VECT, 'Q' )
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
*
*     NQ is the order of Q or P and NW is the minimum dimension of WORK
*
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.APPLYQ .AND. .NOT.LSAME( VECT, 'P' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( K.LT.0 ) THEN
         INFO = -6
      ELSE IF( ( APPLYQ .AND. LDA.LT.MAX( 1, NQ ) ) .OR.
     $         ( .NOT.APPLYQ .AND. LDA.LT.MAX( 1, MIN( NQ, K ) ) ) )
     $          THEN
         INFO = -8
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -11
      ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
         INFO = -13
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( APPLYQ ) THEN
            IF( LEFT ) THEN
               NB = ILAENV( 1, 'ZUNMQR', SIDE // TRANS, M-1, N, M-1,
     $              -1 )
            ELSE
               NB = ILAENV( 1, 'ZUNMQR', SIDE // TRANS, M, N-1, N-1,
     $              -1 )
            END IF
         ELSE
            IF( LEFT ) THEN
               NB = ILAENV( 1, 'ZUNMLQ', SIDE // TRANS, M-1, N, M-1,
     $              -1 )
            ELSE
               NB = ILAENV( 1, 'ZUNMLQ', SIDE // TRANS, M, N-1, N-1,
     $              -1 )
            END IF
         END IF
         LWKOPT = MAX( 1, NW )*NB
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNMBR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
      END IF
*
*     Quick return if possible
*
      WORK( 1 ) = 1
      IF( M.EQ.0 .OR. N.EQ.0 )
     $   RETURN
*
      IF( APPLYQ ) THEN
*
*        Apply Q
*
         IF( NQ.GE.K ) THEN
*
*           Q was determined by a call to ZGEBRD with nq >= k
*
            CALL ZUNMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, IINFO )
         ELSE IF( NQ.GT.1 ) THEN
*
*           Q was determined by a call to ZGEBRD with nq < k
*
            IF( LEFT ) THEN
               MI = M - 1
               NI = N
               I1 = 2
               I2 = 1
            ELSE
               MI = M
               NI = N - 1
               I1 = 1
               I2 = 2
            END IF
            CALL ZUNMQR( SIDE, TRANS, MI, NI, NQ-1, A( 2, 1 ), LDA, TAU,
     $                   C( I1, I2 ), LDC, WORK, LWORK, IINFO )
         END IF
      ELSE
*
*        Apply P
*
         IF( NOTRAN ) THEN
            TRANST = 'C'
         ELSE
            TRANST = 'N'
         END IF
         IF( NQ.GT.K ) THEN
*
*           P was determined by a call to ZGEBRD with nq > k
*
            CALL ZUNMLQ( SIDE, TRANST, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, IINFO )
         ELSE IF( NQ.GT.1 ) THEN
*
*           P was determined by a call to ZGEBRD with nq <= k
*
            IF( LEFT ) THEN
               MI = M - 1
               NI = N
               I1 = 2
               I2 = 1
            ELSE
               MI = M
               NI = N - 1
               I1 = 1
               I2 = 2
            END IF
            CALL ZUNMLQ( SIDE, TRANST, MI, NI, NQ-1, A( 1, 2 ), LDA,
     $                   TAU, C( I1, I2 ), LDC, WORK, LWORK, IINFO )
         END IF
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of ZUNMBR
*
      END
      SUBROUTINE ZUNML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNML2 overwrites the general complex m-by-n matrix C with
*
*        Q * C  if SIDE = 'L' and TRANS = 'N', or
*
*        Q'* C  if SIDE = 'L' and TRANS = 'C', or
*
*        C * Q  if SIDE = 'R' and TRANS = 'N', or
*
*        C * Q' if SIDE = 'R' and TRANS = 'C',
*
*  where Q is a complex unitary matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(k)' . . . H(2)' H(1)'
*
*  as returned by ZGELQF. Q is of order m if SIDE = 'L' and of order n
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q' from the Left
*          = 'R': apply Q or Q' from the Right
*
*  TRANS   (input) CHARACTER*1
*          = 'N': apply Q  (No transpose)
*          = 'C': apply Q' (Conjugate transpose)
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) COMPLEX*16 array, dimension
*                               (LDA,M) if SIDE = 'L',
*                               (LDA,N) if SIDE = 'R'
*          The i-th row must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          ZGELQF in the first k rows of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,K).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGELQF.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
*          On entry, the m-by-n matrix C.
*          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) COMPLEX*16 array, dimension
*                                   (N) if SIDE = 'L',
*                                   (M) if SIDE = 'R'
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      COMPLEX*16         AII, TAUI
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLACGV, ZLARF
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DCONJG, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
*
*     NQ is the order of Q
*
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNML2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 )
     $   RETURN
*
      IF( ( LEFT .AND. NOTRAN .OR. .NOT.LEFT .AND. .NOT.NOTRAN ) ) THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      END IF
*
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      END IF
*
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
*
*           H(i) or H(i)' is applied to C(i:m,1:n)
*
            MI = M - I + 1
            IC = I
         ELSE
*
*           H(i) or H(i)' is applied to C(1:m,i:n)
*
            NI = N - I + 1
            JC = I
         END IF
*
*        Apply H(i) or H(i)'
*
         IF( NOTRAN ) THEN
            TAUI = DCONJG( TAU( I ) )
         ELSE
            TAUI = TAU( I )
         END IF
         IF( I.LT.NQ )
     $      CALL ZLACGV( NQ-I, A( I, I+1 ), LDA )
         AII = A( I, I )
         A( I, I ) = ONE
         CALL ZLARF( SIDE, MI, NI, A( I, I ), LDA, TAUI, C( IC, JC ),
     $               LDC, WORK )
         A( I, I ) = AII
         IF( I.LT.NQ )
     $      CALL ZLACGV( NQ-I, A( I, I+1 ), LDA )
   10 CONTINUE
      RETURN
*
*     End of ZUNML2
*
      END
      SUBROUTINE ZUNMLQ( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNMLQ overwrites the general complex M-by-N matrix C with
*
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'C':      Q**H * C       C * Q**H
*
*  where Q is a complex unitary matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(k)' . . . H(2)' H(1)'
*
*  as returned by ZGELQF. Q is of order M if SIDE = 'L' and of order N
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q**H from the Left;
*          = 'R': apply Q or Q**H from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q;
*          = 'C':  Conjugate transpose, apply Q**H.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) COMPLEX*16 array, dimension
*                               (LDA,M) if SIDE = 'L',
*                               (LDA,N) if SIDE = 'R'
*          The i-th row must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          ZGELQF in the first k rows of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,K).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGELQF.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      CHARACTER          TRANST
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   LWKOPT, MI, NB, NBMIN, NI, NQ, NW
*     ..
*     .. Local Arrays ..
      COMPLEX*16         T( LDT, NBMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNML2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
*
*     NQ is the order of Q and NW is the minimum dimension of WORK
*
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, K ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
*
      IF( INFO.EQ.0 ) THEN
*
*        Determine the block size.  NB may be at most NBMAX, where NBMAX
*        is used to define the local array T.
*
         NB = MIN( NBMAX, ILAENV( 1, 'ZUNMLQ', SIDE // TRANS, M, N, K,
     $        -1 ) )
         LWKOPT = MAX( 1, NW )*NB
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNMLQ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'ZUNMLQ', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      ELSE
         IWS = NW
      END IF
*
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
*
*        Use unblocked code
*
         CALL ZUNML2( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
*
*        Use blocked code
*
         IF( ( LEFT .AND. NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. .NOT.NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
*
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
*
         IF( NOTRAN ) THEN
            TRANST = 'C'
         ELSE
            TRANST = 'N'
         END IF
*
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
*
*           Form the triangular factor of the block reflector
*           H = H(i) H(i+1) . . . H(i+ib-1)
*
            CALL ZLARFT( 'Forward', 'Rowwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
*
*              H or H' is applied to C(i:m,1:n)
*
               MI = M - I + 1
               IC = I
            ELSE
*
*              H or H' is applied to C(1:m,i:n)
*
               NI = N - I + 1
               JC = I
            END IF
*
*           Apply H or H'
*
            CALL ZLARFB( SIDE, TRANST, 'Forward', 'Rowwise', MI, NI, IB,
     $                   A( I, I ), LDA, T, LDT, C( IC, JC ), LDC, WORK,
     $                   LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of ZUNMLQ
*
      END
      SUBROUTINE ZUNMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZUNMQR overwrites the general complex M-by-N matrix C with
*
*                  SIDE = 'L'     SIDE = 'R'
*  TRANS = 'N':      Q * C          C * Q
*  TRANS = 'C':      Q**H * C       C * Q**H
*
*  where Q is a complex unitary matrix defined as the product of k
*  elementary reflectors
*
*        Q = H(1) H(2) . . . H(k)
*
*  as returned by ZGEQRF. Q is of order M if SIDE = 'L' and of order N
*  if SIDE = 'R'.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          = 'L': apply Q or Q**H from the Left;
*          = 'R': apply Q or Q**H from the Right.
*
*  TRANS   (input) CHARACTER*1
*          = 'N':  No transpose, apply Q;
*          = 'C':  Conjugate transpose, apply Q**H.
*
*  M       (input) INTEGER
*          The number of rows of the matrix C. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix C. N >= 0.
*
*  K       (input) INTEGER
*          The number of elementary reflectors whose product defines
*          the matrix Q.
*          If SIDE = 'L', M >= K >= 0;
*          if SIDE = 'R', N >= K >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,K)
*          The i-th column must contain the vector which defines the
*          elementary reflector H(i), for i = 1,2,...,k, as returned by
*          ZGEQRF in the first k columns of its array argument A.
*          A is modified by the routine but restored on exit.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.
*          If SIDE = 'L', LDA >= max(1,M);
*          if SIDE = 'R', LDA >= max(1,N).
*
*  TAU     (input) COMPLEX*16 array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i), as returned by ZGEQRF.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC,N)
*          On entry, the M-by-N matrix C.
*          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If SIDE = 'L', LWORK >= max(1,N);
*          if SIDE = 'R', LWORK >= max(1,M).
*          For optimum performance LWORK >= N*NB if SIDE = 'L', and
*          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
*          blocksize.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK,
     $                   LWKOPT, MI, NB, NBMIN, NI, NQ, NW
*     ..
*     .. Local Arrays ..
      COMPLEX*16         T( LDT, NBMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA, ZLARFB, ZLARFT, ZUNM2R
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
*
*     NQ is the order of Q and NW is the minimum dimension of WORK
*
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      END IF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
         INFO = -12
      END IF
*
      IF( INFO.EQ.0 ) THEN
*
*        Determine the block size.  NB may be at most NBMAX, where NBMAX
*        is used to define the local array T.
*
         NB = MIN( NBMAX, ILAENV( 1, 'ZUNMQR', SIDE // TRANS, M, N, K,
     $        -1 ) )
         LWKOPT = MAX( 1, NW )*NB
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZUNMQR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'ZUNMQR', SIDE // TRANS, M, N, K,
     $              -1 ) )
         END IF
      ELSE
         IWS = NW
      END IF
*
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
*
*        Use unblocked code
*
         CALL ZUNM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK,
     $                IINFO )
      ELSE
*
*        Use blocked code
*
         IF( ( LEFT .AND. .NOT.NOTRAN ) .OR.
     $       ( .NOT.LEFT .AND. NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         END IF
*
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         END IF
*
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
*
*           Form the triangular factor of the block reflector
*           H = H(i) H(i+1) . . . H(i+ib-1)
*
            CALL ZLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ),
     $                   LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
*
*              H or H' is applied to C(i:m,1:n)
*
               MI = M - I + 1
               IC = I
            ELSE
*
*              H or H' is applied to C(1:m,i:n)
*
               NI = N - I + 1
               JC = I
            END IF
*
*           Apply H or H'
*
            CALL ZLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NI,
     $                   IB, A( I, I ), LDA, T, LDT, C( IC, JC ), LDC,
     $                   WORK, LDWORK )
   10    CONTINUE
      END IF
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of ZUNMQR
*
      END
