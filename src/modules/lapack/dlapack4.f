      LOGICAL FUNCTION DISNAN(DIN)
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION DIN
*     ..
*
*  Purpose
*  =======
*
*  DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
*  otherwise.  To be replaced by the Fortran 2003 intrinsic in the
*  future.
*
*  Arguments
*  =========
*
*  DIN      (input) DOUBLE PRECISION
*          Input to test for NaN.
*
*  =====================================================================
*
*  .. External Functions ..
      LOGICAL DLAISNAN
      EXTERNAL DLAISNAN
*  ..
*  .. Executable Statements ..
      DISNAN = DLAISNAN(DIN,DIN)
      RETURN
      END
      SUBROUTINE DLACN2( N, V, X, ISGN, EST, KASE, ISAVE )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            KASE, N
      DOUBLE PRECISION   EST
*     ..
*     .. Array Arguments ..
      INTEGER            ISGN( * ), ISAVE( 3 )
      DOUBLE PRECISION   V( * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLACN2 estimates the 1-norm of a square, real matrix A.
*  Reverse communication is used for evaluating matrix-vector products.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         The order of the matrix.  N >= 1.
*
*  V      (workspace) DOUBLE PRECISION array, dimension (N)
*         On the final return, V = A*W,  where  EST = norm(V)/norm(W)
*         (W is not returned).
*
*  X      (input/output) DOUBLE PRECISION array, dimension (N)
*         On an intermediate return, X should be overwritten by
*               A * X,   if KASE=1,
*               A' * X,  if KASE=2,
*         and DLACN2 must be re-called with all the other parameters
*         unchanged.
*
*  ISGN   (workspace) INTEGER array, dimension (N)
*
*  EST    (input/output) DOUBLE PRECISION
*         On entry with KASE = 1 or 2 and ISAVE(1) = 3, EST should be
*         unchanged from the previous call to DLACN2.
*         On exit, EST is an estimate (a lower bound) for norm(A). 
*
*  KASE   (input/output) INTEGER
*         On the initial call to DLACN2, KASE should be 0.
*         On an intermediate return, KASE will be 1 or 2, indicating
*         whether X should be overwritten by A * X  or A' * X.
*         On the final return from DLACN2, KASE will again be 0.
*
*  ISAVE  (input/output) INTEGER array, dimension (3)
*         ISAVE is used to save variables between calls to DLACN2
*
*  Further Details
*  ======= =======
*
*  Contributed by Nick Higham, University of Manchester.
*  Originally named SONEST, dated March 16, 1988.
*
*  Reference: N.J. Higham, "FORTRAN codes for estimating the one-norm of
*  a real or complex matrix, with applications to condition estimation",
*  ACM Trans. Math. Soft., vol. 14, no. 4, pp. 381-396, December 1988.
*
*  This is a thread safe version of DLACON, which uses the array ISAVE
*  in place of a SAVE statement, as follows:
*
*     DLACON     DLACN2
*      JUMP     ISAVE(1)
*      J        ISAVE(2)
*      ITER     ISAVE(3)
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            ITMAX
      PARAMETER          ( ITMAX = 5 )
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, JLAST
      DOUBLE PRECISION   ALTSGN, ESTOLD, TEMP
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM
      EXTERNAL           IDAMAX, DASUM
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, NINT, SIGN
*     ..
*     .. Executable Statements ..
*
      IF( KASE.EQ.0 ) THEN
         DO 10 I = 1, N
            X( I ) = ONE / DBLE( N )
   10    CONTINUE
         KASE = 1
         ISAVE( 1 ) = 1
         RETURN
      END IF
*
      GO TO ( 20, 40, 70, 110, 140 )ISAVE( 1 )
*
*     ................ ENTRY   (ISAVE( 1 ) = 1)
*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY A*X.
*
   20 CONTINUE
      IF( N.EQ.1 ) THEN
         V( 1 ) = X( 1 )
         EST = ABS( V( 1 ) )
*        ... QUIT
         GO TO 150
      END IF
      EST = DASUM( N, X, 1 )
*
      DO 30 I = 1, N
         X( I ) = SIGN( ONE, X( I ) )
         ISGN( I ) = NINT( X( I ) )
   30 CONTINUE
      KASE = 2
      ISAVE( 1 ) = 2
      RETURN
*
*     ................ ENTRY   (ISAVE( 1 ) = 2)
*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY TRANSPOSE(A)*X.
*
   40 CONTINUE
      ISAVE( 2 ) = IDAMAX( N, X, 1 )
      ISAVE( 3 ) = 2
*
*     MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
*
   50 CONTINUE
      DO 60 I = 1, N
         X( I ) = ZERO
   60 CONTINUE
      X( ISAVE( 2 ) ) = ONE
      KASE = 1
      ISAVE( 1 ) = 3
      RETURN
*
*     ................ ENTRY   (ISAVE( 1 ) = 3)
*     X HAS BEEN OVERWRITTEN BY A*X.
*
   70 CONTINUE
      CALL DCOPY( N, X, 1, V, 1 )
      ESTOLD = EST
      EST = DASUM( N, V, 1 )
      DO 80 I = 1, N
         IF( NINT( SIGN( ONE, X( I ) ) ).NE.ISGN( I ) )
     $      GO TO 90
   80 CONTINUE
*     REPEATED SIGN VECTOR DETECTED, HENCE ALGORITHM HAS CONVERGED.
      GO TO 120
*
   90 CONTINUE
*     TEST FOR CYCLING.
      IF( EST.LE.ESTOLD )
     $   GO TO 120
*
      DO 100 I = 1, N
         X( I ) = SIGN( ONE, X( I ) )
         ISGN( I ) = NINT( X( I ) )
  100 CONTINUE
      KASE = 2
      ISAVE( 1 ) = 4
      RETURN
*
*     ................ ENTRY   (ISAVE( 1 ) = 4)
*     X HAS BEEN OVERWRITTEN BY TRANSPOSE(A)*X.
*
  110 CONTINUE
      JLAST = ISAVE( 2 )
      ISAVE( 2 ) = IDAMAX( N, X, 1 )
      IF( ( X( JLAST ).NE.ABS( X( ISAVE( 2 ) ) ) ) .AND.
     $    ( ISAVE( 3 ).LT.ITMAX ) ) THEN
         ISAVE( 3 ) = ISAVE( 3 ) + 1
         GO TO 50
      END IF
*
*     ITERATION COMPLETE.  FINAL STAGE.
*
  120 CONTINUE
      ALTSGN = ONE
      DO 130 I = 1, N
         X( I ) = ALTSGN*( ONE+DBLE( I-1 ) / DBLE( N-1 ) )
         ALTSGN = -ALTSGN
  130 CONTINUE
      KASE = 1
      ISAVE( 1 ) = 5
      RETURN
*
*     ................ ENTRY   (ISAVE( 1 ) = 5)
*     X HAS BEEN OVERWRITTEN BY A*X.
*
  140 CONTINUE
      TEMP = TWO*( DASUM( N, X, 1 ) / DBLE( 3*N ) )
      IF( TEMP.GT.EST ) THEN
         CALL DCOPY( N, X, 1, V, 1 )
         EST = TEMP
      END IF
*
  150 CONTINUE
      KASE = 0
      RETURN
*
*     End of DLACN2
*
      END
      SUBROUTINE DLAHR2( N, K, NB, A, LDA, TAU, T, LDT, Y, LDY )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            K, LDA, LDT, LDY, N, NB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION  A( LDA, * ), T( LDT, NB ), TAU( NB ),
     $                   Y( LDY, NB )
*     ..
*
*  Purpose
*  =======
*
*  DLAHR2 reduces the first NB columns of A real general n-BY-(n-k+1)
*  matrix A so that elements below the k-th subdiagonal are zero. The
*  reduction is performed by an orthogonal similarity transformation
*  Q' * A * Q. The routine returns the matrices V and T which determine
*  Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T.
*
*  This is an auxiliary routine called by DGEHRD.
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
*          K < N.
*
*  NB      (input) INTEGER
*          The number of columns to be reduced.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N-K+1)
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
*  TAU     (output) DOUBLE PRECISION array, dimension (NB)
*          The scalar factors of the elementary reflectors. See Further
*          Details.
*
*  T       (output) DOUBLE PRECISION array, dimension (LDT,NB)
*          The upper triangular matrix T.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T.  LDT >= NB.
*
*  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
*          The n-by-nb matrix Y.
*
*  LDY     (input) INTEGER
*          The leading dimension of the array Y. LDY >= N.
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
*  where tau is a real scalar, and v is a real vector with
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
*     ( a   a   a   a   a )
*     ( a   a   a   a   a )
*     ( a   a   a   a   a )
*     ( h   h   a   a   a )
*     ( v1  h   a   a   a )
*     ( v1  v2  a   a   a )
*     ( v1  v2  a   a   a )
*
*  where a denotes an element of the original matrix A, h denotes a
*  modified element of the upper Hessenberg matrix H, and vi denotes an
*  element of the vector defining H(i).
*
*  This file is a slight modification of LAPACK-3.0's DLAHRD
*  incorporating improvements proposed by Quintana-Orti and Van de
*  Gejin. Note that the entries of A(1:K,2:NB) differ from those
*  returned by the original LAPACK routine. This function is
*  not backward compatible with LAPACK3.0.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION  ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, 
     $                     ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION  EI
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DGEMV, DLACPY,
     $                   DLARFG, DSCAL, DTRMM, DTRMV
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
*           Update A(K+1:N,I)
*
*           Update I-th column of A - Y * V'
*
            CALL DGEMV( 'NO TRANSPOSE', N-K, I-1, -ONE, Y(K+1,1), LDY,
     $                  A( K+I-1, 1 ), LDA, ONE, A( K+1, I ), 1 )
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
            CALL DCOPY( I-1, A( K+1, I ), 1, T( 1, NB ), 1 )
            CALL DTRMV( 'Lower', 'Transpose', 'UNIT', 
     $                  I-1, A( K+1, 1 ),
     $                  LDA, T( 1, NB ), 1 )
*
*           w := w + V2'*b2
*
            CALL DGEMV( 'Transpose', N-K-I+1, I-1, 
     $                  ONE, A( K+I, 1 ),
     $                  LDA, A( K+I, I ), 1, ONE, T( 1, NB ), 1 )
*
*           w := T'*w
*
            CALL DTRMV( 'Upper', 'Transpose', 'NON-UNIT', 
     $                  I-1, T, LDT,
     $                  T( 1, NB ), 1 )
*
*           b2 := b2 - V2*w
*
            CALL DGEMV( 'NO TRANSPOSE', N-K-I+1, I-1, -ONE, 
     $                  A( K+I, 1 ),
     $                  LDA, T( 1, NB ), 1, ONE, A( K+I, I ), 1 )
*
*           b1 := b1 - V1*w
*
            CALL DTRMV( 'Lower', 'NO TRANSPOSE', 
     $                  'UNIT', I-1,
     $                  A( K+1, 1 ), LDA, T( 1, NB ), 1 )
            CALL DAXPY( I-1, -ONE, T( 1, NB ), 1, A( K+1, I ), 1 )
*
            A( K+I-1, I-1 ) = EI
         END IF
*
*        Generate the elementary reflector H(I) to annihilate
*        A(K+I+1:N,I)
*
         CALL DLARFG( N-K-I+1, A( K+I, I ), A( MIN( K+I+1, N ), I ), 1,
     $                TAU( I ) )
         EI = A( K+I, I )
         A( K+I, I ) = ONE
*
*        Compute  Y(K+1:N,I)
*
         CALL DGEMV( 'NO TRANSPOSE', N-K, N-K-I+1, 
     $               ONE, A( K+1, I+1 ),
     $               LDA, A( K+I, I ), 1, ZERO, Y( K+1, I ), 1 )
         CALL DGEMV( 'Transpose', N-K-I+1, I-1, 
     $               ONE, A( K+I, 1 ), LDA,
     $               A( K+I, I ), 1, ZERO, T( 1, I ), 1 )
         CALL DGEMV( 'NO TRANSPOSE', N-K, I-1, -ONE, 
     $               Y( K+1, 1 ), LDY,
     $               T( 1, I ), 1, ONE, Y( K+1, I ), 1 )
         CALL DSCAL( N-K, TAU( I ), Y( K+1, I ), 1 )
*
*        Compute T(1:I,I)
*
         CALL DSCAL( I-1, -TAU( I ), T( 1, I ), 1 )
         CALL DTRMV( 'Upper', 'No Transpose', 'NON-UNIT', 
     $               I-1, T, LDT,
     $               T( 1, I ), 1 )
         T( I, I ) = TAU( I )
*
   10 CONTINUE
      A( K+NB, NB ) = EI
*
*     Compute Y(1:K,1:NB)
*
      CALL DLACPY( 'ALL', K, NB, A( 1, 2 ), LDA, Y, LDY )
      CALL DTRMM( 'RIGHT', 'Lower', 'NO TRANSPOSE', 
     $            'UNIT', K, NB,
     $            ONE, A( K+1, 1 ), LDA, Y, LDY )
      IF( N.GT.K+NB )
     $   CALL DGEMM( 'NO TRANSPOSE', 'NO TRANSPOSE', K, 
     $               NB, N-K-NB, ONE,
     $               A( 1, 2+NB ), LDA, A( K+1+NB, 1 ), LDA, ONE, Y,
     $               LDY )
      CALL DTRMM( 'RIGHT', 'Upper', 'NO TRANSPOSE', 
     $            'NON-UNIT', K, NB,
     $            ONE, T, LDT, Y, LDY )
*
      RETURN
*
*     End of DLAHR2
*
      END
      LOGICAL FUNCTION DLAISNAN(DIN1,DIN2)
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION DIN1,DIN2
*     ..
*
*  Purpose
*  =======
*
*  This routine is not for general use.  It exists solely to avoid
*  over-optimization in DISNAN.
*
*  DLAISNAN checks for NaNs by comparing its two arguments for
*  inequality.  NaN is the only floating-point value where NaN != NaN
*  returns .TRUE.  To check for NaNs, pass the same variable as both
*  arguments.
*
*  Strictly speaking, Fortran does not allow aliasing of function
*  arguments. So a compiler must assume that the two arguments are
*  not the same variable, and the test will not be optimized away.
*  Interprocedural or whole-program optimization may delete this
*  test.  The ISNAN functions will be replaced by the correct
*  Fortran 03 intrinsic once the intrinsic is widely available.
*
*  Arguments
*  =========
*
*  DIN1     (input) DOUBLE PRECISION
*  DIN2     (input) DOUBLE PRECISION
*          Two numbers to compare for inequality.
*
*  =====================================================================
*
*  .. Executable Statements ..
      DLAISNAN = (DIN1.NE.DIN2)
      RETURN
      END
      FUNCTION DLANEG( N, D, LLD, SIGMA, PIVMIN, R )
      IMPLICIT NONE
      INTEGER DLANEG
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            N, R
      DOUBLE PRECISION   PIVMIN, SIGMA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), LLD( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANEG computes the Sturm count, the number of negative pivots
*  encountered while factoring tridiagonal T - sigma I = L D L^T.
*  This implementation works directly on the factors without forming
*  the tridiagonal matrix T.  The Sturm count is also the number of
*  eigenvalues of T less than sigma.
*
*  This routine is called from DLARRB.
*
*  The current routine does not use the PIVMIN parameter but rather
*  requires IEEE-754 propagation of Infinities and NaNs.  This
*  routine also has no input range restrictions but does require
*  default exception handling such that x/0 produces Inf when x is
*  non-zero, and Inf/Inf produces NaN.  For more information, see:
*
*    Marques, Riedy, and Voemel, "Benefits of IEEE-754 Features in
*    Modern Symmetric Tridiagonal Eigensolvers," SIAM Journal on
*    Scientific Computing, v28, n5, 2006.  DOI 10.1137/050641624
*    (Tech report version in LAWN 172 with the same title.)
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The N diagonal elements of the diagonal matrix D.
*
*  LLD     (input) DOUBLE PRECISION array, dimension (N-1)
*          The (N-1) elements L(i)*L(i)*D(i).
*
*  SIGMA   (input) DOUBLE PRECISION
*          Shift amount in T - sigma I = L D L^T.
*
*  PIVMIN  (input) DOUBLE PRECISION
*          The minimum pivot in the Sturm sequence.  May be used
*          when zero pivots are encountered on non-IEEE-754
*          architectures.
*
*  R       (input) INTEGER
*          The twist index for the twisted factorization that is used
*          for the negcount.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Osni Marques, LBNL/NERSC, USA
*     Christof Voemel, University of California, Berkeley, USA
*     Jason Riedy, University of California, Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
*     Some architectures propagate Infinities and NaNs very slowly, so
*     the code computes counts in BLKLEN chunks.  Then a NaN can
*     propagate at most BLKLEN columns before being detected.  This is
*     not a general tuning parameter; it needs only to be just large
*     enough that the overhead is tiny in common cases.
      INTEGER BLKLEN
      PARAMETER ( BLKLEN = 128 )
*     ..
*     .. Local Scalars ..
      INTEGER            BJ, J, NEG1, NEG2, NEGCNT
      DOUBLE PRECISION   BSAV, DMINUS, DPLUS, GAMMA, P, T, TMP
      LOGICAL SAWNAN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC MIN, MAX
*     ..
*     .. External Functions ..
      LOGICAL DISNAN
      EXTERNAL DISNAN
*     ..
*     .. Executable Statements ..

      NEGCNT = 0

*     I) upper part: L D L^T - SIGMA I = L+ D+ L+^T
      T = -SIGMA
      DO 210 BJ = 1, R-1, BLKLEN
         NEG1 = 0
         BSAV = T
         DO 21 J = BJ, MIN(BJ+BLKLEN-1, R-1)
            DPLUS = D( J ) + T
            IF( DPLUS.LT.ZERO ) NEG1 = NEG1 + 1
            TMP = T / DPLUS
            T = TMP * LLD( J ) - SIGMA
 21      CONTINUE
         SAWNAN = DISNAN( T )
*     Run a slower version of the above loop if a NaN is detected.
*     A NaN should occur only with a zero pivot after an infinite
*     pivot.  In that case, substituting 1 for T/DPLUS is the
*     correct limit.
         IF( SAWNAN ) THEN
            NEG1 = 0
            T = BSAV
            DO 22 J = BJ, MIN(BJ+BLKLEN-1, R-1)
               DPLUS = D( J ) + T
               IF( DPLUS.LT.ZERO ) NEG1 = NEG1 + 1
               TMP = T / DPLUS
               IF (DISNAN(TMP)) TMP = ONE
               T = TMP * LLD(J) - SIGMA
 22         CONTINUE
         END IF
         NEGCNT = NEGCNT + NEG1
 210  CONTINUE
*
*     II) lower part: L D L^T - SIGMA I = U- D- U-^T
      P = D( N ) - SIGMA
      DO 230 BJ = N-1, R, -BLKLEN
         NEG2 = 0
         BSAV = P
         DO 23 J = BJ, MAX(BJ-BLKLEN+1, R), -1
            DMINUS = LLD( J ) + P
            IF( DMINUS.LT.ZERO ) NEG2 = NEG2 + 1
            TMP = P / DMINUS
            P = TMP * D( J ) - SIGMA
 23      CONTINUE
         SAWNAN = DISNAN( P )
*     As above, run a slower version that substitutes 1 for Inf/Inf.
*
         IF( SAWNAN ) THEN
            NEG2 = 0
            P = BSAV
            DO 24 J = BJ, MAX(BJ-BLKLEN+1, R), -1
               DMINUS = LLD( J ) + P
               IF( DMINUS.LT.ZERO ) NEG2 = NEG2 + 1
               TMP = P / DMINUS
               IF (DISNAN(TMP)) TMP = ONE
               P = TMP * D(J) - SIGMA
 24         CONTINUE
         END IF
         NEGCNT = NEGCNT + NEG2
 230  CONTINUE
*
*     III) Twist index
*       T was shifted by SIGMA initially.
      GAMMA = (T + SIGMA) + P
      IF( GAMMA.LT.ZERO ) NEGCNT = NEGCNT+1

      DLANEG = NEGCNT
      END
      SUBROUTINE DLAQR0( WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
     $                   ILOZ, IHIZ, Z, LDZ, WORK, LWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, LWORK, N
      LOGICAL            WANTT, WANTZ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), WI( * ), WORK( * ), WR( * ),
     $                   Z( LDZ, * )
*     ..
*
*     Purpose
*     =======
*
*     DLAQR0 computes the eigenvalues of a Hessenberg matrix H
*     and, optionally, the matrices T and Z from the Schur decomposition
*     H = Z T Z**T, where T is an upper quasi-triangular matrix (the
*     Schur form), and Z is the orthogonal matrix of Schur vectors.
*
*     Optionally Z may be postmultiplied into an input orthogonal
*     matrix Q so that this routine can give the Schur factorization
*     of a matrix A which has been reduced to the Hessenberg form H
*     by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
*
*     Arguments
*     =========
*
*     WANTT   (input) LOGICAL
*          = .TRUE. : the full Schur form T is required;
*          = .FALSE.: only eigenvalues are required.
*
*     WANTZ   (input) LOGICAL
*          = .TRUE. : the matrix of Schur vectors Z is required;
*          = .FALSE.: Schur vectors are not required.
*
*     N     (input) INTEGER
*           The order of the matrix H.  N .GE. 0.
*
*     ILO   (input) INTEGER
*     IHI   (input) INTEGER
*           It is assumed that H is already upper triangular in rows
*           and columns 1:ILO-1 and IHI+1:N and, if ILO.GT.1,
*           H(ILO,ILO-1) is zero. ILO and IHI are normally set by a
*           previous call to DGEBAL, and then passed to DGEHRD when the
*           matrix output by DGEBAL is reduced to Hessenberg form.
*           Otherwise, ILO and IHI should be set to 1 and N,
*           respectively.  If N.GT.0, then 1.LE.ILO.LE.IHI.LE.N.
*           If N = 0, then ILO = 1 and IHI = 0.
*
*     H     (input/output) DOUBLE PRECISION array, dimension (LDH,N)
*           On entry, the upper Hessenberg matrix H.
*           On exit, if INFO = 0 and WANTT is .TRUE., then H contains
*           the upper quasi-triangular matrix T from the Schur
*           decomposition (the Schur form); 2-by-2 diagonal blocks
*           (corresponding to complex conjugate pairs of eigenvalues)
*           are returned in standard form, with H(i,i) = H(i+1,i+1)
*           and H(i+1,i)*H(i,i+1).LT.0. If INFO = 0 and WANTT is
*           .FALSE., then the contents of H are unspecified on exit.
*           (The output value of H when INFO.GT.0 is given under the
*           description of INFO below.)
*
*           This subroutine may explicitly set H(i,j) = 0 for i.GT.j and
*           j = 1, 2, ... ILO-1 or j = IHI+1, IHI+2, ... N.
*
*     LDH   (input) INTEGER
*           The leading dimension of the array H. LDH .GE. max(1,N).
*
*     WR    (output) DOUBLE PRECISION array, dimension (IHI)
*     WI    (output) DOUBLE PRECISION array, dimension (IHI)
*           The real and imaginary parts, respectively, of the computed
*           eigenvalues of H(ILO:IHI,ILO:IHI) are stored WR(ILO:IHI)
*           and WI(ILO:IHI). If two eigenvalues are computed as a
*           complex conjugate pair, they are stored in consecutive
*           elements of WR and WI, say the i-th and (i+1)th, with
*           WI(i) .GT. 0 and WI(i+1) .LT. 0. If WANTT is .TRUE., then
*           the eigenvalues are stored in the same order as on the
*           diagonal of the Schur form returned in H, with
*           WR(i) = H(i,i) and, if H(i:i+1,i:i+1) is a 2-by-2 diagonal
*           block, WI(i) = sqrt(-H(i+1,i)*H(i,i+1)) and
*           WI(i+1) = -WI(i).
*
*     ILOZ     (input) INTEGER
*     IHIZ     (input) INTEGER
*           Specify the rows of Z to which transformations must be
*           applied if WANTZ is .TRUE..
*           1 .LE. ILOZ .LE. ILO; IHI .LE. IHIZ .LE. N.
*
*     Z     (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
*           If WANTZ is .FALSE., then Z is not referenced.
*           If WANTZ is .TRUE., then Z(ILO:IHI,ILOZ:IHIZ) is
*           replaced by Z(ILO:IHI,ILOZ:IHIZ)*U where U is the
*           orthogonal Schur factor of H(ILO:IHI,ILO:IHI).
*           (The output value of Z when INFO.GT.0 is given under
*           the description of INFO below.)
*
*     LDZ   (input) INTEGER
*           The leading dimension of the array Z.  if WANTZ is .TRUE.
*           then LDZ.GE.MAX(1,IHIZ).  Otherwize, LDZ.GE.1.
*
*     WORK  (workspace/output) DOUBLE PRECISION array, dimension LWORK
*           On exit, if LWORK = -1, WORK(1) returns an estimate of
*           the optimal value for LWORK.
*
*     LWORK (input) INTEGER
*           The dimension of the array WORK.  LWORK .GE. max(1,N)
*           is sufficient, but LWORK typically as large as 6*N may
*           be required for optimal performance.  A workspace query
*           to determine the optimal workspace size is recommended.
*
*           If LWORK = -1, then DLAQR0 does a workspace query.
*           In this case, DLAQR0 checks the input parameters and
*           estimates the optimal workspace size for the given
*           values of N, ILO and IHI.  The estimate is returned
*           in WORK(1).  No error message related to LWORK is
*           issued by XERBLA.  Neither H nor Z are accessed.
*
*
*     INFO  (output) INTEGER
*             =  0:  successful exit
*           .GT. 0:  if INFO = i, DLAQR0 failed to compute all of
*                the eigenvalues.  Elements 1:ilo-1 and i+1:n of WR
*                and WI contain those eigenvalues which have been
*                successfully computed.  (Failures are rare.)
*
*                If INFO .GT. 0 and WANT is .FALSE., then on exit,
*                the remaining unconverged eigenvalues are the eigen-
*                values of the upper Hessenberg matrix rows and
*                columns ILO through INFO of the final, output
*                value of H.
*
*                If INFO .GT. 0 and WANTT is .TRUE., then on exit
*
*           (*)  (initial value of H)*U  = U*(final value of H)
*
*                where U is an orthogonal matrix.  The final
*                value of H is upper Hessenberg and quasi-triangular
*                in rows and columns INFO+1 through IHI.
*
*                If INFO .GT. 0 and WANTZ is .TRUE., then on exit
*
*                  (final value of Z(ILO:IHI,ILOZ:IHIZ)
*                   =  (initial value of Z(ILO:IHI,ILOZ:IHIZ)*U
*
*                where U is the orthogonal matrix in (*) (regard-
*                less of the value of WANTT.)
*
*                If INFO .GT. 0 and WANTZ is .FALSE., then Z is not
*                accessed.
*
*
*     ================================================================
*     Based on contributions by
*        Karen Braman and Ralph Byers, Department of Mathematics,
*        University of Kansas, USA
*
*     ================================================================
*
*     References:
*       K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
*       Algorithm Part I: Maintaining Well Focused Shifts, and Level 3
*       Performance, SIAM Journal of Matrix Analysis, volume 23, pages
*       929--947, 2002.
*
*       K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
*       Algorithm Part II: Aggressive Early Deflation, SIAM Journal
*       of Matrix Analysis, volume 23, pages 948--973, 2002.
*
*     ================================================================
*     .. Parameters ..
*
*     ==== Matrices of order NTINY or smaller must be processed by
*     .    DLAHQR because of insufficient subdiagonal scratch space.
*     .    (This is a hard limit.) ====
*
*     ==== Exceptional deflation windows:  try to cure rare
*     .    slow convergence by increasing the size of the
*     .    deflation window after KEXNW iterations. =====
*
*     ==== Exceptional shifts: try to cure rare slow convergence
*     .    with ad-hoc exceptional shifts every KEXSH iterations.
*     .    The constants WILK1 and WILK2 are used to form the
*     .    exceptional shifts. ====
*
      INTEGER            NTINY
      PARAMETER          ( NTINY = 11 )
      INTEGER            KEXNW, KEXSH
      PARAMETER          ( KEXNW = 5, KEXSH = 6 )
      DOUBLE PRECISION   WILK1, WILK2
      PARAMETER          ( WILK1 = 0.75d0, WILK2 = -0.4375d0 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0d0, ONE = 1.0d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AA, BB, CC, CS, DD, SN, SS, SWAP
      INTEGER            I, INF, IT, ITMAX, K, KACC22, KBOT, KDU, KS,
     $                   KT, KTOP, KU, KV, KWH, KWTOP, KWV, LD, LS,
     $                   LWKOPT, NDFL, NH, NHO, NIBBLE, NMIN, NS, NSMAX,
     $                   NSR, NVE, NW, NWMAX, NWR
      LOGICAL            NWINC, SORTED
      CHARACTER          JBCMPZ*2
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   ZDUM( 1, 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLACPY, DLAHQR, DLANV2, DLAQR3, DLAQR4, DLAQR5
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MIN, MOD
*     ..
*     .. Executable Statements ..
      INFO = 0
*
*     ==== Quick return for N = 0: nothing to do. ====
*
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = ONE
         RETURN
      END IF
*
*     ==== Set up job flags for ILAENV. ====
*
      IF( WANTT ) THEN
         JBCMPZ( 1: 1 ) = 'S'
      ELSE
         JBCMPZ( 1: 1 ) = 'E'
      END IF
      IF( WANTZ ) THEN
         JBCMPZ( 2: 2 ) = 'V'
      ELSE
         JBCMPZ( 2: 2 ) = 'N'
      END IF
*
*     ==== Tiny matrices must use DLAHQR. ====
*
      IF( N.LE.NTINY ) THEN
*
*        ==== Estimate optimal workspace. ====
*
         LWKOPT = 1
         IF( LWORK.NE.-1 )
     $      CALL DLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
     $                   ILOZ, IHIZ, Z, LDZ, INFO )
      ELSE
*
*        ==== Use small bulge multi-shift QR with aggressive early
*        .    deflation on larger-than-tiny matrices. ====
*
*        ==== Hope for the best. ====
*
         INFO = 0
*
*        ==== NWR = recommended deflation window size.  At this
*        .    point,  N .GT. NTINY = 11, so there is enough
*        .    subdiagonal workspace for NWR.GE.2 as required.
*        .    (In fact, there is enough subdiagonal space for
*        .    NWR.GE.3.) ====
*
         NWR = ILAENV( 13, 'DLAQR0', JBCMPZ, N, ILO, IHI, LWORK )
         NWR = MAX( 2, NWR )
         NWR = MIN( IHI-ILO+1, ( N-1 ) / 3, NWR )
         NW = NWR
*
*        ==== NSR = recommended number of simultaneous shifts.
*        .    At this point N .GT. NTINY = 11, so there is at
*        .    enough subdiagonal workspace for NSR to be even
*        .    and greater than or equal to two as required. ====
*
         NSR = ILAENV( 15, 'DLAQR0', JBCMPZ, N, ILO, IHI, LWORK )
         NSR = MIN( NSR, ( N+6 ) / 9, IHI-ILO )
         NSR = MAX( 2, NSR-MOD( NSR, 2 ) )
*
*        ==== Estimate optimal workspace ====
*
*        ==== Workspace query call to DLAQR3 ====
*
         CALL DLAQR3( WANTT, WANTZ, N, ILO, IHI, NWR+1, H, LDH, ILOZ,
     $                IHIZ, Z, LDZ, LS, LD, WR, WI, H, LDH, N, H, LDH,
     $                N, H, LDH, WORK, -1 )
*
*        ==== Optimal workspace = MAX(DLAQR5, DLAQR3) ====
*
         LWKOPT = MAX( 3*NSR / 2, INT( WORK( 1 ) ) )
*
*        ==== Quick return in case of workspace query. ====
*
         IF( LWORK.EQ.-1 ) THEN
            WORK( 1 ) = DBLE( LWKOPT )
            RETURN
         END IF
*
*        ==== DLAHQR/DLAQR0 crossover point ====
*
         NMIN = ILAENV( 12, 'DLAQR0', JBCMPZ, N, ILO, IHI, LWORK )
         NMIN = MAX( NTINY, NMIN )
*
*        ==== Nibble crossover point ====
*
         NIBBLE = ILAENV( 14, 'DLAQR0', JBCMPZ, N, ILO, IHI, LWORK )
         NIBBLE = MAX( 0, NIBBLE )
*
*        ==== Accumulate reflections during ttswp?  Use block
*        .    2-by-2 structure during matrix-matrix multiply? ====
*
         KACC22 = ILAENV( 16, 'DLAQR0', JBCMPZ, N, ILO, IHI, LWORK )
         KACC22 = MAX( 0, KACC22 )
         KACC22 = MIN( 2, KACC22 )
*
*        ==== NWMAX = the largest possible deflation window for
*        .    which there is sufficient workspace. ====
*
         NWMAX = MIN( ( N-1 ) / 3, LWORK / 2 )
*
*        ==== NSMAX = the Largest number of simultaneous shifts
*        .    for which there is sufficient workspace. ====
*
         NSMAX = MIN( ( N+6 ) / 9, 2*LWORK / 3 )
         NSMAX = NSMAX - MOD( NSMAX, 2 )
*
*        ==== NDFL: an iteration count restarted at deflation. ====
*
         NDFL = 1
*
*        ==== ITMAX = iteration limit ====
*
         ITMAX = MAX( 30, 2*KEXSH )*MAX( 10, ( IHI-ILO+1 ) )
*
*        ==== Last row and column in the active block ====
*
         KBOT = IHI
*
*        ==== Main Loop ====
*
         DO 80 IT = 1, ITMAX
*
*           ==== Done when KBOT falls below ILO ====
*
            IF( KBOT.LT.ILO )
     $         GO TO 90
*
*           ==== Locate active block ====
*
            DO 10 K = KBOT, ILO + 1, -1
               IF( H( K, K-1 ).EQ.ZERO )
     $            GO TO 20
   10       CONTINUE
            K = ILO
   20       CONTINUE
            KTOP = K
*
*           ==== Select deflation window size ====
*
            NH = KBOT - KTOP + 1
            IF( NDFL.LT.KEXNW .OR. NH.LT.NW ) THEN
*
*              ==== Typical deflation window.  If possible and
*              .    advisable, nibble the entire active block.
*              .    If not, use size NWR or NWR+1 depending upon
*              .    which has the smaller corresponding subdiagonal
*              .    entry (a heuristic). ====
*
               NWINC = .TRUE.
               IF( NH.LE.MIN( NMIN, NWMAX ) ) THEN
                  NW = NH
               ELSE
                  NW = MIN( NWR, NH, NWMAX )
                  IF( NW.LT.NWMAX ) THEN
                     IF( NW.GE.NH-1 ) THEN
                        NW = NH
                     ELSE
                        KWTOP = KBOT - NW + 1
                        IF( ABS( H( KWTOP, KWTOP-1 ) ).GT.
     $                      ABS( H( KWTOP-1, KWTOP-2 ) ) )NW = NW + 1
                     END IF
                  END IF
               END IF
            ELSE
*
*              ==== Exceptional deflation window.  If there have
*              .    been no deflations in KEXNW or more iterations,
*              .    then vary the deflation window size.   At first,
*              .    because, larger windows are, in general, more
*              .    powerful than smaller ones, rapidly increase the
*              .    window up to the maximum reasonable and possible.
*              .    Then maybe try a slightly smaller window.  ====
*
               IF( NWINC .AND. NW.LT.MIN( NWMAX, NH ) ) THEN
                  NW = MIN( NWMAX, NH, 2*NW )
               ELSE
                  NWINC = .FALSE.
                  IF( NW.EQ.NH .AND. NH.GT.2 )
     $               NW = NH - 1
               END IF
            END IF
*
*           ==== Aggressive early deflation:
*           .    split workspace under the subdiagonal into
*           .      - an nw-by-nw work array V in the lower
*           .        left-hand-corner,
*           .      - an NW-by-at-least-NW-but-more-is-better
*           .        (NW-by-NHO) horizontal work array along
*           .        the bottom edge,
*           .      - an at-least-NW-but-more-is-better (NHV-by-NW)
*           .        vertical work array along the left-hand-edge.
*           .        ====
*
            KV = N - NW + 1
            KT = NW + 1
            NHO = ( N-NW-1 ) - KT + 1
            KWV = NW + 2
            NVE = ( N-NW ) - KWV + 1
*
*           ==== Aggressive early deflation ====
*
            CALL DLAQR3( WANTT, WANTZ, N, KTOP, KBOT, NW, H, LDH, ILOZ,
     $                   IHIZ, Z, LDZ, LS, LD, WR, WI, H( KV, 1 ), LDH,
     $                   NHO, H( KV, KT ), LDH, NVE, H( KWV, 1 ), LDH,
     $                   WORK, LWORK )
*
*           ==== Adjust KBOT accounting for new deflations. ====
*
            KBOT = KBOT - LD
*
*           ==== KS points to the shifts. ====
*
            KS = KBOT - LS + 1
*
*           ==== Skip an expensive QR sweep if there is a (partly
*           .    heuristic) reason to expect that many eigenvalues
*           .    will deflate without it.  Here, the QR sweep is
*           .    skipped if many eigenvalues have just been deflated
*           .    or if the remaining active block is small.
*
            IF( ( LD.EQ.0 ) .OR. ( ( 100*LD.LE.NW*NIBBLE ) .AND. ( KBOT-
     $          KTOP+1.GT.MIN( NMIN, NWMAX ) ) ) ) THEN
*
*              ==== NS = nominal number of simultaneous shifts.
*              .    This may be lowered (slightly) if DLAQR3
*              .    did not provide that many shifts. ====
*
               NS = MIN( NSMAX, NSR, MAX( 2, KBOT-KTOP ) )
               NS = NS - MOD( NS, 2 )
*
*              ==== If there have been no deflations
*              .    in a multiple of KEXSH iterations,
*              .    then try exceptional shifts.
*              .    Otherwise use shifts provided by
*              .    DLAQR3 above or from the eigenvalues
*              .    of a trailing principal submatrix. ====
*
               IF( MOD( NDFL, KEXSH ).EQ.0 ) THEN
                  KS = KBOT - NS + 1
                  DO 30 I = KBOT, MAX( KS+1, KTOP+2 ), -2
                     SS = ABS( H( I, I-1 ) ) + ABS( H( I-1, I-2 ) )
                     AA = WILK1*SS + H( I, I )
                     BB = SS
                     CC = WILK2*SS
                     DD = AA
                     CALL DLANV2( AA, BB, CC, DD, WR( I-1 ), WI( I-1 ),
     $                            WR( I ), WI( I ), CS, SN )
   30             CONTINUE
                  IF( KS.EQ.KTOP ) THEN
                     WR( KS+1 ) = H( KS+1, KS+1 )
                     WI( KS+1 ) = ZERO
                     WR( KS ) = WR( KS+1 )
                     WI( KS ) = WI( KS+1 )
                  END IF
               ELSE
*
*                 ==== Got NS/2 or fewer shifts? Use DLAQR4 or
*                 .    DLAHQR on a trailing principal submatrix to
*                 .    get more. (Since NS.LE.NSMAX.LE.(N+6)/9,
*                 .    there is enough space below the subdiagonal
*                 .    to fit an NS-by-NS scratch array.) ====
*
                  IF( KBOT-KS+1.LE.NS / 2 ) THEN
                     KS = KBOT - NS + 1
                     KT = N - NS + 1
                     CALL DLACPY( 'A', NS, NS, H( KS, KS ), LDH,
     $                            H( KT, 1 ), LDH )
                     IF( NS.GT.NMIN ) THEN
                        CALL DLAQR4( .false., .false., NS, 1, NS,
     $                               H( KT, 1 ), LDH, WR( KS ),
     $                               WI( KS ), 1, 1, ZDUM, 1, WORK,
     $                               LWORK, INF )
                     ELSE
                        CALL DLAHQR( .false., .false., NS, 1, NS,
     $                               H( KT, 1 ), LDH, WR( KS ),
     $                               WI( KS ), 1, 1, ZDUM, 1, INF )
                     END IF
                     KS = KS + INF
*
*                    ==== In case of a rare QR failure use
*                    .    eigenvalues of the trailing 2-by-2
*                    .    principal submatrix.  ====
*
                     IF( KS.GE.KBOT ) THEN
                        AA = H( KBOT-1, KBOT-1 )
                        CC = H( KBOT, KBOT-1 )
                        BB = H( KBOT-1, KBOT )
                        DD = H( KBOT, KBOT )
                        CALL DLANV2( AA, BB, CC, DD, WR( KBOT-1 ),
     $                               WI( KBOT-1 ), WR( KBOT ),
     $                               WI( KBOT ), CS, SN )
                        KS = KBOT - 1
                     END IF
                  END IF
*
                  IF( KBOT-KS+1.GT.NS ) THEN
*
*                    ==== Sort the shifts (Helps a little)
*                    .    Bubble sort keeps complex conjugate
*                    .    pairs together. ====
*
                     SORTED = .false.
                     DO 50 K = KBOT, KS + 1, -1
                        IF( SORTED )
     $                     GO TO 60
                        SORTED = .true.
                        DO 40 I = KS, K - 1
                           IF( ABS( WR( I ) )+ABS( WI( I ) ).LT.
     $                         ABS( WR( I+1 ) )+ABS( WI( I+1 ) ) ) THEN
                              SORTED = .false.
*
                              SWAP = WR( I )
                              WR( I ) = WR( I+1 )
                              WR( I+1 ) = SWAP
*
                              SWAP = WI( I )
                              WI( I ) = WI( I+1 )
                              WI( I+1 ) = SWAP
                           END IF
   40                   CONTINUE
   50                CONTINUE
   60                CONTINUE
                  END IF
*
*                 ==== Shuffle shifts into pairs of real shifts
*                 .    and pairs of complex conjugate shifts
*                 .    assuming complex conjugate shifts are
*                 .    already adjacent to one another. (Yes,
*                 .    they are.)  ====
*
                  DO 70 I = KBOT, KS + 2, -2
                     IF( WI( I ).NE.-WI( I-1 ) ) THEN
*
                        SWAP = WR( I )
                        WR( I ) = WR( I-1 )
                        WR( I-1 ) = WR( I-2 )
                        WR( I-2 ) = SWAP
*
                        SWAP = WI( I )
                        WI( I ) = WI( I-1 )
                        WI( I-1 ) = WI( I-2 )
                        WI( I-2 ) = SWAP
                     END IF
   70             CONTINUE
               END IF
*
*              ==== If there are only two shifts and both are
*              .    real, then use only one.  ====
*
               IF( KBOT-KS+1.EQ.2 ) THEN
                  IF( WI( KBOT ).EQ.ZERO ) THEN
                     IF( ABS( WR( KBOT )-H( KBOT, KBOT ) ).LT.
     $                   ABS( WR( KBOT-1 )-H( KBOT, KBOT ) ) ) THEN
                        WR( KBOT-1 ) = WR( KBOT )
                     ELSE
                        WR( KBOT ) = WR( KBOT-1 )
                     END IF
                  END IF
               END IF
*
*              ==== Use up to NS of the the smallest magnatiude
*              .    shifts.  If there aren't NS shifts available,
*              .    then use them all, possibly dropping one to
*              .    make the number of shifts even. ====
*
               NS = MIN( NS, KBOT-KS+1 )
               NS = NS - MOD( NS, 2 )
               KS = KBOT - NS + 1
*
*              ==== Small-bulge multi-shift QR sweep:
*              .    split workspace under the subdiagonal into
*              .    - a KDU-by-KDU work array U in the lower
*              .      left-hand-corner,
*              .    - a KDU-by-at-least-KDU-but-more-is-better
*              .      (KDU-by-NHo) horizontal work array WH along
*              .      the bottom edge,
*              .    - and an at-least-KDU-but-more-is-better-by-KDU
*              .      (NVE-by-KDU) vertical work WV arrow along
*              .      the left-hand-edge. ====
*
               KDU = 3*NS - 3
               KU = N - KDU + 1
               KWH = KDU + 1
               NHO = ( N-KDU+1-4 ) - ( KDU+1 ) + 1
               KWV = KDU + 4
               NVE = N - KDU - KWV + 1
*
*              ==== Small-bulge multi-shift QR sweep ====
*
               CALL DLAQR5( WANTT, WANTZ, KACC22, N, KTOP, KBOT, NS,
     $                      WR( KS ), WI( KS ), H, LDH, ILOZ, IHIZ, Z,
     $                      LDZ, WORK, 3, H( KU, 1 ), LDH, NVE,
     $                      H( KWV, 1 ), LDH, NHO, H( KU, KWH ), LDH )
            END IF
*
*           ==== Note progress (or the lack of it). ====
*
            IF( LD.GT.0 ) THEN
               NDFL = 1
            ELSE
               NDFL = NDFL + 1
            END IF
*
*           ==== End of main loop ====
   80    CONTINUE
*
*        ==== Iteration limit exceeded.  Set INFO to show where
*        .    the problem occurred and exit. ====
*
         INFO = KBOT
   90    CONTINUE
      END IF
*
*     ==== Return the optimal value of LWORK. ====
*
      WORK( 1 ) = DBLE( LWKOPT )
*
*     ==== End of DLAQR0 ====
*
      END
      SUBROUTINE DLAQR1( N, H, LDH, SR1, SI1, SR2, SI2, V )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   SI1, SI2, SR1, SR2
      INTEGER            LDH, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), V( * )
*     ..
*
*       Given a 2-by-2 or 3-by-3 matrix H, DLAQR1 sets v to a
*       scalar multiple of the first column of the product
*
*       (*)  K = (H - (sr1 + i*si1)*I)*(H - (sr2 + i*si2)*I)
*
*       scaling to avoid overflows and most underflows. It
*       is assumed that either
*
*               1) sr1 = sr2 and si1 = -si2
*           or
*               2) si1 = si2 = 0.
*
*       This is useful for starting double implicit shift bulges
*       in the QR algorithm.
*
*
*       N      (input) integer
*              Order of the matrix H. N must be either 2 or 3.
*
*       H      (input) DOUBLE PRECISION array of dimension (LDH,N)
*              The 2-by-2 or 3-by-3 matrix H in (*).
*
*       LDH    (input) integer
*              The leading dimension of H as declared in
*              the calling procedure.  LDH.GE.N
*
*       SR1    (input) DOUBLE PRECISION
*       SI1    The shifts in (*).
*       SR2
*       SI2
*
*       V      (output) DOUBLE PRECISION array of dimension N
*              A scalar multiple of the first column of the
*              matrix K in (*).
*
*     ================================================================
*     Based on contributions by
*        Karen Braman and Ralph Byers, Department of Mathematics,
*        University of Kansas, USA
*
*     ================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   H21S, H31S, S
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
      IF( N.EQ.2 ) THEN
         S = ABS( H( 1, 1 )-SR2 ) + ABS( SI2 ) + ABS( H( 2, 1 ) )
         IF( S.EQ.ZERO ) THEN
            V( 1 ) = ZERO
            V( 2 ) = ZERO
         ELSE
            H21S = H( 2, 1 ) / S
            V( 1 ) = H21S*H( 1, 2 ) + ( H( 1, 1 )-SR1 )*
     $               ( ( H( 1, 1 )-SR2 ) / S ) - SI1*( SI2 / S )
            V( 2 ) = H21S*( H( 1, 1 )+H( 2, 2 )-SR1-SR2 )
         END IF
      ELSE
         S = ABS( H( 1, 1 )-SR2 ) + ABS( SI2 ) + ABS( H( 2, 1 ) ) +
     $       ABS( H( 3, 1 ) )
         IF( S.EQ.ZERO ) THEN
            V( 1 ) = ZERO
            V( 2 ) = ZERO
            V( 3 ) = ZERO
         ELSE
            H21S = H( 2, 1 ) / S
            H31S = H( 3, 1 ) / S
            V( 1 ) = ( H( 1, 1 )-SR1 )*( ( H( 1, 1 )-SR2 ) / S ) -
     $               SI1*( SI2 / S ) + H( 1, 2 )*H21S + H( 1, 3 )*H31S
            V( 2 ) = H21S*( H( 1, 1 )+H( 2, 2 )-SR1-SR2 ) +
     $               H( 2, 3 )*H31S
            V( 3 ) = H31S*( H( 1, 1 )+H( 3, 3 )-SR1-SR2 ) +
     $               H21S*H( 3, 2 )
         END IF
      END IF
      END
      SUBROUTINE DLAQR2( WANTT, WANTZ, N, KTOP, KBOT, NW, H, LDH, ILOZ,
     $                   IHIZ, Z, LDZ, NS, ND, SR, SI, V, LDV, NH, T,
     $                   LDT, NV, WV, LDWV, WORK, LWORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IHIZ, ILOZ, KBOT, KTOP, LDH, LDT, LDV, LDWV,
     $                   LDZ, LWORK, N, ND, NH, NS, NV, NW
      LOGICAL            WANTT, WANTZ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), SI( * ), SR( * ), T( LDT, * ),
     $                   V( LDV, * ), WORK( * ), WV( LDWV, * ),
     $                   Z( LDZ, * )
*     ..
*
*     This subroutine is identical to DLAQR3 except that it avoids
*     recursion by calling DLAHQR instead of DLAQR4.
*
*
*     ******************************************************************
*     Aggressive early deflation:
*
*     This subroutine accepts as input an upper Hessenberg matrix
*     H and performs an orthogonal similarity transformation
*     designed to detect and deflate fully converged eigenvalues from
*     a trailing principal submatrix.  On output H has been over-
*     written by a new Hessenberg matrix that is a perturbation of
*     an orthogonal similarity transformation of H.  It is to be
*     hoped that the final version of H has many zero subdiagonal
*     entries.
*
*     ******************************************************************
*     WANTT   (input) LOGICAL
*          If .TRUE., then the Hessenberg matrix H is fully updated
*          so that the quasi-triangular Schur factor may be
*          computed (in cooperation with the calling subroutine).
*          If .FALSE., then only enough of H is updated to preserve
*          the eigenvalues.
*
*     WANTZ   (input) LOGICAL
*          If .TRUE., then the orthogonal matrix Z is updated so
*          so that the orthogonal Schur factor may be computed
*          (in cooperation with the calling subroutine).
*          If .FALSE., then Z is not referenced.
*
*     N       (input) INTEGER
*          The order of the matrix H and (if WANTZ is .TRUE.) the
*          order of the orthogonal matrix Z.
*
*     KTOP    (input) INTEGER
*          It is assumed that either KTOP = 1 or H(KTOP,KTOP-1)=0.
*          KBOT and KTOP together determine an isolated block
*          along the diagonal of the Hessenberg matrix.
*
*     KBOT    (input) INTEGER
*          It is assumed without a check that either
*          KBOT = N or H(KBOT+1,KBOT)=0.  KBOT and KTOP together
*          determine an isolated block along the diagonal of the
*          Hessenberg matrix.
*
*     NW      (input) INTEGER
*          Deflation window size.  1 .LE. NW .LE. (KBOT-KTOP+1).
*
*     H       (input/output) DOUBLE PRECISION array, dimension (LDH,N)
*          On input the initial N-by-N section of H stores the
*          Hessenberg matrix undergoing aggressive early deflation.
*          On output H has been transformed by an orthogonal
*          similarity transformation, perturbed, and the returned
*          to Hessenberg form that (it is to be hoped) has some
*          zero subdiagonal entries.
*
*     LDH     (input) integer
*          Leading dimension of H just as declared in the calling
*          subroutine.  N .LE. LDH
*
*     ILOZ    (input) INTEGER
*     IHIZ    (input) INTEGER
*          Specify the rows of Z to which transformations must be
*          applied if WANTZ is .TRUE.. 1 .LE. ILOZ .LE. IHIZ .LE. N.
*
*     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
*          IF WANTZ is .TRUE., then on output, the orthogonal
*          similarity transformation mentioned above has been
*          accumulated into Z(ILOZ:IHIZ,ILO:IHI) from the right.
*          If WANTZ is .FALSE., then Z is unreferenced.
*
*     LDZ     (input) integer
*          The leading dimension of Z just as declared in the
*          calling subroutine.  1 .LE. LDZ.
*
*     NS      (output) integer
*          The number of unconverged (ie approximate) eigenvalues
*          returned in SR and SI that may be used as shifts by the
*          calling subroutine.
*
*     ND      (output) integer
*          The number of converged eigenvalues uncovered by this
*          subroutine.
*
*     SR      (output) DOUBLE PRECISION array, dimension KBOT
*     SI      (output) DOUBLE PRECISION array, dimension KBOT
*          On output, the real and imaginary parts of approximate
*          eigenvalues that may be used for shifts are stored in
*          SR(KBOT-ND-NS+1) through SR(KBOT-ND) and
*          SI(KBOT-ND-NS+1) through SI(KBOT-ND), respectively.
*          The real and imaginary parts of converged eigenvalues
*          are stored in SR(KBOT-ND+1) through SR(KBOT) and
*          SI(KBOT-ND+1) through SI(KBOT), respectively.
*
*     V       (workspace) DOUBLE PRECISION array, dimension (LDV,NW)
*          An NW-by-NW work array.
*
*     LDV     (input) integer scalar
*          The leading dimension of V just as declared in the
*          calling subroutine.  NW .LE. LDV
*
*     NH      (input) integer scalar
*          The number of columns of T.  NH.GE.NW.
*
*     T       (workspace) DOUBLE PRECISION array, dimension (LDT,NW)
*
*     LDT     (input) integer
*          The leading dimension of T just as declared in the
*          calling subroutine.  NW .LE. LDT
*
*     NV      (input) integer
*          The number of rows of work array WV available for
*          workspace.  NV.GE.NW.
*
*     WV      (workspace) DOUBLE PRECISION array, dimension (LDWV,NW)
*
*     LDWV    (input) integer
*          The leading dimension of W just as declared in the
*          calling subroutine.  NW .LE. LDV
*
*     WORK    (workspace) DOUBLE PRECISION array, dimension LWORK.
*          On exit, WORK(1) is set to an estimate of the optimal value
*          of LWORK for the given values of N, NW, KTOP and KBOT.
*
*     LWORK   (input) integer
*          The dimension of the work array WORK.  LWORK = 2*NW
*          suffices, but greater efficiency may result from larger
*          values of LWORK.
*
*          If LWORK = -1, then a workspace query is assumed; DLAQR2
*          only estimates the optimal workspace size for the given
*          values of N, NW, KTOP and KBOT.  The estimate is returned
*          in WORK(1).  No error message related to LWORK is issued
*          by XERBLA.  Neither H nor Z are accessed.
*
*     ================================================================
*     Based on contributions by
*        Karen Braman and Ralph Byers, Department of Mathematics,
*        University of Kansas, USA
*
*     ================================================================
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0d0, ONE = 1.0d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AA, BB, BETA, CC, CS, DD, EVI, EVK, FOO, S,
     $                   SAFMAX, SAFMIN, SMLNUM, SN, TAU, ULP
      INTEGER            I, IFST, ILST, INFO, INFQR, J, JW, K, KCOL,
     $                   KEND, KLN, KROW, KWTOP, LTOP, LWK1, LWK2,
     $                   LWKOPT
      LOGICAL            BULGE, SORTED
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEHRD, DGEMM, DLABAD, DLACPY, DLAHQR,
     $                   DLANV2, DLARF, DLARFG, DLASET, DORGHR, DTREXC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     ==== Estimate optimal workspace. ====
*
      JW = MIN( NW, KBOT-KTOP+1 )
      IF( JW.LE.2 ) THEN
         LWKOPT = 1
      ELSE
*
*        ==== Workspace query call to DGEHRD ====
*
         CALL DGEHRD( JW, 1, JW-1, T, LDT, WORK, WORK, -1, INFO )
         LWK1 = INT( WORK( 1 ) )
*
*        ==== Workspace query call to DORGHR ====
*
         CALL DORGHR( JW, 1, JW-1, T, LDT, WORK, WORK, -1, INFO )
         LWK2 = INT( WORK( 1 ) )
*
*        ==== Optimal workspace ====
*
         LWKOPT = JW + MAX( LWK1, LWK2 )
      END IF
*
*     ==== Quick return in case of workspace query. ====
*
      IF( LWORK.EQ.-1 ) THEN
         WORK( 1 ) = DBLE( LWKOPT )
         RETURN
      END IF
*
*     ==== Nothing to do ...
*     ... for an empty active block ... ====
      NS = 0
      ND = 0
      IF( KTOP.GT.KBOT )
     $   RETURN
*     ... nor for an empty deflation window. ====
      IF( NW.LT.1 )
     $   RETURN
*
*     ==== Machine constants ====
*
      SAFMIN = DLAMCH( 'SAFE MINIMUM' )
      SAFMAX = ONE / SAFMIN
      CALL DLABAD( SAFMIN, SAFMAX )
      ULP = DLAMCH( 'PRECISION' )
      SMLNUM = SAFMIN*( DBLE( N ) / ULP )
*
*     ==== Setup deflation window ====
*
      JW = MIN( NW, KBOT-KTOP+1 )
      KWTOP = KBOT - JW + 1
      IF( KWTOP.EQ.KTOP ) THEN
         S = ZERO
      ELSE
         S = H( KWTOP, KWTOP-1 )
      END IF
*
      IF( KBOT.EQ.KWTOP ) THEN
*
*        ==== 1-by-1 deflation window: not much to do ====
*
         SR( KWTOP ) = H( KWTOP, KWTOP )
         SI( KWTOP ) = ZERO
         NS = 1
         ND = 0
         IF( ABS( S ).LE.MAX( SMLNUM, ULP*ABS( H( KWTOP, KWTOP ) ) ) )
     $        THEN
            NS = 0
            ND = 1
            IF( KWTOP.GT.KTOP )
     $         H( KWTOP, KWTOP-1 ) = ZERO
         END IF
         RETURN
      END IF
*
*     ==== Convert to spike-triangular form.  (In case of a
*     .    rare QR failure, this routine continues to do
*     .    aggressive early deflation using that part of
*     .    the deflation window that converged using INFQR
*     .    here and there to keep track.) ====
*
      CALL DLACPY( 'U', JW, JW, H( KWTOP, KWTOP ), LDH, T, LDT )
      CALL DCOPY( JW-1, H( KWTOP+1, KWTOP ), LDH+1, T( 2, 1 ), LDT+1 )
*
      CALL DLASET( 'A', JW, JW, ZERO, ONE, V, LDV )
      CALL DLAHQR( .true., .true., JW, 1, JW, T, LDT, SR( KWTOP ),
     $             SI( KWTOP ), 1, JW, V, LDV, INFQR )
*
*     ==== DTREXC needs a clean margin near the diagonal ====
*
      DO 10 J = 1, JW - 3
         T( J+2, J ) = ZERO
         T( J+3, J ) = ZERO
   10 CONTINUE
      IF( JW.GT.2 )
     $   T( JW, JW-2 ) = ZERO
*
*     ==== Deflation detection loop ====
*
      NS = JW
      ILST = INFQR + 1
   20 CONTINUE
      IF( ILST.LE.NS ) THEN
         IF( NS.EQ.1 ) THEN
            BULGE = .FALSE.
         ELSE
            BULGE = T( NS, NS-1 ).NE.ZERO
         END IF
*
*        ==== Small spike tip test for deflation ====
*
         IF( .NOT.BULGE ) THEN
*
*           ==== Real eigenvalue ====
*
            FOO = ABS( T( NS, NS ) )
            IF( FOO.EQ.ZERO )
     $         FOO = ABS( S )
            IF( ABS( S*V( 1, NS ) ).LE.MAX( SMLNUM, ULP*FOO ) ) THEN
*
*              ==== Deflatable ====
*
               NS = NS - 1
            ELSE
*
*              ==== Undeflatable.   Move it up out of the way.
*              .    (DTREXC can not fail in this case.) ====
*
               IFST = NS
               CALL DTREXC( 'V', JW, T, LDT, V, LDV, IFST, ILST, WORK,
     $                      INFO )
               ILST = ILST + 1
            END IF
         ELSE
*
*           ==== Complex conjugate pair ====
*
            FOO = ABS( T( NS, NS ) ) + SQRT( ABS( T( NS, NS-1 ) ) )*
     $            SQRT( ABS( T( NS-1, NS ) ) )
            IF( FOO.EQ.ZERO )
     $         FOO = ABS( S )
            IF( MAX( ABS( S*V( 1, NS ) ), ABS( S*V( 1, NS-1 ) ) ).LE.
     $          MAX( SMLNUM, ULP*FOO ) ) THEN
*
*              ==== Deflatable ====
*
               NS = NS - 2
            ELSE
*
*              ==== Undflatable. Move them up out of the way.
*              .    Fortunately, DTREXC does the right thing with
*              .    ILST in case of a rare exchange failure. ====
*
               IFST = NS
               CALL DTREXC( 'V', JW, T, LDT, V, LDV, IFST, ILST, WORK,
     $                      INFO )
               ILST = ILST + 2
            END IF
         END IF
*
*        ==== End deflation detection loop ====
*
         GO TO 20
      END IF
*
*        ==== Return to Hessenberg form ====
*
      IF( NS.EQ.0 )
     $   S = ZERO
*
      IF( NS.LT.JW ) THEN
*
*        ==== sorting diagonal blocks of T improves accuracy for
*        .    graded matrices.  Bubble sort deals well with
*        .    exchange failures. ====
*
         SORTED = .false.
         I = NS + 1
   30    CONTINUE
         IF( SORTED )
     $      GO TO 50
         SORTED = .true.
*
         KEND = I - 1
         I = INFQR + 1
         IF( I.EQ.NS ) THEN
            K = I + 1
         ELSE IF( T( I+1, I ).EQ.ZERO ) THEN
            K = I + 1
         ELSE
            K = I + 2
         END IF
   40    CONTINUE
         IF( K.LE.KEND ) THEN
            IF( K.EQ.I+1 ) THEN
               EVI = ABS( T( I, I ) )
            ELSE
               EVI = ABS( T( I, I ) ) + SQRT( ABS( T( I+1, I ) ) )*
     $               SQRT( ABS( T( I, I+1 ) ) )
            END IF
*
            IF( K.EQ.KEND ) THEN
               EVK = ABS( T( K, K ) )
            ELSE IF( T( K+1, K ).EQ.ZERO ) THEN
               EVK = ABS( T( K, K ) )
            ELSE
               EVK = ABS( T( K, K ) ) + SQRT( ABS( T( K+1, K ) ) )*
     $               SQRT( ABS( T( K, K+1 ) ) )
            END IF
*
            IF( EVI.GE.EVK ) THEN
               I = K
            ELSE
               SORTED = .false.
               IFST = I
               ILST = K
               CALL DTREXC( 'V', JW, T, LDT, V, LDV, IFST, ILST, WORK,
     $                      INFO )
               IF( INFO.EQ.0 ) THEN
                  I = ILST
               ELSE
                  I = K
               END IF
            END IF
            IF( I.EQ.KEND ) THEN
               K = I + 1
            ELSE IF( T( I+1, I ).EQ.ZERO ) THEN
               K = I + 1
            ELSE
               K = I + 2
            END IF
            GO TO 40
         END IF
         GO TO 30
   50    CONTINUE
      END IF
*
*     ==== Restore shift/eigenvalue array from T ====
*
      I = JW
   60 CONTINUE
      IF( I.GE.INFQR+1 ) THEN
         IF( I.EQ.INFQR+1 ) THEN
            SR( KWTOP+I-1 ) = T( I, I )
            SI( KWTOP+I-1 ) = ZERO
            I = I - 1
         ELSE IF( T( I, I-1 ).EQ.ZERO ) THEN
            SR( KWTOP+I-1 ) = T( I, I )
            SI( KWTOP+I-1 ) = ZERO
            I = I - 1
         ELSE
            AA = T( I-1, I-1 )
            CC = T( I, I-1 )
            BB = T( I-1, I )
            DD = T( I, I )
            CALL DLANV2( AA, BB, CC, DD, SR( KWTOP+I-2 ),
     $                   SI( KWTOP+I-2 ), SR( KWTOP+I-1 ),
     $                   SI( KWTOP+I-1 ), CS, SN )
            I = I - 2
         END IF
         GO TO 60
      END IF
*
      IF( NS.LT.JW .OR. S.EQ.ZERO ) THEN
         IF( NS.GT.1 .AND. S.NE.ZERO ) THEN
*
*           ==== Reflect spike back into lower triangle ====
*
            CALL DCOPY( NS, V, LDV, WORK, 1 )
            BETA = WORK( 1 )
            CALL DLARFG( NS, BETA, WORK( 2 ), 1, TAU )
            WORK( 1 ) = ONE
*
            CALL DLASET( 'L', JW-2, JW-2, ZERO, ZERO, T( 3, 1 ), LDT )
*
            CALL DLARF( 'L', NS, JW, WORK, 1, TAU, T, LDT,
     $                  WORK( JW+1 ) )
            CALL DLARF( 'R', NS, NS, WORK, 1, TAU, T, LDT,
     $                  WORK( JW+1 ) )
            CALL DLARF( 'R', JW, NS, WORK, 1, TAU, V, LDV,
     $                  WORK( JW+1 ) )
*
            CALL DGEHRD( JW, 1, NS, T, LDT, WORK, WORK( JW+1 ),
     $                   LWORK-JW, INFO )
         END IF
*
*        ==== Copy updated reduced window into place ====
*
         IF( KWTOP.GT.1 )
     $      H( KWTOP, KWTOP-1 ) = S*V( 1, 1 )
         CALL DLACPY( 'U', JW, JW, T, LDT, H( KWTOP, KWTOP ), LDH )
         CALL DCOPY( JW-1, T( 2, 1 ), LDT+1, H( KWTOP+1, KWTOP ),
     $               LDH+1 )
*
*        ==== Accumulate orthogonal matrix in order update
*        .    H and Z, if requested.  (A modified version
*        .    of  DORGHR that accumulates block Householder
*        .    transformations into V directly might be
*        .    marginally more efficient than the following.) ====
*
         IF( NS.GT.1 .AND. S.NE.ZERO ) THEN
            CALL DORGHR( JW, 1, NS, T, LDT, WORK, WORK( JW+1 ),
     $                   LWORK-JW, INFO )
            CALL DGEMM( 'N', 'N', JW, NS, NS, ONE, V, LDV, T, LDT, ZERO,
     $                  WV, LDWV )
            CALL DLACPY( 'A', JW, NS, WV, LDWV, V, LDV )
         END IF
*
*        ==== Update vertical slab in H ====
*
         IF( WANTT ) THEN
            LTOP = 1
         ELSE
            LTOP = KTOP
         END IF
         DO 70 KROW = LTOP, KWTOP - 1, NV
            KLN = MIN( NV, KWTOP-KROW )
            CALL DGEMM( 'N', 'N', KLN, JW, JW, ONE, H( KROW, KWTOP ),
     $                  LDH, V, LDV, ZERO, WV, LDWV )
            CALL DLACPY( 'A', KLN, JW, WV, LDWV, H( KROW, KWTOP ), LDH )
   70    CONTINUE
*
*        ==== Update horizontal slab in H ====
*
         IF( WANTT ) THEN
            DO 80 KCOL = KBOT + 1, N, NH
               KLN = MIN( NH, N-KCOL+1 )
               CALL DGEMM( 'C', 'N', JW, KLN, JW, ONE, V, LDV,
     $                     H( KWTOP, KCOL ), LDH, ZERO, T, LDT )
               CALL DLACPY( 'A', JW, KLN, T, LDT, H( KWTOP, KCOL ),
     $                      LDH )
   80       CONTINUE
         END IF
*
*        ==== Update vertical slab in Z ====
*
         IF( WANTZ ) THEN
            DO 90 KROW = ILOZ, IHIZ, NV
               KLN = MIN( NV, IHIZ-KROW+1 )
               CALL DGEMM( 'N', 'N', KLN, JW, JW, ONE, Z( KROW, KWTOP ),
     $                     LDZ, V, LDV, ZERO, WV, LDWV )
               CALL DLACPY( 'A', KLN, JW, WV, LDWV, Z( KROW, KWTOP ),
     $                      LDZ )
   90       CONTINUE
         END IF
      END IF
*
*     ==== Return the number of deflations ... ====
*
      ND = JW - NS
*
*     ==== ... and the number of shifts. (Subtracting
*     .    INFQR from the spike length takes care
*     .    of the case of a rare QR failure while
*     .    calculating eigenvalues of the deflation
*     .    window.)  ====
*
      NS = NS - INFQR
*
*      ==== Return optimal workspace. ====
*
      WORK( 1 ) = DBLE( LWKOPT )
*
*     ==== End of DLAQR2 ====
*
      END
      SUBROUTINE DLAQR3( WANTT, WANTZ, N, KTOP, KBOT, NW, H, LDH, ILOZ,
     $                   IHIZ, Z, LDZ, NS, ND, SR, SI, V, LDV, NH, T,
     $                   LDT, NV, WV, LDWV, WORK, LWORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IHIZ, ILOZ, KBOT, KTOP, LDH, LDT, LDV, LDWV,
     $                   LDZ, LWORK, N, ND, NH, NS, NV, NW
      LOGICAL            WANTT, WANTZ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), SI( * ), SR( * ), T( LDT, * ),
     $                   V( LDV, * ), WORK( * ), WV( LDWV, * ),
     $                   Z( LDZ, * )
*     ..
*
*     ******************************************************************
*     Aggressive early deflation:
*
*     This subroutine accepts as input an upper Hessenberg matrix
*     H and performs an orthogonal similarity transformation
*     designed to detect and deflate fully converged eigenvalues from
*     a trailing principal submatrix.  On output H has been over-
*     written by a new Hessenberg matrix that is a perturbation of
*     an orthogonal similarity transformation of H.  It is to be
*     hoped that the final version of H has many zero subdiagonal
*     entries.
*
*     ******************************************************************
*     WANTT   (input) LOGICAL
*          If .TRUE., then the Hessenberg matrix H is fully updated
*          so that the quasi-triangular Schur factor may be
*          computed (in cooperation with the calling subroutine).
*          If .FALSE., then only enough of H is updated to preserve
*          the eigenvalues.
*
*     WANTZ   (input) LOGICAL
*          If .TRUE., then the orthogonal matrix Z is updated so
*          so that the orthogonal Schur factor may be computed
*          (in cooperation with the calling subroutine).
*          If .FALSE., then Z is not referenced.
*
*     N       (input) INTEGER
*          The order of the matrix H and (if WANTZ is .TRUE.) the
*          order of the orthogonal matrix Z.
*
*     KTOP    (input) INTEGER
*          It is assumed that either KTOP = 1 or H(KTOP,KTOP-1)=0.
*          KBOT and KTOP together determine an isolated block
*          along the diagonal of the Hessenberg matrix.
*
*     KBOT    (input) INTEGER
*          It is assumed without a check that either
*          KBOT = N or H(KBOT+1,KBOT)=0.  KBOT and KTOP together
*          determine an isolated block along the diagonal of the
*          Hessenberg matrix.
*
*     NW      (input) INTEGER
*          Deflation window size.  1 .LE. NW .LE. (KBOT-KTOP+1).
*
*     H       (input/output) DOUBLE PRECISION array, dimension (LDH,N)
*          On input the initial N-by-N section of H stores the
*          Hessenberg matrix undergoing aggressive early deflation.
*          On output H has been transformed by an orthogonal
*          similarity transformation, perturbed, and the returned
*          to Hessenberg form that (it is to be hoped) has some
*          zero subdiagonal entries.
*
*     LDH     (input) integer
*          Leading dimension of H just as declared in the calling
*          subroutine.  N .LE. LDH
*
*     ILOZ    (input) INTEGER
*     IHIZ    (input) INTEGER
*          Specify the rows of Z to which transformations must be
*          applied if WANTZ is .TRUE.. 1 .LE. ILOZ .LE. IHIZ .LE. N.
*
*     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
*          IF WANTZ is .TRUE., then on output, the orthogonal
*          similarity transformation mentioned above has been
*          accumulated into Z(ILOZ:IHIZ,ILO:IHI) from the right.
*          If WANTZ is .FALSE., then Z is unreferenced.
*
*     LDZ     (input) integer
*          The leading dimension of Z just as declared in the
*          calling subroutine.  1 .LE. LDZ.
*
*     NS      (output) integer
*          The number of unconverged (ie approximate) eigenvalues
*          returned in SR and SI that may be used as shifts by the
*          calling subroutine.
*
*     ND      (output) integer
*          The number of converged eigenvalues uncovered by this
*          subroutine.
*
*     SR      (output) DOUBLE PRECISION array, dimension KBOT
*     SI      (output) DOUBLE PRECISION array, dimension KBOT
*          On output, the real and imaginary parts of approximate
*          eigenvalues that may be used for shifts are stored in
*          SR(KBOT-ND-NS+1) through SR(KBOT-ND) and
*          SI(KBOT-ND-NS+1) through SI(KBOT-ND), respectively.
*          The real and imaginary parts of converged eigenvalues
*          are stored in SR(KBOT-ND+1) through SR(KBOT) and
*          SI(KBOT-ND+1) through SI(KBOT), respectively.
*
*     V       (workspace) DOUBLE PRECISION array, dimension (LDV,NW)
*          An NW-by-NW work array.
*
*     LDV     (input) integer scalar
*          The leading dimension of V just as declared in the
*          calling subroutine.  NW .LE. LDV
*
*     NH      (input) integer scalar
*          The number of columns of T.  NH.GE.NW.
*
*     T       (workspace) DOUBLE PRECISION array, dimension (LDT,NW)
*
*     LDT     (input) integer
*          The leading dimension of T just as declared in the
*          calling subroutine.  NW .LE. LDT
*
*     NV      (input) integer
*          The number of rows of work array WV available for
*          workspace.  NV.GE.NW.
*
*     WV      (workspace) DOUBLE PRECISION array, dimension (LDWV,NW)
*
*     LDWV    (input) integer
*          The leading dimension of W just as declared in the
*          calling subroutine.  NW .LE. LDV
*
*     WORK    (workspace) DOUBLE PRECISION array, dimension LWORK.
*          On exit, WORK(1) is set to an estimate of the optimal value
*          of LWORK for the given values of N, NW, KTOP and KBOT.
*
*     LWORK   (input) integer
*          The dimension of the work array WORK.  LWORK = 2*NW
*          suffices, but greater efficiency may result from larger
*          values of LWORK.
*
*          If LWORK = -1, then a workspace query is assumed; DLAQR3
*          only estimates the optimal workspace size for the given
*          values of N, NW, KTOP and KBOT.  The estimate is returned
*          in WORK(1).  No error message related to LWORK is issued
*          by XERBLA.  Neither H nor Z are accessed.
*
*     ================================================================
*     Based on contributions by
*        Karen Braman and Ralph Byers, Department of Mathematics,
*        University of Kansas, USA
*
*     ==================================================================
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0d0, ONE = 1.0d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AA, BB, BETA, CC, CS, DD, EVI, EVK, FOO, S,
     $                   SAFMAX, SAFMIN, SMLNUM, SN, TAU, ULP
      INTEGER            I, IFST, ILST, INFO, INFQR, J, JW, K, KCOL,
     $                   KEND, KLN, KROW, KWTOP, LTOP, LWK1, LWK2, LWK3,
     $                   LWKOPT, NMIN
      LOGICAL            BULGE, SORTED
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      INTEGER            ILAENV
      EXTERNAL           DLAMCH, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEHRD, DGEMM, DLABAD, DLACPY, DLAHQR,
     $                   DLANV2, DLAQR4, DLARF, DLARFG, DLASET, DORGHR,
     $                   DTREXC
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     ==== Estimate optimal workspace. ====
*
      JW = MIN( NW, KBOT-KTOP+1 )
      IF( JW.LE.2 ) THEN
         LWKOPT = 1
      ELSE
*
*        ==== Workspace query call to DGEHRD ====
*
         CALL DGEHRD( JW, 1, JW-1, T, LDT, WORK, WORK, -1, INFO )
         LWK1 = INT( WORK( 1 ) )
*
*        ==== Workspace query call to DORGHR ====
*
         CALL DORGHR( JW, 1, JW-1, T, LDT, WORK, WORK, -1, INFO )
         LWK2 = INT( WORK( 1 ) )
*
*        ==== Workspace query call to DLAQR4 ====
*
         CALL DLAQR4( .true., .true., JW, 1, JW, T, LDT, SR, SI, 1, JW,
     $                V, LDV, WORK, -1, INFQR )
         LWK3 = INT( WORK( 1 ) )
*
*        ==== Optimal workspace ====
*
         LWKOPT = MAX( JW+MAX( LWK1, LWK2 ), LWK3 )
      END IF
*
*     ==== Quick return in case of workspace query. ====
*
      IF( LWORK.EQ.-1 ) THEN
         WORK( 1 ) = DBLE( LWKOPT )
         RETURN
      END IF
*
*     ==== Nothing to do ...
*     ... for an empty active block ... ====
      NS = 0
      ND = 0
      IF( KTOP.GT.KBOT )
     $   RETURN
*     ... nor for an empty deflation window. ====
      IF( NW.LT.1 )
     $   RETURN
*
*     ==== Machine constants ====
*
      SAFMIN = DLAMCH( 'SAFE MINIMUM' )
      SAFMAX = ONE / SAFMIN
      CALL DLABAD( SAFMIN, SAFMAX )
      ULP = DLAMCH( 'PRECISION' )
      SMLNUM = SAFMIN*( DBLE( N ) / ULP )
*
*     ==== Setup deflation window ====
*
      JW = MIN( NW, KBOT-KTOP+1 )
      KWTOP = KBOT - JW + 1
      IF( KWTOP.EQ.KTOP ) THEN
         S = ZERO
      ELSE
         S = H( KWTOP, KWTOP-1 )
      END IF
*
      IF( KBOT.EQ.KWTOP ) THEN
*
*        ==== 1-by-1 deflation window: not much to do ====
*
         SR( KWTOP ) = H( KWTOP, KWTOP )
         SI( KWTOP ) = ZERO
         NS = 1
         ND = 0
         IF( ABS( S ).LE.MAX( SMLNUM, ULP*ABS( H( KWTOP, KWTOP ) ) ) )
     $        THEN
            NS = 0
            ND = 1
            IF( KWTOP.GT.KTOP )
     $         H( KWTOP, KWTOP-1 ) = ZERO
         END IF
         RETURN
      END IF
*
*     ==== Convert to spike-triangular form.  (In case of a
*     .    rare QR failure, this routine continues to do
*     .    aggressive early deflation using that part of
*     .    the deflation window that converged using INFQR
*     .    here and there to keep track.) ====
*
      CALL DLACPY( 'U', JW, JW, H( KWTOP, KWTOP ), LDH, T, LDT )
      CALL DCOPY( JW-1, H( KWTOP+1, KWTOP ), LDH+1, T( 2, 1 ), LDT+1 )
*
      CALL DLASET( 'A', JW, JW, ZERO, ONE, V, LDV )
      NMIN = ILAENV( 12, 'DLAQR3', 'SV', JW, 1, JW, LWORK )
      IF( JW.GT.NMIN ) THEN
         CALL DLAQR4( .true., .true., JW, 1, JW, T, LDT, SR( KWTOP ),
     $                SI( KWTOP ), 1, JW, V, LDV, WORK, LWORK, INFQR )
      ELSE
         CALL DLAHQR( .true., .true., JW, 1, JW, T, LDT, SR( KWTOP ),
     $                SI( KWTOP ), 1, JW, V, LDV, INFQR )
      END IF
*
*     ==== DTREXC needs a clean margin near the diagonal ====
*
      DO 10 J = 1, JW - 3
         T( J+2, J ) = ZERO
         T( J+3, J ) = ZERO
   10 CONTINUE
      IF( JW.GT.2 )
     $   T( JW, JW-2 ) = ZERO
*
*     ==== Deflation detection loop ====
*
      NS = JW
      ILST = INFQR + 1
   20 CONTINUE
      IF( ILST.LE.NS ) THEN
         IF( NS.EQ.1 ) THEN
            BULGE = .FALSE.
         ELSE
            BULGE = T( NS, NS-1 ).NE.ZERO
         END IF
*
*        ==== Small spike tip test for deflation ====
*
         IF( .NOT.BULGE ) THEN
*
*           ==== Real eigenvalue ====
*
            FOO = ABS( T( NS, NS ) )
            IF( FOO.EQ.ZERO )
     $         FOO = ABS( S )
            IF( ABS( S*V( 1, NS ) ).LE.MAX( SMLNUM, ULP*FOO ) ) THEN
*
*              ==== Deflatable ====
*
               NS = NS - 1
            ELSE
*
*              ==== Undeflatable.   Move it up out of the way.
*              .    (DTREXC can not fail in this case.) ====
*
               IFST = NS
               CALL DTREXC( 'V', JW, T, LDT, V, LDV, IFST, ILST, WORK,
     $                      INFO )
               ILST = ILST + 1
            END IF
         ELSE
*
*           ==== Complex conjugate pair ====
*
            FOO = ABS( T( NS, NS ) ) + SQRT( ABS( T( NS, NS-1 ) ) )*
     $            SQRT( ABS( T( NS-1, NS ) ) )
            IF( FOO.EQ.ZERO )
     $         FOO = ABS( S )
            IF( MAX( ABS( S*V( 1, NS ) ), ABS( S*V( 1, NS-1 ) ) ).LE.
     $          MAX( SMLNUM, ULP*FOO ) ) THEN
*
*              ==== Deflatable ====
*
               NS = NS - 2
            ELSE
*
*              ==== Undflatable. Move them up out of the way.
*              .    Fortunately, DTREXC does the right thing with
*              .    ILST in case of a rare exchange failure. ====
*
               IFST = NS
               CALL DTREXC( 'V', JW, T, LDT, V, LDV, IFST, ILST, WORK,
     $                      INFO )
               ILST = ILST + 2
            END IF
         END IF
*
*        ==== End deflation detection loop ====
*
         GO TO 20
      END IF
*
*        ==== Return to Hessenberg form ====
*
      IF( NS.EQ.0 )
     $   S = ZERO
*
      IF( NS.LT.JW ) THEN
*
*        ==== sorting diagonal blocks of T improves accuracy for
*        .    graded matrices.  Bubble sort deals well with
*        .    exchange failures. ====
*
         SORTED = .false.
         I = NS + 1
   30    CONTINUE
         IF( SORTED )
     $      GO TO 50
         SORTED = .true.
*
         KEND = I - 1
         I = INFQR + 1
         IF( I.EQ.NS ) THEN
            K = I + 1
         ELSE IF( T( I+1, I ).EQ.ZERO ) THEN
            K = I + 1
         ELSE
            K = I + 2
         END IF
   40    CONTINUE
         IF( K.LE.KEND ) THEN
            IF( K.EQ.I+1 ) THEN
               EVI = ABS( T( I, I ) )
            ELSE
               EVI = ABS( T( I, I ) ) + SQRT( ABS( T( I+1, I ) ) )*
     $               SQRT( ABS( T( I, I+1 ) ) )
            END IF
*
            IF( K.EQ.KEND ) THEN
               EVK = ABS( T( K, K ) )
            ELSE IF( T( K+1, K ).EQ.ZERO ) THEN
               EVK = ABS( T( K, K ) )
            ELSE
               EVK = ABS( T( K, K ) ) + SQRT( ABS( T( K+1, K ) ) )*
     $               SQRT( ABS( T( K, K+1 ) ) )
            END IF
*
            IF( EVI.GE.EVK ) THEN
               I = K
            ELSE
               SORTED = .false.
               IFST = I
               ILST = K
               CALL DTREXC( 'V', JW, T, LDT, V, LDV, IFST, ILST, WORK,
     $                      INFO )
               IF( INFO.EQ.0 ) THEN
                  I = ILST
               ELSE
                  I = K
               END IF
            END IF
            IF( I.EQ.KEND ) THEN
               K = I + 1
            ELSE IF( T( I+1, I ).EQ.ZERO ) THEN
               K = I + 1
            ELSE
               K = I + 2
            END IF
            GO TO 40
         END IF
         GO TO 30
   50    CONTINUE
      END IF
*
*     ==== Restore shift/eigenvalue array from T ====
*
      I = JW
   60 CONTINUE
      IF( I.GE.INFQR+1 ) THEN
         IF( I.EQ.INFQR+1 ) THEN
            SR( KWTOP+I-1 ) = T( I, I )
            SI( KWTOP+I-1 ) = ZERO
            I = I - 1
         ELSE IF( T( I, I-1 ).EQ.ZERO ) THEN
            SR( KWTOP+I-1 ) = T( I, I )
            SI( KWTOP+I-1 ) = ZERO
            I = I - 1
         ELSE
            AA = T( I-1, I-1 )
            CC = T( I, I-1 )
            BB = T( I-1, I )
            DD = T( I, I )
            CALL DLANV2( AA, BB, CC, DD, SR( KWTOP+I-2 ),
     $                   SI( KWTOP+I-2 ), SR( KWTOP+I-1 ),
     $                   SI( KWTOP+I-1 ), CS, SN )
            I = I - 2
         END IF
         GO TO 60
      END IF
*
      IF( NS.LT.JW .OR. S.EQ.ZERO ) THEN
         IF( NS.GT.1 .AND. S.NE.ZERO ) THEN
*
*           ==== Reflect spike back into lower triangle ====
*
            CALL DCOPY( NS, V, LDV, WORK, 1 )
            BETA = WORK( 1 )
            CALL DLARFG( NS, BETA, WORK( 2 ), 1, TAU )
            WORK( 1 ) = ONE
*
            CALL DLASET( 'L', JW-2, JW-2, ZERO, ZERO, T( 3, 1 ), LDT )
*
            CALL DLARF( 'L', NS, JW, WORK, 1, TAU, T, LDT,
     $                  WORK( JW+1 ) )
            CALL DLARF( 'R', NS, NS, WORK, 1, TAU, T, LDT,
     $                  WORK( JW+1 ) )
            CALL DLARF( 'R', JW, NS, WORK, 1, TAU, V, LDV,
     $                  WORK( JW+1 ) )
*
            CALL DGEHRD( JW, 1, NS, T, LDT, WORK, WORK( JW+1 ),
     $                   LWORK-JW, INFO )
         END IF
*
*        ==== Copy updated reduced window into place ====
*
         IF( KWTOP.GT.1 )
     $      H( KWTOP, KWTOP-1 ) = S*V( 1, 1 )
         CALL DLACPY( 'U', JW, JW, T, LDT, H( KWTOP, KWTOP ), LDH )
         CALL DCOPY( JW-1, T( 2, 1 ), LDT+1, H( KWTOP+1, KWTOP ),
     $               LDH+1 )
*
*        ==== Accumulate orthogonal matrix in order update
*        .    H and Z, if requested.  (A modified version
*        .    of  DORGHR that accumulates block Householder
*        .    transformations into V directly might be
*        .    marginally more efficient than the following.) ====
*
         IF( NS.GT.1 .AND. S.NE.ZERO ) THEN
            CALL DORGHR( JW, 1, NS, T, LDT, WORK, WORK( JW+1 ),
     $                   LWORK-JW, INFO )
            CALL DGEMM( 'N', 'N', JW, NS, NS, ONE, V, LDV, T, LDT, ZERO,
     $                  WV, LDWV )
            CALL DLACPY( 'A', JW, NS, WV, LDWV, V, LDV )
         END IF
*
*        ==== Update vertical slab in H ====
*
         IF( WANTT ) THEN
            LTOP = 1
         ELSE
            LTOP = KTOP
         END IF
         DO 70 KROW = LTOP, KWTOP - 1, NV
            KLN = MIN( NV, KWTOP-KROW )
            CALL DGEMM( 'N', 'N', KLN, JW, JW, ONE, H( KROW, KWTOP ),
     $                  LDH, V, LDV, ZERO, WV, LDWV )
            CALL DLACPY( 'A', KLN, JW, WV, LDWV, H( KROW, KWTOP ), LDH )
   70    CONTINUE
*
*        ==== Update horizontal slab in H ====
*
         IF( WANTT ) THEN
            DO 80 KCOL = KBOT + 1, N, NH
               KLN = MIN( NH, N-KCOL+1 )
               CALL DGEMM( 'C', 'N', JW, KLN, JW, ONE, V, LDV,
     $                     H( KWTOP, KCOL ), LDH, ZERO, T, LDT )
               CALL DLACPY( 'A', JW, KLN, T, LDT, H( KWTOP, KCOL ),
     $                      LDH )
   80       CONTINUE
         END IF
*
*        ==== Update vertical slab in Z ====
*
         IF( WANTZ ) THEN
            DO 90 KROW = ILOZ, IHIZ, NV
               KLN = MIN( NV, IHIZ-KROW+1 )
               CALL DGEMM( 'N', 'N', KLN, JW, JW, ONE, Z( KROW, KWTOP ),
     $                     LDZ, V, LDV, ZERO, WV, LDWV )
               CALL DLACPY( 'A', KLN, JW, WV, LDWV, Z( KROW, KWTOP ),
     $                      LDZ )
   90       CONTINUE
         END IF
      END IF
*
*     ==== Return the number of deflations ... ====
*
      ND = JW - NS
*
*     ==== ... and the number of shifts. (Subtracting
*     .    INFQR from the spike length takes care
*     .    of the case of a rare QR failure while
*     .    calculating eigenvalues of the deflation
*     .    window.)  ====
*
      NS = NS - INFQR
*
*      ==== Return optimal workspace. ====
*
      WORK( 1 ) = DBLE( LWKOPT )
*
*     ==== End of DLAQR3 ====
*
      END
      SUBROUTINE DLAQR4( WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
     $                   ILOZ, IHIZ, Z, LDZ, WORK, LWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, LWORK, N
      LOGICAL            WANTT, WANTZ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), WI( * ), WORK( * ), WR( * ),
     $                   Z( LDZ, * )
*     ..
*
*     This subroutine implements one level of recursion for DLAQR0.
*     It is a complete implementation of the small bulge multi-shift
*     QR algorithm.  It may be called by DLAQR0 and, for large enough
*     deflation window size, it may be called by DLAQR3.  This
*     subroutine is identical to DLAQR0 except that it calls DLAQR2
*     instead of DLAQR3.
*
*     Purpose
*     =======
*
*     DLAQR4 computes the eigenvalues of a Hessenberg matrix H
*     and, optionally, the matrices T and Z from the Schur decomposition
*     H = Z T Z**T, where T is an upper quasi-triangular matrix (the
*     Schur form), and Z is the orthogonal matrix of Schur vectors.
*
*     Optionally Z may be postmultiplied into an input orthogonal
*     matrix Q so that this routine can give the Schur factorization
*     of a matrix A which has been reduced to the Hessenberg form H
*     by the orthogonal matrix Q:  A = Q*H*Q**T = (QZ)*T*(QZ)**T.
*
*     Arguments
*     =========
*
*     WANTT   (input) LOGICAL
*          = .TRUE. : the full Schur form T is required;
*          = .FALSE.: only eigenvalues are required.
*
*     WANTZ   (input) LOGICAL
*          = .TRUE. : the matrix of Schur vectors Z is required;
*          = .FALSE.: Schur vectors are not required.
*
*     N     (input) INTEGER
*           The order of the matrix H.  N .GE. 0.
*
*     ILO   (input) INTEGER
*     IHI   (input) INTEGER
*           It is assumed that H is already upper triangular in rows
*           and columns 1:ILO-1 and IHI+1:N and, if ILO.GT.1,
*           H(ILO,ILO-1) is zero. ILO and IHI are normally set by a
*           previous call to DGEBAL, and then passed to DGEHRD when the
*           matrix output by DGEBAL is reduced to Hessenberg form.
*           Otherwise, ILO and IHI should be set to 1 and N,
*           respectively.  If N.GT.0, then 1.LE.ILO.LE.IHI.LE.N.
*           If N = 0, then ILO = 1 and IHI = 0.
*
*     H     (input/output) DOUBLE PRECISION array, dimension (LDH,N)
*           On entry, the upper Hessenberg matrix H.
*           On exit, if INFO = 0 and WANTT is .TRUE., then H contains
*           the upper quasi-triangular matrix T from the Schur
*           decomposition (the Schur form); 2-by-2 diagonal blocks
*           (corresponding to complex conjugate pairs of eigenvalues)
*           are returned in standard form, with H(i,i) = H(i+1,i+1)
*           and H(i+1,i)*H(i,i+1).LT.0. If INFO = 0 and WANTT is
*           .FALSE., then the contents of H are unspecified on exit.
*           (The output value of H when INFO.GT.0 is given under the
*           description of INFO below.)
*
*           This subroutine may explicitly set H(i,j) = 0 for i.GT.j and
*           j = 1, 2, ... ILO-1 or j = IHI+1, IHI+2, ... N.
*
*     LDH   (input) INTEGER
*           The leading dimension of the array H. LDH .GE. max(1,N).
*
*     WR    (output) DOUBLE PRECISION array, dimension (IHI)
*     WI    (output) DOUBLE PRECISION array, dimension (IHI)
*           The real and imaginary parts, respectively, of the computed
*           eigenvalues of H(ILO:IHI,ILO:IHI) are stored WR(ILO:IHI)
*           and WI(ILO:IHI). If two eigenvalues are computed as a
*           complex conjugate pair, they are stored in consecutive
*           elements of WR and WI, say the i-th and (i+1)th, with
*           WI(i) .GT. 0 and WI(i+1) .LT. 0. If WANTT is .TRUE., then
*           the eigenvalues are stored in the same order as on the
*           diagonal of the Schur form returned in H, with
*           WR(i) = H(i,i) and, if H(i:i+1,i:i+1) is a 2-by-2 diagonal
*           block, WI(i) = sqrt(-H(i+1,i)*H(i,i+1)) and
*           WI(i+1) = -WI(i).
*
*     ILOZ     (input) INTEGER
*     IHIZ     (input) INTEGER
*           Specify the rows of Z to which transformations must be
*           applied if WANTZ is .TRUE..
*           1 .LE. ILOZ .LE. ILO; IHI .LE. IHIZ .LE. N.
*
*     Z     (input/output) DOUBLE PRECISION array, dimension (LDZ,IHI)
*           If WANTZ is .FALSE., then Z is not referenced.
*           If WANTZ is .TRUE., then Z(ILO:IHI,ILOZ:IHIZ) is
*           replaced by Z(ILO:IHI,ILOZ:IHIZ)*U where U is the
*           orthogonal Schur factor of H(ILO:IHI,ILO:IHI).
*           (The output value of Z when INFO.GT.0 is given under
*           the description of INFO below.)
*
*     LDZ   (input) INTEGER
*           The leading dimension of the array Z.  if WANTZ is .TRUE.
*           then LDZ.GE.MAX(1,IHIZ).  Otherwize, LDZ.GE.1.
*
*     WORK  (workspace/output) DOUBLE PRECISION array, dimension LWORK
*           On exit, if LWORK = -1, WORK(1) returns an estimate of
*           the optimal value for LWORK.
*
*     LWORK (input) INTEGER
*           The dimension of the array WORK.  LWORK .GE. max(1,N)
*           is sufficient, but LWORK typically as large as 6*N may
*           be required for optimal performance.  A workspace query
*           to determine the optimal workspace size is recommended.
*
*           If LWORK = -1, then DLAQR4 does a workspace query.
*           In this case, DLAQR4 checks the input parameters and
*           estimates the optimal workspace size for the given
*           values of N, ILO and IHI.  The estimate is returned
*           in WORK(1).  No error message related to LWORK is
*           issued by XERBLA.  Neither H nor Z are accessed.
*
*
*     INFO  (output) INTEGER
*             =  0:  successful exit
*           .GT. 0:  if INFO = i, DLAQR4 failed to compute all of
*                the eigenvalues.  Elements 1:ilo-1 and i+1:n of WR
*                and WI contain those eigenvalues which have been
*                successfully computed.  (Failures are rare.)
*
*                If INFO .GT. 0 and WANT is .FALSE., then on exit,
*                the remaining unconverged eigenvalues are the eigen-
*                values of the upper Hessenberg matrix rows and
*                columns ILO through INFO of the final, output
*                value of H.
*
*                If INFO .GT. 0 and WANTT is .TRUE., then on exit
*
*           (*)  (initial value of H)*U  = U*(final value of H)
*
*                where U is an orthogonal matrix.  The final
*                value of H is upper Hessenberg and quasi-triangular
*                in rows and columns INFO+1 through IHI.
*
*                If INFO .GT. 0 and WANTZ is .TRUE., then on exit
*
*                  (final value of Z(ILO:IHI,ILOZ:IHIZ)
*                   =  (initial value of Z(ILO:IHI,ILOZ:IHIZ)*U
*
*                where U is the orthogonal matrix in (*) (regard-
*                less of the value of WANTT.)
*
*                If INFO .GT. 0 and WANTZ is .FALSE., then Z is not
*                accessed.
*
*     ================================================================
*     Based on contributions by
*        Karen Braman and Ralph Byers, Department of Mathematics,
*        University of Kansas, USA
*
*     ================================================================
*     References:
*       K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
*       Algorithm Part I: Maintaining Well Focused Shifts, and Level 3
*       Performance, SIAM Journal of Matrix Analysis, volume 23, pages
*       929--947, 2002.
*
*       K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
*       Algorithm Part II: Aggressive Early Deflation, SIAM Journal
*       of Matrix Analysis, volume 23, pages 948--973, 2002.
*
*     ================================================================
*     .. Parameters ..
*
*     ==== Matrices of order NTINY or smaller must be processed by
*     .    DLAHQR because of insufficient subdiagonal scratch space.
*     .    (This is a hard limit.) ====
*
*     ==== Exceptional deflation windows:  try to cure rare
*     .    slow convergence by increasing the size of the
*     .    deflation window after KEXNW iterations. =====
*
*     ==== Exceptional shifts: try to cure rare slow convergence
*     .    with ad-hoc exceptional shifts every KEXSH iterations.
*     .    The constants WILK1 and WILK2 are used to form the
*     .    exceptional shifts. ====
*
      INTEGER            NTINY
      PARAMETER          ( NTINY = 11 )
      INTEGER            KEXNW, KEXSH
      PARAMETER          ( KEXNW = 5, KEXSH = 6 )
      DOUBLE PRECISION   WILK1, WILK2
      PARAMETER          ( WILK1 = 0.75d0, WILK2 = -0.4375d0 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0d0, ONE = 1.0d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AA, BB, CC, CS, DD, SN, SS, SWAP
      INTEGER            I, INF, IT, ITMAX, K, KACC22, KBOT, KDU, KS,
     $                   KT, KTOP, KU, KV, KWH, KWTOP, KWV, LD, LS,
     $                   LWKOPT, NDFL, NH, NHO, NIBBLE, NMIN, NS, NSMAX,
     $                   NSR, NVE, NW, NWMAX, NWR
      LOGICAL            NWINC, SORTED
      CHARACTER          JBCMPZ*2
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   ZDUM( 1, 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLACPY, DLAHQR, DLANV2, DLAQR2, DLAQR5
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, MAX, MIN, MOD
*     ..
*     .. Executable Statements ..
      INFO = 0
*
*     ==== Quick return for N = 0: nothing to do. ====
*
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = ONE
         RETURN
      END IF
*
*     ==== Set up job flags for ILAENV. ====
*
      IF( WANTT ) THEN
         JBCMPZ( 1: 1 ) = 'S'
      ELSE
         JBCMPZ( 1: 1 ) = 'E'
      END IF
      IF( WANTZ ) THEN
         JBCMPZ( 2: 2 ) = 'V'
      ELSE
         JBCMPZ( 2: 2 ) = 'N'
      END IF
*
*     ==== Tiny matrices must use DLAHQR. ====
*
      IF( N.LE.NTINY ) THEN
*
*        ==== Estimate optimal workspace. ====
*
         LWKOPT = 1
         IF( LWORK.NE.-1 )
     $      CALL DLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
     $                   ILOZ, IHIZ, Z, LDZ, INFO )
      ELSE
*
*        ==== Use small bulge multi-shift QR with aggressive early
*        .    deflation on larger-than-tiny matrices. ====
*
*        ==== Hope for the best. ====
*
         INFO = 0
*
*        ==== NWR = recommended deflation window size.  At this
*        .    point,  N .GT. NTINY = 11, so there is enough
*        .    subdiagonal workspace for NWR.GE.2 as required.
*        .    (In fact, there is enough subdiagonal space for
*        .    NWR.GE.3.) ====
*
         NWR = ILAENV( 13, 'DLAQR4', JBCMPZ, N, ILO, IHI, LWORK )
         NWR = MAX( 2, NWR )
         NWR = MIN( IHI-ILO+1, ( N-1 ) / 3, NWR )
         NW = NWR
*
*        ==== NSR = recommended number of simultaneous shifts.
*        .    At this point N .GT. NTINY = 11, so there is at
*        .    enough subdiagonal workspace for NSR to be even
*        .    and greater than or equal to two as required. ====
*
         NSR = ILAENV( 15, 'DLAQR4', JBCMPZ, N, ILO, IHI, LWORK )
         NSR = MIN( NSR, ( N+6 ) / 9, IHI-ILO )
         NSR = MAX( 2, NSR-MOD( NSR, 2 ) )
*
*        ==== Estimate optimal workspace ====
*
*        ==== Workspace query call to DLAQR2 ====
*
         CALL DLAQR2( WANTT, WANTZ, N, ILO, IHI, NWR+1, H, LDH, ILOZ,
     $                IHIZ, Z, LDZ, LS, LD, WR, WI, H, LDH, N, H, LDH,
     $                N, H, LDH, WORK, -1 )
*
*        ==== Optimal workspace = MAX(DLAQR5, DLAQR2) ====
*
         LWKOPT = MAX( 3*NSR / 2, INT( WORK( 1 ) ) )
*
*        ==== Quick return in case of workspace query. ====
*
         IF( LWORK.EQ.-1 ) THEN
            WORK( 1 ) = DBLE( LWKOPT )
            RETURN
         END IF
*
*        ==== DLAHQR/DLAQR0 crossover point ====
*
         NMIN = ILAENV( 12, 'DLAQR4', JBCMPZ, N, ILO, IHI, LWORK )
         NMIN = MAX( NTINY, NMIN )
*
*        ==== Nibble crossover point ====
*
         NIBBLE = ILAENV( 14, 'DLAQR4', JBCMPZ, N, ILO, IHI, LWORK )
         NIBBLE = MAX( 0, NIBBLE )
*
*        ==== Accumulate reflections during ttswp?  Use block
*        .    2-by-2 structure during matrix-matrix multiply? ====
*
         KACC22 = ILAENV( 16, 'DLAQR4', JBCMPZ, N, ILO, IHI, LWORK )
         KACC22 = MAX( 0, KACC22 )
         KACC22 = MIN( 2, KACC22 )
*
*        ==== NWMAX = the largest possible deflation window for
*        .    which there is sufficient workspace. ====
*
         NWMAX = MIN( ( N-1 ) / 3, LWORK / 2 )
*
*        ==== NSMAX = the Largest number of simultaneous shifts
*        .    for which there is sufficient workspace. ====
*
         NSMAX = MIN( ( N+6 ) / 9, 2*LWORK / 3 )
         NSMAX = NSMAX - MOD( NSMAX, 2 )
*
*        ==== NDFL: an iteration count restarted at deflation. ====
*
         NDFL = 1
*
*        ==== ITMAX = iteration limit ====
*
         ITMAX = MAX( 30, 2*KEXSH )*MAX( 10, ( IHI-ILO+1 ) )
*
*        ==== Last row and column in the active block ====
*
         KBOT = IHI
*
*        ==== Main Loop ====
*
         DO 80 IT = 1, ITMAX
*
*           ==== Done when KBOT falls below ILO ====
*
            IF( KBOT.LT.ILO )
     $         GO TO 90
*
*           ==== Locate active block ====
*
            DO 10 K = KBOT, ILO + 1, -1
               IF( H( K, K-1 ).EQ.ZERO )
     $            GO TO 20
   10       CONTINUE
            K = ILO
   20       CONTINUE
            KTOP = K
*
*           ==== Select deflation window size ====
*
            NH = KBOT - KTOP + 1
            IF( NDFL.LT.KEXNW .OR. NH.LT.NW ) THEN
*
*              ==== Typical deflation window.  If possible and
*              .    advisable, nibble the entire active block.
*              .    If not, use size NWR or NWR+1 depending upon
*              .    which has the smaller corresponding subdiagonal
*              .    entry (a heuristic). ====
*
               NWINC = .TRUE.
               IF( NH.LE.MIN( NMIN, NWMAX ) ) THEN
                  NW = NH
               ELSE
                  NW = MIN( NWR, NH, NWMAX )
                  IF( NW.LT.NWMAX ) THEN
                     IF( NW.GE.NH-1 ) THEN
                        NW = NH
                     ELSE
                        KWTOP = KBOT - NW + 1
                        IF( ABS( H( KWTOP, KWTOP-1 ) ).GT.
     $                      ABS( H( KWTOP-1, KWTOP-2 ) ) )NW = NW + 1
                     END IF
                  END IF
               END IF
            ELSE
*
*              ==== Exceptional deflation window.  If there have
*              .    been no deflations in KEXNW or more iterations,
*              .    then vary the deflation window size.   At first,
*              .    because, larger windows are, in general, more
*              .    powerful than smaller ones, rapidly increase the
*              .    window up to the maximum reasonable and possible.
*              .    Then maybe try a slightly smaller window.  ====
*
               IF( NWINC .AND. NW.LT.MIN( NWMAX, NH ) ) THEN
                  NW = MIN( NWMAX, NH, 2*NW )
               ELSE
                  NWINC = .FALSE.
                  IF( NW.EQ.NH .AND. NH.GT.2 )
     $               NW = NH - 1
               END IF
            END IF
*
*           ==== Aggressive early deflation:
*           .    split workspace under the subdiagonal into
*           .      - an nw-by-nw work array V in the lower
*           .        left-hand-corner,
*           .      - an NW-by-at-least-NW-but-more-is-better
*           .        (NW-by-NHO) horizontal work array along
*           .        the bottom edge,
*           .      - an at-least-NW-but-more-is-better (NHV-by-NW)
*           .        vertical work array along the left-hand-edge.
*           .        ====
*
            KV = N - NW + 1
            KT = NW + 1
            NHO = ( N-NW-1 ) - KT + 1
            KWV = NW + 2
            NVE = ( N-NW ) - KWV + 1
*
*           ==== Aggressive early deflation ====
*
            CALL DLAQR2( WANTT, WANTZ, N, KTOP, KBOT, NW, H, LDH, ILOZ,
     $                   IHIZ, Z, LDZ, LS, LD, WR, WI, H( KV, 1 ), LDH,
     $                   NHO, H( KV, KT ), LDH, NVE, H( KWV, 1 ), LDH,
     $                   WORK, LWORK )
*
*           ==== Adjust KBOT accounting for new deflations. ====
*
            KBOT = KBOT - LD
*
*           ==== KS points to the shifts. ====
*
            KS = KBOT - LS + 1
*
*           ==== Skip an expensive QR sweep if there is a (partly
*           .    heuristic) reason to expect that many eigenvalues
*           .    will deflate without it.  Here, the QR sweep is
*           .    skipped if many eigenvalues have just been deflated
*           .    or if the remaining active block is small.
*
            IF( ( LD.EQ.0 ) .OR. ( ( 100*LD.LE.NW*NIBBLE ) .AND. ( KBOT-
     $          KTOP+1.GT.MIN( NMIN, NWMAX ) ) ) ) THEN
*
*              ==== NS = nominal number of simultaneous shifts.
*              .    This may be lowered (slightly) if DLAQR2
*              .    did not provide that many shifts. ====
*
               NS = MIN( NSMAX, NSR, MAX( 2, KBOT-KTOP ) )
               NS = NS - MOD( NS, 2 )
*
*              ==== If there have been no deflations
*              .    in a multiple of KEXSH iterations,
*              .    then try exceptional shifts.
*              .    Otherwise use shifts provided by
*              .    DLAQR2 above or from the eigenvalues
*              .    of a trailing principal submatrix. ====
*
               IF( MOD( NDFL, KEXSH ).EQ.0 ) THEN
                  KS = KBOT - NS + 1
                  DO 30 I = KBOT, MAX( KS+1, KTOP+2 ), -2
                     SS = ABS( H( I, I-1 ) ) + ABS( H( I-1, I-2 ) )
                     AA = WILK1*SS + H( I, I )
                     BB = SS
                     CC = WILK2*SS
                     DD = AA
                     CALL DLANV2( AA, BB, CC, DD, WR( I-1 ), WI( I-1 ),
     $                            WR( I ), WI( I ), CS, SN )
   30             CONTINUE
                  IF( KS.EQ.KTOP ) THEN
                     WR( KS+1 ) = H( KS+1, KS+1 )
                     WI( KS+1 ) = ZERO
                     WR( KS ) = WR( KS+1 )
                     WI( KS ) = WI( KS+1 )
                  END IF
               ELSE
*
*                 ==== Got NS/2 or fewer shifts? Use DLAHQR
*                 .    on a trailing principal submatrix to
*                 .    get more. (Since NS.LE.NSMAX.LE.(N+6)/9,
*                 .    there is enough space below the subdiagonal
*                 .    to fit an NS-by-NS scratch array.) ====
*
                  IF( KBOT-KS+1.LE.NS / 2 ) THEN
                     KS = KBOT - NS + 1
                     KT = N - NS + 1
                     CALL DLACPY( 'A', NS, NS, H( KS, KS ), LDH,
     $                            H( KT, 1 ), LDH )
                     CALL DLAHQR( .false., .false., NS, 1, NS,
     $                            H( KT, 1 ), LDH, WR( KS ), WI( KS ),
     $                            1, 1, ZDUM, 1, INF )
                     KS = KS + INF
*
*                    ==== In case of a rare QR failure use
*                    .    eigenvalues of the trailing 2-by-2
*                    .    principal submatrix.  ====
*
                     IF( KS.GE.KBOT ) THEN
                        AA = H( KBOT-1, KBOT-1 )
                        CC = H( KBOT, KBOT-1 )
                        BB = H( KBOT-1, KBOT )
                        DD = H( KBOT, KBOT )
                        CALL DLANV2( AA, BB, CC, DD, WR( KBOT-1 ),
     $                               WI( KBOT-1 ), WR( KBOT ),
     $                               WI( KBOT ), CS, SN )
                        KS = KBOT - 1
                     END IF
                  END IF
*
                  IF( KBOT-KS+1.GT.NS ) THEN
*
*                    ==== Sort the shifts (Helps a little)
*                    .    Bubble sort keeps complex conjugate
*                    .    pairs together. ====
*
                     SORTED = .false.
                     DO 50 K = KBOT, KS + 1, -1
                        IF( SORTED )
     $                     GO TO 60
                        SORTED = .true.
                        DO 40 I = KS, K - 1
                           IF( ABS( WR( I ) )+ABS( WI( I ) ).LT.
     $                         ABS( WR( I+1 ) )+ABS( WI( I+1 ) ) ) THEN
                              SORTED = .false.
*
                              SWAP = WR( I )
                              WR( I ) = WR( I+1 )
                              WR( I+1 ) = SWAP
*
                              SWAP = WI( I )
                              WI( I ) = WI( I+1 )
                              WI( I+1 ) = SWAP
                           END IF
   40                   CONTINUE
   50                CONTINUE
   60                CONTINUE
                  END IF
*
*                 ==== Shuffle shifts into pairs of real shifts
*                 .    and pairs of complex conjugate shifts
*                 .    assuming complex conjugate shifts are
*                 .    already adjacent to one another. (Yes,
*                 .    they are.)  ====
*
                  DO 70 I = KBOT, KS + 2, -2
                     IF( WI( I ).NE.-WI( I-1 ) ) THEN
*
                        SWAP = WR( I )
                        WR( I ) = WR( I-1 )
                        WR( I-1 ) = WR( I-2 )
                        WR( I-2 ) = SWAP
*
                        SWAP = WI( I )
                        WI( I ) = WI( I-1 )
                        WI( I-1 ) = WI( I-2 )
                        WI( I-2 ) = SWAP
                     END IF
   70             CONTINUE
               END IF
*
*              ==== If there are only two shifts and both are
*              .    real, then use only one.  ====
*
               IF( KBOT-KS+1.EQ.2 ) THEN
                  IF( WI( KBOT ).EQ.ZERO ) THEN
                     IF( ABS( WR( KBOT )-H( KBOT, KBOT ) ).LT.
     $                   ABS( WR( KBOT-1 )-H( KBOT, KBOT ) ) ) THEN
                        WR( KBOT-1 ) = WR( KBOT )
                     ELSE
                        WR( KBOT ) = WR( KBOT-1 )
                     END IF
                  END IF
               END IF
*
*              ==== Use up to NS of the the smallest magnatiude
*              .    shifts.  If there aren't NS shifts available,
*              .    then use them all, possibly dropping one to
*              .    make the number of shifts even. ====
*
               NS = MIN( NS, KBOT-KS+1 )
               NS = NS - MOD( NS, 2 )
               KS = KBOT - NS + 1
*
*              ==== Small-bulge multi-shift QR sweep:
*              .    split workspace under the subdiagonal into
*              .    - a KDU-by-KDU work array U in the lower
*              .      left-hand-corner,
*              .    - a KDU-by-at-least-KDU-but-more-is-better
*              .      (KDU-by-NHo) horizontal work array WH along
*              .      the bottom edge,
*              .    - and an at-least-KDU-but-more-is-better-by-KDU
*              .      (NVE-by-KDU) vertical work WV arrow along
*              .      the left-hand-edge. ====
*
               KDU = 3*NS - 3
               KU = N - KDU + 1
               KWH = KDU + 1
               NHO = ( N-KDU+1-4 ) - ( KDU+1 ) + 1
               KWV = KDU + 4
               NVE = N - KDU - KWV + 1
*
*              ==== Small-bulge multi-shift QR sweep ====
*
               CALL DLAQR5( WANTT, WANTZ, KACC22, N, KTOP, KBOT, NS,
     $                      WR( KS ), WI( KS ), H, LDH, ILOZ, IHIZ, Z,
     $                      LDZ, WORK, 3, H( KU, 1 ), LDH, NVE,
     $                      H( KWV, 1 ), LDH, NHO, H( KU, KWH ), LDH )
            END IF
*
*           ==== Note progress (or the lack of it). ====
*
            IF( LD.GT.0 ) THEN
               NDFL = 1
            ELSE
               NDFL = NDFL + 1
            END IF
*
*           ==== End of main loop ====
   80    CONTINUE
*
*        ==== Iteration limit exceeded.  Set INFO to show where
*        .    the problem occurred and exit. ====
*
         INFO = KBOT
   90    CONTINUE
      END IF
*
*     ==== Return the optimal value of LWORK. ====
*
      WORK( 1 ) = DBLE( LWKOPT )
*
*     ==== End of DLAQR4 ====
*
      END
      SUBROUTINE DLAQR5( WANTT, WANTZ, KACC22, N, KTOP, KBOT, NSHFTS,
     $                   SR, SI, H, LDH, ILOZ, IHIZ, Z, LDZ, V, LDV, U,
     $                   LDU, NV, WV, LDWV, NH, WH, LDWH )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IHIZ, ILOZ, KACC22, KBOT, KTOP, LDH, LDU, LDV,
     $                   LDWH, LDWV, LDZ, N, NH, NSHFTS, NV
      LOGICAL            WANTT, WANTZ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), SI( * ), SR( * ), U( LDU, * ),
     $                   V( LDV, * ), WH( LDWH, * ), WV( LDWV, * ),
     $                   Z( LDZ, * )
*     ..
*
*     This auxiliary subroutine called by DLAQR0 performs a
*     single small-bulge multi-shift QR sweep.
*
*      WANTT  (input) logical scalar
*             WANTT = .true. if the quasi-triangular Schur factor
*             is being computed.  WANTT is set to .false. otherwise.
*
*      WANTZ  (input) logical scalar
*             WANTZ = .true. if the orthogonal Schur factor is being
*             computed.  WANTZ is set to .false. otherwise.
*
*      KACC22 (input) integer with value 0, 1, or 2.
*             Specifies the computation mode of far-from-diagonal
*             orthogonal updates.
*        = 0: DLAQR5 does not accumulate reflections and does not
*             use matrix-matrix multiply to update far-from-diagonal
*             matrix entries.
*        = 1: DLAQR5 accumulates reflections and uses matrix-matrix
*             multiply to update the far-from-diagonal matrix entries.
*        = 2: DLAQR5 accumulates reflections, uses matrix-matrix
*             multiply to update the far-from-diagonal matrix entries,
*             and takes advantage of 2-by-2 block structure during
*             matrix multiplies.
*
*      N      (input) integer scalar
*             N is the order of the Hessenberg matrix H upon which this
*             subroutine operates.
*
*      KTOP   (input) integer scalar
*      KBOT   (input) integer scalar
*             These are the first and last rows and columns of an
*             isolated diagonal block upon which the QR sweep is to be
*             applied. It is assumed without a check that
*                       either KTOP = 1  or   H(KTOP,KTOP-1) = 0
*             and
*                       either KBOT = N  or   H(KBOT+1,KBOT) = 0.
*
*      NSHFTS (input) integer scalar
*             NSHFTS gives the number of simultaneous shifts.  NSHFTS
*             must be positive and even.
*
*      SR     (input) DOUBLE PRECISION array of size (NSHFTS)
*      SI     (input) DOUBLE PRECISION array of size (NSHFTS)
*             SR contains the real parts and SI contains the imaginary
*             parts of the NSHFTS shifts of origin that define the
*             multi-shift QR sweep.
*
*      H      (input/output) DOUBLE PRECISION array of size (LDH,N)
*             On input H contains a Hessenberg matrix.  On output a
*             multi-shift QR sweep with shifts SR(J)+i*SI(J) is applied
*             to the isolated diagonal block in rows and columns KTOP
*             through KBOT.
*
*      LDH    (input) integer scalar
*             LDH is the leading dimension of H just as declared in the
*             calling procedure.  LDH.GE.MAX(1,N).
*
*      ILOZ   (input) INTEGER
*      IHIZ   (input) INTEGER
*             Specify the rows of Z to which transformations must be
*             applied if WANTZ is .TRUE.. 1 .LE. ILOZ .LE. IHIZ .LE. N
*
*      Z      (input/output) DOUBLE PRECISION array of size (LDZ,IHI)
*             If WANTZ = .TRUE., then the QR Sweep orthogonal
*             similarity transformation is accumulated into
*             Z(ILOZ:IHIZ,ILO:IHI) from the right.
*             If WANTZ = .FALSE., then Z is unreferenced.
*
*      LDZ    (input) integer scalar
*             LDA is the leading dimension of Z just as declared in
*             the calling procedure. LDZ.GE.N.
*
*      V      (workspace) DOUBLE PRECISION array of size (LDV,NSHFTS/2)
*
*      LDV    (input) integer scalar
*             LDV is the leading dimension of V as declared in the
*             calling procedure.  LDV.GE.3.
*
*      U      (workspace) DOUBLE PRECISION array of size
*             (LDU,3*NSHFTS-3)
*
*      LDU    (input) integer scalar
*             LDU is the leading dimension of U just as declared in the
*             in the calling subroutine.  LDU.GE.3*NSHFTS-3.
*
*      NH     (input) integer scalar
*             NH is the number of columns in array WH available for
*             workspace. NH.GE.1.
*
*      WH     (workspace) DOUBLE PRECISION array of size (LDWH,NH)
*
*      LDWH   (input) integer scalar
*             Leading dimension of WH just as declared in the
*             calling procedure.  LDWH.GE.3*NSHFTS-3.
*
*      NV     (input) integer scalar
*             NV is the number of rows in WV agailable for workspace.
*             NV.GE.1.
*
*      WV     (workspace) DOUBLE PRECISION array of size
*             (LDWV,3*NSHFTS-3)
*
*      LDWV   (input) integer scalar
*             LDWV is the leading dimension of WV as declared in the
*             in the calling subroutine.  LDWV.GE.NV.
*
*
*     ================================================================
*     Based on contributions by
*        Karen Braman and Ralph Byers, Department of Mathematics,
*        University of Kansas, USA
*
*     ============================================================
*     Reference:
*
*     K. Braman, R. Byers and R. Mathias, The Multi-Shift QR
*     Algorithm Part I: Maintaining Well Focused Shifts, and
*     Level 3 Performance, SIAM Journal of Matrix Analysis,
*     volume 23, pages 929--947, 2002.
*
*     ============================================================
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0d0, ONE = 1.0d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   ALPHA, BETA, H11, H12, H21, H22, REFSUM,
     $                   SAFMAX, SAFMIN, SCL, SMLNUM, SWAP, TST1, TST2,
     $                   ULP
      INTEGER            I, I2, I4, INCOL, J, J2, J4, JBOT, JCOL, JLEN,
     $                   JROW, JTOP, K, K1, KDU, KMS, KNZ, KRCOL, KZS,
     $                   M, M22, MBOT, MEND, MSTART, MTOP, NBMPS, NDCOL,
     $                   NS, NU
      LOGICAL            ACCUM, BLK22, BMP22
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
*
      INTRINSIC          ABS, DBLE, MAX, MIN, MOD
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   VT( 3 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DLABAD, DLACPY, DLAQR1, DLARFG, DLASET,
     $                   DTRMM
*     ..
*     .. Executable Statements ..
*
*     ==== If there are no shifts, then there is nothing to do. ====
*
      IF( NSHFTS.LT.2 )
     $   RETURN
*
*     ==== If the active block is empty or 1-by-1, then there
*     .    is nothing to do. ====
*
      IF( KTOP.GE.KBOT )
     $   RETURN
*
*     ==== Shuffle shifts into pairs of real shifts and pairs
*     .    of complex conjugate shifts assuming complex
*     .    conjugate shifts are already adjacent to one
*     .    another. ====
*
      DO 10 I = 1, NSHFTS - 2, 2
         IF( SI( I ).NE.-SI( I+1 ) ) THEN
*
            SWAP = SR( I )
            SR( I ) = SR( I+1 )
            SR( I+1 ) = SR( I+2 )
            SR( I+2 ) = SWAP
*
            SWAP = SI( I )
            SI( I ) = SI( I+1 )
            SI( I+1 ) = SI( I+2 )
            SI( I+2 ) = SWAP
         END IF
   10 CONTINUE
*
*     ==== NSHFTS is supposed to be even, but if is odd,
*     .    then simply reduce it by one.  The shuffle above
*     .    ensures that the dropped shift is real and that
*     .    the remaining shifts are paired. ====
*
      NS = NSHFTS - MOD( NSHFTS, 2 )
*
*     ==== Machine constants for deflation ====
*
      SAFMIN = DLAMCH( 'SAFE MINIMUM' )
      SAFMAX = ONE / SAFMIN
      CALL DLABAD( SAFMIN, SAFMAX )
      ULP = DLAMCH( 'PRECISION' )
      SMLNUM = SAFMIN*( DBLE( N ) / ULP )
*
*     ==== Use accumulated reflections to update far-from-diagonal
*     .    entries ? ====
*
      ACCUM = ( KACC22.EQ.1 ) .OR. ( KACC22.EQ.2 )
*
*     ==== If so, exploit the 2-by-2 block structure? ====
*
      BLK22 = ( NS.GT.2 ) .AND. ( KACC22.EQ.2 )
*
*     ==== clear trash ====
*
      IF( KTOP+2.LE.KBOT )
     $   H( KTOP+2, KTOP ) = ZERO
*
*     ==== NBMPS = number of 2-shift bulges in the chain ====
*
      NBMPS = NS / 2
*
*     ==== KDU = width of slab ====
*
      KDU = 6*NBMPS - 3
*
*     ==== Create and chase chains of NBMPS bulges ====
*
      DO 220 INCOL = 3*( 1-NBMPS ) + KTOP - 1, KBOT - 2, 3*NBMPS - 2
         NDCOL = INCOL + KDU
         IF( ACCUM )
     $      CALL DLASET( 'ALL', KDU, KDU, ZERO, ONE, U, LDU )
*
*        ==== Near-the-diagonal bulge chase.  The following loop
*        .    performs the near-the-diagonal part of a small bulge
*        .    multi-shift QR sweep.  Each 6*NBMPS-2 column diagonal
*        .    chunk extends from column INCOL to column NDCOL
*        .    (including both column INCOL and column NDCOL). The
*        .    following loop chases a 3*NBMPS column long chain of
*        .    NBMPS bulges 3*NBMPS-2 columns to the right.  (INCOL
*        .    may be less than KTOP and and NDCOL may be greater than
*        .    KBOT indicating phantom columns from which to chase
*        .    bulges before they are actually introduced or to which
*        .    to chase bulges beyond column KBOT.)  ====
*
         DO 150 KRCOL = INCOL, MIN( INCOL+3*NBMPS-3, KBOT-2 )
*
*           ==== Bulges number MTOP to MBOT are active double implicit
*           .    shift bulges.  There may or may not also be small
*           .    2-by-2 bulge, if there is room.  The inactive bulges
*           .    (if any) must wait until the active bulges have moved
*           .    down the diagonal to make room.  The phantom matrix
*           .    paradigm described above helps keep track.  ====
*
            MTOP = MAX( 1, ( ( KTOP-1 )-KRCOL+2 ) / 3+1 )
            MBOT = MIN( NBMPS, ( KBOT-KRCOL ) / 3 )
            M22 = MBOT + 1
            BMP22 = ( MBOT.LT.NBMPS ) .AND. ( KRCOL+3*( M22-1 ) ).EQ.
     $              ( KBOT-2 )
*
*           ==== Generate reflections to chase the chain right
*           .    one column.  (The minimum value of K is KTOP-1.) ====
*
            DO 20 M = MTOP, MBOT
               K = KRCOL + 3*( M-1 )
               IF( K.EQ.KTOP-1 ) THEN
                  CALL DLAQR1( 3, H( KTOP, KTOP ), LDH, SR( 2*M-1 ),
     $                         SI( 2*M-1 ), SR( 2*M ), SI( 2*M ),
     $                         V( 1, M ) )
                  ALPHA = V( 1, M )
                  CALL DLARFG( 3, ALPHA, V( 2, M ), 1, V( 1, M ) )
               ELSE
                  BETA = H( K+1, K )
                  V( 2, M ) = H( K+2, K )
                  V( 3, M ) = H( K+3, K )
                  CALL DLARFG( 3, BETA, V( 2, M ), 1, V( 1, M ) )
*
*                 ==== A Bulge may collapse because of vigilant
*                 .    deflation or destructive underflow.  (The
*                 .    initial bulge is always collapsed.) Use
*                 .    the two-small-subdiagonals trick to try
*                 .    to get it started again. If V(2,M).NE.0 and
*                 .    V(3,M) = H(K+3,K+1) = H(K+3,K+2) = 0, then
*                 .    this bulge is collapsing into a zero
*                 .    subdiagonal.  It will be restarted next
*                 .    trip through the loop.)
*
                  IF( V( 1, M ).NE.ZERO .AND.
     $                ( V( 3, M ).NE.ZERO .OR. ( H( K+3,
     $                K+1 ).EQ.ZERO .AND. H( K+3, K+2 ).EQ.ZERO ) ) )
     $                 THEN
*
*                    ==== Typical case: not collapsed (yet). ====
*
                     H( K+1, K ) = BETA
                     H( K+2, K ) = ZERO
                     H( K+3, K ) = ZERO
                  ELSE
*
*                    ==== Atypical case: collapsed.  Attempt to
*                    .    reintroduce ignoring H(K+1,K).  If the
*                    .    fill resulting from the new reflector
*                    .    is too large, then abandon it.
*                    .    Otherwise, use the new one. ====
*
                     CALL DLAQR1( 3, H( K+1, K+1 ), LDH, SR( 2*M-1 ),
     $                            SI( 2*M-1 ), SR( 2*M ), SI( 2*M ),
     $                            VT )
                     SCL = ABS( VT( 1 ) ) + ABS( VT( 2 ) ) +
     $                     ABS( VT( 3 ) )
                     IF( SCL.NE.ZERO ) THEN
                        VT( 1 ) = VT( 1 ) / SCL
                        VT( 2 ) = VT( 2 ) / SCL
                        VT( 3 ) = VT( 3 ) / SCL
                     END IF
*
*                    ==== The following is the traditional and
*                    .    conservative two-small-subdiagonals
*                    .    test.  ====
*                    .
                     IF( ABS( H( K+1, K ) )*( ABS( VT( 2 ) )+
     $                   ABS( VT( 3 ) ) ).GT.ULP*ABS( VT( 1 ) )*
     $                   ( ABS( H( K, K ) )+ABS( H( K+1,
     $                   K+1 ) )+ABS( H( K+2, K+2 ) ) ) ) THEN
*
*                       ==== Starting a new bulge here would
*                       .    create non-negligible fill.   If
*                       .    the old reflector is diagonal (only
*                       .    possible with underflows), then
*                       .    change it to I.  Otherwise, use
*                       .    it with trepidation. ====
*
                        IF( V( 2, M ).EQ.ZERO .AND. V( 3, M ).EQ.ZERO )
     $                       THEN
                           V( 1, M ) = ZERO
                        ELSE
                           H( K+1, K ) = BETA
                           H( K+2, K ) = ZERO
                           H( K+3, K ) = ZERO
                        END IF
                     ELSE
*
*                       ==== Stating a new bulge here would
*                       .    create only negligible fill.
*                       .    Replace the old reflector with
*                       .    the new one. ====
*
                        ALPHA = VT( 1 )
                        CALL DLARFG( 3, ALPHA, VT( 2 ), 1, VT( 1 ) )
                        REFSUM = H( K+1, K ) + H( K+2, K )*VT( 2 ) +
     $                           H( K+3, K )*VT( 3 )
                        H( K+1, K ) = H( K+1, K ) - VT( 1 )*REFSUM
                        H( K+2, K ) = ZERO
                        H( K+3, K ) = ZERO
                        V( 1, M ) = VT( 1 )
                        V( 2, M ) = VT( 2 )
                        V( 3, M ) = VT( 3 )
                     END IF
                  END IF
               END IF
   20       CONTINUE
*
*           ==== Generate a 2-by-2 reflection, if needed. ====
*
            K = KRCOL + 3*( M22-1 )
            IF( BMP22 ) THEN
               IF( K.EQ.KTOP-1 ) THEN
                  CALL DLAQR1( 2, H( K+1, K+1 ), LDH, SR( 2*M22-1 ),
     $                         SI( 2*M22-1 ), SR( 2*M22 ), SI( 2*M22 ),
     $                         V( 1, M22 ) )
                  BETA = V( 1, M22 )
                  CALL DLARFG( 2, BETA, V( 2, M22 ), 1, V( 1, M22 ) )
               ELSE
                  BETA = H( K+1, K )
                  V( 2, M22 ) = H( K+2, K )
                  CALL DLARFG( 2, BETA, V( 2, M22 ), 1, V( 1, M22 ) )
                  H( K+1, K ) = BETA
                  H( K+2, K ) = ZERO
               END IF
            ELSE
*
*              ==== Initialize V(1,M22) here to avoid possible undefined
*              .    variable problems later. ====
*
               V( 1, M22 ) = ZERO
            END IF
*
*           ==== Multiply H by reflections from the left ====
*
            IF( ACCUM ) THEN
               JBOT = MIN( NDCOL, KBOT )
            ELSE IF( WANTT ) THEN
               JBOT = N
            ELSE
               JBOT = KBOT
            END IF
            DO 40 J = MAX( KTOP, KRCOL ), JBOT
               MEND = MIN( MBOT, ( J-KRCOL+2 ) / 3 )
               DO 30 M = MTOP, MEND
                  K = KRCOL + 3*( M-1 )
                  REFSUM = V( 1, M )*( H( K+1, J )+V( 2, M )*
     $                     H( K+2, J )+V( 3, M )*H( K+3, J ) )
                  H( K+1, J ) = H( K+1, J ) - REFSUM
                  H( K+2, J ) = H( K+2, J ) - REFSUM*V( 2, M )
                  H( K+3, J ) = H( K+3, J ) - REFSUM*V( 3, M )
   30          CONTINUE
   40       CONTINUE
            IF( BMP22 ) THEN
               K = KRCOL + 3*( M22-1 )
               DO 50 J = MAX( K+1, KTOP ), JBOT
                  REFSUM = V( 1, M22 )*( H( K+1, J )+V( 2, M22 )*
     $                     H( K+2, J ) )
                  H( K+1, J ) = H( K+1, J ) - REFSUM
                  H( K+2, J ) = H( K+2, J ) - REFSUM*V( 2, M22 )
   50          CONTINUE
            END IF
*
*           ==== Multiply H by reflections from the right.
*           .    Delay filling in the last row until the
*           .    vigilant deflation check is complete. ====
*
            IF( ACCUM ) THEN
               JTOP = MAX( KTOP, INCOL )
            ELSE IF( WANTT ) THEN
               JTOP = 1
            ELSE
               JTOP = KTOP
            END IF
            DO 90 M = MTOP, MBOT
               IF( V( 1, M ).NE.ZERO ) THEN
                  K = KRCOL + 3*( M-1 )
                  DO 60 J = JTOP, MIN( KBOT, K+3 )
                     REFSUM = V( 1, M )*( H( J, K+1 )+V( 2, M )*
     $                        H( J, K+2 )+V( 3, M )*H( J, K+3 ) )
                     H( J, K+1 ) = H( J, K+1 ) - REFSUM
                     H( J, K+2 ) = H( J, K+2 ) - REFSUM*V( 2, M )
                     H( J, K+3 ) = H( J, K+3 ) - REFSUM*V( 3, M )
   60             CONTINUE
*
                  IF( ACCUM ) THEN
*
*                    ==== Accumulate U. (If necessary, update Z later
*                    .    with with an efficient matrix-matrix
*                    .    multiply.) ====
*
                     KMS = K - INCOL
                     DO 70 J = MAX( 1, KTOP-INCOL ), KDU
                        REFSUM = V( 1, M )*( U( J, KMS+1 )+V( 2, M )*
     $                           U( J, KMS+2 )+V( 3, M )*U( J, KMS+3 ) )
                        U( J, KMS+1 ) = U( J, KMS+1 ) - REFSUM
                        U( J, KMS+2 ) = U( J, KMS+2 ) - REFSUM*V( 2, M )
                        U( J, KMS+3 ) = U( J, KMS+3 ) - REFSUM*V( 3, M )
   70                CONTINUE
                  ELSE IF( WANTZ ) THEN
*
*                    ==== U is not accumulated, so update Z
*                    .    now by multiplying by reflections
*                    .    from the right. ====
*
                     DO 80 J = ILOZ, IHIZ
                        REFSUM = V( 1, M )*( Z( J, K+1 )+V( 2, M )*
     $                           Z( J, K+2 )+V( 3, M )*Z( J, K+3 ) )
                        Z( J, K+1 ) = Z( J, K+1 ) - REFSUM
                        Z( J, K+2 ) = Z( J, K+2 ) - REFSUM*V( 2, M )
                        Z( J, K+3 ) = Z( J, K+3 ) - REFSUM*V( 3, M )
   80                CONTINUE
                  END IF
               END IF
   90       CONTINUE
*
*           ==== Special case: 2-by-2 reflection (if needed) ====
*
            K = KRCOL + 3*( M22-1 )
            IF( BMP22 .AND. ( V( 1, M22 ).NE.ZERO ) ) THEN
               DO 100 J = JTOP, MIN( KBOT, K+3 )
                  REFSUM = V( 1, M22 )*( H( J, K+1 )+V( 2, M22 )*
     $                     H( J, K+2 ) )
                  H( J, K+1 ) = H( J, K+1 ) - REFSUM
                  H( J, K+2 ) = H( J, K+2 ) - REFSUM*V( 2, M22 )
  100          CONTINUE
*
               IF( ACCUM ) THEN
                  KMS = K - INCOL
                  DO 110 J = MAX( 1, KTOP-INCOL ), KDU
                     REFSUM = V( 1, M22 )*( U( J, KMS+1 )+V( 2, M22 )*
     $                        U( J, KMS+2 ) )
                     U( J, KMS+1 ) = U( J, KMS+1 ) - REFSUM
                     U( J, KMS+2 ) = U( J, KMS+2 ) - REFSUM*V( 2, M22 )
  110             CONTINUE
               ELSE IF( WANTZ ) THEN
                  DO 120 J = ILOZ, IHIZ
                     REFSUM = V( 1, M22 )*( Z( J, K+1 )+V( 2, M22 )*
     $                        Z( J, K+2 ) )
                     Z( J, K+1 ) = Z( J, K+1 ) - REFSUM
                     Z( J, K+2 ) = Z( J, K+2 ) - REFSUM*V( 2, M22 )
  120             CONTINUE
               END IF
            END IF
*
*           ==== Vigilant deflation check ====
*
            MSTART = MTOP
            IF( KRCOL+3*( MSTART-1 ).LT.KTOP )
     $         MSTART = MSTART + 1
            MEND = MBOT
            IF( BMP22 )
     $         MEND = MEND + 1
            IF( KRCOL.EQ.KBOT-2 )
     $         MEND = MEND + 1
            DO 130 M = MSTART, MEND
               K = MIN( KBOT-1, KRCOL+3*( M-1 ) )
*
*              ==== The following convergence test requires that
*              .    the tradition small-compared-to-nearby-diagonals
*              .    criterion and the Ahues & Tisseur (LAWN 122, 1997)
*              .    criteria both be satisfied.  The latter improves
*              .    accuracy in some examples. Falling back on an
*              .    alternate convergence criterion when TST1 or TST2
*              .    is zero (as done here) is traditional but probably
*              .    unnecessary. ====
*
               IF( H( K+1, K ).NE.ZERO ) THEN
                  TST1 = ABS( H( K, K ) ) + ABS( H( K+1, K+1 ) )
                  IF( TST1.EQ.ZERO ) THEN
                     IF( K.GE.KTOP+1 )
     $                  TST1 = TST1 + ABS( H( K, K-1 ) )
                     IF( K.GE.KTOP+2 )
     $                  TST1 = TST1 + ABS( H( K, K-2 ) )
                     IF( K.GE.KTOP+3 )
     $                  TST1 = TST1 + ABS( H( K, K-3 ) )
                     IF( K.LE.KBOT-2 )
     $                  TST1 = TST1 + ABS( H( K+2, K+1 ) )
                     IF( K.LE.KBOT-3 )
     $                  TST1 = TST1 + ABS( H( K+3, K+1 ) )
                     IF( K.LE.KBOT-4 )
     $                  TST1 = TST1 + ABS( H( K+4, K+1 ) )
                  END IF
                  IF( ABS( H( K+1, K ) ).LE.MAX( SMLNUM, ULP*TST1 ) )
     $                 THEN
                     H12 = MAX( ABS( H( K+1, K ) ), ABS( H( K, K+1 ) ) )
                     H21 = MIN( ABS( H( K+1, K ) ), ABS( H( K, K+1 ) ) )
                     H11 = MAX( ABS( H( K+1, K+1 ) ),
     $                     ABS( H( K, K )-H( K+1, K+1 ) ) )
                     H22 = MIN( ABS( H( K+1, K+1 ) ),
     $                     ABS( H( K, K )-H( K+1, K+1 ) ) )
                     SCL = H11 + H12
                     TST2 = H22*( H11 / SCL )
*
                     IF( TST2.EQ.ZERO .OR. H21*( H12 / SCL ).LE.
     $                   MAX( SMLNUM, ULP*TST2 ) )H( K+1, K ) = ZERO
                  END IF
               END IF
  130       CONTINUE
*
*           ==== Fill in the last row of each bulge. ====
*
            MEND = MIN( NBMPS, ( KBOT-KRCOL-1 ) / 3 )
            DO 140 M = MTOP, MEND
               K = KRCOL + 3*( M-1 )
               REFSUM = V( 1, M )*V( 3, M )*H( K+4, K+3 )
               H( K+4, K+1 ) = -REFSUM
               H( K+4, K+2 ) = -REFSUM*V( 2, M )
               H( K+4, K+3 ) = H( K+4, K+3 ) - REFSUM*V( 3, M )
  140       CONTINUE
*
*           ==== End of near-the-diagonal bulge chase. ====
*
  150    CONTINUE
*
*        ==== Use U (if accumulated) to update far-from-diagonal
*        .    entries in H.  If required, use U to update Z as
*        .    well. ====
*
         IF( ACCUM ) THEN
            IF( WANTT ) THEN
               JTOP = 1
               JBOT = N
            ELSE
               JTOP = KTOP
               JBOT = KBOT
            END IF
            IF( ( .NOT.BLK22 ) .OR. ( INCOL.LT.KTOP ) .OR.
     $          ( NDCOL.GT.KBOT ) .OR. ( NS.LE.2 ) ) THEN
*
*              ==== Updates not exploiting the 2-by-2 block
*              .    structure of U.  K1 and NU keep track of
*              .    the location and size of U in the special
*              .    cases of introducing bulges and chasing
*              .    bulges off the bottom.  In these special
*              .    cases and in case the number of shifts
*              .    is NS = 2, there is no 2-by-2 block
*              .    structure to exploit.  ====
*
               K1 = MAX( 1, KTOP-INCOL )
               NU = ( KDU-MAX( 0, NDCOL-KBOT ) ) - K1 + 1
*
*              ==== Horizontal Multiply ====
*
               DO 160 JCOL = MIN( NDCOL, KBOT ) + 1, JBOT, NH
                  JLEN = MIN( NH, JBOT-JCOL+1 )
                  CALL DGEMM( 'C', 'N', NU, JLEN, NU, ONE, U( K1, K1 ),
     $                        LDU, H( INCOL+K1, JCOL ), LDH, ZERO, WH,
     $                        LDWH )
                  CALL DLACPY( 'ALL', NU, JLEN, WH, LDWH,
     $                         H( INCOL+K1, JCOL ), LDH )
  160          CONTINUE
*
*              ==== Vertical multiply ====
*
               DO 170 JROW = JTOP, MAX( KTOP, INCOL ) - 1, NV
                  JLEN = MIN( NV, MAX( KTOP, INCOL )-JROW )
                  CALL DGEMM( 'N', 'N', JLEN, NU, NU, ONE,
     $                        H( JROW, INCOL+K1 ), LDH, U( K1, K1 ),
     $                        LDU, ZERO, WV, LDWV )
                  CALL DLACPY( 'ALL', JLEN, NU, WV, LDWV,
     $                         H( JROW, INCOL+K1 ), LDH )
  170          CONTINUE
*
*              ==== Z multiply (also vertical) ====
*
               IF( WANTZ ) THEN
                  DO 180 JROW = ILOZ, IHIZ, NV
                     JLEN = MIN( NV, IHIZ-JROW+1 )
                     CALL DGEMM( 'N', 'N', JLEN, NU, NU, ONE,
     $                           Z( JROW, INCOL+K1 ), LDZ, U( K1, K1 ),
     $                           LDU, ZERO, WV, LDWV )
                     CALL DLACPY( 'ALL', JLEN, NU, WV, LDWV,
     $                            Z( JROW, INCOL+K1 ), LDZ )
  180             CONTINUE
               END IF
            ELSE
*
*              ==== Updates exploiting U's 2-by-2 block structure.
*              .    (I2, I4, J2, J4 are the last rows and columns
*              .    of the blocks.) ====
*
               I2 = ( KDU+1 ) / 2
               I4 = KDU
               J2 = I4 - I2
               J4 = KDU
*
*              ==== KZS and KNZ deal with the band of zeros
*              .    along the diagonal of one of the triangular
*              .    blocks. ====
*
               KZS = ( J4-J2 ) - ( NS+1 )
               KNZ = NS + 1
*
*              ==== Horizontal multiply ====
*
               DO 190 JCOL = MIN( NDCOL, KBOT ) + 1, JBOT, NH
                  JLEN = MIN( NH, JBOT-JCOL+1 )
*
*                 ==== Copy bottom of H to top+KZS of scratch ====
*                  (The first KZS rows get multiplied by zero.) ====
*
                  CALL DLACPY( 'ALL', KNZ, JLEN, H( INCOL+1+J2, JCOL ),
     $                         LDH, WH( KZS+1, 1 ), LDWH )
*
*                 ==== Multiply by U21' ====
*
                  CALL DLASET( 'ALL', KZS, JLEN, ZERO, ZERO, WH, LDWH )
                  CALL DTRMM( 'L', 'U', 'C', 'N', KNZ, JLEN, ONE,
     $                        U( J2+1, 1+KZS ), LDU, WH( KZS+1, 1 ),
     $                        LDWH )
*
*                 ==== Multiply top of H by U11' ====
*
                  CALL DGEMM( 'C', 'N', I2, JLEN, J2, ONE, U, LDU,
     $                        H( INCOL+1, JCOL ), LDH, ONE, WH, LDWH )
*
*                 ==== Copy top of H bottom of WH ====
*
                  CALL DLACPY( 'ALL', J2, JLEN, H( INCOL+1, JCOL ), LDH,
     $                         WH( I2+1, 1 ), LDWH )
*
*                 ==== Multiply by U21' ====
*
                  CALL DTRMM( 'L', 'L', 'C', 'N', J2, JLEN, ONE,
     $                        U( 1, I2+1 ), LDU, WH( I2+1, 1 ), LDWH )
*
*                 ==== Multiply by U22 ====
*
                  CALL DGEMM( 'C', 'N', I4-I2, JLEN, J4-J2, ONE,
     $                        U( J2+1, I2+1 ), LDU,
     $                        H( INCOL+1+J2, JCOL ), LDH, ONE,
     $                        WH( I2+1, 1 ), LDWH )
*
*                 ==== Copy it back ====
*
                  CALL DLACPY( 'ALL', KDU, JLEN, WH, LDWH,
     $                         H( INCOL+1, JCOL ), LDH )
  190          CONTINUE
*
*              ==== Vertical multiply ====
*
               DO 200 JROW = JTOP, MAX( INCOL, KTOP ) - 1, NV
                  JLEN = MIN( NV, MAX( INCOL, KTOP )-JROW )
*
*                 ==== Copy right of H to scratch (the first KZS
*                 .    columns get multiplied by zero) ====
*
                  CALL DLACPY( 'ALL', JLEN, KNZ, H( JROW, INCOL+1+J2 ),
     $                         LDH, WV( 1, 1+KZS ), LDWV )
*
*                 ==== Multiply by U21 ====
*
                  CALL DLASET( 'ALL', JLEN, KZS, ZERO, ZERO, WV, LDWV )
                  CALL DTRMM( 'R', 'U', 'N', 'N', JLEN, KNZ, ONE,
     $                        U( J2+1, 1+KZS ), LDU, WV( 1, 1+KZS ),
     $                        LDWV )
*
*                 ==== Multiply by U11 ====
*
                  CALL DGEMM( 'N', 'N', JLEN, I2, J2, ONE,
     $                        H( JROW, INCOL+1 ), LDH, U, LDU, ONE, WV,
     $                        LDWV )
*
*                 ==== Copy left of H to right of scratch ====
*
                  CALL DLACPY( 'ALL', JLEN, J2, H( JROW, INCOL+1 ), LDH,
     $                         WV( 1, 1+I2 ), LDWV )
*
*                 ==== Multiply by U21 ====
*
                  CALL DTRMM( 'R', 'L', 'N', 'N', JLEN, I4-I2, ONE,
     $                        U( 1, I2+1 ), LDU, WV( 1, 1+I2 ), LDWV )
*
*                 ==== Multiply by U22 ====
*
                  CALL DGEMM( 'N', 'N', JLEN, I4-I2, J4-J2, ONE,
     $                        H( JROW, INCOL+1+J2 ), LDH,
     $                        U( J2+1, I2+1 ), LDU, ONE, WV( 1, 1+I2 ),
     $                        LDWV )
*
*                 ==== Copy it back ====
*
                  CALL DLACPY( 'ALL', JLEN, KDU, WV, LDWV,
     $                         H( JROW, INCOL+1 ), LDH )
  200          CONTINUE
*
*              ==== Multiply Z (also vertical) ====
*
               IF( WANTZ ) THEN
                  DO 210 JROW = ILOZ, IHIZ, NV
                     JLEN = MIN( NV, IHIZ-JROW+1 )
*
*                    ==== Copy right of Z to left of scratch (first
*                    .     KZS columns get multiplied by zero) ====
*
                     CALL DLACPY( 'ALL', JLEN, KNZ,
     $                            Z( JROW, INCOL+1+J2 ), LDZ,
     $                            WV( 1, 1+KZS ), LDWV )
*
*                    ==== Multiply by U12 ====
*
                     CALL DLASET( 'ALL', JLEN, KZS, ZERO, ZERO, WV,
     $                            LDWV )
                     CALL DTRMM( 'R', 'U', 'N', 'N', JLEN, KNZ, ONE,
     $                           U( J2+1, 1+KZS ), LDU, WV( 1, 1+KZS ),
     $                           LDWV )
*
*                    ==== Multiply by U11 ====
*
                     CALL DGEMM( 'N', 'N', JLEN, I2, J2, ONE,
     $                           Z( JROW, INCOL+1 ), LDZ, U, LDU, ONE,
     $                           WV, LDWV )
*
*                    ==== Copy left of Z to right of scratch ====
*
                     CALL DLACPY( 'ALL', JLEN, J2, Z( JROW, INCOL+1 ),
     $                            LDZ, WV( 1, 1+I2 ), LDWV )
*
*                    ==== Multiply by U21 ====
*
                     CALL DTRMM( 'R', 'L', 'N', 'N', JLEN, I4-I2, ONE,
     $                           U( 1, I2+1 ), LDU, WV( 1, 1+I2 ),
     $                           LDWV )
*
*                    ==== Multiply by U22 ====
*
                     CALL DGEMM( 'N', 'N', JLEN, I4-I2, J4-J2, ONE,
     $                           Z( JROW, INCOL+1+J2 ), LDZ,
     $                           U( J2+1, I2+1 ), LDU, ONE,
     $                           WV( 1, 1+I2 ), LDWV )
*
*                    ==== Copy the result back to Z ====
*
                     CALL DLACPY( 'ALL', JLEN, KDU, WV, LDWV,
     $                            Z( JROW, INCOL+1 ), LDZ )
  210             CONTINUE
               END IF
            END IF
         END IF
  220 CONTINUE
*
*     ==== End of DLAQR5 ====
*
      END
      SUBROUTINE DLARRA( N, D, E, E2, SPLTOL, TNRM,
     $                    NSPLIT, ISPLIT, INFO )
      IMPLICIT NONE
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, N, NSPLIT
      DOUBLE PRECISION    SPLTOL, TNRM
*     ..
*     .. Array Arguments ..
      INTEGER            ISPLIT( * )
      DOUBLE PRECISION   D( * ), E( * ), E2( * )
*     ..
*
*  Purpose
*  =======
*
*  Compute the splitting points with threshold SPLTOL.
*  DLARRA sets any "small" off-diagonal elements to zero.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix. N > 0.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          On entry, the N diagonal elements of the tridiagonal
*          matrix T.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the first (N-1) entries contain the subdiagonal
*          elements of the tridiagonal matrix T; E(N) need not be set.
*          On exit, the entries E( ISPLIT( I ) ), 1 <= I <= NSPLIT,
*          are set to zero, the other entries of E are untouched.
*
*  E2      (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the first (N-1) entries contain the SQUARES of the
*          subdiagonal elements of the tridiagonal matrix T;
*          E2(N) need not be set.
*          On exit, the entries E2( ISPLIT( I ) ),
*          1 <= I <= NSPLIT, have been set to zero
*
*  SPLTOL (input) DOUBLE PRECISION
*          The threshold for splitting. Two criteria can be used:
*          SPLTOL<0 : criterion based on absolute off-diagonal value
*          SPLTOL>0 : criterion that preserves relative accuracy
*
*  TNRM (input) DOUBLE PRECISION
*          The norm of the matrix.
*
*  NSPLIT  (output) INTEGER
*          The number of blocks T splits into. 1 <= NSPLIT <= N.
*
*  ISPLIT  (output) INTEGER array, dimension (N)
*          The splitting points, at which T breaks up into blocks.
*          The first block consists of rows/columns 1 to ISPLIT(1),
*          the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
*          etc., and the NSPLIT-th consists of rows/columns
*          ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
*
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Beresford Parlett, University of California, Berkeley, USA
*     Jim Demmel, University of California, Berkeley, USA
*     Inderjit Dhillon, University of Texas, Austin, USA
*     Osni Marques, LBNL/NERSC, USA
*     Christof Voemel, University of California, Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   EABS, TMP1

*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      INFO = 0

*     Compute splitting points
      NSPLIT = 1
      IF(SPLTOL.LT.ZERO) THEN
*        Criterion based on absolute off-diagonal value
         TMP1 = ABS(SPLTOL)* TNRM
         DO 9 I = 1, N-1
            EABS = ABS( E(I) )
            IF( EABS .LE. TMP1) THEN
               E(I) = ZERO
               E2(I) = ZERO
               ISPLIT( NSPLIT ) = I
               NSPLIT = NSPLIT + 1
            END IF
 9       CONTINUE
      ELSE
*        Criterion that guarantees relative accuracy
         DO 10 I = 1, N-1
            EABS = ABS( E(I) )
            IF( EABS .LE. SPLTOL * SQRT(ABS(D(I)))*SQRT(ABS(D(I+1))) )
     $      THEN
               E(I) = ZERO
               E2(I) = ZERO
               ISPLIT( NSPLIT ) = I
               NSPLIT = NSPLIT + 1
            END IF
 10      CONTINUE
      ENDIF
      ISPLIT( NSPLIT ) = N

      RETURN
*
*     End of DLARRA
*
      END
      SUBROUTINE DLARRC( JOBT, N, VL, VU, D, E, PIVMIN,
     $                            EIGCNT, LCNT, RCNT, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBT
      INTEGER            EIGCNT, INFO, LCNT, N, RCNT
      DOUBLE PRECISION   PIVMIN, VL, VU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
*     ..
*
*  Purpose
*  =======
*
*  Find the number of eigenvalues of the symmetric tridiagonal matrix T
*  that are in the interval (VL,VU] if JOBT = 'T', and of L D L^T
*  if JOBT = 'L'.
*
*  Arguments
*  =========
*
*  JOBT    (input) CHARACTER*1
*          = 'T':  Compute Sturm count for matrix T.
*          = 'L':  Compute Sturm count for matrix L D L^T.
*
*  N       (input) INTEGER
*          The order of the matrix. N > 0.
*
*  VL      (input) DOUBLE PRECISION
*  VU      (input) DOUBLE PRECISION
*          The lower and upper bounds for the eigenvalues.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          JOBT = 'T': The N diagonal elements of the tridiagonal matrix T.
*          JOBT = 'L': The N diagonal elements of the diagonal matrix D.
*
*  E       (input) DOUBLE PRECISION array, dimension (N)
*          JOBT = 'T': The N-1 offdiagonal elements of the matrix T.
*          JOBT = 'L': The N-1 offdiagonal elements of the matrix L.
*
*  PIVMIN  (input) DOUBLE PRECISION
*          The minimum pivot in the Sturm sequence for T.
*
*  EIGCNT  (output) INTEGER
*          The number of eigenvalues of the symmetric tridiagonal matrix T
*          that are in the interval (VL,VU]
*
*  LCNT    (output) INTEGER
*  RCNT    (output) INTEGER
*          The left and right negcounts of the interval.
*
*  INFO    (output) INTEGER
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Beresford Parlett, University of California, Berkeley, USA
*     Jim Demmel, University of California, Berkeley, USA
*     Inderjit Dhillon, University of Texas, Austin, USA
*     Osni Marques, LBNL/NERSC, USA
*     Christof Voemel, University of California, Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      LOGICAL            MATT
      DOUBLE PRECISION   LPIVOT, RPIVOT, SL, SU, TMP, TMP2

*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      LCNT = 0
      RCNT = 0
      EIGCNT = 0
      MATT = LSAME( JOBT, 'T' )


      IF (MATT) THEN
*        Sturm sequence count on T
         LPIVOT = D( 1 ) - VL
         RPIVOT = D( 1 ) - VU
         IF( LPIVOT.LE.ZERO ) THEN
            LCNT = LCNT + 1
         ENDIF
         IF( RPIVOT.LE.ZERO ) THEN
            RCNT = RCNT + 1
         ENDIF
         DO 10 I = 1, N-1
            TMP = E(I)**2
            LPIVOT = ( D( I+1 )-VL ) - TMP/LPIVOT
            RPIVOT = ( D( I+1 )-VU ) - TMP/RPIVOT
            IF( LPIVOT.LE.ZERO ) THEN
               LCNT = LCNT + 1
            ENDIF
            IF( RPIVOT.LE.ZERO ) THEN
               RCNT = RCNT + 1
            ENDIF
 10      CONTINUE
      ELSE
*        Sturm sequence count on L D L^T
         SL = -VL
         SU = -VU
         DO 20 I = 1, N - 1
            LPIVOT = D( I ) + SL
            RPIVOT = D( I ) + SU
            IF( LPIVOT.LE.ZERO ) THEN
               LCNT = LCNT + 1
            ENDIF
            IF( RPIVOT.LE.ZERO ) THEN
               RCNT = RCNT + 1
            ENDIF
            TMP = E(I) * D(I) * E(I)
*
            TMP2 = TMP / LPIVOT
            IF( TMP2.EQ.ZERO ) THEN
               SL =  TMP - VL
            ELSE
               SL = SL*TMP2 - VL
            END IF
*
            TMP2 = TMP / RPIVOT
            IF( TMP2.EQ.ZERO ) THEN
               SU =  TMP - VU
            ELSE
               SU = SU*TMP2 - VU
            END IF
 20      CONTINUE
         LPIVOT = D( N ) + SL
         RPIVOT = D( N ) + SU
         IF( LPIVOT.LE.ZERO ) THEN
            LCNT = LCNT + 1
         ENDIF
         IF( RPIVOT.LE.ZERO ) THEN
            RCNT = RCNT + 1
         ENDIF
      ENDIF
      EIGCNT = RCNT - LCNT

      RETURN
*
*     end of DLARRC
*
      END
      SUBROUTINE DLARRD( RANGE, ORDER, N, VL, VU, IL, IU, GERS,
     $                    RELTOL, D, E, E2, PIVMIN, NSPLIT, ISPLIT,
     $                    M, W, WERR, WL, WU, IBLOCK, INDEXW,
     $                    WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          ORDER, RANGE
      INTEGER            IL, INFO, IU, M, N, NSPLIT
      DOUBLE PRECISION    PIVMIN, RELTOL, VL, VU, WL, WU
*     ..
*     .. Array Arguments ..
      INTEGER            IBLOCK( * ), INDEXW( * ),
     $                   ISPLIT( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), E2( * ),
     $                   GERS( * ), W( * ), WERR( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARRD computes the eigenvalues of a symmetric tridiagonal
*  matrix T to suitable accuracy. This is an auxiliary code to be
*  called from DSTEMR.
*  The user may ask for all eigenvalues, all eigenvalues
*  in the half-open interval (VL, VU], or the IL-th through IU-th
*  eigenvalues.
*
*  To avoid overflow, the matrix must be scaled so that its
*  largest element is no greater than overflow**(1/2) *
*  underflow**(1/4) in absolute value, and for greatest
*  accuracy, it should not be much smaller than that.
*
*  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
*  Matrix", Report CS41, Computer Science Dept., Stanford
*  University, July 21, 1966.
*
*  Arguments
*  =========
*
*  RANGE   (input) CHARACTER
*          = 'A': ("All")   all eigenvalues will be found.
*          = 'V': ("Value") all eigenvalues in the half-open interval
*                           (VL, VU] will be found.
*          = 'I': ("Index") the IL-th through IU-th eigenvalues (of the
*                           entire matrix) will be found.
*
*  ORDER   (input) CHARACTER
*          = 'B': ("By Block") the eigenvalues will be grouped by
*                              split-off block (see IBLOCK, ISPLIT) and
*                              ordered from smallest to largest within
*                              the block.
*          = 'E': ("Entire matrix")
*                              the eigenvalues for the entire matrix
*                              will be ordered from smallest to
*                              largest.
*
*  N       (input) INTEGER
*          The order of the tridiagonal matrix T.  N >= 0.
*
*  VL      (input) DOUBLE PRECISION
*  VU      (input) DOUBLE PRECISION
*          If RANGE='V', the lower and upper bounds of the interval to
*          be searched for eigenvalues.  Eigenvalues less than or equal
*          to VL, or greater than VU, will not be returned.  VL < VU.
*          Not referenced if RANGE = 'A' or 'I'.
*
*  IL      (input) INTEGER
*  IU      (input) INTEGER
*          If RANGE='I', the indices (in ascending order) of the
*          smallest and largest eigenvalues to be returned.
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  GERS    (input) DOUBLE PRECISION array, dimension (2*N)
*          The N Gerschgorin intervals (the i-th Gerschgorin interval
*          is (GERS(2*i-1), GERS(2*i)).
*
*  RELTOL  (input) DOUBLE PRECISION
*          The minimum relative width of an interval.  When an interval
*          is narrower than RELTOL times the larger (in
*          magnitude) endpoint, then it is considered to be
*          sufficiently small, i.e., converged.  Note: this should
*          always be at least radix*machine epsilon.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The n diagonal elements of the tridiagonal matrix T.
*
*  E       (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) off-diagonal elements of the tridiagonal matrix T.
*
*  E2      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) squared off-diagonal elements of the tridiagonal matrix T.
*
*  PIVMIN  (input) DOUBLE PRECISION
*          The minimum pivot allowed in the Sturm sequence for T.
*
*  NSPLIT  (input) INTEGER
*          The number of diagonal blocks in the matrix T.
*          1 <= NSPLIT <= N.
*
*  ISPLIT  (input) INTEGER array, dimension (N)
*          The splitting points, at which T breaks up into submatrices.
*          The first submatrix consists of rows/columns 1 to ISPLIT(1),
*          the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
*          etc., and the NSPLIT-th consists of rows/columns
*          ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
*          (Only the first NSPLIT elements will actually be used, but
*          since the user cannot know a priori what value NSPLIT will
*          have, N words must be reserved for ISPLIT.)
*
*  M       (output) INTEGER
*          The actual number of eigenvalues found. 0 <= M <= N.
*          (See also the description of INFO=2,3.)
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          On exit, the first M elements of W will contain the
*          eigenvalue approximations. DLARRD computes an interval
*          I_j = (a_j, b_j] that includes eigenvalue j. The eigenvalue
*          approximation is given as the interval midpoint
*          W(j)= ( a_j + b_j)/2. The corresponding error is bounded by
*          WERR(j) = abs( a_j - b_j)/2
*
*  WERR    (output) DOUBLE PRECISION array, dimension (N)
*          The error bound on the corresponding eigenvalue approximation
*          in W.
*
*  WL      (output) DOUBLE PRECISION
*  WU      (output) DOUBLE PRECISION
*          The interval (WL, WU] contains all the wanted eigenvalues.
*          If RANGE='V', then WL=VL and WU=VU.
*          If RANGE='A', then WL and WU are the global Gerschgorin bounds
*                        on the spectrum.
*          If RANGE='I', then WL and WU are computed by DLAEBZ from the
*                        index range specified.
*
*  IBLOCK  (output) INTEGER array, dimension (N)
*          At each row/column j where E(j) is zero or small, the
*          matrix T is considered to split into a block diagonal
*          matrix.  On exit, if INFO = 0, IBLOCK(i) specifies to which
*          block (from 1 to the number of blocks) the eigenvalue W(i)
*          belongs.  (DLARRD may use the remaining N-M elements as
*          workspace.)
*
*  INDEXW  (output) INTEGER array, dimension (N)
*          The indices of the eigenvalues within each block (submatrix);
*          for example, INDEXW(i)= j and IBLOCK(i)=k imply that the
*          i-th eigenvalue W(i) is the j-th eigenvalue in block k.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
*
*  IWORK   (workspace) INTEGER array, dimension (3*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  some or all of the eigenvalues failed to converge or
*                were not computed:
*                =1 or 3: Bisection failed to converge for some
*                        eigenvalues; these eigenvalues are flagged by a
*                        negative block number.  The effect is that the
*                        eigenvalues may not be as accurate as the
*                        absolute and relative tolerances.  This is
*                        generally caused by unexpectedly inaccurate
*                        arithmetic.
*                =2 or 3: RANGE='I' only: Not all of the eigenvalues
*                        IL:IU were found.
*                        Effect: M < IU+1-IL
*                        Cause:  non-monotonic arithmetic, causing the
*                                Sturm sequence to be non-monotonic.
*                        Cure:   recalculate, using RANGE='A', and pick
*                                out eigenvalues IL:IU.  In some cases,
*                                increasing the PARAMETER "FUDGE" may
*                                make things work.
*                = 4:    RANGE='I', and the Gershgorin interval
*                        initially used was too small.  No eigenvalues
*                        were computed.
*                        Probable cause: your machine has sloppy
*                                        floating-point arithmetic.
*                        Cure: Increase the PARAMETER "FUDGE",
*                              recompile, and try again.
*
*  Internal Parameters
*  ===================
*
*  FUDGE   DOUBLE PRECISION, default = 2
*          A "fudge factor" to widen the Gershgorin intervals.  Ideally,
*          a value of 1 should work, but on machines with sloppy
*          arithmetic, this needs to be larger.  The default for
*          publicly released versions should be large enough to handle
*          the worst machine around.  Note that this has no effect
*          on accuracy of the solution.
*
*  Based on contributions by
*     W. Kahan, University of California, Berkeley, USA
*     Beresford Parlett, University of California, Berkeley, USA
*     Jim Demmel, University of California, Berkeley, USA
*     Inderjit Dhillon, University of Texas, Austin, USA
*     Osni Marques, LBNL/NERSC, USA
*     Christof Voemel, University of California, Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, HALF, FUDGE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0,
     $                     TWO = 2.0D0, HALF = ONE/TWO,
     $                     FUDGE = TWO )
      INTEGER   ALLRNG, VALRNG, INDRNG
      PARAMETER ( ALLRNG = 1, VALRNG = 2, INDRNG = 3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NCNVRG, TOOFEW
      INTEGER            I, IB, IBEGIN, IDISCL, IDISCU, IE, IEND, IINFO,
     $                   IM, IN, IOFF, IOUT, IRANGE, ITMAX, ITMP1,
     $                   ITMP2, IW, IWOFF, J, JBLK, JDISC, JE, JEE, NB,
     $                   NWL, NWU
      DOUBLE PRECISION   ATOLI, EPS, GL, GU, RTOLI, SPDIAM, TMP1, TMP2,
     $                   TNORM, UFLOW, WKILL, WLU, WUL

*     ..
*     .. Local Arrays ..
      INTEGER            IDUMMA( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, ILAENV, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAEBZ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
*     Decode RANGE
*
      IF( LSAME( RANGE, 'A' ) ) THEN
         IRANGE = ALLRNG
      ELSE IF( LSAME( RANGE, 'V' ) ) THEN
         IRANGE = VALRNG
      ELSE IF( LSAME( RANGE, 'I' ) ) THEN
         IRANGE = INDRNG
      ELSE
         IRANGE = 0
      END IF
*
*     Check for Errors
*
      IF( IRANGE.LE.0 ) THEN
         INFO = -1
      ELSE IF( .NOT.(LSAME(ORDER,'B').OR.LSAME(ORDER,'E')) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( IRANGE.EQ.VALRNG ) THEN
         IF( VL.GE.VU )
     $      INFO = -5
      ELSE IF( IRANGE.EQ.INDRNG .AND.
     $        ( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) ) THEN
         INFO = -6
      ELSE IF( IRANGE.EQ.INDRNG .AND.
     $        ( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) ) THEN
         INFO = -7
      END IF
*
      IF( INFO.NE.0 ) THEN
         RETURN
      END IF

*     Initialize error flags
      INFO = 0
      NCNVRG = .FALSE.
      TOOFEW = .FALSE.

*     Quick return if possible
      M = 0
      IF( N.EQ.0 ) RETURN

*     Simplification:
      IF( IRANGE.EQ.INDRNG .AND. IL.EQ.1 .AND. IU.EQ.N ) IRANGE = 1

*     Get machine constants
      EPS = DLAMCH( 'P' )
      UFLOW = DLAMCH( 'U' )


*     Special Case when N=1
*     Treat case of 1x1 matrix for quick return
      IF( N.EQ.1 ) THEN
         IF( (IRANGE.EQ.ALLRNG).OR.
     $       ((IRANGE.EQ.VALRNG).AND.(D(1).GT.VL).AND.(D(1).LE.VU)).OR.
     $       ((IRANGE.EQ.INDRNG).AND.(IL.EQ.1).AND.(IU.EQ.1)) ) THEN
            M = 1
            W(1) = D(1)
*           The computation error of the eigenvalue is zero
            WERR(1) = ZERO
            IBLOCK( 1 ) = 1
            INDEXW( 1 ) = 1
         ENDIF
         RETURN
      END IF

*     NB is the minimum vector length for vector bisection, or 0
*     if only scalar is to be done.
      NB = ILAENV( 1, 'DSTEBZ', ' ', N, -1, -1, -1 )
      IF( NB.LE.1 ) NB = 0

*     Find global spectral radius
      GL = D(1)
      GU = D(1)
      DO 5 I = 1,N
         GL =  MIN( GL, GERS( 2*I - 1))
         GU = MAX( GU, GERS(2*I) )
 5    CONTINUE
*     Compute global Gerschgorin bounds and spectral diameter
      TNORM = MAX( ABS( GL ), ABS( GU ) )
      GL = GL - FUDGE*TNORM*EPS*N - FUDGE*TWO*PIVMIN
      GU = GU + FUDGE*TNORM*EPS*N + FUDGE*TWO*PIVMIN
      SPDIAM = GU - GL
*     Input arguments for DLAEBZ:
*     The relative tolerance.  An interval (a,b] lies within
*     "relative tolerance" if  b-a < RELTOL*max(|a|,|b|),
      RTOLI = RELTOL
*     Set the absolute tolerance for interval convergence to zero to force
*     interval convergence based on relative size of the interval.
*     This is dangerous because intervals might not converge when RELTOL is
*     small. But at least a very small number should be selected so that for
*     strongly graded matrices, the code can get relatively accurate
*     eigenvalues.
      ATOLI = FUDGE*TWO*UFLOW + FUDGE*TWO*PIVMIN

      IF( IRANGE.EQ.INDRNG ) THEN

*        RANGE='I': Compute an interval containing eigenvalues
*        IL through IU. The initial interval [GL,GU] from the global
*        Gerschgorin bounds GL and GU is refined by DLAEBZ.
         ITMAX = INT( ( LOG( TNORM+PIVMIN )-LOG( PIVMIN ) ) /
     $           LOG( TWO ) ) + 2
         WORK( N+1 ) = GL
         WORK( N+2 ) = GL
         WORK( N+3 ) = GU
         WORK( N+4 ) = GU
         WORK( N+5 ) = GL
         WORK( N+6 ) = GU
         IWORK( 1 ) = -1
         IWORK( 2 ) = -1
         IWORK( 3 ) = N + 1
         IWORK( 4 ) = N + 1
         IWORK( 5 ) = IL - 1
         IWORK( 6 ) = IU
*
         CALL DLAEBZ( 3, ITMAX, N, 2, 2, NB, ATOLI, RTOLI, PIVMIN,
     $         D, E, E2, IWORK( 5 ), WORK( N+1 ), WORK( N+5 ), IOUT,
     $                IWORK, W, IBLOCK, IINFO )
         IF( IINFO .NE. 0 ) THEN
            INFO = IINFO
            RETURN
         END IF
*        On exit, output intervals may not be ordered by ascending negcount
         IF( IWORK( 6 ).EQ.IU ) THEN
            WL = WORK( N+1 )
            WLU = WORK( N+3 )
            NWL = IWORK( 1 )
            WU = WORK( N+4 )
            WUL = WORK( N+2 )
            NWU = IWORK( 4 )
         ELSE
            WL = WORK( N+2 )
            WLU = WORK( N+4 )
            NWL = IWORK( 2 )
            WU = WORK( N+3 )
            WUL = WORK( N+1 )
            NWU = IWORK( 3 )
         END IF
*        On exit, the interval [WL, WLU] contains a value with negcount NWL,
*        and [WUL, WU] contains a value with negcount NWU.
         IF( NWL.LT.0 .OR. NWL.GE.N .OR. NWU.LT.1 .OR. NWU.GT.N ) THEN
            INFO = 4
            RETURN
         END IF

      ELSEIF( IRANGE.EQ.VALRNG ) THEN
         WL = VL
         WU = VU

      ELSEIF( IRANGE.EQ.ALLRNG ) THEN
         WL = GL
         WU = GU
      ENDIF



*     Find Eigenvalues -- Loop Over blocks and recompute NWL and NWU.
*     NWL accumulates the number of eigenvalues .le. WL,
*     NWU accumulates the number of eigenvalues .le. WU
      M = 0
      IEND = 0
      INFO = 0
      NWL = 0
      NWU = 0
*
      DO 70 JBLK = 1, NSPLIT
         IOFF = IEND
         IBEGIN = IOFF + 1
         IEND = ISPLIT( JBLK )
         IN = IEND - IOFF
*
         IF( IN.EQ.1 ) THEN
*           1x1 block
            IF( WL.GE.D( IBEGIN )-PIVMIN )
     $         NWL = NWL + 1
            IF( WU.GE.D( IBEGIN )-PIVMIN )
     $         NWU = NWU + 1
            IF( IRANGE.EQ.ALLRNG .OR.
     $           ( WL.LT.D( IBEGIN )-PIVMIN
     $             .AND. WU.GE. D( IBEGIN )-PIVMIN ) ) THEN
               M = M + 1
               W( M ) = D( IBEGIN )
               WERR(M) = ZERO
*              The gap for a single block doesn't matter for the later
*              algorithm and is assigned an arbitrary large value
               IBLOCK( M ) = JBLK
               INDEXW( M ) = 1
            END IF

*        Disabled 2x2 case because of a failure on the following matrix
*        RANGE = 'I', IL = IU = 4
*          Original Tridiagonal, d = [
*           -0.150102010615740E+00
*           -0.849897989384260E+00
*           -0.128208148052635E-15
*            0.128257718286320E-15
*          ];
*          e = [
*           -0.357171383266986E+00
*           -0.180411241501588E-15
*           -0.175152352710251E-15
*          ];
*
*         ELSE IF( IN.EQ.2 ) THEN
**           2x2 block
*            DISC = SQRT( (HALF*(D(IBEGIN)-D(IEND)))**2 + E(IBEGIN)**2 )
*            TMP1 = HALF*(D(IBEGIN)+D(IEND))
*            L1 = TMP1 - DISC
*            IF( WL.GE. L1-PIVMIN )
*     $         NWL = NWL + 1
*            IF( WU.GE. L1-PIVMIN )
*     $         NWU = NWU + 1
*            IF( IRANGE.EQ.ALLRNG .OR. ( WL.LT.L1-PIVMIN .AND. WU.GE.
*     $          L1-PIVMIN ) ) THEN
*               M = M + 1
*               W( M ) = L1
**              The uncertainty of eigenvalues of a 2x2 matrix is very small
*               WERR( M ) = EPS * ABS( W( M ) ) * TWO
*               IBLOCK( M ) = JBLK
*               INDEXW( M ) = 1
*            ENDIF
*            L2 = TMP1 + DISC
*            IF( WL.GE. L2-PIVMIN )
*     $         NWL = NWL + 1
*            IF( WU.GE. L2-PIVMIN )
*     $         NWU = NWU + 1
*            IF( IRANGE.EQ.ALLRNG .OR. ( WL.LT.L2-PIVMIN .AND. WU.GE.
*     $          L2-PIVMIN ) ) THEN
*               M = M + 1
*               W( M ) = L2
**              The uncertainty of eigenvalues of a 2x2 matrix is very small
*               WERR( M ) = EPS * ABS( W( M ) ) * TWO
*               IBLOCK( M ) = JBLK
*               INDEXW( M ) = 2
*            ENDIF
         ELSE
*           General Case - block of size IN >= 2
*           Compute local Gerschgorin interval and use it as the initial
*           interval for DLAEBZ
            GU = D( IBEGIN )
            GL = D( IBEGIN )
            TMP1 = ZERO

            DO 40 J = IBEGIN, IEND
               GL =  MIN( GL, GERS( 2*J - 1))
               GU = MAX( GU, GERS(2*J) )
   40       CONTINUE
            SPDIAM = GU - GL
            GL = GL - FUDGE*SPDIAM*EPS*IN - FUDGE*PIVMIN
            GU = GU + FUDGE*SPDIAM*EPS*IN + FUDGE*PIVMIN
*
            IF( IRANGE.GT.1 ) THEN
               IF( GU.LT.WL ) THEN
*                 the local block contains none of the wanted eigenvalues
                  NWL = NWL + IN
                  NWU = NWU + IN
                  GO TO 70
               END IF
*              refine search interval if possible, only range (WL,WU] matters
               GL = MAX( GL, WL )
               GU = MIN( GU, WU )
               IF( GL.GE.GU )
     $            GO TO 70
            END IF

*           Find negcount of initial interval boundaries GL and GU
            WORK( N+1 ) = GL
            WORK( N+IN+1 ) = GU
            CALL DLAEBZ( 1, 0, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN,
     $                   D( IBEGIN ), E( IBEGIN ), E2( IBEGIN ),
     $                   IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IM,
     $                   IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
            IF( IINFO .NE. 0 ) THEN
               INFO = IINFO
               RETURN
            END IF
*
            NWL = NWL + IWORK( 1 )
            NWU = NWU + IWORK( IN+1 )
            IWOFF = M - IWORK( 1 )

*           Compute Eigenvalues
            ITMAX = INT( ( LOG( GU-GL+PIVMIN )-LOG( PIVMIN ) ) /
     $              LOG( TWO ) ) + 2
            CALL DLAEBZ( 2, ITMAX, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN,
     $                   D( IBEGIN ), E( IBEGIN ), E2( IBEGIN ),
     $                   IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IOUT,
     $                   IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
            IF( IINFO .NE. 0 ) THEN
               INFO = IINFO
               RETURN
            END IF
*
*           Copy eigenvalues into W and IBLOCK
*           Use -JBLK for block number for unconverged eigenvalues.
*           Loop over the number of output intervals from DLAEBZ
            DO 60 J = 1, IOUT
*              eigenvalue approximation is middle point of interval
               TMP1 = HALF*( WORK( J+N )+WORK( J+IN+N ) )
*              semi length of error interval
               TMP2 = HALF*ABS( WORK( J+N )-WORK( J+IN+N ) )
               IF( J.GT.IOUT-IINFO ) THEN
*                 Flag non-convergence.
                  NCNVRG = .TRUE.
                  IB = -JBLK
               ELSE
                  IB = JBLK
               END IF
               DO 50 JE = IWORK( J ) + 1 + IWOFF,
     $                 IWORK( J+IN ) + IWOFF
                  W( JE ) = TMP1
                  WERR( JE ) = TMP2
                  INDEXW( JE ) = JE - IWOFF
                  IBLOCK( JE ) = IB
   50          CONTINUE
   60       CONTINUE
*
            M = M + IM
         END IF
   70 CONTINUE

*     If RANGE='I', then (WL,WU) contains eigenvalues NWL+1,...,NWU
*     If NWL+1 < IL or NWU > IU, discard extra eigenvalues.
      IF( IRANGE.EQ.INDRNG ) THEN
         IDISCL = IL - 1 - NWL
         IDISCU = NWU - IU
*
         IF( IDISCL.GT.0 ) THEN
            IM = 0
            DO 80 JE = 1, M
*              Remove some of the smallest eigenvalues from the left so that
*              at the end IDISCL =0. Move all eigenvalues up to the left.
               IF( W( JE ).LE.WLU .AND. IDISCL.GT.0 ) THEN
                  IDISCL = IDISCL - 1
               ELSE
                  IM = IM + 1
                  W( IM ) = W( JE )
                  WERR( IM ) = WERR( JE )
                  INDEXW( IM ) = INDEXW( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               END IF
 80         CONTINUE
            M = IM
         END IF
         IF( IDISCU.GT.0 ) THEN
*           Remove some of the largest eigenvalues from the right so that
*           at the end IDISCU =0. Move all eigenvalues up to the left.
            IM=M+1
            DO 81 JE = M, 1, -1
               IF( W( JE ).GE.WUL .AND. IDISCU.GT.0 ) THEN
                  IDISCU = IDISCU - 1
               ELSE
                  IM = IM - 1
                  W( IM ) = W( JE )
                  WERR( IM ) = WERR( JE )
                  INDEXW( IM ) = INDEXW( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               END IF
 81         CONTINUE
            JEE = 0
            DO 82 JE = IM, M
               JEE = JEE + 1
               W( JEE ) = W( JE )
               WERR( JEE ) = WERR( JE )
               INDEXW( JEE ) = INDEXW( JE )
               IBLOCK( JEE ) = IBLOCK( JE )
 82         CONTINUE
            M = M-IM+1
         END IF

         IF( IDISCL.GT.0 .OR. IDISCU.GT.0 ) THEN
*           Code to deal with effects of bad arithmetic. (If N(w) is
*           monotone non-decreasing, this should never happen.)
*           Some low eigenvalues to be discarded are not in (WL,WLU],
*           or high eigenvalues to be discarded are not in (WUL,WU]
*           so just kill off the smallest IDISCL/largest IDISCU
*           eigenvalues, by marking the corresponding IBLOCK = 0
            IF( IDISCL.GT.0 ) THEN
               WKILL = WU
               DO 100 JDISC = 1, IDISCL
                  IW = 0
                  DO 90 JE = 1, M
                     IF( IBLOCK( JE ).NE.0 .AND.
     $                    ( W( JE ).LT.WKILL .OR. IW.EQ.0 ) ) THEN
                        IW = JE
                        WKILL = W( JE )
                     END IF
 90               CONTINUE
                  IBLOCK( IW ) = 0
 100           CONTINUE
            END IF
            IF( IDISCU.GT.0 ) THEN
               WKILL = WL
               DO 120 JDISC = 1, IDISCU
                  IW = 0
                  DO 110 JE = 1, M
                     IF( IBLOCK( JE ).NE.0 .AND.
     $                    ( W( JE ).GE.WKILL .OR. IW.EQ.0 ) ) THEN
                        IW = JE
                        WKILL = W( JE )
                     END IF
 110              CONTINUE
                  IBLOCK( IW ) = 0
 120           CONTINUE
            END IF
*           Now erase all eigenvalues with IBLOCK set to zero
            IM = 0
            DO 130 JE = 1, M
               IF( IBLOCK( JE ).NE.0 ) THEN
                  IM = IM + 1
                  W( IM ) = W( JE )
                  WERR( IM ) = WERR( JE )
                  INDEXW( IM ) = INDEXW( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               END IF
 130        CONTINUE
            M = IM
         END IF
         IF( IDISCL.LT.0 .OR. IDISCU.LT.0 ) THEN
            TOOFEW = .TRUE.
         END IF
      END IF
*
      IF(( IRANGE.EQ.ALLRNG .AND. M.NE.N ).OR.
     $   ( IRANGE.EQ.INDRNG .AND. M.NE.IU-IL+1 ) ) THEN
         TOOFEW = .TRUE.
      END IF

*     If ORDER='B', do nothing the eigenvalues are already sorted by
*        block.
*     If ORDER='E', sort the eigenvalues from smallest to largest

      IF( LSAME(ORDER,'E') .AND. NSPLIT.GT.1 ) THEN
         DO 150 JE = 1, M - 1
            IE = 0
            TMP1 = W( JE )
            DO 140 J = JE + 1, M
               IF( W( J ).LT.TMP1 ) THEN
                  IE = J
                  TMP1 = W( J )
               END IF
  140       CONTINUE
            IF( IE.NE.0 ) THEN
               TMP2 = WERR( IE )
               ITMP1 = IBLOCK( IE )
               ITMP2 = INDEXW( IE )
               W( IE ) = W( JE )
               WERR( IE ) = WERR( JE )
               IBLOCK( IE ) = IBLOCK( JE )
               INDEXW( IE ) = INDEXW( JE )
               W( JE ) = TMP1
               WERR( JE ) = TMP2
               IBLOCK( JE ) = ITMP1
               INDEXW( JE ) = ITMP2
            END IF
  150    CONTINUE
      END IF
*
      INFO = 0
      IF( NCNVRG )
     $   INFO = INFO + 1
      IF( TOOFEW )
     $   INFO = INFO + 2
      RETURN
*
*     End of DLARRD
*
      END
      SUBROUTINE DLARRJ( N, D, E2, IFIRST, ILAST,
     $                   RTOL, OFFSET, W, WERR, WORK, IWORK,
     $                   PIVMIN, SPDIAM, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IFIRST, ILAST, INFO, N, OFFSET
      DOUBLE PRECISION   PIVMIN, RTOL, SPDIAM
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E2( * ), W( * ),
     $                   WERR( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  Given the initial eigenvalue approximations of T, DLARRJ
*  does  bisection to refine the eigenvalues of T,
*  W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
*  guesses for these eigenvalues are input in W, the corresponding estimate
*  of the error in these guesses in WERR. During bisection, intervals
*  [left, right] are maintained by storing their mid-points and
*  semi-widths in the arrays W and WERR respectively.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The N diagonal elements of T.
*
*  E2      (input) DOUBLE PRECISION array, dimension (N-1)
*          The Squares of the (N-1) subdiagonal elements of T.
*
*  IFIRST  (input) INTEGER
*          The index of the first eigenvalue to be computed.
*
*  ILAST   (input) INTEGER
*          The index of the last eigenvalue to be computed.
*
*  RTOL   (input) DOUBLE PRECISION
*          Tolerance for the convergence of the bisection intervals.
*          An interval [LEFT,RIGHT] has converged if
*          RIGHT-LEFT.LT.RTOL*MAX(|LEFT|,|RIGHT|).
*
*  OFFSET  (input) INTEGER
*          Offset for the arrays W and WERR, i.e., the IFIRST-OFFSET
*          through ILAST-OFFSET elements of these arrays are to be used.
*
*  W       (input/output) DOUBLE PRECISION array, dimension (N)
*          On input, W( IFIRST-OFFSET ) through W( ILAST-OFFSET ) are
*          estimates of the eigenvalues of L D L^T indexed IFIRST through
*          ILAST.
*          On output, these estimates are refined.
*
*  WERR    (input/output) DOUBLE PRECISION array, dimension (N)
*          On input, WERR( IFIRST-OFFSET ) through WERR( ILAST-OFFSET ) are
*          the errors in the estimates of the corresponding elements in W.
*          On output, these errors are refined.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
*          Workspace.
*
*  IWORK   (workspace) INTEGER array, dimension (2*N)
*          Workspace.
*
*  PIVMIN  (input) DOUBLE PRECISION
*          The minimum pivot in the Sturm sequence for T.
*
*  SPDIAM  (input) DOUBLE PRECISION
*          The spectral diameter of T.
*
*  INFO    (output) INTEGER
*          Error flag.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Beresford Parlett, University of California, Berkeley, USA
*     Jim Demmel, University of California, Berkeley, USA
*     Inderjit Dhillon, University of Texas, Austin, USA
*     Osni Marques, LBNL/NERSC, USA
*     Christof Voemel, University of California, Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, HALF
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                   HALF = 0.5D0 )
      INTEGER   MAXITR
*     ..
*     .. Local Scalars ..
      INTEGER            CNT, I, I1, I2, II, ITER, J, K, NEXT, NINT,
     $                   OLNINT, P, PREV, SAVI1
      DOUBLE PRECISION   DPLUS, FAC, LEFT, MID, RIGHT, S, TMP, WIDTH
*
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
      MAXITR = INT( ( LOG( SPDIAM+PIVMIN )-LOG( PIVMIN ) ) /
     $           LOG( TWO ) ) + 2
*
*     Initialize unconverged intervals in [ WORK(2*I-1), WORK(2*I) ].
*     The Sturm Count, Count( WORK(2*I-1) ) is arranged to be I-1, while
*     Count( WORK(2*I) ) is stored in IWORK( 2*I ). The integer IWORK( 2*I-1 )
*     for an unconverged interval is set to the index of the next unconverged
*     interval, and is -1 or 0 for a converged interval. Thus a linked
*     list of unconverged intervals is set up.
*

      I1 = IFIRST
      I2 = ILAST
*     The number of unconverged intervals
      NINT = 0
*     The last unconverged interval found
      PREV = 0
      DO 75 I = I1, I2
         K = 2*I
         II = I - OFFSET
         LEFT = W( II ) - WERR( II )
         MID = W(II)
         RIGHT = W( II ) + WERR( II )
         WIDTH = RIGHT - MID
         TMP = MAX( ABS( LEFT ), ABS( RIGHT ) )

*        The following test prevents the test of converged intervals
         IF( WIDTH.LT.RTOL*TMP ) THEN
*           This interval has already converged and does not need refinement.
*           (Note that the gaps might change through refining the
*            eigenvalues, however, they can only get bigger.)
*           Remove it from the list.
            IWORK( K-1 ) = -1
*           Make sure that I1 always points to the first unconverged interval
            IF((I.EQ.I1).AND.(I.LT.I2)) I1 = I + 1
            IF((PREV.GE.I1).AND.(I.LE.I2)) IWORK( 2*PREV-1 ) = I + 1
         ELSE
*           unconverged interval found
            PREV = I
*           Make sure that [LEFT,RIGHT] contains the desired eigenvalue
*
*           Do while( CNT(LEFT).GT.I-1 )
*
            FAC = ONE
 20         CONTINUE
            CNT = 0
            S = LEFT
            DPLUS = D( 1 ) - S
            IF( DPLUS.LT.ZERO ) CNT = CNT + 1
            DO 30 J = 2, N
               DPLUS = D( J ) - S - E2( J-1 )/DPLUS
               IF( DPLUS.LT.ZERO ) CNT = CNT + 1
 30         CONTINUE
            IF( CNT.GT.I-1 ) THEN
               LEFT = LEFT - WERR( II )*FAC
               FAC = TWO*FAC
               GO TO 20
            END IF
*
*           Do while( CNT(RIGHT).LT.I )
*
            FAC = ONE
 50         CONTINUE
            CNT = 0
            S = RIGHT
            DPLUS = D( 1 ) - S
            IF( DPLUS.LT.ZERO ) CNT = CNT + 1
            DO 60 J = 2, N
               DPLUS = D( J ) - S - E2( J-1 )/DPLUS
               IF( DPLUS.LT.ZERO ) CNT = CNT + 1
 60         CONTINUE
            IF( CNT.LT.I ) THEN
               RIGHT = RIGHT + WERR( II )*FAC
               FAC = TWO*FAC
               GO TO 50
            END IF
            NINT = NINT + 1
            IWORK( K-1 ) = I + 1
            IWORK( K ) = CNT
         END IF
         WORK( K-1 ) = LEFT
         WORK( K ) = RIGHT
 75   CONTINUE


      SAVI1 = I1
*
*     Do while( NINT.GT.0 ), i.e. there are still unconverged intervals
*     and while (ITER.LT.MAXITR)
*
      ITER = 0
 80   CONTINUE
      PREV = I1 - 1
      I = I1
      OLNINT = NINT

      DO 100 P = 1, OLNINT
         K = 2*I
         II = I - OFFSET
         NEXT = IWORK( K-1 )
         LEFT = WORK( K-1 )
         RIGHT = WORK( K )
         MID = HALF*( LEFT + RIGHT )

*        semiwidth of interval
         WIDTH = RIGHT - MID
         TMP = MAX( ABS( LEFT ), ABS( RIGHT ) )

         IF( ( WIDTH.LT.RTOL*TMP ) .OR.
     $      (ITER.EQ.MAXITR) )THEN
*           reduce number of unconverged intervals
            NINT = NINT - 1
*           Mark interval as converged.
            IWORK( K-1 ) = 0
            IF( I1.EQ.I ) THEN
               I1 = NEXT
            ELSE
*              Prev holds the last unconverged interval previously examined
               IF(PREV.GE.I1) IWORK( 2*PREV-1 ) = NEXT
            END IF
            I = NEXT
            GO TO 100
         END IF
         PREV = I
*
*        Perform one bisection step
*
         CNT = 0
         S = MID
         DPLUS = D( 1 ) - S
         IF( DPLUS.LT.ZERO ) CNT = CNT + 1
         DO 90 J = 2, N
            DPLUS = D( J ) - S - E2( J-1 )/DPLUS
            IF( DPLUS.LT.ZERO ) CNT = CNT + 1
 90      CONTINUE
         IF( CNT.LE.I-1 ) THEN
            WORK( K-1 ) = MID
         ELSE
            WORK( K ) = MID
         END IF
         I = NEXT

 100  CONTINUE
      ITER = ITER + 1
*     do another loop if there are still unconverged intervals
*     However, in the last iteration, all intervals are accepted
*     since this is the best we can do.
      IF( ( NINT.GT.0 ).AND.(ITER.LE.MAXITR) ) GO TO 80
*
*
*     At this point, all the intervals have converged
      DO 110 I = SAVI1, ILAST
         K = 2*I
         II = I - OFFSET
*        All intervals marked by '0' have been refined.
         IF( IWORK( K-1 ).EQ.0 ) THEN
            W( II ) = HALF*( WORK( K-1 )+WORK( K ) )
            WERR( II ) = WORK( K ) - W( II )
         END IF
 110  CONTINUE
*

      RETURN
*
*     End of DLARRJ
*
      END
      SUBROUTINE DLARRK( N, IW, GL, GU,
     $                    D, E2, PIVMIN, RELTOL, W, WERR, INFO)
      IMPLICIT NONE
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER   INFO, IW, N
      DOUBLE PRECISION    PIVMIN, RELTOL, GL, GU, W, WERR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E2( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARRK computes one eigenvalue of a symmetric tridiagonal
*  matrix T to suitable accuracy. This is an auxiliary code to be
*  called from DSTEMR.
*
*  To avoid overflow, the matrix must be scaled so that its
*  largest element is no greater than overflow**(1/2) *
*  underflow**(1/4) in absolute value, and for greatest
*  accuracy, it should not be much smaller than that.
*
*  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
*  Matrix", Report CS41, Computer Science Dept., Stanford
*  University, July 21, 1966.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the tridiagonal matrix T.  N >= 0.
*
*  IW      (input) INTEGER
*          The index of the eigenvalues to be returned.
*
*  GL      (input) DOUBLE PRECISION
*  GU      (input) DOUBLE PRECISION
*          An upper and a lower bound on the eigenvalue.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The n diagonal elements of the tridiagonal matrix T.
*
*  E2      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) squared off-diagonal elements of the tridiagonal matrix T.
*
*  PIVMIN  (input) DOUBLE PRECISION
*          The minimum pivot allowed in the Sturm sequence for T.
*
*  RELTOL  (input) DOUBLE PRECISION
*          The minimum relative width of an interval.  When an interval
*          is narrower than RELTOL times the larger (in
*          magnitude) endpoint, then it is considered to be
*          sufficiently small, i.e., converged.  Note: this should
*          always be at least radix*machine epsilon.
*
*  W       (output) DOUBLE PRECISION
*
*  WERR    (output) DOUBLE PRECISION
*          The error bound on the corresponding eigenvalue approximation
*          in W.
*
*  INFO    (output) INTEGER
*          = 0:       Eigenvalue converged
*          = -1:      Eigenvalue did NOT converge
*
*  Internal Parameters
*  ===================
*
*  FUDGE   DOUBLE PRECISION, default = 2
*          A "fudge factor" to widen the Gershgorin intervals.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   FUDGE, HALF, TWO, ZERO
      PARAMETER          ( HALF = 0.5D0, TWO = 2.0D0,
     $                     FUDGE = TWO, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER   I, IT, ITMAX, NEGCNT
      DOUBLE PRECISION   ATOLI, EPS, LEFT, MID, RIGHT, RTOLI, TMP1,
     $                   TMP2, TNORM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL   DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX
*     ..
*     .. Executable Statements ..
*
*     Get machine constants
      EPS = DLAMCH( 'P' )

      TNORM = MAX( ABS( GL ), ABS( GU ) )
      RTOLI = RELTOL
      ATOLI = FUDGE*TWO*PIVMIN

      ITMAX = INT( ( LOG( TNORM+PIVMIN )-LOG( PIVMIN ) ) /
     $           LOG( TWO ) ) + 2

      INFO = -1

      LEFT = GL - FUDGE*TNORM*EPS*N - FUDGE*TWO*PIVMIN
      RIGHT = GU + FUDGE*TNORM*EPS*N + FUDGE*TWO*PIVMIN
      IT = 0

 10   CONTINUE
*
*     Check if interval converged or maximum number of iterations reached
*
      TMP1 = ABS( RIGHT - LEFT )
      TMP2 = MAX( ABS(RIGHT), ABS(LEFT) )
      IF( TMP1.LT.MAX( ATOLI, PIVMIN, RTOLI*TMP2 ) ) THEN
         INFO = 0
         GOTO 30
      ENDIF
      IF(IT.GT.ITMAX)
     $   GOTO 30

*
*     Count number of negative pivots for mid-point
*
      IT = IT + 1
      MID = HALF * (LEFT + RIGHT)
      NEGCNT = 0
      TMP1 = D( 1 ) - MID
      IF( ABS( TMP1 ).LT.PIVMIN )
     $   TMP1 = -PIVMIN
      IF( TMP1.LE.ZERO )
     $   NEGCNT = NEGCNT + 1
*
      DO 20 I = 2, N
         TMP1 = D( I ) - E2( I-1 ) / TMP1 - MID
         IF( ABS( TMP1 ).LT.PIVMIN )
     $      TMP1 = -PIVMIN
         IF( TMP1.LE.ZERO )
     $      NEGCNT = NEGCNT + 1
 20   CONTINUE

      IF(NEGCNT.GE.IW) THEN
         RIGHT = MID
      ELSE
         LEFT = MID
      ENDIF
      GOTO 10

 30   CONTINUE
*
*     Converged or maximum number of iterations reached
*
      W = HALF * (LEFT + RIGHT)
      WERR = HALF * ABS( RIGHT - LEFT )

      RETURN
*
*     End of DLARRK
*
      END
      SUBROUTINE DLARRR( N, D, E, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            N, INFO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
*     ..
*
*
*  Purpose
*  =======
*
*  Perform tests to decide whether the symmetric tridiagonal matrix T
*  warrants expensive computations which guarantee high relative accuracy
*  in the eigenvalues.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix. N > 0.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The N diagonal elements of the tridiagonal matrix T.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the first (N-1) entries contain the subdiagonal
*          elements of the tridiagonal matrix T; E(N) is set to ZERO.
*
*  INFO    (output) INTEGER
*          INFO = 0(default) : the matrix warrants computations preserving
*                              relative accuracy.
*          INFO = 1          : the matrix warrants computations guaranteeing
*                              only absolute accuracy.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Beresford Parlett, University of California, Berkeley, USA
*     Jim Demmel, University of California, Berkeley, USA
*     Inderjit Dhillon, University of Texas, Austin, USA
*     Osni Marques, LBNL/NERSC, USA
*     Christof Voemel, University of California, Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, RELCOND
      PARAMETER          ( ZERO = 0.0D0,
     $                     RELCOND = 0.999D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      LOGICAL            YESREL
      DOUBLE PRECISION   EPS, SAFMIN, SMLNUM, RMIN, TMP, TMP2,
     $          OFFDIG, OFFDIG2

*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
*     As a default, do NOT go for relative-accuracy preserving computations.
      INFO = 1

      SAFMIN = DLAMCH( 'Safe minimum' )
      EPS = DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      RMIN = SQRT( SMLNUM )

*     Tests for relative accuracy
*
*     Test for scaled diagonal dominance
*     Scale the diagonal entries to one and check whether the sum of the
*     off-diagonals is less than one
*
*     The sdd relative error bounds have a 1/(1- 2*x) factor in them,
*     x = max(OFFDIG + OFFDIG2), so when x is close to 1/2, no relative
*     accuracy is promised.  In the notation of the code fragment below,
*     1/(1 - (OFFDIG + OFFDIG2)) is the condition number.
*     We don't think it is worth going into "sdd mode" unless the relative
*     condition number is reasonable, not 1/macheps.
*     The threshold should be compatible with other thresholds used in the
*     code. We set  OFFDIG + OFFDIG2 <= .999 =: RELCOND, it corresponds
*     to losing at most 3 decimal digits: 1 / (1 - (OFFDIG + OFFDIG2)) <= 1000
*     instead of the current OFFDIG + OFFDIG2 < 1
*
      YESREL = .TRUE.
      OFFDIG = ZERO
      TMP = SQRT(ABS(D(1)))
      IF (TMP.LT.RMIN) YESREL = .FALSE.
      IF(.NOT.YESREL) GOTO 11
      DO 10 I = 2, N
         TMP2 = SQRT(ABS(D(I)))
         IF (TMP2.LT.RMIN) YESREL = .FALSE.
         IF(.NOT.YESREL) GOTO 11
         OFFDIG2 = ABS(E(I-1))/(TMP*TMP2)
         IF(OFFDIG+OFFDIG2.GE.RELCOND) YESREL = .FALSE.
         IF(.NOT.YESREL) GOTO 11
         TMP = TMP2
         OFFDIG = OFFDIG2
 10   CONTINUE
 11   CONTINUE

      IF( YESREL ) THEN
         INFO = 0
         RETURN
      ELSE
      ENDIF
*

*
*     *** MORE TO BE IMPLEMENTED ***
*

*
*     Test if the lower bidiagonal matrix L from T = L D L^T
*     (zero shift facto) is well conditioned
*

*
*     Test if the upper bidiagonal matrix U from T = U D U^T
*     (zero shift facto) is well conditioned.
*     In this case, the matrix needs to be flipped and, at the end
*     of the eigenvector computation, the flip needs to be applied
*     to the computed eigenvectors (and the support)
*

*
      RETURN
*
*     END OF DLARRR
*
      END
      SUBROUTINE DLAZQ3( I0, N0, Z, PP, DMIN, SIGMA, DESIG, QMAX, NFAIL,
     $                   ITER, NDIV, IEEE, TTYPE, DMIN1, DMIN2, DN, DN1,
     $                   DN2, TAU )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            I0, ITER, N0, NDIV, NFAIL, PP, TTYPE
      DOUBLE PRECISION   DESIG, DMIN, DMIN1, DMIN2, DN, DN1, DN2, QMAX,
     $                   SIGMA, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAZQ3 checks for deflation, computes a shift (TAU) and calls dqds.
*  In case of failure it changes shifts, and tries again until output
*  is positive.
*
*  Arguments
*  =========
*
*  I0     (input) INTEGER
*         First index.
*
*  N0     (input) INTEGER
*         Last index.
*
*  Z      (input) DOUBLE PRECISION array, dimension ( 4*N )
*         Z holds the qd array.
*
*  PP     (input) INTEGER
*         PP=0 for ping, PP=1 for pong.
*
*  DMIN   (output) DOUBLE PRECISION
*         Minimum value of d.
*
*  SIGMA  (output) DOUBLE PRECISION
*         Sum of shifts used in current segment.
*
*  DESIG  (input/output) DOUBLE PRECISION
*         Lower order part of SIGMA
*
*  QMAX   (input) DOUBLE PRECISION
*         Maximum value of q.
*
*  NFAIL  (output) INTEGER
*         Number of times shift was too big.
*
*  ITER   (output) INTEGER
*         Number of iterations.
*
*  NDIV   (output) INTEGER
*         Number of divisions.
*
*  IEEE   (input) LOGICAL
*         Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
*
*  TTYPE  (input/output) INTEGER
*         Shift type.  TTYPE is passed as an argument in order to save
*         its value between calls to DLAZQ3
*
*  DMIN1  (input/output) REAL
*  DMIN2  (input/output) REAL
*  DN     (input/output) REAL
*  DN1    (input/output) REAL
*  DN2    (input/output) REAL
*  TAU    (input/output) REAL
*         These are passed as arguments in order to save their values
*         between calls to DLAZQ3
*
*  This is a thread safe version of DLASQ3, which passes TTYPE, DMIN1,
*  DMIN2, DN, DN1. DN2 and TAU through the argument list in place of
*  declaring them in a SAVE statment.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   CBIAS
      PARAMETER          ( CBIAS = 1.50D0 )
      DOUBLE PRECISION   ZERO, QURTR, HALF, ONE, TWO, HUNDRD
      PARAMETER          ( ZERO = 0.0D0, QURTR = 0.250D0, HALF = 0.5D0,
     $                     ONE = 1.0D0, TWO = 2.0D0, HUNDRD = 100.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            IPN4, J4, N0IN, NN
      DOUBLE PRECISION   EPS, G, S, SAFMIN, T, TEMP, TOL, TOL2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASQ5, DLASQ6, DLAZQ4
*     ..
*     .. External Function ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      N0IN   = N0
      EPS    = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      TOL    = EPS*HUNDRD
      TOL2   = TOL**2
      G      = ZERO
*
*     Check for deflation.
*
   10 CONTINUE
*
      IF( N0.LT.I0 )
     $   RETURN
      IF( N0.EQ.I0 )
     $   GO TO 20
      NN = 4*N0 + PP
      IF( N0.EQ.( I0+1 ) )
     $   GO TO 40
*
*     Check whether E(N0-1) is negligible, 1 eigenvalue.
*
      IF( Z( NN-5 ).GT.TOL2*( SIGMA+Z( NN-3 ) ) .AND.
     $    Z( NN-2*PP-4 ).GT.TOL2*Z( NN-7 ) )
     $   GO TO 30
*
   20 CONTINUE
*
      Z( 4*N0-3 ) = Z( 4*N0+PP-3 ) + SIGMA
      N0 = N0 - 1
      GO TO 10
*
*     Check  whether E(N0-2) is negligible, 2 eigenvalues.
*
   30 CONTINUE
*
      IF( Z( NN-9 ).GT.TOL2*SIGMA .AND.
     $    Z( NN-2*PP-8 ).GT.TOL2*Z( NN-11 ) )
     $   GO TO 50
*
   40 CONTINUE
*
      IF( Z( NN-3 ).GT.Z( NN-7 ) ) THEN
         S = Z( NN-3 )
         Z( NN-3 ) = Z( NN-7 )
         Z( NN-7 ) = S
      END IF
      IF( Z( NN-5 ).GT.Z( NN-3 )*TOL2 ) THEN
         T = HALF*( ( Z( NN-7 )-Z( NN-3 ) )+Z( NN-5 ) )
         S = Z( NN-3 )*( Z( NN-5 ) / T )
         IF( S.LE.T ) THEN
            S = Z( NN-3 )*( Z( NN-5 ) /
     $          ( T*( ONE+SQRT( ONE+S / T ) ) ) )
         ELSE
            S = Z( NN-3 )*( Z( NN-5 ) / ( T+SQRT( T )*SQRT( T+S ) ) )
         END IF
         T = Z( NN-7 ) + ( S+Z( NN-5 ) )
         Z( NN-3 ) = Z( NN-3 )*( Z( NN-7 ) / T )
         Z( NN-7 ) = T
      END IF
      Z( 4*N0-7 ) = Z( NN-7 ) + SIGMA
      Z( 4*N0-3 ) = Z( NN-3 ) + SIGMA
      N0 = N0 - 2
      GO TO 10
*
   50 CONTINUE
*
*     Reverse the qd-array, if warranted.
*
      IF( DMIN.LE.ZERO .OR. N0.LT.N0IN ) THEN
         IF( CBIAS*Z( 4*I0+PP-3 ).LT.Z( 4*N0+PP-3 ) ) THEN
            IPN4 = 4*( I0+N0 )
            DO 60 J4 = 4*I0, 2*( I0+N0-1 ), 4
               TEMP = Z( J4-3 )
               Z( J4-3 ) = Z( IPN4-J4-3 )
               Z( IPN4-J4-3 ) = TEMP
               TEMP = Z( J4-2 )
               Z( J4-2 ) = Z( IPN4-J4-2 )
               Z( IPN4-J4-2 ) = TEMP
               TEMP = Z( J4-1 )
               Z( J4-1 ) = Z( IPN4-J4-5 )
               Z( IPN4-J4-5 ) = TEMP
               TEMP = Z( J4 )
               Z( J4 ) = Z( IPN4-J4-4 )
               Z( IPN4-J4-4 ) = TEMP
   60       CONTINUE
            IF( N0-I0.LE.4 ) THEN
               Z( 4*N0+PP-1 ) = Z( 4*I0+PP-1 )
               Z( 4*N0-PP ) = Z( 4*I0-PP )
            END IF
            DMIN2 = MIN( DMIN2, Z( 4*N0+PP-1 ) )
            Z( 4*N0+PP-1 ) = MIN( Z( 4*N0+PP-1 ), Z( 4*I0+PP-1 ),
     $                            Z( 4*I0+PP+3 ) )
            Z( 4*N0-PP ) = MIN( Z( 4*N0-PP ), Z( 4*I0-PP ),
     $                          Z( 4*I0-PP+4 ) )
            QMAX = MAX( QMAX, Z( 4*I0+PP-3 ), Z( 4*I0+PP+1 ) )
            DMIN = -ZERO
         END IF
      END IF
*
      IF( DMIN.LT.ZERO .OR. SAFMIN*QMAX.LT.MIN( Z( 4*N0+PP-1 ),
     $    Z( 4*N0+PP-9 ), DMIN2+Z( 4*N0-PP ) ) ) THEN
*
*        Choose a shift.
*
         CALL DLAZQ4( I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN, DN1,
     $                DN2, TAU, TTYPE, G )
*
*        Call dqds until DMIN > 0.
*
   80    CONTINUE
*
         CALL DLASQ5( I0, N0, Z, PP, TAU, DMIN, DMIN1, DMIN2, DN,
     $                DN1, DN2, IEEE )
*
         NDIV = NDIV + ( N0-I0+2 )
         ITER = ITER + 1
*
*        Check status.
*
         IF( DMIN.GE.ZERO .AND. DMIN1.GT.ZERO ) THEN
*
*           Success.
*
            GO TO 100
*
         ELSE IF( DMIN.LT.ZERO .AND. DMIN1.GT.ZERO .AND.
     $            Z( 4*( N0-1 )-PP ).LT.TOL*( SIGMA+DN1 ) .AND.
     $            ABS( DN ).LT.TOL*SIGMA ) THEN
*
*           Convergence hidden by negative DN.
*
            Z( 4*( N0-1 )-PP+2 ) = ZERO
            DMIN = ZERO
            GO TO 100
         ELSE IF( DMIN.LT.ZERO ) THEN
*
*           TAU too big. Select new TAU and try again.
*
            NFAIL = NFAIL + 1
            IF( TTYPE.LT.-22 ) THEN
*
*              Failed twice. Play it safe.
*
               TAU = ZERO
            ELSE IF( DMIN1.GT.ZERO ) THEN
*
*              Late failure. Gives excellent shift.
*
               TAU = ( TAU+DMIN )*( ONE-TWO*EPS )
               TTYPE = TTYPE - 11
            ELSE
*
*              Early failure. Divide by 4.
*
               TAU = QURTR*TAU
               TTYPE = TTYPE - 12
            END IF
            GO TO 80
         ELSE IF( DMIN.NE.DMIN ) THEN
*
*           NaN.
*
            TAU = ZERO
            GO TO 80
         ELSE
*
*           Possible underflow. Play it safe.
*
            GO TO 90
         END IF
      END IF
*
*     Risk of underflow.
*
   90 CONTINUE
      CALL DLASQ6( I0, N0, Z, PP, DMIN, DMIN1, DMIN2, DN, DN1, DN2 )
      NDIV = NDIV + ( N0-I0+2 )
      ITER = ITER + 1
      TAU = ZERO
*
  100 CONTINUE
      IF( TAU.LT.SIGMA ) THEN
         DESIG = DESIG + TAU
         T = SIGMA + DESIG
         DESIG = DESIG - ( T-SIGMA )
      ELSE
         T = SIGMA + TAU
         DESIG = SIGMA - ( T-TAU ) + DESIG
      END IF
      SIGMA = T
*
      RETURN
*
*     End of DLAZQ3
*
      END
      SUBROUTINE DLAZQ4( I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN,
     $                   DN1, DN2, TAU, TTYPE, G )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            I0, N0, N0IN, PP, TTYPE
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DN1, DN2, G, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAZQ4 computes an approximation TAU to the smallest eigenvalue 
*  using values of d from the previous transform.
*
*  I0    (input) INTEGER
*        First index.
*
*  N0    (input) INTEGER
*        Last index.
*
*  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
*        Z holds the qd array.
*
*  PP    (input) INTEGER
*        PP=0 for ping, PP=1 for pong.
*
*  N0IN  (input) INTEGER
*        The value of N0 at start of EIGTEST.
*
*  DMIN  (input) DOUBLE PRECISION
*        Minimum value of d.
*
*  DMIN1 (input) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ).
*
*  DMIN2 (input) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
*
*  DN    (input) DOUBLE PRECISION
*        d(N)
*
*  DN1   (input) DOUBLE PRECISION
*        d(N-1)
*
*  DN2   (input) DOUBLE PRECISION
*        d(N-2)
*
*  TAU   (output) DOUBLE PRECISION
*        This is the shift.
*
*  TTYPE (output) INTEGER
*        Shift type.
*
*  G     (input/output) DOUBLE PRECISION
*        G is passed as an argument in order to save its value between
*        calls to DLAZQ4
*
*  Further Details
*  ===============
*  CNST1 = 9/16
*
*  This is a thread safe version of DLASQ4, which passes G through the
*  argument list in place of declaring G in a SAVE statment.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   CNST1, CNST2, CNST3
      PARAMETER          ( CNST1 = 0.5630D0, CNST2 = 1.010D0,
     $                   CNST3 = 1.050D0 )
      DOUBLE PRECISION   QURTR, THIRD, HALF, ZERO, ONE, TWO, HUNDRD
      PARAMETER          ( QURTR = 0.250D0, THIRD = 0.3330D0,
     $                   HALF = 0.50D0, ZERO = 0.0D0, ONE = 1.0D0,
     $                   TWO = 2.0D0, HUNDRD = 100.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I4, NN, NP
      DOUBLE PRECISION   A2, B1, B2, GAM, GAP1, GAP2, S
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     A negative DMIN forces the shift to take that absolute value
*     TTYPE records the type of shift.
*
      IF( DMIN.LE.ZERO ) THEN
         TAU = -DMIN
         TTYPE = -1
         RETURN
      END IF
*       
      NN = 4*N0 + PP
      IF( N0IN.EQ.N0 ) THEN
*
*        No eigenvalues deflated.
*
         IF( DMIN.EQ.DN .OR. DMIN.EQ.DN1 ) THEN
*
            B1 = SQRT( Z( NN-3 ) )*SQRT( Z( NN-5 ) )
            B2 = SQRT( Z( NN-7 ) )*SQRT( Z( NN-9 ) )
            A2 = Z( NN-7 ) + Z( NN-5 )
*
*           Cases 2 and 3.
*
            IF( DMIN.EQ.DN .AND. DMIN1.EQ.DN1 ) THEN
               GAP2 = DMIN2 - A2 - DMIN2*QURTR
               IF( GAP2.GT.ZERO .AND. GAP2.GT.B2 ) THEN
                  GAP1 = A2 - DN - ( B2 / GAP2 )*B2
               ELSE
                  GAP1 = A2 - DN - ( B1+B2 )
               END IF
               IF( GAP1.GT.ZERO .AND. GAP1.GT.B1 ) THEN
                  S = MAX( DN-( B1 / GAP1 )*B1, HALF*DMIN )
                  TTYPE = -2
               ELSE
                  S = ZERO
                  IF( DN.GT.B1 )
     $               S = DN - B1
                  IF( A2.GT.( B1+B2 ) )
     $               S = MIN( S, A2-( B1+B2 ) )
                  S = MAX( S, THIRD*DMIN )
                  TTYPE = -3
               END IF
            ELSE
*
*              Case 4.
*
               TTYPE = -4
               S = QURTR*DMIN
               IF( DMIN.EQ.DN ) THEN
                  GAM = DN
                  A2 = ZERO
                  IF( Z( NN-5 ) .GT. Z( NN-7 ) )
     $               RETURN
                  B2 = Z( NN-5 ) / Z( NN-7 )
                  NP = NN - 9
               ELSE
                  NP = NN - 2*PP
                  B2 = Z( NP-2 )
                  GAM = DN1
                  IF( Z( NP-4 ) .GT. Z( NP-2 ) )
     $               RETURN
                  A2 = Z( NP-4 ) / Z( NP-2 )
                  IF( Z( NN-9 ) .GT. Z( NN-11 ) )
     $               RETURN
                  B2 = Z( NN-9 ) / Z( NN-11 )
                  NP = NN - 13
               END IF
*
*              Approximate contribution to norm squared from I < NN-1.
*
               A2 = A2 + B2
               DO 10 I4 = NP, 4*I0 - 1 + PP, -4
                  IF( B2.EQ.ZERO )
     $               GO TO 20
                  B1 = B2
                  IF( Z( I4 ) .GT. Z( I4-2 ) )
     $               RETURN
                  B2 = B2*( Z( I4 ) / Z( I4-2 ) )
                  A2 = A2 + B2
                  IF( HUNDRD*MAX( B2, B1 ).LT.A2 .OR. CNST1.LT.A2 ) 
     $               GO TO 20
   10          CONTINUE
   20          CONTINUE
               A2 = CNST3*A2
*
*              Rayleigh quotient residual bound.
*
               IF( A2.LT.CNST1 )
     $            S = GAM*( ONE-SQRT( A2 ) ) / ( ONE+A2 )
            END IF
         ELSE IF( DMIN.EQ.DN2 ) THEN
*
*           Case 5.
*
            TTYPE = -5
            S = QURTR*DMIN
*
*           Compute contribution to norm squared from I > NN-2.
*
            NP = NN - 2*PP
            B1 = Z( NP-2 )
            B2 = Z( NP-6 )
            GAM = DN2
            IF( Z( NP-8 ).GT.B2 .OR. Z( NP-4 ).GT.B1 )
     $         RETURN
            A2 = ( Z( NP-8 ) / B2 )*( ONE+Z( NP-4 ) / B1 )
*
*           Approximate contribution to norm squared from I < NN-2.
*
            IF( N0-I0.GT.2 ) THEN
               B2 = Z( NN-13 ) / Z( NN-15 )
               A2 = A2 + B2
               DO 30 I4 = NN - 17, 4*I0 - 1 + PP, -4
                  IF( B2.EQ.ZERO )
     $               GO TO 40
                  B1 = B2
                  IF( Z( I4 ) .GT. Z( I4-2 ) )
     $               RETURN
                  B2 = B2*( Z( I4 ) / Z( I4-2 ) )
                  A2 = A2 + B2
                  IF( HUNDRD*MAX( B2, B1 ).LT.A2 .OR. CNST1.LT.A2 ) 
     $               GO TO 40
   30          CONTINUE
   40          CONTINUE
               A2 = CNST3*A2
            END IF
*
            IF( A2.LT.CNST1 )
     $         S = GAM*( ONE-SQRT( A2 ) ) / ( ONE+A2 )
         ELSE
*
*           Case 6, no information to guide us.
*
            IF( TTYPE.EQ.-6 ) THEN
               G = G + THIRD*( ONE-G )
            ELSE IF( TTYPE.EQ.-18 ) THEN
               G = QURTR*THIRD
            ELSE
               G = QURTR
            END IF
            S = G*DMIN
            TTYPE = -6
         END IF
*
      ELSE IF( N0IN.EQ.( N0+1 ) ) THEN
*
*        One eigenvalue just deflated. Use DMIN1, DN1 for DMIN and DN.
*
         IF( DMIN1.EQ.DN1 .AND. DMIN2.EQ.DN2 ) THEN 
*
*           Cases 7 and 8.
*
            TTYPE = -7
            S = THIRD*DMIN1
            IF( Z( NN-5 ).GT.Z( NN-7 ) )
     $         RETURN
            B1 = Z( NN-5 ) / Z( NN-7 )
            B2 = B1
            IF( B2.EQ.ZERO )
     $         GO TO 60
            DO 50 I4 = 4*N0 - 9 + PP, 4*I0 - 1 + PP, -4
               A2 = B1
               IF( Z( I4 ).GT.Z( I4-2 ) )
     $            RETURN
               B1 = B1*( Z( I4 ) / Z( I4-2 ) )
               B2 = B2 + B1
               IF( HUNDRD*MAX( B1, A2 ).LT.B2 ) 
     $            GO TO 60
   50       CONTINUE
   60       CONTINUE
            B2 = SQRT( CNST3*B2 )
            A2 = DMIN1 / ( ONE+B2**2 )
            GAP2 = HALF*DMIN2 - A2
            IF( GAP2.GT.ZERO .AND. GAP2.GT.B2*A2 ) THEN
               S = MAX( S, A2*( ONE-CNST2*A2*( B2 / GAP2 )*B2 ) )
            ELSE 
               S = MAX( S, A2*( ONE-CNST2*B2 ) )
               TTYPE = -8
            END IF
         ELSE
*
*           Case 9.
*
            S = QURTR*DMIN1
            IF( DMIN1.EQ.DN1 )
     $         S = HALF*DMIN1
            TTYPE = -9
         END IF
*
      ELSE IF( N0IN.EQ.( N0+2 ) ) THEN
*
*        Two eigenvalues deflated. Use DMIN2, DN2 for DMIN and DN.
*
*        Cases 10 and 11.
*
         IF( DMIN2.EQ.DN2 .AND. TWO*Z( NN-5 ).LT.Z( NN-7 ) ) THEN 
            TTYPE = -10
            S = THIRD*DMIN2
            IF( Z( NN-5 ).GT.Z( NN-7 ) )
     $         RETURN
            B1 = Z( NN-5 ) / Z( NN-7 )
            B2 = B1
            IF( B2.EQ.ZERO )
     $         GO TO 80
            DO 70 I4 = 4*N0 - 9 + PP, 4*I0 - 1 + PP, -4
               IF( Z( I4 ).GT.Z( I4-2 ) )
     $            RETURN
               B1 = B1*( Z( I4 ) / Z( I4-2 ) )
               B2 = B2 + B1
               IF( HUNDRD*B1.LT.B2 )
     $            GO TO 80
   70       CONTINUE
   80       CONTINUE
            B2 = SQRT( CNST3*B2 )
            A2 = DMIN2 / ( ONE+B2**2 )
            GAP2 = Z( NN-7 ) + Z( NN-9 ) -
     $             SQRT( Z( NN-11 ) )*SQRT( Z( NN-9 ) ) - A2
            IF( GAP2.GT.ZERO .AND. GAP2.GT.B2*A2 ) THEN
               S = MAX( S, A2*( ONE-CNST2*A2*( B2 / GAP2 )*B2 ) )
            ELSE 
               S = MAX( S, A2*( ONE-CNST2*B2 ) )
            END IF
         ELSE
            S = QURTR*DMIN2
            TTYPE = -11
         END IF
      ELSE IF( N0IN.GT.( N0+2 ) ) THEN
*
*        Case 12, more than two eigenvalues deflated. No information.
*
         S = ZERO 
         TTYPE = -12
      END IF
*
      TAU = S
      RETURN
*
*     End of DLAZQ4
*
      END
      SUBROUTINE DSTEMR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU,
     $                   M, W, Z, LDZ, NZC, ISUPPZ, TRYRAC, WORK, LWORK,
     $                   IWORK, LIWORK, INFO )
      IMPLICIT NONE
*
*  -- LAPACK computational routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE
      LOGICAL            TRYRAC
      INTEGER            IL, INFO, IU, LDZ, NZC, LIWORK, LWORK, M, N
      DOUBLE PRECISION VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * )
      DOUBLE PRECISION   Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSTEMR computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
*  a well defined set of pairwise different real eigenvalues, the corresponding
*  real eigenvectors are pairwise orthogonal.
*
*  The spectrum may be computed either completely or partially by specifying
*  either an interval (VL,VU] or a range of indices IL:IU for the desired
*  eigenvalues.
*
*  Depending on the number of desired eigenvalues, these are computed either
*  by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
*  computed by the use of various suitable L D L^T factorizations near clusters
*  of close eigenvalues (referred to as RRRs, Relatively Robust
*  Representations). An informal sketch of the algorithm follows.
*
*  For each unreduced block (submatrix) of T,
*     (a) Compute T - sigma I  = L D L^T, so that L and D
*         define all the wanted eigenvalues to high relative accuracy.
*         This means that small relative changes in the entries of D and L
*         cause only small relative changes in the eigenvalues and
*         eigenvectors. The standard (unfactored) representation of the
*         tridiagonal matrix T does not have this property in general.
*     (b) Compute the eigenvalues to suitable accuracy.
*         If the eigenvectors are desired, the algorithm attains full
*         accuracy of the computed eigenvalues only right before
*         the corresponding vectors have to be computed, see steps c) and d).
*     (c) For each cluster of close eigenvalues, select a new
*         shift close to the cluster, find a new factorization, and refine
*         the shifted eigenvalues to suitable accuracy.
*     (d) For each eigenvalue with a large enough relative separation compute
*         the corresponding eigenvector by forming a rank revealing twisted
*         factorization. Go back to (c) for any clusters that remain.
*
*  For more details, see:
*  - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
*    to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
*    Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
*  - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
*    Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
*    2004.  Also LAPACK Working Note 154.
*  - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
*    tridiagonal eigenvalue/eigenvector problem",
*    Computer Science Division Technical Report No. UCB/CSD-97-971,
*    UC Berkeley, May 1997.
*
*  Notes:
*  1.DSTEMR works only on machines which follow IEEE-754
*  floating-point standard in their handling of infinities and NaNs.
*  This permits the use of efficient inner loops avoiding a check for
*  zero divisors.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  RANGE   (input) CHARACTER*1
*          = 'A': all eigenvalues will be found.
*          = 'V': all eigenvalues in the half-open interval (VL,VU]
*                 will be found.
*          = 'I': the IL-th through IU-th eigenvalues will be found.
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the N diagonal elements of the tridiagonal matrix
*          T. On exit, D is overwritten.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the (N-1) subdiagonal elements of the tridiagonal
*          matrix T in elements 1 to N-1 of E. E(N) need not be set on
*          input, but is used internally as workspace.
*          On exit, E is overwritten.
*
*  VL      (input) DOUBLE PRECISION
*  VU      (input) DOUBLE PRECISION
*          If RANGE='V', the lower and upper bounds of the interval to
*          be searched for eigenvalues. VL < VU.
*          Not referenced if RANGE = 'A' or 'I'.
*
*  IL      (input) INTEGER
*  IU      (input) INTEGER
*          If RANGE='I', the indices (in ascending order) of the
*          smallest and largest eigenvalues to be returned.
*          1 <= IL <= IU <= N, if N > 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          The first M elements contain the selected eigenvalues in
*          ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )
*          If JOBZ = 'V', and if INFO = 0, then the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix T
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and can be computed with a workspace
*          query by setting NZC = -1, see below.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', then LDZ >= max(1,N).
*
*  NZC     (input) INTEGER
*          The number of eigenvectors to be held in the array Z.
*          If RANGE = 'A', then NZC >= max(1,N).
*          If RANGE = 'V', then NZC >= the number of eigenvalues in (VL,VU].
*          If RANGE = 'I', then NZC >= IU-IL+1.
*          If NZC = -1, then a workspace query is assumed; the
*          routine calculates the number of columns of the array Z that
*          are needed to hold the eigenvectors.
*          This value is returned as the first entry of the Z array, and
*          no error message related to NZC is issued by XERBLA.
*
*  ISUPPZ  (output) INTEGER ARRAY, dimension ( 2*max(1,M) )
*          The support of the eigenvectors in Z, i.e., the indices
*          indicating the nonzero elements in Z. The i-th computed eigenvector
*          is nonzero only in elements ISUPPZ( 2*i-1 ) through
*          ISUPPZ( 2*i ). This is relevant in the case when the matrix
*          is split. ISUPPZ is only accessed when JOBZ is 'V' and N > 0.
*
*  TRYRAC  (input/output) LOGICAL
*          If TRYRAC.EQ..TRUE., indicates that the code should check whether
*          the tridiagonal matrix defines its eigenvalues to high relative
*          accuracy.  If so, the code uses relative-accuracy preserving
*          algorithms that might be (a bit) slower depending on the matrix.
*          If the matrix does not define its eigenvalues to high relative
*          accuracy, the code can uses possibly faster algorithms.
*          If TRYRAC.EQ..FALSE., the code is not required to guarantee
*          relatively accurate eigenvalues and can use the fastest possible
*          techniques.
*          On exit, a .TRUE. TRYRAC will be set to .FALSE. if the matrix
*          does not define its eigenvalues to high relative accuracy.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal
*          (and minimal) LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,18*N)
*          if JOBZ = 'V', and LWORK >= max(1,12*N) if JOBZ = 'N'.
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (LIWORK)
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.  LIWORK >= max(1,10*N)
*          if the eigenvectors are desired, and LIWORK >= max(1,8*N)
*          if only the eigenvalues are to be computed.
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal size of the IWORK array,
*          returns this value as the first entry of the IWORK array, and
*          no error message related to LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          On exit, INFO
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = 1X, internal error in DLARRE,
*                if INFO = 2X, internal error in DLARRV.
*                Here, the digit X = ABS( IINFO ) < 10, where IINFO is
*                the nonzero error code returned by DLARRE or
*                DLARRV, respectively.
*
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Beresford Parlett, University of California, Berkeley, USA
*     Jim Demmel, University of California, Berkeley, USA
*     Inderjit Dhillon, University of Texas, Austin, USA
*     Osni Marques, LBNL/NERSC, USA
*     Christof Voemel, University of California, Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, FOUR, MINRGP
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0,
     $                     FOUR = 4.0D0,
     $                     MINRGP = 1.0D-3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LQUERY, VALEIG, WANTZ, ZQUERY
      INTEGER            I, IBEGIN, IEND, IFIRST, IIL, IINDBL, IINDW,
     $                   IINDWK, IINFO, IINSPL, IIU, ILAST, IN, INDD,
     $                   INDE2, INDERR, INDGP, INDGRS, INDWRK, ITMP,
     $                   ITMP2, J, JBLK, JJ, LIWMIN, LWMIN, NSPLIT,
     $                   NZCMIN, OFFSET, WBEGIN, WEND
      DOUBLE PRECISION   BIGNUM, CS, EPS, PIVMIN, R1, R2, RMAX, RMIN,
     $                   RTOL1, RTOL2, SAFMIN, SCALE, SMLNUM, SN,
     $                   THRESH, TMP, TNRM, WL, WU
*     ..
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAE2, DLAEV2, DLARRC, DLARRE, DLARRJ,
     $                   DLARRR, DLARRV, DLASRT, DSCAL, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT


*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
*
      LQUERY = ( ( LWORK.EQ.-1 ).OR.( LIWORK.EQ.-1 ) )
      ZQUERY = ( NZC.EQ.-1 )
      TRYRAC = ( INFO.NE.0 )

*     DSTEMR needs WORK of size 6*N, IWORK of size 3*N.
*     In addition, DLARRE needs WORK of size 6*N, IWORK of size 5*N.
*     Furthermore, DLARRV needs WORK of size 12*N, IWORK of size 7*N.
      IF( WANTZ ) THEN
         LWMIN = 18*N
         LIWMIN = 10*N
      ELSE
*        need less workspace if only the eigenvalues are wanted
         LWMIN = 12*N
         LIWMIN = 8*N
      ENDIF

      WL = ZERO
      WU = ZERO
      IIL = 0
      IIU = 0

      IF( VALEIG ) THEN
*        We do not reference VL, VU in the cases RANGE = 'I','A'
*        The interval (WL, WU] contains all the wanted eigenvalues.
*        It is either given by the user or computed in DLARRE.
         WL = VL
         WU = VU
      ELSEIF( INDEIG ) THEN
*        We do not reference IL, IU in the cases RANGE = 'V','A'
         IIL = IL
         IIU = IU
      ENDIF
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( VALEIG .AND. N.GT.0 .AND. WU.LE.WL ) THEN
         INFO = -7
      ELSE IF( INDEIG .AND. ( IIL.LT.1 .OR. IIL.GT.N ) ) THEN
         INFO = -8
      ELSE IF( INDEIG .AND. ( IIU.LT.IIL .OR. IIU.GT.N ) ) THEN
         INFO = -9
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -13
      ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -17
      ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -19
      END IF
*
*     Get machine constants.
*
      SAFMIN = DLAMCH( 'Safe minimum' )
      EPS = DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN = SQRT( SMLNUM )
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
*
         IF( WANTZ .AND. ALLEIG ) THEN
            NZCMIN = N
         ELSE IF( WANTZ .AND. VALEIG ) THEN
            CALL DLARRC( 'T', N, VL, VU, D, E, SAFMIN,
     $                            NZCMIN, ITMP, ITMP2, INFO )
         ELSE IF( WANTZ .AND. INDEIG ) THEN
            NZCMIN = IIU-IIL+1
         ELSE
*           WANTZ .EQ. FALSE.
            NZCMIN = 0
         ENDIF
         IF( ZQUERY .AND. INFO.EQ.0 ) THEN
            Z( 1,1 ) = NZCMIN
         ELSE IF( NZC.LT.NZCMIN .AND. .NOT.ZQUERY ) THEN
            INFO = -14
         END IF
      END IF

      IF( INFO.NE.0 ) THEN
*
         CALL XERBLA( 'DSTEMR', -INFO )
*
         RETURN
      ELSE IF( LQUERY .OR. ZQUERY ) THEN
         RETURN
      END IF
*
*     Handle N = 0, 1, and 2 cases immediately
*
      M = 0
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = D( 1 )
         ELSE
            IF( WL.LT.D( 1 ) .AND. WU.GE.D( 1 ) ) THEN
               M = 1
               W( 1 ) = D( 1 )
            END IF
         END IF
         IF( WANTZ.AND.(.NOT.ZQUERY) ) THEN
            Z( 1, 1 ) = ONE
            ISUPPZ(1) = 1
            ISUPPZ(2) = 1
         END IF
         RETURN
      END IF
*
      IF( N.EQ.2 ) THEN
         IF( .NOT.WANTZ ) THEN
            CALL DLAE2( D(1), E(1), D(2), R1, R2 )
         ELSE IF( WANTZ.AND.(.NOT.ZQUERY) ) THEN
            CALL DLAEV2( D(1), E(1), D(2), R1, R2, CS, SN )
         END IF
         IF( ALLEIG.OR.
     $      (VALEIG.AND.(R2.GT.WL).AND.
     $                  (R2.LE.WU)).OR.
     $      (INDEIG.AND.(IIL.EQ.1)) ) THEN
            M = M+1
            W( M ) = R2
            IF( WANTZ.AND.(.NOT.ZQUERY) ) THEN
               Z( 1, M ) = -SN
               Z( 2, M ) = CS
*              Note: At most one of SN and CS can be zero.
               IF (SN.NE.ZERO) THEN
                  IF (CS.NE.ZERO) THEN
                     ISUPPZ(2*M-1) = 1
                     ISUPPZ(2*M-1) = 2
                  ELSE
                     ISUPPZ(2*M-1) = 1
                     ISUPPZ(2*M-1) = 1
                  END IF
               ELSE
                  ISUPPZ(2*M-1) = 2
                  ISUPPZ(2*M) = 2
               END IF
            ENDIF
         ENDIF
         IF( ALLEIG.OR.
     $      (VALEIG.AND.(R1.GT.WL).AND.
     $                  (R1.LE.WU)).OR.
     $      (INDEIG.AND.(IIU.EQ.2)) ) THEN
            M = M+1
            W( M ) = R1
            IF( WANTZ.AND.(.NOT.ZQUERY) ) THEN
               Z( 1, M ) = CS
               Z( 2, M ) = SN
*              Note: At most one of SN and CS can be zero.
               IF (SN.NE.ZERO) THEN
                  IF (CS.NE.ZERO) THEN
                     ISUPPZ(2*M-1) = 1
                     ISUPPZ(2*M-1) = 2
                  ELSE
                     ISUPPZ(2*M-1) = 1
                     ISUPPZ(2*M-1) = 1
                  END IF
               ELSE
                  ISUPPZ(2*M-1) = 2
                  ISUPPZ(2*M) = 2
               END IF
            ENDIF
         ENDIF
         RETURN
      END IF

*     Continue with general N

      INDGRS = 1
      INDERR = 2*N + 1
      INDGP = 3*N + 1
      INDD = 4*N + 1
      INDE2 = 5*N + 1
      INDWRK = 6*N + 1
*
      IINSPL = 1
      IINDBL = N + 1
      IINDW = 2*N + 1
      IINDWK = 3*N + 1
*
*     Scale matrix to allowable range, if necessary.
*     The allowable range is related to the PIVMIN parameter; see the
*     comments in DLARRD.  The preference for scaling small values
*     up is heuristic; we expect users' matrices not to be close to the
*     RMAX threshold.
*
      SCALE = ONE
      TNRM = DLANST( 'M', N, D, E )
      IF( TNRM.GT.ZERO .AND. TNRM.LT.RMIN ) THEN
         SCALE = RMIN / TNRM
      ELSE IF( TNRM.GT.RMAX ) THEN
         SCALE = RMAX / TNRM
      END IF
      IF( SCALE.NE.ONE ) THEN
         CALL DSCAL( N, SCALE, D, 1 )
         CALL DSCAL( N-1, SCALE, E, 1 )
         TNRM = TNRM*SCALE
         IF( VALEIG ) THEN
*           If eigenvalues in interval have to be found,
*           scale (WL, WU] accordingly
            WL = WL*SCALE
            WU = WU*SCALE
         ENDIF
      END IF
*
*     Compute the desired eigenvalues of the tridiagonal after splitting
*     into smaller subblocks if the corresponding off-diagonal elements
*     are small
*     THRESH is the splitting parameter for DLARRE
*     A negative THRESH forces the old splitting criterion based on the
*     size of the off-diagonal. A positive THRESH switches to splitting
*     which preserves relative accuracy.
*
      IF( TRYRAC ) THEN
*        Test whether the matrix warrants the more expensive relative approach.
         CALL DLARRR( N, D, E, IINFO )
      ELSE
*        The user does not care about relative accurately eigenvalues
         IINFO = -1
      ENDIF
*     Set the splitting criterion
      IF (IINFO.EQ.0) THEN
         THRESH = EPS
      ELSE
         THRESH = -EPS
*        relative accuracy is desired but T does not guarantee it
         TRYRAC = .FALSE.
      ENDIF
*
      IF( TRYRAC ) THEN
*        Copy original diagonal, needed to guarantee relative accuracy
         CALL DCOPY(N,D,1,WORK(INDD),1)
      ENDIF
*     Store the squares of the offdiagonal values of T
      DO 5 J = 1, N-1
         WORK( INDE2+J-1 ) = E(J)**2
 5    CONTINUE

*     Set the tolerance parameters for bisection
      IF( .NOT.WANTZ ) THEN
*        DLARRE computes the eigenvalues to full precision.
         RTOL1 = FOUR * EPS
         RTOL2 = FOUR * EPS
      ELSE
*        DLARRE computes the eigenvalues to less than full precision.
*        DLARRV will refine the eigenvalue approximations, and we can
*        need less accurate initial bisection in DLARRE.
*        Note: these settings do only affect the subset case and DLARRE
         RTOL1 = SQRT(EPS)
         RTOL2 = MAX( SQRT(EPS)*5.0D-3, FOUR * EPS )
      ENDIF
      CALL DLARRE( RANGE, N, WL, WU, IIL, IIU, D, E,
     $             WORK(INDE2), RTOL1, RTOL2, THRESH, NSPLIT,
     $             IWORK( IINSPL ), M, W, WORK( INDERR ),
     $             WORK( INDGP ), IWORK( IINDBL ),
     $             IWORK( IINDW ), WORK( INDGRS ), PIVMIN,
     $             WORK( INDWRK ), IWORK( IINDWK ), IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 10 + ABS( IINFO )
         RETURN
      END IF
*     Note that if RANGE .NE. 'V', DLARRE computes bounds on the desired
*     part of the spectrum. All desired eigenvalues are contained in
*     (WL,WU]


      IF( WANTZ ) THEN
*
*        Compute the desired eigenvectors corresponding to the computed
*        eigenvalues
*
         CALL DLARRV( N, WL, WU, D, E,
     $                PIVMIN, IWORK( IINSPL ), M,
     $                1, M, MINRGP, RTOL1, RTOL2,
     $                W, WORK( INDERR ), WORK( INDGP ), IWORK( IINDBL ),
     $                IWORK( IINDW ), WORK( INDGRS ), Z, LDZ,
     $                ISUPPZ, WORK( INDWRK ), IWORK( IINDWK ), IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = 20 + ABS( IINFO )
            RETURN
         END IF
      ELSE
*        DLARRE computes eigenvalues of the (shifted) root representation
*        DLARRV returns the eigenvalues of the unshifted matrix.
*        However, if the eigenvectors are not desired by the user, we need
*        to apply the corresponding shifts from DLARRE to obtain the
*        eigenvalues of the original matrix.
         DO 20 J = 1, M
            ITMP = IWORK( IINDBL+J-1 )
            W( J ) = W( J ) + E( IWORK( IINSPL+ITMP-1 ) )
 20      CONTINUE
      END IF
*

      IF ( TRYRAC ) THEN
*        Refine computed eigenvalues so that they are relatively accurate
*        with respect to the original matrix T.
         IBEGIN = 1
         WBEGIN = 1
         DO 39  JBLK = 1, IWORK( IINDBL+M-1 )
            IEND = IWORK( IINSPL+JBLK-1 )
            IN = IEND - IBEGIN + 1
            WEND = WBEGIN - 1
*           check if any eigenvalues have to be refined in this block
 36         CONTINUE
            IF( WEND.LT.M ) THEN
               IF( IWORK( IINDBL+WEND ).EQ.JBLK ) THEN
                  WEND = WEND + 1
                  GO TO 36
               END IF
            END IF
            IF( WEND.LT.WBEGIN ) THEN
               IBEGIN = IEND + 1
               GO TO 39
            END IF

            OFFSET = IWORK(IINDW+WBEGIN-1)-1
            IFIRST = IWORK(IINDW+WBEGIN-1)
            ILAST = IWORK(IINDW+WEND-1)
            RTOL2 = FOUR * EPS
            CALL DLARRJ( IN,
     $                   WORK(INDD+IBEGIN-1), WORK(INDE2+IBEGIN-1),
     $                   IFIRST, ILAST, RTOL2, OFFSET, W(WBEGIN),
     $                   WORK( INDERR+WBEGIN-1 ),
     $                   WORK( INDWRK ), IWORK( IINDWK ), PIVMIN,
     $                   TNRM, IINFO )
            IBEGIN = IEND + 1
            WBEGIN = WEND + 1
 39      CONTINUE
      ENDIF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( SCALE.NE.ONE ) THEN
         CALL DSCAL( M, ONE / SCALE, W, 1 )
      END IF
*
*     If eigenvalues are not in increasing order, then sort them,
*     possibly along with eigenvectors.
*
      IF( NSPLIT.GT.1 ) THEN
         IF( .NOT. WANTZ ) THEN
            CALL DLASRT( 'I', M, W, IINFO )
            IF( IINFO.NE.0 ) THEN
               INFO = 3
               RETURN
            END IF
         ELSE
            DO 60 J = 1, M - 1
               I = 0
               TMP = W( J )
               DO 50 JJ = J + 1, M
                  IF( W( JJ ).LT.TMP ) THEN
                     I = JJ
                     TMP = W( JJ )
                  END IF
 50            CONTINUE
               IF( I.NE.0 ) THEN
                  W( I ) = W( J )
                  W( J ) = TMP
                  IF( WANTZ ) THEN
                     CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
                     ITMP = ISUPPZ( 2*I-1 )
                     ISUPPZ( 2*I-1 ) = ISUPPZ( 2*J-1 )
                     ISUPPZ( 2*J-1 ) = ITMP
                     ITMP = ISUPPZ( 2*I )
                     ISUPPZ( 2*I ) = ISUPPZ( 2*J )
                     ISUPPZ( 2*J ) = ITMP
                  END IF
               END IF
 60         CONTINUE
         END IF
      ENDIF
*
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
      RETURN
*
*     End of DSTEMR
*
      END
      DOUBLE PRECISION FUNCTION DZSUM1( N, CX, INCX )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
*     ..
*     .. Array Arguments ..
      COMPLEX*16         CX( * )
*     ..
*
*  Purpose
*  =======
*
*  DZSUM1 takes the sum of the absolute values of a complex
*  vector and returns a double precision result.
*
*  Based on DZASUM from the Level 1 BLAS.
*  The change is to use the 'genuine' absolute value.
*
*  Contributed by Nick Higham for use with ZLACON.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of elements in the vector CX.
*
*  CX      (input) COMPLEX*16 array, dimension (N)
*          The vector whose elements will be summed.
*
*  INCX    (input) INTEGER
*          The spacing between successive values of CX.  INCX > 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, NINCX
      DOUBLE PRECISION   STEMP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      DZSUM1 = 0.0D0
      STEMP = 0.0D0
      IF( N.LE.0 )
     $   RETURN
      IF( INCX.EQ.1 )
     $   GO TO 20
*
*     CODE FOR INCREMENT NOT EQUAL TO 1
*
      NINCX = N*INCX
      DO 10 I = 1, NINCX, INCX
*
*        NEXT LINE MODIFIED.
*
         STEMP = STEMP + ABS( CX( I ) )
   10 CONTINUE
      DZSUM1 = STEMP
      RETURN
*
*     CODE FOR INCREMENT EQUAL TO 1
*
   20 CONTINUE
      DO 30 I = 1, N
*
*        NEXT LINE MODIFIED.
*
         STEMP = STEMP + ABS( CX( I ) )
   30 CONTINUE
      DZSUM1 = STEMP
      RETURN
*
*     End of DZSUM1
*
      END
      INTEGER FUNCTION IPARMQ( ISPEC, NAME, OPTS, N, ILO, IHI, LWORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*     
*     .. Scalar Arguments ..
      INTEGER            IHI, ILO, ISPEC, LWORK, N
      CHARACTER          NAME*( * ), OPTS*( * )
*
*  Purpose
*  =======
*
*       This program sets problem and machine dependent parameters
*       useful for xHSEQR and its subroutines. It is called whenever 
*       ILAENV is called with 12 <= ISPEC <= 16
*
*  Arguments
*  =========
*
*       ISPEC  (input) integer scalar
*              ISPEC specifies which tunable parameter IPARMQ should
*              return.
*
*              ISPEC=12: (INMIN)  Matrices of order nmin or less
*                        are sent directly to xLAHQR, the implicit
*                        double shift QR algorithm.  NMIN must be
*                        at least 11.
*
*              ISPEC=13: (INWIN)  Size of the deflation window.
*                        This is best set greater than or equal to
*                        the number of simultaneous shifts NS.
*                        Larger matrices benefit from larger deflation
*                        windows.
*
*              ISPEC=14: (INIBL) Determines when to stop nibbling and
*                        invest in an (expensive) multi-shift QR sweep.
*                        If the aggressive early deflation subroutine
*                        finds LD converged eigenvalues from an order
*                        NW deflation window and LD.GT.(NW*NIBBLE)/100,
*                        then the next QR sweep is skipped and early
*                        deflation is applied immediately to the
*                        remaining active diagonal block.  Setting
*                        IPARMQ(ISPEC=14) = 0 causes TTQRE to skip a
*                        multi-shift QR sweep whenever early deflation
*                        finds a converged eigenvalue.  Setting
*                        IPARMQ(ISPEC=14) greater than or equal to 100
*                        prevents TTQRE from skipping a multi-shift
*                        QR sweep.
*
*              ISPEC=15: (NSHFTS) The number of simultaneous shifts in
*                        a multi-shift QR iteration.
*
*              ISPEC=16: (IACC22) IPARMQ is set to 0, 1 or 2 with the
*                        following meanings.
*                        0:  During the multi-shift QR sweep,
*                            xLAQR5 does not accumulate reflections and
*                            does not use matrix-matrix multiply to
*                            update the far-from-diagonal matrix
*                            entries.
*                        1:  During the multi-shift QR sweep,
*                            xLAQR5 and/or xLAQRaccumulates reflections and uses
*                            matrix-matrix multiply to update the
*                            far-from-diagonal matrix entries.
*                        2:  During the multi-shift QR sweep.
*                            xLAQR5 accumulates reflections and takes
*                            advantage of 2-by-2 block structure during
*                            matrix-matrix multiplies.
*                        (If xTRMM is slower than xGEMM, then
*                        IPARMQ(ISPEC=16)=1 may be more efficient than
*                        IPARMQ(ISPEC=16)=2 despite the greater level of
*                        arithmetic work implied by the latter choice.)
*
*       NAME    (input) character string
*               Name of the calling subroutine
*
*       OPTS    (input) character string
*               This is a concatenation of the string arguments to
*               TTQRE.
*
*       N       (input) integer scalar
*               N is the order of the Hessenberg matrix H.
*
*       ILO     (input) INTEGER
*       IHI     (input) INTEGER
*               It is assumed that H is already upper triangular
*               in rows and columns 1:ILO-1 and IHI+1:N.
*
*       LWORK   (input) integer scalar
*               The amount of workspace available.
*
*  Further Details
*  ===============
*
*       Little is known about how best to choose these parameters.
*       It is possible to use different values of the parameters
*       for each of CHSEQR, DHSEQR, SHSEQR and ZHSEQR.
*
*       It is probably best to choose different parameters for
*       different matrices and different parameters at different
*       times during the iteration, but this has not been
*       implemented --- yet.
*
*
*       The best choices of most of the parameters depend
*       in an ill-understood way on the relative execution
*       rate of xLAQR3 and xLAQR5 and on the nature of each
*       particular eigenvalue problem.  Experiment may be the
*       only practical way to determine which choices are most
*       effective.
*
*       Following is a list of default values supplied by IPARMQ.
*       These defaults may be adjusted in order to attain better
*       performance in any particular computational environment.
*
*       IPARMQ(ISPEC=12) The xLAHQR vs xLAQR0 crossover point.
*                        Default: 75. (Must be at least 11.)
*
*       IPARMQ(ISPEC=13) Recommended deflation window size.
*                        This depends on ILO, IHI and NS, the
*                        number of simultaneous shifts returned
*                        by IPARMQ(ISPEC=15).  The default for
*                        (IHI-ILO+1).LE.500 is NS.  The default
*                        for (IHI-ILO+1).GT.500 is 3*NS/2.
*
*       IPARMQ(ISPEC=14) Nibble crossover point.  Default: 14.
*
*       IPARMQ(ISPEC=15) Number of simultaneous shifts, NS.
*                        a multi-shift QR iteration.
*
*                        If IHI-ILO+1 is ...
*
*                        greater than      ...but less    ... the
*                        or equal to ...      than        default is
*
*                                0               30       NS =   2+
*                               30               60       NS =   4+
*                               60              150       NS =  10
*                              150              590       NS =  **
*                              590             3000       NS =  64
*                             3000             6000       NS = 128
*                             6000             infinity   NS = 256
*
*                    (+)  By default matrices of this order are
*                         passed to the implicit double shift routine
*                         xLAHQR.  See IPARMQ(ISPEC=12) above.   These
*                         values of NS are used only in case of a rare
*                         xLAHQR failure.
*
*                    (**) The asterisks (**) indicate an ad-hoc
*                         function increasing from 10 to 64.
*
*       IPARMQ(ISPEC=16) Select structured matrix multiply.
*                        (See ISPEC=16 above for details.)
*                        Default: 3.
*
*     ================================================================
*     .. Parameters ..
      INTEGER            INMIN, INWIN, INIBL, ISHFTS, IACC22
      PARAMETER          ( INMIN = 12, INWIN = 13, INIBL = 14,
     $                   ISHFTS = 15, IACC22 = 16 )
      INTEGER            NMIN, K22MIN, KACMIN, NIBBLE, KNWSWP
      PARAMETER          ( NMIN = 75, K22MIN = 14, KACMIN = 14,
     $                   NIBBLE = 14, KNWSWP = 500 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0 )
*     ..
*     .. Local Scalars ..
      INTEGER            NH, NS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LOG, MAX, MOD, NINT, REAL
*     ..
*     .. Executable Statements ..
      IF( ( ISPEC.EQ.ISHFTS ) .OR. ( ISPEC.EQ.INWIN ) .OR.
     $    ( ISPEC.EQ.IACC22 ) ) THEN
*
*        ==== Set the number simultaneous shifts ====
*
         NH = IHI - ILO + 1
         NS = 2
         IF( NH.GE.30 )
     $      NS = 4
         IF( NH.GE.60 )
     $      NS = 10
         IF( NH.GE.150 )
     $      NS = MAX( 10, NH / NINT( LOG( REAL( NH ) ) / LOG( TWO ) ) )
         IF( NH.GE.590 )
     $      NS = 64
         IF( NH.GE.3000 )
     $      NS = 128
         IF( NH.GE.6000 )
     $      NS = 256
         NS = MAX( 2, NS-MOD( NS, 2 ) )
      END IF
*
      IF( ISPEC.EQ.INMIN ) THEN
*
*
*        ===== Matrices of order smaller than NMIN get sent
*        .     to xLAHQR, the classic double shift algorithm.
*        .     This must be at least 11. ====
*
         IPARMQ = NMIN
*
      ELSE IF( ISPEC.EQ.INIBL ) THEN
*
*        ==== INIBL: skip a multi-shift qr iteration and
*        .    whenever aggressive early deflation finds
*        .    at least (NIBBLE*(window size)/100) deflations. ====
*
         IPARMQ = NIBBLE
*
      ELSE IF( ISPEC.EQ.ISHFTS ) THEN
*
*        ==== NSHFTS: The number of simultaneous shifts =====
*
         IPARMQ = NS
*
      ELSE IF( ISPEC.EQ.INWIN ) THEN
*
*        ==== NW: deflation window size.  ====
*
         IF( NH.LE.KNWSWP ) THEN
            IPARMQ = NS
         ELSE
            IPARMQ = 3*NS / 2
         END IF
*
      ELSE IF( ISPEC.EQ.IACC22 ) THEN
*
*        ==== IACC22: Whether to accumulate reflections
*        .     before updating the far-from-diagonal elements
*        .     and whether to use 2-by-2 block structure while
*        .     doing it.  A small amount of work could be saved
*        .     by making this choice dependent also upon the
*        .     NH=IHI-ILO+1.
*
         IPARMQ = 0
         IF( NS.GE.KACMIN )
     $      IPARMQ = 1
         IF( NS.GE.K22MIN )
     $      IPARMQ = 2
*
      ELSE
*        ===== invalid value of ispec =====
         IPARMQ = -1
*
      END IF
*
*     ==== End of IPARMQ ====
*
      END
