      SUBROUTINE DGESC2( N, A, LDA, RHS, IPIV, JPIV, SCALE )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            LDA, N
      DOUBLE PRECISION   SCALE
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * ), JPIV( * )
      DOUBLE PRECISION   A( LDA, * ), RHS( * )
*     ..
*
*  Purpose
*  =======
*
*  DGESC2 solves a system of linear equations
*
*            A * X = scale* RHS
*
*  with a general N-by-N matrix A using the LU factorization with
*  complete pivoting computed by DGETC2.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix A.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the  LU part of the factorization of the n-by-n
*          matrix A computed by DGETC2:  A = P * L * U * Q
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1, N).
*
*  RHS     (input/output) DOUBLE PRECISION array, dimension (N).
*          On entry, the right hand side vector b.
*          On exit, the solution vector X.
*
*  IPIV    (input) INTEGER array, dimension (N).
*          The pivot indices; for 1 <= i <= N, row i of the
*          matrix has been interchanged with row IPIV(i).
*
*  JPIV    (input) INTEGER array, dimension (N).
*          The pivot indices; for 1 <= j <= N, column j of the
*          matrix has been interchanged with column JPIV(j).
*
*  SCALE    (output) DOUBLE PRECISION
*           On exit, SCALE contains the scale factor. SCALE is chosen
*           0 <= SCALE <= 1 to prevent owerflow in the solution.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
*     Umea University, S-901 87 Umea, Sweden.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, TWO
      PARAMETER          ( ONE = 1.0D+0, TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   BIGNUM, EPS, SMLNUM, TEMP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASWP, DSCAL
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           IDAMAX, DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
*      Set constant to control owerflow
*
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Apply permutations IPIV to RHS
*
      CALL DLASWP( 1, RHS, LDA, 1, N-1, IPIV, 1 )
*
*     Solve for L part
*
      DO 20 I = 1, N - 1
         DO 10 J = I + 1, N
            RHS( J ) = RHS( J ) - A( J, I )*RHS( I )
   10    CONTINUE
   20 CONTINUE
*
*     Solve for U part
*
      SCALE = ONE
*
*     Check for scaling
*
      I = IDAMAX( N, RHS, 1 )
      IF( TWO*SMLNUM*ABS( RHS( I ) ).GT.ABS( A( N, N ) ) ) THEN
         TEMP = ( ONE / TWO ) / ABS( RHS( I ) )
         CALL DSCAL( N, TEMP, RHS( 1 ), 1 )
         SCALE = SCALE*TEMP
      END IF
*
      DO 40 I = N, 1, -1
         TEMP = ONE / A( I, I )
         RHS( I ) = RHS( I )*TEMP
         DO 30 J = I + 1, N
            RHS( I ) = RHS( I ) - RHS( J )*( A( I, J )*TEMP )
   30    CONTINUE
   40 CONTINUE
*
*     Apply permutations JPIV to the solution (RHS)
*
      CALL DLASWP( 1, RHS, LDA, 1, N-1, JPIV, -1 )
      RETURN
*
*     End of DGESC2
*
      END
      SUBROUTINE DGETC2( N, A, LDA, IPIV, JPIV, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * ), JPIV( * )
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DGETC2 computes an LU factorization with complete pivoting of the
*  n-by-n matrix A. The factorization has the form A = P * L * U * Q,
*  where P and Q are permutation matrices, L is lower triangular with
*  unit diagonal elements and U is upper triangular.
*
*  This is the Level 2 BLAS algorithm.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix A. N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the n-by-n matrix A to be factored.
*          On exit, the factors L and U from the factorization
*          A = P*L*U*Q; the unit diagonal elements of L are not stored.
*          If U(k, k) appears to be less than SMIN, U(k, k) is given the
*          value of SMIN, i.e., giving a nonsingular perturbed system.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  IPIV    (output) INTEGER array, dimension(N).
*          The pivot indices; for 1 <= i <= N, row i of the
*          matrix has been interchanged with row IPIV(i).
*
*  JPIV    (output) INTEGER array, dimension(N).
*          The pivot indices; for 1 <= j <= N, column j of the
*          matrix has been interchanged with column JPIV(j).
*
*  INFO    (output) INTEGER
*           = 0: successful exit
*           > 0: if INFO = k, U(k, k) is likely to produce owerflow if
*                we try to solve for x in Ax = b. So U is perturbed to
*                avoid the overflow.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
*     Umea University, S-901 87 Umea, Sweden.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IP, IPV, J, JP, JPV
      DOUBLE PRECISION   BIGNUM, EPS, SMIN, SMLNUM, XMAX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGER, DSWAP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Set constants to control overflow
*
      INFO = 0
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Factorize A using complete pivoting.
*     Set pivots less than SMIN to SMIN.
*
      DO 40 I = 1, N - 1
*
*        Find max element in matrix A
*
         XMAX = ZERO
         DO 20 IP = I, N
            DO 10 JP = I, N
               IF( ABS( A( IP, JP ) ).GE.XMAX ) THEN
                  XMAX = ABS( A( IP, JP ) )
                  IPV = IP
                  JPV = JP
               END IF
   10       CONTINUE
   20    CONTINUE
         IF( I.EQ.1 )
     $      SMIN = MAX( EPS*XMAX, SMLNUM )
*
*        Swap rows
*
         IF( IPV.NE.I )
     $      CALL DSWAP( N, A( IPV, 1 ), LDA, A( I, 1 ), LDA )
         IPIV( I ) = IPV
*
*        Swap columns
*
         IF( JPV.NE.I )
     $      CALL DSWAP( N, A( 1, JPV ), 1, A( 1, I ), 1 )
         JPIV( I ) = JPV
*
*        Check for singularity
*
         IF( ABS( A( I, I ) ).LT.SMIN ) THEN
            INFO = I
            A( I, I ) = SMIN
         END IF
         DO 30 J = I + 1, N
            A( J, I ) = A( J, I ) / A( I, I )
   30    CONTINUE
         CALL DGER( N-I, N-I, -ONE, A( I+1, I ), 1, A( I, I+1 ), LDA,
     $              A( I+1, I+1 ), LDA )
   40 CONTINUE
*
      IF( ABS( A( N, N ) ).LT.SMIN ) THEN
         INFO = N
         A( N, N ) = SMIN
      END IF
*
      RETURN
*
*     End of DGETC2
*
      END
      SUBROUTINE DGTTS2( ITRANS, N, NRHS, DL, D, DU, DU2, IPIV, B, LDB )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            ITRANS, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * ), DU2( * )
*     ..
*
*  Purpose
*  =======
*
*  DGTTS2 solves one of the systems of equations
*     A*X = B  or  A'*X = B,
*  with a tridiagonal matrix A using the LU factorization computed
*  by DGTTRF.
*
*  Arguments
*  =========
*
*  ITRANS  (input) INTEGER
*          Specifies the form of the system of equations.
*          = 0:  A * X = B  (No transpose)
*          = 1:  A'* X = B  (Transpose)
*          = 2:  A'* X = B  (Conjugate transpose = Transpose)
*
*  N       (input) INTEGER
*          The order of the matrix A.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  DL      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) multipliers that define the matrix L from the
*          LU factorization of A.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The n diagonal elements of the upper triangular matrix U from
*          the LU factorization of A.
*
*  DU      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) elements of the first super-diagonal of U.
*
*  DU2     (input) DOUBLE PRECISION array, dimension (N-2)
*          The (n-2) elements of the second super-diagonal of U.
*
*  IPIV    (input) INTEGER array, dimension (N)
*          The pivot indices; for 1 <= i <= n, row i of the matrix was
*          interchanged with row IPIV(i).  IPIV(i) will always be either
*          i or i+1; IPIV(i) = i indicates a row interchange was not
*          required.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the matrix of right hand side vectors B.
*          On exit, B is overwritten by the solution vectors X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IP, J
      DOUBLE PRECISION   TEMP
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 )
     $   RETURN
*
      IF( ITRANS.EQ.0 ) THEN
*
*        Solve A*X = B using the LU factorization of A,
*        overwriting each right hand side vector with its solution.
*
         IF( NRHS.LE.1 ) THEN
            J = 1
   10       CONTINUE
*
*           Solve L*x = b.
*
            DO 20 I = 1, N - 1
               IP = IPIV( I )
               TEMP = B( I+1-IP+I, J ) - DL( I )*B( IP, J )
               B( I, J ) = B( IP, J )
               B( I+1, J ) = TEMP
   20       CONTINUE
*
*           Solve U*x = b.
*
            B( N, J ) = B( N, J ) / D( N )
            IF( N.GT.1 )
     $         B( N-1, J ) = ( B( N-1, J )-DU( N-1 )*B( N, J ) ) /
     $                       D( N-1 )
            DO 30 I = N - 2, 1, -1
               B( I, J ) = ( B( I, J )-DU( I )*B( I+1, J )-DU2( I )*
     $                     B( I+2, J ) ) / D( I )
   30       CONTINUE
            IF( J.LT.NRHS ) THEN
               J = J + 1
               GO TO 10
            END IF
         ELSE
            DO 60 J = 1, NRHS
*
*              Solve L*x = b.
*
               DO 40 I = 1, N - 1
                  IF( IPIV( I ).EQ.I ) THEN
                     B( I+1, J ) = B( I+1, J ) - DL( I )*B( I, J )
                  ELSE
                     TEMP = B( I, J )
                     B( I, J ) = B( I+1, J )
                     B( I+1, J ) = TEMP - DL( I )*B( I, J )
                  END IF
   40          CONTINUE
*
*              Solve U*x = b.
*
               B( N, J ) = B( N, J ) / D( N )
               IF( N.GT.1 )
     $            B( N-1, J ) = ( B( N-1, J )-DU( N-1 )*B( N, J ) ) /
     $                          D( N-1 )
               DO 50 I = N - 2, 1, -1
                  B( I, J ) = ( B( I, J )-DU( I )*B( I+1, J )-DU2( I )*
     $                        B( I+2, J ) ) / D( I )
   50          CONTINUE
   60       CONTINUE
         END IF
      ELSE
*
*        Solve A' * X = B.
*
         IF( NRHS.LE.1 ) THEN
*
*           Solve U'*x = b.
*
            J = 1
   70       CONTINUE
            B( 1, J ) = B( 1, J ) / D( 1 )
            IF( N.GT.1 )
     $         B( 2, J ) = ( B( 2, J )-DU( 1 )*B( 1, J ) ) / D( 2 )
            DO 80 I = 3, N
               B( I, J ) = ( B( I, J )-DU( I-1 )*B( I-1, J )-DU2( I-2 )*
     $                     B( I-2, J ) ) / D( I )
   80       CONTINUE
*
*           Solve L'*x = b.
*
            DO 90 I = N - 1, 1, -1
               IP = IPIV( I )
               TEMP = B( I, J ) - DL( I )*B( I+1, J )
               B( I, J ) = B( IP, J )
               B( IP, J ) = TEMP
   90       CONTINUE
            IF( J.LT.NRHS ) THEN
               J = J + 1
               GO TO 70
            END IF
*
         ELSE
            DO 120 J = 1, NRHS
*
*              Solve U'*x = b.
*
               B( 1, J ) = B( 1, J ) / D( 1 )
               IF( N.GT.1 )
     $            B( 2, J ) = ( B( 2, J )-DU( 1 )*B( 1, J ) ) / D( 2 )
               DO 100 I = 3, N
                  B( I, J ) = ( B( I, J )-DU( I-1 )*B( I-1, J )-
     $                        DU2( I-2 )*B( I-2, J ) ) / D( I )
  100          CONTINUE
               DO 110 I = N - 1, 1, -1
                  IF( IPIV( I ).EQ.I ) THEN
                     B( I, J ) = B( I, J ) - DL( I )*B( I+1, J )
                  ELSE
                     TEMP = B( I+1, J )
                     B( I+1, J ) = B( I, J ) - DL( I )*TEMP
                     B( I, J ) = TEMP
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      END IF
*
*     End of DGTTS2
*
      END
      SUBROUTINE DLABAD( SMALL, LARGE )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   LARGE, SMALL
*     ..
*
*  Purpose
*  =======
*
*  DLABAD takes as input the values computed by DLAMCH for underflow and
*  overflow, and returns the square root of each of these values if the
*  log of LARGE is sufficiently large.  This subroutine is intended to
*  identify machines with a large exponent range, such as the Crays, and
*  redefine the underflow and overflow limits to be the square roots of
*  the values computed by DLAMCH.  This subroutine is needed because
*  DLAMCH does not compensate for poor arithmetic in the upper half of
*  the exponent range, as is found on a Cray.
*
*  Arguments
*  =========
*
*  SMALL   (input/output) DOUBLE PRECISION
*          On entry, the underflow threshold as computed by DLAMCH.
*          On exit, if LOG10(LARGE) is sufficiently large, the square
*          root of SMALL, otherwise unchanged.
*
*  LARGE   (input/output) DOUBLE PRECISION
*          On entry, the overflow threshold as computed by DLAMCH.
*          On exit, if LOG10(LARGE) is sufficiently large, the square
*          root of LARGE, otherwise unchanged.
*
*  =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          LOG10, SQRT
*     ..
*     .. Executable Statements ..
*
*     If it looks like we're on a Cray, take the square root of
*     SMALL and LARGE to avoid overflow and underflow problems.
*
      IF( LOG10( LARGE ).GT.2000.D0 ) THEN
         SMALL = SQRT( SMALL )
         LARGE = SQRT( LARGE )
      END IF
*
      RETURN
*
*     End of DLABAD
*
      END
      SUBROUTINE DLABRD( M, N, NB, A, LDA, D, E, TAUQ, TAUP, X, LDX, Y,
     $                   LDY )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDX, LDY, M, N, NB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAUP( * ),
     $                   TAUQ( * ), X( LDX, * ), Y( LDY, * )
*     ..
*
*  Purpose
*  =======
*
*  DLABRD reduces the first NB rows and columns of a real general
*  m by n matrix A to upper or lower bidiagonal form by an orthogonal
*  transformation Q' * A * P, and returns the matrices X and Y which
*  are needed to apply the transformation to the unreduced part of A.
*
*  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
*  bidiagonal form.
*
*  This is an auxiliary routine called by DGEBRD
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the m by n general matrix to be reduced.
*          On exit, the first NB rows and columns of the matrix are
*          overwritten; the rest of the array is unchanged.
*          If m >= n, elements on and below the diagonal in the first NB
*            columns, with the array TAUQ, represent the orthogonal
*            matrix Q as a product of elementary reflectors; and
*            elements above the diagonal in the first NB rows, with the
*            array TAUP, represent the orthogonal matrix P as a product
*            of elementary reflectors.
*          If m < n, elements below the diagonal in the first NB
*            columns, with the array TAUQ, represent the orthogonal
*            matrix Q as a product of elementary reflectors, and
*            elements on and above the diagonal in the first NB rows,
*            with the array TAUP, represent the orthogonal matrix P as
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
*  TAUQ    (output) DOUBLE PRECISION array dimension (NB)
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix Q. See Further Details.
*
*  TAUP    (output) DOUBLE PRECISION array, dimension (NB)
*          The scalar factors of the elementary reflectors which
*          represent the orthogonal matrix P. See Further Details.
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NB)
*          The m-by-nb matrix X required to update the unreduced part
*          of A.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X. LDX >= M.
*
*  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
*          The n-by-nb matrix Y required to update the unreduced part
*          of A.
*
*  LDY     (input) INTEGER
*          The leading dimension of the array Y. LDY >= N.
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
*  where tauq and taup are real scalars, and v and u are real vectors.
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
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DLARFG, DSCAL
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
            CALL DGEMV( 'No transpose', M-I+1, I-1, -ONE, A( I, 1 ),
     $                  LDA, Y( I, 1 ), LDY, ONE, A( I, I ), 1 )
            CALL DGEMV( 'No transpose', M-I+1, I-1, -ONE, X( I, 1 ),
     $                  LDX, A( 1, I ), 1, ONE, A( I, I ), 1 )
*
*           Generate reflection Q(i) to annihilate A(i+1:m,i)
*
            CALL DLARFG( M-I+1, A( I, I ), A( MIN( I+1, M ), I ), 1,
     $                   TAUQ( I ) )
            D( I ) = A( I, I )
            IF( I.LT.N ) THEN
               A( I, I ) = ONE
*
*              Compute Y(i+1:n,i)
*
               CALL DGEMV( 'Transpose', M-I+1, N-I, ONE, A( I, I+1 ),
     $                     LDA, A( I, I ), 1, ZERO, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I+1, I-1, ONE, A( I, 1 ), LDA,
     $                     A( I, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I+1, I-1, ONE, X( I, 1 ), LDX,
     $                     A( I, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'Transpose', I-1, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
*
*              Update A(i,i+1:n)
*
               CALL DGEMV( 'No transpose', N-I, I, -ONE, Y( I+1, 1 ),
     $                     LDY, A( I, 1 ), LDA, ONE, A( I, I+1 ), LDA )
               CALL DGEMV( 'Transpose', I-1, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, X( I, 1 ), LDX, ONE, A( I, I+1 ), LDA )
*
*              Generate reflection P(i) to annihilate A(i,i+2:n)
*
               CALL DLARFG( N-I, A( I, I+1 ), A( I, MIN( I+2, N ) ),
     $                      LDA, TAUP( I ) )
               E( I ) = A( I, I+1 )
               A( I, I+1 ) = ONE
*
*              Compute X(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I, ONE, Y( I+1, 1 ), LDY,
     $                     A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', I-1, N-I, ONE, A( 1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
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
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, Y( I, 1 ),
     $                  LDY, A( I, 1 ), LDA, ONE, A( I, I ), LDA )
            CALL DGEMV( 'Transpose', I-1, N-I+1, -ONE, A( 1, I ), LDA,
     $                  X( I, 1 ), LDX, ONE, A( I, I ), LDA )
*
*           Generate reflection P(i) to annihilate A(i,i+1:n)
*
            CALL DLARFG( N-I+1, A( I, I ), A( I, MIN( I+1, N ) ), LDA,
     $                   TAUP( I ) )
            D( I ) = A( I, I )
            IF( I.LT.M ) THEN
               A( I, I ) = ONE
*
*              Compute X(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, N-I+1, ONE, A( I+1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I+1, I-1, ONE, Y( I, 1 ), LDY,
     $                     A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', I-1, N-I+1, ONE, A( 1, I ),
     $                     LDA, A( I, I ), LDA, ZERO, X( 1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, X( I+1, 1 ),
     $                     LDX, X( 1, I ), 1, ONE, X( I+1, I ), 1 )
               CALL DSCAL( M-I, TAUP( I ), X( I+1, I ), 1 )
*
*              Update A(i+1:m,i)
*
               CALL DGEMV( 'No transpose', M-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, Y( I, 1 ), LDY, ONE, A( I+1, I ), 1 )
               CALL DGEMV( 'No transpose', M-I, I, -ONE, X( I+1, 1 ),
     $                     LDX, A( 1, I ), 1, ONE, A( I+1, I ), 1 )
*
*              Generate reflection Q(i) to annihilate A(i+2:m,i)
*
               CALL DLARFG( M-I, A( I+1, I ), A( MIN( I+2, M ), I ), 1,
     $                      TAUQ( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
*
*              Compute Y(i+1:n,i)
*
               CALL DGEMV( 'Transpose', M-I, N-I, ONE, A( I+1, I+1 ),
     $                     LDA, A( I+1, I ), 1, ZERO, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I, I-1, ONE, A( I+1, 1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, Y( I+1, 1 ),
     $                     LDY, Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', M-I, I, ONE, X( I+1, 1 ), LDX,
     $                     A( I+1, I ), 1, ZERO, Y( 1, I ), 1 )
               CALL DGEMV( 'Transpose', I, N-I, -ONE, A( 1, I+1 ), LDA,
     $                     Y( 1, I ), 1, ONE, Y( I+1, I ), 1 )
               CALL DSCAL( N-I, TAUQ( I ), Y( I+1, I ), 1 )
            END IF
   20    CONTINUE
      END IF
      RETURN
*
*     End of DLABRD
*
      END
      SUBROUTINE DLACON( N, V, X, ISGN, EST, KASE )
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
      INTEGER            ISGN( * )
      DOUBLE PRECISION   V( * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLACON estimates the 1-norm of a square, real matrix A.
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
*         and DLACON must be re-called with all the other parameters
*         unchanged.
*
*  ISGN   (workspace) INTEGER array, dimension (N)
*
*  EST    (input/output) DOUBLE PRECISION
*         On entry with KASE = 1 or 2 and JUMP = 3, EST should be
*         unchanged from the previous call to DLACON.
*         On exit, EST is an estimate (a lower bound) for norm(A). 
*
*  KASE   (input/output) INTEGER
*         On the initial call to DLACON, KASE should be 0.
*         On an intermediate return, KASE will be 1 or 2, indicating
*         whether X should be overwritten by A * X  or A' * X.
*         On the final return from DLACON, KASE will again be 0.
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
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            ITMAX
      PARAMETER          ( ITMAX = 5 )
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ITER, J, JLAST, JUMP
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
*     .. Save statement ..
      SAVE
*     ..
*     .. Executable Statements ..
*
      IF( KASE.EQ.0 ) THEN
         DO 10 I = 1, N
            X( I ) = ONE / DBLE( N )
   10    CONTINUE
         KASE = 1
         JUMP = 1
         RETURN
      END IF
*
      GO TO ( 20, 40, 70, 110, 140 )JUMP
*
*     ................ ENTRY   (JUMP = 1)
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
      JUMP = 2
      RETURN
*
*     ................ ENTRY   (JUMP = 2)
*     FIRST ITERATION.  X HAS BEEN OVERWRITTEN BY TRANSPOSE(A)*X.
*
   40 CONTINUE
      J = IDAMAX( N, X, 1 )
      ITER = 2
*
*     MAIN LOOP - ITERATIONS 2,3,...,ITMAX.
*
   50 CONTINUE
      DO 60 I = 1, N
         X( I ) = ZERO
   60 CONTINUE
      X( J ) = ONE
      KASE = 1
      JUMP = 3
      RETURN
*
*     ................ ENTRY   (JUMP = 3)
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
      JUMP = 4
      RETURN
*
*     ................ ENTRY   (JUMP = 4)
*     X HAS BEEN OVERWRITTEN BY TRANSPOSE(A)*X.
*
  110 CONTINUE
      JLAST = J
      J = IDAMAX( N, X, 1 )
      IF( ( X( JLAST ).NE.ABS( X( J ) ) ) .AND. ( ITER.LT.ITMAX ) ) THEN
         ITER = ITER + 1
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
      JUMP = 5
      RETURN
*
*     ................ ENTRY   (JUMP = 5)
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
*     End of DLACON
*
      END
      SUBROUTINE DLACPY( UPLO, M, N, A, LDA, B, LDB )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDB, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DLACPY copies all or part of a two-dimensional matrix A to another
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
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The m by n matrix A.  If UPLO = 'U', only the upper triangle
*          or trapezoid is accessed; if UPLO = 'L', only the lower
*          triangle or trapezoid is accessed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (output) DOUBLE PRECISION array, dimension (LDB,N)
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
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
         DO 40 J = 1, N
            DO 30 I = J, M
               B( I, J ) = A( I, J )
   30       CONTINUE
   40    CONTINUE
      ELSE
         DO 60 J = 1, N
            DO 50 I = 1, M
               B( I, J ) = A( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
      RETURN
*
*     End of DLACPY
*
      END
      SUBROUTINE DLADIV( A, B, C, D, P, Q )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, D, P, Q
*     ..
*
*  Purpose
*  =======
*
*  DLADIV performs complex division in  real arithmetic
*
*                        a + i*b
*             p + i*q = ---------
*                        c + i*d
*
*  The algorithm is due to Robert L. Smith and can be found
*  in D. Knuth, The art of Computer Programming, Vol.2, p.195
*
*  Arguments
*  =========
*
*  A       (input) DOUBLE PRECISION
*  B       (input) DOUBLE PRECISION
*  C       (input) DOUBLE PRECISION
*  D       (input) DOUBLE PRECISION
*          The scalars a, b, c, and d in the above expression.
*
*  P       (output) DOUBLE PRECISION
*  Q       (output) DOUBLE PRECISION
*          The scalars p and q in the above expression.
*
*  =====================================================================
*
*     .. Local Scalars ..
      DOUBLE PRECISION   E, F
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      IF( ABS( D ).LT.ABS( C ) ) THEN
         E = D / C
         F = C + D*E
         P = ( A+B*E ) / F
         Q = ( B-A*E ) / F
      ELSE
         E = C / D
         F = D + C*E
         P = ( B+A*E ) / F
         Q = ( -A+B*E ) / F
      END IF
*
      RETURN
*
*     End of DLADIV
*
      END
      SUBROUTINE DLAE2( A, B, C, RT1, RT2 )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, RT1, RT2
*     ..
*
*  Purpose
*  =======
*
*  DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
*     [  A   B  ]
*     [  B   C  ].
*  On return, RT1 is the eigenvalue of larger absolute value, and RT2
*  is the eigenvalue of smaller absolute value.
*
*  Arguments
*  =========
*
*  A       (input) DOUBLE PRECISION
*          The (1,1) element of the 2-by-2 matrix.
*
*  B       (input) DOUBLE PRECISION
*          The (1,2) and (2,1) elements of the 2-by-2 matrix.
*
*  C       (input) DOUBLE PRECISION
*          The (2,2) element of the 2-by-2 matrix.
*
*  RT1     (output) DOUBLE PRECISION
*          The eigenvalue of larger absolute value.
*
*  RT2     (output) DOUBLE PRECISION
*          The eigenvalue of smaller absolute value.
*
*  Further Details
*  ===============
*
*  RT1 is accurate to a few ulps barring over/underflow.
*
*  RT2 may be inaccurate if there is massive cancellation in the
*  determinant A*C-B*B; higher precision or correctly rounded or
*  correctly truncated arithmetic would be needed to compute RT2
*  accurately in all cases.
*
*  Overflow is possible only if RT1 is within a factor of 5 of overflow.
*  Underflow is harmless if the input data is 0 or exceeds
*     underflow_threshold / macheps.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AB, ACMN, ACMX, ADF, DF, RT, SM, TB
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
*     ..
*     .. Executable Statements ..
*
*     Compute the eigenvalues
*
      SM = A + C
      DF = A - C
      ADF = ABS( DF )
      TB = B + B
      AB = ABS( TB )
      IF( ABS( A ).GT.ABS( C ) ) THEN
         ACMX = A
         ACMN = C
      ELSE
         ACMX = C
         ACMN = A
      END IF
      IF( ADF.GT.AB ) THEN
         RT = ADF*SQRT( ONE+( AB / ADF )**2 )
      ELSE IF( ADF.LT.AB ) THEN
         RT = AB*SQRT( ONE+( ADF / AB )**2 )
      ELSE
*
*        Includes case AB=ADF=0
*
         RT = AB*SQRT( TWO )
      END IF
      IF( SM.LT.ZERO ) THEN
         RT1 = HALF*( SM-RT )
*
*        Order of execution important.
*        To get fully accurate smaller eigenvalue,
*        next line needs to be executed in higher precision.
*
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE IF( SM.GT.ZERO ) THEN
         RT1 = HALF*( SM+RT )
*
*        Order of execution important.
*        To get fully accurate smaller eigenvalue,
*        next line needs to be executed in higher precision.
*
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE
*
*        Includes case RT1 = RT2 = 0
*
         RT1 = HALF*RT
         RT2 = -HALF*RT
      END IF
      RETURN
*
*     End of DLAE2
*
      END
      SUBROUTINE DLAEBZ( IJOB, NITMAX, N, MMAX, MINP, NBMIN, ABSTOL,
     $                   RELTOL, PIVMIN, D, E, E2, NVAL, AB, C, MOUT,
     $                   NAB, WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IJOB, INFO, MINP, MMAX, MOUT, N, NBMIN, NITMAX
      DOUBLE PRECISION   ABSTOL, PIVMIN, RELTOL
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * ), NAB( MMAX, * ), NVAL( * )
      DOUBLE PRECISION   AB( MMAX, * ), C( * ), D( * ), E( * ), E2( * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAEBZ contains the iteration loops which compute and use the
*  function N(w), which is the count of eigenvalues of a symmetric
*  tridiagonal matrix T less than or equal to its argument  w.  It
*  performs a choice of two types of loops:
*
*  IJOB=1, followed by
*  IJOB=2: It takes as input a list of intervals and returns a list of
*          sufficiently small intervals whose union contains the same
*          eigenvalues as the union of the original intervals.
*          The input intervals are (AB(j,1),AB(j,2)], j=1,...,MINP.
*          The output interval (AB(j,1),AB(j,2)] will contain
*          eigenvalues NAB(j,1)+1,...,NAB(j,2), where 1 <= j <= MOUT.
*
*  IJOB=3: It performs a binary search in each input interval
*          (AB(j,1),AB(j,2)] for a point  w(j)  such that
*          N(w(j))=NVAL(j), and uses  C(j)  as the starting point of
*          the search.  If such a w(j) is found, then on output
*          AB(j,1)=AB(j,2)=w.  If no such w(j) is found, then on output
*          (AB(j,1),AB(j,2)] will be a small interval containing the
*          point where N(w) jumps through NVAL(j), unless that point
*          lies outside the initial interval.
*
*  Note that the intervals are in all cases half-open intervals,
*  i.e., of the form  (a,b] , which includes  b  but not  a .
*
*  To avoid underflow, the matrix should be scaled so that its largest
*  element is no greater than  overflow**(1/2) * underflow**(1/4)
*  in absolute value.  To assure the most accurate computation
*  of small eigenvalues, the matrix should be scaled to be
*  not much smaller than that, either.
*
*  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
*  Matrix", Report CS41, Computer Science Dept., Stanford
*  University, July 21, 1966
*
*  Note: the arguments are, in general, *not* checked for unreasonable
*  values.
*
*  Arguments
*  =========
*
*  IJOB    (input) INTEGER
*          Specifies what is to be done:
*          = 1:  Compute NAB for the initial intervals.
*          = 2:  Perform bisection iteration to find eigenvalues of T.
*          = 3:  Perform bisection iteration to invert N(w), i.e.,
*                to find a point which has a specified number of
*                eigenvalues of T to its left.
*          Other values will cause DLAEBZ to return with INFO=-1.
*
*  NITMAX  (input) INTEGER
*          The maximum number of "levels" of bisection to be
*          performed, i.e., an interval of width W will not be made
*          smaller than 2^(-NITMAX) * W.  If not all intervals
*          have converged after NITMAX iterations, then INFO is set
*          to the number of non-converged intervals.
*
*  N       (input) INTEGER
*          The dimension n of the tridiagonal matrix T.  It must be at
*          least 1.
*
*  MMAX    (input) INTEGER
*          The maximum number of intervals.  If more than MMAX intervals
*          are generated, then DLAEBZ will quit with INFO=MMAX+1.
*
*  MINP    (input) INTEGER
*          The initial number of intervals.  It may not be greater than
*          MMAX.
*
*  NBMIN   (input) INTEGER
*          The smallest number of intervals that should be processed
*          using a vector loop.  If zero, then only the scalar loop
*          will be used.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The minimum (absolute) width of an interval.  When an
*          interval is narrower than ABSTOL, or than RELTOL times the
*          larger (in magnitude) endpoint, then it is considered to be
*          sufficiently small, i.e., converged.  This must be at least
*          zero.
*
*  RELTOL  (input) DOUBLE PRECISION
*          The minimum relative width of an interval.  When an interval
*          is narrower than ABSTOL, or than RELTOL times the larger (in
*          magnitude) endpoint, then it is considered to be
*          sufficiently small, i.e., converged.  Note: this should
*          always be at least radix*machine epsilon.
*
*  PIVMIN  (input) DOUBLE PRECISION
*          The minimum absolute value of a "pivot" in the Sturm
*          sequence loop.  This *must* be at least  max |e(j)**2| *
*          safe_min  and at least safe_min, where safe_min is at least
*          the smallest number that can divide one without overflow.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The diagonal elements of the tridiagonal matrix T.
*
*  E       (input) DOUBLE PRECISION array, dimension (N)
*          The offdiagonal elements of the tridiagonal matrix T in
*          positions 1 through N-1.  E(N) is arbitrary.
*
*  E2      (input) DOUBLE PRECISION array, dimension (N)
*          The squares of the offdiagonal elements of the tridiagonal
*          matrix T.  E2(N) is ignored.
*
*  NVAL    (input/output) INTEGER array, dimension (MINP)
*          If IJOB=1 or 2, not referenced.
*          If IJOB=3, the desired values of N(w).  The elements of NVAL
*          will be reordered to correspond with the intervals in AB.
*          Thus, NVAL(j) on output will not, in general be the same as
*          NVAL(j) on input, but it will correspond with the interval
*          (AB(j,1),AB(j,2)] on output.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (MMAX,2)
*          The endpoints of the intervals.  AB(j,1) is  a(j), the left
*          endpoint of the j-th interval, and AB(j,2) is b(j), the
*          right endpoint of the j-th interval.  The input intervals
*          will, in general, be modified, split, and reordered by the
*          calculation.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (MMAX)
*          If IJOB=1, ignored.
*          If IJOB=2, workspace.
*          If IJOB=3, then on input C(j) should be initialized to the
*          first search point in the binary search.
*
*  MOUT    (output) INTEGER
*          If IJOB=1, the number of eigenvalues in the intervals.
*          If IJOB=2 or 3, the number of intervals output.
*          If IJOB=3, MOUT will equal MINP.
*
*  NAB     (input/output) INTEGER array, dimension (MMAX,2)
*          If IJOB=1, then on output NAB(i,j) will be set to N(AB(i,j)).
*          If IJOB=2, then on input, NAB(i,j) should be set.  It must
*             satisfy the condition:
*             N(AB(i,1)) <= NAB(i,1) <= NAB(i,2) <= N(AB(i,2)),
*             which means that in interval i only eigenvalues
*             NAB(i,1)+1,...,NAB(i,2) will be considered.  Usually,
*             NAB(i,j)=N(AB(i,j)), from a previous call to DLAEBZ with
*             IJOB=1.
*             On output, NAB(i,j) will contain
*             max(na(k),min(nb(k),N(AB(i,j)))), where k is the index of
*             the input interval that the output interval
*             (AB(j,1),AB(j,2)] came from, and na(k) and nb(k) are the
*             the input values of NAB(k,1) and NAB(k,2).
*          If IJOB=3, then on output, NAB(i,j) contains N(AB(i,j)),
*             unless N(w) > NVAL(i) for all search points  w , in which
*             case NAB(i,1) will not be modified, i.e., the output
*             value will be the same as the input value (modulo
*             reorderings -- see NVAL and AB), or unless N(w) < NVAL(i)
*             for all search points  w , in which case NAB(i,2) will
*             not be modified.  Normally, NAB should be set to some
*             distinctive value(s) before DLAEBZ is called.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MMAX)
*          Workspace.
*
*  IWORK   (workspace) INTEGER array, dimension (MMAX)
*          Workspace.
*
*  INFO    (output) INTEGER
*          = 0:       All intervals converged.
*          = 1--MMAX: The last INFO intervals did not converge.
*          = MMAX+1:  More than MMAX intervals were generated.
*
*  Further Details
*  ===============
*
*      This routine is intended to be called only by other LAPACK
*  routines, thus the interface is less user-friendly.  It is intended
*  for two purposes:
*
*  (a) finding eigenvalues.  In this case, DLAEBZ should have one or
*      more initial intervals set up in AB, and DLAEBZ should be called
*      with IJOB=1.  This sets up NAB, and also counts the eigenvalues.
*      Intervals with no eigenvalues would usually be thrown out at
*      this point.  Also, if not all the eigenvalues in an interval i
*      are desired, NAB(i,1) can be increased or NAB(i,2) decreased.
*      For example, set NAB(i,1)=NAB(i,2)-1 to get the largest
*      eigenvalue.  DLAEBZ is then called with IJOB=2 and MMAX
*      no smaller than the value of MOUT returned by the call with
*      IJOB=1.  After this (IJOB=2) call, eigenvalues NAB(i,1)+1
*      through NAB(i,2) are approximately AB(i,1) (or AB(i,2)) to the
*      tolerance specified by ABSTOL and RELTOL.
*
*  (b) finding an interval (a',b'] containing eigenvalues w(f),...,w(l).
*      In this case, start with a Gershgorin interval  (a,b).  Set up
*      AB to contain 2 search intervals, both initially (a,b).  One
*      NVAL element should contain  f-1  and the other should contain  l
*      , while C should contain a and b, resp.  NAB(i,1) should be -1
*      and NAB(i,2) should be N+1, to flag an error if the desired
*      interval does not lie in (a,b).  DLAEBZ is then called with
*      IJOB=3.  On exit, if w(f-1) < w(f), then one of the intervals --
*      j -- will have AB(j,1)=AB(j,2) and NAB(j,1)=NAB(j,2)=f-1, while
*      if, to the specified tolerance, w(f-k)=...=w(f+r), k > 0 and r
*      >= 0, then the interval will have  N(AB(j,1))=NAB(j,1)=f-k and
*      N(AB(j,2))=NAB(j,2)=f+r.  The cases w(l) < w(l+1) and
*      w(l-r)=...=w(l+k) are handled similarly.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, TWO, HALF
      PARAMETER          ( ZERO = 0.0D0, TWO = 2.0D0,
     $                   HALF = 1.0D0 / TWO )
*     ..
*     .. Local Scalars ..
      INTEGER            ITMP1, ITMP2, J, JI, JIT, JP, KF, KFNEW, KL,
     $                   KLNEW
      DOUBLE PRECISION   TMP1, TMP2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Check for Errors
*
      INFO = 0
      IF( IJOB.LT.1 .OR. IJOB.GT.3 ) THEN
         INFO = -1
         RETURN
      END IF
*
*     Initialize NAB
*
      IF( IJOB.EQ.1 ) THEN
*
*        Compute the number of eigenvalues in the initial intervals.
*
         MOUT = 0
*DIR$ NOVECTOR
         DO 30 JI = 1, MINP
            DO 20 JP = 1, 2
               TMP1 = D( 1 ) - AB( JI, JP )
               IF( ABS( TMP1 ).LT.PIVMIN )
     $            TMP1 = -PIVMIN
               NAB( JI, JP ) = 0
               IF( TMP1.LE.ZERO )
     $            NAB( JI, JP ) = 1
*
               DO 10 J = 2, N
                  TMP1 = D( J ) - E2( J-1 ) / TMP1 - AB( JI, JP )
                  IF( ABS( TMP1 ).LT.PIVMIN )
     $               TMP1 = -PIVMIN
                  IF( TMP1.LE.ZERO )
     $               NAB( JI, JP ) = NAB( JI, JP ) + 1
   10          CONTINUE
   20       CONTINUE
            MOUT = MOUT + NAB( JI, 2 ) - NAB( JI, 1 )
   30    CONTINUE
         RETURN
      END IF
*
*     Initialize for loop
*
*     KF and KL have the following meaning:
*        Intervals 1,...,KF-1 have converged.
*        Intervals KF,...,KL  still need to be refined.
*
      KF = 1
      KL = MINP
*
*     If IJOB=2, initialize C.
*     If IJOB=3, use the user-supplied starting point.
*
      IF( IJOB.EQ.2 ) THEN
         DO 40 JI = 1, MINP
            C( JI ) = HALF*( AB( JI, 1 )+AB( JI, 2 ) )
   40    CONTINUE
      END IF
*
*     Iteration loop
*
      DO 130 JIT = 1, NITMAX
*
*        Loop over intervals
*
         IF( KL-KF+1.GE.NBMIN .AND. NBMIN.GT.0 ) THEN
*
*           Begin of Parallel Version of the loop
*
            DO 60 JI = KF, KL
*
*              Compute N(c), the number of eigenvalues less than c
*
               WORK( JI ) = D( 1 ) - C( JI )
               IWORK( JI ) = 0
               IF( WORK( JI ).LE.PIVMIN ) THEN
                  IWORK( JI ) = 1
                  WORK( JI ) = MIN( WORK( JI ), -PIVMIN )
               END IF
*
               DO 50 J = 2, N
                  WORK( JI ) = D( J ) - E2( J-1 ) / WORK( JI ) - C( JI )
                  IF( WORK( JI ).LE.PIVMIN ) THEN
                     IWORK( JI ) = IWORK( JI ) + 1
                     WORK( JI ) = MIN( WORK( JI ), -PIVMIN )
                  END IF
   50          CONTINUE
   60       CONTINUE
*
            IF( IJOB.LE.2 ) THEN
*
*              IJOB=2: Choose all intervals containing eigenvalues.
*
               KLNEW = KL
               DO 70 JI = KF, KL
*
*                 Insure that N(w) is monotone
*
                  IWORK( JI ) = MIN( NAB( JI, 2 ),
     $                          MAX( NAB( JI, 1 ), IWORK( JI ) ) )
*
*                 Update the Queue -- add intervals if both halves
*                 contain eigenvalues.
*
                  IF( IWORK( JI ).EQ.NAB( JI, 2 ) ) THEN
*
*                    No eigenvalue in the upper interval:
*                    just use the lower interval.
*
                     AB( JI, 2 ) = C( JI )
*
                  ELSE IF( IWORK( JI ).EQ.NAB( JI, 1 ) ) THEN
*
*                    No eigenvalue in the lower interval:
*                    just use the upper interval.
*
                     AB( JI, 1 ) = C( JI )
                  ELSE
                     KLNEW = KLNEW + 1
                     IF( KLNEW.LE.MMAX ) THEN
*
*                       Eigenvalue in both intervals -- add upper to
*                       queue.
*
                        AB( KLNEW, 2 ) = AB( JI, 2 )
                        NAB( KLNEW, 2 ) = NAB( JI, 2 )
                        AB( KLNEW, 1 ) = C( JI )
                        NAB( KLNEW, 1 ) = IWORK( JI )
                        AB( JI, 2 ) = C( JI )
                        NAB( JI, 2 ) = IWORK( JI )
                     ELSE
                        INFO = MMAX + 1
                     END IF
                  END IF
   70          CONTINUE
               IF( INFO.NE.0 )
     $            RETURN
               KL = KLNEW
            ELSE
*
*              IJOB=3: Binary search.  Keep only the interval containing
*                      w   s.t. N(w) = NVAL
*
               DO 80 JI = KF, KL
                  IF( IWORK( JI ).LE.NVAL( JI ) ) THEN
                     AB( JI, 1 ) = C( JI )
                     NAB( JI, 1 ) = IWORK( JI )
                  END IF
                  IF( IWORK( JI ).GE.NVAL( JI ) ) THEN
                     AB( JI, 2 ) = C( JI )
                     NAB( JI, 2 ) = IWORK( JI )
                  END IF
   80          CONTINUE
            END IF
*
         ELSE
*
*           End of Parallel Version of the loop
*
*           Begin of Serial Version of the loop
*
            KLNEW = KL
            DO 100 JI = KF, KL
*
*              Compute N(w), the number of eigenvalues less than w
*
               TMP1 = C( JI )
               TMP2 = D( 1 ) - TMP1
               ITMP1 = 0
               IF( TMP2.LE.PIVMIN ) THEN
                  ITMP1 = 1
                  TMP2 = MIN( TMP2, -PIVMIN )
               END IF
*
*              A series of compiler directives to defeat vectorization
*              for the next loop
*
*$PL$ CMCHAR=' '
CDIR$          NEXTSCALAR
C$DIR          SCALAR
CDIR$          NEXT SCALAR
CVD$L          NOVECTOR
CDEC$          NOVECTOR
CVD$           NOVECTOR
*VDIR          NOVECTOR
*VOCL          LOOP,SCALAR
CIBM           PREFER SCALAR
*$PL$ CMCHAR='*'
*
               DO 90 J = 2, N
                  TMP2 = D( J ) - E2( J-1 ) / TMP2 - TMP1
                  IF( TMP2.LE.PIVMIN ) THEN
                     ITMP1 = ITMP1 + 1
                     TMP2 = MIN( TMP2, -PIVMIN )
                  END IF
   90          CONTINUE
*
               IF( IJOB.LE.2 ) THEN
*
*                 IJOB=2: Choose all intervals containing eigenvalues.
*
*                 Insure that N(w) is monotone
*
                  ITMP1 = MIN( NAB( JI, 2 ),
     $                    MAX( NAB( JI, 1 ), ITMP1 ) )
*
*                 Update the Queue -- add intervals if both halves
*                 contain eigenvalues.
*
                  IF( ITMP1.EQ.NAB( JI, 2 ) ) THEN
*
*                    No eigenvalue in the upper interval:
*                    just use the lower interval.
*
                     AB( JI, 2 ) = TMP1
*
                  ELSE IF( ITMP1.EQ.NAB( JI, 1 ) ) THEN
*
*                    No eigenvalue in the lower interval:
*                    just use the upper interval.
*
                     AB( JI, 1 ) = TMP1
                  ELSE IF( KLNEW.LT.MMAX ) THEN
*
*                    Eigenvalue in both intervals -- add upper to queue.
*
                     KLNEW = KLNEW + 1
                     AB( KLNEW, 2 ) = AB( JI, 2 )
                     NAB( KLNEW, 2 ) = NAB( JI, 2 )
                     AB( KLNEW, 1 ) = TMP1
                     NAB( KLNEW, 1 ) = ITMP1
                     AB( JI, 2 ) = TMP1
                     NAB( JI, 2 ) = ITMP1
                  ELSE
                     INFO = MMAX + 1
                     RETURN
                  END IF
               ELSE
*
*                 IJOB=3: Binary search.  Keep only the interval
*                         containing  w  s.t. N(w) = NVAL
*
                  IF( ITMP1.LE.NVAL( JI ) ) THEN
                     AB( JI, 1 ) = TMP1
                     NAB( JI, 1 ) = ITMP1
                  END IF
                  IF( ITMP1.GE.NVAL( JI ) ) THEN
                     AB( JI, 2 ) = TMP1
                     NAB( JI, 2 ) = ITMP1
                  END IF
               END IF
  100       CONTINUE
            KL = KLNEW
*
*           End of Serial Version of the loop
*
         END IF
*
*        Check for convergence
*
         KFNEW = KF
         DO 110 JI = KF, KL
            TMP1 = ABS( AB( JI, 2 )-AB( JI, 1 ) )
            TMP2 = MAX( ABS( AB( JI, 2 ) ), ABS( AB( JI, 1 ) ) )
            IF( TMP1.LT.MAX( ABSTOL, PIVMIN, RELTOL*TMP2 ) .OR.
     $          NAB( JI, 1 ).GE.NAB( JI, 2 ) ) THEN
*
*              Converged -- Swap with position KFNEW,
*                           then increment KFNEW
*
               IF( JI.GT.KFNEW ) THEN
                  TMP1 = AB( JI, 1 )
                  TMP2 = AB( JI, 2 )
                  ITMP1 = NAB( JI, 1 )
                  ITMP2 = NAB( JI, 2 )
                  AB( JI, 1 ) = AB( KFNEW, 1 )
                  AB( JI, 2 ) = AB( KFNEW, 2 )
                  NAB( JI, 1 ) = NAB( KFNEW, 1 )
                  NAB( JI, 2 ) = NAB( KFNEW, 2 )
                  AB( KFNEW, 1 ) = TMP1
                  AB( KFNEW, 2 ) = TMP2
                  NAB( KFNEW, 1 ) = ITMP1
                  NAB( KFNEW, 2 ) = ITMP2
                  IF( IJOB.EQ.3 ) THEN
                     ITMP1 = NVAL( JI )
                     NVAL( JI ) = NVAL( KFNEW )
                     NVAL( KFNEW ) = ITMP1
                  END IF
               END IF
               KFNEW = KFNEW + 1
            END IF
  110    CONTINUE
         KF = KFNEW
*
*        Choose Midpoints
*
         DO 120 JI = KF, KL
            C( JI ) = HALF*( AB( JI, 1 )+AB( JI, 2 ) )
  120    CONTINUE
*
*        If no more intervals to refine, quit.
*
         IF( KF.GT.KL )
     $      GO TO 140
  130 CONTINUE
*
*     Converged
*
  140 CONTINUE
      INFO = MAX( KL+1-KF, 0 )
      MOUT = KL
*
      RETURN
*
*     End of DLAEBZ
*
      END
      SUBROUTINE DLAEIN( RIGHTV, NOINIT, N, H, LDH, WR, WI, VR, VI, B,
     $                   LDB, WORK, EPS3, SMLNUM, BIGNUM, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            NOINIT, RIGHTV
      INTEGER            INFO, LDB, LDH, N
      DOUBLE PRECISION   BIGNUM, EPS3, SMLNUM, WI, WR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   B( LDB, * ), H( LDH, * ), VI( * ), VR( * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAEIN uses inverse iteration to find a right or left eigenvector
*  corresponding to the eigenvalue (WR,WI) of a real upper Hessenberg
*  matrix H.
*
*  Arguments
*  =========
*
*  RIGHTV   (input) LOGICAL
*          = .TRUE. : compute right eigenvector;
*          = .FALSE.: compute left eigenvector.
*
*  NOINIT   (input) LOGICAL
*          = .TRUE. : no initial vector supplied in (VR,VI).
*          = .FALSE.: initial vector supplied in (VR,VI).
*
*  N       (input) INTEGER
*          The order of the matrix H.  N >= 0.
*
*  H       (input) DOUBLE PRECISION array, dimension (LDH,N)
*          The upper Hessenberg matrix H.
*
*  LDH     (input) INTEGER
*          The leading dimension of the array H.  LDH >= max(1,N).
*
*  WR      (input) DOUBLE PRECISION
*  WI      (input) DOUBLE PRECISION
*          The real and imaginary parts of the eigenvalue of H whose
*          corresponding right or left eigenvector is to be computed.
*
*  VR      (input/output) DOUBLE PRECISION array, dimension (N)
*  VI      (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, if NOINIT = .FALSE. and WI = 0.0, VR must contain
*          a real starting vector for inverse iteration using the real
*          eigenvalue WR; if NOINIT = .FALSE. and WI.ne.0.0, VR and VI
*          must contain the real and imaginary parts of a complex
*          starting vector for inverse iteration using the complex
*          eigenvalue (WR,WI); otherwise VR and VI need not be set.
*          On exit, if WI = 0.0 (real eigenvalue), VR contains the
*          computed real eigenvector; if WI.ne.0.0 (complex eigenvalue),
*          VR and VI contain the real and imaginary parts of the
*          computed complex eigenvector. The eigenvector is normalized
*          so that the component of largest magnitude has magnitude 1;
*          here the magnitude of a complex number (x,y) is taken to be
*          |x| + |y|.
*          VI is not referenced if WI = 0.0.
*
*  B       (workspace) DOUBLE PRECISION array, dimension (LDB,N)
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= N+1.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension (N)
*
*  EPS3    (input) DOUBLE PRECISION
*          A small machine-dependent value which is used to perturb
*          close eigenvalues, and to replace zero pivots.
*
*  SMLNUM  (input) DOUBLE PRECISION
*          A machine-dependent value close to the underflow threshold.
*
*  BIGNUM  (input) DOUBLE PRECISION
*          A machine-dependent value close to the overflow threshold.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          = 1:  inverse iteration did not converge; VR is set to the
*                last iterate, and so is VI if WI.ne.0.0.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TENTH
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TENTH = 1.0D-1 )
*     ..
*     .. Local Scalars ..
      CHARACTER          NORMIN, TRANS
      INTEGER            I, I1, I2, I3, IERR, ITS, J
      DOUBLE PRECISION   ABSBII, ABSBJJ, EI, EJ, GROWTO, NORM, NRMSML,
     $                   REC, ROOTN, SCALE, TEMP, VCRIT, VMAX, VNORM, W,
     $                   W1, X, XI, XR, Y
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM, DLAPY2, DNRM2
      EXTERNAL           IDAMAX, DASUM, DLAPY2, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLADIV, DLATRS, DSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
*     GROWTO is the threshold used in the acceptance test for an
*     eigenvector.
*
      ROOTN = SQRT( DBLE( N ) )
      GROWTO = TENTH / ROOTN
      NRMSML = MAX( ONE, EPS3*ROOTN )*SMLNUM
*
*     Form B = H - (WR,WI)*I (except that the subdiagonal elements and
*     the imaginary parts of the diagonal elements are not stored).
*
      DO 20 J = 1, N
         DO 10 I = 1, J - 1
            B( I, J ) = H( I, J )
   10    CONTINUE
         B( J, J ) = H( J, J ) - WR
   20 CONTINUE
*
      IF( WI.EQ.ZERO ) THEN
*
*        Real eigenvalue.
*
         IF( NOINIT ) THEN
*
*           Set initial vector.
*
            DO 30 I = 1, N
               VR( I ) = EPS3
   30       CONTINUE
         ELSE
*
*           Scale supplied initial vector.
*
            VNORM = DNRM2( N, VR, 1 )
            CALL DSCAL( N, ( EPS3*ROOTN ) / MAX( VNORM, NRMSML ), VR,
     $                  1 )
         END IF
*
         IF( RIGHTV ) THEN
*
*           LU decomposition with partial pivoting of B, replacing zero
*           pivots by EPS3.
*
            DO 60 I = 1, N - 1
               EI = H( I+1, I )
               IF( ABS( B( I, I ) ).LT.ABS( EI ) ) THEN
*
*                 Interchange rows and eliminate.
*
                  X = B( I, I ) / EI
                  B( I, I ) = EI
                  DO 40 J = I + 1, N
                     TEMP = B( I+1, J )
                     B( I+1, J ) = B( I, J ) - X*TEMP
                     B( I, J ) = TEMP
   40             CONTINUE
               ELSE
*
*                 Eliminate without interchange.
*
                  IF( B( I, I ).EQ.ZERO )
     $               B( I, I ) = EPS3
                  X = EI / B( I, I )
                  IF( X.NE.ZERO ) THEN
                     DO 50 J = I + 1, N
                        B( I+1, J ) = B( I+1, J ) - X*B( I, J )
   50                CONTINUE
                  END IF
               END IF
   60       CONTINUE
            IF( B( N, N ).EQ.ZERO )
     $         B( N, N ) = EPS3
*
            TRANS = 'N'
*
         ELSE
*
*           UL decomposition with partial pivoting of B, replacing zero
*           pivots by EPS3.
*
            DO 90 J = N, 2, -1
               EJ = H( J, J-1 )
               IF( ABS( B( J, J ) ).LT.ABS( EJ ) ) THEN
*
*                 Interchange columns and eliminate.
*
                  X = B( J, J ) / EJ
                  B( J, J ) = EJ
                  DO 70 I = 1, J - 1
                     TEMP = B( I, J-1 )
                     B( I, J-1 ) = B( I, J ) - X*TEMP
                     B( I, J ) = TEMP
   70             CONTINUE
               ELSE
*
*                 Eliminate without interchange.
*
                  IF( B( J, J ).EQ.ZERO )
     $               B( J, J ) = EPS3
                  X = EJ / B( J, J )
                  IF( X.NE.ZERO ) THEN
                     DO 80 I = 1, J - 1
                        B( I, J-1 ) = B( I, J-1 ) - X*B( I, J )
   80                CONTINUE
                  END IF
               END IF
   90       CONTINUE
            IF( B( 1, 1 ).EQ.ZERO )
     $         B( 1, 1 ) = EPS3
*
            TRANS = 'T'
*
         END IF
*
         NORMIN = 'N'
         DO 110 ITS = 1, N
*
*           Solve U*x = scale*v for a right eigenvector
*             or U'*x = scale*v for a left eigenvector,
*           overwriting x on v.
*
            CALL DLATRS( 'Upper', TRANS, 'Nonunit', NORMIN, N, B, LDB,
     $                   VR, SCALE, WORK, IERR )
            NORMIN = 'Y'
*
*           Test for sufficient growth in the norm of v.
*
            VNORM = DASUM( N, VR, 1 )
            IF( VNORM.GE.GROWTO*SCALE )
     $         GO TO 120
*
*           Choose new orthogonal starting vector and try again.
*
            TEMP = EPS3 / ( ROOTN+ONE )
            VR( 1 ) = EPS3
            DO 100 I = 2, N
               VR( I ) = TEMP
  100       CONTINUE
            VR( N-ITS+1 ) = VR( N-ITS+1 ) - EPS3*ROOTN
  110    CONTINUE
*
*        Failure to find eigenvector in N iterations.
*
         INFO = 1
*
  120    CONTINUE
*
*        Normalize eigenvector.
*
         I = IDAMAX( N, VR, 1 )
         CALL DSCAL( N, ONE / ABS( VR( I ) ), VR, 1 )
      ELSE
*
*        Complex eigenvalue.
*
         IF( NOINIT ) THEN
*
*           Set initial vector.
*
            DO 130 I = 1, N
               VR( I ) = EPS3
               VI( I ) = ZERO
  130       CONTINUE
         ELSE
*
*           Scale supplied initial vector.
*
            NORM = DLAPY2( DNRM2( N, VR, 1 ), DNRM2( N, VI, 1 ) )
            REC = ( EPS3*ROOTN ) / MAX( NORM, NRMSML )
            CALL DSCAL( N, REC, VR, 1 )
            CALL DSCAL( N, REC, VI, 1 )
         END IF
*
         IF( RIGHTV ) THEN
*
*           LU decomposition with partial pivoting of B, replacing zero
*           pivots by EPS3.
*
*           The imaginary part of the (i,j)-th element of U is stored in
*           B(j+1,i).
*
            B( 2, 1 ) = -WI
            DO 140 I = 2, N
               B( I+1, 1 ) = ZERO
  140       CONTINUE
*
            DO 170 I = 1, N - 1
               ABSBII = DLAPY2( B( I, I ), B( I+1, I ) )
               EI = H( I+1, I )
               IF( ABSBII.LT.ABS( EI ) ) THEN
*
*                 Interchange rows and eliminate.
*
                  XR = B( I, I ) / EI
                  XI = B( I+1, I ) / EI
                  B( I, I ) = EI
                  B( I+1, I ) = ZERO
                  DO 150 J = I + 1, N
                     TEMP = B( I+1, J )
                     B( I+1, J ) = B( I, J ) - XR*TEMP
                     B( J+1, I+1 ) = B( J+1, I ) - XI*TEMP
                     B( I, J ) = TEMP
                     B( J+1, I ) = ZERO
  150             CONTINUE
                  B( I+2, I ) = -WI
                  B( I+1, I+1 ) = B( I+1, I+1 ) - XI*WI
                  B( I+2, I+1 ) = B( I+2, I+1 ) + XR*WI
               ELSE
*
*                 Eliminate without interchanging rows.
*
                  IF( ABSBII.EQ.ZERO ) THEN
                     B( I, I ) = EPS3
                     B( I+1, I ) = ZERO
                     ABSBII = EPS3
                  END IF
                  EI = ( EI / ABSBII ) / ABSBII
                  XR = B( I, I )*EI
                  XI = -B( I+1, I )*EI
                  DO 160 J = I + 1, N
                     B( I+1, J ) = B( I+1, J ) - XR*B( I, J ) +
     $                             XI*B( J+1, I )
                     B( J+1, I+1 ) = -XR*B( J+1, I ) - XI*B( I, J )
  160             CONTINUE
                  B( I+2, I+1 ) = B( I+2, I+1 ) - WI
               END IF
*
*              Compute 1-norm of offdiagonal elements of i-th row.
*
               WORK( I ) = DASUM( N-I, B( I, I+1 ), LDB ) +
     $                     DASUM( N-I, B( I+2, I ), 1 )
  170       CONTINUE
            IF( B( N, N ).EQ.ZERO .AND. B( N+1, N ).EQ.ZERO )
     $         B( N, N ) = EPS3
            WORK( N ) = ZERO
*
            I1 = N
            I2 = 1
            I3 = -1
         ELSE
*
*           UL decomposition with partial pivoting of conjg(B),
*           replacing zero pivots by EPS3.
*
*           The imaginary part of the (i,j)-th element of U is stored in
*           B(j+1,i).
*
            B( N+1, N ) = WI
            DO 180 J = 1, N - 1
               B( N+1, J ) = ZERO
  180       CONTINUE
*
            DO 210 J = N, 2, -1
               EJ = H( J, J-1 )
               ABSBJJ = DLAPY2( B( J, J ), B( J+1, J ) )
               IF( ABSBJJ.LT.ABS( EJ ) ) THEN
*
*                 Interchange columns and eliminate
*
                  XR = B( J, J ) / EJ
                  XI = B( J+1, J ) / EJ
                  B( J, J ) = EJ
                  B( J+1, J ) = ZERO
                  DO 190 I = 1, J - 1
                     TEMP = B( I, J-1 )
                     B( I, J-1 ) = B( I, J ) - XR*TEMP
                     B( J, I ) = B( J+1, I ) - XI*TEMP
                     B( I, J ) = TEMP
                     B( J+1, I ) = ZERO
  190             CONTINUE
                  B( J+1, J-1 ) = WI
                  B( J-1, J-1 ) = B( J-1, J-1 ) + XI*WI
                  B( J, J-1 ) = B( J, J-1 ) - XR*WI
               ELSE
*
*                 Eliminate without interchange.
*
                  IF( ABSBJJ.EQ.ZERO ) THEN
                     B( J, J ) = EPS3
                     B( J+1, J ) = ZERO
                     ABSBJJ = EPS3
                  END IF
                  EJ = ( EJ / ABSBJJ ) / ABSBJJ
                  XR = B( J, J )*EJ
                  XI = -B( J+1, J )*EJ
                  DO 200 I = 1, J - 1
                     B( I, J-1 ) = B( I, J-1 ) - XR*B( I, J ) +
     $                             XI*B( J+1, I )
                     B( J, I ) = -XR*B( J+1, I ) - XI*B( I, J )
  200             CONTINUE
                  B( J, J-1 ) = B( J, J-1 ) + WI
               END IF
*
*              Compute 1-norm of offdiagonal elements of j-th column.
*
               WORK( J ) = DASUM( J-1, B( 1, J ), 1 ) +
     $                     DASUM( J-1, B( J+1, 1 ), LDB )
  210       CONTINUE
            IF( B( 1, 1 ).EQ.ZERO .AND. B( 2, 1 ).EQ.ZERO )
     $         B( 1, 1 ) = EPS3
            WORK( 1 ) = ZERO
*
            I1 = 1
            I2 = N
            I3 = 1
         END IF
*
         DO 270 ITS = 1, N
            SCALE = ONE
            VMAX = ONE
            VCRIT = BIGNUM
*
*           Solve U*(xr,xi) = scale*(vr,vi) for a right eigenvector,
*             or U'*(xr,xi) = scale*(vr,vi) for a left eigenvector,
*           overwriting (xr,xi) on (vr,vi).
*
            DO 250 I = I1, I2, I3
*
               IF( WORK( I ).GT.VCRIT ) THEN
                  REC = ONE / VMAX
                  CALL DSCAL( N, REC, VR, 1 )
                  CALL DSCAL( N, REC, VI, 1 )
                  SCALE = SCALE*REC
                  VMAX = ONE
                  VCRIT = BIGNUM
               END IF
*
               XR = VR( I )
               XI = VI( I )
               IF( RIGHTV ) THEN
                  DO 220 J = I + 1, N
                     XR = XR - B( I, J )*VR( J ) + B( J+1, I )*VI( J )
                     XI = XI - B( I, J )*VI( J ) - B( J+1, I )*VR( J )
  220             CONTINUE
               ELSE
                  DO 230 J = 1, I - 1
                     XR = XR - B( J, I )*VR( J ) + B( I+1, J )*VI( J )
                     XI = XI - B( J, I )*VI( J ) - B( I+1, J )*VR( J )
  230             CONTINUE
               END IF
*
               W = ABS( B( I, I ) ) + ABS( B( I+1, I ) )
               IF( W.GT.SMLNUM ) THEN
                  IF( W.LT.ONE ) THEN
                     W1 = ABS( XR ) + ABS( XI )
                     IF( W1.GT.W*BIGNUM ) THEN
                        REC = ONE / W1
                        CALL DSCAL( N, REC, VR, 1 )
                        CALL DSCAL( N, REC, VI, 1 )
                        XR = VR( I )
                        XI = VI( I )
                        SCALE = SCALE*REC
                        VMAX = VMAX*REC
                     END IF
                  END IF
*
*                 Divide by diagonal element of B.
*
                  CALL DLADIV( XR, XI, B( I, I ), B( I+1, I ), VR( I ),
     $                         VI( I ) )
                  VMAX = MAX( ABS( VR( I ) )+ABS( VI( I ) ), VMAX )
                  VCRIT = BIGNUM / VMAX
               ELSE
                  DO 240 J = 1, N
                     VR( J ) = ZERO
                     VI( J ) = ZERO
  240             CONTINUE
                  VR( I ) = ONE
                  VI( I ) = ONE
                  SCALE = ZERO
                  VMAX = ONE
                  VCRIT = BIGNUM
               END IF
  250       CONTINUE
*
*           Test for sufficient growth in the norm of (VR,VI).
*
            VNORM = DASUM( N, VR, 1 ) + DASUM( N, VI, 1 )
            IF( VNORM.GE.GROWTO*SCALE )
     $         GO TO 280
*
*           Choose a new orthogonal starting vector and try again.
*
            Y = EPS3 / ( ROOTN+ONE )
            VR( 1 ) = EPS3
            VI( 1 ) = ZERO
*
            DO 260 I = 2, N
               VR( I ) = Y
               VI( I ) = ZERO
  260       CONTINUE
            VR( N-ITS+1 ) = VR( N-ITS+1 ) - EPS3*ROOTN
  270    CONTINUE
*
*        Failure to find eigenvector in N iterations
*
         INFO = 1
*
  280    CONTINUE
*
*        Normalize eigenvector.
*
         VNORM = ZERO
         DO 290 I = 1, N
            VNORM = MAX( VNORM, ABS( VR( I ) )+ABS( VI( I ) ) )
  290    CONTINUE
         CALL DSCAL( N, ONE / VNORM, VR, 1 )
         CALL DSCAL( N, ONE / VNORM, VI, 1 )
*
      END IF
*
      RETURN
*
*     End of DLAEIN
*
      END
      SUBROUTINE DLAEV2( A, B, C, RT1, RT2, CS1, SN1 )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, CS1, RT1, RT2, SN1
*     ..
*
*  Purpose
*  =======
*
*  DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix
*     [  A   B  ]
*     [  B   C  ].
*  On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
*  eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
*  eigenvector for RT1, giving the decomposition
*
*     [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
*     [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].
*
*  Arguments
*  =========
*
*  A       (input) DOUBLE PRECISION
*          The (1,1) element of the 2-by-2 matrix.
*
*  B       (input) DOUBLE PRECISION
*          The (1,2) element and the conjugate of the (2,1) element of
*          the 2-by-2 matrix.
*
*  C       (input) DOUBLE PRECISION
*          The (2,2) element of the 2-by-2 matrix.
*
*  RT1     (output) DOUBLE PRECISION
*          The eigenvalue of larger absolute value.
*
*  RT2     (output) DOUBLE PRECISION
*          The eigenvalue of smaller absolute value.
*
*  CS1     (output) DOUBLE PRECISION
*  SN1     (output) DOUBLE PRECISION
*          The vector (CS1, SN1) is a unit right eigenvector for RT1.
*
*  Further Details
*  ===============
*
*  RT1 is accurate to a few ulps barring over/underflow.
*
*  RT2 may be inaccurate if there is massive cancellation in the
*  determinant A*C-B*B; higher precision or correctly rounded or
*  correctly truncated arithmetic would be needed to compute RT2
*  accurately in all cases.
*
*  CS1 and SN1 are accurate to a few ulps barring over/underflow.
*
*  Overflow is possible only if RT1 is within a factor of 5 of overflow.
*  Underflow is harmless if the input data is 0 or exceeds
*     underflow_threshold / macheps.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            SGN1, SGN2
      DOUBLE PRECISION   AB, ACMN, ACMX, ACS, ADF, CS, CT, DF, RT, SM,
     $                   TB, TN
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
*     ..
*     .. Executable Statements ..
*
*     Compute the eigenvalues
*
      SM = A + C
      DF = A - C
      ADF = ABS( DF )
      TB = B + B
      AB = ABS( TB )
      IF( ABS( A ).GT.ABS( C ) ) THEN
         ACMX = A
         ACMN = C
      ELSE
         ACMX = C
         ACMN = A
      END IF
      IF( ADF.GT.AB ) THEN
         RT = ADF*SQRT( ONE+( AB / ADF )**2 )
      ELSE IF( ADF.LT.AB ) THEN
         RT = AB*SQRT( ONE+( ADF / AB )**2 )
      ELSE
*
*        Includes case AB=ADF=0
*
         RT = AB*SQRT( TWO )
      END IF
      IF( SM.LT.ZERO ) THEN
         RT1 = HALF*( SM-RT )
         SGN1 = -1
*
*        Order of execution important.
*        To get fully accurate smaller eigenvalue,
*        next line needs to be executed in higher precision.
*
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE IF( SM.GT.ZERO ) THEN
         RT1 = HALF*( SM+RT )
         SGN1 = 1
*
*        Order of execution important.
*        To get fully accurate smaller eigenvalue,
*        next line needs to be executed in higher precision.
*
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE
*
*        Includes case RT1 = RT2 = 0
*
         RT1 = HALF*RT
         RT2 = -HALF*RT
         SGN1 = 1
      END IF
*
*     Compute the eigenvector
*
      IF( DF.GE.ZERO ) THEN
         CS = DF + RT
         SGN2 = 1
      ELSE
         CS = DF - RT
         SGN2 = -1
      END IF
      ACS = ABS( CS )
      IF( ACS.GT.AB ) THEN
         CT = -TB / CS
         SN1 = ONE / SQRT( ONE+CT*CT )
         CS1 = CT*SN1
      ELSE
         IF( AB.EQ.ZERO ) THEN
            CS1 = ONE
            SN1 = ZERO
         ELSE
            TN = -CS / TB
            CS1 = ONE / SQRT( ONE+TN*TN )
            SN1 = TN*CS1
         END IF
      END IF
      IF( SGN1.EQ.SGN2 ) THEN
         TN = CS1
         CS1 = -SN1
         SN1 = TN
      END IF
      RETURN
*
*     End of DLAEV2
*
      END
      SUBROUTINE DLAEXC( WANTQ, N, T, LDT, Q, LDQ, J1, N1, N2, WORK,
     $                   INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            WANTQ
      INTEGER            INFO, J1, LDQ, LDT, N, N1, N2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Q( LDQ, * ), T( LDT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAEXC swaps adjacent diagonal blocks T11 and T22 of order 1 or 2 in
*  an upper quasi-triangular matrix T by an orthogonal similarity
*  transformation.
*
*  T must be in Schur canonical form, that is, block upper triangular
*  with 1-by-1 and 2-by-2 diagonal blocks; each 2-by-2 diagonal block
*  has its diagonal elemnts equal and its off-diagonal elements of
*  opposite sign.
*
*  Arguments
*  =========
*
*  WANTQ   (input) LOGICAL
*          = .TRUE. : accumulate the transformation in the matrix Q;
*          = .FALSE.: do not accumulate the transformation.
*
*  N       (input) INTEGER
*          The order of the matrix T. N >= 0.
*
*  T       (input/output) DOUBLE PRECISION array, dimension (LDT,N)
*          On entry, the upper quasi-triangular matrix T, in Schur
*          canonical form.
*          On exit, the updated matrix T, again in Schur canonical form.
*
*  LDT     (input)  INTEGER
*          The leading dimension of the array T. LDT >= max(1,N).
*
*  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
*          On entry, if WANTQ is .TRUE., the orthogonal matrix Q.
*          On exit, if WANTQ is .TRUE., the updated matrix Q.
*          If WANTQ is .FALSE., Q is not referenced.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.
*          LDQ >= 1; and if WANTQ is .TRUE., LDQ >= N.
*
*  J1      (input) INTEGER
*          The index of the first row of the first block T11.
*
*  N1      (input) INTEGER
*          The order of the first block T11. N1 = 0, 1 or 2.
*
*  N2      (input) INTEGER
*          The order of the second block T22. N2 = 0, 1 or 2.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          = 1: the transformed matrix T would be too far from Schur
*               form; the blocks are not swapped and T and Q are
*               unchanged.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 1.0D+1 )
      INTEGER            LDD, LDX
      PARAMETER          ( LDD = 4, LDX = 2 )
*     ..
*     .. Local Scalars ..
      INTEGER            IERR, J2, J3, J4, K, ND
      DOUBLE PRECISION   CS, DNORM, EPS, SCALE, SMLNUM, SN, T11, T22,
     $                   T33, TAU, TAU1, TAU2, TEMP, THRESH, WI1, WI2,
     $                   WR1, WR2, XNORM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   D( LDD, 4 ), U( 3 ), U1( 3 ), U2( 3 ),
     $                   X( LDX, 2 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLACPY, DLANV2, DLARFG, DLARFX, DLARTG, DLASY2,
     $                   DROT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. N1.EQ.0 .OR. N2.EQ.0 )
     $   RETURN
      IF( J1+N1.GT.N )
     $   RETURN
*
      J2 = J1 + 1
      J3 = J1 + 2
      J4 = J1 + 3
*
      IF( N1.EQ.1 .AND. N2.EQ.1 ) THEN
*
*        Swap two 1-by-1 blocks.
*
         T11 = T( J1, J1 )
         T22 = T( J2, J2 )
*
*        Determine the transformation to perform the interchange.
*
         CALL DLARTG( T( J1, J2 ), T22-T11, CS, SN, TEMP )
*
*        Apply transformation to the matrix T.
*
         IF( J3.LE.N )
     $      CALL DROT( N-J1-1, T( J1, J3 ), LDT, T( J2, J3 ), LDT, CS,
     $                 SN )
         CALL DROT( J1-1, T( 1, J1 ), 1, T( 1, J2 ), 1, CS, SN )
*
         T( J1, J1 ) = T22
         T( J2, J2 ) = T11
*
         IF( WANTQ ) THEN
*
*           Accumulate transformation in the matrix Q.
*
            CALL DROT( N, Q( 1, J1 ), 1, Q( 1, J2 ), 1, CS, SN )
         END IF
*
      ELSE
*
*        Swapping involves at least one 2-by-2 block.
*
*        Copy the diagonal block of order N1+N2 to the local array D
*        and compute its norm.
*
         ND = N1 + N2
         CALL DLACPY( 'Full', ND, ND, T( J1, J1 ), LDT, D, LDD )
         DNORM = DLANGE( 'Max', ND, ND, D, LDD, WORK )
*
*        Compute machine-dependent threshold for test for accepting
*        swap.
*
         EPS = DLAMCH( 'P' )
         SMLNUM = DLAMCH( 'S' ) / EPS
         THRESH = MAX( TEN*EPS*DNORM, SMLNUM )
*
*        Solve T11*X - X*T22 = scale*T12 for X.
*
         CALL DLASY2( .FALSE., .FALSE., -1, N1, N2, D, LDD,
     $                D( N1+1, N1+1 ), LDD, D( 1, N1+1 ), LDD, SCALE, X,
     $                LDX, XNORM, IERR )
*
*        Swap the adjacent diagonal blocks.
*
         K = N1 + N1 + N2 - 3
         GO TO ( 10, 20, 30 )K
*
   10    CONTINUE
*
*        N1 = 1, N2 = 2: generate elementary reflector H so that:
*
*        ( scale, X11, X12 ) H = ( 0, 0, * )
*
         U( 1 ) = SCALE
         U( 2 ) = X( 1, 1 )
         U( 3 ) = X( 1, 2 )
         CALL DLARFG( 3, U( 3 ), U, 1, TAU )
         U( 3 ) = ONE
         T11 = T( J1, J1 )
*
*        Perform swap provisionally on diagonal block in D.
*
         CALL DLARFX( 'L', 3, 3, U, TAU, D, LDD, WORK )
         CALL DLARFX( 'R', 3, 3, U, TAU, D, LDD, WORK )
*
*        Test whether to reject swap.
*
         IF( MAX( ABS( D( 3, 1 ) ), ABS( D( 3, 2 ) ), ABS( D( 3,
     $       3 )-T11 ) ).GT.THRESH )GO TO 50
*
*        Accept swap: apply transformation to the entire matrix T.
*
         CALL DLARFX( 'L', 3, N-J1+1, U, TAU, T( J1, J1 ), LDT, WORK )
         CALL DLARFX( 'R', J2, 3, U, TAU, T( 1, J1 ), LDT, WORK )
*
         T( J3, J1 ) = ZERO
         T( J3, J2 ) = ZERO
         T( J3, J3 ) = T11
*
         IF( WANTQ ) THEN
*
*           Accumulate transformation in the matrix Q.
*
            CALL DLARFX( 'R', N, 3, U, TAU, Q( 1, J1 ), LDQ, WORK )
         END IF
         GO TO 40
*
   20    CONTINUE
*
*        N1 = 2, N2 = 1: generate elementary reflector H so that:
*
*        H (  -X11 ) = ( * )
*          (  -X21 ) = ( 0 )
*          ( scale ) = ( 0 )
*
         U( 1 ) = -X( 1, 1 )
         U( 2 ) = -X( 2, 1 )
         U( 3 ) = SCALE
         CALL DLARFG( 3, U( 1 ), U( 2 ), 1, TAU )
         U( 1 ) = ONE
         T33 = T( J3, J3 )
*
*        Perform swap provisionally on diagonal block in D.
*
         CALL DLARFX( 'L', 3, 3, U, TAU, D, LDD, WORK )
         CALL DLARFX( 'R', 3, 3, U, TAU, D, LDD, WORK )
*
*        Test whether to reject swap.
*
         IF( MAX( ABS( D( 2, 1 ) ), ABS( D( 3, 1 ) ), ABS( D( 1,
     $       1 )-T33 ) ).GT.THRESH )GO TO 50
*
*        Accept swap: apply transformation to the entire matrix T.
*
         CALL DLARFX( 'R', J3, 3, U, TAU, T( 1, J1 ), LDT, WORK )
         CALL DLARFX( 'L', 3, N-J1, U, TAU, T( J1, J2 ), LDT, WORK )
*
         T( J1, J1 ) = T33
         T( J2, J1 ) = ZERO
         T( J3, J1 ) = ZERO
*
         IF( WANTQ ) THEN
*
*           Accumulate transformation in the matrix Q.
*
            CALL DLARFX( 'R', N, 3, U, TAU, Q( 1, J1 ), LDQ, WORK )
         END IF
         GO TO 40
*
   30    CONTINUE
*
*        N1 = 2, N2 = 2: generate elementary reflectors H(1) and H(2) so
*        that:
*
*        H(2) H(1) (  -X11  -X12 ) = (  *  * )
*                  (  -X21  -X22 )   (  0  * )
*                  ( scale    0  )   (  0  0 )
*                  (    0  scale )   (  0  0 )
*
         U1( 1 ) = -X( 1, 1 )
         U1( 2 ) = -X( 2, 1 )
         U1( 3 ) = SCALE
         CALL DLARFG( 3, U1( 1 ), U1( 2 ), 1, TAU1 )
         U1( 1 ) = ONE
*
         TEMP = -TAU1*( X( 1, 2 )+U1( 2 )*X( 2, 2 ) )
         U2( 1 ) = -TEMP*U1( 2 ) - X( 2, 2 )
         U2( 2 ) = -TEMP*U1( 3 )
         U2( 3 ) = SCALE
         CALL DLARFG( 3, U2( 1 ), U2( 2 ), 1, TAU2 )
         U2( 1 ) = ONE
*
*        Perform swap provisionally on diagonal block in D.
*
         CALL DLARFX( 'L', 3, 4, U1, TAU1, D, LDD, WORK )
         CALL DLARFX( 'R', 4, 3, U1, TAU1, D, LDD, WORK )
         CALL DLARFX( 'L', 3, 4, U2, TAU2, D( 2, 1 ), LDD, WORK )
         CALL DLARFX( 'R', 4, 3, U2, TAU2, D( 1, 2 ), LDD, WORK )
*
*        Test whether to reject swap.
*
         IF( MAX( ABS( D( 3, 1 ) ), ABS( D( 3, 2 ) ), ABS( D( 4, 1 ) ),
     $       ABS( D( 4, 2 ) ) ).GT.THRESH )GO TO 50
*
*        Accept swap: apply transformation to the entire matrix T.
*
         CALL DLARFX( 'L', 3, N-J1+1, U1, TAU1, T( J1, J1 ), LDT, WORK )
         CALL DLARFX( 'R', J4, 3, U1, TAU1, T( 1, J1 ), LDT, WORK )
         CALL DLARFX( 'L', 3, N-J1+1, U2, TAU2, T( J2, J1 ), LDT, WORK )
         CALL DLARFX( 'R', J4, 3, U2, TAU2, T( 1, J2 ), LDT, WORK )
*
         T( J3, J1 ) = ZERO
         T( J3, J2 ) = ZERO
         T( J4, J1 ) = ZERO
         T( J4, J2 ) = ZERO
*
         IF( WANTQ ) THEN
*
*           Accumulate transformation in the matrix Q.
*
            CALL DLARFX( 'R', N, 3, U1, TAU1, Q( 1, J1 ), LDQ, WORK )
            CALL DLARFX( 'R', N, 3, U2, TAU2, Q( 1, J2 ), LDQ, WORK )
         END IF
*
   40    CONTINUE
*
         IF( N2.EQ.2 ) THEN
*
*           Standardize new 2-by-2 block T11
*
            CALL DLANV2( T( J1, J1 ), T( J1, J2 ), T( J2, J1 ),
     $                   T( J2, J2 ), WR1, WI1, WR2, WI2, CS, SN )
            CALL DROT( N-J1-1, T( J1, J1+2 ), LDT, T( J2, J1+2 ), LDT,
     $                 CS, SN )
            CALL DROT( J1-1, T( 1, J1 ), 1, T( 1, J2 ), 1, CS, SN )
            IF( WANTQ )
     $         CALL DROT( N, Q( 1, J1 ), 1, Q( 1, J2 ), 1, CS, SN )
         END IF
*
         IF( N1.EQ.2 ) THEN
*
*           Standardize new 2-by-2 block T22
*
            J3 = J1 + N2
            J4 = J3 + 1
            CALL DLANV2( T( J3, J3 ), T( J3, J4 ), T( J4, J3 ),
     $                   T( J4, J4 ), WR1, WI1, WR2, WI2, CS, SN )
            IF( J3+2.LE.N )
     $         CALL DROT( N-J3-1, T( J3, J3+2 ), LDT, T( J4, J3+2 ),
     $                    LDT, CS, SN )
            CALL DROT( J3-1, T( 1, J3 ), 1, T( 1, J4 ), 1, CS, SN )
            IF( WANTQ )
     $         CALL DROT( N, Q( 1, J3 ), 1, Q( 1, J4 ), 1, CS, SN )
         END IF
*
      END IF
      RETURN
*
*     Exit with INFO = 1 if swap was rejected.
*
   50 CONTINUE
      INFO = 1
      RETURN
*
*     End of DLAEXC
*
      END
      SUBROUTINE DLAG2( A, LDA, B, LDB, SAFMIN, SCALE1, SCALE2, WR1,
     $                  WR2, WI )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDB
      DOUBLE PRECISION   SAFMIN, SCALE1, SCALE2, WI, WR1, WR2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DLAG2 computes the eigenvalues of a 2 x 2 generalized eigenvalue
*  problem  A - w B, with scaling as necessary to avoid over-/underflow.
*
*  The scaling factor "s" results in a modified eigenvalue equation
*
*      s A - w B
*
*  where  s  is a non-negative scaling factor chosen so that  w,  w B,
*  and  s A  do not overflow and, if possible, do not underflow, either.
*
*  Arguments
*  =========
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA, 2)
*          On entry, the 2 x 2 matrix A.  It is assumed that its 1-norm
*          is less than 1/SAFMIN.  Entries less than
*          sqrt(SAFMIN)*norm(A) are subject to being treated as zero.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= 2.
*
*  B       (input) DOUBLE PRECISION array, dimension (LDB, 2)
*          On entry, the 2 x 2 upper triangular matrix B.  It is
*          assumed that the one-norm of B is less than 1/SAFMIN.  The
*          diagonals should be at least sqrt(SAFMIN) times the largest
*          element of B (in absolute value); if a diagonal is smaller
*          than that, then  +/- sqrt(SAFMIN) will be used instead of
*          that diagonal.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= 2.
*
*  SAFMIN  (input) DOUBLE PRECISION
*          The smallest positive number s.t. 1/SAFMIN does not
*          overflow.  (This should always be DLAMCH('S') -- it is an
*          argument in order to avoid having to call DLAMCH frequently.)
*
*  SCALE1  (output) DOUBLE PRECISION
*          A scaling factor used to avoid over-/underflow in the
*          eigenvalue equation which defines the first eigenvalue.  If
*          the eigenvalues are complex, then the eigenvalues are
*          ( WR1  +/-  WI i ) / SCALE1  (which may lie outside the
*          exponent range of the machine), SCALE1=SCALE2, and SCALE1
*          will always be positive.  If the eigenvalues are real, then
*          the first (real) eigenvalue is  WR1 / SCALE1 , but this may
*          overflow or underflow, and in fact, SCALE1 may be zero or
*          less than the underflow threshhold if the exact eigenvalue
*          is sufficiently large.
*
*  SCALE2  (output) DOUBLE PRECISION
*          A scaling factor used to avoid over-/underflow in the
*          eigenvalue equation which defines the second eigenvalue.  If
*          the eigenvalues are complex, then SCALE2=SCALE1.  If the
*          eigenvalues are real, then the second (real) eigenvalue is
*          WR2 / SCALE2 , but this may overflow or underflow, and in
*          fact, SCALE2 may be zero or less than the underflow
*          threshhold if the exact eigenvalue is sufficiently large.
*
*  WR1     (output) DOUBLE PRECISION
*          If the eigenvalue is real, then WR1 is SCALE1 times the
*          eigenvalue closest to the (2,2) element of A B**(-1).  If the
*          eigenvalue is complex, then WR1=WR2 is SCALE1 times the real
*          part of the eigenvalues.
*
*  WR2     (output) DOUBLE PRECISION
*          If the eigenvalue is real, then WR2 is SCALE2 times the
*          other eigenvalue.  If the eigenvalue is complex, then
*          WR1=WR2 is SCALE1 times the real part of the eigenvalues.
*
*  WI      (output) DOUBLE PRECISION
*          If the eigenvalue is real, then WI is zero.  If the
*          eigenvalue is complex, then WI is SCALE1 times the imaginary
*          part of the eigenvalues.  WI will always be non-negative.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = ONE / TWO )
      DOUBLE PRECISION   FUZZY1
      PARAMETER          ( FUZZY1 = ONE+1.0D-5 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   A11, A12, A21, A22, ABI22, ANORM, AS11, AS12,
     $                   AS22, ASCALE, B11, B12, B22, BINV11, BINV22,
     $                   BMIN, BNORM, BSCALE, BSIZE, C1, C2, C3, C4, C5,
     $                   DIFF, DISCR, PP, QQ, R, RTMAX, RTMIN, S1, S2,
     $                   SAFMAX, SHIFT, SS, SUM, WABS, WBIG, WDET,
     $                   WSCALE, WSIZE, WSMALL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
      RTMIN = SQRT( SAFMIN )
      RTMAX = ONE / RTMIN
      SAFMAX = ONE / SAFMIN
*
*     Scale A
*
      ANORM = MAX( ABS( A( 1, 1 ) )+ABS( A( 2, 1 ) ),
     $        ABS( A( 1, 2 ) )+ABS( A( 2, 2 ) ), SAFMIN )
      ASCALE = ONE / ANORM
      A11 = ASCALE*A( 1, 1 )
      A21 = ASCALE*A( 2, 1 )
      A12 = ASCALE*A( 1, 2 )
      A22 = ASCALE*A( 2, 2 )
*
*     Perturb B if necessary to insure non-singularity
*
      B11 = B( 1, 1 )
      B12 = B( 1, 2 )
      B22 = B( 2, 2 )
      BMIN = RTMIN*MAX( ABS( B11 ), ABS( B12 ), ABS( B22 ), RTMIN )
      IF( ABS( B11 ).LT.BMIN )
     $   B11 = SIGN( BMIN, B11 )
      IF( ABS( B22 ).LT.BMIN )
     $   B22 = SIGN( BMIN, B22 )
*
*     Scale B
*
      BNORM = MAX( ABS( B11 ), ABS( B12 )+ABS( B22 ), SAFMIN )
      BSIZE = MAX( ABS( B11 ), ABS( B22 ) )
      BSCALE = ONE / BSIZE
      B11 = B11*BSCALE
      B12 = B12*BSCALE
      B22 = B22*BSCALE
*
*     Compute larger eigenvalue by method described by C. van Loan
*
*     ( AS is A shifted by -SHIFT*B )
*
      BINV11 = ONE / B11
      BINV22 = ONE / B22
      S1 = A11*BINV11
      S2 = A22*BINV22
      IF( ABS( S1 ).LE.ABS( S2 ) ) THEN
         AS12 = A12 - S1*B12
         AS22 = A22 - S1*B22
         SS = A21*( BINV11*BINV22 )
         ABI22 = AS22*BINV22 - SS*B12
         PP = HALF*ABI22
         SHIFT = S1
      ELSE
         AS12 = A12 - S2*B12
         AS11 = A11 - S2*B11
         SS = A21*( BINV11*BINV22 )
         ABI22 = -SS*B12
         PP = HALF*( AS11*BINV11+ABI22 )
         SHIFT = S2
      END IF
      QQ = SS*AS12
      IF( ABS( PP*RTMIN ).GE.ONE ) THEN
         DISCR = ( RTMIN*PP )**2 + QQ*SAFMIN
         R = SQRT( ABS( DISCR ) )*RTMAX
      ELSE
         IF( PP**2+ABS( QQ ).LE.SAFMIN ) THEN
            DISCR = ( RTMAX*PP )**2 + QQ*SAFMAX
            R = SQRT( ABS( DISCR ) )*RTMIN
         ELSE
            DISCR = PP**2 + QQ
            R = SQRT( ABS( DISCR ) )
         END IF
      END IF
*
*     Note: the test of R in the following IF is to cover the case when
*           DISCR is small and negative and is flushed to zero during
*           the calculation of R.  On machines which have a consistent
*           flush-to-zero threshhold and handle numbers above that
*           threshhold correctly, it would not be necessary.
*
      IF( DISCR.GE.ZERO .OR. R.EQ.ZERO ) THEN
         SUM = PP + SIGN( R, PP )
         DIFF = PP - SIGN( R, PP )
         WBIG = SHIFT + SUM
*
*        Compute smaller eigenvalue
*
         WSMALL = SHIFT + DIFF
         IF( HALF*ABS( WBIG ).GT.MAX( ABS( WSMALL ), SAFMIN ) ) THEN
            WDET = ( A11*A22-A12*A21 )*( BINV11*BINV22 )
            WSMALL = WDET / WBIG
         END IF
*
*        Choose (real) eigenvalue closest to 2,2 element of A*B**(-1)
*        for WR1.
*
         IF( PP.GT.ABI22 ) THEN
            WR1 = MIN( WBIG, WSMALL )
            WR2 = MAX( WBIG, WSMALL )
         ELSE
            WR1 = MAX( WBIG, WSMALL )
            WR2 = MIN( WBIG, WSMALL )
         END IF
         WI = ZERO
      ELSE
*
*        Complex eigenvalues
*
         WR1 = SHIFT + PP
         WR2 = WR1
         WI = R
      END IF
*
*     Further scaling to avoid underflow and overflow in computing
*     SCALE1 and overflow in computing w*B.
*
*     This scale factor (WSCALE) is bounded from above using C1 and C2,
*     and from below using C3 and C4.
*        C1 implements the condition  s A  must never overflow.
*        C2 implements the condition  w B  must never overflow.
*        C3, with C2,
*           implement the condition that s A - w B must never overflow.
*        C4 implements the condition  s    should not underflow.
*        C5 implements the condition  max(s,|w|) should be at least 2.
*
      C1 = BSIZE*( SAFMIN*MAX( ONE, ASCALE ) )
      C2 = SAFMIN*MAX( ONE, BNORM )
      C3 = BSIZE*SAFMIN
      IF( ASCALE.LE.ONE .AND. BSIZE.LE.ONE ) THEN
         C4 = MIN( ONE, ( ASCALE / SAFMIN )*BSIZE )
      ELSE
         C4 = ONE
      END IF
      IF( ASCALE.LE.ONE .OR. BSIZE.LE.ONE ) THEN
         C5 = MIN( ONE, ASCALE*BSIZE )
      ELSE
         C5 = ONE
      END IF
*
*     Scale first eigenvalue
*
      WABS = ABS( WR1 ) + ABS( WI )
      WSIZE = MAX( SAFMIN, C1, FUZZY1*( WABS*C2+C3 ),
     $        MIN( C4, HALF*MAX( WABS, C5 ) ) )
      IF( WSIZE.NE.ONE ) THEN
         WSCALE = ONE / WSIZE
         IF( WSIZE.GT.ONE ) THEN
            SCALE1 = ( MAX( ASCALE, BSIZE )*WSCALE )*
     $               MIN( ASCALE, BSIZE )
         ELSE
            SCALE1 = ( MIN( ASCALE, BSIZE )*WSCALE )*
     $               MAX( ASCALE, BSIZE )
         END IF
         WR1 = WR1*WSCALE
         IF( WI.NE.ZERO ) THEN
            WI = WI*WSCALE
            WR2 = WR1
            SCALE2 = SCALE1
         END IF
      ELSE
         SCALE1 = ASCALE*BSIZE
         SCALE2 = SCALE1
      END IF
*
*     Scale second eigenvalue (if real)
*
      IF( WI.EQ.ZERO ) THEN
         WSIZE = MAX( SAFMIN, C1, FUZZY1*( ABS( WR2 )*C2+C3 ),
     $           MIN( C4, HALF*MAX( ABS( WR2 ), C5 ) ) )
         IF( WSIZE.NE.ONE ) THEN
            WSCALE = ONE / WSIZE
            IF( WSIZE.GT.ONE ) THEN
               SCALE2 = ( MAX( ASCALE, BSIZE )*WSCALE )*
     $                  MIN( ASCALE, BSIZE )
            ELSE
               SCALE2 = ( MIN( ASCALE, BSIZE )*WSCALE )*
     $                  MAX( ASCALE, BSIZE )
            END IF
            WR2 = WR2*WSCALE
         ELSE
            SCALE2 = ASCALE*BSIZE
         END IF
      END IF
*
*     End of DLAG2
*
      RETURN
      END
      SUBROUTINE DLAGS2( UPPER, A1, A2, A3, B1, B2, B3, CSU, SNU, CSV,
     $                   SNV, CSQ, SNQ )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            UPPER
      DOUBLE PRECISION   A1, A2, A3, B1, B2, B3, CSQ, CSU, CSV, SNQ,
     $                   SNU, SNV
*     ..
*
*  Purpose
*  =======
*
*  DLAGS2 computes 2-by-2 orthogonal matrices U, V and Q, such
*  that if ( UPPER ) then
*
*            U'*A*Q = U'*( A1 A2 )*Q = ( x  0  )
*                        ( 0  A3 )     ( x  x  )
*  and
*            V'*B*Q = V'*( B1 B2 )*Q = ( x  0  )
*                        ( 0  B3 )     ( x  x  )
*
*  or if ( .NOT.UPPER ) then
*
*            U'*A*Q = U'*( A1 0  )*Q = ( x  x  )
*                        ( A2 A3 )     ( 0  x  )
*  and
*            V'*B*Q = V'*( B1 0  )*Q = ( x  x  )
*                        ( B2 B3 )     ( 0  x  )
*
*  The rows of the transformed A and B are parallel, where
*
*    U = (  CSU  SNU ), V = (  CSV SNV ), Q = (  CSQ   SNQ )
*        ( -SNU  CSU )      ( -SNV CSV )      ( -SNQ   CSQ )
*
*  Z' denotes the transpose of Z.
*
*
*  Arguments
*  =========
*
*  UPPER   (input) LOGICAL
*          = .TRUE.: the input matrices A and B are upper triangular.
*          = .FALSE.: the input matrices A and B are lower triangular.
*
*  A1      (input) DOUBLE PRECISION
*  A2      (input) DOUBLE PRECISION
*  A3      (input) DOUBLE PRECISION
*          On entry, A1, A2 and A3 are elements of the input 2-by-2
*          upper (lower) triangular matrix A.
*
*  B1      (input) DOUBLE PRECISION
*  B2      (input) DOUBLE PRECISION
*  B3      (input) DOUBLE PRECISION
*          On entry, B1, B2 and B3 are elements of the input 2-by-2
*          upper (lower) triangular matrix B.
*
*  CSU     (output) DOUBLE PRECISION
*  SNU     (output) DOUBLE PRECISION
*          The desired orthogonal matrix U.
*
*  CSV     (output) DOUBLE PRECISION
*  SNV     (output) DOUBLE PRECISION
*          The desired orthogonal matrix V.
*
*  CSQ     (output) DOUBLE PRECISION
*  SNQ     (output) DOUBLE PRECISION
*          The desired orthogonal matrix Q.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   A, AUA11, AUA12, AUA21, AUA22, AVB11, AVB12,
     $                   AVB21, AVB22, B, C, CSL, CSR, D, R, S1, S2,
     $                   SNL, SNR, UA11, UA11R, UA12, UA21, UA22, UA22R,
     $                   VB11, VB11R, VB12, VB21, VB22, VB22R
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARTG, DLASV2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      IF( UPPER ) THEN
*
*        Input matrices A and B are upper triangular matrices
*
*        Form matrix C = A*adj(B) = ( a b )
*                                   ( 0 d )
*
         A = A1*B3
         D = A3*B1
         B = A2*B1 - A1*B2
*
*        The SVD of real 2-by-2 triangular C
*
*         ( CSL -SNL )*( A B )*(  CSR  SNR ) = ( R 0 )
*         ( SNL  CSL ) ( 0 D ) ( -SNR  CSR )   ( 0 T )
*
         CALL DLASV2( A, B, D, S1, S2, SNR, CSR, SNL, CSL )
*
         IF( ABS( CSL ).GE.ABS( SNL ) .OR. ABS( CSR ).GE.ABS( SNR ) )
     $        THEN
*
*           Compute the (1,1) and (1,2) elements of U'*A and V'*B,
*           and (1,2) element of |U|'*|A| and |V|'*|B|.
*
            UA11R = CSL*A1
            UA12 = CSL*A2 + SNL*A3
*
            VB11R = CSR*B1
            VB12 = CSR*B2 + SNR*B3
*
            AUA12 = ABS( CSL )*ABS( A2 ) + ABS( SNL )*ABS( A3 )
            AVB12 = ABS( CSR )*ABS( B2 ) + ABS( SNR )*ABS( B3 )
*
*           zero (1,2) elements of U'*A and V'*B
*
            IF( ( ABS( UA11R )+ABS( UA12 ) ).NE.ZERO ) THEN
               IF( AUA12 / ( ABS( UA11R )+ABS( UA12 ) ).LE.AVB12 /
     $             ( ABS( VB11R )+ABS( VB12 ) ) ) THEN
                  CALL DLARTG( -UA11R, UA12, CSQ, SNQ, R )
               ELSE
                  CALL DLARTG( -VB11R, VB12, CSQ, SNQ, R )
               END IF
            ELSE
               CALL DLARTG( -VB11R, VB12, CSQ, SNQ, R )
            END IF
*
            CSU = CSL
            SNU = -SNL
            CSV = CSR
            SNV = -SNR
*
         ELSE
*
*           Compute the (2,1) and (2,2) elements of U'*A and V'*B,
*           and (2,2) element of |U|'*|A| and |V|'*|B|.
*
            UA21 = -SNL*A1
            UA22 = -SNL*A2 + CSL*A3
*
            VB21 = -SNR*B1
            VB22 = -SNR*B2 + CSR*B3
*
            AUA22 = ABS( SNL )*ABS( A2 ) + ABS( CSL )*ABS( A3 )
            AVB22 = ABS( SNR )*ABS( B2 ) + ABS( CSR )*ABS( B3 )
*
*           zero (2,2) elements of U'*A and V'*B, and then swap.
*
            IF( ( ABS( UA21 )+ABS( UA22 ) ).NE.ZERO ) THEN
               IF( AUA22 / ( ABS( UA21 )+ABS( UA22 ) ).LE.AVB22 /
     $             ( ABS( VB21 )+ABS( VB22 ) ) ) THEN
                  CALL DLARTG( -UA21, UA22, CSQ, SNQ, R )
               ELSE
                  CALL DLARTG( -VB21, VB22, CSQ, SNQ, R )
               END IF
            ELSE
               CALL DLARTG( -VB21, VB22, CSQ, SNQ, R )
            END IF
*
            CSU = SNL
            SNU = CSL
            CSV = SNR
            SNV = CSR
*
         END IF
*
      ELSE
*
*        Input matrices A and B are lower triangular matrices
*
*        Form matrix C = A*adj(B) = ( a 0 )
*                                   ( c d )
*
         A = A1*B3
         D = A3*B1
         C = A2*B3 - A3*B2
*
*        The SVD of real 2-by-2 triangular C
*
*         ( CSL -SNL )*( A 0 )*(  CSR  SNR ) = ( R 0 )
*         ( SNL  CSL ) ( C D ) ( -SNR  CSR )   ( 0 T )
*
         CALL DLASV2( A, C, D, S1, S2, SNR, CSR, SNL, CSL )
*
         IF( ABS( CSR ).GE.ABS( SNR ) .OR. ABS( CSL ).GE.ABS( SNL ) )
     $        THEN
*
*           Compute the (2,1) and (2,2) elements of U'*A and V'*B,
*           and (2,1) element of |U|'*|A| and |V|'*|B|.
*
            UA21 = -SNR*A1 + CSR*A2
            UA22R = CSR*A3
*
            VB21 = -SNL*B1 + CSL*B2
            VB22R = CSL*B3
*
            AUA21 = ABS( SNR )*ABS( A1 ) + ABS( CSR )*ABS( A2 )
            AVB21 = ABS( SNL )*ABS( B1 ) + ABS( CSL )*ABS( B2 )
*
*           zero (2,1) elements of U'*A and V'*B.
*
            IF( ( ABS( UA21 )+ABS( UA22R ) ).NE.ZERO ) THEN
               IF( AUA21 / ( ABS( UA21 )+ABS( UA22R ) ).LE.AVB21 /
     $             ( ABS( VB21 )+ABS( VB22R ) ) ) THEN
                  CALL DLARTG( UA22R, UA21, CSQ, SNQ, R )
               ELSE
                  CALL DLARTG( VB22R, VB21, CSQ, SNQ, R )
               END IF
            ELSE
               CALL DLARTG( VB22R, VB21, CSQ, SNQ, R )
            END IF
*
            CSU = CSR
            SNU = -SNR
            CSV = CSL
            SNV = -SNL
*
         ELSE
*
*           Compute the (1,1) and (1,2) elements of U'*A and V'*B,
*           and (1,1) element of |U|'*|A| and |V|'*|B|.
*
            UA11 = CSR*A1 + SNR*A2
            UA12 = SNR*A3
*
            VB11 = CSL*B1 + SNL*B2
            VB12 = SNL*B3
*
            AUA11 = ABS( CSR )*ABS( A1 ) + ABS( SNR )*ABS( A2 )
            AVB11 = ABS( CSL )*ABS( B1 ) + ABS( SNL )*ABS( B2 )
*
*           zero (1,1) elements of U'*A and V'*B, and then swap.
*
            IF( ( ABS( UA11 )+ABS( UA12 ) ).NE.ZERO ) THEN
               IF( AUA11 / ( ABS( UA11 )+ABS( UA12 ) ).LE.AVB11 /
     $             ( ABS( VB11 )+ABS( VB12 ) ) ) THEN
                  CALL DLARTG( UA12, UA11, CSQ, SNQ, R )
               ELSE
                  CALL DLARTG( VB12, VB11, CSQ, SNQ, R )
               END IF
            ELSE
               CALL DLARTG( VB12, VB11, CSQ, SNQ, R )
            END IF
*
            CSU = SNR
            SNU = CSR
            CSV = SNL
            SNV = CSL
*
         END IF
*
      END IF
*
      RETURN
*
*     End of DLAGS2
*
      END
      SUBROUTINE DLAGTM( TRANS, N, NRHS, ALPHA, DL, D, DU, X, LDX, BETA,
     $                   B, LDB )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            LDB, LDX, N, NRHS
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * ),
     $                   X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DLAGTM performs a matrix-vector product of the form
*
*     B := alpha * A * X + beta * B
*
*  where A is a tridiagonal matrix of order N, B and X are N by NRHS
*  matrices, and alpha and beta are real scalars, each of which may be
*  0., 1., or -1.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          Specifies the operation applied to A.
*          = 'N':  No transpose, B := alpha * A * X + beta * B
*          = 'T':  Transpose,    B := alpha * A'* X + beta * B
*          = 'C':  Conjugate transpose = Transpose
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices X and B.
*
*  ALPHA   (input) DOUBLE PRECISION
*          The scalar alpha.  ALPHA must be 0., 1., or -1.; otherwise,
*          it is assumed to be 0.
*
*  DL      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) sub-diagonal elements of T.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The diagonal elements of T.
*
*  DU      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) super-diagonal elements of T.
*
*  X       (input) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          The N by NRHS matrix X.
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(N,1).
*
*  BETA    (input) DOUBLE PRECISION
*          The scalar beta.  BETA must be 0., 1., or -1.; otherwise,
*          it is assumed to be 1.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N by NRHS matrix B.
*          On exit, B is overwritten by the matrix expression
*          B := alpha * A * X + beta * B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(N,1).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Executable Statements ..
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Multiply B by BETA if BETA.NE.1.
*
      IF( BETA.EQ.ZERO ) THEN
         DO 20 J = 1, NRHS
            DO 10 I = 1, N
               B( I, J ) = ZERO
   10       CONTINUE
   20    CONTINUE
      ELSE IF( BETA.EQ.-ONE ) THEN
         DO 40 J = 1, NRHS
            DO 30 I = 1, N
               B( I, J ) = -B( I, J )
   30       CONTINUE
   40    CONTINUE
      END IF
*
      IF( ALPHA.EQ.ONE ) THEN
         IF( LSAME( TRANS, 'N' ) ) THEN
*
*           Compute B := B + A*X
*
            DO 60 J = 1, NRHS
               IF( N.EQ.1 ) THEN
                  B( 1, J ) = B( 1, J ) + D( 1 )*X( 1, J )
               ELSE
                  B( 1, J ) = B( 1, J ) + D( 1 )*X( 1, J ) +
     $                        DU( 1 )*X( 2, J )
                  B( N, J ) = B( N, J ) + DL( N-1 )*X( N-1, J ) +
     $                        D( N )*X( N, J )
                  DO 50 I = 2, N - 1
                     B( I, J ) = B( I, J ) + DL( I-1 )*X( I-1, J ) +
     $                           D( I )*X( I, J ) + DU( I )*X( I+1, J )
   50             CONTINUE
               END IF
   60       CONTINUE
         ELSE
*
*           Compute B := B + A'*X
*
            DO 80 J = 1, NRHS
               IF( N.EQ.1 ) THEN
                  B( 1, J ) = B( 1, J ) + D( 1 )*X( 1, J )
               ELSE
                  B( 1, J ) = B( 1, J ) + D( 1 )*X( 1, J ) +
     $                        DL( 1 )*X( 2, J )
                  B( N, J ) = B( N, J ) + DU( N-1 )*X( N-1, J ) +
     $                        D( N )*X( N, J )
                  DO 70 I = 2, N - 1
                     B( I, J ) = B( I, J ) + DU( I-1 )*X( I-1, J ) +
     $                           D( I )*X( I, J ) + DL( I )*X( I+1, J )
   70             CONTINUE
               END IF
   80       CONTINUE
         END IF
      ELSE IF( ALPHA.EQ.-ONE ) THEN
         IF( LSAME( TRANS, 'N' ) ) THEN
*
*           Compute B := B - A*X
*
            DO 100 J = 1, NRHS
               IF( N.EQ.1 ) THEN
                  B( 1, J ) = B( 1, J ) - D( 1 )*X( 1, J )
               ELSE
                  B( 1, J ) = B( 1, J ) - D( 1 )*X( 1, J ) -
     $                        DU( 1 )*X( 2, J )
                  B( N, J ) = B( N, J ) - DL( N-1 )*X( N-1, J ) -
     $                        D( N )*X( N, J )
                  DO 90 I = 2, N - 1
                     B( I, J ) = B( I, J ) - DL( I-1 )*X( I-1, J ) -
     $                           D( I )*X( I, J ) - DU( I )*X( I+1, J )
   90             CONTINUE
               END IF
  100       CONTINUE
         ELSE
*
*           Compute B := B - A'*X
*
            DO 120 J = 1, NRHS
               IF( N.EQ.1 ) THEN
                  B( 1, J ) = B( 1, J ) - D( 1 )*X( 1, J )
               ELSE
                  B( 1, J ) = B( 1, J ) - D( 1 )*X( 1, J ) -
     $                        DL( 1 )*X( 2, J )
                  B( N, J ) = B( N, J ) - DU( N-1 )*X( N-1, J ) -
     $                        D( N )*X( N, J )
                  DO 110 I = 2, N - 1
                     B( I, J ) = B( I, J ) - DU( I-1 )*X( I-1, J ) -
     $                           D( I )*X( I, J ) - DL( I )*X( I+1, J )
  110             CONTINUE
               END IF
  120       CONTINUE
         END IF
      END IF
      RETURN
*
*     End of DLAGTM
*
      END
      SUBROUTINE DLAGTS( JOB, N, A, B, C, D, IN, Y, TOL, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, JOB, N
      DOUBLE PRECISION   TOL
*     ..
*     .. Array Arguments ..
      INTEGER            IN( * )
      DOUBLE PRECISION   A( * ), B( * ), C( * ), D( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAGTS may be used to solve one of the systems of equations
*
*     (T - lambda*I)*x = y   or   (T - lambda*I)'*x = y,
*
*  where T is an n by n tridiagonal matrix, for x, following the
*  factorization of (T - lambda*I) as
*
*     (T - lambda*I) = P*L*U ,
*
*  by routine DLAGTF. The choice of equation to be solved is
*  controlled by the argument JOB, and in each case there is an option
*  to perturb zero or very small diagonal elements of U, this option
*  being intended for use in applications such as inverse iteration.
*
*  Arguments
*  =========
*
*  JOB     (input) INTEGER
*          Specifies the job to be performed by DLAGTS as follows:
*          =  1: The equations  (T - lambda*I)x = y  are to be solved,
*                but diagonal elements of U are not to be perturbed.
*          = -1: The equations  (T - lambda*I)x = y  are to be solved
*                and, if overflow would otherwise occur, the diagonal
*                elements of U are to be perturbed. See argument TOL
*                below.
*          =  2: The equations  (T - lambda*I)'x = y  are to be solved,
*                but diagonal elements of U are not to be perturbed.
*          = -2: The equations  (T - lambda*I)'x = y  are to be solved
*                and, if overflow would otherwise occur, the diagonal
*                elements of U are to be perturbed. See argument TOL
*                below.
*
*  N       (input) INTEGER
*          The order of the matrix T.
*
*  A       (input) DOUBLE PRECISION array, dimension (N)
*          On entry, A must contain the diagonal elements of U as
*          returned from DLAGTF.
*
*  B       (input) DOUBLE PRECISION array, dimension (N-1)
*          On entry, B must contain the first super-diagonal elements of
*          U as returned from DLAGTF.
*
*  C       (input) DOUBLE PRECISION array, dimension (N-1)
*          On entry, C must contain the sub-diagonal elements of L as
*          returned from DLAGTF.
*
*  D       (input) DOUBLE PRECISION array, dimension (N-2)
*          On entry, D must contain the second super-diagonal elements
*          of U as returned from DLAGTF.
*
*  IN      (input) INTEGER array, dimension (N)
*          On entry, IN must contain details of the matrix P as returned
*          from DLAGTF.
*
*  Y       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the right hand side vector y.
*          On exit, Y is overwritten by the solution vector x.
*
*  TOL     (input/output) DOUBLE PRECISION
*          On entry, with  JOB .lt. 0, TOL should be the minimum
*          perturbation to be made to very small diagonal elements of U.
*          TOL should normally be chosen as about eps*norm(U), where eps
*          is the relative machine precision, but if TOL is supplied as
*          non-positive, then it is reset to eps*max( abs( u(i,j) ) ).
*          If  JOB .gt. 0  then TOL is not referenced.
*
*          On exit, TOL is changed as described above, only if TOL is
*          non-positive on entry. Otherwise TOL is unchanged.
*
*  INFO    (output) INTEGER
*          = 0   : successful exit
*          .lt. 0: if INFO = -i, the i-th argument had an illegal value
*          .gt. 0: overflow would occur when computing the INFO(th)
*                  element of the solution vector x. This can only occur
*                  when JOB is supplied as positive and either means
*                  that a diagonal element of U is very small, or that
*                  the elements of the right-hand side vector y are very
*                  large.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            K
      DOUBLE PRECISION   ABSAK, AK, BIGNUM, EPS, PERT, SFMIN, TEMP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      IF( ( ABS( JOB ).GT.2 ) .OR. ( JOB.EQ.0 ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAGTS', -INFO )
         RETURN
      END IF
*
      IF( N.EQ.0 )
     $   RETURN
*
      EPS = DLAMCH( 'Epsilon' )
      SFMIN = DLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SFMIN
*
      IF( JOB.LT.0 ) THEN
         IF( TOL.LE.ZERO ) THEN
            TOL = ABS( A( 1 ) )
            IF( N.GT.1 )
     $         TOL = MAX( TOL, ABS( A( 2 ) ), ABS( B( 1 ) ) )
            DO 10 K = 3, N
               TOL = MAX( TOL, ABS( A( K ) ), ABS( B( K-1 ) ),
     $               ABS( D( K-2 ) ) )
   10       CONTINUE
            TOL = TOL*EPS
            IF( TOL.EQ.ZERO )
     $         TOL = EPS
         END IF
      END IF
*
      IF( ABS( JOB ).EQ.1 ) THEN
         DO 20 K = 2, N
            IF( IN( K-1 ).EQ.0 ) THEN
               Y( K ) = Y( K ) - C( K-1 )*Y( K-1 )
            ELSE
               TEMP = Y( K-1 )
               Y( K-1 ) = Y( K )
               Y( K ) = TEMP - C( K-1 )*Y( K )
            END IF
   20    CONTINUE
         IF( JOB.EQ.1 ) THEN
            DO 30 K = N, 1, -1
               IF( K.LE.N-2 ) THEN
                  TEMP = Y( K ) - B( K )*Y( K+1 ) - D( K )*Y( K+2 )
               ELSE IF( K.EQ.N-1 ) THEN
                  TEMP = Y( K ) - B( K )*Y( K+1 )
               ELSE
                  TEMP = Y( K )
               END IF
               AK = A( K )
               ABSAK = ABS( AK )
               IF( ABSAK.LT.ONE ) THEN
                  IF( ABSAK.LT.SFMIN ) THEN
                     IF( ABSAK.EQ.ZERO .OR. ABS( TEMP )*SFMIN.GT.ABSAK )
     $                    THEN
                        INFO = K
                        RETURN
                     ELSE
                        TEMP = TEMP*BIGNUM
                        AK = AK*BIGNUM
                     END IF
                  ELSE IF( ABS( TEMP ).GT.ABSAK*BIGNUM ) THEN
                     INFO = K
                     RETURN
                  END IF
               END IF
               Y( K ) = TEMP / AK
   30       CONTINUE
         ELSE
            DO 50 K = N, 1, -1
               IF( K.LE.N-2 ) THEN
                  TEMP = Y( K ) - B( K )*Y( K+1 ) - D( K )*Y( K+2 )
               ELSE IF( K.EQ.N-1 ) THEN
                  TEMP = Y( K ) - B( K )*Y( K+1 )
               ELSE
                  TEMP = Y( K )
               END IF
               AK = A( K )
               PERT = SIGN( TOL, AK )
   40          CONTINUE
               ABSAK = ABS( AK )
               IF( ABSAK.LT.ONE ) THEN
                  IF( ABSAK.LT.SFMIN ) THEN
                     IF( ABSAK.EQ.ZERO .OR. ABS( TEMP )*SFMIN.GT.ABSAK )
     $                    THEN
                        AK = AK + PERT
                        PERT = 2*PERT
                        GO TO 40
                     ELSE
                        TEMP = TEMP*BIGNUM
                        AK = AK*BIGNUM
                     END IF
                  ELSE IF( ABS( TEMP ).GT.ABSAK*BIGNUM ) THEN
                     AK = AK + PERT
                     PERT = 2*PERT
                     GO TO 40
                  END IF
               END IF
               Y( K ) = TEMP / AK
   50       CONTINUE
         END IF
      ELSE
*
*        Come to here if  JOB = 2 or -2
*
         IF( JOB.EQ.2 ) THEN
            DO 60 K = 1, N
               IF( K.GE.3 ) THEN
                  TEMP = Y( K ) - B( K-1 )*Y( K-1 ) - D( K-2 )*Y( K-2 )
               ELSE IF( K.EQ.2 ) THEN
                  TEMP = Y( K ) - B( K-1 )*Y( K-1 )
               ELSE
                  TEMP = Y( K )
               END IF
               AK = A( K )
               ABSAK = ABS( AK )
               IF( ABSAK.LT.ONE ) THEN
                  IF( ABSAK.LT.SFMIN ) THEN
                     IF( ABSAK.EQ.ZERO .OR. ABS( TEMP )*SFMIN.GT.ABSAK )
     $                    THEN
                        INFO = K
                        RETURN
                     ELSE
                        TEMP = TEMP*BIGNUM
                        AK = AK*BIGNUM
                     END IF
                  ELSE IF( ABS( TEMP ).GT.ABSAK*BIGNUM ) THEN
                     INFO = K
                     RETURN
                  END IF
               END IF
               Y( K ) = TEMP / AK
   60       CONTINUE
         ELSE
            DO 80 K = 1, N
               IF( K.GE.3 ) THEN
                  TEMP = Y( K ) - B( K-1 )*Y( K-1 ) - D( K-2 )*Y( K-2 )
               ELSE IF( K.EQ.2 ) THEN
                  TEMP = Y( K ) - B( K-1 )*Y( K-1 )
               ELSE
                  TEMP = Y( K )
               END IF
               AK = A( K )
               PERT = SIGN( TOL, AK )
   70          CONTINUE
               ABSAK = ABS( AK )
               IF( ABSAK.LT.ONE ) THEN
                  IF( ABSAK.LT.SFMIN ) THEN
                     IF( ABSAK.EQ.ZERO .OR. ABS( TEMP )*SFMIN.GT.ABSAK )
     $                    THEN
                        AK = AK + PERT
                        PERT = 2*PERT
                        GO TO 70
                     ELSE
                        TEMP = TEMP*BIGNUM
                        AK = AK*BIGNUM
                     END IF
                  ELSE IF( ABS( TEMP ).GT.ABSAK*BIGNUM ) THEN
                     AK = AK + PERT
                     PERT = 2*PERT
                     GO TO 70
                  END IF
               END IF
               Y( K ) = TEMP / AK
   80       CONTINUE
         END IF
*
         DO 90 K = N, 2, -1
            IF( IN( K-1 ).EQ.0 ) THEN
               Y( K-1 ) = Y( K-1 ) - C( K-1 )*Y( K )
            ELSE
               TEMP = Y( K-1 )
               Y( K-1 ) = Y( K )
               Y( K ) = TEMP - C( K-1 )*Y( K )
            END IF
   90    CONTINUE
      END IF
*
*     End of DLAGTS
*
      END
      SUBROUTINE DLAGV2( A, LDA, B, LDB, ALPHAR, ALPHAI, BETA, CSL, SNL,
     $                   CSR, SNR )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDB
      DOUBLE PRECISION   CSL, CSR, SNL, SNR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( 2 ), ALPHAR( 2 ),
     $                   B( LDB, * ), BETA( 2 )
*     ..
*
*  Purpose
*  =======
*
*  DLAGV2 computes the Generalized Schur factorization of a real 2-by-2
*  matrix pencil (A,B) where B is upper triangular. This routine
*  computes orthogonal (rotation) matrices given by CSL, SNL and CSR,
*  SNR such that
*
*  1) if the pencil (A,B) has two real eigenvalues (include 0/0 or 1/0
*     types), then
*
*     [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
*     [  0  a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
*
*     [ b11 b12 ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
*     [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ],
*
*  2) if the pencil (A,B) has a pair of complex conjugate eigenvalues,
*     then
*
*     [ a11 a12 ] := [  CSL  SNL ] [ a11 a12 ] [  CSR -SNR ]
*     [ a21 a22 ]    [ -SNL  CSL ] [ a21 a22 ] [  SNR  CSR ]
*
*     [ b11  0  ] := [  CSL  SNL ] [ b11 b12 ] [  CSR -SNR ]
*     [  0  b22 ]    [ -SNL  CSL ] [  0  b22 ] [  SNR  CSR ]
*
*     where b11 >= b22 > 0.
*
*
*  Arguments
*  =========
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, 2)
*          On entry, the 2 x 2 matrix A.
*          On exit, A is overwritten by the ``A-part'' of the
*          generalized Schur form.
*
*  LDA     (input) INTEGER
*          THe leading dimension of the array A.  LDA >= 2.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, 2)
*          On entry, the upper triangular 2 x 2 matrix B.
*          On exit, B is overwritten by the ``B-part'' of the
*          generalized Schur form.
*
*  LDB     (input) INTEGER
*          THe leading dimension of the array B.  LDB >= 2.
*
*  ALPHAR  (output) DOUBLE PRECISION array, dimension (2)
*  ALPHAI  (output) DOUBLE PRECISION array, dimension (2)
*  BETA    (output) DOUBLE PRECISION array, dimension (2)
*          (ALPHAR(k)+i*ALPHAI(k))/BETA(k) are the eigenvalues of the
*          pencil (A,B), k=1,2, i = sqrt(-1).  Note that BETA(k) may
*          be zero.
*
*  CSL     (output) DOUBLE PRECISION
*          The cosine of the left rotation matrix.
*
*  SNL     (output) DOUBLE PRECISION
*          The sine of the left rotation matrix.
*
*  CSR     (output) DOUBLE PRECISION
*          The cosine of the right rotation matrix.
*
*  SNR     (output) DOUBLE PRECISION
*          The sine of the right rotation matrix.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   ANORM, ASCALE, BNORM, BSCALE, H1, H2, H3, QQ,
     $                   R, RR, SAFMIN, SCALE1, SCALE2, T, ULP, WI, WR1,
     $                   WR2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAG2, DLARTG, DLASV2, DROT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
      SAFMIN = DLAMCH( 'S' )
      ULP = DLAMCH( 'P' )
*
*     Scale A
*
      ANORM = MAX( ABS( A( 1, 1 ) )+ABS( A( 2, 1 ) ),
     $        ABS( A( 1, 2 ) )+ABS( A( 2, 2 ) ), SAFMIN )
      ASCALE = ONE / ANORM
      A( 1, 1 ) = ASCALE*A( 1, 1 )
      A( 1, 2 ) = ASCALE*A( 1, 2 )
      A( 2, 1 ) = ASCALE*A( 2, 1 )
      A( 2, 2 ) = ASCALE*A( 2, 2 )
*
*     Scale B
*
      BNORM = MAX( ABS( B( 1, 1 ) ), ABS( B( 1, 2 ) )+ABS( B( 2, 2 ) ),
     $        SAFMIN )
      BSCALE = ONE / BNORM
      B( 1, 1 ) = BSCALE*B( 1, 1 )
      B( 1, 2 ) = BSCALE*B( 1, 2 )
      B( 2, 2 ) = BSCALE*B( 2, 2 )
*
*     Check if A can be deflated
*
      IF( ABS( A( 2, 1 ) ).LE.ULP ) THEN
         CSL = ONE
         SNL = ZERO
         CSR = ONE
         SNR = ZERO
         A( 2, 1 ) = ZERO
         B( 2, 1 ) = ZERO
*
*     Check if B is singular
*
      ELSE IF( ABS( B( 1, 1 ) ).LE.ULP ) THEN
         CALL DLARTG( A( 1, 1 ), A( 2, 1 ), CSL, SNL, R )
         CSR = ONE
         SNR = ZERO
         CALL DROT( 2, A( 1, 1 ), LDA, A( 2, 1 ), LDA, CSL, SNL )
         CALL DROT( 2, B( 1, 1 ), LDB, B( 2, 1 ), LDB, CSL, SNL )
         A( 2, 1 ) = ZERO
         B( 1, 1 ) = ZERO
         B( 2, 1 ) = ZERO
*
      ELSE IF( ABS( B( 2, 2 ) ).LE.ULP ) THEN
         CALL DLARTG( A( 2, 2 ), A( 2, 1 ), CSR, SNR, T )
         SNR = -SNR
         CALL DROT( 2, A( 1, 1 ), 1, A( 1, 2 ), 1, CSR, SNR )
         CALL DROT( 2, B( 1, 1 ), 1, B( 1, 2 ), 1, CSR, SNR )
         CSL = ONE
         SNL = ZERO
         A( 2, 1 ) = ZERO
         B( 2, 1 ) = ZERO
         B( 2, 2 ) = ZERO
*
      ELSE
*
*        B is nonsingular, first compute the eigenvalues of (A,B)
*
         CALL DLAG2( A, LDA, B, LDB, SAFMIN, SCALE1, SCALE2, WR1, WR2,
     $               WI )
*
         IF( WI.EQ.ZERO ) THEN
*
*           two real eigenvalues, compute s*A-w*B
*
            H1 = SCALE1*A( 1, 1 ) - WR1*B( 1, 1 )
            H2 = SCALE1*A( 1, 2 ) - WR1*B( 1, 2 )
            H3 = SCALE1*A( 2, 2 ) - WR1*B( 2, 2 )
*
            RR = DLAPY2( H1, H2 )
            QQ = DLAPY2( SCALE1*A( 2, 1 ), H3 )
*
            IF( RR.GT.QQ ) THEN
*
*              find right rotation matrix to zero 1,1 element of
*              (sA - wB)
*
               CALL DLARTG( H2, H1, CSR, SNR, T )
*
            ELSE
*
*              find right rotation matrix to zero 2,1 element of
*              (sA - wB)
*
               CALL DLARTG( H3, SCALE1*A( 2, 1 ), CSR, SNR, T )
*
            END IF
*
            SNR = -SNR
            CALL DROT( 2, A( 1, 1 ), 1, A( 1, 2 ), 1, CSR, SNR )
            CALL DROT( 2, B( 1, 1 ), 1, B( 1, 2 ), 1, CSR, SNR )
*
*           compute inf norms of A and B
*
            H1 = MAX( ABS( A( 1, 1 ) )+ABS( A( 1, 2 ) ),
     $           ABS( A( 2, 1 ) )+ABS( A( 2, 2 ) ) )
            H2 = MAX( ABS( B( 1, 1 ) )+ABS( B( 1, 2 ) ),
     $           ABS( B( 2, 1 ) )+ABS( B( 2, 2 ) ) )
*
            IF( ( SCALE1*H1 ).GE.ABS( WR1 )*H2 ) THEN
*
*              find left rotation matrix Q to zero out B(2,1)
*
               CALL DLARTG( B( 1, 1 ), B( 2, 1 ), CSL, SNL, R )
*
            ELSE
*
*              find left rotation matrix Q to zero out A(2,1)
*
               CALL DLARTG( A( 1, 1 ), A( 2, 1 ), CSL, SNL, R )
*
            END IF
*
            CALL DROT( 2, A( 1, 1 ), LDA, A( 2, 1 ), LDA, CSL, SNL )
            CALL DROT( 2, B( 1, 1 ), LDB, B( 2, 1 ), LDB, CSL, SNL )
*
            A( 2, 1 ) = ZERO
            B( 2, 1 ) = ZERO
*
         ELSE
*
*           a pair of complex conjugate eigenvalues
*           first compute the SVD of the matrix B
*
            CALL DLASV2( B( 1, 1 ), B( 1, 2 ), B( 2, 2 ), R, T, SNR,
     $                   CSR, SNL, CSL )
*
*           Form (A,B) := Q(A,B)Z' where Q is left rotation matrix and
*           Z is right rotation matrix computed from DLASV2
*
            CALL DROT( 2, A( 1, 1 ), LDA, A( 2, 1 ), LDA, CSL, SNL )
            CALL DROT( 2, B( 1, 1 ), LDB, B( 2, 1 ), LDB, CSL, SNL )
            CALL DROT( 2, A( 1, 1 ), 1, A( 1, 2 ), 1, CSR, SNR )
            CALL DROT( 2, B( 1, 1 ), 1, B( 1, 2 ), 1, CSR, SNR )
*
            B( 2, 1 ) = ZERO
            B( 1, 2 ) = ZERO
*
         END IF
*
      END IF
*
*     Unscaling
*
      A( 1, 1 ) = ANORM*A( 1, 1 )
      A( 2, 1 ) = ANORM*A( 2, 1 )
      A( 1, 2 ) = ANORM*A( 1, 2 )
      A( 2, 2 ) = ANORM*A( 2, 2 )
      B( 1, 1 ) = BNORM*B( 1, 1 )
      B( 2, 1 ) = BNORM*B( 2, 1 )
      B( 1, 2 ) = BNORM*B( 1, 2 )
      B( 2, 2 ) = BNORM*B( 2, 2 )
*
      IF( WI.EQ.ZERO ) THEN
         ALPHAR( 1 ) = A( 1, 1 )
         ALPHAR( 2 ) = A( 2, 2 )
         ALPHAI( 1 ) = ZERO
         ALPHAI( 2 ) = ZERO
         BETA( 1 ) = B( 1, 1 )
         BETA( 2 ) = B( 2, 2 )
      ELSE
         ALPHAR( 1 ) = ANORM*WR1 / SCALE1 / BNORM
         ALPHAI( 1 ) = ANORM*WI / SCALE1 / BNORM
         ALPHAR( 2 ) = ALPHAR( 1 )
         ALPHAI( 2 ) = -ALPHAI( 1 )
         BETA( 1 ) = ONE
         BETA( 2 ) = ONE
      END IF
*
      RETURN
*
*     End of DLAGV2
*
      END
      SUBROUTINE DLAHQR( WANTT, WANTZ, N, ILO, IHI, H, LDH, WR, WI,
     $                   ILOZ, IHIZ, Z, LDZ, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IHI, IHIZ, ILO, ILOZ, INFO, LDH, LDZ, N
      LOGICAL            WANTT, WANTZ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   H( LDH, * ), WI( * ), WR( * ), Z( LDZ, * )
*     ..
*
*     Purpose
*     =======
*
*     DLAHQR is an auxiliary routine called by DHSEQR to update the
*     eigenvalues and Schur decomposition already computed by DHSEQR, by
*     dealing with the Hessenberg submatrix in rows and columns ILO to
*     IHI.
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
*     N       (input) INTEGER
*          The order of the matrix H.  N >= 0.
*
*     ILO     (input) INTEGER
*     IHI     (input) INTEGER
*          It is assumed that H is already upper quasi-triangular in
*          rows and columns IHI+1:N, and that H(ILO,ILO-1) = 0 (unless
*          ILO = 1). DLAHQR works primarily with the Hessenberg
*          submatrix in rows and columns ILO to IHI, but applies
*          transformations to all of H if WANTT is .TRUE..
*          1 <= ILO <= max(1,IHI); IHI <= N.
*
*     H       (input/output) DOUBLE PRECISION array, dimension (LDH,N)
*          On entry, the upper Hessenberg matrix H.
*          On exit, if INFO is zero and if WANTT is .TRUE., H is upper
*          quasi-triangular in rows and columns ILO:IHI, with any
*          2-by-2 diagonal blocks in standard form. If INFO is zero
*          and WANTT is .FALSE., the contents of H are unspecified on
*          exit.  The output state of H if INFO is nonzero is given
*          below under the description of INFO.
*
*     LDH     (input) INTEGER
*          The leading dimension of the array H. LDH >= max(1,N).
*
*     WR      (output) DOUBLE PRECISION array, dimension (N)
*     WI      (output) DOUBLE PRECISION array, dimension (N)
*          The real and imaginary parts, respectively, of the computed
*          eigenvalues ILO to IHI are stored in the corresponding
*          elements of WR and WI. If two eigenvalues are computed as a
*          complex conjugate pair, they are stored in consecutive
*          elements of WR and WI, say the i-th and (i+1)th, with
*          WI(i) > 0 and WI(i+1) < 0. If WANTT is .TRUE., the
*          eigenvalues are stored in the same order as on the diagonal
*          of the Schur form returned in H, with WR(i) = H(i,i), and, if
*          H(i:i+1,i:i+1) is a 2-by-2 diagonal block,
*          WI(i) = sqrt(H(i+1,i)*H(i,i+1)) and WI(i+1) = -WI(i).
*
*     ILOZ    (input) INTEGER
*     IHIZ    (input) INTEGER
*          Specify the rows of Z to which transformations must be
*          applied if WANTZ is .TRUE..
*          1 <= ILOZ <= ILO; IHI <= IHIZ <= N.
*
*     Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
*          If WANTZ is .TRUE., on entry Z must contain the current
*          matrix Z of transformations accumulated by DHSEQR, and on
*          exit Z has been updated; transformations are applied only to
*          the submatrix Z(ILOZ:IHIZ,ILO:IHI).
*          If WANTZ is .FALSE., Z is not referenced.
*
*     LDZ     (input) INTEGER
*          The leading dimension of the array Z. LDZ >= max(1,N).
*
*     INFO    (output) INTEGER
*           =   0: successful exit
*          .GT. 0: If INFO = i, DLAHQR failed to compute all the
*                  eigenvalues ILO to IHI in a total of 30 iterations
*                  per eigenvalue; elements i+1:ihi of WR and WI
*                  contain those eigenvalues which have been
*                  successfully computed.
*
*                  If INFO .GT. 0 and WANTT is .FALSE., then on exit,
*                  the remaining unconverged eigenvalues are the
*                  eigenvalues of the upper Hessenberg matrix rows
*                  and columns ILO thorugh INFO of the final, output
*                  value of H.
*
*                  If INFO .GT. 0 and WANTT is .TRUE., then on exit
*          (*)       (initial value of H)*U  = U*(final value of H)
*                  where U is an orthognal matrix.    The final
*                  value of H is upper Hessenberg and triangular in
*                  rows and columns INFO+1 through IHI.
*
*                  If INFO .GT. 0 and WANTZ is .TRUE., then on exit
*                      (final value of Z)  = (initial value of Z)*U
*                  where U is the orthogonal matrix in (*)
*                  (regardless of the value of WANTT.)
*
*     Further Details
*     ===============
*
*     02-96 Based on modifications by
*     David Day, Sandia National Laboratory, USA
*
*     12-04 Further modifications by
*     Ralph Byers, University of Kansas, USA
*
*       This is a modified version of DLAHQR from LAPACK version 3.0.
*       It is (1) more robust against overflow and underflow and
*       (2) adopts the more conservative Ahues & Tisseur stopping
*       criterion (LAWN 122, 1997).
*
*     =========================================================
*
*     .. Parameters ..
      INTEGER            ITMAX
      PARAMETER          ( ITMAX = 30 )
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0d0, ONE = 1.0d0, TWO = 2.0d0 )
      DOUBLE PRECISION   DAT1, DAT2
      PARAMETER          ( DAT1 = 3.0d0 / 4.0d0, DAT2 = -0.4375d0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AA, AB, BA, BB, CS, DET, H11, H12, H21, H21S,
     $                   H22, RT1I, RT1R, RT2I, RT2R, RTDISC, S, SAFMAX,
     $                   SAFMIN, SMLNUM, SN, SUM, T1, T2, T3, TR, TST,
     $                   ULP, V2, V3
      INTEGER            I, I1, I2, ITS, J, K, L, M, NH, NR, NZ
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   V( 3 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLABAD, DLANV2, DLARFG, DROT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT
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
         WR( ILO ) = H( ILO, ILO )
         WI( ILO ) = ZERO
         RETURN
      END IF
*
*     ==== clear out the trash ====
      DO 10 J = ILO, IHI - 3
         H( J+2, J ) = ZERO
         H( J+3, J ) = ZERO
   10 CONTINUE
      IF( ILO.LE.IHI-2 )
     $   H( IHI, IHI-2 ) = ZERO
*
      NH = IHI - ILO + 1
      NZ = IHIZ - ILOZ + 1
*
*     Set machine-dependent constants for the stopping criterion.
*
      SAFMIN = DLAMCH( 'SAFE MINIMUM' )
      SAFMAX = ONE / SAFMIN
      CALL DLABAD( SAFMIN, SAFMAX )
      ULP = DLAMCH( 'PRECISION' )
      SMLNUM = SAFMIN*( DBLE( NH ) / ULP )
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
*     The main loop begins here. I is the loop index and decreases from
*     IHI to ILO in steps of 1 or 2. Each iteration of the loop works
*     with the active submatrix in rows and columns L to I.
*     Eigenvalues I+1 to IHI have already converged. Either L = ILO or
*     H(L,L-1) is negligible so that the matrix splits.
*
      I = IHI
   20 CONTINUE
      L = ILO
      IF( I.LT.ILO )
     $   GO TO 160
*
*     Perform QR iterations on rows and columns ILO to I until a
*     submatrix of order 1 or 2 splits off at the bottom because a
*     subdiagonal element has become negligible.
*
      DO 140 ITS = 0, ITMAX
*
*        Look for a single small subdiagonal element.
*
         DO 30 K = I, L + 1, -1
            IF( ABS( H( K, K-1 ) ).LE.SMLNUM )
     $         GO TO 40
            TST = ABS( H( K-1, K-1 ) ) + ABS( H( K, K ) )
            IF( TST.EQ.ZERO ) THEN
               IF( K-2.GE.ILO )
     $            TST = TST + ABS( H( K-1, K-2 ) )
               IF( K+1.LE.IHI )
     $            TST = TST + ABS( H( K+1, K ) )
            END IF
*           ==== The following is a conservative small subdiagonal
*           .    deflation  criterion due to Ahues & Tisseur (LAWN 122,
*           .    1997). It has better mathematical foundation and
*           .    improves accuracy in some cases.  ====
            IF( ABS( H( K, K-1 ) ).LE.ULP*TST ) THEN
               AB = MAX( ABS( H( K, K-1 ) ), ABS( H( K-1, K ) ) )
               BA = MIN( ABS( H( K, K-1 ) ), ABS( H( K-1, K ) ) )
               AA = MAX( ABS( H( K, K ) ),
     $              ABS( H( K-1, K-1 )-H( K, K ) ) )
               BB = MIN( ABS( H( K, K ) ),
     $              ABS( H( K-1, K-1 )-H( K, K ) ) )
               S = AA + AB
               IF( BA*( AB / S ).LE.MAX( SMLNUM,
     $             ULP*( BB*( AA / S ) ) ) )GO TO 40
            END IF
   30    CONTINUE
   40    CONTINUE
         L = K
         IF( L.GT.ILO ) THEN
*
*           H(L,L-1) is negligible
*
            H( L, L-1 ) = ZERO
         END IF
*
*        Exit from loop if a submatrix of order 1 or 2 has split off.
*
         IF( L.GE.I-1 )
     $      GO TO 150
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
            H11 = DAT1*S + H( I, I )
            H12 = DAT2*S
            H21 = S
            H22 = H11
         ELSE
*
*           Prepare to use Francis' double shift
*           (i.e. 2nd degree generalized Rayleigh quotient)
*
            H11 = H( I-1, I-1 )
            H21 = H( I, I-1 )
            H12 = H( I-1, I )
            H22 = H( I, I )
         END IF
         S = ABS( H11 ) + ABS( H12 ) + ABS( H21 ) + ABS( H22 )
         IF( S.EQ.ZERO ) THEN
            RT1R = ZERO
            RT1I = ZERO
            RT2R = ZERO
            RT2I = ZERO
         ELSE
            H11 = H11 / S
            H21 = H21 / S
            H12 = H12 / S
            H22 = H22 / S
            TR = ( H11+H22 ) / TWO
            DET = ( H11-TR )*( H22-TR ) - H12*H21
            RTDISC = SQRT( ABS( DET ) )
            IF( DET.GE.ZERO ) THEN
*
*              ==== complex conjugate shifts ====
*
               RT1R = TR*S
               RT2R = RT1R
               RT1I = RTDISC*S
               RT2I = -RT1I
            ELSE
*
*              ==== real shifts (use only one of them)  ====
*
               RT1R = TR + RTDISC
               RT2R = TR - RTDISC
               IF( ABS( RT1R-H22 ).LE.ABS( RT2R-H22 ) ) THEN
                  RT1R = RT1R*S
                  RT2R = RT1R
               ELSE
                  RT2R = RT2R*S
                  RT1R = RT2R
               END IF
               RT1I = ZERO
               RT2I = ZERO
            END IF
         END IF
*
*        Look for two consecutive small subdiagonal elements.
*
         DO 50 M = I - 2, L, -1
*           Determine the effect of starting the double-shift QR
*           iteration at row M, and see if this would make H(M,M-1)
*           negligible.  (The following uses scaling to avoid
*           overflows and most underflows.)
*
            H21S = H( M+1, M )
            S = ABS( H( M, M )-RT2R ) + ABS( RT2I ) + ABS( H21S )
            H21S = H( M+1, M ) / S
            V( 1 ) = H21S*H( M, M+1 ) + ( H( M, M )-RT1R )*
     $               ( ( H( M, M )-RT2R ) / S ) - RT1I*( RT2I / S )
            V( 2 ) = H21S*( H( M, M )+H( M+1, M+1 )-RT1R-RT2R )
            V( 3 ) = H21S*H( M+2, M+1 )
            S = ABS( V( 1 ) ) + ABS( V( 2 ) ) + ABS( V( 3 ) )
            V( 1 ) = V( 1 ) / S
            V( 2 ) = V( 2 ) / S
            V( 3 ) = V( 3 ) / S
            IF( M.EQ.L )
     $         GO TO 60
            IF( ABS( H( M, M-1 ) )*( ABS( V( 2 ) )+ABS( V( 3 ) ) ).LE.
     $          ULP*ABS( V( 1 ) )*( ABS( H( M-1, M-1 ) )+ABS( H( M,
     $          M ) )+ABS( H( M+1, M+1 ) ) ) )GO TO 60
   50    CONTINUE
   60    CONTINUE
*
*        Double-shift QR step
*
         DO 130 K = M, I - 1
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
            NR = MIN( 3, I-K+1 )
            IF( K.GT.M )
     $         CALL DCOPY( NR, H( K, K-1 ), 1, V, 1 )
            CALL DLARFG( NR, V( 1 ), V( 2 ), 1, T1 )
            IF( K.GT.M ) THEN
               H( K, K-1 ) = V( 1 )
               H( K+1, K-1 ) = ZERO
               IF( K.LT.I-1 )
     $            H( K+2, K-1 ) = ZERO
            ELSE IF( M.GT.L ) THEN
               H( K, K-1 ) = -H( K, K-1 )
            END IF
            V2 = V( 2 )
            T2 = T1*V2
            IF( NR.EQ.3 ) THEN
               V3 = V( 3 )
               T3 = T1*V3
*
*              Apply G from the left to transform the rows of the matrix
*              in columns K to I2.
*
               DO 70 J = K, I2
                  SUM = H( K, J ) + V2*H( K+1, J ) + V3*H( K+2, J )
                  H( K, J ) = H( K, J ) - SUM*T1
                  H( K+1, J ) = H( K+1, J ) - SUM*T2
                  H( K+2, J ) = H( K+2, J ) - SUM*T3
   70          CONTINUE
*
*              Apply G from the right to transform the columns of the
*              matrix in rows I1 to min(K+3,I).
*
               DO 80 J = I1, MIN( K+3, I )
                  SUM = H( J, K ) + V2*H( J, K+1 ) + V3*H( J, K+2 )
                  H( J, K ) = H( J, K ) - SUM*T1
                  H( J, K+1 ) = H( J, K+1 ) - SUM*T2
                  H( J, K+2 ) = H( J, K+2 ) - SUM*T3
   80          CONTINUE
*
               IF( WANTZ ) THEN
*
*                 Accumulate transformations in the matrix Z
*
                  DO 90 J = ILOZ, IHIZ
                     SUM = Z( J, K ) + V2*Z( J, K+1 ) + V3*Z( J, K+2 )
                     Z( J, K ) = Z( J, K ) - SUM*T1
                     Z( J, K+1 ) = Z( J, K+1 ) - SUM*T2
                     Z( J, K+2 ) = Z( J, K+2 ) - SUM*T3
   90             CONTINUE
               END IF
            ELSE IF( NR.EQ.2 ) THEN
*
*              Apply G from the left to transform the rows of the matrix
*              in columns K to I2.
*
               DO 100 J = K, I2
                  SUM = H( K, J ) + V2*H( K+1, J )
                  H( K, J ) = H( K, J ) - SUM*T1
                  H( K+1, J ) = H( K+1, J ) - SUM*T2
  100          CONTINUE
*
*              Apply G from the right to transform the columns of the
*              matrix in rows I1 to min(K+3,I).
*
               DO 110 J = I1, I
                  SUM = H( J, K ) + V2*H( J, K+1 )
                  H( J, K ) = H( J, K ) - SUM*T1
                  H( J, K+1 ) = H( J, K+1 ) - SUM*T2
  110          CONTINUE
*
               IF( WANTZ ) THEN
*
*                 Accumulate transformations in the matrix Z
*
                  DO 120 J = ILOZ, IHIZ
                     SUM = Z( J, K ) + V2*Z( J, K+1 )
                     Z( J, K ) = Z( J, K ) - SUM*T1
                     Z( J, K+1 ) = Z( J, K+1 ) - SUM*T2
  120             CONTINUE
               END IF
            END IF
  130    CONTINUE
*
  140 CONTINUE
*
*     Failure to converge in remaining number of iterations
*
      INFO = I
      RETURN
*
  150 CONTINUE
*
      IF( L.EQ.I ) THEN
*
*        H(I,I-1) is negligible: one eigenvalue has converged.
*
         WR( I ) = H( I, I )
         WI( I ) = ZERO
      ELSE IF( L.EQ.I-1 ) THEN
*
*        H(I-1,I-2) is negligible: a pair of eigenvalues have converged.
*
*        Transform the 2-by-2 submatrix to standard Schur form,
*        and compute and store the eigenvalues.
*
         CALL DLANV2( H( I-1, I-1 ), H( I-1, I ), H( I, I-1 ),
     $                H( I, I ), WR( I-1 ), WI( I-1 ), WR( I ), WI( I ),
     $                CS, SN )
*
         IF( WANTT ) THEN
*
*           Apply the transformation to the rest of H.
*
            IF( I2.GT.I )
     $         CALL DROT( I2-I, H( I-1, I+1 ), LDH, H( I, I+1 ), LDH,
     $                    CS, SN )
            CALL DROT( I-I1-1, H( I1, I-1 ), 1, H( I1, I ), 1, CS, SN )
         END IF
         IF( WANTZ ) THEN
*
*           Apply the transformation to Z.
*
            CALL DROT( NZ, Z( ILOZ, I-1 ), 1, Z( ILOZ, I ), 1, CS, SN )
         END IF
      END IF
*
*     return to start of the main loop with new value of I.
*
      I = L - 1
      GO TO 20
*
  160 CONTINUE
      RETURN
*
*     End of DLAHQR
*
      END
      SUBROUTINE DLAHRD( N, K, NB, A, LDA, TAU, T, LDT, Y, LDY )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            K, LDA, LDT, LDY, N, NB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), T( LDT, NB ), TAU( NB ),
     $                   Y( LDY, NB )
*     ..
*
*  Purpose
*  =======
*
*  DLAHRD reduces the first NB columns of a real general n-by-(n-k+1)
*  matrix A so that elements below the k-th subdiagonal are zero. The
*  reduction is performed by an orthogonal similarity transformation
*  Q' * A * Q. The routine returns the matrices V and T which determine
*  Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T.
*
*  This is an OBSOLETE auxiliary routine. 
*  This routine will be 'deprecated' in a  future release.
*  Please use the new routine DLAHR2 instead.
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
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   EI
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMV, DLARFG, DSCAL, DTRMV
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
            CALL DGEMV( 'No transpose', N, I-1, -ONE, Y, LDY,
     $                  A( K+I-1, 1 ), LDA, ONE, A( 1, I ), 1 )
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
            CALL DTRMV( 'Lower', 'Transpose', 'Unit', I-1, A( K+1, 1 ),
     $                  LDA, T( 1, NB ), 1 )
*
*           w := w + V2'*b2
*
            CALL DGEMV( 'Transpose', N-K-I+1, I-1, ONE, A( K+I, 1 ),
     $                  LDA, A( K+I, I ), 1, ONE, T( 1, NB ), 1 )
*
*           w := T'*w
*
            CALL DTRMV( 'Upper', 'Transpose', 'Non-unit', I-1, T, LDT,
     $                  T( 1, NB ), 1 )
*
*           b2 := b2 - V2*w
*
            CALL DGEMV( 'No transpose', N-K-I+1, I-1, -ONE, A( K+I, 1 ),
     $                  LDA, T( 1, NB ), 1, ONE, A( K+I, I ), 1 )
*
*           b1 := b1 - V1*w
*
            CALL DTRMV( 'Lower', 'No transpose', 'Unit', I-1,
     $                  A( K+1, 1 ), LDA, T( 1, NB ), 1 )
            CALL DAXPY( I-1, -ONE, T( 1, NB ), 1, A( K+1, I ), 1 )
*
            A( K+I-1, I-1 ) = EI
         END IF
*
*        Generate the elementary reflector H(i) to annihilate
*        A(k+i+1:n,i)
*
         CALL DLARFG( N-K-I+1, A( K+I, I ), A( MIN( K+I+1, N ), I ), 1,
     $                TAU( I ) )
         EI = A( K+I, I )
         A( K+I, I ) = ONE
*
*        Compute  Y(1:n,i)
*
         CALL DGEMV( 'No transpose', N, N-K-I+1, ONE, A( 1, I+1 ), LDA,
     $               A( K+I, I ), 1, ZERO, Y( 1, I ), 1 )
         CALL DGEMV( 'Transpose', N-K-I+1, I-1, ONE, A( K+I, 1 ), LDA,
     $               A( K+I, I ), 1, ZERO, T( 1, I ), 1 )
         CALL DGEMV( 'No transpose', N, I-1, -ONE, Y, LDY, T( 1, I ), 1,
     $               ONE, Y( 1, I ), 1 )
         CALL DSCAL( N, TAU( I ), Y( 1, I ), 1 )
*
*        Compute T(1:i,i)
*
         CALL DSCAL( I-1, -TAU( I ), T( 1, I ), 1 )
         CALL DTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T, LDT,
     $               T( 1, I ), 1 )
         T( I, I ) = TAU( I )
*
   10 CONTINUE
      A( K+NB, NB ) = EI
*
      RETURN
*
*     End of DLAHRD
*
      END
      SUBROUTINE DLAIC1( JOB, J, X, SEST, W, GAMMA, SESTPR, S, C )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            J, JOB
      DOUBLE PRECISION   C, GAMMA, S, SEST, SESTPR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   W( J ), X( J )
*     ..
*
*  Purpose
*  =======
*
*  DLAIC1 applies one step of incremental condition estimation in
*  its simplest version:
*
*  Let x, twonorm(x) = 1, be an approximate singular vector of an j-by-j
*  lower triangular matrix L, such that
*           twonorm(L*x) = sest
*  Then DLAIC1 computes sestpr, s, c such that
*  the vector
*                  [ s*x ]
*           xhat = [  c  ]
*  is an approximate singular vector of
*                  [ L     0  ]
*           Lhat = [ w' gamma ]
*  in the sense that
*           twonorm(Lhat*xhat) = sestpr.
*
*  Depending on JOB, an estimate for the largest or smallest singular
*  value is computed.
*
*  Note that [s c]' and sestpr**2 is an eigenpair of the system
*
*      diag(sest*sest, 0) + [alpha  gamma] * [ alpha ]
*                                            [ gamma ]
*
*  where  alpha =  x'*w.
*
*  Arguments
*  =========
*
*  JOB     (input) INTEGER
*          = 1: an estimate for the largest singular value is computed.
*          = 2: an estimate for the smallest singular value is computed.
*
*  J       (input) INTEGER
*          Length of X and W
*
*  X       (input) DOUBLE PRECISION array, dimension (J)
*          The j-vector x.
*
*  SEST    (input) DOUBLE PRECISION
*          Estimated singular value of j by j matrix L
*
*  W       (input) DOUBLE PRECISION array, dimension (J)
*          The j-vector w.
*
*  GAMMA   (input) DOUBLE PRECISION
*          The diagonal element gamma.
*
*  SESTPR  (output) DOUBLE PRECISION
*          Estimated singular value of (j+1) by (j+1) matrix Lhat.
*
*  S       (output) DOUBLE PRECISION
*          Sine needed in forming xhat.
*
*  C       (output) DOUBLE PRECISION
*          Cosine needed in forming xhat.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
      DOUBLE PRECISION   HALF, FOUR
      PARAMETER          ( HALF = 0.5D0, FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   ABSALP, ABSEST, ABSGAM, ALPHA, B, COSINE, EPS,
     $                   NORMA, S1, S2, SINE, T, TEST, TMP, ZETA1, ZETA2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN, SQRT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DLAMCH
      EXTERNAL           DDOT, DLAMCH
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'Epsilon' )
      ALPHA = DDOT( J, X, 1, W, 1 )
*
      ABSALP = ABS( ALPHA )
      ABSGAM = ABS( GAMMA )
      ABSEST = ABS( SEST )
*
      IF( JOB.EQ.1 ) THEN
*
*        Estimating largest singular value
*
*        special cases
*
         IF( SEST.EQ.ZERO ) THEN
            S1 = MAX( ABSGAM, ABSALP )
            IF( S1.EQ.ZERO ) THEN
               S = ZERO
               C = ONE
               SESTPR = ZERO
            ELSE
               S = ALPHA / S1
               C = GAMMA / S1
               TMP = SQRT( S*S+C*C )
               S = S / TMP
               C = C / TMP
               SESTPR = S1*TMP
            END IF
            RETURN
         ELSE IF( ABSGAM.LE.EPS*ABSEST ) THEN
            S = ONE
            C = ZERO
            TMP = MAX( ABSEST, ABSALP )
            S1 = ABSEST / TMP
            S2 = ABSALP / TMP
            SESTPR = TMP*SQRT( S1*S1+S2*S2 )
            RETURN
         ELSE IF( ABSALP.LE.EPS*ABSEST ) THEN
            S1 = ABSGAM
            S2 = ABSEST
            IF( S1.LE.S2 ) THEN
               S = ONE
               C = ZERO
               SESTPR = S2
            ELSE
               S = ZERO
               C = ONE
               SESTPR = S1
            END IF
            RETURN
         ELSE IF( ABSEST.LE.EPS*ABSALP .OR. ABSEST.LE.EPS*ABSGAM ) THEN
            S1 = ABSGAM
            S2 = ABSALP
            IF( S1.LE.S2 ) THEN
               TMP = S1 / S2
               S = SQRT( ONE+TMP*TMP )
               SESTPR = S2*S
               C = ( GAMMA / S2 ) / S
               S = SIGN( ONE, ALPHA ) / S
            ELSE
               TMP = S2 / S1
               C = SQRT( ONE+TMP*TMP )
               SESTPR = S1*C
               S = ( ALPHA / S1 ) / C
               C = SIGN( ONE, GAMMA ) / C
            END IF
            RETURN
         ELSE
*
*           normal case
*
            ZETA1 = ALPHA / ABSEST
            ZETA2 = GAMMA / ABSEST
*
            B = ( ONE-ZETA1*ZETA1-ZETA2*ZETA2 )*HALF
            C = ZETA1*ZETA1
            IF( B.GT.ZERO ) THEN
               T = C / ( B+SQRT( B*B+C ) )
            ELSE
               T = SQRT( B*B+C ) - B
            END IF
*
            SINE = -ZETA1 / T
            COSINE = -ZETA2 / ( ONE+T )
            TMP = SQRT( SINE*SINE+COSINE*COSINE )
            S = SINE / TMP
            C = COSINE / TMP
            SESTPR = SQRT( T+ONE )*ABSEST
            RETURN
         END IF
*
      ELSE IF( JOB.EQ.2 ) THEN
*
*        Estimating smallest singular value
*
*        special cases
*
         IF( SEST.EQ.ZERO ) THEN
            SESTPR = ZERO
            IF( MAX( ABSGAM, ABSALP ).EQ.ZERO ) THEN
               SINE = ONE
               COSINE = ZERO
            ELSE
               SINE = -GAMMA
               COSINE = ALPHA
            END IF
            S1 = MAX( ABS( SINE ), ABS( COSINE ) )
            S = SINE / S1
            C = COSINE / S1
            TMP = SQRT( S*S+C*C )
            S = S / TMP
            C = C / TMP
            RETURN
         ELSE IF( ABSGAM.LE.EPS*ABSEST ) THEN
            S = ZERO
            C = ONE
            SESTPR = ABSGAM
            RETURN
         ELSE IF( ABSALP.LE.EPS*ABSEST ) THEN
            S1 = ABSGAM
            S2 = ABSEST
            IF( S1.LE.S2 ) THEN
               S = ZERO
               C = ONE
               SESTPR = S1
            ELSE
               S = ONE
               C = ZERO
               SESTPR = S2
            END IF
            RETURN
         ELSE IF( ABSEST.LE.EPS*ABSALP .OR. ABSEST.LE.EPS*ABSGAM ) THEN
            S1 = ABSGAM
            S2 = ABSALP
            IF( S1.LE.S2 ) THEN
               TMP = S1 / S2
               C = SQRT( ONE+TMP*TMP )
               SESTPR = ABSEST*( TMP / C )
               S = -( GAMMA / S2 ) / C
               C = SIGN( ONE, ALPHA ) / C
            ELSE
               TMP = S2 / S1
               S = SQRT( ONE+TMP*TMP )
               SESTPR = ABSEST / S
               C = ( ALPHA / S1 ) / S
               S = -SIGN( ONE, GAMMA ) / S
            END IF
            RETURN
         ELSE
*
*           normal case
*
            ZETA1 = ALPHA / ABSEST
            ZETA2 = GAMMA / ABSEST
*
            NORMA = MAX( ONE+ZETA1*ZETA1+ABS( ZETA1*ZETA2 ),
     $              ABS( ZETA1*ZETA2 )+ZETA2*ZETA2 )
*
*           See if root is closer to zero or to ONE
*
            TEST = ONE + TWO*( ZETA1-ZETA2 )*( ZETA1+ZETA2 )
            IF( TEST.GE.ZERO ) THEN
*
*              root is close to zero, compute directly
*
               B = ( ZETA1*ZETA1+ZETA2*ZETA2+ONE )*HALF
               C = ZETA2*ZETA2
               T = C / ( B+SQRT( ABS( B*B-C ) ) )
               SINE = ZETA1 / ( ONE-T )
               COSINE = -ZETA2 / T
               SESTPR = SQRT( T+FOUR*EPS*EPS*NORMA )*ABSEST
            ELSE
*
*              root is closer to ONE, shift by that amount
*
               B = ( ZETA2*ZETA2+ZETA1*ZETA1-ONE )*HALF
               C = ZETA1*ZETA1
               IF( B.GE.ZERO ) THEN
                  T = -C / ( B+SQRT( B*B+C ) )
               ELSE
                  T = B - SQRT( B*B+C )
               END IF
               SINE = -ZETA1 / T
               COSINE = -ZETA2 / ( ONE+T )
               SESTPR = SQRT( ONE+T+FOUR*EPS*EPS*NORMA )*ABSEST
            END IF
            TMP = SQRT( SINE*SINE+COSINE*COSINE )
            S = SINE / TMP
            C = COSINE / TMP
            RETURN
*
         END IF
      END IF
      RETURN
*
*     End of DLAIC1
*
      END
      SUBROUTINE DLALN2( LTRANS, NA, NW, SMIN, CA, A, LDA, D1, D2, B,
     $                   LDB, WR, WI, X, LDX, SCALE, XNORM, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            LTRANS
      INTEGER            INFO, LDA, LDB, LDX, NA, NW
      DOUBLE PRECISION   CA, D1, D2, SCALE, SMIN, WI, WR, XNORM
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DLALN2 solves a system of the form  (ca A - w D ) X = s B
*  or (ca A' - w D) X = s B   with possible scaling ("s") and
*  perturbation of A.  (A' means A-transpose.)
*
*  A is an NA x NA real matrix, ca is a real scalar, D is an NA x NA
*  real diagonal matrix, w is a real or complex value, and X and B are
*  NA x 1 matrices -- real if w is real, complex if w is complex.  NA
*  may be 1 or 2.
*
*  If w is complex, X and B are represented as NA x 2 matrices,
*  the first column of each being the real part and the second
*  being the imaginary part.
*
*  "s" is a scaling factor (.LE. 1), computed by DLALN2, which is
*  so chosen that X can be computed without overflow.  X is further
*  scaled if necessary to assure that norm(ca A - w D)*norm(X) is less
*  than overflow.
*
*  If both singular values of (ca A - w D) are less than SMIN,
*  SMIN*identity will be used instead of (ca A - w D).  If only one
*  singular value is less than SMIN, one element of (ca A - w D) will be
*  perturbed enough to make the smallest singular value roughly SMIN.
*  If both singular values are at least SMIN, (ca A - w D) will not be
*  perturbed.  In any case, the perturbation will be at most some small
*  multiple of max( SMIN, ulp*norm(ca A - w D) ).  The singular values
*  are computed by infinity-norm approximations, and thus will only be
*  correct to a factor of 2 or so.
*
*  Note: all input quantities are assumed to be smaller than overflow
*  by a reasonable factor.  (See BIGNUM.)
*
*  Arguments
*  ==========
*
*  LTRANS  (input) LOGICAL
*          =.TRUE.:  A-transpose will be used.
*          =.FALSE.: A will be used (not transposed.)
*
*  NA      (input) INTEGER
*          The size of the matrix A.  It may (only) be 1 or 2.
*
*  NW      (input) INTEGER
*          1 if "w" is real, 2 if "w" is complex.  It may only be 1
*          or 2.
*
*  SMIN    (input) DOUBLE PRECISION
*          The desired lower bound on the singular values of A.  This
*          should be a safe distance away from underflow or overflow,
*          say, between (underflow/machine precision) and  (machine
*          precision * overflow ).  (See BIGNUM and ULP.)
*
*  CA      (input) DOUBLE PRECISION
*          The coefficient c, which A is multiplied by.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,NA)
*          The NA x NA matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.  It must be at least NA.
*
*  D1      (input) DOUBLE PRECISION
*          The 1,1 element in the diagonal matrix D.
*
*  D2      (input) DOUBLE PRECISION
*          The 2,2 element in the diagonal matrix D.  Not used if NW=1.
*
*  B       (input) DOUBLE PRECISION array, dimension (LDB,NW)
*          The NA x NW matrix B (right-hand side).  If NW=2 ("w" is
*          complex), column 1 contains the real part of B and column 2
*          contains the imaginary part.
*
*  LDB     (input) INTEGER
*          The leading dimension of B.  It must be at least NA.
*
*  WR      (input) DOUBLE PRECISION
*          The real part of the scalar "w".
*
*  WI      (input) DOUBLE PRECISION
*          The imaginary part of the scalar "w".  Not used if NW=1.
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NW)
*          The NA x NW matrix X (unknowns), as computed by DLALN2.
*          If NW=2 ("w" is complex), on exit, column 1 will contain
*          the real part of X and column 2 will contain the imaginary
*          part.
*
*  LDX     (input) INTEGER
*          The leading dimension of X.  It must be at least NA.
*
*  SCALE   (output) DOUBLE PRECISION
*          The scale factor that B must be multiplied by to insure
*          that overflow does not occur when computing X.  Thus,
*          (ca A - w D) X  will be SCALE*B, not B (ignoring
*          perturbations of A.)  It will be at most 1.
*
*  XNORM   (output) DOUBLE PRECISION
*          The infinity-norm of X, when X is regarded as an NA x NW
*          real matrix.
*
*  INFO    (output) INTEGER
*          An error flag.  It will be set to zero if no error occurs,
*          a negative number if an argument is in error, or a positive
*          number if  ca A - w D  had to be perturbed.
*          The possible values are:
*          = 0: No error occurred, and (ca A - w D) did not have to be
*                 perturbed.
*          = 1: (ca A - w D) had to be perturbed to make its smallest
*               (or only) singular value greater than SMIN.
*          NOTE: In the interests of speed, this routine does not
*                check the inputs for errors.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            ICMAX, J
      DOUBLE PRECISION   BBND, BI1, BI2, BIGNUM, BNORM, BR1, BR2, CI21,
     $                   CI22, CMAX, CNORM, CR21, CR22, CSI, CSR, LI21,
     $                   LR21, SMINI, SMLNUM, TEMP, U22ABS, UI11, UI11R,
     $                   UI12, UI12S, UI22, UR11, UR11R, UR12, UR12S,
     $                   UR22, XI1, XI2, XR1, XR2
*     ..
*     .. Local Arrays ..
      LOGICAL            RSWAP( 4 ), ZSWAP( 4 )
      INTEGER            IPIVOT( 4, 4 )
      DOUBLE PRECISION   CI( 2, 2 ), CIV( 4 ), CR( 2, 2 ), CRV( 4 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLADIV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Equivalences ..
      EQUIVALENCE        ( CI( 1, 1 ), CIV( 1 ) ),
     $                   ( CR( 1, 1 ), CRV( 1 ) )
*     ..
*     .. Data statements ..
      DATA               ZSWAP / .FALSE., .FALSE., .TRUE., .TRUE. /
      DATA               RSWAP / .FALSE., .TRUE., .FALSE., .TRUE. /
      DATA               IPIVOT / 1, 2, 3, 4, 2, 1, 4, 3, 3, 4, 1, 2, 4,
     $                   3, 2, 1 /
*     ..
*     .. Executable Statements ..
*
*     Compute BIGNUM
*
      SMLNUM = TWO*DLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SMLNUM
      SMINI = MAX( SMIN, SMLNUM )
*
*     Don't check for input errors
*
      INFO = 0
*
*     Standard Initializations
*
      SCALE = ONE
*
      IF( NA.EQ.1 ) THEN
*
*        1 x 1  (i.e., scalar) system   C X = B
*
         IF( NW.EQ.1 ) THEN
*
*           Real 1x1 system.
*
*           C = ca A - w D
*
            CSR = CA*A( 1, 1 ) - WR*D1
            CNORM = ABS( CSR )
*
*           If | C | < SMINI, use C = SMINI
*
            IF( CNORM.LT.SMINI ) THEN
               CSR = SMINI
               CNORM = SMINI
               INFO = 1
            END IF
*
*           Check scaling for  X = B / C
*
            BNORM = ABS( B( 1, 1 ) )
            IF( CNORM.LT.ONE .AND. BNORM.GT.ONE ) THEN
               IF( BNORM.GT.BIGNUM*CNORM )
     $            SCALE = ONE / BNORM
            END IF
*
*           Compute X
*
            X( 1, 1 ) = ( B( 1, 1 )*SCALE ) / CSR
            XNORM = ABS( X( 1, 1 ) )
         ELSE
*
*           Complex 1x1 system (w is complex)
*
*           C = ca A - w D
*
            CSR = CA*A( 1, 1 ) - WR*D1
            CSI = -WI*D1
            CNORM = ABS( CSR ) + ABS( CSI )
*
*           If | C | < SMINI, use C = SMINI
*
            IF( CNORM.LT.SMINI ) THEN
               CSR = SMINI
               CSI = ZERO
               CNORM = SMINI
               INFO = 1
            END IF
*
*           Check scaling for  X = B / C
*
            BNORM = ABS( B( 1, 1 ) ) + ABS( B( 1, 2 ) )
            IF( CNORM.LT.ONE .AND. BNORM.GT.ONE ) THEN
               IF( BNORM.GT.BIGNUM*CNORM )
     $            SCALE = ONE / BNORM
            END IF
*
*           Compute X
*
            CALL DLADIV( SCALE*B( 1, 1 ), SCALE*B( 1, 2 ), CSR, CSI,
     $                   X( 1, 1 ), X( 1, 2 ) )
            XNORM = ABS( X( 1, 1 ) ) + ABS( X( 1, 2 ) )
         END IF
*
      ELSE
*
*        2x2 System
*
*        Compute the real part of  C = ca A - w D  (or  ca A' - w D )
*
         CR( 1, 1 ) = CA*A( 1, 1 ) - WR*D1
         CR( 2, 2 ) = CA*A( 2, 2 ) - WR*D2
         IF( LTRANS ) THEN
            CR( 1, 2 ) = CA*A( 2, 1 )
            CR( 2, 1 ) = CA*A( 1, 2 )
         ELSE
            CR( 2, 1 ) = CA*A( 2, 1 )
            CR( 1, 2 ) = CA*A( 1, 2 )
         END IF
*
         IF( NW.EQ.1 ) THEN
*
*           Real 2x2 system  (w is real)
*
*           Find the largest element in C
*
            CMAX = ZERO
            ICMAX = 0
*
            DO 10 J = 1, 4
               IF( ABS( CRV( J ) ).GT.CMAX ) THEN
                  CMAX = ABS( CRV( J ) )
                  ICMAX = J
               END IF
   10       CONTINUE
*
*           If norm(C) < SMINI, use SMINI*identity.
*
            IF( CMAX.LT.SMINI ) THEN
               BNORM = MAX( ABS( B( 1, 1 ) ), ABS( B( 2, 1 ) ) )
               IF( SMINI.LT.ONE .AND. BNORM.GT.ONE ) THEN
                  IF( BNORM.GT.BIGNUM*SMINI )
     $               SCALE = ONE / BNORM
               END IF
               TEMP = SCALE / SMINI
               X( 1, 1 ) = TEMP*B( 1, 1 )
               X( 2, 1 ) = TEMP*B( 2, 1 )
               XNORM = TEMP*BNORM
               INFO = 1
               RETURN
            END IF
*
*           Gaussian elimination with complete pivoting.
*
            UR11 = CRV( ICMAX )
            CR21 = CRV( IPIVOT( 2, ICMAX ) )
            UR12 = CRV( IPIVOT( 3, ICMAX ) )
            CR22 = CRV( IPIVOT( 4, ICMAX ) )
            UR11R = ONE / UR11
            LR21 = UR11R*CR21
            UR22 = CR22 - UR12*LR21
*
*           If smaller pivot < SMINI, use SMINI
*
            IF( ABS( UR22 ).LT.SMINI ) THEN
               UR22 = SMINI
               INFO = 1
            END IF
            IF( RSWAP( ICMAX ) ) THEN
               BR1 = B( 2, 1 )
               BR2 = B( 1, 1 )
            ELSE
               BR1 = B( 1, 1 )
               BR2 = B( 2, 1 )
            END IF
            BR2 = BR2 - LR21*BR1
            BBND = MAX( ABS( BR1*( UR22*UR11R ) ), ABS( BR2 ) )
            IF( BBND.GT.ONE .AND. ABS( UR22 ).LT.ONE ) THEN
               IF( BBND.GE.BIGNUM*ABS( UR22 ) )
     $            SCALE = ONE / BBND
            END IF
*
            XR2 = ( BR2*SCALE ) / UR22
            XR1 = ( SCALE*BR1 )*UR11R - XR2*( UR11R*UR12 )
            IF( ZSWAP( ICMAX ) ) THEN
               X( 1, 1 ) = XR2
               X( 2, 1 ) = XR1
            ELSE
               X( 1, 1 ) = XR1
               X( 2, 1 ) = XR2
            END IF
            XNORM = MAX( ABS( XR1 ), ABS( XR2 ) )
*
*           Further scaling if  norm(A) norm(X) > overflow
*
            IF( XNORM.GT.ONE .AND. CMAX.GT.ONE ) THEN
               IF( XNORM.GT.BIGNUM / CMAX ) THEN
                  TEMP = CMAX / BIGNUM
                  X( 1, 1 ) = TEMP*X( 1, 1 )
                  X( 2, 1 ) = TEMP*X( 2, 1 )
                  XNORM = TEMP*XNORM
                  SCALE = TEMP*SCALE
               END IF
            END IF
         ELSE
*
*           Complex 2x2 system  (w is complex)
*
*           Find the largest element in C
*
            CI( 1, 1 ) = -WI*D1
            CI( 2, 1 ) = ZERO
            CI( 1, 2 ) = ZERO
            CI( 2, 2 ) = -WI*D2
            CMAX = ZERO
            ICMAX = 0
*
            DO 20 J = 1, 4
               IF( ABS( CRV( J ) )+ABS( CIV( J ) ).GT.CMAX ) THEN
                  CMAX = ABS( CRV( J ) ) + ABS( CIV( J ) )
                  ICMAX = J
               END IF
   20       CONTINUE
*
*           If norm(C) < SMINI, use SMINI*identity.
*
            IF( CMAX.LT.SMINI ) THEN
               BNORM = MAX( ABS( B( 1, 1 ) )+ABS( B( 1, 2 ) ),
     $                 ABS( B( 2, 1 ) )+ABS( B( 2, 2 ) ) )
               IF( SMINI.LT.ONE .AND. BNORM.GT.ONE ) THEN
                  IF( BNORM.GT.BIGNUM*SMINI )
     $               SCALE = ONE / BNORM
               END IF
               TEMP = SCALE / SMINI
               X( 1, 1 ) = TEMP*B( 1, 1 )
               X( 2, 1 ) = TEMP*B( 2, 1 )
               X( 1, 2 ) = TEMP*B( 1, 2 )
               X( 2, 2 ) = TEMP*B( 2, 2 )
               XNORM = TEMP*BNORM
               INFO = 1
               RETURN
            END IF
*
*           Gaussian elimination with complete pivoting.
*
            UR11 = CRV( ICMAX )
            UI11 = CIV( ICMAX )
            CR21 = CRV( IPIVOT( 2, ICMAX ) )
            CI21 = CIV( IPIVOT( 2, ICMAX ) )
            UR12 = CRV( IPIVOT( 3, ICMAX ) )
            UI12 = CIV( IPIVOT( 3, ICMAX ) )
            CR22 = CRV( IPIVOT( 4, ICMAX ) )
            CI22 = CIV( IPIVOT( 4, ICMAX ) )
            IF( ICMAX.EQ.1 .OR. ICMAX.EQ.4 ) THEN
*
*              Code when off-diagonals of pivoted C are real
*
               IF( ABS( UR11 ).GT.ABS( UI11 ) ) THEN
                  TEMP = UI11 / UR11
                  UR11R = ONE / ( UR11*( ONE+TEMP**2 ) )
                  UI11R = -TEMP*UR11R
               ELSE
                  TEMP = UR11 / UI11
                  UI11R = -ONE / ( UI11*( ONE+TEMP**2 ) )
                  UR11R = -TEMP*UI11R
               END IF
               LR21 = CR21*UR11R
               LI21 = CR21*UI11R
               UR12S = UR12*UR11R
               UI12S = UR12*UI11R
               UR22 = CR22 - UR12*LR21
               UI22 = CI22 - UR12*LI21
            ELSE
*
*              Code when diagonals of pivoted C are real
*
               UR11R = ONE / UR11
               UI11R = ZERO
               LR21 = CR21*UR11R
               LI21 = CI21*UR11R
               UR12S = UR12*UR11R
               UI12S = UI12*UR11R
               UR22 = CR22 - UR12*LR21 + UI12*LI21
               UI22 = -UR12*LI21 - UI12*LR21
            END IF
            U22ABS = ABS( UR22 ) + ABS( UI22 )
*
*           If smaller pivot < SMINI, use SMINI
*
            IF( U22ABS.LT.SMINI ) THEN
               UR22 = SMINI
               UI22 = ZERO
               INFO = 1
            END IF
            IF( RSWAP( ICMAX ) ) THEN
               BR2 = B( 1, 1 )
               BR1 = B( 2, 1 )
               BI2 = B( 1, 2 )
               BI1 = B( 2, 2 )
            ELSE
               BR1 = B( 1, 1 )
               BR2 = B( 2, 1 )
               BI1 = B( 1, 2 )
               BI2 = B( 2, 2 )
            END IF
            BR2 = BR2 - LR21*BR1 + LI21*BI1
            BI2 = BI2 - LI21*BR1 - LR21*BI1
            BBND = MAX( ( ABS( BR1 )+ABS( BI1 ) )*
     $             ( U22ABS*( ABS( UR11R )+ABS( UI11R ) ) ),
     $             ABS( BR2 )+ABS( BI2 ) )
            IF( BBND.GT.ONE .AND. U22ABS.LT.ONE ) THEN
               IF( BBND.GE.BIGNUM*U22ABS ) THEN
                  SCALE = ONE / BBND
                  BR1 = SCALE*BR1
                  BI1 = SCALE*BI1
                  BR2 = SCALE*BR2
                  BI2 = SCALE*BI2
               END IF
            END IF
*
            CALL DLADIV( BR2, BI2, UR22, UI22, XR2, XI2 )
            XR1 = UR11R*BR1 - UI11R*BI1 - UR12S*XR2 + UI12S*XI2
            XI1 = UI11R*BR1 + UR11R*BI1 - UI12S*XR2 - UR12S*XI2
            IF( ZSWAP( ICMAX ) ) THEN
               X( 1, 1 ) = XR2
               X( 2, 1 ) = XR1
               X( 1, 2 ) = XI2
               X( 2, 2 ) = XI1
            ELSE
               X( 1, 1 ) = XR1
               X( 2, 1 ) = XR2
               X( 1, 2 ) = XI1
               X( 2, 2 ) = XI2
            END IF
            XNORM = MAX( ABS( XR1 )+ABS( XI1 ), ABS( XR2 )+ABS( XI2 ) )
*
*           Further scaling if  norm(A) norm(X) > overflow
*
            IF( XNORM.GT.ONE .AND. CMAX.GT.ONE ) THEN
               IF( XNORM.GT.BIGNUM / CMAX ) THEN
                  TEMP = CMAX / BIGNUM
                  X( 1, 1 ) = TEMP*X( 1, 1 )
                  X( 2, 1 ) = TEMP*X( 2, 1 )
                  X( 1, 2 ) = TEMP*X( 1, 2 )
                  X( 2, 2 ) = TEMP*X( 2, 2 )
                  XNORM = TEMP*XNORM
                  SCALE = TEMP*SCALE
               END IF
            END IF
         END IF
      END IF
*
      RETURN
*
*     End of DLALN2
*
      END
      DOUBLE PRECISION FUNCTION DLANGB( NORM, N, KL, KU, AB, LDAB,
     $                 WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            KL, KU, LDAB, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANGB  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the element of  largest absolute value  of an
*  n by n band matrix  A,  with kl sub-diagonals and ku super-diagonals.
*
*  Description
*  ===========
*
*  DLANGB returns the value
*
*     DLANGB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANGB as described
*          above.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANGB is
*          set to zero.
*
*  KL      (input) INTEGER
*          The number of sub-diagonals of the matrix A.  KL >= 0.
*
*  KU      (input) INTEGER
*          The number of super-diagonals of the matrix A.  KU >= 0.
*
*  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
*          The band matrix A, stored in rows 1 to KL+KU+1.  The j-th
*          column of A is stored in the j-th column of the array AB as
*          follows:
*          AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(n,j+kl).
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KL+KU+1.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
*          where LWORK >= N when NORM = 'I'; otherwise, WORK is not
*          referenced.
*
* =====================================================================
*
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, K, L
      DOUBLE PRECISION   SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
            DO 10 I = MAX( KU+2-J, 1 ), MIN( N+KU+1-J, KL+KU+1 )
               VALUE = MAX( VALUE, ABS( AB( I, J ) ) )
   10       CONTINUE
   20    CONTINUE
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         DO 40 J = 1, N
            SUM = ZERO
            DO 30 I = MAX( KU+2-J, 1 ), MIN( N+KU+1-J, KL+KU+1 )
               SUM = SUM + ABS( AB( I, J ) )
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
            K = KU + 1 - J
            DO 60 I = MAX( 1, J-KU ), MIN( N, J+KL )
               WORK( I ) = WORK( I ) + ABS( AB( K+I, J ) )
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
            L = MAX( 1, J-KU )
            K = KU + 1 - J + L
            CALL DLASSQ( MIN( N, J+KL )-L+1, AB( K, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANGB = VALUE
      RETURN
*
*     End of DLANGB
*
      END
      DOUBLE PRECISION FUNCTION DLANGE( NORM, M, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANGE  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real matrix A.
*
*  Description
*  ===========
*
*  DLANGE returns the value
*
*     DLANGE = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANGE as described
*          above.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.  When M = 0,
*          DLANGE is set to zero.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.  When N = 0,
*          DLANGE is set to zero.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The m by n matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(M,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
            CALL DLASSQ( M, A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANGE = VALUE
      RETURN
*
*     End of DLANGE
*
      END
      DOUBLE PRECISION FUNCTION DLANGT( NORM, N, DL, D, DU )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DL( * ), DU( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANGT  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real tridiagonal matrix A.
*
*  Description
*  ===========
*
*  DLANGT returns the value
*
*     DLANGT = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANGT as described
*          above.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANGT is
*          set to zero.
*
*  DL      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) sub-diagonal elements of A.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The diagonal elements of A.
*
*  DU      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) super-diagonal elements of A.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   ANORM, SCALE, SUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.0 ) THEN
         ANORM = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         ANORM = ABS( D( N ) )
         DO 10 I = 1, N - 1
            ANORM = MAX( ANORM, ABS( DL( I ) ) )
            ANORM = MAX( ANORM, ABS( D( I ) ) )
            ANORM = MAX( ANORM, ABS( DU( I ) ) )
   10    CONTINUE
      ELSE IF( LSAME( NORM, 'O' ) .OR. NORM.EQ.'1' ) THEN
*
*        Find norm1(A).
*
         IF( N.EQ.1 ) THEN
            ANORM = ABS( D( 1 ) )
         ELSE
            ANORM = MAX( ABS( D( 1 ) )+ABS( DL( 1 ) ),
     $              ABS( D( N ) )+ABS( DU( N-1 ) ) )
            DO 20 I = 2, N - 1
               ANORM = MAX( ANORM, ABS( D( I ) )+ABS( DL( I ) )+
     $                 ABS( DU( I-1 ) ) )
   20       CONTINUE
         END IF
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         IF( N.EQ.1 ) THEN
            ANORM = ABS( D( 1 ) )
         ELSE
            ANORM = MAX( ABS( D( 1 ) )+ABS( DU( 1 ) ),
     $              ABS( D( N ) )+ABS( DL( N-1 ) ) )
            DO 30 I = 2, N - 1
               ANORM = MAX( ANORM, ABS( D( I ) )+ABS( DU( I ) )+
     $                 ABS( DL( I-1 ) ) )
   30       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         CALL DLASSQ( N, D, 1, SCALE, SUM )
         IF( N.GT.1 ) THEN
            CALL DLASSQ( N-1, DL, 1, SCALE, SUM )
            CALL DLASSQ( N-1, DU, 1, SCALE, SUM )
         END IF
         ANORM = SCALE*SQRT( SUM )
      END IF
*
      DLANGT = ANORM
      RETURN
*
*     End of DLANGT
*
      END
      DOUBLE PRECISION FUNCTION DLANHS( NORM, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANHS  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  Hessenberg matrix A.
*
*  Description
*  ===========
*
*  DLANHS returns the value
*
*     DLANHS = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANHS as described
*          above.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANHS is
*          set to zero.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The n by n upper Hessenberg matrix A; the part of A below the
*          first sub-diagonal is not referenced.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(N,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
            CALL DLASSQ( MIN( N, J+1 ), A( 1, J ), 1, SCALE, SUM )
   90    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANHS = VALUE
      RETURN
*
*     End of DLANHS
*
      END
      DOUBLE PRECISION FUNCTION DLANSB( NORM, UPLO, N, K, AB, LDAB,
     $                 WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          NORM, UPLO
      INTEGER            K, LDAB, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANSB  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the element of  largest absolute value  of an
*  n by n symmetric band matrix A,  with k super-diagonals.
*
*  Description
*  ===========
*
*  DLANSB returns the value
*
*     DLANSB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANSB as described
*          above.
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          band matrix A is supplied.
*          = 'U':  Upper triangular part is supplied
*          = 'L':  Lower triangular part is supplied
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANSB is
*          set to zero.
*
*  K       (input) INTEGER
*          The number of super-diagonals or sub-diagonals of the
*          band matrix A.  K >= 0.
*
*  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
*          The upper or lower triangle of the symmetric band matrix A,
*          stored in the first K+1 rows of AB.  The j-th column of A is
*          stored in the j-th column of the array AB as follows:
*          if UPLO = 'U', AB(k+1+i-j,j) = A(i,j) for max(1,j-k)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)   = A(i,j) for j<=i<=min(n,j+k).
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= K+1.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
      INTEGER            I, J, L
      DOUBLE PRECISION   ABSA, SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 20 J = 1, N
               DO 10 I = MAX( K+2-J, 1 ), K + 1
                  VALUE = MAX( VALUE, ABS( AB( I, J ) ) )
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40 J = 1, N
               DO 30 I = 1, MIN( N+1-J, K+1 )
                  VALUE = MAX( VALUE, ABS( AB( I, J ) ) )
   30          CONTINUE
   40       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'I' ) ) .OR. ( LSAME( NORM, 'O' ) ) .OR.
     $         ( NORM.EQ.'1' ) ) THEN
*
*        Find normI(A) ( = norm1(A), since A is symmetric).
*
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 60 J = 1, N
               SUM = ZERO
               L = K + 1 - J
               DO 50 I = MAX( 1, J-K ), J - 1
                  ABSA = ABS( AB( L+I, J ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
   50          CONTINUE
               WORK( J ) = SUM + ABS( AB( K+1, J ) )
   60       CONTINUE
            DO 70 I = 1, N
               VALUE = MAX( VALUE, WORK( I ) )
   70       CONTINUE
         ELSE
            DO 80 I = 1, N
               WORK( I ) = ZERO
   80       CONTINUE
            DO 100 J = 1, N
               SUM = WORK( J ) + ABS( AB( 1, J ) )
               L = 1 - J
               DO 90 I = J + 1, MIN( N, J+K )
                  ABSA = ABS( AB( L+I, J ) )
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
         IF( K.GT.0 ) THEN
            IF( LSAME( UPLO, 'U' ) ) THEN
               DO 110 J = 2, N
                  CALL DLASSQ( MIN( J-1, K ), AB( MAX( K+2-J, 1 ), J ),
     $                         1, SCALE, SUM )
  110          CONTINUE
               L = K + 1
            ELSE
               DO 120 J = 1, N - 1
                  CALL DLASSQ( MIN( N-J, K ), AB( 2, J ), 1, SCALE,
     $                         SUM )
  120          CONTINUE
               L = 1
            END IF
            SUM = 2*SUM
         ELSE
            L = 1
         END IF
         CALL DLASSQ( N, AB( L, 1 ), LDAB, SCALE, SUM )
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANSB = VALUE
      RETURN
*
*     End of DLANSB
*
      END
      DOUBLE PRECISION FUNCTION DLANSP( NORM, UPLO, N, AP, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          NORM, UPLO
      INTEGER            N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AP( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANSP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real symmetric matrix A,  supplied in packed form.
*
*  Description
*  ===========
*
*  DLANSP returns the value
*
*     DLANSP = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANSP as described
*          above.
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is supplied.
*          = 'U':  Upper triangular part of A is supplied
*          = 'L':  Lower triangular part of A is supplied
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANSP is
*          set to zero.
*
*  AP      (input) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          The upper or lower triangle of the symmetric matrix A, packed
*          columnwise in a linear array.  The j-th column of A is stored
*          in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
      INTEGER            I, J, K
      DOUBLE PRECISION   ABSA, SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
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
            K = 1
            DO 20 J = 1, N
               DO 10 I = K, K + J - 1
                  VALUE = MAX( VALUE, ABS( AP( I ) ) )
   10          CONTINUE
               K = K + J
   20       CONTINUE
         ELSE
            K = 1
            DO 40 J = 1, N
               DO 30 I = K, K + N - J
                  VALUE = MAX( VALUE, ABS( AP( I ) ) )
   30          CONTINUE
               K = K + N - J + 1
   40       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'I' ) ) .OR. ( LSAME( NORM, 'O' ) ) .OR.
     $         ( NORM.EQ.'1' ) ) THEN
*
*        Find normI(A) ( = norm1(A), since A is symmetric).
*
         VALUE = ZERO
         K = 1
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 60 J = 1, N
               SUM = ZERO
               DO 50 I = 1, J - 1
                  ABSA = ABS( AP( K ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
                  K = K + 1
   50          CONTINUE
               WORK( J ) = SUM + ABS( AP( K ) )
               K = K + 1
   60       CONTINUE
            DO 70 I = 1, N
               VALUE = MAX( VALUE, WORK( I ) )
   70       CONTINUE
         ELSE
            DO 80 I = 1, N
               WORK( I ) = ZERO
   80       CONTINUE
            DO 100 J = 1, N
               SUM = WORK( J ) + ABS( AP( K ) )
               K = K + 1
               DO 90 I = J + 1, N
                  ABSA = ABS( AP( K ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
                  K = K + 1
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
         K = 2
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 110 J = 2, N
               CALL DLASSQ( J-1, AP( K ), 1, SCALE, SUM )
               K = K + J
  110       CONTINUE
         ELSE
            DO 120 J = 1, N - 1
               CALL DLASSQ( N-J, AP( K ), 1, SCALE, SUM )
               K = K + N - J + 1
  120       CONTINUE
         END IF
         SUM = 2*SUM
         K = 1
         DO 130 I = 1, N
            IF( AP( K ).NE.ZERO ) THEN
               ABSA = ABS( AP( K ) )
               IF( SCALE.LT.ABSA ) THEN
                  SUM = ONE + SUM*( SCALE / ABSA )**2
                  SCALE = ABSA
               ELSE
                  SUM = SUM + ( ABSA / SCALE )**2
               END IF
            END IF
            IF( LSAME( UPLO, 'U' ) ) THEN
               K = K + I + 1
            ELSE
               K = K + N - I + 1
            END IF
  130    CONTINUE
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANSP = VALUE
      RETURN
*
*     End of DLANSP
*
      END
      DOUBLE PRECISION FUNCTION DLANST( NORM, N, D, E )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANST  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real symmetric tridiagonal matrix A.
*
*  Description
*  ===========
*
*  DLANST returns the value
*
*     DLANST = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANST as described
*          above.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANST is
*          set to zero.
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The diagonal elements of A.
*
*  E       (input) DOUBLE PRECISION array, dimension (N-1)
*          The (n-1) sub-diagonal or super-diagonal elements of A.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   ANORM, SCALE, SUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.0 ) THEN
         ANORM = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         ANORM = ABS( D( N ) )
         DO 10 I = 1, N - 1
            ANORM = MAX( ANORM, ABS( D( I ) ) )
            ANORM = MAX( ANORM, ABS( E( I ) ) )
   10    CONTINUE
      ELSE IF( LSAME( NORM, 'O' ) .OR. NORM.EQ.'1' .OR.
     $         LSAME( NORM, 'I' ) ) THEN
*
*        Find norm1(A).
*
         IF( N.EQ.1 ) THEN
            ANORM = ABS( D( 1 ) )
         ELSE
            ANORM = MAX( ABS( D( 1 ) )+ABS( E( 1 ) ),
     $              ABS( E( N-1 ) )+ABS( D( N ) ) )
            DO 20 I = 2, N - 1
               ANORM = MAX( ANORM, ABS( D( I ) )+ABS( E( I ) )+
     $                 ABS( E( I-1 ) ) )
   20       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         SCALE = ZERO
         SUM = ONE
         IF( N.GT.1 ) THEN
            CALL DLASSQ( N-1, E, 1, SCALE, SUM )
            SUM = 2*SUM
         END IF
         CALL DLASSQ( N, D, 1, SCALE, SUM )
         ANORM = SCALE*SQRT( SUM )
      END IF
*
      DLANST = ANORM
      RETURN
*
*     End of DLANST
*
      END
      DOUBLE PRECISION FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          NORM, UPLO
      INTEGER            LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANSY  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  real symmetric matrix A.
*
*  Description
*  ===========
*
*  DLANSY returns the value
*
*     DLANSY = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANSY as described
*          above.
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is to be referenced.
*          = 'U':  Upper triangular part of A is referenced
*          = 'L':  Lower triangular part of A is referenced
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANSY is
*          set to zero.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The symmetric matrix A.  If UPLO = 'U', the leading n by n
*          upper triangular part of A contains the upper triangular part
*          of the matrix A, and the strictly lower triangular part of A
*          is not referenced.  If UPLO = 'L', the leading n by n lower
*          triangular part of A contains the lower triangular part of
*          the matrix A, and the strictly upper triangular part of A is
*          not referenced.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(N,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
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
               DO 10 I = 1, J
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40 J = 1, N
               DO 30 I = J, N
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   30          CONTINUE
   40       CONTINUE
         END IF
      ELSE IF( ( LSAME( NORM, 'I' ) ) .OR. ( LSAME( NORM, 'O' ) ) .OR.
     $         ( NORM.EQ.'1' ) ) THEN
*
*        Find normI(A) ( = norm1(A), since A is symmetric).
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
               WORK( J ) = SUM + ABS( A( J, J ) )
   60       CONTINUE
            DO 70 I = 1, N
               VALUE = MAX( VALUE, WORK( I ) )
   70       CONTINUE
         ELSE
            DO 80 I = 1, N
               WORK( I ) = ZERO
   80       CONTINUE
            DO 100 J = 1, N
               SUM = WORK( J ) + ABS( A( J, J ) )
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
               CALL DLASSQ( J-1, A( 1, J ), 1, SCALE, SUM )
  110       CONTINUE
         ELSE
            DO 120 J = 1, N - 1
               CALL DLASSQ( N-J, A( J+1, J ), 1, SCALE, SUM )
  120       CONTINUE
         END IF
         SUM = 2*SUM
         CALL DLASSQ( N, A, LDA+1, SCALE, SUM )
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANSY = VALUE
      RETURN
*
*     End of DLANSY
*
      END
      DOUBLE PRECISION FUNCTION DLANTB( NORM, UPLO, DIAG, N, K, AB,
     $                 LDAB, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, NORM, UPLO
      INTEGER            K, LDAB, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANTB  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the element of  largest absolute value  of an
*  n by n triangular band matrix A,  with ( k + 1 ) diagonals.
*
*  Description
*  ===========
*
*  DLANTB returns the value
*
*     DLANTB = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANTB as described
*          above.
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the matrix A is upper or lower triangular.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  DIAG    (input) CHARACTER*1
*          Specifies whether or not the matrix A is unit triangular.
*          = 'N':  Non-unit triangular
*          = 'U':  Unit triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANTB is
*          set to zero.
*
*  K       (input) INTEGER
*          The number of super-diagonals of the matrix A if UPLO = 'U',
*          or the number of sub-diagonals of the matrix A if UPLO = 'L'.
*          K >= 0.
*
*  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
*          The upper or lower triangular band matrix A, stored in the
*          first k+1 rows of AB.  The j-th column of A is stored
*          in the j-th column of the array AB as follows:
*          if UPLO = 'U', AB(k+1+i-j,j) = A(i,j) for max(1,j-k)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)   = A(i,j) for j<=i<=min(n,j+k).
*          Note that when DIAG = 'U', the elements of the array AB
*          corresponding to the diagonal elements of the matrix A are
*          not referenced, but are assumed to be one.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= K+1.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
      LOGICAL            UDIAG
      INTEGER            I, J, L
      DOUBLE PRECISION   SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
         IF( LSAME( DIAG, 'U' ) ) THEN
            VALUE = ONE
            IF( LSAME( UPLO, 'U' ) ) THEN
               DO 20 J = 1, N
                  DO 10 I = MAX( K+2-J, 1 ), K
                     VALUE = MAX( VALUE, ABS( AB( I, J ) ) )
   10             CONTINUE
   20          CONTINUE
            ELSE
               DO 40 J = 1, N
                  DO 30 I = 2, MIN( N+1-J, K+1 )
                     VALUE = MAX( VALUE, ABS( AB( I, J ) ) )
   30             CONTINUE
   40          CONTINUE
            END IF
         ELSE
            VALUE = ZERO
            IF( LSAME( UPLO, 'U' ) ) THEN
               DO 60 J = 1, N
                  DO 50 I = MAX( K+2-J, 1 ), K + 1
                     VALUE = MAX( VALUE, ABS( AB( I, J ) ) )
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 80 J = 1, N
                  DO 70 I = 1, MIN( N+1-J, K+1 )
                     VALUE = MAX( VALUE, ABS( AB( I, J ) ) )
   70             CONTINUE
   80          CONTINUE
            END IF
         END IF
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         UDIAG = LSAME( DIAG, 'U' )
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 110 J = 1, N
               IF( UDIAG ) THEN
                  SUM = ONE
                  DO 90 I = MAX( K+2-J, 1 ), K
                     SUM = SUM + ABS( AB( I, J ) )
   90             CONTINUE
               ELSE
                  SUM = ZERO
                  DO 100 I = MAX( K+2-J, 1 ), K + 1
                     SUM = SUM + ABS( AB( I, J ) )
  100             CONTINUE
               END IF
               VALUE = MAX( VALUE, SUM )
  110       CONTINUE
         ELSE
            DO 140 J = 1, N
               IF( UDIAG ) THEN
                  SUM = ONE
                  DO 120 I = 2, MIN( N+1-J, K+1 )
                     SUM = SUM + ABS( AB( I, J ) )
  120             CONTINUE
               ELSE
                  SUM = ZERO
                  DO 130 I = 1, MIN( N+1-J, K+1 )
                     SUM = SUM + ABS( AB( I, J ) )
  130             CONTINUE
               END IF
               VALUE = MAX( VALUE, SUM )
  140       CONTINUE
         END IF
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            IF( LSAME( DIAG, 'U' ) ) THEN
               DO 150 I = 1, N
                  WORK( I ) = ONE
  150          CONTINUE
               DO 170 J = 1, N
                  L = K + 1 - J
                  DO 160 I = MAX( 1, J-K ), J - 1
                     WORK( I ) = WORK( I ) + ABS( AB( L+I, J ) )
  160             CONTINUE
  170          CONTINUE
            ELSE
               DO 180 I = 1, N
                  WORK( I ) = ZERO
  180          CONTINUE
               DO 200 J = 1, N
                  L = K + 1 - J
                  DO 190 I = MAX( 1, J-K ), J
                     WORK( I ) = WORK( I ) + ABS( AB( L+I, J ) )
  190             CONTINUE
  200          CONTINUE
            END IF
         ELSE
            IF( LSAME( DIAG, 'U' ) ) THEN
               DO 210 I = 1, N
                  WORK( I ) = ONE
  210          CONTINUE
               DO 230 J = 1, N
                  L = 1 - J
                  DO 220 I = J + 1, MIN( N, J+K )
                     WORK( I ) = WORK( I ) + ABS( AB( L+I, J ) )
  220             CONTINUE
  230          CONTINUE
            ELSE
               DO 240 I = 1, N
                  WORK( I ) = ZERO
  240          CONTINUE
               DO 260 J = 1, N
                  L = 1 - J
                  DO 250 I = J, MIN( N, J+K )
                     WORK( I ) = WORK( I ) + ABS( AB( L+I, J ) )
  250             CONTINUE
  260          CONTINUE
            END IF
         END IF
         DO 270 I = 1, N
            VALUE = MAX( VALUE, WORK( I ) )
  270    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         IF( LSAME( UPLO, 'U' ) ) THEN
            IF( LSAME( DIAG, 'U' ) ) THEN
               SCALE = ONE
               SUM = N
               IF( K.GT.0 ) THEN
                  DO 280 J = 2, N
                     CALL DLASSQ( MIN( J-1, K ),
     $                            AB( MAX( K+2-J, 1 ), J ), 1, SCALE,
     $                            SUM )
  280             CONTINUE
               END IF
            ELSE
               SCALE = ZERO
               SUM = ONE
               DO 290 J = 1, N
                  CALL DLASSQ( MIN( J, K+1 ), AB( MAX( K+2-J, 1 ), J ),
     $                         1, SCALE, SUM )
  290          CONTINUE
            END IF
         ELSE
            IF( LSAME( DIAG, 'U' ) ) THEN
               SCALE = ONE
               SUM = N
               IF( K.GT.0 ) THEN
                  DO 300 J = 1, N - 1
                     CALL DLASSQ( MIN( N-J, K ), AB( 2, J ), 1, SCALE,
     $                            SUM )
  300             CONTINUE
               END IF
            ELSE
               SCALE = ZERO
               SUM = ONE
               DO 310 J = 1, N
                  CALL DLASSQ( MIN( N-J+1, K+1 ), AB( 1, J ), 1, SCALE,
     $                         SUM )
  310          CONTINUE
            END IF
         END IF
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANTB = VALUE
      RETURN
*
*     End of DLANTB
*
      END
      DOUBLE PRECISION FUNCTION DLANTP( NORM, UPLO, DIAG, N, AP, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, NORM, UPLO
      INTEGER            N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AP( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANTP  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  triangular matrix A, supplied in packed form.
*
*  Description
*  ===========
*
*  DLANTP returns the value
*
*     DLANTP = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANTP as described
*          above.
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the matrix A is upper or lower triangular.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  DIAG    (input) CHARACTER*1
*          Specifies whether or not the matrix A is unit triangular.
*          = 'N':  Non-unit triangular
*          = 'U':  Unit triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.  When N = 0, DLANTP is
*          set to zero.
*
*  AP      (input) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          The upper or lower triangular matrix A, packed columnwise in
*          a linear array.  The j-th column of A is stored in the array
*          AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*          Note that when DIAG = 'U', the elements of the array AP
*          corresponding to the diagonal elements of the matrix A are
*          not referenced, but are assumed to be one.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
      LOGICAL            UDIAG
      INTEGER            I, J, K
      DOUBLE PRECISION   SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( N.EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
*
*        Find max(abs(A(i,j))).
*
         K = 1
         IF( LSAME( DIAG, 'U' ) ) THEN
            VALUE = ONE
            IF( LSAME( UPLO, 'U' ) ) THEN
               DO 20 J = 1, N
                  DO 10 I = K, K + J - 2
                     VALUE = MAX( VALUE, ABS( AP( I ) ) )
   10             CONTINUE
                  K = K + J
   20          CONTINUE
            ELSE
               DO 40 J = 1, N
                  DO 30 I = K + 1, K + N - J
                     VALUE = MAX( VALUE, ABS( AP( I ) ) )
   30             CONTINUE
                  K = K + N - J + 1
   40          CONTINUE
            END IF
         ELSE
            VALUE = ZERO
            IF( LSAME( UPLO, 'U' ) ) THEN
               DO 60 J = 1, N
                  DO 50 I = K, K + J - 1
                     VALUE = MAX( VALUE, ABS( AP( I ) ) )
   50             CONTINUE
                  K = K + J
   60          CONTINUE
            ELSE
               DO 80 J = 1, N
                  DO 70 I = K, K + N - J
                     VALUE = MAX( VALUE, ABS( AP( I ) ) )
   70             CONTINUE
                  K = K + N - J + 1
   80          CONTINUE
            END IF
         END IF
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         K = 1
         UDIAG = LSAME( DIAG, 'U' )
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 110 J = 1, N
               IF( UDIAG ) THEN
                  SUM = ONE
                  DO 90 I = K, K + J - 2
                     SUM = SUM + ABS( AP( I ) )
   90             CONTINUE
               ELSE
                  SUM = ZERO
                  DO 100 I = K, K + J - 1
                     SUM = SUM + ABS( AP( I ) )
  100             CONTINUE
               END IF
               K = K + J
               VALUE = MAX( VALUE, SUM )
  110       CONTINUE
         ELSE
            DO 140 J = 1, N
               IF( UDIAG ) THEN
                  SUM = ONE
                  DO 120 I = K + 1, K + N - J
                     SUM = SUM + ABS( AP( I ) )
  120             CONTINUE
               ELSE
                  SUM = ZERO
                  DO 130 I = K, K + N - J
                     SUM = SUM + ABS( AP( I ) )
  130             CONTINUE
               END IF
               K = K + N - J + 1
               VALUE = MAX( VALUE, SUM )
  140       CONTINUE
         END IF
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         K = 1
         IF( LSAME( UPLO, 'U' ) ) THEN
            IF( LSAME( DIAG, 'U' ) ) THEN
               DO 150 I = 1, N
                  WORK( I ) = ONE
  150          CONTINUE
               DO 170 J = 1, N
                  DO 160 I = 1, J - 1
                     WORK( I ) = WORK( I ) + ABS( AP( K ) )
                     K = K + 1
  160             CONTINUE
                  K = K + 1
  170          CONTINUE
            ELSE
               DO 180 I = 1, N
                  WORK( I ) = ZERO
  180          CONTINUE
               DO 200 J = 1, N
                  DO 190 I = 1, J
                     WORK( I ) = WORK( I ) + ABS( AP( K ) )
                     K = K + 1
  190             CONTINUE
  200          CONTINUE
            END IF
         ELSE
            IF( LSAME( DIAG, 'U' ) ) THEN
               DO 210 I = 1, N
                  WORK( I ) = ONE
  210          CONTINUE
               DO 230 J = 1, N
                  K = K + 1
                  DO 220 I = J + 1, N
                     WORK( I ) = WORK( I ) + ABS( AP( K ) )
                     K = K + 1
  220             CONTINUE
  230          CONTINUE
            ELSE
               DO 240 I = 1, N
                  WORK( I ) = ZERO
  240          CONTINUE
               DO 260 J = 1, N
                  DO 250 I = J, N
                     WORK( I ) = WORK( I ) + ABS( AP( K ) )
                     K = K + 1
  250             CONTINUE
  260          CONTINUE
            END IF
         END IF
         VALUE = ZERO
         DO 270 I = 1, N
            VALUE = MAX( VALUE, WORK( I ) )
  270    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         IF( LSAME( UPLO, 'U' ) ) THEN
            IF( LSAME( DIAG, 'U' ) ) THEN
               SCALE = ONE
               SUM = N
               K = 2
               DO 280 J = 2, N
                  CALL DLASSQ( J-1, AP( K ), 1, SCALE, SUM )
                  K = K + J
  280          CONTINUE
            ELSE
               SCALE = ZERO
               SUM = ONE
               K = 1
               DO 290 J = 1, N
                  CALL DLASSQ( J, AP( K ), 1, SCALE, SUM )
                  K = K + J
  290          CONTINUE
            END IF
         ELSE
            IF( LSAME( DIAG, 'U' ) ) THEN
               SCALE = ONE
               SUM = N
               K = 2
               DO 300 J = 1, N - 1
                  CALL DLASSQ( N-J, AP( K ), 1, SCALE, SUM )
                  K = K + N - J + 1
  300          CONTINUE
            ELSE
               SCALE = ZERO
               SUM = ONE
               K = 1
               DO 310 J = 1, N
                  CALL DLASSQ( N-J+1, AP( K ), 1, SCALE, SUM )
                  K = K + N - J + 1
  310          CONTINUE
            END IF
         END IF
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANTP = VALUE
      RETURN
*
*     End of DLANTP
*
      END
      DOUBLE PRECISION FUNCTION DLANTR( NORM, UPLO, DIAG, M, N, A, LDA,
     $                 WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, NORM, UPLO
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLANTR  returns the value of the one norm,  or the Frobenius norm, or
*  the  infinity norm,  or the  element of  largest absolute value  of a
*  trapezoidal or triangular matrix A.
*
*  Description
*  ===========
*
*  DLANTR returns the value
*
*     DLANTR = ( max(abs(A(i,j))), NORM = 'M' or 'm'
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
*  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
*
*  Arguments
*  =========
*
*  NORM    (input) CHARACTER*1
*          Specifies the value to be returned in DLANTR as described
*          above.
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the matrix A is upper or lower trapezoidal.
*          = 'U':  Upper trapezoidal
*          = 'L':  Lower trapezoidal
*          Note that A is triangular instead of trapezoidal if M = N.
*
*  DIAG    (input) CHARACTER*1
*          Specifies whether or not the matrix A has unit diagonal.
*          = 'N':  Non-unit diagonal
*          = 'U':  Unit diagonal
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0, and if
*          UPLO = 'U', M <= N.  When M = 0, DLANTR is set to zero.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0, and if
*          UPLO = 'L', N <= M.  When N = 0, DLANTR is set to zero.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The trapezoidal matrix A (A is triangular if M = N).
*          If UPLO = 'U', the leading m by n upper trapezoidal part of
*          the array A contains the upper trapezoidal matrix, and the
*          strictly lower triangular part of A is not referenced.
*          If UPLO = 'L', the leading m by n lower trapezoidal part of
*          the array A contains the lower trapezoidal matrix, and the
*          strictly upper triangular part of A is not referenced.  Note
*          that when DIAG = 'U', the diagonal elements of A are not
*          referenced and are assumed to be one.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(M,1).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
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
      LOGICAL            UDIAG
      INTEGER            I, J
      DOUBLE PRECISION   SCALE, SUM, VALUE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASSQ
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
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
         IF( LSAME( DIAG, 'U' ) ) THEN
            VALUE = ONE
            IF( LSAME( UPLO, 'U' ) ) THEN
               DO 20 J = 1, N
                  DO 10 I = 1, MIN( M, J-1 )
                     VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10             CONTINUE
   20          CONTINUE
            ELSE
               DO 40 J = 1, N
                  DO 30 I = J + 1, M
                     VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   30             CONTINUE
   40          CONTINUE
            END IF
         ELSE
            VALUE = ZERO
            IF( LSAME( UPLO, 'U' ) ) THEN
               DO 60 J = 1, N
                  DO 50 I = 1, MIN( M, J )
                     VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   50             CONTINUE
   60          CONTINUE
            ELSE
               DO 80 J = 1, N
                  DO 70 I = J, M
                     VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   70             CONTINUE
   80          CONTINUE
            END IF
         END IF
      ELSE IF( ( LSAME( NORM, 'O' ) ) .OR. ( NORM.EQ.'1' ) ) THEN
*
*        Find norm1(A).
*
         VALUE = ZERO
         UDIAG = LSAME( DIAG, 'U' )
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 110 J = 1, N
               IF( ( UDIAG ) .AND. ( J.LE.M ) ) THEN
                  SUM = ONE
                  DO 90 I = 1, J - 1
                     SUM = SUM + ABS( A( I, J ) )
   90             CONTINUE
               ELSE
                  SUM = ZERO
                  DO 100 I = 1, MIN( M, J )
                     SUM = SUM + ABS( A( I, J ) )
  100             CONTINUE
               END IF
               VALUE = MAX( VALUE, SUM )
  110       CONTINUE
         ELSE
            DO 140 J = 1, N
               IF( UDIAG ) THEN
                  SUM = ONE
                  DO 120 I = J + 1, M
                     SUM = SUM + ABS( A( I, J ) )
  120             CONTINUE
               ELSE
                  SUM = ZERO
                  DO 130 I = J, M
                     SUM = SUM + ABS( A( I, J ) )
  130             CONTINUE
               END IF
               VALUE = MAX( VALUE, SUM )
  140       CONTINUE
         END IF
      ELSE IF( LSAME( NORM, 'I' ) ) THEN
*
*        Find normI(A).
*
         IF( LSAME( UPLO, 'U' ) ) THEN
            IF( LSAME( DIAG, 'U' ) ) THEN
               DO 150 I = 1, M
                  WORK( I ) = ONE
  150          CONTINUE
               DO 170 J = 1, N
                  DO 160 I = 1, MIN( M, J-1 )
                     WORK( I ) = WORK( I ) + ABS( A( I, J ) )
  160             CONTINUE
  170          CONTINUE
            ELSE
               DO 180 I = 1, M
                  WORK( I ) = ZERO
  180          CONTINUE
               DO 200 J = 1, N
                  DO 190 I = 1, MIN( M, J )
                     WORK( I ) = WORK( I ) + ABS( A( I, J ) )
  190             CONTINUE
  200          CONTINUE
            END IF
         ELSE
            IF( LSAME( DIAG, 'U' ) ) THEN
               DO 210 I = 1, N
                  WORK( I ) = ONE
  210          CONTINUE
               DO 220 I = N + 1, M
                  WORK( I ) = ZERO
  220          CONTINUE
               DO 240 J = 1, N
                  DO 230 I = J + 1, M
                     WORK( I ) = WORK( I ) + ABS( A( I, J ) )
  230             CONTINUE
  240          CONTINUE
            ELSE
               DO 250 I = 1, M
                  WORK( I ) = ZERO
  250          CONTINUE
               DO 270 J = 1, N
                  DO 260 I = J, M
                     WORK( I ) = WORK( I ) + ABS( A( I, J ) )
  260             CONTINUE
  270          CONTINUE
            END IF
         END IF
         VALUE = ZERO
         DO 280 I = 1, M
            VALUE = MAX( VALUE, WORK( I ) )
  280    CONTINUE
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
*
*        Find normF(A).
*
         IF( LSAME( UPLO, 'U' ) ) THEN
            IF( LSAME( DIAG, 'U' ) ) THEN
               SCALE = ONE
               SUM = MIN( M, N )
               DO 290 J = 2, N
                  CALL DLASSQ( MIN( M, J-1 ), A( 1, J ), 1, SCALE, SUM )
  290          CONTINUE
            ELSE
               SCALE = ZERO
               SUM = ONE
               DO 300 J = 1, N
                  CALL DLASSQ( MIN( M, J ), A( 1, J ), 1, SCALE, SUM )
  300          CONTINUE
            END IF
         ELSE
            IF( LSAME( DIAG, 'U' ) ) THEN
               SCALE = ONE
               SUM = MIN( M, N )
               DO 310 J = 1, N
                  CALL DLASSQ( M-J, A( MIN( M, J+1 ), J ), 1, SCALE,
     $                         SUM )
  310          CONTINUE
            ELSE
               SCALE = ZERO
               SUM = ONE
               DO 320 J = 1, N
                  CALL DLASSQ( M-J+1, A( J, J ), 1, SCALE, SUM )
  320          CONTINUE
            END IF
         END IF
         VALUE = SCALE*SQRT( SUM )
      END IF
*
      DLANTR = VALUE
      RETURN
*
*     End of DLANTR
*
      END
      SUBROUTINE DLAPLL( N, X, INCX, Y, INCY, SSMIN )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCX, INCY, N
      DOUBLE PRECISION   SSMIN
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  Given two column vectors X and Y, let
*
*                       A = ( X Y ).
*
*  The subroutine first computes the QR factorization of A = Q*R,
*  and then computes the SVD of the 2-by-2 upper triangular matrix R.
*  The smaller singular value of R is returned in SSMIN, which is used
*  as the measurement of the linear dependency of the vectors X and Y.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The length of the vectors X and Y.
*
*  X       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCX)
*          On entry, X contains the N-vector X.
*          On exit, X is overwritten.
*
*  INCX    (input) INTEGER
*          The increment between successive elements of X. INCX > 0.
*
*  Y       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCY)
*          On entry, Y contains the N-vector Y.
*          On exit, Y is overwritten.
*
*  INCY    (input) INTEGER
*          The increment between successive elements of Y. INCY > 0.
*
*  SSMIN   (output) DOUBLE PRECISION
*          The smallest singular value of the N-by-2 matrix A = ( X Y ).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   A11, A12, A22, C, SSMAX, TAU
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT
      EXTERNAL           DDOT
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DLARFG, DLAS2
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.1 ) THEN
         SSMIN = ZERO
         RETURN
      END IF
*
*     Compute the QR factorization of the N-by-2 matrix ( X Y )
*
      CALL DLARFG( N, X( 1 ), X( 1+INCX ), INCX, TAU )
      A11 = X( 1 )
      X( 1 ) = ONE
*
      C = -TAU*DDOT( N, X, INCX, Y, INCY )
      CALL DAXPY( N, C, X, INCX, Y, INCY )
*
      CALL DLARFG( N-1, Y( 1+INCY ), Y( 1+2*INCY ), INCY, TAU )
*
      A12 = Y( 1 )
      A22 = Y( 1+INCY )
*
*     Compute the SVD of 2-by-2 Upper triangular matrix.
*
      CALL DLAS2( A11, A12, A22, SSMIN, SSMAX )
*
      RETURN
*
*     End of DLAPLL
*
      END
      SUBROUTINE DLAPMT( FORWRD, M, N, X, LDX, K )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            FORWRD
      INTEGER            LDX, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            K( * )
      DOUBLE PRECISION   X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DLAPMT rearranges the columns of the M by N matrix X as specified
*  by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
*  If FORWRD = .TRUE.,  forward permutation:
*
*       X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
*
*  If FORWRD = .FALSE., backward permutation:
*
*       X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
*
*  Arguments
*  =========
*
*  FORWRD  (input) LOGICAL
*          = .TRUE., forward permutation
*          = .FALSE., backward permutation
*
*  M       (input) INTEGER
*          The number of rows of the matrix X. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix X. N >= 0.
*
*  X       (input/output) DOUBLE PRECISION array, dimension (LDX,N)
*          On entry, the M by N matrix X.
*          On exit, X contains the permuted matrix X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X, LDX >= MAX(1,M).
*
*  K       (input/output) INTEGER array, dimension (N)
*          On entry, K contains the permutation vector. K is used as
*          internal workspace, but reset to its original value on
*          output.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, II, IN, J
      DOUBLE PRECISION   TEMP
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.1 )
     $   RETURN
*
      DO 10 I = 1, N
         K( I ) = -K( I )
   10 CONTINUE
*
      IF( FORWRD ) THEN
*
*        Forward permutation
*
         DO 50 I = 1, N
*
            IF( K( I ).GT.0 )
     $         GO TO 40
*
            J = I
            K( J ) = -K( J )
            IN = K( J )
*
   20       CONTINUE
            IF( K( IN ).GT.0 )
     $         GO TO 40
*
            DO 30 II = 1, M
               TEMP = X( II, J )
               X( II, J ) = X( II, IN )
               X( II, IN ) = TEMP
   30       CONTINUE
*
            K( IN ) = -K( IN )
            J = IN
            IN = K( IN )
            GO TO 20
*
   40       CONTINUE
*
   50    CONTINUE
*
      ELSE
*
*        Backward permutation
*
         DO 90 I = 1, N
*
            IF( K( I ).GT.0 )
     $         GO TO 80
*
            K( I ) = -K( I )
            J = K( I )
   60       CONTINUE
            IF( J.EQ.I )
     $         GO TO 80
*
            DO 70 II = 1, M
               TEMP = X( II, I )
               X( II, I ) = X( II, J )
               X( II, J ) = TEMP
   70       CONTINUE
*
            K( J ) = -K( J )
            J = K( J )
            GO TO 60
*
   80       CONTINUE
*
   90    CONTINUE
*
      END IF
*
      RETURN
*
*     End of DLAPMT
*
      END
      DOUBLE PRECISION FUNCTION DLAPY2( X, Y )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   X, Y
*     ..
*
*  Purpose
*  =======
*
*  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
*  overflow.
*
*  Arguments
*  =========
*
*  X       (input) DOUBLE PRECISION
*  Y       (input) DOUBLE PRECISION
*          X and Y specify the values x and y.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   W, XABS, YABS, Z
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      IF( Z.EQ.ZERO ) THEN
         DLAPY2 = W
      ELSE
         DLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      END IF
      RETURN
*
*     End of DLAPY2
*
      END
      DOUBLE PRECISION FUNCTION DLAPY3( X, Y, Z )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   X, Y, Z
*     ..
*
*  Purpose
*  =======
*
*  DLAPY3 returns sqrt(x**2+y**2+z**2), taking care not to cause
*  unnecessary overflow.
*
*  Arguments
*  =========
*
*  X       (input) DOUBLE PRECISION
*  Y       (input) DOUBLE PRECISION
*  Z       (input) DOUBLE PRECISION
*          X, Y and Z specify the values x, y and z.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   W, XABS, YABS, ZABS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      XABS = ABS( X )
      YABS = ABS( Y )
      ZABS = ABS( Z )
      W = MAX( XABS, YABS, ZABS )
      IF( W.EQ.ZERO ) THEN
*     W can be zero for max(0,nan,0)
*     adding all three entries together will make sure
*     NaN will not disappear.
         DLAPY3 =  XABS + YABS + ZABS
      ELSE
         DLAPY3 = W*SQRT( ( XABS / W )**2+( YABS / W )**2+
     $            ( ZABS / W )**2 )
      END IF
      RETURN
*
*     End of DLAPY3
*
      END
      SUBROUTINE DLAQGB( M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,
     $                   AMAX, EQUED )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED
      INTEGER            KL, KU, LDAB, M, N
      DOUBLE PRECISION   AMAX, COLCND, ROWCND
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), C( * ), R( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAQGB equilibrates a general M by N band matrix A with KL
*  subdiagonals and KU superdiagonals using the row and scaling factors
*  in the vectors R and C.
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
*  KL      (input) INTEGER
*          The number of subdiagonals within the band of A.  KL >= 0.
*
*  KU      (input) INTEGER
*          The number of superdiagonals within the band of A.  KU >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
*          On entry, the matrix A in band storage, in rows 1 to KL+KU+1.
*          The j-th column of A is stored in the j-th column of the
*          array AB as follows:
*          AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)
*
*          On exit, the equilibrated matrix, in the same storage format
*          as A.  See EQUED for the form of the equilibrated matrix.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDA >= KL+KU+1.
*
*  R       (input) DOUBLE PRECISION array, dimension (M)
*          The row scale factors for A.
*
*  C       (input) DOUBLE PRECISION array, dimension (N)
*          The column scale factors for A.
*
*  ROWCND  (input) DOUBLE PRECISION
*          Ratio of the smallest R(i) to the largest R(i).
*
*  COLCND  (input) DOUBLE PRECISION
*          Ratio of the smallest C(i) to the largest C(i).
*
*  AMAX    (input) DOUBLE PRECISION
*          Absolute value of largest matrix entry.
*
*  EQUED   (output) CHARACTER*1
*          Specifies the form of equilibration that was done.
*          = 'N':  No equilibration
*          = 'R':  Row equilibration, i.e., A has been premultiplied by
*                  diag(R).
*          = 'C':  Column equilibration, i.e., A has been postmultiplied
*                  by diag(C).
*          = 'B':  Both row and column equilibration, i.e., A has been
*                  replaced by diag(R) * A * diag(C).
*
*  Internal Parameters
*  ===================
*
*  THRESH is a threshold value used to decide if row or column scaling
*  should be done based on the ratio of the row or column scaling
*  factors.  If ROWCND < THRESH, row scaling is done, and if
*  COLCND < THRESH, column scaling is done.
*
*  LARGE and SMALL are threshold values used to decide if row scaling
*  should be done based on the absolute size of the largest matrix
*  element.  If AMAX > LARGE or AMAX < SMALL, row scaling is done.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, THRESH
      PARAMETER          ( ONE = 1.0D+0, THRESH = 0.1D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   CJ, LARGE, SMALL
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 ) THEN
         EQUED = 'N'
         RETURN
      END IF
*
*     Initialize LARGE and SMALL.
*
      SMALL = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' )
      LARGE = ONE / SMALL
*
      IF( ROWCND.GE.THRESH .AND. AMAX.GE.SMALL .AND. AMAX.LE.LARGE )
     $     THEN
*
*        No row scaling
*
         IF( COLCND.GE.THRESH ) THEN
*
*           No column scaling
*
            EQUED = 'N'
         ELSE
*
*           Column scaling
*
            DO 20 J = 1, N
               CJ = C( J )
               DO 10 I = MAX( 1, J-KU ), MIN( M, J+KL )
                  AB( KU+1+I-J, J ) = CJ*AB( KU+1+I-J, J )
   10          CONTINUE
   20       CONTINUE
            EQUED = 'C'
         END IF
      ELSE IF( COLCND.GE.THRESH ) THEN
*
*        Row scaling, no column scaling
*
         DO 40 J = 1, N
            DO 30 I = MAX( 1, J-KU ), MIN( M, J+KL )
               AB( KU+1+I-J, J ) = R( I )*AB( KU+1+I-J, J )
   30       CONTINUE
   40    CONTINUE
         EQUED = 'R'
      ELSE
*
*        Row and column scaling
*
         DO 60 J = 1, N
            CJ = C( J )
            DO 50 I = MAX( 1, J-KU ), MIN( M, J+KL )
               AB( KU+1+I-J, J ) = CJ*R( I )*AB( KU+1+I-J, J )
   50       CONTINUE
   60    CONTINUE
         EQUED = 'B'
      END IF
*
      RETURN
*
*     End of DLAQGB
*
      END
      SUBROUTINE DLAQGE( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX,
     $                   EQUED )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED
      INTEGER            LDA, M, N
      DOUBLE PRECISION   AMAX, COLCND, ROWCND
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( * ), R( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAQGE equilibrates a general M by N matrix A using the row and
*  column scaling factors in the vectors R and C.
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M by N matrix A.
*          On exit, the equilibrated matrix.  See EQUED for the form of
*          the equilibrated matrix.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(M,1).
*
*  R       (input) DOUBLE PRECISION array, dimension (M)
*          The row scale factors for A.
*
*  C       (input) DOUBLE PRECISION array, dimension (N)
*          The column scale factors for A.
*
*  ROWCND  (input) DOUBLE PRECISION
*          Ratio of the smallest R(i) to the largest R(i).
*
*  COLCND  (input) DOUBLE PRECISION
*          Ratio of the smallest C(i) to the largest C(i).
*
*  AMAX    (input) DOUBLE PRECISION
*          Absolute value of largest matrix entry.
*
*  EQUED   (output) CHARACTER*1
*          Specifies the form of equilibration that was done.
*          = 'N':  No equilibration
*          = 'R':  Row equilibration, i.e., A has been premultiplied by
*                  diag(R).
*          = 'C':  Column equilibration, i.e., A has been postmultiplied
*                  by diag(C).
*          = 'B':  Both row and column equilibration, i.e., A has been
*                  replaced by diag(R) * A * diag(C).
*
*  Internal Parameters
*  ===================
*
*  THRESH is a threshold value used to decide if row or column scaling
*  should be done based on the ratio of the row or column scaling
*  factors.  If ROWCND < THRESH, row scaling is done, and if
*  COLCND < THRESH, column scaling is done.
*
*  LARGE and SMALL are threshold values used to decide if row scaling
*  should be done based on the absolute size of the largest matrix
*  element.  If AMAX > LARGE or AMAX < SMALL, row scaling is done.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, THRESH
      PARAMETER          ( ONE = 1.0D+0, THRESH = 0.1D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   CJ, LARGE, SMALL
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 ) THEN
         EQUED = 'N'
         RETURN
      END IF
*
*     Initialize LARGE and SMALL.
*
      SMALL = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' )
      LARGE = ONE / SMALL
*
      IF( ROWCND.GE.THRESH .AND. AMAX.GE.SMALL .AND. AMAX.LE.LARGE )
     $     THEN
*
*        No row scaling
*
         IF( COLCND.GE.THRESH ) THEN
*
*           No column scaling
*
            EQUED = 'N'
         ELSE
*
*           Column scaling
*
            DO 20 J = 1, N
               CJ = C( J )
               DO 10 I = 1, M
                  A( I, J ) = CJ*A( I, J )
   10          CONTINUE
   20       CONTINUE
            EQUED = 'C'
         END IF
      ELSE IF( COLCND.GE.THRESH ) THEN
*
*        Row scaling, no column scaling
*
         DO 40 J = 1, N
            DO 30 I = 1, M
               A( I, J ) = R( I )*A( I, J )
   30       CONTINUE
   40    CONTINUE
         EQUED = 'R'
      ELSE
*
*        Row and column scaling
*
         DO 60 J = 1, N
            CJ = C( J )
            DO 50 I = 1, M
               A( I, J ) = CJ*R( I )*A( I, J )
   50       CONTINUE
   60    CONTINUE
         EQUED = 'B'
      END IF
*
      RETURN
*
*     End of DLAQGE
*
      END
      SUBROUTINE DLAQP2( M, N, OFFSET, A, LDA, JPVT, TAU, VN1, VN2,
     $                   WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            LDA, M, N, OFFSET
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), VN1( * ), VN2( * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAQP2 computes a QR factorization with column pivoting of
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
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
*  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The scalar factors of the elementary reflectors.
*
*  VN1     (input/output) DOUBLE PRECISION array, dimension (N)
*          The vector with the partial column norms.
*
*  VN2     (input/output) DOUBLE PRECISION array, dimension (N)
*          The vector with the exact column norms.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*
*  Further Details
*  ===============
*
*  Based on contributions by
*    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*    X. Sun, Computer Science Dept., Duke University, USA
*
*  Partial column norm updating strategy modified by
*    Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
*    University of Zagreb, Croatia.
*    June 2006.
*  For more details see LAPACK Working Note 176.
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, ITEMP, J, MN, OFFPI, PVT
      DOUBLE PRECISION   AII, TEMP, TEMP2, TOL3Z
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARF, DLARFG, DSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DNRM2
      EXTERNAL           IDAMAX, DLAMCH, DNRM2
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M-OFFSET, N )
      TOL3Z = SQRT(DLAMCH('Epsilon'))
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
            CALL DSWAP( M, A( 1, PVT ), 1, A( 1, I ), 1 )
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
            CALL DLARFG( M-OFFPI+1, A( OFFPI, I ), A( OFFPI+1, I ), 1,
     $                   TAU( I ) )
         ELSE
            CALL DLARFG( 1, A( M, I ), A( M, I ), 1, TAU( I ) )
         END IF
*
         IF( I.LT.N ) THEN
*
*           Apply H(i)' to A(offset+i:m,i+1:n) from the left.
*
            AII = A( OFFPI, I )
            A( OFFPI, I ) = ONE
            CALL DLARF( 'Left', M-OFFPI+1, N-I, A( OFFPI, I ), 1,
     $                  TAU( I ), A( OFFPI, I+1 ), LDA, WORK( 1 ) )
            A( OFFPI, I ) = AII
         END IF
*
*        Update partial column norms.
*
         DO 10 J = I + 1, N
            IF( VN1( J ).NE.ZERO ) THEN
*
*              NOTE: The following 4 lines follow from the analysis in
*              Lapack Working Note 176.
*
               TEMP = ONE - ( ABS( A( OFFPI, J ) ) / VN1( J ) )**2
               TEMP = MAX( TEMP, ZERO )
               TEMP2 = TEMP*( VN1( J ) / VN2( J ) )**2
               IF( TEMP2 .LE. TOL3Z ) THEN
                  IF( OFFPI.LT.M ) THEN
                     VN1( J ) = DNRM2( M-OFFPI, A( OFFPI+1, J ), 1 )
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
*     End of DLAQP2
*
      END
      SUBROUTINE DLAQPS( M, N, OFFSET, NB, KB, A, LDA, JPVT, TAU, VN1,
     $                   VN2, AUXV, F, LDF )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            KB, LDA, LDF, M, N, NB, OFFSET
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), AUXV( * ), F( LDF, * ), TAU( * ),
     $                   VN1( * ), VN2( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAQPS computes a step of QR factorization with column pivoting
*  of a real M-by-N matrix A by using Blas-3.  It tries to factorize
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
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
*  TAU     (output) DOUBLE PRECISION array, dimension (KB)
*          The scalar factors of the elementary reflectors.
*
*  VN1     (input/output) DOUBLE PRECISION array, dimension (N)
*          The vector with the partial column norms.
*
*  VN2     (input/output) DOUBLE PRECISION array, dimension (N)
*          The vector with the exact column norms.
*
*  AUXV    (input/output) DOUBLE PRECISION array, dimension (NB)
*          Auxiliar vector.
*
*  F       (input/output) DOUBLE PRECISION array, dimension (LDF,NB)
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
*  Partial column norm updating strategy modified by
*    Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
*    University of Zagreb, Croatia.
*    June 2006.
*  For more details see LAPACK Working Note 176.
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            ITEMP, J, K, LASTRK, LSTICC, PVT, RK
      DOUBLE PRECISION   AKK, TEMP, TEMP2, TOL3Z
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DGEMV, DLARFG, DSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, NINT, SQRT
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH, DNRM2
      EXTERNAL           IDAMAX, DLAMCH, DNRM2
*     ..
*     .. Executable Statements ..
*
      LASTRK = MIN( M, N+OFFSET )
      LSTICC = 0
      K = 0
      TOL3Z = SQRT(DLAMCH('Epsilon'))
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
            CALL DSWAP( M, A( 1, PVT ), 1, A( 1, K ), 1 )
            CALL DSWAP( K-1, F( PVT, 1 ), LDF, F( K, 1 ), LDF )
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
            CALL DGEMV( 'No transpose', M-RK+1, K-1, -ONE, A( RK, 1 ),
     $                  LDA, F( K, 1 ), LDF, ONE, A( RK, K ), 1 )
         END IF
*
*        Generate elementary reflector H(k).
*
         IF( RK.LT.M ) THEN
            CALL DLARFG( M-RK+1, A( RK, K ), A( RK+1, K ), 1, TAU( K ) )
         ELSE
            CALL DLARFG( 1, A( RK, K ), A( RK, K ), 1, TAU( K ) )
         END IF
*
         AKK = A( RK, K )
         A( RK, K ) = ONE
*
*        Compute Kth column of F:
*
*        Compute  F(K+1:N,K) := tau(K)*A(RK:M,K+1:N)'*A(RK:M,K).
*
         IF( K.LT.N ) THEN
            CALL DGEMV( 'Transpose', M-RK+1, N-K, TAU( K ),
     $                  A( RK, K+1 ), LDA, A( RK, K ), 1, ZERO,
     $                  F( K+1, K ), 1 )
         END IF
*
*        Padding F(1:K,K) with zeros.
*
         DO 20 J = 1, K
            F( J, K ) = ZERO
   20    CONTINUE
*
*        Incremental updating of F:
*        F(1:N,K) := F(1:N,K) - tau(K)*F(1:N,1:K-1)*A(RK:M,1:K-1)'
*                    *A(RK:M,K).
*
         IF( K.GT.1 ) THEN
            CALL DGEMV( 'Transpose', M-RK+1, K-1, -TAU( K ), A( RK, 1 ),
     $                  LDA, A( RK, K ), 1, ZERO, AUXV( 1 ), 1 )
*
            CALL DGEMV( 'No transpose', N, K-1, ONE, F( 1, 1 ), LDF,
     $                  AUXV( 1 ), 1, ONE, F( 1, K ), 1 )
         END IF
*
*        Update the current row of A:
*        A(RK,K+1:N) := A(RK,K+1:N) - A(RK,1:K)*F(K+1:N,1:K)'.
*
         IF( K.LT.N ) THEN
            CALL DGEMV( 'No transpose', N-K, K, -ONE, F( K+1, 1 ), LDF,
     $                  A( RK, 1 ), LDA, ONE, A( RK, K+1 ), LDA )
         END IF
*
*        Update partial column norms.
*
         IF( RK.LT.LASTRK ) THEN
            DO 30 J = K + 1, N
               IF( VN1( J ).NE.ZERO ) THEN
*
*                 NOTE: The following 4 lines follow from the analysis in
*                 Lapack Working Note 176.
*
                  TEMP = ABS( A( RK, J ) ) / VN1( J )
                  TEMP = MAX( ZERO, ( ONE+TEMP )*( ONE-TEMP ) )
                  TEMP2 = TEMP*( VN1( J ) / VN2( J ) )**2
                  IF( TEMP2 .LE. TOL3Z ) THEN
                     VN2( J ) = DBLE( LSTICC )
                     LSTICC = J
                  ELSE
                     VN1( J ) = VN1( J )*SQRT( TEMP )
                  END IF
               END IF
   30       CONTINUE
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
         CALL DGEMM( 'No transpose', 'Transpose', M-RK, N-KB, KB, -ONE,
     $               A( RK+1, 1 ), LDA, F( KB+1, 1 ), LDF, ONE,
     $               A( RK+1, KB+1 ), LDA )
      END IF
*
*     Recomputation of difficult columns.
*
   40 CONTINUE
      IF( LSTICC.GT.0 ) THEN
         ITEMP = NINT( VN2( LSTICC ) )
         VN1( LSTICC ) = DNRM2( M-RK, A( RK+1, LSTICC ), 1 )
*
*        NOTE: The computation of VN1( LSTICC ) relies on the fact that 
*        SNRM2 does not fail on vectors with norm below the value of
*        SQRT(DLAMCH('S')) 
*
         VN2( LSTICC ) = VN1( LSTICC )
         LSTICC = ITEMP
         GO TO 40
      END IF
*
      RETURN
*
*     End of DLAQPS
*
      END
      SUBROUTINE DLAQSB( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, EQUED )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED, UPLO
      INTEGER            KD, LDAB, N
      DOUBLE PRECISION   AMAX, SCOND
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), S( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAQSB equilibrates a symmetric band matrix A using the scaling
*  factors in the vector S.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is stored.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  KD      (input) INTEGER
*          The number of super-diagonals of the matrix A if UPLO = 'U',
*          or the number of sub-diagonals if UPLO = 'L'.  KD >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first KD+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*
*          On exit, if INFO = 0, the triangular factor U or L from the
*          Cholesky factorization A = U'*U or A = L*L' of the band
*          matrix A, in the same storage format as A.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD+1.
*
*  S       (input) DOUBLE PRECISION array, dimension (N)
*          The scale factors for A.
*
*  SCOND   (input) DOUBLE PRECISION
*          Ratio of the smallest S(i) to the largest S(i).
*
*  AMAX    (input) DOUBLE PRECISION
*          Absolute value of largest matrix entry.
*
*  EQUED   (output) CHARACTER*1
*          Specifies whether or not equilibration was done.
*          = 'N':  No equilibration.
*          = 'Y':  Equilibration was done, i.e., A has been replaced by
*                  diag(S) * A * diag(S).
*
*  Internal Parameters
*  ===================
*
*  THRESH is a threshold value used to decide if scaling should be done
*  based on the ratio of the scaling factors.  If SCOND < THRESH,
*  scaling is done.
*
*  LARGE and SMALL are threshold values used to decide if scaling should
*  be done based on the absolute size of the largest matrix element.
*  If AMAX > LARGE or AMAX < SMALL, scaling is done.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, THRESH
      PARAMETER          ( ONE = 1.0D+0, THRESH = 0.1D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   CJ, LARGE, SMALL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         EQUED = 'N'
         RETURN
      END IF
*
*     Initialize LARGE and SMALL.
*
      SMALL = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' )
      LARGE = ONE / SMALL
*
      IF( SCOND.GE.THRESH .AND. AMAX.GE.SMALL .AND. AMAX.LE.LARGE ) THEN
*
*        No equilibration
*
         EQUED = 'N'
      ELSE
*
*        Replace A by diag(S) * A * diag(S).
*
         IF( LSAME( UPLO, 'U' ) ) THEN
*
*           Upper triangle of A is stored in band format.
*
            DO 20 J = 1, N
               CJ = S( J )
               DO 10 I = MAX( 1, J-KD ), J
                  AB( KD+1+I-J, J ) = CJ*S( I )*AB( KD+1+I-J, J )
   10          CONTINUE
   20       CONTINUE
         ELSE
*
*           Lower triangle of A is stored.
*
            DO 40 J = 1, N
               CJ = S( J )
               DO 30 I = J, MIN( N, J+KD )
                  AB( 1+I-J, J ) = CJ*S( I )*AB( 1+I-J, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         EQUED = 'Y'
      END IF
*
      RETURN
*
*     End of DLAQSB
*
      END
      SUBROUTINE DLAQSP( UPLO, N, AP, S, SCOND, AMAX, EQUED )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED, UPLO
      INTEGER            N
      DOUBLE PRECISION   AMAX, SCOND
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AP( * ), S( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAQSP equilibrates a symmetric matrix A using the scaling factors
*  in the vector S.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is stored.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*
*          On exit, the equilibrated matrix:  diag(S) * A * diag(S), in
*          the same storage format as A.
*
*  S       (input) DOUBLE PRECISION array, dimension (N)
*          The scale factors for A.
*
*  SCOND   (input) DOUBLE PRECISION
*          Ratio of the smallest S(i) to the largest S(i).
*
*  AMAX    (input) DOUBLE PRECISION
*          Absolute value of largest matrix entry.
*
*  EQUED   (output) CHARACTER*1
*          Specifies whether or not equilibration was done.
*          = 'N':  No equilibration.
*          = 'Y':  Equilibration was done, i.e., A has been replaced by
*                  diag(S) * A * diag(S).
*
*  Internal Parameters
*  ===================
*
*  THRESH is a threshold value used to decide if scaling should be done
*  based on the ratio of the scaling factors.  If SCOND < THRESH,
*  scaling is done.
*
*  LARGE and SMALL are threshold values used to decide if scaling should
*  be done based on the absolute size of the largest matrix element.
*  If AMAX > LARGE or AMAX < SMALL, scaling is done.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, THRESH
      PARAMETER          ( ONE = 1.0D+0, THRESH = 0.1D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, JC
      DOUBLE PRECISION   CJ, LARGE, SMALL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         EQUED = 'N'
         RETURN
      END IF
*
*     Initialize LARGE and SMALL.
*
      SMALL = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' )
      LARGE = ONE / SMALL
*
      IF( SCOND.GE.THRESH .AND. AMAX.GE.SMALL .AND. AMAX.LE.LARGE ) THEN
*
*        No equilibration
*
         EQUED = 'N'
      ELSE
*
*        Replace A by diag(S) * A * diag(S).
*
         IF( LSAME( UPLO, 'U' ) ) THEN
*
*           Upper triangle of A is stored.
*
            JC = 1
            DO 20 J = 1, N
               CJ = S( J )
               DO 10 I = 1, J
                  AP( JC+I-1 ) = CJ*S( I )*AP( JC+I-1 )
   10          CONTINUE
               JC = JC + J
   20       CONTINUE
         ELSE
*
*           Lower triangle of A is stored.
*
            JC = 1
            DO 40 J = 1, N
               CJ = S( J )
               DO 30 I = J, N
                  AP( JC+I-J ) = CJ*S( I )*AP( JC+I-J )
   30          CONTINUE
               JC = JC + N - J + 1
   40       CONTINUE
         END IF
         EQUED = 'Y'
      END IF
*
      RETURN
*
*     End of DLAQSP
*
      END
      SUBROUTINE DLAQSY( UPLO, N, A, LDA, S, SCOND, AMAX, EQUED )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED, UPLO
      INTEGER            LDA, N
      DOUBLE PRECISION   AMAX, SCOND
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), S( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAQSY equilibrates a symmetric matrix A using the scaling factors
*  in the vector S.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is stored.
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
*          n by n upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading n by n lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*
*          On exit, if EQUED = 'Y', the equilibrated matrix:
*          diag(S) * A * diag(S).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(N,1).
*
*  S       (input) DOUBLE PRECISION array, dimension (N)
*          The scale factors for A.
*
*  SCOND   (input) DOUBLE PRECISION
*          Ratio of the smallest S(i) to the largest S(i).
*
*  AMAX    (input) DOUBLE PRECISION
*          Absolute value of largest matrix entry.
*
*  EQUED   (output) CHARACTER*1
*          Specifies whether or not equilibration was done.
*          = 'N':  No equilibration.
*          = 'Y':  Equilibration was done, i.e., A has been replaced by
*                  diag(S) * A * diag(S).
*
*  Internal Parameters
*  ===================
*
*  THRESH is a threshold value used to decide if scaling should be done
*  based on the ratio of the scaling factors.  If SCOND < THRESH,
*  scaling is done.
*
*  LARGE and SMALL are threshold values used to decide if scaling should
*  be done based on the absolute size of the largest matrix element.
*  If AMAX > LARGE or AMAX < SMALL, scaling is done.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, THRESH
      PARAMETER          ( ONE = 1.0D+0, THRESH = 0.1D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   CJ, LARGE, SMALL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.0 ) THEN
         EQUED = 'N'
         RETURN
      END IF
*
*     Initialize LARGE and SMALL.
*
      SMALL = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' )
      LARGE = ONE / SMALL
*
      IF( SCOND.GE.THRESH .AND. AMAX.GE.SMALL .AND. AMAX.LE.LARGE ) THEN
*
*        No equilibration
*
         EQUED = 'N'
      ELSE
*
*        Replace A by diag(S) * A * diag(S).
*
         IF( LSAME( UPLO, 'U' ) ) THEN
*
*           Upper triangle of A is stored.
*
            DO 20 J = 1, N
               CJ = S( J )
               DO 10 I = 1, J
                  A( I, J ) = CJ*S( I )*A( I, J )
   10          CONTINUE
   20       CONTINUE
         ELSE
*
*           Lower triangle of A is stored.
*
            DO 40 J = 1, N
               CJ = S( J )
               DO 30 I = J, N
                  A( I, J ) = CJ*S( I )*A( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         EQUED = 'Y'
      END IF
*
      RETURN
*
*     End of DLAQSY
*
      END
      SUBROUTINE DLAQTR( LTRAN, LREAL, N, T, LDT, B, W, SCALE, X, WORK,
     $                   INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            LREAL, LTRAN
      INTEGER            INFO, LDT, N
      DOUBLE PRECISION   SCALE, W
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   B( * ), T( LDT, * ), WORK( * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAQTR solves the real quasi-triangular system
*
*               op(T)*p = scale*c,               if LREAL = .TRUE.
*
*  or the complex quasi-triangular systems
*
*             op(T + iB)*(p+iq) = scale*(c+id),  if LREAL = .FALSE.
*
*  in real arithmetic, where T is upper quasi-triangular.
*  If LREAL = .FALSE., then the first diagonal block of T must be
*  1 by 1, B is the specially structured matrix
*
*                 B = [ b(1) b(2) ... b(n) ]
*                     [       w            ]
*                     [           w        ]
*                     [              .     ]
*                     [                 w  ]
*
*  op(A) = A or A', A' denotes the conjugate transpose of
*  matrix A.
*
*  On input, X = [ c ].  On output, X = [ p ].
*                [ d ]                  [ q ]
*
*  This subroutine is designed for the condition number estimation
*  in routine DTRSNA.
*
*  Arguments
*  =========
*
*  LTRAN   (input) LOGICAL
*          On entry, LTRAN specifies the option of conjugate transpose:
*             = .FALSE.,    op(T+i*B) = T+i*B,
*             = .TRUE.,     op(T+i*B) = (T+i*B)'.
*
*  LREAL   (input) LOGICAL
*          On entry, LREAL specifies the input matrix structure:
*             = .FALSE.,    the input is complex
*             = .TRUE.,     the input is real
*
*  N       (input) INTEGER
*          On entry, N specifies the order of T+i*B. N >= 0.
*
*  T       (input) DOUBLE PRECISION array, dimension (LDT,N)
*          On entry, T contains a matrix in Schur canonical form.
*          If LREAL = .FALSE., then the first diagonal block of T mu
*          be 1 by 1.
*
*  LDT     (input) INTEGER
*          The leading dimension of the matrix T. LDT >= max(1,N).
*
*  B       (input) DOUBLE PRECISION array, dimension (N)
*          On entry, B contains the elements to form the matrix
*          B as described above.
*          If LREAL = .TRUE., B is not referenced.
*
*  W       (input) DOUBLE PRECISION
*          On entry, W is the diagonal element of the matrix B.
*          If LREAL = .TRUE., W is not referenced.
*
*  SCALE   (output) DOUBLE PRECISION
*          On exit, SCALE is the scale factor.
*
*  X       (input/output) DOUBLE PRECISION array, dimension (2*N)
*          On entry, X contains the right hand side of the system.
*          On exit, X is overwritten by the solution.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
*
*  INFO    (output) INTEGER
*          On exit, INFO is set to
*             0: successful exit.
*               1: the some diagonal 1 by 1 block has been perturbed by
*                  a small number SMIN to keep nonsingularity.
*               2: the some diagonal 2 by 2 block has been perturbed by
*                  a small number in DLALN2 to keep nonsingularity.
*          NOTE: In the interests of speed, this routine does not
*                check the inputs for errors.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN
      INTEGER            I, IERR, J, J1, J2, JNEXT, K, N1, N2
      DOUBLE PRECISION   BIGNUM, EPS, REC, SCALOC, SI, SMIN, SMINW,
     $                   SMLNUM, SR, TJJ, TMP, XJ, XMAX, XNORM, Z
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   D( 2, 2 ), V( 2, 2 )
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM, DDOT, DLAMCH, DLANGE
      EXTERNAL           IDAMAX, DASUM, DDOT, DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DLADIV, DLALN2, DSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Do not test the input parameters for errors
*
      NOTRAN = .NOT.LTRAN
      INFO = 0
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Set constants to control overflow
*
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      BIGNUM = ONE / SMLNUM
*
      XNORM = DLANGE( 'M', N, N, T, LDT, D )
      IF( .NOT.LREAL )
     $   XNORM = MAX( XNORM, ABS( W ), DLANGE( 'M', N, 1, B, N, D ) )
      SMIN = MAX( SMLNUM, EPS*XNORM )
*
*     Compute 1-norm of each column of strictly upper triangular
*     part of T to control overflow in triangular solver.
*
      WORK( 1 ) = ZERO
      DO 10 J = 2, N
         WORK( J ) = DASUM( J-1, T( 1, J ), 1 )
   10 CONTINUE
*
      IF( .NOT.LREAL ) THEN
         DO 20 I = 2, N
            WORK( I ) = WORK( I ) + ABS( B( I ) )
   20    CONTINUE
      END IF
*
      N2 = 2*N
      N1 = N
      IF( .NOT.LREAL )
     $   N1 = N2
      K = IDAMAX( N1, X, 1 )
      XMAX = ABS( X( K ) )
      SCALE = ONE
*
      IF( XMAX.GT.BIGNUM ) THEN
         SCALE = BIGNUM / XMAX
         CALL DSCAL( N1, SCALE, X, 1 )
         XMAX = BIGNUM
      END IF
*
      IF( LREAL ) THEN
*
         IF( NOTRAN ) THEN
*
*           Solve T*p = scale*c
*
            JNEXT = N
            DO 30 J = N, 1, -1
               IF( J.GT.JNEXT )
     $            GO TO 30
               J1 = J
               J2 = J
               JNEXT = J - 1
               IF( J.GT.1 ) THEN
                  IF( T( J, J-1 ).NE.ZERO ) THEN
                     J1 = J - 1
                     JNEXT = J - 2
                  END IF
               END IF
*
               IF( J1.EQ.J2 ) THEN
*
*                 Meet 1 by 1 diagonal block
*
*                 Scale to avoid overflow when computing
*                     x(j) = b(j)/T(j,j)
*
                  XJ = ABS( X( J1 ) )
                  TJJ = ABS( T( J1, J1 ) )
                  TMP = T( J1, J1 )
                  IF( TJJ.LT.SMIN ) THEN
                     TMP = SMIN
                     TJJ = SMIN
                     INFO = 1
                  END IF
*
                  IF( XJ.EQ.ZERO )
     $               GO TO 30
*
                  IF( TJJ.LT.ONE ) THEN
                     IF( XJ.GT.BIGNUM*TJJ ) THEN
                        REC = ONE / XJ
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  X( J1 ) = X( J1 ) / TMP
                  XJ = ABS( X( J1 ) )
*
*                 Scale x if necessary to avoid overflow when adding a
*                 multiple of column j1 of T.
*
                  IF( XJ.GT.ONE ) THEN
                     REC = ONE / XJ
                     IF( WORK( J1 ).GT.( BIGNUM-XMAX )*REC ) THEN
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                     END IF
                  END IF
                  IF( J1.GT.1 ) THEN
                     CALL DAXPY( J1-1, -X( J1 ), T( 1, J1 ), 1, X, 1 )
                     K = IDAMAX( J1-1, X, 1 )
                     XMAX = ABS( X( K ) )
                  END IF
*
               ELSE
*
*                 Meet 2 by 2 diagonal block
*
*                 Call 2 by 2 linear system solve, to take
*                 care of possible overflow by scaling factor.
*
                  D( 1, 1 ) = X( J1 )
                  D( 2, 1 ) = X( J2 )
                  CALL DLALN2( .FALSE., 2, 1, SMIN, ONE, T( J1, J1 ),
     $                         LDT, ONE, ONE, D, 2, ZERO, ZERO, V, 2,
     $                         SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     $               INFO = 2
*
                  IF( SCALOC.NE.ONE ) THEN
                     CALL DSCAL( N, SCALOC, X, 1 )
                     SCALE = SCALE*SCALOC
                  END IF
                  X( J1 ) = V( 1, 1 )
                  X( J2 ) = V( 2, 1 )
*
*                 Scale V(1,1) (= X(J1)) and/or V(2,1) (=X(J2))
*                 to avoid overflow in updating right-hand side.
*
                  XJ = MAX( ABS( V( 1, 1 ) ), ABS( V( 2, 1 ) ) )
                  IF( XJ.GT.ONE ) THEN
                     REC = ONE / XJ
                     IF( MAX( WORK( J1 ), WORK( J2 ) ).GT.
     $                   ( BIGNUM-XMAX )*REC ) THEN
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                     END IF
                  END IF
*
*                 Update right-hand side
*
                  IF( J1.GT.1 ) THEN
                     CALL DAXPY( J1-1, -X( J1 ), T( 1, J1 ), 1, X, 1 )
                     CALL DAXPY( J1-1, -X( J2 ), T( 1, J2 ), 1, X, 1 )
                     K = IDAMAX( J1-1, X, 1 )
                     XMAX = ABS( X( K ) )
                  END IF
*
               END IF
*
   30       CONTINUE
*
         ELSE
*
*           Solve T'*p = scale*c
*
            JNEXT = 1
            DO 40 J = 1, N
               IF( J.LT.JNEXT )
     $            GO TO 40
               J1 = J
               J2 = J
               JNEXT = J + 1
               IF( J.LT.N ) THEN
                  IF( T( J+1, J ).NE.ZERO ) THEN
                     J2 = J + 1
                     JNEXT = J + 2
                  END IF
               END IF
*
               IF( J1.EQ.J2 ) THEN
*
*                 1 by 1 diagonal block
*
*                 Scale if necessary to avoid overflow in forming the
*                 right-hand side element by inner product.
*
                  XJ = ABS( X( J1 ) )
                  IF( XMAX.GT.ONE ) THEN
                     REC = ONE / XMAX
                     IF( WORK( J1 ).GT.( BIGNUM-XJ )*REC ) THEN
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
*
                  X( J1 ) = X( J1 ) - DDOT( J1-1, T( 1, J1 ), 1, X, 1 )
*
                  XJ = ABS( X( J1 ) )
                  TJJ = ABS( T( J1, J1 ) )
                  TMP = T( J1, J1 )
                  IF( TJJ.LT.SMIN ) THEN
                     TMP = SMIN
                     TJJ = SMIN
                     INFO = 1
                  END IF
*
                  IF( TJJ.LT.ONE ) THEN
                     IF( XJ.GT.BIGNUM*TJJ ) THEN
                        REC = ONE / XJ
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  X( J1 ) = X( J1 ) / TMP
                  XMAX = MAX( XMAX, ABS( X( J1 ) ) )
*
               ELSE
*
*                 2 by 2 diagonal block
*
*                 Scale if necessary to avoid overflow in forming the
*                 right-hand side elements by inner product.
*
                  XJ = MAX( ABS( X( J1 ) ), ABS( X( J2 ) ) )
                  IF( XMAX.GT.ONE ) THEN
                     REC = ONE / XMAX
                     IF( MAX( WORK( J2 ), WORK( J1 ) ).GT.( BIGNUM-XJ )*
     $                   REC ) THEN
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
*
                  D( 1, 1 ) = X( J1 ) - DDOT( J1-1, T( 1, J1 ), 1, X,
     $                        1 )
                  D( 2, 1 ) = X( J2 ) - DDOT( J1-1, T( 1, J2 ), 1, X,
     $                        1 )
*
                  CALL DLALN2( .TRUE., 2, 1, SMIN, ONE, T( J1, J1 ),
     $                         LDT, ONE, ONE, D, 2, ZERO, ZERO, V, 2,
     $                         SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     $               INFO = 2
*
                  IF( SCALOC.NE.ONE ) THEN
                     CALL DSCAL( N, SCALOC, X, 1 )
                     SCALE = SCALE*SCALOC
                  END IF
                  X( J1 ) = V( 1, 1 )
                  X( J2 ) = V( 2, 1 )
                  XMAX = MAX( ABS( X( J1 ) ), ABS( X( J2 ) ), XMAX )
*
               END IF
   40       CONTINUE
         END IF
*
      ELSE
*
         SMINW = MAX( EPS*ABS( W ), SMIN )
         IF( NOTRAN ) THEN
*
*           Solve (T + iB)*(p+iq) = c+id
*
            JNEXT = N
            DO 70 J = N, 1, -1
               IF( J.GT.JNEXT )
     $            GO TO 70
               J1 = J
               J2 = J
               JNEXT = J - 1
               IF( J.GT.1 ) THEN
                  IF( T( J, J-1 ).NE.ZERO ) THEN
                     J1 = J - 1
                     JNEXT = J - 2
                  END IF
               END IF
*
               IF( J1.EQ.J2 ) THEN
*
*                 1 by 1 diagonal block
*
*                 Scale if necessary to avoid overflow in division
*
                  Z = W
                  IF( J1.EQ.1 )
     $               Z = B( 1 )
                  XJ = ABS( X( J1 ) ) + ABS( X( N+J1 ) )
                  TJJ = ABS( T( J1, J1 ) ) + ABS( Z )
                  TMP = T( J1, J1 )
                  IF( TJJ.LT.SMINW ) THEN
                     TMP = SMINW
                     TJJ = SMINW
                     INFO = 1
                  END IF
*
                  IF( XJ.EQ.ZERO )
     $               GO TO 70
*
                  IF( TJJ.LT.ONE ) THEN
                     IF( XJ.GT.BIGNUM*TJJ ) THEN
                        REC = ONE / XJ
                        CALL DSCAL( N2, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  CALL DLADIV( X( J1 ), X( N+J1 ), TMP, Z, SR, SI )
                  X( J1 ) = SR
                  X( N+J1 ) = SI
                  XJ = ABS( X( J1 ) ) + ABS( X( N+J1 ) )
*
*                 Scale x if necessary to avoid overflow when adding a
*                 multiple of column j1 of T.
*
                  IF( XJ.GT.ONE ) THEN
                     REC = ONE / XJ
                     IF( WORK( J1 ).GT.( BIGNUM-XMAX )*REC ) THEN
                        CALL DSCAL( N2, REC, X, 1 )
                        SCALE = SCALE*REC
                     END IF
                  END IF
*
                  IF( J1.GT.1 ) THEN
                     CALL DAXPY( J1-1, -X( J1 ), T( 1, J1 ), 1, X, 1 )
                     CALL DAXPY( J1-1, -X( N+J1 ), T( 1, J1 ), 1,
     $                           X( N+1 ), 1 )
*
                     X( 1 ) = X( 1 ) + B( J1 )*X( N+J1 )
                     X( N+1 ) = X( N+1 ) - B( J1 )*X( J1 )
*
                     XMAX = ZERO
                     DO 50 K = 1, J1 - 1
                        XMAX = MAX( XMAX, ABS( X( K ) )+
     $                         ABS( X( K+N ) ) )
   50                CONTINUE
                  END IF
*
               ELSE
*
*                 Meet 2 by 2 diagonal block
*
                  D( 1, 1 ) = X( J1 )
                  D( 2, 1 ) = X( J2 )
                  D( 1, 2 ) = X( N+J1 )
                  D( 2, 2 ) = X( N+J2 )
                  CALL DLALN2( .FALSE., 2, 2, SMINW, ONE, T( J1, J1 ),
     $                         LDT, ONE, ONE, D, 2, ZERO, -W, V, 2,
     $                         SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     $               INFO = 2
*
                  IF( SCALOC.NE.ONE ) THEN
                     CALL DSCAL( 2*N, SCALOC, X, 1 )
                     SCALE = SCALOC*SCALE
                  END IF
                  X( J1 ) = V( 1, 1 )
                  X( J2 ) = V( 2, 1 )
                  X( N+J1 ) = V( 1, 2 )
                  X( N+J2 ) = V( 2, 2 )
*
*                 Scale X(J1), .... to avoid overflow in
*                 updating right hand side.
*
                  XJ = MAX( ABS( V( 1, 1 ) )+ABS( V( 1, 2 ) ),
     $                 ABS( V( 2, 1 ) )+ABS( V( 2, 2 ) ) )
                  IF( XJ.GT.ONE ) THEN
                     REC = ONE / XJ
                     IF( MAX( WORK( J1 ), WORK( J2 ) ).GT.
     $                   ( BIGNUM-XMAX )*REC ) THEN
                        CALL DSCAL( N2, REC, X, 1 )
                        SCALE = SCALE*REC
                     END IF
                  END IF
*
*                 Update the right-hand side.
*
                  IF( J1.GT.1 ) THEN
                     CALL DAXPY( J1-1, -X( J1 ), T( 1, J1 ), 1, X, 1 )
                     CALL DAXPY( J1-1, -X( J2 ), T( 1, J2 ), 1, X, 1 )
*
                     CALL DAXPY( J1-1, -X( N+J1 ), T( 1, J1 ), 1,
     $                           X( N+1 ), 1 )
                     CALL DAXPY( J1-1, -X( N+J2 ), T( 1, J2 ), 1,
     $                           X( N+1 ), 1 )
*
                     X( 1 ) = X( 1 ) + B( J1 )*X( N+J1 ) +
     $                        B( J2 )*X( N+J2 )
                     X( N+1 ) = X( N+1 ) - B( J1 )*X( J1 ) -
     $                          B( J2 )*X( J2 )
*
                     XMAX = ZERO
                     DO 60 K = 1, J1 - 1
                        XMAX = MAX( ABS( X( K ) )+ABS( X( K+N ) ),
     $                         XMAX )
   60                CONTINUE
                  END IF
*
               END IF
   70       CONTINUE
*
         ELSE
*
*           Solve (T + iB)'*(p+iq) = c+id
*
            JNEXT = 1
            DO 80 J = 1, N
               IF( J.LT.JNEXT )
     $            GO TO 80
               J1 = J
               J2 = J
               JNEXT = J + 1
               IF( J.LT.N ) THEN
                  IF( T( J+1, J ).NE.ZERO ) THEN
                     J2 = J + 1
                     JNEXT = J + 2
                  END IF
               END IF
*
               IF( J1.EQ.J2 ) THEN
*
*                 1 by 1 diagonal block
*
*                 Scale if necessary to avoid overflow in forming the
*                 right-hand side element by inner product.
*
                  XJ = ABS( X( J1 ) ) + ABS( X( J1+N ) )
                  IF( XMAX.GT.ONE ) THEN
                     REC = ONE / XMAX
                     IF( WORK( J1 ).GT.( BIGNUM-XJ )*REC ) THEN
                        CALL DSCAL( N2, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
*
                  X( J1 ) = X( J1 ) - DDOT( J1-1, T( 1, J1 ), 1, X, 1 )
                  X( N+J1 ) = X( N+J1 ) - DDOT( J1-1, T( 1, J1 ), 1,
     $                        X( N+1 ), 1 )
                  IF( J1.GT.1 ) THEN
                     X( J1 ) = X( J1 ) - B( J1 )*X( N+1 )
                     X( N+J1 ) = X( N+J1 ) + B( J1 )*X( 1 )
                  END IF
                  XJ = ABS( X( J1 ) ) + ABS( X( J1+N ) )
*
                  Z = W
                  IF( J1.EQ.1 )
     $               Z = B( 1 )
*
*                 Scale if necessary to avoid overflow in
*                 complex division
*
                  TJJ = ABS( T( J1, J1 ) ) + ABS( Z )
                  TMP = T( J1, J1 )
                  IF( TJJ.LT.SMINW ) THEN
                     TMP = SMINW
                     TJJ = SMINW
                     INFO = 1
                  END IF
*
                  IF( TJJ.LT.ONE ) THEN
                     IF( XJ.GT.BIGNUM*TJJ ) THEN
                        REC = ONE / XJ
                        CALL DSCAL( N2, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  CALL DLADIV( X( J1 ), X( N+J1 ), TMP, -Z, SR, SI )
                  X( J1 ) = SR
                  X( J1+N ) = SI
                  XMAX = MAX( ABS( X( J1 ) )+ABS( X( J1+N ) ), XMAX )
*
               ELSE
*
*                 2 by 2 diagonal block
*
*                 Scale if necessary to avoid overflow in forming the
*                 right-hand side element by inner product.
*
                  XJ = MAX( ABS( X( J1 ) )+ABS( X( N+J1 ) ),
     $                 ABS( X( J2 ) )+ABS( X( N+J2 ) ) )
                  IF( XMAX.GT.ONE ) THEN
                     REC = ONE / XMAX
                     IF( MAX( WORK( J1 ), WORK( J2 ) ).GT.
     $                   ( BIGNUM-XJ ) / XMAX ) THEN
                        CALL DSCAL( N2, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
*
                  D( 1, 1 ) = X( J1 ) - DDOT( J1-1, T( 1, J1 ), 1, X,
     $                        1 )
                  D( 2, 1 ) = X( J2 ) - DDOT( J1-1, T( 1, J2 ), 1, X,
     $                        1 )
                  D( 1, 2 ) = X( N+J1 ) - DDOT( J1-1, T( 1, J1 ), 1,
     $                        X( N+1 ), 1 )
                  D( 2, 2 ) = X( N+J2 ) - DDOT( J1-1, T( 1, J2 ), 1,
     $                        X( N+1 ), 1 )
                  D( 1, 1 ) = D( 1, 1 ) - B( J1 )*X( N+1 )
                  D( 2, 1 ) = D( 2, 1 ) - B( J2 )*X( N+1 )
                  D( 1, 2 ) = D( 1, 2 ) + B( J1 )*X( 1 )
                  D( 2, 2 ) = D( 2, 2 ) + B( J2 )*X( 1 )
*
                  CALL DLALN2( .TRUE., 2, 2, SMINW, ONE, T( J1, J1 ),
     $                         LDT, ONE, ONE, D, 2, ZERO, W, V, 2,
     $                         SCALOC, XNORM, IERR )
                  IF( IERR.NE.0 )
     $               INFO = 2
*
                  IF( SCALOC.NE.ONE ) THEN
                     CALL DSCAL( N2, SCALOC, X, 1 )
                     SCALE = SCALOC*SCALE
                  END IF
                  X( J1 ) = V( 1, 1 )
                  X( J2 ) = V( 2, 1 )
                  X( N+J1 ) = V( 1, 2 )
                  X( N+J2 ) = V( 2, 2 )
                  XMAX = MAX( ABS( X( J1 ) )+ABS( X( N+J1 ) ),
     $                   ABS( X( J2 ) )+ABS( X( N+J2 ) ), XMAX )
*
               END IF
*
   80       CONTINUE
*
         END IF
*
      END IF
*
      RETURN
*
*     End of DLAQTR
*
      END
      SUBROUTINE DLAR1V( N, B1, BN, LAMBDA, D, L, LD, LLD,
     $           PIVMIN, GAPTOL, Z, WANTNC, NEGCNT, ZTZ, MINGMA,
     $           R, ISUPPZ, NRMINV, RESID, RQCORR, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            WANTNC
      INTEGER   B1, BN, N, NEGCNT, R
      DOUBLE PRECISION   GAPTOL, LAMBDA, MINGMA, NRMINV, PIVMIN, RESID,
     $                   RQCORR, ZTZ
*     ..
*     .. Array Arguments ..
      INTEGER            ISUPPZ( * )
      DOUBLE PRECISION   D( * ), L( * ), LD( * ), LLD( * ),
     $                  WORK( * )
      DOUBLE PRECISION Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAR1V computes the (scaled) r-th column of the inverse of
*  the sumbmatrix in rows B1 through BN of the tridiagonal matrix
*  L D L^T - sigma I. When sigma is close to an eigenvalue, the
*  computed vector is an accurate eigenvector. Usually, r corresponds
*  to the index where the eigenvector is largest in magnitude.
*  The following steps accomplish this computation :
*  (a) Stationary qd transform,  L D L^T - sigma I = L(+) D(+) L(+)^T,
*  (b) Progressive qd transform, L D L^T - sigma I = U(-) D(-) U(-)^T,
*  (c) Computation of the diagonal elements of the inverse of
*      L D L^T - sigma I by combining the above transforms, and choosing
*      r as the index where the diagonal of the inverse is (one of the)
*      largest in magnitude.
*  (d) Computation of the (scaled) r-th column of the inverse using the
*      twisted factorization obtained by combining the top part of the
*      the stationary and the bottom part of the progressive transform.
*
*  Arguments
*  =========
*
*  N        (input) INTEGER
*           The order of the matrix L D L^T.
*
*  B1       (input) INTEGER
*           First index of the submatrix of L D L^T.
*
*  BN       (input) INTEGER
*           Last index of the submatrix of L D L^T.
*
*  LAMBDA    (input) DOUBLE PRECISION
*           The shift. In order to compute an accurate eigenvector,
*           LAMBDA should be a good approximation to an eigenvalue
*           of L D L^T.
*
*  L        (input) DOUBLE PRECISION array, dimension (N-1)
*           The (n-1) subdiagonal elements of the unit bidiagonal matrix
*           L, in elements 1 to N-1.
*
*  D        (input) DOUBLE PRECISION array, dimension (N)
*           The n diagonal elements of the diagonal matrix D.
*
*  LD       (input) DOUBLE PRECISION array, dimension (N-1)
*           The n-1 elements L(i)*D(i).
*
*  LLD      (input) DOUBLE PRECISION array, dimension (N-1)
*           The n-1 elements L(i)*L(i)*D(i).
*
*  PIVMIN   (input) DOUBLE PRECISION
*           The minimum pivot in the Sturm sequence.
*
*  GAPTOL   (input) DOUBLE PRECISION
*           Tolerance that indicates when eigenvector entries are negligible
*           w.r.t. their contribution to the residual.
*
*  Z        (input/output) DOUBLE PRECISION array, dimension (N)
*           On input, all entries of Z must be set to 0.
*           On output, Z contains the (scaled) r-th column of the
*           inverse. The scaling is such that Z(R) equals 1.
*
*  WANTNC   (input) LOGICAL
*           Specifies whether NEGCNT has to be computed.
*
*  NEGCNT   (output) INTEGER
*           If WANTNC is .TRUE. then NEGCNT = the number of pivots < pivmin
*           in the  matrix factorization L D L^T, and NEGCNT = -1 otherwise.
*
*  ZTZ      (output) DOUBLE PRECISION
*           The square of the 2-norm of Z.
*
*  MINGMA   (output) DOUBLE PRECISION
*           The reciprocal of the largest (in magnitude) diagonal
*           element of the inverse of L D L^T - sigma I.
*
*  R        (input/output) INTEGER
*           The twist index for the twisted factorization used to
*           compute Z.
*           On input, 0 <= R <= N. If R is input as 0, R is set to
*           the index where (L D L^T - sigma I)^{-1} is largest
*           in magnitude. If 1 <= R <= N, R is unchanged.
*           On output, R contains the twist index used to compute Z.
*           Ideally, R designates the position of the maximum entry in the
*           eigenvector.
*
*  ISUPPZ   (output) INTEGER array, dimension (2)
*           The support of the vector in Z, i.e., the vector Z is
*           nonzero only in elements ISUPPZ(1) through ISUPPZ( 2 ).
*
*  NRMINV   (output) DOUBLE PRECISION
*           NRMINV = 1/SQRT( ZTZ )
*
*  RESID    (output) DOUBLE PRECISION
*           The residual of the FP vector.
*           RESID = ABS( MINGMA )/SQRT( ZTZ )
*
*  RQCORR   (output) DOUBLE PRECISION
*           The Rayleigh Quotient correction to LAMBDA.
*           RQCORR = MINGMA*TMP
*
*  WORK     (workspace) DOUBLE PRECISION array, dimension (4*N)
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
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )

*     ..
*     .. Local Scalars ..
      LOGICAL            SAWNAN1, SAWNAN2
      INTEGER            I, INDLPL, INDP, INDS, INDUMN, NEG1, NEG2, R1,
     $                   R2
      DOUBLE PRECISION   DMINUS, DPLUS, EPS, S, TMP
*     ..
*     .. External Functions ..
      LOGICAL DISNAN
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DISNAN, DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'Precision' )


      IF( R.EQ.0 ) THEN
         R1 = B1
         R2 = BN
      ELSE
         R1 = R
         R2 = R
      END IF

*     Storage for LPLUS
      INDLPL = 0
*     Storage for UMINUS
      INDUMN = N
      INDS = 2*N + 1
      INDP = 3*N + 1

      IF( B1.EQ.1 ) THEN
         WORK( INDS ) = ZERO
      ELSE
         WORK( INDS+B1-1 ) = LLD( B1-1 )
      END IF

*
*     Compute the stationary transform (using the differential form)
*     until the index R2.
*
      SAWNAN1 = .FALSE.
      NEG1 = 0
      S = WORK( INDS+B1-1 ) - LAMBDA
      DO 50 I = B1, R1 - 1
         DPLUS = D( I ) + S
         WORK( INDLPL+I ) = LD( I ) / DPLUS
         IF(DPLUS.LT.ZERO) NEG1 = NEG1 + 1
         WORK( INDS+I ) = S*WORK( INDLPL+I )*L( I )
         S = WORK( INDS+I ) - LAMBDA
 50   CONTINUE
      SAWNAN1 = DISNAN( S )
      IF( SAWNAN1 ) GOTO 60
      DO 51 I = R1, R2 - 1
         DPLUS = D( I ) + S
         WORK( INDLPL+I ) = LD( I ) / DPLUS
         WORK( INDS+I ) = S*WORK( INDLPL+I )*L( I )
         S = WORK( INDS+I ) - LAMBDA
 51   CONTINUE
      SAWNAN1 = DISNAN( S )
*
 60   CONTINUE
      IF( SAWNAN1 ) THEN
*        Runs a slower version of the above loop if a NaN is detected
         NEG1 = 0
         S = WORK( INDS+B1-1 ) - LAMBDA
         DO 70 I = B1, R1 - 1
            DPLUS = D( I ) + S
            IF(ABS(DPLUS).LT.PIVMIN) DPLUS = -PIVMIN
            WORK( INDLPL+I ) = LD( I ) / DPLUS
            IF(DPLUS.LT.ZERO) NEG1 = NEG1 + 1
            WORK( INDS+I ) = S*WORK( INDLPL+I )*L( I )
            IF( WORK( INDLPL+I ).EQ.ZERO )
     $                      WORK( INDS+I ) = LLD( I )
            S = WORK( INDS+I ) - LAMBDA
 70      CONTINUE
         DO 71 I = R1, R2 - 1
            DPLUS = D( I ) + S
            IF(ABS(DPLUS).LT.PIVMIN) DPLUS = -PIVMIN
            WORK( INDLPL+I ) = LD( I ) / DPLUS
            WORK( INDS+I ) = S*WORK( INDLPL+I )*L( I )
            IF( WORK( INDLPL+I ).EQ.ZERO )
     $                      WORK( INDS+I ) = LLD( I )
            S = WORK( INDS+I ) - LAMBDA
 71      CONTINUE
      END IF
*
*     Compute the progressive transform (using the differential form)
*     until the index R1
*
      SAWNAN2 = .FALSE.
      NEG2 = 0
      WORK( INDP+BN-1 ) = D( BN ) - LAMBDA
      DO 80 I = BN - 1, R1, -1
         DMINUS = LLD( I ) + WORK( INDP+I )
         TMP = D( I ) / DMINUS
         IF(DMINUS.LT.ZERO) NEG2 = NEG2 + 1
         WORK( INDUMN+I ) = L( I )*TMP
         WORK( INDP+I-1 ) = WORK( INDP+I )*TMP - LAMBDA
 80   CONTINUE
      TMP = WORK( INDP+R1-1 )
      SAWNAN2 = DISNAN( TMP )

      IF( SAWNAN2 ) THEN
*        Runs a slower version of the above loop if a NaN is detected
         NEG2 = 0
         DO 100 I = BN-1, R1, -1
            DMINUS = LLD( I ) + WORK( INDP+I )
            IF(ABS(DMINUS).LT.PIVMIN) DMINUS = -PIVMIN
            TMP = D( I ) / DMINUS
            IF(DMINUS.LT.ZERO) NEG2 = NEG2 + 1
            WORK( INDUMN+I ) = L( I )*TMP
            WORK( INDP+I-1 ) = WORK( INDP+I )*TMP - LAMBDA
            IF( TMP.EQ.ZERO )
     $          WORK( INDP+I-1 ) = D( I ) - LAMBDA
 100     CONTINUE
      END IF
*
*     Find the index (from R1 to R2) of the largest (in magnitude)
*     diagonal element of the inverse
*
      MINGMA = WORK( INDS+R1-1 ) + WORK( INDP+R1-1 )
      IF( MINGMA.LT.ZERO ) NEG1 = NEG1 + 1
      IF( WANTNC ) THEN
         NEGCNT = NEG1 + NEG2
      ELSE
         NEGCNT = -1
      ENDIF
      IF( ABS(MINGMA).EQ.ZERO )
     $   MINGMA = EPS*WORK( INDS+R1-1 )
      R = R1
      DO 110 I = R1, R2 - 1
         TMP = WORK( INDS+I ) + WORK( INDP+I )
         IF( TMP.EQ.ZERO )
     $      TMP = EPS*WORK( INDS+I )
         IF( ABS( TMP ).LE.ABS( MINGMA ) ) THEN
            MINGMA = TMP
            R = I + 1
         END IF
 110  CONTINUE
*
*     Compute the FP vector: solve N^T v = e_r
*
      ISUPPZ( 1 ) = B1
      ISUPPZ( 2 ) = BN
      Z( R ) = ONE
      ZTZ = ONE
*
*     Compute the FP vector upwards from R
*
      IF( .NOT.SAWNAN1 .AND. .NOT.SAWNAN2 ) THEN
         DO 210 I = R-1, B1, -1
            Z( I ) = -( WORK( INDLPL+I )*Z( I+1 ) )
            IF( (ABS(Z(I))+ABS(Z(I+1)))* ABS(LD(I)).LT.GAPTOL )
     $           THEN
               Z( I ) = ZERO
               ISUPPZ( 1 ) = I + 1
               GOTO 220
            ENDIF
            ZTZ = ZTZ + Z( I )*Z( I )
 210     CONTINUE
 220     CONTINUE
      ELSE
*        Run slower loop if NaN occurred.
         DO 230 I = R - 1, B1, -1
            IF( Z( I+1 ).EQ.ZERO ) THEN
               Z( I ) = -( LD( I+1 ) / LD( I ) )*Z( I+2 )
            ELSE
               Z( I ) = -( WORK( INDLPL+I )*Z( I+1 ) )
            END IF
            IF( (ABS(Z(I))+ABS(Z(I+1)))* ABS(LD(I)).LT.GAPTOL )
     $           THEN
               Z( I ) = ZERO
               ISUPPZ( 1 ) = I + 1
               GO TO 240
            END IF
            ZTZ = ZTZ + Z( I )*Z( I )
 230     CONTINUE
 240     CONTINUE
      ENDIF

*     Compute the FP vector downwards from R in blocks of size BLKSIZ
      IF( .NOT.SAWNAN1 .AND. .NOT.SAWNAN2 ) THEN
         DO 250 I = R, BN-1
            Z( I+1 ) = -( WORK( INDUMN+I )*Z( I ) )
            IF( (ABS(Z(I))+ABS(Z(I+1)))* ABS(LD(I)).LT.GAPTOL )
     $         THEN
               Z( I+1 ) = ZERO
               ISUPPZ( 2 ) = I
               GO TO 260
            END IF
            ZTZ = ZTZ + Z( I+1 )*Z( I+1 )
 250     CONTINUE
 260     CONTINUE
      ELSE
*        Run slower loop if NaN occurred.
         DO 270 I = R, BN - 1
            IF( Z( I ).EQ.ZERO ) THEN
               Z( I+1 ) = -( LD( I-1 ) / LD( I ) )*Z( I-1 )
            ELSE
               Z( I+1 ) = -( WORK( INDUMN+I )*Z( I ) )
            END IF
            IF( (ABS(Z(I))+ABS(Z(I+1)))* ABS(LD(I)).LT.GAPTOL )
     $           THEN
               Z( I+1 ) = ZERO
               ISUPPZ( 2 ) = I
               GO TO 280
            END IF
            ZTZ = ZTZ + Z( I+1 )*Z( I+1 )
 270     CONTINUE
 280     CONTINUE
      END IF
*
*     Compute quantities for convergence test
*
      TMP = ONE / ZTZ
      NRMINV = SQRT( TMP )
      RESID = ABS( MINGMA )*NRMINV
      RQCORR = MINGMA*TMP
*
*
      RETURN
*
*     End of DLAR1V
*
      END
      SUBROUTINE DLAR2V( N, X, Y, Z, INCX, C, S, INCC )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCC, INCX, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( * ), S( * ), X( * ), Y( * ), Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLAR2V applies a vector of real plane rotations from both sides to
*  a sequence of 2-by-2 real symmetric matrices, defined by the elements
*  of the vectors x, y and z. For i = 1,2,...,n
*
*     ( x(i)  z(i) ) := (  c(i)  s(i) ) ( x(i)  z(i) ) ( c(i) -s(i) )
*     ( z(i)  y(i) )    ( -s(i)  c(i) ) ( z(i)  y(i) ) ( s(i)  c(i) )
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of plane rotations to be applied.
*
*  X       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCX)
*          The vector x.
*
*  Y       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCX)
*          The vector y.
*
*  Z       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCX)
*          The vector z.
*
*  INCX    (input) INTEGER
*          The increment between elements of X, Y and Z. INCX > 0.
*
*  C       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
*          The cosines of the plane rotations.
*
*  S       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
*          The sines of the plane rotations.
*
*  INCC    (input) INTEGER
*          The increment between elements of C and S. INCC > 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IC, IX
      DOUBLE PRECISION   CI, SI, T1, T2, T3, T4, T5, T6, XI, YI, ZI
*     ..
*     .. Executable Statements ..
*
      IX = 1
      IC = 1
      DO 10 I = 1, N
         XI = X( IX )
         YI = Y( IX )
         ZI = Z( IX )
         CI = C( IC )
         SI = S( IC )
         T1 = SI*ZI
         T2 = CI*ZI
         T3 = T2 - SI*XI
         T4 = T2 + SI*YI
         T5 = CI*XI + T1
         T6 = CI*YI - T1
         X( IX ) = CI*T5 + SI*T4
         Y( IX ) = CI*T6 - SI*T3
         Z( IX ) = CI*T4 - SI*T5
         IX = IX + INCX
         IC = IC + INCC
   10 CONTINUE
*
*     End of DLAR2V
*
      RETURN
      END
      SUBROUTINE DLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      DOUBLE PRECISION   TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARF applies a real elementary reflector H to a real m by n matrix
*  C, from either the left or the right. H is represented in the form
*
*        H = I - tau * v * v'
*
*  where tau is a real scalar and v is a real vector.
*
*  If tau = 0, then H is taken to be the unit matrix.
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
*  V       (input) DOUBLE PRECISION array, dimension
*                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
*                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
*          The vector v in the representation of H. V is not used if
*          TAU = 0.
*
*  INCV    (input) INTEGER
*          The increment between elements of v. INCV <> 0.
*
*  TAU     (input) DOUBLE PRECISION
*          The value tau in the representation of H.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*          or C * H if SIDE = 'R'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDC >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                         (N) if SIDE = 'L'
*                      or (M) if SIDE = 'R'
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER
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
            CALL DGEMV( 'Transpose', M, N, ONE, C, LDC, V, INCV, ZERO,
     $                  WORK, 1 )
*
*           C := C - v * w'
*
            CALL DGER( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         END IF
      ELSE
*
*        Form  C * H
*
         IF( TAU.NE.ZERO ) THEN
*
*           w := C * v
*
            CALL DGEMV( 'No transpose', M, N, ONE, C, LDC, V, INCV,
     $                  ZERO, WORK, 1 )
*
*           C := C - w * v'
*
            CALL DGER( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         END IF
      END IF
      RETURN
*
*     End of DLARF
*
      END
      SUBROUTINE DLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV,
     $                   T, LDT, C, LDC, WORK, LDWORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), T( LDT, * ), V( LDV, * ),
     $                   WORK( LDWORK, * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFB applies a real block reflector H or its transpose H' to a
*  real m by n matrix C, from either the left or the right.
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
*          = 'T': apply H' (Transpose)
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
*  V       (input) DOUBLE PRECISION array, dimension
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
*  T       (input) DOUBLE PRECISION array, dimension (LDT,K)
*          The triangular k by k matrix T in the representation of the
*          block reflector.
*
*  LDT     (input) INTEGER
*          The leading dimension of the array T. LDT >= K.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDA >= max(1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
*
*  LDWORK  (input) INTEGER
*          The leading dimension of the array WORK.
*          If SIDE = 'L', LDWORK >= max(1,N);
*          if SIDE = 'R', LDWORK >= max(1,M).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
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
      EXTERNAL           DCOPY, DGEMM, DTRMM
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'T'
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
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
   10          CONTINUE
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C2'*V2
*
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C( K+1, 1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               IF( M.GT.K ) THEN
*
*                 C2 := C2 - V2 * W'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V( K+1, 1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 30 J = 1, K
                  DO 20 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
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
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          CONTINUE
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
     $                     K, ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C2 * V2
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               IF( N.GT.K ) THEN
*
*                 C2 := C2 - W * V2'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( K+1, 1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
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
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
   70          CONTINUE
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C1'*V1
*
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V * W'
*
               IF( M.GT.K ) THEN
*
*                 C1 := C1 - V1 * W'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K,
     $                        -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 90 J = 1, K
                  DO 80 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
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
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  100          CONTINUE
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
     $                     K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C1 * V1
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V'
*
               IF( N.GT.K ) THEN
*
*                 C1 := C1 - W * V1'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
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
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
  130          CONTINUE
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C2'*V2'
*
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE,
     $                        WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               IF( M.GT.K ) THEN
*
*                 C2 := C2 - V2' * W'
*
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V( 1, K+1 ), LDV, WORK, LDWORK, ONE,
     $                        C( K+1, 1 ), LDC )
               END IF
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N,
     $                     K, ONE, V, LDV, WORK, LDWORK )
*
*              C1 := C1 - W'
*
               DO 150 J = 1, K
                  DO 140 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
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
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          CONTINUE
*
*              W := W * V1'
*
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K,
     $                     ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C2 * V2'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C( 1, K+1 ), LDC, V( 1, K+1 ), LDV,
     $                        ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               IF( N.GT.K ) THEN
*
*                 C2 := C2 - W * V2
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE,
     $                        C( 1, K+1 ), LDC )
               END IF
*
*              W := W * V1
*
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M,
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
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
  190          CONTINUE
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K,
     $                     ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
*
*                 W := W + C1'*V1'
*
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE,
     $                        C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T'  or  W * T
*
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - V' * W'
*
               IF( M.GT.K ) THEN
*
*                 C1 := C1 - V1' * W'
*
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE,
     $                        V, LDV, WORK, LDWORK, ONE, C, LDC )
               END IF
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N,
     $                     K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
*
*              C2 := C2 - W'
*
               DO 210 J = 1, K
                  DO 200 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
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
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  220          CONTINUE
*
*              W := W * V2'
*
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K,
     $                     ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
*
*                 W := W + C1 * V1'
*
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K,
     $                        ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               END IF
*
*              W := W * T  or  W * T'
*
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K,
     $                     ONE, T, LDT, WORK, LDWORK )
*
*              C := C - W * V
*
               IF( N.GT.K ) THEN
*
*                 C1 := C1 - W * V1
*
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K,
     $                        -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               END IF
*
*              W := W * V2
*
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M,
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
*     End of DLARFB
*
      END
      SUBROUTINE DLARFG( N, ALPHA, X, INCX, TAU )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   ALPHA, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFG generates a real elementary reflector H of order n, such
*  that
*
*        H * ( alpha ) = ( beta ),   H' * H = I.
*            (   x   )   (   0  )
*
*  where alpha and beta are scalars, and x is an (n-1)-element real
*  vector. H is represented in the form
*
*        H = I - tau * ( 1 ) * ( 1 v' ) ,
*                      ( v )
*
*  where tau is a real scalar and v is a real (n-1)-element
*  vector.
*
*  If the elements of x are all zero, then tau = 0 and H is taken to be
*  the unit matrix.
*
*  Otherwise  1 <= tau <= 2.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the elementary reflector.
*
*  ALPHA   (input/output) DOUBLE PRECISION
*          On entry, the value alpha.
*          On exit, it is overwritten with the value beta.
*
*  X       (input/output) DOUBLE PRECISION array, dimension
*                         (1+(N-2)*abs(INCX))
*          On entry, the vector x.
*          On exit, it is overwritten with the vector v.
*
*  INCX    (input) INTEGER
*          The increment between elements of X. INCX > 0.
*
*  TAU     (output) DOUBLE PRECISION
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
      DOUBLE PRECISION   BETA, RSAFMN, SAFMIN, XNORM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2, DNRM2
      EXTERNAL           DLAMCH, DLAPY2, DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSCAL
*     ..
*     .. Executable Statements ..
*
      IF( N.LE.1 ) THEN
         TAU = ZERO
         RETURN
      END IF
*
      XNORM = DNRM2( N-1, X, INCX )
*
      IF( XNORM.EQ.ZERO ) THEN
*
*        H  =  I
*
         TAU = ZERO
      ELSE
*
*        general case
*
         BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
         IF( ABS( BETA ).LT.SAFMIN ) THEN
*
*           XNORM, BETA may be inaccurate; scale X and recompute them
*
            RSAFMN = ONE / SAFMIN
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL DSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN )
     $         GO TO 10
*
*           New BETA is at most 1, at least SAFMIN
*
            XNORM = DNRM2( N-1, X, INCX )
            BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
*
*           If ALPHA is subnormal, it may lose relative accuracy
*
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
            ALPHA = BETA
         END IF
      END IF
*
      RETURN
*
*     End of DLARFG
*
      END
      SUBROUTINE DLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   T( LDT, * ), TAU( * ), V( LDV, * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFT forms the triangular factor T of a real block reflector H
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
*  V       (input/output) DOUBLE PRECISION array, dimension
*                               (LDV,K) if STOREV = 'C'
*                               (LDV,N) if STOREV = 'R'
*          The matrix V. See further details.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V.
*          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
*
*  TAU     (input) DOUBLE PRECISION array, dimension (K)
*          TAU(i) must contain the scalar factor of the elementary
*          reflector H(i).
*
*  T       (output) DOUBLE PRECISION array, dimension (LDT,K)
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
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   VII
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DTRMV
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
                  CALL DGEMV( 'Transpose', N-I+1, I-1, -TAU( I ),
     $                        V( I, 1 ), LDV, V( I, I ), 1, ZERO,
     $                        T( 1, I ), 1 )
               ELSE
*
*                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'
*
                  CALL DGEMV( 'No transpose', I-1, N-I+1, -TAU( I ),
     $                        V( 1, I ), LDV, V( I, I ), LDV, ZERO,
     $                        T( 1, I ), 1 )
               END IF
               V( I, I ) = VII
*
*              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
*
               CALL DTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T,
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
                     CALL DGEMV( 'Transpose', N-K+I, K-I, -TAU( I ),
     $                           V( 1, I+1 ), LDV, V( 1, I ), 1, ZERO,
     $                           T( I+1, I ), 1 )
                     V( N-K+I, I ) = VII
                  ELSE
                     VII = V( I, N-K+I )
                     V( I, N-K+I ) = ONE
*
*                    T(i+1:k,i) :=
*                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'
*
                     CALL DGEMV( 'No transpose', K-I, N-K+I, -TAU( I ),
     $                           V( I+1, 1 ), LDV, V( I, 1 ), LDV, ZERO,
     $                           T( I+1, I ), 1 )
                     V( I, N-K+I ) = VII
                  END IF
*
*                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
*
                  CALL DTRMV( 'Lower', 'No transpose', 'Non-unit', K-I,
     $                        T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
               END IF
               T( I, I ) = TAU( I )
            END IF
   40    CONTINUE
      END IF
      RETURN
*
*     End of DLARFT
*
      END
      SUBROUTINE DLARFX( SIDE, M, N, V, TAU, C, LDC, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            LDC, M, N
      DOUBLE PRECISION   TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARFX applies a real elementary reflector H to a real m by n
*  matrix C, from either the left or the right. H is represented in the
*  form
*
*        H = I - tau * v * v'
*
*  where tau is a real scalar and v is a real vector.
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
*  V       (input) DOUBLE PRECISION array, dimension (M) if SIDE = 'L'
*                                     or (N) if SIDE = 'R'
*          The vector v in the representation of H.
*
*  TAU     (input) DOUBLE PRECISION
*          The value tau in the representation of H.
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
*          On entry, the m by n matrix C.
*          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
*          or C * H if SIDE = 'R'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C. LDA >= (1,M).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                      (N) if SIDE = 'L'
*                      or (M) if SIDE = 'R'
*          WORK is not referenced if H has order < 11.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J
      DOUBLE PRECISION   SUM, T1, T10, T2, T3, T4, T5, T6, T7, T8, T9,
     $                   V1, V10, V2, V3, V4, V5, V6, V7, V8, V9
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER
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
         CALL DGEMV( 'Transpose', M, N, ONE, C, LDC, V, 1, ZERO, WORK,
     $               1 )
*
*        C := C - tau * v * w'
*
         CALL DGER( M, N, -TAU, V, 1, WORK, 1, C, LDC )
         GO TO 410
   10    CONTINUE
*
*        Special code for 1 x 1 Householder
*
         T1 = ONE - TAU*V( 1 )*V( 1 )
         DO 20 J = 1, N
            C( 1, J ) = T1*C( 1, J )
   20    CONTINUE
         GO TO 410
   30    CONTINUE
*
*        Special code for 2 x 2 Householder
*
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
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
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
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
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
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
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
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
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
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
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
         V7 = V( 7 )
         T7 = TAU*V7
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
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
         V7 = V( 7 )
         T7 = TAU*V7
         V8 = V( 8 )
         T8 = TAU*V8
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
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
         V7 = V( 7 )
         T7 = TAU*V7
         V8 = V( 8 )
         T8 = TAU*V8
         V9 = V( 9 )
         T9 = TAU*V9
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
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
         V7 = V( 7 )
         T7 = TAU*V7
         V8 = V( 8 )
         T8 = TAU*V8
         V9 = V( 9 )
         T9 = TAU*V9
         V10 = V( 10 )
         T10 = TAU*V10
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
         CALL DGEMV( 'No transpose', M, N, ONE, C, LDC, V, 1, ZERO,
     $               WORK, 1 )
*
*        C := C - tau * w * v'
*
         CALL DGER( M, N, -TAU, WORK, 1, V, 1, C, LDC )
         GO TO 410
  210    CONTINUE
*
*        Special code for 1 x 1 Householder
*
         T1 = ONE - TAU*V( 1 )*V( 1 )
         DO 220 J = 1, M
            C( J, 1 ) = T1*C( J, 1 )
  220    CONTINUE
         GO TO 410
  230    CONTINUE
*
*        Special code for 2 x 2 Householder
*
         V1 = V( 1 )
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
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
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
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
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
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
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
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
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
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
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
         V7 = V( 7 )
         T7 = TAU*V7
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
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
         V7 = V( 7 )
         T7 = TAU*V7
         V8 = V( 8 )
         T8 = TAU*V8
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
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
         V7 = V( 7 )
         T7 = TAU*V7
         V8 = V( 8 )
         T8 = TAU*V8
         V9 = V( 9 )
         T9 = TAU*V9
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
         T1 = TAU*V1
         V2 = V( 2 )
         T2 = TAU*V2
         V3 = V( 3 )
         T3 = TAU*V3
         V4 = V( 4 )
         T4 = TAU*V4
         V5 = V( 5 )
         T5 = TAU*V5
         V6 = V( 6 )
         T6 = TAU*V6
         V7 = V( 7 )
         T7 = TAU*V7
         V8 = V( 8 )
         T8 = TAU*V8
         V9 = V( 9 )
         T9 = TAU*V9
         V10 = V( 10 )
         T10 = TAU*V10
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
*     End of DLARFX
*
      END
      SUBROUTINE DLARGV( N, X, INCX, Y, INCY, C, INCC )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCC, INCX, INCY, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARGV generates a vector of real plane rotations, determined by
*  elements of the real vectors x and y. For i = 1,2,...,n
*
*     (  c(i)  s(i) ) ( x(i) ) = ( a(i) )
*     ( -s(i)  c(i) ) ( y(i) ) = (   0  )
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of plane rotations to be generated.
*
*  X       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCX)
*          On entry, the vector x.
*          On exit, x(i) is overwritten by a(i), for i = 1,...,n.
*
*  INCX    (input) INTEGER
*          The increment between elements of X. INCX > 0.
*
*  Y       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCY)
*          On entry, the vector y.
*          On exit, the sines of the plane rotations.
*
*  INCY    (input) INTEGER
*          The increment between elements of Y. INCY > 0.
*
*  C       (output) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
*          The cosines of the plane rotations.
*
*  INCC    (input) INTEGER
*          The increment between elements of C. INCC > 0.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IC, IX, IY
      DOUBLE PRECISION   F, G, T, TT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
*     ..
*     .. Executable Statements ..
*
      IX = 1
      IY = 1
      IC = 1
      DO 10 I = 1, N
         F = X( IX )
         G = Y( IY )
         IF( G.EQ.ZERO ) THEN
            C( IC ) = ONE
         ELSE IF( F.EQ.ZERO ) THEN
            C( IC ) = ZERO
            Y( IY ) = ONE
            X( IX ) = G
         ELSE IF( ABS( F ).GT.ABS( G ) ) THEN
            T = G / F
            TT = SQRT( ONE+T*T )
            C( IC ) = ONE / TT
            Y( IY ) = T*C( IC )
            X( IX ) = F*TT
         ELSE
            T = F / G
            TT = SQRT( ONE+T*T )
            Y( IY ) = ONE / TT
            C( IC ) = T*Y( IY )
            X( IX ) = G*TT
         END IF
         IC = IC + INCC
         IY = IY + INCY
         IX = IX + INCX
   10 CONTINUE
      RETURN
*
*     End of DLARGV
*
      END
      SUBROUTINE DLARNV( IDIST, ISEED, N, X )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IDIST, N
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARNV returns a vector of n random real numbers from a uniform or
*  normal distribution.
*
*  Arguments
*  =========
*
*  IDIST   (input) INTEGER
*          Specifies the distribution of the random numbers:
*          = 1:  uniform (0,1)
*          = 2:  uniform (-1,1)
*          = 3:  normal (0,1)
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator; the array
*          elements must be between 0 and 4095, and ISEED(4) must be
*          odd.
*          On exit, the seed is updated.
*
*  N       (input) INTEGER
*          The number of random numbers to be generated.
*
*  X       (output) DOUBLE PRECISION array, dimension (N)
*          The generated random numbers.
*
*  Further Details
*  ===============
*
*  This routine calls the auxiliary routine DLARUV to generate random
*  real numbers from a uniform (0,1) distribution, in batches of up to
*  128 using vectorisable code. The Box-Muller method is used to
*  transform numbers from a uniform to a normal distribution.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, TWO
      PARAMETER          ( ONE = 1.0D+0, TWO = 2.0D+0 )
      INTEGER            LV
      PARAMETER          ( LV = 128 )
      DOUBLE PRECISION   TWOPI
      PARAMETER          ( TWOPI = 6.2831853071795864769252867663D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IL, IL2, IV
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   U( LV )
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          COS, LOG, MIN, SQRT
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLARUV
*     ..
*     .. Executable Statements ..
*
      DO 40 IV = 1, N, LV / 2
         IL = MIN( LV / 2, N-IV+1 )
         IF( IDIST.EQ.3 ) THEN
            IL2 = 2*IL
         ELSE
            IL2 = IL
         END IF
*
*        Call DLARUV to generate IL2 numbers from a uniform (0,1)
*        distribution (IL2 <= LV)
*
         CALL DLARUV( ISEED, IL2, U )
*
         IF( IDIST.EQ.1 ) THEN
*
*           Copy generated numbers
*
            DO 10 I = 1, IL
               X( IV+I-1 ) = U( I )
   10       CONTINUE
         ELSE IF( IDIST.EQ.2 ) THEN
*
*           Convert generated numbers to uniform (-1,1) distribution
*
            DO 20 I = 1, IL
               X( IV+I-1 ) = TWO*U( I ) - ONE
   20       CONTINUE
         ELSE IF( IDIST.EQ.3 ) THEN
*
*           Convert generated numbers to normal (0,1) distribution
*
            DO 30 I = 1, IL
               X( IV+I-1 ) = SQRT( -TWO*LOG( U( 2*I-1 ) ) )*
     $                       COS( TWOPI*U( 2*I ) )
   30       CONTINUE
         END IF
   40 CONTINUE
      RETURN
*
*     End of DLARNV
*
      END
      SUBROUTINE DLARRB( N, D, LLD, IFIRST, ILAST, RTOL1,
     $                   RTOL2, OFFSET, W, WGAP, WERR, WORK, IWORK,
     $                   PIVMIN, SPDIAM, TWIST, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IFIRST, ILAST, INFO, N, OFFSET, TWIST
      DOUBLE PRECISION   PIVMIN, RTOL1, RTOL2, SPDIAM
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), LLD( * ), W( * ),
     $                   WERR( * ), WGAP( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  Given the relatively robust representation(RRR) L D L^T, DLARRB
*  does "limited" bisection to refine the eigenvalues of L D L^T,
*  W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
*  guesses for these eigenvalues are input in W, the corresponding estimate
*  of the error in these guesses and their gaps are input in WERR
*  and WGAP, respectively. During bisection, intervals
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
*          The N diagonal elements of the diagonal matrix D.
*
*  LLD     (input) DOUBLE PRECISION array, dimension (N-1)
*          The (N-1) elements L(i)*L(i)*D(i).
*
*  IFIRST  (input) INTEGER
*          The index of the first eigenvalue to be computed.
*
*  ILAST   (input) INTEGER
*          The index of the last eigenvalue to be computed.
*
*  RTOL1   (input) DOUBLE PRECISION
*  RTOL2   (input) DOUBLE PRECISION
*          Tolerance for the convergence of the bisection intervals.
*          An interval [LEFT,RIGHT] has converged if
*          RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )
*          where GAP is the (estimated) distance to the nearest
*          eigenvalue.
*
*  OFFSET  (input) INTEGER
*          Offset for the arrays W, WGAP and WERR, i.e., the IFIRST-OFFSET
*          through ILAST-OFFSET elements of these arrays are to be used.
*
*  W       (input/output) DOUBLE PRECISION array, dimension (N)
*          On input, W( IFIRST-OFFSET ) through W( ILAST-OFFSET ) are
*          estimates of the eigenvalues of L D L^T indexed IFIRST throug
*          ILAST.
*          On output, these estimates are refined.
*
*  WGAP    (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On input, the (estimated) gaps between consecutive
*          eigenvalues of L D L^T, i.e., WGAP(I-OFFSET) is the gap between
*          eigenvalues I and I+1. Note that if IFIRST.EQ.ILAST
*          then WGAP(IFIRST-OFFSET) must be set to ZERO.
*          On output, these gaps are refined.
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
*          The minimum pivot in the Sturm sequence.
*
*  SPDIAM  (input) DOUBLE PRECISION
*          The spectral diameter of the matrix.
*
*  TWIST   (input) INTEGER
*          The twist index for the twisted factorization that is used
*          for the negcount.
*          TWIST = N: Compute negcount from L D L^T - LAMBDA I = L+ D+ L+^T
*          TWIST = 1: Compute negcount from L D L^T - LAMBDA I = U- D- U-^T
*          TWIST = R: Compute negcount from L D L^T - LAMBDA I = N(r) D(r) N(r)
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
      DOUBLE PRECISION   ZERO, TWO, HALF
      PARAMETER        ( ZERO = 0.0D0, TWO = 2.0D0,
     $                   HALF = 0.5D0 )
      INTEGER   MAXITR
*     ..
*     .. Local Scalars ..
      INTEGER            I, I1, II, IP, ITER, K, NEGCNT, NEXT, NINT,
     $                   OLNINT, PREV, R
      DOUBLE PRECISION   BACK, CVRGD, GAP, LEFT, LGAP, MID, MNWDTH,
     $                   RGAP, RIGHT, TMP, WIDTH
*     ..
*     .. External Functions ..
      INTEGER            DLANEG
      EXTERNAL           DLANEG
*
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
      MAXITR = INT( ( LOG( SPDIAM+PIVMIN )-LOG( PIVMIN ) ) /
     $           LOG( TWO ) ) + 2
      MNWDTH = TWO * PIVMIN
*
      R = TWIST
      IF((R.LT.1).OR.(R.GT.N)) R = N
*
*     Initialize unconverged intervals in [ WORK(2*I-1), WORK(2*I) ].
*     The Sturm Count, Count( WORK(2*I-1) ) is arranged to be I-1, while
*     Count( WORK(2*I) ) is stored in IWORK( 2*I ). The integer IWORK( 2*I-1 )
*     for an unconverged interval is set to the index of the next unconverged
*     interval, and is -1 or 0 for a converged interval. Thus a linked
*     list of unconverged intervals is set up.
*
      I1 = IFIRST
*     The number of unconverged intervals
      NINT = 0
*     The last unconverged interval found
      PREV = 0

      RGAP = WGAP( I1-OFFSET )
      DO 75 I = I1, ILAST
         K = 2*I
         II = I - OFFSET
         LEFT = W( II ) - WERR( II )
         RIGHT = W( II ) + WERR( II )
         LGAP = RGAP
         RGAP = WGAP( II )
         GAP = MIN( LGAP, RGAP )

*        Make sure that [LEFT,RIGHT] contains the desired eigenvalue
*        Compute negcount from dstqds facto L+D+L+^T = L D L^T - LEFT
*
*        Do while( NEGCNT(LEFT).GT.I-1 )
*
         BACK = WERR( II )
 20      CONTINUE
         NEGCNT = DLANEG( N, D, LLD, LEFT, PIVMIN, R )
         IF( NEGCNT.GT.I-1 ) THEN
            LEFT = LEFT - BACK
            BACK = TWO*BACK
            GO TO 20
         END IF
*
*        Do while( NEGCNT(RIGHT).LT.I )
*        Compute negcount from dstqds facto L+D+L+^T = L D L^T - RIGHT
*
         BACK = WERR( II )
 50      CONTINUE

         NEGCNT = DLANEG( N, D, LLD, RIGHT, PIVMIN, R )
          IF( NEGCNT.LT.I ) THEN
             RIGHT = RIGHT + BACK
             BACK = TWO*BACK
             GO TO 50
          END IF
         WIDTH = HALF*ABS( LEFT - RIGHT )
         TMP = MAX( ABS( LEFT ), ABS( RIGHT ) )
         CVRGD = MAX(RTOL1*GAP,RTOL2*TMP)
         IF( WIDTH.LE.CVRGD .OR. WIDTH.LE.MNWDTH ) THEN
*           This interval has already converged and does not need refinement.
*           (Note that the gaps might change through refining the
*            eigenvalues, however, they can only get bigger.)
*           Remove it from the list.
            IWORK( K-1 ) = -1
*           Make sure that I1 always points to the first unconverged interval
            IF((I.EQ.I1).AND.(I.LT.ILAST)) I1 = I + 1
            IF((PREV.GE.I1).AND.(I.LE.ILAST)) IWORK( 2*PREV-1 ) = I + 1
         ELSE
*           unconverged interval found
            PREV = I
            NINT = NINT + 1
            IWORK( K-1 ) = I + 1
            IWORK( K ) = NEGCNT
         END IF
         WORK( K-1 ) = LEFT
         WORK( K ) = RIGHT
 75   CONTINUE

*
*     Do while( NINT.GT.0 ), i.e. there are still unconverged intervals
*     and while (ITER.LT.MAXITR)
*
      ITER = 0
 80   CONTINUE
      PREV = I1 - 1
      I = I1
      OLNINT = NINT

      DO 100 IP = 1, OLNINT
         K = 2*I
         II = I - OFFSET
         RGAP = WGAP( II )
         LGAP = RGAP
         IF(II.GT.1) LGAP = WGAP( II-1 )
         GAP = MIN( LGAP, RGAP )
         NEXT = IWORK( K-1 )
         LEFT = WORK( K-1 )
         RIGHT = WORK( K )
         MID = HALF*( LEFT + RIGHT )

*        semiwidth of interval
         WIDTH = RIGHT - MID
         TMP = MAX( ABS( LEFT ), ABS( RIGHT ) )
         CVRGD = MAX(RTOL1*GAP,RTOL2*TMP)
         IF( ( WIDTH.LE.CVRGD ) .OR. ( WIDTH.LE.MNWDTH ).OR.
     $       ( ITER.EQ.MAXITR ) )THEN
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
         NEGCNT = DLANEG( N, D, LLD, MID, PIVMIN, R )
         IF( NEGCNT.LE.I-1 ) THEN
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
      DO 110 I = IFIRST, ILAST
         K = 2*I
         II = I - OFFSET
*        All intervals marked by '0' have been refined.
         IF( IWORK( K-1 ).EQ.0 ) THEN
            W( II ) = HALF*( WORK( K-1 )+WORK( K ) )
            WERR( II ) = WORK( K ) - W( II )
         END IF
 110  CONTINUE
*
      DO 111 I = IFIRST+1, ILAST
         K = 2*I
         II = I - OFFSET
         WGAP( II-1 ) = MAX( ZERO,
     $                     W(II) - WERR (II) - W( II-1 ) - WERR( II-1 ))
 111  CONTINUE

      RETURN
*
*     End of DLARRB
*
      END
      SUBROUTINE DLARRE( RANGE, N, VL, VU, IL, IU, D, E, E2,
     $                    RTOL1, RTOL2, SPLTOL, NSPLIT, ISPLIT, M,
     $                    W, WERR, WGAP, IBLOCK, INDEXW, GERS, PIVMIN,
     $                    WORK, IWORK, INFO )
      IMPLICIT NONE
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          RANGE
      INTEGER            IL, INFO, IU, M, N, NSPLIT
      DOUBLE PRECISION  PIVMIN, RTOL1, RTOL2, SPLTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IBLOCK( * ), ISPLIT( * ), IWORK( * ),
     $                   INDEXW( * )
      DOUBLE PRECISION   D( * ), E( * ), E2( * ), GERS( * ),
     $                   W( * ),WERR( * ), WGAP( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  To find the desired eigenvalues of a given real symmetric
*  tridiagonal matrix T, DLARRE sets any "small" off-diagonal
*  elements to zero, and for each unreduced block T_i, it finds
*  (a) a suitable shift at one end of the block's spectrum,
*  (b) the base representation, T_i - sigma_i I = L_i D_i L_i^T, and
*  (c) eigenvalues of each L_i D_i L_i^T.
*  The representations and eigenvalues found are then used by
*  DSTEMR to compute the eigenvectors of T.
*  The accuracy varies depending on whether bisection is used to
*  find a few eigenvalues or the dqds algorithm (subroutine DLASQ2) to
*  conpute all and then discard any unwanted one.
*  As an added benefit, DLARRE also outputs the n
*  Gerschgorin intervals for the matrices L_i D_i L_i^T.
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
*  N       (input) INTEGER
*          The order of the matrix. N > 0.
*
*  VL      (input/output) DOUBLE PRECISION
*  VU      (input/output) DOUBLE PRECISION
*          If RANGE='V', the lower and upper bounds for the eigenvalues.
*          Eigenvalues less than or equal to VL, or greater than VU,
*          will not be returned.  VL < VU.
*          If RANGE='I' or ='A', DLARRE computes bounds on the desired
*          part of the spectrum.
*
*  IL      (input) INTEGER
*  IU      (input) INTEGER
*          If RANGE='I', the indices (in ascending order) of the
*          smallest and largest eigenvalues to be returned.
*          1 <= IL <= IU <= N.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the N diagonal elements of the tridiagonal
*          matrix T.
*          On exit, the N diagonal elements of the diagonal
*          matrices D_i.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the first (N-1) entries contain the subdiagonal
*          elements of the tridiagonal matrix T; E(N) need not be set.
*          On exit, E contains the subdiagonal elements of the unit
*          bidiagonal matrices L_i. The entries E( ISPLIT( I ) ),
*          1 <= I <= NSPLIT, contain the base points sigma_i on output.
*
*  E2      (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the first (N-1) entries contain the SQUARES of the
*          subdiagonal elements of the tridiagonal matrix T;
*          E2(N) need not be set.
*          On exit, the entries E2( ISPLIT( I ) ),
*          1 <= I <= NSPLIT, have been set to zero
*
*  RTOL1   (input) DOUBLE PRECISION
*  RTOL2   (input) DOUBLE PRECISION
*           Parameters for bisection.
*           An interval [LEFT,RIGHT] has converged if
*           RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )
*
*  SPLTOL (input) DOUBLE PRECISION
*          The threshold for splitting.
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
*  M       (output) INTEGER
*          The total number of eigenvalues (of all L_i D_i L_i^T)
*          found.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          The first M elements contain the eigenvalues. The
*          eigenvalues of each of the blocks, L_i D_i L_i^T, are
*          sorted in ascending order ( DLARRE may use the
*          remaining N-M elements as workspace).
*
*  WERR    (output) DOUBLE PRECISION array, dimension (N)
*          The error bound on the corresponding eigenvalue in W.
*
*  WGAP    (output) DOUBLE PRECISION array, dimension (N)
*          The separation from the right neighbor eigenvalue in W.
*          The gap is only with respect to the eigenvalues of the same block
*          as each block has its own representation tree.
*          Exception: at the right end of a block we store the left gap
*
*  IBLOCK  (output) INTEGER array, dimension (N)
*          The indices of the blocks (submatrices) associated with the
*          corresponding eigenvalues in W; IBLOCK(i)=1 if eigenvalue
*          W(i) belongs to the first block from the top, =2 if W(i)
*          belongs to the second block, etc.
*
*  INDEXW  (output) INTEGER array, dimension (N)
*          The indices of the eigenvalues within each block (submatrix);
*          for example, INDEXW(i)= 10 and IBLOCK(i)=2 imply that the
*          i-th eigenvalue W(i) is the 10-th eigenvalue in block 2
*
*  GERS    (output) DOUBLE PRECISION array, dimension (2*N)
*          The N Gerschgorin intervals (the i-th Gerschgorin interval
*          is (GERS(2*i-1), GERS(2*i)).
*
*  PIVMIN  (output) DOUBLE PRECISION
*          The minimum pivot in the Sturm sequence for T.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (6*N)
*          Workspace.
*
*  IWORK   (workspace) INTEGER array, dimension (5*N)
*          Workspace.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          > 0:  A problem occured in DLARRE.
*          < 0:  One of the called subroutines signaled an internal problem.
*                Needs inspection of the corresponding parameter IINFO
*                for further information.
*
*          =-1:  Problem in DLARRD.
*          = 2:  No base representation could be found in MAXTRY iterations.
*                Increasing MAXTRY and recompilation might be a remedy.
*          =-3:  Problem in DLARRB when computing the refined root
*                representation for DLASQ2.
*          =-4:  Problem in DLARRB when preforming bisection on the
*                desired part of the spectrum.
*          =-5:  Problem in DLASQ2.
*          =-6:  Problem in DLASQ2.
*
*  Further Details
*  The base representations are required to suffer very little
*  element growth and consequently define all their eigenvalues to
*  high relative accuracy.
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
      DOUBLE PRECISION   FAC, FOUR, FOURTH, FUDGE, HALF, HNDRD,
     $                   MAXGROWTH, ONE, PERT, TWO, ZERO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0,
     $                     TWO = 2.0D0, FOUR=4.0D0,
     $                     HNDRD = 100.0D0,
     $                     PERT = 8.0D0,
     $                     HALF = ONE/TWO, FOURTH = ONE/FOUR, FAC= HALF,
     $                     MAXGROWTH = 64.0D0, FUDGE = 2.0D0 )
      INTEGER            MAXTRY, ALLRNG, INDRNG, VALRNG
      PARAMETER          ( MAXTRY = 6, ALLRNG = 1, INDRNG = 2,
     $                     VALRNG = 3 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FORCEB, NOREP, USEDQD
      INTEGER            CNT, CNT1, CNT2, I, IBEGIN, IDUM, IEND, IINFO,
     $                   IN, INDL, INDU, IRANGE, J, JBLK, MB, MM,
     $                   WBEGIN, WEND
      DOUBLE PRECISION   AVGAP, BSRTOL, CLWDTH, DMAX, DPIVOT, EABS,
     $                   EMAX, EOLD, EPS, GL, GU, ISLEFT, ISRGHT, RTL,
     $                   RTOL, S1, S2, SAFMIN, SGNDEF, SIGMA, SPDIAM,
     $                   TAU, TMP, TMP1


*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION            DLAMCH
      EXTERNAL           DLAMCH, LSAME

*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLARNV, DLARRA, DLARRB, DLARRC, DLARRD,
     $                   DLASQ2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN

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
      END IF

      M = 0

*     Get machine constants
      SAFMIN = DLAMCH( 'S' )
      EPS = DLAMCH( 'P' )

*     Set parameters
      RTL = SQRT(EPS)
      BSRTOL = SQRT(EPS)

*     Treat case of 1x1 matrix for quick return
      IF( N.EQ.1 ) THEN
         IF( (IRANGE.EQ.ALLRNG).OR.
     $       ((IRANGE.EQ.VALRNG).AND.(D(1).GT.VL).AND.(D(1).LE.VU)).OR.
     $       ((IRANGE.EQ.INDRNG).AND.(IL.EQ.1).AND.(IU.EQ.1)) ) THEN
            M = 1
            W(1) = D(1)
*           The computation error of the eigenvalue is zero
            WERR(1) = ZERO
            WGAP(1) = ZERO
            IBLOCK( 1 ) = 1
            INDEXW( 1 ) = 1
            GERS(1) = D( 1 )
            GERS(2) = D( 1 )
         ENDIF
*        store the shift for the initial RRR, which is zero in this case
         E(1) = ZERO
         RETURN
      END IF

*     General case: tridiagonal matrix of order > 1
*
*     Init WERR, WGAP. Compute Gerschgorin intervals and spectral diameter.
*     Compute maximum off-diagonal entry and pivmin.
      GL = D(1)
      GU = D(1)
      EOLD = ZERO
      EMAX = ZERO
      E(N) = ZERO
      DO 5 I = 1,N
         WERR(I) = ZERO
         WGAP(I) = ZERO
         EABS = ABS( E(I) )
         IF( EABS .GE. EMAX ) THEN
            EMAX = EABS
         END IF
         TMP1 = EABS + EOLD
         GERS( 2*I-1) = D(I) - TMP1
         GL =  MIN( GL, GERS( 2*I - 1))
         GERS( 2*I ) = D(I) + TMP1
         GU = MAX( GU, GERS(2*I) )
         EOLD  = EABS
 5    CONTINUE
*     The minimum pivot allowed in the Sturm sequence for T
      PIVMIN = SAFMIN * MAX( ONE, EMAX**2 )
*     Compute spectral diameter. The Gerschgorin bounds give an
*     estimate that is wrong by at most a factor of SQRT(2)
      SPDIAM = GU - GL

*     Compute splitting points
      CALL DLARRA( N, D, E, E2, SPLTOL, SPDIAM,
     $                    NSPLIT, ISPLIT, IINFO )

*     Can force use of bisection instead of faster DQDS.
*     Option left in the code for future multisection work.
      FORCEB = .FALSE.

      IF( (IRANGE.EQ.ALLRNG) .AND. (.NOT. FORCEB) ) THEN
*        Set interval [VL,VU] that contains all eigenvalues
         VL = GL
         VU = GU
      ELSE
*        We call DLARRD to find crude approximations to the eigenvalues
*        in the desired range. In case IRANGE = INDRNG, we also obtain the
*        interval (VL,VU] that contains all the wanted eigenvalues.
*        An interval [LEFT,RIGHT] has converged if
*        RIGHT-LEFT.LT.RTOL*MAX(ABS(LEFT),ABS(RIGHT))
*        DLARRD needs a WORK of size 4*N, IWORK of size 3*N
         CALL DLARRD( RANGE, 'B', N, VL, VU, IL, IU, GERS,
     $                    BSRTOL, D, E, E2, PIVMIN, NSPLIT, ISPLIT,
     $                    MM, W, WERR, VL, VU, IBLOCK, INDEXW,
     $                    WORK, IWORK, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = -1
            RETURN
         ENDIF
*        Make sure that the entries M+1 to N in W, WERR, IBLOCK, INDEXW are 0
         DO 14 I = MM+1,N
            W( I ) = ZERO
            WERR( I ) = ZERO
            IBLOCK( I ) = 0
            INDEXW( I ) = 0
 14      CONTINUE
      END IF


***
*     Loop over unreduced blocks
      IBEGIN = 1
      WBEGIN = 1
      DO 170 JBLK = 1, NSPLIT
         IEND = ISPLIT( JBLK )
         IN = IEND - IBEGIN + 1

*        1 X 1 block
         IF( IN.EQ.1 ) THEN
            IF( (IRANGE.EQ.ALLRNG).OR.( (IRANGE.EQ.VALRNG).AND.
     $         ( D( IBEGIN ).GT.VL ).AND.( D( IBEGIN ).LE.VU ) )
     $        .OR. ( (IRANGE.EQ.INDRNG).AND.(IBLOCK(WBEGIN).EQ.JBLK))
     $        ) THEN
               M = M + 1
               W( M ) = D( IBEGIN )
               WERR(M) = ZERO
*              The gap for a single block doesn't matter for the later
*              algorithm and is assigned an arbitrary large value
               WGAP(M) = ZERO
               IBLOCK( M ) = JBLK
               INDEXW( M ) = 1
               WBEGIN = WBEGIN + 1
            ENDIF
*           E( IEND ) holds the shift for the initial RRR
            E( IEND ) = ZERO
            IBEGIN = IEND + 1
            GO TO 170
         END IF
*
*        Blocks of size larger than 1x1
*
*        E( IEND ) will hold the shift for the initial RRR, for now set it =0
         E( IEND ) = ZERO
*
*        Find local outer bounds GL,GU for the block
         GL = D(IBEGIN)
         GU = D(IBEGIN)
         DO 15 I = IBEGIN , IEND
            GL = MIN( GERS( 2*I-1 ), GL )
            GU = MAX( GERS( 2*I ), GU )
 15      CONTINUE
         SPDIAM = GU - GL

         IF(.NOT. ((IRANGE.EQ.ALLRNG).AND.(.NOT.FORCEB)) ) THEN
*           Count the number of eigenvalues in the current block.
            MB = 0
            DO 20 I = WBEGIN,MM
               IF( IBLOCK(I).EQ.JBLK ) THEN
                  MB = MB+1
               ELSE
                  GOTO 21
               ENDIF
 20         CONTINUE
 21         CONTINUE

            IF( MB.EQ.0) THEN
*              No eigenvalue in the current block lies in the desired range
*              E( IEND ) holds the shift for the initial RRR
               E( IEND ) = ZERO
               IBEGIN = IEND + 1
               GO TO 170
            ELSE

*              Decide whether dqds or bisection is more efficient
               USEDQD = ( (MB .GT. FAC*IN) .AND. (.NOT.FORCEB) )
               WEND = WBEGIN + MB - 1
*              Calculate gaps for the current block
*              In later stages, when representations for individual
*              eigenvalues are different, we use SIGMA = E( IEND ).
               SIGMA = ZERO
               DO 30 I = WBEGIN, WEND - 1
                  WGAP( I ) = MAX( ZERO,
     $                        W(I+1)-WERR(I+1) - (W(I)+WERR(I)) )
 30            CONTINUE
               WGAP( WEND ) = MAX( ZERO,
     $                     VU - SIGMA - (W( WEND )+WERR( WEND )))
*              Find local index of the first and last desired evalue.
               INDL = INDEXW(WBEGIN)
               INDU = INDEXW( WEND )
            ENDIF
         ENDIF
         IF(( (IRANGE.EQ.ALLRNG) .AND. (.NOT. FORCEB) ).OR.USEDQD) THEN
*           Case of DQDS
*           Find approximations to the extremal eigenvalues of the block
            CALL DLARRK( IN, 1, GL, GU, D(IBEGIN),
     $               E2(IBEGIN), PIVMIN, RTL, TMP, TMP1, IINFO )
            IF( IINFO.NE.0 ) THEN
               INFO = -1
               RETURN
            ENDIF
            ISLEFT = MAX(GL, TMP - TMP1
     $               - HNDRD * EPS* ABS(TMP - TMP1))

            CALL DLARRK( IN, IN, GL, GU, D(IBEGIN),
     $               E2(IBEGIN), PIVMIN, RTL, TMP, TMP1, IINFO )
            IF( IINFO.NE.0 ) THEN
               INFO = -1
               RETURN
            ENDIF
            ISRGHT = MIN(GU, TMP + TMP1
     $                 + HNDRD * EPS * ABS(TMP + TMP1))
*           Improve the estimate of the spectral diameter
            SPDIAM = ISRGHT - ISLEFT
         ELSE
*           Case of bisection
*           Find approximations to the wanted extremal eigenvalues
            ISLEFT = MAX(GL, W(WBEGIN) - WERR(WBEGIN)
     $                  - HNDRD * EPS*ABS(W(WBEGIN)- WERR(WBEGIN) ))
            ISRGHT = MIN(GU,W(WEND) + WERR(WEND)
     $                  + HNDRD * EPS * ABS(W(WEND)+ WERR(WEND)))
         ENDIF


*        Decide whether the base representation for the current block
*        L_JBLK D_JBLK L_JBLK^T = T_JBLK - sigma_JBLK I
*        should be on the left or the right end of the current block.
*        The strategy is to shift to the end which is "more populated"
*        Furthermore, decide whether to use DQDS for the computation of
*        the eigenvalue approximations at the end of DLARRE or bisection.
*        dqds is chosen if all eigenvalues are desired or the number of
*        eigenvalues to be computed is large compared to the blocksize.
         IF( ( IRANGE.EQ.ALLRNG ) .AND. (.NOT.FORCEB) ) THEN
*           If all the eigenvalues have to be computed, we use dqd
            USEDQD = .TRUE.
*           INDL is the local index of the first eigenvalue to compute
            INDL = 1
            INDU = IN
*           MB =  number of eigenvalues to compute
            MB = IN
            WEND = WBEGIN + MB - 1
*           Define 1/4 and 3/4 points of the spectrum
            S1 = ISLEFT + FOURTH * SPDIAM
            S2 = ISRGHT - FOURTH * SPDIAM
         ELSE
*           DLARRD has computed IBLOCK and INDEXW for each eigenvalue
*           approximation.
*           choose sigma
            IF( USEDQD ) THEN
               S1 = ISLEFT + FOURTH * SPDIAM
               S2 = ISRGHT - FOURTH * SPDIAM
            ELSE
               TMP = MIN(ISRGHT,VU) -  MAX(ISLEFT,VL)
               S1 =  MAX(ISLEFT,VL) + FOURTH * TMP
               S2 =  MIN(ISRGHT,VU) - FOURTH * TMP
            ENDIF
         ENDIF

*        Compute the negcount at the 1/4 and 3/4 points
         IF(MB.GT.1) THEN
            CALL DLARRC( 'T', IN, S1, S2, D(IBEGIN),
     $                    E(IBEGIN), PIVMIN, CNT, CNT1, CNT2, IINFO)
         ENDIF

         IF(MB.EQ.1) THEN
            SIGMA = GL
            SGNDEF = ONE
         ELSEIF( CNT1 - INDL .GE. INDU - CNT2 ) THEN
            IF( ( IRANGE.EQ.ALLRNG ) .AND. (.NOT.FORCEB) ) THEN
               SIGMA = MAX(ISLEFT,GL)
            ELSEIF( USEDQD ) THEN
*              use Gerschgorin bound as shift to get pos def matrix
*              for dqds
               SIGMA = ISLEFT
            ELSE
*              use approximation of the first desired eigenvalue of the
*              block as shift
               SIGMA = MAX(ISLEFT,VL)
            ENDIF
            SGNDEF = ONE
         ELSE
            IF( ( IRANGE.EQ.ALLRNG ) .AND. (.NOT.FORCEB) ) THEN
               SIGMA = MIN(ISRGHT,GU)
            ELSEIF( USEDQD ) THEN
*              use Gerschgorin bound as shift to get neg def matrix
*              for dqds
               SIGMA = ISRGHT
            ELSE
*              use approximation of the first desired eigenvalue of the
*              block as shift
               SIGMA = MIN(ISRGHT,VU)
            ENDIF
            SGNDEF = -ONE
         ENDIF


*        An initial SIGMA has been chosen that will be used for computing
*        T - SIGMA I = L D L^T
*        Define the increment TAU of the shift in case the initial shift
*        needs to be refined to obtain a factorization with not too much
*        element growth.
         IF( USEDQD ) THEN
*           The initial SIGMA was to the outer end of the spectrum
*           the matrix is definite and we need not retreat.
            TAU = SPDIAM*EPS*N + TWO*PIVMIN
         ELSE
            IF(MB.GT.1) THEN
               CLWDTH = W(WEND) + WERR(WEND) - W(WBEGIN) - WERR(WBEGIN)
               AVGAP = ABS(CLWDTH / DBLE(WEND-WBEGIN))
               IF( SGNDEF.EQ.ONE ) THEN
                  TAU = HALF*MAX(WGAP(WBEGIN),AVGAP)
                  TAU = MAX(TAU,WERR(WBEGIN))
               ELSE
                  TAU = HALF*MAX(WGAP(WEND-1),AVGAP)
                  TAU = MAX(TAU,WERR(WEND))
               ENDIF
            ELSE
               TAU = WERR(WBEGIN)
            ENDIF
         ENDIF
*
         DO 80 IDUM = 1, MAXTRY
*           Compute L D L^T factorization of tridiagonal matrix T - sigma I.
*           Store D in WORK(1:IN), L in WORK(IN+1:2*IN), and reciprocals of
*           pivots in WORK(2*IN+1:3*IN)
            DPIVOT = D( IBEGIN ) - SIGMA
            WORK( 1 ) = DPIVOT
            DMAX = ABS( WORK(1) )
            J = IBEGIN
            DO 70 I = 1, IN - 1
               WORK( 2*IN+I ) = ONE / WORK( I )
               TMP = E( J )*WORK( 2*IN+I )
               WORK( IN+I ) = TMP
               DPIVOT = ( D( J+1 )-SIGMA ) - TMP*E( J )
               WORK( I+1 ) = DPIVOT
               DMAX = MAX( DMAX, ABS(DPIVOT) )
               J = J + 1
 70         CONTINUE
*           check for element growth
            IF( DMAX .GT. MAXGROWTH*SPDIAM ) THEN
               NOREP = .TRUE.
            ELSE
               NOREP = .FALSE.
            ENDIF
            IF( USEDQD .AND. .NOT.NOREP ) THEN
*              Ensure the definiteness of the representation
*              All entries of D (of L D L^T) must have the same sign
               DO 71 I = 1, IN
                  TMP = SGNDEF*WORK( I )
                  IF( TMP.LT.ZERO ) NOREP = .TRUE.
 71            CONTINUE
            ENDIF
            IF(NOREP) THEN
*              Note that in the case of IRANGE=ALLRNG, we use the Gerschgorin
*              shift which makes the matrix definite. So we should end up
*              here really only in the case of IRANGE = VALRNG or INDRNG.
               IF( IDUM.EQ.MAXTRY-1 ) THEN
                  IF( SGNDEF.EQ.ONE ) THEN
*                    The fudged Gerschgorin shift should succeed
                     SIGMA =
     $                    GL - FUDGE*SPDIAM*EPS*N - FUDGE*TWO*PIVMIN
                  ELSE
                     SIGMA =
     $                    GU + FUDGE*SPDIAM*EPS*N + FUDGE*TWO*PIVMIN
                  END IF
               ELSE
                  SIGMA = SIGMA - SGNDEF * TAU
                  TAU = TWO * TAU
               END IF
            ELSE
*              an initial RRR is found
               GO TO 83
            END IF
 80      CONTINUE
*        if the program reaches this point, no base representation could be
*        found in MAXTRY iterations.
         INFO = 2
         RETURN

 83      CONTINUE
*        At this point, we have found an initial base representation
*        T - SIGMA I = L D L^T with not too much element growth.
*        Store the shift.
         E( IEND ) = SIGMA
*        Store D and L.
         CALL DCOPY( IN, WORK, 1, D( IBEGIN ), 1 )
         CALL DCOPY( IN-1, WORK( IN+1 ), 1, E( IBEGIN ), 1 )


         IF(MB.GT.1 ) THEN
*
*           Perturb each entry of the base representation by a small
*           (but random) relative amount to overcome difficulties with
*           glued matrices.
*
            DO 122 I = 1, 4
               ISEED( I ) = 1
 122        CONTINUE

            CALL DLARNV(2, ISEED, 2*IN-1, WORK(1))
            DO 125 I = 1,IN-1
               D(IBEGIN+I-1) = D(IBEGIN+I-1)*(ONE+EPS*PERT*WORK(I))
               E(IBEGIN+I-1) = E(IBEGIN+I-1)*(ONE+EPS*PERT*WORK(IN+I))
 125        CONTINUE
            D(IEND) = D(IEND)*(ONE+EPS*FOUR*WORK(IN))
*
         ENDIF
*
*        Don't update the Gerschgorin intervals because keeping track
*        of the updates would be too much work in DLARRV.
*        We update W instead and use it to locate the proper Gerschgorin
*        intervals.

*        Compute the required eigenvalues of L D L' by bisection or dqds
         IF ( .NOT.USEDQD ) THEN
*           If DLARRD has been used, shift the eigenvalue approximations
*           according to their representation. This is necessary for
*           a uniform DLARRV since dqds computes eigenvalues of the
*           shifted representation. In DLARRV, W will always hold the
*           UNshifted eigenvalue approximation.
            DO 134 J=WBEGIN,WEND
               W(J) = W(J) - SIGMA
               WERR(J) = WERR(J) + ABS(W(J)) * EPS
 134        CONTINUE
*           call DLARRB to reduce eigenvalue error of the approximations
*           from DLARRD
            DO 135 I = IBEGIN, IEND-1
               WORK( I ) = D( I ) * E( I )**2
 135        CONTINUE
*           use bisection to find EV from INDL to INDU
            CALL DLARRB(IN, D(IBEGIN), WORK(IBEGIN),
     $                  INDL, INDU, RTOL1, RTOL2, INDL-1,
     $                  W(WBEGIN), WGAP(WBEGIN), WERR(WBEGIN),
     $                  WORK( 2*N+1 ), IWORK, PIVMIN, SPDIAM,
     $                  IN, IINFO )
            IF( IINFO .NE. 0 ) THEN
               INFO = -4
               RETURN
            END IF
*           DLARRB computes all gaps correctly except for the last one
*           Record distance to VU/GU
            WGAP( WEND ) = MAX( ZERO,
     $           ( VU-SIGMA ) - ( W( WEND ) + WERR( WEND ) ) )
            DO 138 I = INDL, INDU
               M = M + 1
               IBLOCK(M) = JBLK
               INDEXW(M) = I
 138        CONTINUE
         ELSE
*           Call dqds to get all eigs (and then possibly delete unwanted
*           eigenvalues).
*           Note that dqds finds the eigenvalues of the L D L^T representation
*           of T to high relative accuracy. High relative accuracy
*           might be lost when the shift of the RRR is subtracted to obtain
*           the eigenvalues of T. However, T is not guaranteed to define its
*           eigenvalues to high relative accuracy anyway.
*           Set RTOL to the order of the tolerance used in DLASQ2
*           This is an ESTIMATED error, the worst case bound is 4*N*EPS
*           which is usually too large and requires unnecessary work to be
*           done by bisection when computing the eigenvectors
            RTOL = LOG(DBLE(IN)) * FOUR * EPS
            J = IBEGIN
            DO 140 I = 1, IN - 1
               WORK( 2*I-1 ) = ABS( D( J ) )
               WORK( 2*I ) = E( J )*E( J )*WORK( 2*I-1 )
               J = J + 1
  140       CONTINUE
            WORK( 2*IN-1 ) = ABS( D( IEND ) )
            WORK( 2*IN ) = ZERO
            CALL DLASQ2( IN, WORK, IINFO )
            IF( IINFO .NE. 0 ) THEN
*              If IINFO = -5 then an index is part of a tight cluster
*              and should be changed. The index is in IWORK(1) and the
*              gap is in WORK(N+1)
               INFO = -5
               RETURN
            ELSE
*              Test that all eigenvalues are positive as expected
               DO 149 I = 1, IN
                  IF( WORK( I ).LT.ZERO ) THEN
                     INFO = -6
                     RETURN
                  ENDIF
 149           CONTINUE
            END IF
            IF( SGNDEF.GT.ZERO ) THEN
               DO 150 I = INDL, INDU
                  M = M + 1
                  W( M ) = WORK( IN-I+1 )
                  IBLOCK( M ) = JBLK
                  INDEXW( M ) = I
 150           CONTINUE
            ELSE
               DO 160 I = INDL, INDU
                  M = M + 1
                  W( M ) = -WORK( I )
                  IBLOCK( M ) = JBLK
                  INDEXW( M ) = I
 160           CONTINUE
            END IF

            DO 165 I = M - MB + 1, M
*              the value of RTOL below should be the tolerance in DLASQ2
               WERR( I ) = RTOL * ABS( W(I) )
 165        CONTINUE
            DO 166 I = M - MB + 1, M - 1
*              compute the right gap between the intervals
               WGAP( I ) = MAX( ZERO,
     $                          W(I+1)-WERR(I+1) - (W(I)+WERR(I)) )
 166        CONTINUE
            WGAP( M ) = MAX( ZERO,
     $           ( VU-SIGMA ) - ( W( M ) + WERR( M ) ) )
         END IF
*        proceed with next block
         IBEGIN = IEND + 1
         WBEGIN = WEND + 1
 170  CONTINUE
*

      RETURN
*
*     end of DLARRE
*
      END
      SUBROUTINE DLARRF( N, D, L, LD, CLSTRT, CLEND,
     $                   W, WGAP, WERR,
     $                   SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA,
     $                   DPLUS, LPLUS, WORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
**
*     .. Scalar Arguments ..
      INTEGER            CLSTRT, CLEND, INFO, N
      DOUBLE PRECISION   CLGAPL, CLGAPR, PIVMIN, SIGMA, SPDIAM
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DPLUS( * ), L( * ), LD( * ),
     $          LPLUS( * ), W( * ), WGAP( * ), WERR( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  Given the initial representation L D L^T and its cluster of close
*  eigenvalues (in a relative measure), W( CLSTRT ), W( CLSTRT+1 ), ...
*  W( CLEND ), DLARRF finds a new relatively robust representation
*  L D L^T - SIGMA I = L(+) D(+) L(+)^T such that at least one of the
*  eigenvalues of L(+) D(+) L(+)^T is relatively isolated.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix (subblock, if the matrix splitted).
*
*  D       (input) DOUBLE PRECISION array, dimension (N)
*          The N diagonal elements of the diagonal matrix D.
*
*  L       (input) DOUBLE PRECISION array, dimension (N-1)
*          The (N-1) subdiagonal elements of the unit bidiagonal
*          matrix L.
*
*  LD      (input) DOUBLE PRECISION array, dimension (N-1)
*          The (N-1) elements L(i)*D(i).
*
*  CLSTRT  (input) INTEGER
*          The index of the first eigenvalue in the cluster.
*
*  CLEND   (input) INTEGER
*          The index of the last eigenvalue in the cluster.
*
*  W       (input) DOUBLE PRECISION array, dimension >=  (CLEND-CLSTRT+1)
*          The eigenvalue APPROXIMATIONS of L D L^T in ascending order.
*          W( CLSTRT ) through W( CLEND ) form the cluster of relatively
*          close eigenalues.
*
*  WGAP    (input/output) DOUBLE PRECISION array, dimension >=  (CLEND-CLSTRT+1)
*          The separation from the right neighbor eigenvalue in W.
*
*  WERR    (input) DOUBLE PRECISION array, dimension >=  (CLEND-CLSTRT+1)
*          WERR contain the semiwidth of the uncertainty
*          interval of the corresponding eigenvalue APPROXIMATION in W
*
*  SPDIAM (input) estimate of the spectral diameter obtained from the
*          Gerschgorin intervals
*
*  CLGAPL, CLGAPR (input) absolute gap on each end of the cluster.
*          Set by the calling routine to protect against shifts too close
*          to eigenvalues outside the cluster.
*
*  PIVMIN  (input) DOUBLE PRECISION
*          The minimum pivot allowed in the Sturm sequence.
*
*  SIGMA   (output) DOUBLE PRECISION
*          The shift used to form L(+) D(+) L(+)^T.
*
*  DPLUS   (output) DOUBLE PRECISION array, dimension (N)
*          The N diagonal elements of the diagonal matrix D(+).
*
*  LPLUS   (output) DOUBLE PRECISION array, dimension (N-1)
*          The first (N-1) elements of LPLUS contain the subdiagonal
*          elements of the unit bidiagonal matrix L(+).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
*          Workspace.
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
      DOUBLE PRECISION   FOUR, MAXGROWTH1, MAXGROWTH2, ONE, QUART, TWO,
     $                   ZERO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0,
     $                     FOUR = 4.0D0, QUART = 0.25D0,
     $                     MAXGROWTH1 = 8.D0,
     $                     MAXGROWTH2 = 8.D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL   DORRR1, FORCER, NOFAIL, SAWNAN1, SAWNAN2, TRYRRR1
      INTEGER            I, INDX, KTRY, KTRYMAX, SLEFT, SRIGHT, SHIFT
      PARAMETER          ( KTRYMAX = 1, SLEFT = 1, SRIGHT = 2 )
      DOUBLE PRECISION   AVGAP, BESTSHIFT, CLWDTH, EPS, FACT, FAIL,
     $                   FAIL2, GROWTHBOUND, LDELTA, LDMAX, LSIGMA,
     $                   MAX1, MAX2, MINGAP, OLDP, PROD, RDELTA, RDMAX,
     $                   RRR1, RRR2, RSIGMA, S, SMLGROWTH, TMP, ZNM2
*     ..
*     .. External Functions ..
      LOGICAL DISNAN
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DISNAN, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      FACT = DBLE(2**KTRYMAX)
      EPS = DLAMCH( 'Precision' )
      SHIFT = 0
      FORCER = .FALSE.


*     Note that we cannot guarantee that for any of the shifts tried,
*     the factorization has a small or even moderate element growth.
*     There could be Ritz values at both ends of the cluster and despite
*     backing off, there are examples where all factorizations tried
*     (in IEEE mode, allowing zero pivots & infinities) have INFINITE
*     element growth.
*     For this reason, we should use PIVMIN in this subroutine so that at
*     least the L D L^T factorization exists. It can be checked afterwards
*     whether the element growth caused bad residuals/orthogonality.

*     Decide whether the code should accept the best among all
*     representations despite large element growth or signal INFO=1
      NOFAIL = .TRUE.
*

*     Compute the average gap length of the cluster
      CLWDTH = ABS(W(CLEND)-W(CLSTRT)) + WERR(CLEND) + WERR(CLSTRT)
      AVGAP = CLWDTH / DBLE(CLEND-CLSTRT)
      MINGAP = MIN(CLGAPL, CLGAPR)
*     Initial values for shifts to both ends of cluster
      LSIGMA = MIN(W( CLSTRT ),W( CLEND )) - WERR( CLSTRT )
      RSIGMA = MAX(W( CLSTRT ),W( CLEND )) + WERR( CLEND )

*     Use a small fudge to make sure that we really shift to the outside
      LSIGMA = LSIGMA - ABS(LSIGMA)* FOUR * EPS
      RSIGMA = RSIGMA + ABS(RSIGMA)* FOUR * EPS

*     Compute upper bounds for how much to back off the initial shifts
      LDMAX = QUART * MINGAP + TWO * PIVMIN
      RDMAX = QUART * MINGAP + TWO * PIVMIN

      LDELTA = MAX(AVGAP,WGAP( CLSTRT ))/FACT
      RDELTA = MAX(AVGAP,WGAP( CLEND-1 ))/FACT
*
*     Initialize the record of the best representation found
*
      S = DLAMCH( 'S' )
      SMLGROWTH = ONE / S
      FAIL = DBLE(N-1)*MINGAP/(SPDIAM*EPS)
      FAIL2 = DBLE(N-1)*MINGAP/(SPDIAM*SQRT(EPS))
      BESTSHIFT = LSIGMA
*
*     while (KTRY <= KTRYMAX)
      KTRY = 0
      GROWTHBOUND = MAXGROWTH1*SPDIAM

 5    CONTINUE
      SAWNAN1 = .FALSE.
      SAWNAN2 = .FALSE.
*     Ensure that we do not back off too much of the initial shifts
      LDELTA = MIN(LDMAX,LDELTA)
      RDELTA = MIN(RDMAX,RDELTA)

*     Compute the element growth when shifting to both ends of the cluster
*     accept the shift if there is no element growth at one of the two ends

*     Left end
      S = -LSIGMA
      DPLUS( 1 ) = D( 1 ) + S
      IF(ABS(DPLUS(1)).LT.PIVMIN) THEN
         DPLUS(1) = -PIVMIN
*        Need to set SAWNAN1 because refined RRR test should not be used
*        in this case
         SAWNAN1 = .TRUE.
      ENDIF
      MAX1 = ABS( DPLUS( 1 ) )
      DO 6 I = 1, N - 1
         LPLUS( I ) = LD( I ) / DPLUS( I )
         S = S*LPLUS( I )*L( I ) - LSIGMA
         DPLUS( I+1 ) = D( I+1 ) + S
         IF(ABS(DPLUS(I+1)).LT.PIVMIN) THEN
            DPLUS(I+1) = -PIVMIN
*           Need to set SAWNAN1 because refined RRR test should not be used
*           in this case
            SAWNAN1 = .TRUE.
         ENDIF
         MAX1 = MAX( MAX1,ABS(DPLUS(I+1)) )
 6    CONTINUE
      SAWNAN1 = SAWNAN1 .OR.  DISNAN( MAX1 )

      IF( FORCER .OR.
     $   (MAX1.LE.GROWTHBOUND .AND. .NOT.SAWNAN1 ) ) THEN
         SIGMA = LSIGMA
         SHIFT = SLEFT
         GOTO 100
      ENDIF

*     Right end
      S = -RSIGMA
      WORK( 1 ) = D( 1 ) + S
      IF(ABS(WORK(1)).LT.PIVMIN) THEN
         WORK(1) = -PIVMIN
*        Need to set SAWNAN2 because refined RRR test should not be used
*        in this case
         SAWNAN2 = .TRUE.
      ENDIF
      MAX2 = ABS( WORK( 1 ) )
      DO 7 I = 1, N - 1
         WORK( N+I ) = LD( I ) / WORK( I )
         S = S*WORK( N+I )*L( I ) - RSIGMA
         WORK( I+1 ) = D( I+1 ) + S
         IF(ABS(WORK(I+1)).LT.PIVMIN) THEN
            WORK(I+1) = -PIVMIN
*           Need to set SAWNAN2 because refined RRR test should not be used
*           in this case
            SAWNAN2 = .TRUE.
         ENDIF
         MAX2 = MAX( MAX2,ABS(WORK(I+1)) )
 7    CONTINUE
      SAWNAN2 = SAWNAN2 .OR.  DISNAN( MAX2 )

      IF( FORCER .OR.
     $   (MAX2.LE.GROWTHBOUND .AND. .NOT.SAWNAN2 ) ) THEN
         SIGMA = RSIGMA
         SHIFT = SRIGHT
         GOTO 100
      ENDIF
*     If we are at this point, both shifts led to too much element growth

*     Record the better of the two shifts (provided it didn't lead to NaN)
      IF(SAWNAN1.AND.SAWNAN2) THEN
*        both MAX1 and MAX2 are NaN
         GOTO 50
      ELSE
         IF( .NOT.SAWNAN1 ) THEN
            INDX = 1
            IF(MAX1.LE.SMLGROWTH) THEN
               SMLGROWTH = MAX1
               BESTSHIFT = LSIGMA
            ENDIF
         ENDIF
         IF( .NOT.SAWNAN2 ) THEN
            IF(SAWNAN1 .OR. MAX2.LE.MAX1) INDX = 2
            IF(MAX2.LE.SMLGROWTH) THEN
               SMLGROWTH = MAX2
               BESTSHIFT = RSIGMA
            ENDIF
         ENDIF
      ENDIF

*     If we are here, both the left and the right shift led to
*     element growth. If the element growth is moderate, then
*     we may still accept the representation, if it passes a
*     refined test for RRR. This test supposes that no NaN occurred.
*     Moreover, we use the refined RRR test only for isolated clusters.
      IF((CLWDTH.LT.MINGAP/DBLE(128)) .AND.
     $   (MIN(MAX1,MAX2).LT.FAIL2)
     $  .AND.(.NOT.SAWNAN1).AND.(.NOT.SAWNAN2)) THEN
         DORRR1 = .TRUE.
      ELSE
         DORRR1 = .FALSE.
      ENDIF
      TRYRRR1 = .TRUE.
      IF( TRYRRR1 .AND. DORRR1 ) THEN
      IF(INDX.EQ.1) THEN
         TMP = ABS( DPLUS( N ) )
         ZNM2 = ONE
         PROD = ONE
         OLDP = ONE
         DO 15 I = N-1, 1, -1
            IF( PROD .LE. EPS ) THEN
               PROD =
     $         ((DPLUS(I+1)*WORK(N+I+1))/(DPLUS(I)*WORK(N+I)))*OLDP
            ELSE
               PROD = PROD*ABS(WORK(N+I))
            END IF
            OLDP = PROD
            ZNM2 = ZNM2 + PROD**2
            TMP = MAX( TMP, ABS( DPLUS( I ) * PROD ))
 15      CONTINUE
         RRR1 = TMP/( SPDIAM * SQRT( ZNM2 ) )
         IF (RRR1.LE.MAXGROWTH2) THEN
            SIGMA = LSIGMA
            SHIFT = SLEFT
            GOTO 100
         ENDIF
      ELSE IF(INDX.EQ.2) THEN
         TMP = ABS( WORK( N ) )
         ZNM2 = ONE
         PROD = ONE
         OLDP = ONE
         DO 16 I = N-1, 1, -1
            IF( PROD .LE. EPS ) THEN
               PROD = ((WORK(I+1)*LPLUS(I+1))/(WORK(I)*LPLUS(I)))*OLDP
            ELSE
               PROD = PROD*ABS(LPLUS(I))
            END IF
            OLDP = PROD
            ZNM2 = ZNM2 + PROD**2
            TMP = MAX( TMP, ABS( WORK( I ) * PROD ))
 16      CONTINUE
         RRR2 = TMP/( SPDIAM * SQRT( ZNM2 ) )
         IF (RRR2.LE.MAXGROWTH2) THEN
            SIGMA = RSIGMA
            SHIFT = SRIGHT
            GOTO 100
         ENDIF
      END IF
      ENDIF

 50   CONTINUE

      IF (KTRY.LT.KTRYMAX) THEN
*        If we are here, both shifts failed also the RRR test.
*        Back off to the outside
         LSIGMA = MAX( LSIGMA - LDELTA,
     $     LSIGMA - LDMAX)
         RSIGMA = MIN( RSIGMA + RDELTA,
     $     RSIGMA + RDMAX )
         LDELTA = TWO * LDELTA
         RDELTA = TWO * RDELTA
         KTRY = KTRY + 1
         GOTO 5
      ELSE
*        None of the representations investigated satisfied our
*        criteria. Take the best one we found.
         IF((SMLGROWTH.LT.FAIL).OR.NOFAIL) THEN
            LSIGMA = BESTSHIFT
            RSIGMA = BESTSHIFT
            FORCER = .TRUE.
            GOTO 5
         ELSE
            INFO = 1
            RETURN
         ENDIF
      END IF

 100  CONTINUE
      IF (SHIFT.EQ.SLEFT) THEN
      ELSEIF (SHIFT.EQ.SRIGHT) THEN
*        store new L and D back into DPLUS, LPLUS
         CALL DCOPY( N, WORK, 1, DPLUS, 1 )
         CALL DCOPY( N-1, WORK(N+1), 1, LPLUS, 1 )
      ENDIF

      RETURN
*
*     End of DLARRF
*
      END
      SUBROUTINE DLARRV( N, VL, VU, D, L, PIVMIN,
     $                   ISPLIT, M, DOL, DOU, MINRGP,
     $                   RTOL1, RTOL2, W, WERR, WGAP,
     $                   IBLOCK, INDEXW, GERS, Z, LDZ, ISUPPZ,
     $                   WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            DOL, DOU, INFO, LDZ, M, N
      DOUBLE PRECISION   MINRGP, PIVMIN, RTOL1, RTOL2, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IBLOCK( * ), INDEXW( * ), ISPLIT( * ),
     $                   ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), GERS( * ), L( * ), W( * ), WERR( * ),
     $                   WGAP( * ), WORK( * )
      DOUBLE PRECISION  Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DLARRV computes the eigenvectors of the tridiagonal matrix
*  T = L D L^T given L, D and APPROXIMATIONS to the eigenvalues of L D L^T.
*  The input eigenvalues should have been computed by DLARRE.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  VL      (input) DOUBLE PRECISION
*  VU      (input) DOUBLE PRECISION
*          Lower and upper bounds of the interval that contains the desired
*          eigenvalues. VL < VU. Needed to compute gaps on the left or right
*          end of the extremal eigenvalues in the desired RANGE.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the N diagonal elements of the diagonal matrix D.
*          On exit, D may be overwritten.
*
*  L       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the (N-1) subdiagonal elements of the unit
*          bidiagonal matrix L are in elements 1 to N-1 of L
*          (if the matrix is not splitted.) At the end of each block
*          is stored the corresponding shift as given by DLARRE.
*          On exit, L is overwritten.
*
*  PIVMIN  (in) DOUBLE PRECISION
*          The minimum pivot allowed in the Sturm sequence.
*
*  ISPLIT  (input) INTEGER array, dimension (N)
*          The splitting points, at which T breaks up into blocks.
*          The first block consists of rows/columns 1 to
*          ISPLIT( 1 ), the second of rows/columns ISPLIT( 1 )+1
*          through ISPLIT( 2 ), etc.
*
*  M       (input) INTEGER
*          The total number of input eigenvalues.  0 <= M <= N.
*
*  DOL     (input) INTEGER
*  DOU     (input) INTEGER
*          If the user wants to compute only selected eigenvectors from all
*          the eigenvalues supplied, he can specify an index range DOL:DOU.
*          Or else the setting DOL=1, DOU=M should be applied.
*          Note that DOL and DOU refer to the order in which the eigenvalues
*          are stored in W.
*          If the user wants to compute only selected eigenpairs, then
*          the columns DOL-1 to DOU+1 of the eigenvector space Z contain the
*          computed eigenvectors. All other columns of Z are set to zero.
*
*  MINRGP  (input) DOUBLE PRECISION
*
*  RTOL1   (input) DOUBLE PRECISION
*  RTOL2   (input) DOUBLE PRECISION
*           Parameters for bisection.
*           An interval [LEFT,RIGHT] has converged if
*           RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )
*
*  W       (input/output) DOUBLE PRECISION array, dimension (N)
*          The first M elements of W contain the APPROXIMATE eigenvalues for
*          which eigenvectors are to be computed.  The eigenvalues
*          should be grouped by split-off block and ordered from
*          smallest to largest within the block ( The output array
*          W from DLARRE is expected here ). Furthermore, they are with
*          respect to the shift of the corresponding root representation
*          for their block. On exit, W holds the eigenvalues of the
*          UNshifted matrix.
*
*  WERR    (input/output) DOUBLE PRECISION array, dimension (N)
*          The first M elements contain the semiwidth of the uncertainty
*          interval of the corresponding eigenvalue in W
*
*  WGAP    (input/output) DOUBLE PRECISION array, dimension (N)
*          The separation from the right neighbor eigenvalue in W.
*
*  IBLOCK  (input) INTEGER array, dimension (N)
*          The indices of the blocks (submatrices) associated with the
*          corresponding eigenvalues in W; IBLOCK(i)=1 if eigenvalue
*          W(i) belongs to the first block from the top, =2 if W(i)
*          belongs to the second block, etc.
*
*  INDEXW  (input) INTEGER array, dimension (N)
*          The indices of the eigenvalues within each block (submatrix);
*          for example, INDEXW(i)= 10 and IBLOCK(i)=2 imply that the
*          i-th eigenvalue W(i) is the 10-th eigenvalue in the second block.
*
*  GERS    (input) DOUBLE PRECISION array, dimension (2*N)
*          The N Gerschgorin intervals (the i-th Gerschgorin interval
*          is (GERS(2*i-1), GERS(2*i)). The Gerschgorin intervals should
*          be computed from the original UNshifted matrix.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )
*          If INFO = 0, the first M columns of Z contain the
*          orthonormal eigenvectors of the matrix T
*          corresponding to the input eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  ISUPPZ  (output) INTEGER array, dimension ( 2*max(1,M) )
*          The support of the eigenvectors in Z, i.e., the indices
*          indicating the nonzero elements in Z. The I-th eigenvector
*          is nonzero only in elements ISUPPZ( 2*I-1 ) through
*          ISUPPZ( 2*I ).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (12*N)
*
*  IWORK   (workspace) INTEGER array, dimension (7*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*
*          > 0:  A problem occured in DLARRV.
*          < 0:  One of the called subroutines signaled an internal problem.
*                Needs inspection of the corresponding parameter IINFO
*                for further information.
*
*          =-1:  Problem in DLARRB when refining a child's eigenvalues.
*          =-2:  Problem in DLARRF when computing the RRR of a child.
*                When a child is inside a tight cluster, it can be difficult
*                to find an RRR. A partial remedy from the user's point of
*                view is to make the parameter MINRGP smaller and recompile.
*                However, as the orthogonality of the computed vectors is
*                proportional to 1/MINRGP, the user should be aware that
*                he might be trading in precision when he decreases MINRGP.
*          =-3:  Problem in DLARRB when refining a single eigenvalue
*                after the Rayleigh correction was rejected.
*          = 5:  The Rayleigh Quotient Iteration failed to converge to
*                full accuracy in MAXITR steps.
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
      INTEGER            MAXITR
      PARAMETER          ( MAXITR = 10 )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR, HALF
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0,
     $                     TWO = 2.0D0, THREE = 3.0D0,
     $                     FOUR = 4.0D0, HALF = 0.5D0)
*     ..
*     .. Local Scalars ..
      LOGICAL            ESKIP, NEEDBS, STP2II, TRYRQC, USEDBS, USEDRQ
      INTEGER            DONE, I, IBEGIN, IDONE, IEND, II, IINDC1,
     $                   IINDC2, IINDR, IINDWK, IINFO, IM, IN, INDEIG,
     $                   INDLD, INDLLD, INDWRK, ISUPMN, ISUPMX, ITER,
     $                   ITMP1, J, JBLK, K, MINIWSIZE, MINWSIZE, NCLUS,
     $                   NDEPTH, NEGCNT, NEWCLS, NEWFST, NEWFTT, NEWLST,
     $                   NEWSIZ, OFFSET, OLDCLS, OLDFST, OLDIEN, OLDLST,
     $                   OLDNCL, P, PARITY, Q, WBEGIN, WEND, WINDEX,
     $                   WINDMN, WINDPL, ZFROM, ZTO, ZUSEDL, ZUSEDU,
     $                   ZUSEDW
      DOUBLE PRECISION   BSTRES, BSTW, EPS, FUDGE, GAP, GAPTOL, GL, GU,
     $                   LAMBDA, LEFT, LGAP, MINGMA, NRMINV, RESID,
     $                   RGAP, RIGHT, RQCORR, RQTOL, SAVGAP, SGNDEF,
     $                   SIGMA, SPDIAM, SSIGMA, TAU, TMP, TOL, ZTZ
*     ..
*     .. External Functions ..
      DOUBLE PRECISION  DLAMCH
      DOUBLE PRECISION   DDOT, DNRM2
      EXTERNAL           DDOT, DLAMCH, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DLAR1V, DLARRB, DLARRF, DLASET,
     $                   DSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC ABS, DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*     ..

*     The first N entries of WORK are reserved for the eigenvalues
      INDLD = N+1
      INDLLD= 2*N+1
      INDWRK= 3*N+1
      MINWSIZE = 12 * N

      DO 5 I= 1,MINWSIZE
         WORK( I ) = ZERO
 5    CONTINUE

*     IWORK(IINDR+1:IINDR+N) hold the twist indices R for the
*     factorization used to compute the FP vector
      IINDR = 0
*     IWORK(IINDC1+1:IINC2+N) are used to store the clusters of the current
*     layer and the one above.
      IINDC1 = N
      IINDC2 = 2*N
      IINDWK = 3*N + 1

      MINIWSIZE = 7 * N
      DO 10 I= 1,MINIWSIZE
         IWORK( I ) = 0
 10   CONTINUE

      ZUSEDL = 1
      IF(DOL.GT.1) THEN
*        Set lower bound for use of Z
         ZUSEDL = DOL-1
      ENDIF
      ZUSEDU = M
      IF(DOU.LT.M) THEN
*        Set lower bound for use of Z
         ZUSEDU = DOU+1
      ENDIF
*     The width of the part of Z that is used
      ZUSEDW = ZUSEDU - ZUSEDL + 1


      CALL DLASET( 'Full', N, ZUSEDW, ZERO, ZERO,
     $                    Z(1,ZUSEDL), LDZ )

      EPS = DLAMCH( 'Precision' )
      RQTOL = TWO * EPS
*
*     Set expert flags for standard code.
      TRYRQC = .TRUE.

      IF((DOL.EQ.1).AND.(DOU.EQ.M)) THEN
      ELSE
*        Only selected eigenpairs are computed. Since the other evalues
*        are not refined by RQ iteration, bisection has to compute to full
*        accuracy.
         RTOL1 = FOUR * EPS
         RTOL2 = FOUR * EPS
      ENDIF

*     The entries WBEGIN:WEND in W, WERR, WGAP correspond to the
*     desired eigenvalues. The support of the nonzero eigenvector
*     entries is contained in the interval IBEGIN:IEND.
*     Remark that if k eigenpairs are desired, then the eigenvectors
*     are stored in k contiguous columns of Z.

*     DONE is the number of eigenvectors already computed
      DONE = 0
      IBEGIN = 1
      WBEGIN = 1
      DO 170 JBLK = 1, IBLOCK( M )
         IEND = ISPLIT( JBLK )
         SIGMA = L( IEND )
*        Find the eigenvectors of the submatrix indexed IBEGIN
*        through IEND.
         WEND = WBEGIN - 1
 15      CONTINUE
         IF( WEND.LT.M ) THEN
            IF( IBLOCK( WEND+1 ).EQ.JBLK ) THEN
               WEND = WEND + 1
               GO TO 15
            END IF
         END IF
         IF( WEND.LT.WBEGIN ) THEN
            IBEGIN = IEND + 1
            GO TO 170
         ELSEIF( (WEND.LT.DOL).OR.(WBEGIN.GT.DOU) ) THEN
            IBEGIN = IEND + 1
            WBEGIN = WEND + 1
            GO TO 170
         END IF

*        Find local spectral diameter of the block
         GL = GERS( 2*IBEGIN-1 )
         GU = GERS( 2*IBEGIN )
         DO 20 I = IBEGIN+1 , IEND
            GL = MIN( GERS( 2*I-1 ), GL )
            GU = MAX( GERS( 2*I ), GU )
 20      CONTINUE
         SPDIAM = GU - GL

*        OLDIEN is the last index of the previous block
         OLDIEN = IBEGIN - 1
*        Calculate the size of the current block
         IN = IEND - IBEGIN + 1
*        The number of eigenvalues in the current block
         IM = WEND - WBEGIN + 1

*        This is for a 1x1 block
         IF( IBEGIN.EQ.IEND ) THEN
            DONE = DONE+1
            Z( IBEGIN, WBEGIN ) = ONE
            ISUPPZ( 2*WBEGIN-1 ) = IBEGIN
            ISUPPZ( 2*WBEGIN ) = IBEGIN
            W( WBEGIN ) = W( WBEGIN ) + SIGMA
            WORK( WBEGIN ) = W( WBEGIN )
            IBEGIN = IEND + 1
            WBEGIN = WBEGIN + 1
            GO TO 170
         END IF

*        The desired (shifted) eigenvalues are stored in W(WBEGIN:WEND)
*        Note that these can be approximations, in this case, the corresp.
*        entries of WERR give the size of the uncertainty interval.
*        The eigenvalue approximations will be refined when necessary as
*        high relative accuracy is required for the computation of the
*        corresponding eigenvectors.
         CALL DCOPY( IM, W( WBEGIN ), 1,
     &                   WORK( WBEGIN ), 1 )

*        We store in W the eigenvalue approximations w.r.t. the original
*        matrix T.
         DO 30 I=1,IM
            W(WBEGIN+I-1) = W(WBEGIN+I-1)+SIGMA
 30      CONTINUE


*        NDEPTH is the current depth of the representation tree
         NDEPTH = 0
*        PARITY is either 1 or 0
         PARITY = 1
*        NCLUS is the number of clusters for the next level of the
*        representation tree, we start with NCLUS = 1 for the root
         NCLUS = 1
         IWORK( IINDC1+1 ) = 1
         IWORK( IINDC1+2 ) = IM

*        IDONE is the number of eigenvectors already computed in the current
*        block
         IDONE = 0
*        loop while( IDONE.LT.IM )
*        generate the representation tree for the current block and
*        compute the eigenvectors
   40    CONTINUE
         IF( IDONE.LT.IM ) THEN
*           This is a crude protection against infinitely deep trees
            IF( NDEPTH.GT.M ) THEN
               INFO = -2
               RETURN
            ENDIF
*           breadth first processing of the current level of the representation
*           tree: OLDNCL = number of clusters on current level
            OLDNCL = NCLUS
*           reset NCLUS to count the number of child clusters
            NCLUS = 0
*
            PARITY = 1 - PARITY
            IF( PARITY.EQ.0 ) THEN
               OLDCLS = IINDC1
               NEWCLS = IINDC2
            ELSE
               OLDCLS = IINDC2
               NEWCLS = IINDC1
            END IF
*           Process the clusters on the current level
            DO 150 I = 1, OLDNCL
               J = OLDCLS + 2*I
*              OLDFST, OLDLST = first, last index of current cluster.
*                               cluster indices start with 1 and are relative
*                               to WBEGIN when accessing W, WGAP, WERR, Z
               OLDFST = IWORK( J-1 )
               OLDLST = IWORK( J )
               IF( NDEPTH.GT.0 ) THEN
*                 Retrieve relatively robust representation (RRR) of cluster
*                 that has been computed at the previous level
*                 The RRR is stored in Z and overwritten once the eigenvectors
*                 have been computed or when the cluster is refined

                  IF((DOL.EQ.1).AND.(DOU.EQ.M)) THEN
*                    Get representation from location of the leftmost evalue
*                    of the cluster
                     J = WBEGIN + OLDFST - 1
                  ELSE
                     IF(WBEGIN+OLDFST-1.LT.DOL) THEN
*                       Get representation from the left end of Z array
                        J = DOL - 1
                     ELSEIF(WBEGIN+OLDFST-1.GT.DOU) THEN
*                       Get representation from the right end of Z array
                        J = DOU
                     ELSE
                        J = WBEGIN + OLDFST - 1
                     ENDIF
                  ENDIF
                  CALL DCOPY( IN, Z( IBEGIN, J ), 1, D( IBEGIN ), 1 )
                  CALL DCOPY( IN-1, Z( IBEGIN, J+1 ), 1, L( IBEGIN ),
     $               1 )
                  SIGMA = Z( IEND, J+1 )

*                 Set the corresponding entries in Z to zero
                  CALL DLASET( 'Full', IN, 2, ZERO, ZERO,
     $                         Z( IBEGIN, J), LDZ )
               END IF

*              Compute DL and DLL of current RRR
               DO 50 J = IBEGIN, IEND-1
                  TMP = D( J )*L( J )
                  WORK( INDLD-1+J ) = TMP
                  WORK( INDLLD-1+J ) = TMP*L( J )
   50          CONTINUE

               IF( NDEPTH.GT.0 ) THEN
*                 P and Q are index of the first and last eigenvalue to compute
*                 within the current block
                  P = INDEXW( WBEGIN-1+OLDFST )
                  Q = INDEXW( WBEGIN-1+OLDLST )
*                 Offset for the arrays WORK, WGAP and WERR, i.e., th P-OFFSET
*                 thru' Q-OFFSET elements of these arrays are to be used.
C                  OFFSET = P-OLDFST
                  OFFSET = INDEXW( WBEGIN ) - 1
*                 perform limited bisection (if necessary) to get approximate
*                 eigenvalues to the precision needed.
                  CALL DLARRB( IN, D( IBEGIN ),
     $                         WORK(INDLLD+IBEGIN-1),
     $                         P, Q, RTOL1, RTOL2, OFFSET,
     $                         WORK(WBEGIN),WGAP(WBEGIN),WERR(WBEGIN),
     $                         WORK( INDWRK ), IWORK( IINDWK ),
     $                         PIVMIN, SPDIAM, IN, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     INFO = -1
                     RETURN
                  ENDIF
*                 We also recompute the extremal gaps. W holds all eigenvalues
*                 of the unshifted matrix and must be used for computation
*                 of WGAP, the entries of WORK might stem from RRRs with
*                 different shifts. The gaps from WBEGIN-1+OLDFST to
*                 WBEGIN-1+OLDLST are correctly computed in DLARRB.
*                 However, we only allow the gaps to become greater since
*                 this is what should happen when we decrease WERR
                  IF( OLDFST.GT.1) THEN
                     WGAP( WBEGIN+OLDFST-2 ) =
     $             MAX(WGAP(WBEGIN+OLDFST-2),
     $                 W(WBEGIN+OLDFST-1)-WERR(WBEGIN+OLDFST-1)
     $                 - W(WBEGIN+OLDFST-2)-WERR(WBEGIN+OLDFST-2) )
                  ENDIF
                  IF( WBEGIN + OLDLST -1 .LT. WEND ) THEN
                     WGAP( WBEGIN+OLDLST-1 ) =
     $               MAX(WGAP(WBEGIN+OLDLST-1),
     $                   W(WBEGIN+OLDLST)-WERR(WBEGIN+OLDLST)
     $                   - W(WBEGIN+OLDLST-1)-WERR(WBEGIN+OLDLST-1) )
                  ENDIF
*                 Each time the eigenvalues in WORK get refined, we store
*                 the newly found approximation with all shifts applied in W
                  DO 53 J=OLDFST,OLDLST
                     W(WBEGIN+J-1) = WORK(WBEGIN+J-1)+SIGMA
 53               CONTINUE
               END IF

*              Process the current node.
               NEWFST = OLDFST
               DO 140 J = OLDFST, OLDLST
                  IF( J.EQ.OLDLST ) THEN
*                    we are at the right end of the cluster, this is also the
*                    boundary of the child cluster
                     NEWLST = J
                  ELSE IF ( WGAP( WBEGIN + J -1).GE.
     $                    MINRGP* ABS( WORK(WBEGIN + J -1) ) ) THEN
*                    the right relative gap is big enough, the child cluster
*                    (NEWFST,..,NEWLST) is well separated from the following
                     NEWLST = J
                   ELSE
*                    inside a child cluster, the relative gap is not
*                    big enough.
                     GOTO 140
                  END IF

*                 Compute size of child cluster found
                  NEWSIZ = NEWLST - NEWFST + 1

*                 NEWFTT is the place in Z where the new RRR or the computed
*                 eigenvector is to be stored
                  IF((DOL.EQ.1).AND.(DOU.EQ.M)) THEN
*                    Store representation at location of the leftmost evalue
*                    of the cluster
                     NEWFTT = WBEGIN + NEWFST - 1
                  ELSE
                     IF(WBEGIN+NEWFST-1.LT.DOL) THEN
*                       Store representation at the left end of Z array
                        NEWFTT = DOL - 1
                     ELSEIF(WBEGIN+NEWFST-1.GT.DOU) THEN
*                       Store representation at the right end of Z array
                        NEWFTT = DOU
                     ELSE
                        NEWFTT = WBEGIN + NEWFST - 1
                     ENDIF
                  ENDIF

                  IF( NEWSIZ.GT.1) THEN
*
*                    Current child is not a singleton but a cluster.
*                    Compute and store new representation of child.
*
*
*                    Compute left and right cluster gap.
*
*                    LGAP and RGAP are not computed from WORK because
*                    the eigenvalue approximations may stem from RRRs
*                    different shifts. However, W hold all eigenvalues
*                    of the unshifted matrix. Still, the entries in WGAP
*                    have to be computed from WORK since the entries
*                    in W might be of the same order so that gaps are not
*                    exhibited correctly for very close eigenvalues.
                     IF( NEWFST.EQ.1 ) THEN
                        LGAP = MAX( ZERO,
     $                       W(WBEGIN)-WERR(WBEGIN) - VL )
                    ELSE
                        LGAP = WGAP( WBEGIN+NEWFST-2 )
                     ENDIF
                     RGAP = WGAP( WBEGIN+NEWLST-1 )
*
*                    Compute left- and rightmost eigenvalue of child
*                    to high precision in order to shift as close
*                    as possible and obtain as large relative gaps
*                    as possible
*
                     DO 55 K =1,2
                        IF(K.EQ.1) THEN
                           P = INDEXW( WBEGIN-1+NEWFST )
                        ELSE
                           P = INDEXW( WBEGIN-1+NEWLST )
                        ENDIF
                        OFFSET = INDEXW( WBEGIN ) - 1
                        CALL DLARRB( IN, D(IBEGIN),
     $                       WORK( INDLLD+IBEGIN-1 ),P,P,
     $                       RQTOL, RQTOL, OFFSET,
     $                       WORK(WBEGIN),WGAP(WBEGIN),
     $                       WERR(WBEGIN),WORK( INDWRK ),
     $                       IWORK( IINDWK ), PIVMIN, SPDIAM,
     $                       IN, IINFO )
 55                  CONTINUE
*
                     IF((WBEGIN+NEWLST-1.LT.DOL).OR.
     $                  (WBEGIN+NEWFST-1.GT.DOU)) THEN
*                       if the cluster contains no desired eigenvalues
*                       skip the computation of that branch of the rep. tree
*
*                       We could skip before the refinement of the extremal
*                       eigenvalues of the child, but then the representation
*                       tree could be different from the one when nothing is
*                       skipped. For this reason we skip at this place.
                        IDONE = IDONE + NEWLST - NEWFST + 1
                        GOTO 139
                     ENDIF
*
*                    Compute RRR of child cluster.
*                    Note that the new RRR is stored in Z
*
C                    DLARRF needs LWORK = 2*N
                     CALL DLARRF( IN, D( IBEGIN ), L( IBEGIN ),
     $                         WORK(INDLD+IBEGIN-1),
     $                         NEWFST, NEWLST, WORK(WBEGIN),
     $                         WGAP(WBEGIN), WERR(WBEGIN),
     $                         SPDIAM, LGAP, RGAP, PIVMIN, TAU,
     $                         Z(IBEGIN, NEWFTT),Z(IBEGIN, NEWFTT+1),
     $                         WORK( INDWRK ), IINFO )
                     IF( IINFO.EQ.0 ) THEN
*                       a new RRR for the cluster was found by DLARRF
*                       update shift and store it
                        SSIGMA = SIGMA + TAU
                        Z( IEND, NEWFTT+1 ) = SSIGMA
*                       WORK() are the midpoints and WERR() the semi-width
*                       Note that the entries in W are unchanged.
                        DO 116 K = NEWFST, NEWLST
                           FUDGE =
     $                          THREE*EPS*ABS(WORK(WBEGIN+K-1))
                           WORK( WBEGIN + K - 1 ) =
     $                          WORK( WBEGIN + K - 1) - TAU
                           FUDGE = FUDGE +
     $                          FOUR*EPS*ABS(WORK(WBEGIN+K-1))
*                          Fudge errors
                           WERR( WBEGIN + K - 1 ) =
     $                          WERR( WBEGIN + K - 1 ) + FUDGE
*                          Gaps are not fudged. Provided that WERR is small
*                          when eigenvalues are close, a zero gap indicates
*                          that a new representation is needed for resolving
*                          the cluster. A fudge could lead to a wrong decision
*                          of judging eigenvalues 'separated' which in
*                          reality are not. This could have a negative impact
*                          on the orthogonality of the computed eigenvectors.
 116                    CONTINUE

                        NCLUS = NCLUS + 1
                        K = NEWCLS + 2*NCLUS
                        IWORK( K-1 ) = NEWFST
                        IWORK( K ) = NEWLST
                     ELSE
                        INFO = -2
                        RETURN
                     ENDIF
                  ELSE
*
*                    Compute eigenvector of singleton
*
                     ITER = 0
*
                     TOL = FOUR * LOG(DBLE(IN)) * EPS
*
                     K = NEWFST
                     WINDEX = WBEGIN + K - 1
                     WINDMN = MAX(WINDEX - 1,1)
                     WINDPL = MIN(WINDEX + 1,M)
                     LAMBDA = WORK( WINDEX )
                     DONE = DONE + 1
*                    Check if eigenvector computation is to be skipped
                     IF((WINDEX.LT.DOL).OR.
     $                  (WINDEX.GT.DOU)) THEN
                        ESKIP = .TRUE.
                        GOTO 125
                     ELSE
                        ESKIP = .FALSE.
                     ENDIF
                     LEFT = WORK( WINDEX ) - WERR( WINDEX )
                     RIGHT = WORK( WINDEX ) + WERR( WINDEX )
                     INDEIG = INDEXW( WINDEX )
*                    Note that since we compute the eigenpairs for a child,
*                    all eigenvalue approximations are w.r.t the same shift.
*                    In this case, the entries in WORK should be used for
*                    computing the gaps since they exhibit even very small
*                    differences in the eigenvalues, as opposed to the
*                    entries in W which might "look" the same.

                     IF( K .EQ. 1) THEN
*                       In the case RANGE='I' and with not much initial
*                       accuracy in LAMBDA and VL, the formula
*                       LGAP = MAX( ZERO, (SIGMA - VL) + LAMBDA )
*                       can lead to an overestimation of the left gap and
*                       thus to inadequately early RQI 'convergence'.
*                       Prevent this by forcing a small left gap.
                        LGAP = EPS*MAX(ABS(LEFT),ABS(RIGHT))
                     ELSE
                        LGAP = WGAP(WINDMN)
                     ENDIF
                     IF( K .EQ. IM) THEN
*                       In the case RANGE='I' and with not much initial
*                       accuracy in LAMBDA and VU, the formula
*                       can lead to an overestimation of the right gap and
*                       thus to inadequately early RQI 'convergence'.
*                       Prevent this by forcing a small right gap.
                        RGAP = EPS*MAX(ABS(LEFT),ABS(RIGHT))
                     ELSE
                        RGAP = WGAP(WINDEX)
                     ENDIF
                     GAP = MIN( LGAP, RGAP )
                     IF(( K .EQ. 1).OR.(K .EQ. IM)) THEN
*                       The eigenvector support can become wrong
*                       because significant entries could be cut off due to a
*                       large GAPTOL parameter in LAR1V. Prevent this.
                        GAPTOL = ZERO
                     ELSE
                        GAPTOL = GAP * EPS
                     ENDIF
                     ISUPMN = IN
                     ISUPMX = 1
*                    Update WGAP so that it holds the minimum gap
*                    to the left or the right. This is crucial in the
*                    case where bisection is used to ensure that the
*                    eigenvalue is refined up to the required precision.
*                    The correct value is restored afterwards.
                     SAVGAP = WGAP(WINDEX)
                     WGAP(WINDEX) = GAP
*                    We want to use the Rayleigh Quotient Correction
*                    as often as possible since it converges quadratically
*                    when we are close enough to the desired eigenvalue.
*                    However, the Rayleigh Quotient can have the wrong sign
*                    and lead us away from the desired eigenvalue. In this
*                    case, the best we can do is to use bisection.
                     USEDBS = .FALSE.
                     USEDRQ = .FALSE.
*                    Bisection is initially turned off unless it is forced
                     NEEDBS =  .NOT.TRYRQC
 120                 CONTINUE
*                    Check if bisection should be used to refine eigenvalue
                     IF(NEEDBS) THEN
*                       Take the bisection as new iterate
                        USEDBS = .TRUE.
                        ITMP1 = IWORK( IINDR+WINDEX )
                        OFFSET = INDEXW( WBEGIN ) - 1
                        CALL DLARRB( IN, D(IBEGIN),
     $                       WORK(INDLLD+IBEGIN-1),INDEIG,INDEIG,
     $                       ZERO, TWO*EPS, OFFSET,
     $                       WORK(WBEGIN),WGAP(WBEGIN),
     $                       WERR(WBEGIN),WORK( INDWRK ),
     $                       IWORK( IINDWK ), PIVMIN, SPDIAM,
     $                       ITMP1, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           INFO = -3
                           RETURN
                        ENDIF
                        LAMBDA = WORK( WINDEX )
*                       Reset twist index from inaccurate LAMBDA to
*                       force computation of true MINGMA
                        IWORK( IINDR+WINDEX ) = 0
                     ENDIF
*                    Given LAMBDA, compute the eigenvector.
                     CALL DLAR1V( IN, 1, IN, LAMBDA, D( IBEGIN ),
     $                    L( IBEGIN ), WORK(INDLD+IBEGIN-1),
     $                    WORK(INDLLD+IBEGIN-1),
     $                    PIVMIN, GAPTOL, Z( IBEGIN, WINDEX ),
     $                    .NOT.USEDBS, NEGCNT, ZTZ, MINGMA,
     $                    IWORK( IINDR+WINDEX ), ISUPPZ( 2*WINDEX-1 ),
     $                    NRMINV, RESID, RQCORR, WORK( INDWRK ) )
                     IF(ITER .EQ. 0) THEN
                        BSTRES = RESID
                        BSTW = LAMBDA
                     ELSEIF(RESID.LT.BSTRES) THEN
                        BSTRES = RESID
                        BSTW = LAMBDA
                     ENDIF
                     ISUPMN = MIN(ISUPMN,ISUPPZ( 2*WINDEX-1 ))
                     ISUPMX = MAX(ISUPMX,ISUPPZ( 2*WINDEX ))
                     ITER = ITER + 1

*                    sin alpha <= |resid|/gap
*                    Note that both the residual and the gap are
*                    proportional to the matrix, so ||T|| doesn't play
*                    a role in the quotient

*
*                    Convergence test for Rayleigh-Quotient iteration
*                    (omitted when Bisection has been used)
*
                     IF( RESID.GT.TOL*GAP .AND. ABS( RQCORR ).GT.
     $                    RQTOL*ABS( LAMBDA ) .AND. .NOT. USEDBS)
     $                    THEN
*                       We need to check that the RQCORR update doesn't
*                       move the eigenvalue away from the desired one and
*                       towards a neighbor. -> protection with bisection
                        IF(INDEIG.LE.NEGCNT) THEN
*                          The wanted eigenvalue lies to the left
                           SGNDEF = -ONE
                        ELSE
*                          The wanted eigenvalue lies to the right
                           SGNDEF = ONE
                        ENDIF
*                       We only use the RQCORR if it improves the
*                       the iterate reasonably.
                        IF( ( RQCORR*SGNDEF.GE.ZERO )
     $                       .AND.( LAMBDA + RQCORR.LE. RIGHT)
     $                       .AND.( LAMBDA + RQCORR.GE. LEFT)
     $                       ) THEN
                           USEDRQ = .TRUE.
*                          Store new midpoint of bisection interval in WORK
                           IF(SGNDEF.EQ.ONE) THEN
*                             The current LAMBDA is on the left of the true
*                             eigenvalue
                              LEFT = LAMBDA
*                             We prefer to assume that the error estimate
*                             is correct. We could make the interval not
*                             as a bracket but to be modified if the RQCORR
*                             chooses to. In this case, the RIGHT side should
*                             be modified as follows:
*                              RIGHT = MAX(RIGHT, LAMBDA + RQCORR)
                           ELSE
*                             The current LAMBDA is on the right of the true
*                             eigenvalue
                              RIGHT = LAMBDA
*                             See comment about assuming the error estimate is
*                             correct above.
*                              LEFT = MIN(LEFT, LAMBDA + RQCORR)
                           ENDIF
                           WORK( WINDEX ) =
     $                       HALF * (RIGHT + LEFT)
*                          Take RQCORR since it has the correct sign and
*                          improves the iterate reasonably
                           LAMBDA = LAMBDA + RQCORR
*                          Update width of error interval
                           WERR( WINDEX ) =
     $                             HALF * (RIGHT-LEFT)
                        ELSE
                           NEEDBS = .TRUE.
                        ENDIF
                        IF(RIGHT-LEFT.LT.RQTOL*ABS(LAMBDA)) THEN
*                             The eigenvalue is computed to bisection accuracy
*                             compute eigenvector and stop
                           USEDBS = .TRUE.
                           GOTO 120
                        ELSEIF( ITER.LT.MAXITR ) THEN
                           GOTO 120
                        ELSEIF( ITER.EQ.MAXITR ) THEN
                           NEEDBS = .TRUE.
                           GOTO 120
                        ELSE
                           INFO = 5
                           RETURN
                        END IF
                     ELSE
                        STP2II = .FALSE.
        IF(USEDRQ .AND. USEDBS .AND.
     $                     BSTRES.LE.RESID) THEN
                           LAMBDA = BSTW
                           STP2II = .TRUE.
                        ENDIF
                        IF (STP2II) THEN
*                          improve error angle by second step
                           CALL DLAR1V( IN, 1, IN, LAMBDA,
     $                          D( IBEGIN ), L( IBEGIN ),
     $                          WORK(INDLD+IBEGIN-1),
     $                          WORK(INDLLD+IBEGIN-1),
     $                          PIVMIN, GAPTOL, Z( IBEGIN, WINDEX ),
     $                          .NOT.USEDBS, NEGCNT, ZTZ, MINGMA,
     $                          IWORK( IINDR+WINDEX ),
     $                          ISUPPZ( 2*WINDEX-1 ),
     $                          NRMINV, RESID, RQCORR, WORK( INDWRK ) )
                        ENDIF
                        WORK( WINDEX ) = LAMBDA
                     END IF
*
*                    Compute FP-vector support w.r.t. whole matrix
*
                     ISUPPZ( 2*WINDEX-1 ) = ISUPPZ( 2*WINDEX-1 )+OLDIEN
                     ISUPPZ( 2*WINDEX ) = ISUPPZ( 2*WINDEX )+OLDIEN
                     ZFROM = ISUPPZ( 2*WINDEX-1 )
                     ZTO = ISUPPZ( 2*WINDEX )
                     ISUPMN = ISUPMN + OLDIEN
                     ISUPMX = ISUPMX + OLDIEN
*                    Ensure vector is ok if support in the RQI has changed
                     IF(ISUPMN.LT.ZFROM) THEN
                        DO 122 II = ISUPMN,ZFROM-1
                           Z( II, WINDEX ) = ZERO
 122                    CONTINUE
                     ENDIF
                     IF(ISUPMX.GT.ZTO) THEN
                        DO 123 II = ZTO+1,ISUPMX
                           Z( II, WINDEX ) = ZERO
 123                    CONTINUE
                     ENDIF
                     CALL DSCAL( ZTO-ZFROM+1, NRMINV,
     $                       Z( ZFROM, WINDEX ), 1 )
 125                 CONTINUE
*                    Update W
                     W( WINDEX ) = LAMBDA+SIGMA
*                    Recompute the gaps on the left and right
*                    But only allow them to become larger and not
*                    smaller (which can only happen through "bad"
*                    cancellation and doesn't reflect the theory
*                    where the initial gaps are underestimated due
*                    to WERR being too crude.)
                     IF(.NOT.ESKIP) THEN
                        IF( K.GT.1) THEN
                           WGAP( WINDMN ) = MAX( WGAP(WINDMN),
     $                          W(WINDEX)-WERR(WINDEX)
     $                          - W(WINDMN)-WERR(WINDMN) )
                        ENDIF
                        IF( WINDEX.LT.WEND ) THEN
                           WGAP( WINDEX ) = MAX( SAVGAP,
     $                          W( WINDPL )-WERR( WINDPL )
     $                          - W( WINDEX )-WERR( WINDEX) )
                        ENDIF
                     ENDIF
                     IDONE = IDONE + 1
                  ENDIF
*                 here ends the code for the current child
*
 139              CONTINUE
*                 Proceed to any remaining child nodes
                  NEWFST = J + 1
 140           CONTINUE
 150        CONTINUE
            NDEPTH = NDEPTH + 1
            GO TO 40
         END IF
         IBEGIN = IEND + 1
         WBEGIN = WEND + 1
 170  CONTINUE
*

      RETURN
*
*     End of DLARRV
*
      END
      SUBROUTINE DLARTG( F, G, CS, SN, R )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   CS, F, G, R, SN
*     ..
*
*  Purpose
*  =======
*
*  DLARTG generate a plane rotation so that
*
*     [  CS  SN  ]  .  [ F ]  =  [ R ]   where CS**2 + SN**2 = 1.
*     [ -SN  CS  ]     [ G ]     [ 0 ]
*
*  This is a slower, more accurate version of the BLAS1 routine DROTG,
*  with the following other differences:
*     F and G are unchanged on return.
*     If G=0, then CS=1 and SN=0.
*     If F=0 and (G .ne. 0), then CS=0 and SN=1 without doing any
*        floating point operations (saves work in DBDSQR when
*        there are zeros on the diagonal).
*
*  If F exceeds G in magnitude, CS will be positive.
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The first component of vector to be rotated.
*
*  G       (input) DOUBLE PRECISION
*          The second component of vector to be rotated.
*
*  CS      (output) DOUBLE PRECISION
*          The cosine of the rotation.
*
*  SN      (output) DOUBLE PRECISION
*          The sine of the rotation.
*
*  R       (output) DOUBLE PRECISION
*          The nonzero component of the rotated vector.
*
*  This version has a few statements commented out for thread safety
*  (machine parameters are computed on each entry). 10 feb 03, SJH.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
*     LOGICAL            FIRST
      INTEGER            COUNT, I
      DOUBLE PRECISION   EPS, F1, G1, SAFMIN, SAFMN2, SAFMX2, SCALE
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, SQRT
*     ..
*     .. Save statement ..
*     SAVE               FIRST, SAFMX2, SAFMIN, SAFMN2
*     ..
*     .. Data statements ..
*     DATA               FIRST / .TRUE. /
*     ..
*     .. Executable Statements ..
*
*     IF( FIRST ) THEN
         SAFMIN = DLAMCH( 'S' )
         EPS = DLAMCH( 'E' )
         SAFMN2 = DLAMCH( 'B' )**INT( LOG( SAFMIN / EPS ) /
     $            LOG( DLAMCH( 'B' ) ) / TWO )
         SAFMX2 = ONE / SAFMN2
*        FIRST = .FALSE.
*     END IF
      IF( G.EQ.ZERO ) THEN
         CS = ONE
         SN = ZERO
         R = F
      ELSE IF( F.EQ.ZERO ) THEN
         CS = ZERO
         SN = ONE
         R = G
      ELSE
         F1 = F
         G1 = G
         SCALE = MAX( ABS( F1 ), ABS( G1 ) )
         IF( SCALE.GE.SAFMX2 ) THEN
            COUNT = 0
   10       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMN2
            G1 = G1*SAFMN2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.GE.SAFMX2 )
     $         GO TO 10
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 20 I = 1, COUNT
               R = R*SAFMX2
   20       CONTINUE
         ELSE IF( SCALE.LE.SAFMN2 ) THEN
            COUNT = 0
   30       CONTINUE
            COUNT = COUNT + 1
            F1 = F1*SAFMX2
            G1 = G1*SAFMX2
            SCALE = MAX( ABS( F1 ), ABS( G1 ) )
            IF( SCALE.LE.SAFMN2 )
     $         GO TO 30
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
            DO 40 I = 1, COUNT
               R = R*SAFMN2
   40       CONTINUE
         ELSE
            R = SQRT( F1**2+G1**2 )
            CS = F1 / R
            SN = G1 / R
         END IF
         IF( ABS( F ).GT.ABS( G ) .AND. CS.LT.ZERO ) THEN
            CS = -CS
            SN = -SN
            R = -R
         END IF
      END IF
      RETURN
*
*     End of DLARTG
*
      END
      SUBROUTINE DLARTV( N, X, INCX, Y, INCY, C, S, INCC )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCC, INCX, INCY, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( * ), S( * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DLARTV applies a vector of real plane rotations to elements of the
*  real vectors x and y. For i = 1,2,...,n
*
*     ( x(i) ) := (  c(i)  s(i) ) ( x(i) )
*     ( y(i) )    ( -s(i)  c(i) ) ( y(i) )
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of plane rotations to be applied.
*
*  X       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCX)
*          The vector x.
*
*  INCX    (input) INTEGER
*          The increment between elements of X. INCX > 0.
*
*  Y       (input/output) DOUBLE PRECISION array,
*                         dimension (1+(N-1)*INCY)
*          The vector y.
*
*  INCY    (input) INTEGER
*          The increment between elements of Y. INCY > 0.
*
*  C       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
*          The cosines of the plane rotations.
*
*  S       (input) DOUBLE PRECISION array, dimension (1+(N-1)*INCC)
*          The sines of the plane rotations.
*
*  INCC    (input) INTEGER
*          The increment between elements of C and S. INCC > 0.
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IC, IX, IY
      DOUBLE PRECISION   XI, YI
*     ..
*     .. Executable Statements ..
*
      IX = 1
      IY = 1
      IC = 1
      DO 10 I = 1, N
         XI = X( IX )
         YI = Y( IY )
         X( IX ) = C( IC )*XI + S( IC )*YI
         Y( IY ) = C( IC )*YI - S( IC )*XI
         IX = IX + INCX
         IY = IY + INCY
         IC = IC + INCC
   10 CONTINUE
      RETURN
*
*     End of DLARTV
*
      END
      SUBROUTINE DLARUV( ISEED, N, X )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            N
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   X( N )
*     ..
*
*  Purpose
*  =======
*
*  DLARUV returns a vector of n random real numbers from a uniform (0,1)
*  distribution (n <= 128).
*
*  This is an auxiliary routine called by DLARNV and ZLARNV.
*
*  Arguments
*  =========
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator; the array
*          elements must be between 0 and 4095, and ISEED(4) must be
*          odd.
*          On exit, the seed is updated.
*
*  N       (input) INTEGER
*          The number of random numbers to be generated. N <= 128.
*
*  X       (output) DOUBLE PRECISION array, dimension (N)
*          The generated random numbers.
*
*  Further Details
*  ===============
*
*  This routine uses a multiplicative congruential method with modulus
*  2**48 and multiplier 33952834046453 (see G.S.Fishman,
*  'Multiplicative congruential random number generators with modulus
*  2**b: an exhaustive analysis for b = 32 and a partial analysis for
*  b = 48', Math. Comp. 189, pp 331-344, 1990).
*
*  48-bit integers are stored in 4 integer array elements with 12 bits
*  per element. Hence the routine is portable across machines with
*  integers of 32 bits or more.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      INTEGER            LV, IPW2
      DOUBLE PRECISION   R
      PARAMETER          ( LV = 128, IPW2 = 4096, R = ONE / IPW2 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, I1, I2, I3, I4, IT1, IT2, IT3, IT4, J
*     ..
*     .. Local Arrays ..
      INTEGER            MM( LV, 4 )
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MIN, MOD
*     ..
*     .. Data statements ..
      DATA               ( MM( 1, J ), J = 1, 4 ) / 494, 322, 2508,
     $                   2549 /
      DATA               ( MM( 2, J ), J = 1, 4 ) / 2637, 789, 3754,
     $                   1145 /
      DATA               ( MM( 3, J ), J = 1, 4 ) / 255, 1440, 1766,
     $                   2253 /
      DATA               ( MM( 4, J ), J = 1, 4 ) / 2008, 752, 3572,
     $                   305 /
      DATA               ( MM( 5, J ), J = 1, 4 ) / 1253, 2859, 2893,
     $                   3301 /
      DATA               ( MM( 6, J ), J = 1, 4 ) / 3344, 123, 307,
     $                   1065 /
      DATA               ( MM( 7, J ), J = 1, 4 ) / 4084, 1848, 1297,
     $                   3133 /
      DATA               ( MM( 8, J ), J = 1, 4 ) / 1739, 643, 3966,
     $                   2913 /
      DATA               ( MM( 9, J ), J = 1, 4 ) / 3143, 2405, 758,
     $                   3285 /
      DATA               ( MM( 10, J ), J = 1, 4 ) / 3468, 2638, 2598,
     $                   1241 /
      DATA               ( MM( 11, J ), J = 1, 4 ) / 688, 2344, 3406,
     $                   1197 /
      DATA               ( MM( 12, J ), J = 1, 4 ) / 1657, 46, 2922,
     $                   3729 /
      DATA               ( MM( 13, J ), J = 1, 4 ) / 1238, 3814, 1038,
     $                   2501 /
      DATA               ( MM( 14, J ), J = 1, 4 ) / 3166, 913, 2934,
     $                   1673 /
      DATA               ( MM( 15, J ), J = 1, 4 ) / 1292, 3649, 2091,
     $                   541 /
      DATA               ( MM( 16, J ), J = 1, 4 ) / 3422, 339, 2451,
     $                   2753 /
      DATA               ( MM( 17, J ), J = 1, 4 ) / 1270, 3808, 1580,
     $                   949 /
      DATA               ( MM( 18, J ), J = 1, 4 ) / 2016, 822, 1958,
     $                   2361 /
      DATA               ( MM( 19, J ), J = 1, 4 ) / 154, 2832, 2055,
     $                   1165 /
      DATA               ( MM( 20, J ), J = 1, 4 ) / 2862, 3078, 1507,
     $                   4081 /
      DATA               ( MM( 21, J ), J = 1, 4 ) / 697, 3633, 1078,
     $                   2725 /
      DATA               ( MM( 22, J ), J = 1, 4 ) / 1706, 2970, 3273,
     $                   3305 /
      DATA               ( MM( 23, J ), J = 1, 4 ) / 491, 637, 17,
     $                   3069 /
      DATA               ( MM( 24, J ), J = 1, 4 ) / 931, 2249, 854,
     $                   3617 /
      DATA               ( MM( 25, J ), J = 1, 4 ) / 1444, 2081, 2916,
     $                   3733 /
      DATA               ( MM( 26, J ), J = 1, 4 ) / 444, 4019, 3971,
     $                   409 /
      DATA               ( MM( 27, J ), J = 1, 4 ) / 3577, 1478, 2889,
     $                   2157 /
      DATA               ( MM( 28, J ), J = 1, 4 ) / 3944, 242, 3831,
     $                   1361 /
      DATA               ( MM( 29, J ), J = 1, 4 ) / 2184, 481, 2621,
     $                   3973 /
      DATA               ( MM( 30, J ), J = 1, 4 ) / 1661, 2075, 1541,
     $                   1865 /
      DATA               ( MM( 31, J ), J = 1, 4 ) / 3482, 4058, 893,
     $                   2525 /
      DATA               ( MM( 32, J ), J = 1, 4 ) / 657, 622, 736,
     $                   1409 /
      DATA               ( MM( 33, J ), J = 1, 4 ) / 3023, 3376, 3992,
     $                   3445 /
      DATA               ( MM( 34, J ), J = 1, 4 ) / 3618, 812, 787,
     $                   3577 /
      DATA               ( MM( 35, J ), J = 1, 4 ) / 1267, 234, 2125,
     $                   77 /
      DATA               ( MM( 36, J ), J = 1, 4 ) / 1828, 641, 2364,
     $                   3761 /
      DATA               ( MM( 37, J ), J = 1, 4 ) / 164, 4005, 2460,
     $                   2149 /
      DATA               ( MM( 38, J ), J = 1, 4 ) / 3798, 1122, 257,
     $                   1449 /
      DATA               ( MM( 39, J ), J = 1, 4 ) / 3087, 3135, 1574,
     $                   3005 /
      DATA               ( MM( 40, J ), J = 1, 4 ) / 2400, 2640, 3912,
     $                   225 /
      DATA               ( MM( 41, J ), J = 1, 4 ) / 2870, 2302, 1216,
     $                   85 /
      DATA               ( MM( 42, J ), J = 1, 4 ) / 3876, 40, 3248,
     $                   3673 /
      DATA               ( MM( 43, J ), J = 1, 4 ) / 1905, 1832, 3401,
     $                   3117 /
      DATA               ( MM( 44, J ), J = 1, 4 ) / 1593, 2247, 2124,
     $                   3089 /
      DATA               ( MM( 45, J ), J = 1, 4 ) / 1797, 2034, 2762,
     $                   1349 /
      DATA               ( MM( 46, J ), J = 1, 4 ) / 1234, 2637, 149,
     $                   2057 /
      DATA               ( MM( 47, J ), J = 1, 4 ) / 3460, 1287, 2245,
     $                   413 /
      DATA               ( MM( 48, J ), J = 1, 4 ) / 328, 1691, 166,
     $                   65 /
      DATA               ( MM( 49, J ), J = 1, 4 ) / 2861, 496, 466,
     $                   1845 /
      DATA               ( MM( 50, J ), J = 1, 4 ) / 1950, 1597, 4018,
     $                   697 /
      DATA               ( MM( 51, J ), J = 1, 4 ) / 617, 2394, 1399,
     $                   3085 /
      DATA               ( MM( 52, J ), J = 1, 4 ) / 2070, 2584, 190,
     $                   3441 /
      DATA               ( MM( 53, J ), J = 1, 4 ) / 3331, 1843, 2879,
     $                   1573 /
      DATA               ( MM( 54, J ), J = 1, 4 ) / 769, 336, 153,
     $                   3689 /
      DATA               ( MM( 55, J ), J = 1, 4 ) / 1558, 1472, 2320,
     $                   2941 /
      DATA               ( MM( 56, J ), J = 1, 4 ) / 2412, 2407, 18,
     $                   929 /
      DATA               ( MM( 57, J ), J = 1, 4 ) / 2800, 433, 712,
     $                   533 /
      DATA               ( MM( 58, J ), J = 1, 4 ) / 189, 2096, 2159,
     $                   2841 /
      DATA               ( MM( 59, J ), J = 1, 4 ) / 287, 1761, 2318,
     $                   4077 /
      DATA               ( MM( 60, J ), J = 1, 4 ) / 2045, 2810, 2091,
     $                   721 /
      DATA               ( MM( 61, J ), J = 1, 4 ) / 1227, 566, 3443,
     $                   2821 /
      DATA               ( MM( 62, J ), J = 1, 4 ) / 2838, 442, 1510,
     $                   2249 /
      DATA               ( MM( 63, J ), J = 1, 4 ) / 209, 41, 449,
     $                   2397 /
      DATA               ( MM( 64, J ), J = 1, 4 ) / 2770, 1238, 1956,
     $                   2817 /
      DATA               ( MM( 65, J ), J = 1, 4 ) / 3654, 1086, 2201,
     $                   245 /
      DATA               ( MM( 66, J ), J = 1, 4 ) / 3993, 603, 3137,
     $                   1913 /
      DATA               ( MM( 67, J ), J = 1, 4 ) / 192, 840, 3399,
     $                   1997 /
      DATA               ( MM( 68, J ), J = 1, 4 ) / 2253, 3168, 1321,
     $                   3121 /
      DATA               ( MM( 69, J ), J = 1, 4 ) / 3491, 1499, 2271,
     $                   997 /
      DATA               ( MM( 70, J ), J = 1, 4 ) / 2889, 1084, 3667,
     $                   1833 /
      DATA               ( MM( 71, J ), J = 1, 4 ) / 2857, 3438, 2703,
     $                   2877 /
      DATA               ( MM( 72, J ), J = 1, 4 ) / 2094, 2408, 629,
     $                   1633 /
      DATA               ( MM( 73, J ), J = 1, 4 ) / 1818, 1589, 2365,
     $                   981 /
      DATA               ( MM( 74, J ), J = 1, 4 ) / 688, 2391, 2431,
     $                   2009 /
      DATA               ( MM( 75, J ), J = 1, 4 ) / 1407, 288, 1113,
     $                   941 /
      DATA               ( MM( 76, J ), J = 1, 4 ) / 634, 26, 3922,
     $                   2449 /
      DATA               ( MM( 77, J ), J = 1, 4 ) / 3231, 512, 2554,
     $                   197 /
      DATA               ( MM( 78, J ), J = 1, 4 ) / 815, 1456, 184,
     $                   2441 /
      DATA               ( MM( 79, J ), J = 1, 4 ) / 3524, 171, 2099,
     $                   285 /
      DATA               ( MM( 80, J ), J = 1, 4 ) / 1914, 1677, 3228,
     $                   1473 /
      DATA               ( MM( 81, J ), J = 1, 4 ) / 516, 2657, 4012,
     $                   2741 /
      DATA               ( MM( 82, J ), J = 1, 4 ) / 164, 2270, 1921,
     $                   3129 /
      DATA               ( MM( 83, J ), J = 1, 4 ) / 303, 2587, 3452,
     $                   909 /
      DATA               ( MM( 84, J ), J = 1, 4 ) / 2144, 2961, 3901,
     $                   2801 /
      DATA               ( MM( 85, J ), J = 1, 4 ) / 3480, 1970, 572,
     $                   421 /
      DATA               ( MM( 86, J ), J = 1, 4 ) / 119, 1817, 3309,
     $                   4073 /
      DATA               ( MM( 87, J ), J = 1, 4 ) / 3357, 676, 3171,
     $                   2813 /
      DATA               ( MM( 88, J ), J = 1, 4 ) / 837, 1410, 817,
     $                   2337 /
      DATA               ( MM( 89, J ), J = 1, 4 ) / 2826, 3723, 3039,
     $                   1429 /
      DATA               ( MM( 90, J ), J = 1, 4 ) / 2332, 2803, 1696,
     $                   1177 /
      DATA               ( MM( 91, J ), J = 1, 4 ) / 2089, 3185, 1256,
     $                   1901 /
      DATA               ( MM( 92, J ), J = 1, 4 ) / 3780, 184, 3715,
     $                   81 /
      DATA               ( MM( 93, J ), J = 1, 4 ) / 1700, 663, 2077,
     $                   1669 /
      DATA               ( MM( 94, J ), J = 1, 4 ) / 3712, 499, 3019,
     $                   2633 /
      DATA               ( MM( 95, J ), J = 1, 4 ) / 150, 3784, 1497,
     $                   2269 /
      DATA               ( MM( 96, J ), J = 1, 4 ) / 2000, 1631, 1101,
     $                   129 /
      DATA               ( MM( 97, J ), J = 1, 4 ) / 3375, 1925, 717,
     $                   1141 /
      DATA               ( MM( 98, J ), J = 1, 4 ) / 1621, 3912, 51,
     $                   249 /
      DATA               ( MM( 99, J ), J = 1, 4 ) / 3090, 1398, 981,
     $                   3917 /
      DATA               ( MM( 100, J ), J = 1, 4 ) / 3765, 1349, 1978,
     $                   2481 /
      DATA               ( MM( 101, J ), J = 1, 4 ) / 1149, 1441, 1813,
     $                   3941 /
      DATA               ( MM( 102, J ), J = 1, 4 ) / 3146, 2224, 3881,
     $                   2217 /
      DATA               ( MM( 103, J ), J = 1, 4 ) / 33, 2411, 76,
     $                   2749 /
      DATA               ( MM( 104, J ), J = 1, 4 ) / 3082, 1907, 3846,
     $                   3041 /
      DATA               ( MM( 105, J ), J = 1, 4 ) / 2741, 3192, 3694,
     $                   1877 /
      DATA               ( MM( 106, J ), J = 1, 4 ) / 359, 2786, 1682,
     $                   345 /
      DATA               ( MM( 107, J ), J = 1, 4 ) / 3316, 382, 124,
     $                   2861 /
      DATA               ( MM( 108, J ), J = 1, 4 ) / 1749, 37, 1660,
     $                   1809 /
      DATA               ( MM( 109, J ), J = 1, 4 ) / 185, 759, 3997,
     $                   3141 /
      DATA               ( MM( 110, J ), J = 1, 4 ) / 2784, 2948, 479,
     $                   2825 /
      DATA               ( MM( 111, J ), J = 1, 4 ) / 2202, 1862, 1141,
     $                   157 /
      DATA               ( MM( 112, J ), J = 1, 4 ) / 2199, 3802, 886,
     $                   2881 /
      DATA               ( MM( 113, J ), J = 1, 4 ) / 1364, 2423, 3514,
     $                   3637 /
      DATA               ( MM( 114, J ), J = 1, 4 ) / 1244, 2051, 1301,
     $                   1465 /
      DATA               ( MM( 115, J ), J = 1, 4 ) / 2020, 2295, 3604,
     $                   2829 /
      DATA               ( MM( 116, J ), J = 1, 4 ) / 3160, 1332, 1888,
     $                   2161 /
      DATA               ( MM( 117, J ), J = 1, 4 ) / 2785, 1832, 1836,
     $                   3365 /
      DATA               ( MM( 118, J ), J = 1, 4 ) / 2772, 2405, 1990,
     $                   361 /
      DATA               ( MM( 119, J ), J = 1, 4 ) / 1217, 3638, 2058,
     $                   2685 /
      DATA               ( MM( 120, J ), J = 1, 4 ) / 1822, 3661, 692,
     $                   3745 /
      DATA               ( MM( 121, J ), J = 1, 4 ) / 1245, 327, 1194,
     $                   2325 /
      DATA               ( MM( 122, J ), J = 1, 4 ) / 2252, 3660, 20,
     $                   3609 /
      DATA               ( MM( 123, J ), J = 1, 4 ) / 3904, 716, 3285,
     $                   3821 /
      DATA               ( MM( 124, J ), J = 1, 4 ) / 2774, 1842, 2046,
     $                   3537 /
      DATA               ( MM( 125, J ), J = 1, 4 ) / 997, 3987, 2107,
     $                   517 /
      DATA               ( MM( 126, J ), J = 1, 4 ) / 2573, 1368, 3508,
     $                   3017 /
      DATA               ( MM( 127, J ), J = 1, 4 ) / 1148, 1848, 3525,
     $                   2141 /
      DATA               ( MM( 128, J ), J = 1, 4 ) / 545, 2366, 3801,
     $                   1537 /
*     ..
*     .. Executable Statements ..
*
      I1 = ISEED( 1 )
      I2 = ISEED( 2 )
      I3 = ISEED( 3 )
      I4 = ISEED( 4 )
*
      DO 10 I = 1, MIN( N, LV )
*	  
  20     CONTINUE
*
*        Multiply the seed by i-th power of the multiplier modulo 2**48
*
         IT4 = I4*MM( I, 4 )
         IT3 = IT4 / IPW2
         IT4 = IT4 - IPW2*IT3
         IT3 = IT3 + I3*MM( I, 4 ) + I4*MM( I, 3 )
         IT2 = IT3 / IPW2
         IT3 = IT3 - IPW2*IT2
         IT2 = IT2 + I2*MM( I, 4 ) + I3*MM( I, 3 ) + I4*MM( I, 2 )
         IT1 = IT2 / IPW2
         IT2 = IT2 - IPW2*IT1
         IT1 = IT1 + I1*MM( I, 4 ) + I2*MM( I, 3 ) + I3*MM( I, 2 ) +
     $         I4*MM( I, 1 )
         IT1 = MOD( IT1, IPW2 )
*
*        Convert 48-bit integer to a real number in the interval (0,1)
*
         X( I ) = R*( DBLE( IT1 )+R*( DBLE( IT2 )+R*( DBLE( IT3 )+R*
     $            DBLE( IT4 ) ) ) )
*
         IF (X( I ).EQ.1.0D0) THEN
*           If a real number has n bits of precision, and the first
*           n bits of the 48-bit integer above happen to be all 1 (which
*           will occur about once every 2**n calls), then X( I ) will
*           be rounded to exactly 1.0. 
*           Since X( I ) is not supposed to return exactly 0.0 or 1.0,
*           the statistically correct thing to do in this situation is
*           simply to iterate again.
*           N.B. the case X( I ) = 0.0 should not be possible.	
            I1 = I1 + 2
            I2 = I2 + 2
            I3 = I3 + 2
            I4 = I4 + 2
            GOTO 20
         END IF
*
   10 CONTINUE
*
*     Return final value of seed
*
      ISEED( 1 ) = IT1
      ISEED( 2 ) = IT2
      ISEED( 3 ) = IT3
      ISEED( 4 ) = IT4
      RETURN
*
*     End of DLARUV
*
      END
      SUBROUTINE DLAS2( F, G, H, SSMIN, SSMAX )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   F, G, H, SSMAX, SSMIN
*     ..
*
*  Purpose
*  =======
*
*  DLAS2  computes the singular values of the 2-by-2 matrix
*     [  F   G  ]
*     [  0   H  ].
*  On return, SSMIN is the smaller singular value and SSMAX is the
*  larger singular value.
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The (1,1) element of the 2-by-2 matrix.
*
*  G       (input) DOUBLE PRECISION
*          The (1,2) element of the 2-by-2 matrix.
*
*  H       (input) DOUBLE PRECISION
*          The (2,2) element of the 2-by-2 matrix.
*
*  SSMIN   (output) DOUBLE PRECISION
*          The smaller singular value.
*
*  SSMAX   (output) DOUBLE PRECISION
*          The larger singular value.
*
*  Further Details
*  ===============
*
*  Barring over/underflow, all output quantities are correct to within
*  a few units in the last place (ulps), even in the absence of a guard
*  digit in addition/subtraction.
*
*  In IEEE arithmetic, the code works correctly if one matrix element is
*  infinite.
*
*  Overflow will not occur unless the largest singular value itself
*  overflows, or is within a few ulps of overflow. (On machines with
*  partial overflow, like the Cray, overflow may occur if the largest
*  singular value is within a factor of 2 of overflow.)
*
*  Underflow is harmless if underflow is gradual. Otherwise, results
*  may correspond to a matrix modified by perturbations of size near
*  the underflow threshold.
*
*  ====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AS, AT, AU, C, FA, FHMN, FHMX, GA, HA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      FA = ABS( F )
      GA = ABS( G )
      HA = ABS( H )
      FHMN = MIN( FA, HA )
      FHMX = MAX( FA, HA )
      IF( FHMN.EQ.ZERO ) THEN
         SSMIN = ZERO
         IF( FHMX.EQ.ZERO ) THEN
            SSMAX = GA
         ELSE
            SSMAX = MAX( FHMX, GA )*SQRT( ONE+
     $              ( MIN( FHMX, GA ) / MAX( FHMX, GA ) )**2 )
         END IF
      ELSE
         IF( GA.LT.FHMX ) THEN
            AS = ONE + FHMN / FHMX
            AT = ( FHMX-FHMN ) / FHMX
            AU = ( GA / FHMX )**2
            C = TWO / ( SQRT( AS*AS+AU )+SQRT( AT*AT+AU ) )
            SSMIN = FHMN*C
            SSMAX = FHMX / C
         ELSE
            AU = FHMX / GA
            IF( AU.EQ.ZERO ) THEN
*
*              Avoid possible harmful underflow if exponent range
*              asymmetric (true SSMIN may not underflow even if
*              AU underflows)
*
               SSMIN = ( FHMN*FHMX ) / GA
               SSMAX = GA
            ELSE
               AS = ONE + FHMN / FHMX
               AT = ( FHMX-FHMN ) / FHMX
               C = ONE / ( SQRT( ONE+( AS*AU )**2 )+
     $             SQRT( ONE+( AT*AU )**2 ) )
               SSMIN = ( FHMN*C )*AU
               SSMIN = SSMIN + SSMIN
               SSMAX = GA / ( C+C )
            END IF
         END IF
      END IF
      RETURN
*
*     End of DLAS2
*
      END
      SUBROUTINE DLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      DOUBLE PRECISION   CFROM, CTO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASCL multiplies the M by N real matrix A by the real scalar
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
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
         CALL XERBLA( 'DLASCL', -INFO )
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
*     End of DLASCL
*
      END
      SUBROUTINE DLASD0( N, SQRE, D, E, U, LDU, VT, LDVT, SMLSIZ, IWORK,
     $                   WORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDU, LDVT, N, SMLSIZ, SQRE
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), U( LDU, * ), VT( LDVT, * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  Using a divide and conquer approach, DLASD0 computes the singular
*  value decomposition (SVD) of a real upper bidiagonal N-by-M
*  matrix B with diagonal D and offdiagonal E, where M = N + SQRE.
*  The algorithm computes orthogonal matrices U and VT such that
*  B = U * S * VT. The singular values S are overwritten on D.
*
*  A related subroutine, DLASDA, computes only the singular values,
*  and optionally, the singular vectors in compact form.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         On entry, the row dimension of the upper bidiagonal matrix.
*         This is also the dimension of the main diagonal array D.
*
*  SQRE   (input) INTEGER
*         Specifies the column dimension of the bidiagonal matrix.
*         = 0: The bidiagonal matrix has column dimension M = N;
*         = 1: The bidiagonal matrix has column dimension M = N+1;
*
*  D      (input/output) DOUBLE PRECISION array, dimension (N)
*         On entry D contains the main diagonal of the bidiagonal
*         matrix.
*         On exit D, if INFO = 0, contains its singular values.
*
*  E      (input) DOUBLE PRECISION array, dimension (M-1)
*         Contains the subdiagonal entries of the bidiagonal matrix.
*         On exit, E has been destroyed.
*
*  U      (output) DOUBLE PRECISION array, dimension at least (LDQ, N)
*         On exit, U contains the left singular vectors.
*
*  LDU    (input) INTEGER
*         On entry, leading dimension of U.
*
*  VT     (output) DOUBLE PRECISION array, dimension at least (LDVT, M)
*         On exit, VT' contains the right singular vectors.
*
*  LDVT   (input) INTEGER
*         On entry, leading dimension of VT.
*
*  SMLSIZ (input) INTEGER
*         On entry, maximum size of the subproblems at the
*         bottom of the computation tree.
*
*  IWORK  (workspace) INTEGER work array.
*         Dimension must be at least (8 * N)
*
*  WORK   (workspace) DOUBLE PRECISION work array.
*         Dimension must be at least (3 * M**2 + 2 * M)
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, I1, IC, IDXQ, IDXQC, IM1, INODE, ITEMP, IWK,
     $                   J, LF, LL, LVL, M, NCC, ND, NDB1, NDIML, NDIMR,
     $                   NL, NLF, NLP1, NLVL, NR, NRF, NRP1, SQREI
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASD1, DLASDQ, DLASDT, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -2
      END IF
*
      M = N + SQRE
*
      IF( LDU.LT.N ) THEN
         INFO = -6
      ELSE IF( LDVT.LT.M ) THEN
         INFO = -8
      ELSE IF( SMLSIZ.LT.3 ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD0', -INFO )
         RETURN
      END IF
*
*     If the input matrix is too small, call DLASDQ to find the SVD.
*
      IF( N.LE.SMLSIZ ) THEN
         CALL DLASDQ( 'U', SQRE, N, M, N, 0, D, E, VT, LDVT, U, LDU, U,
     $                LDU, WORK, INFO )
         RETURN
      END IF
*
*     Set up the computation tree.
*
      INODE = 1
      NDIML = INODE + N
      NDIMR = NDIML + N
      IDXQ = NDIMR + N
      IWK = IDXQ + N
      CALL DLASDT( N, NLVL, ND, IWORK( INODE ), IWORK( NDIML ),
     $             IWORK( NDIMR ), SMLSIZ )
*
*     For the nodes on bottom level of the tree, solve
*     their subproblems by DLASDQ.
*
      NDB1 = ( ND+1 ) / 2
      NCC = 0
      DO 30 I = NDB1, ND
*
*     IC : center row of each node
*     NL : number of rows of left  subproblem
*     NR : number of rows of right subproblem
*     NLF: starting row of the left   subproblem
*     NRF: starting row of the right  subproblem
*
         I1 = I - 1
         IC = IWORK( INODE+I1 )
         NL = IWORK( NDIML+I1 )
         NLP1 = NL + 1
         NR = IWORK( NDIMR+I1 )
         NRP1 = NR + 1
         NLF = IC - NL
         NRF = IC + 1
         SQREI = 1
         CALL DLASDQ( 'U', SQREI, NL, NLP1, NL, NCC, D( NLF ), E( NLF ),
     $                VT( NLF, NLF ), LDVT, U( NLF, NLF ), LDU,
     $                U( NLF, NLF ), LDU, WORK, INFO )
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         ITEMP = IDXQ + NLF - 2
         DO 10 J = 1, NL
            IWORK( ITEMP+J ) = J
   10    CONTINUE
         IF( I.EQ.ND ) THEN
            SQREI = SQRE
         ELSE
            SQREI = 1
         END IF
         NRP1 = NR + SQREI
         CALL DLASDQ( 'U', SQREI, NR, NRP1, NR, NCC, D( NRF ), E( NRF ),
     $                VT( NRF, NRF ), LDVT, U( NRF, NRF ), LDU,
     $                U( NRF, NRF ), LDU, WORK, INFO )
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         ITEMP = IDXQ + IC
         DO 20 J = 1, NR
            IWORK( ITEMP+J-1 ) = J
   20    CONTINUE
   30 CONTINUE
*
*     Now conquer each subproblem bottom-up.
*
      DO 50 LVL = NLVL, 1, -1
*
*        Find the first node LF and last node LL on the
*        current level LVL.
*
         IF( LVL.EQ.1 ) THEN
            LF = 1
            LL = 1
         ELSE
            LF = 2**( LVL-1 )
            LL = 2*LF - 1
         END IF
         DO 40 I = LF, LL
            IM1 = I - 1
            IC = IWORK( INODE+IM1 )
            NL = IWORK( NDIML+IM1 )
            NR = IWORK( NDIMR+IM1 )
            NLF = IC - NL
            IF( ( SQRE.EQ.0 ) .AND. ( I.EQ.LL ) ) THEN
               SQREI = SQRE
            ELSE
               SQREI = 1
            END IF
            IDXQC = IDXQ + NLF - 1
            ALPHA = D( IC )
            BETA = E( IC )
            CALL DLASD1( NL, NR, SQREI, D( NLF ), ALPHA, BETA,
     $                   U( NLF, NLF ), LDU, VT( NLF, NLF ), LDVT,
     $                   IWORK( IDXQC ), IWORK( IWK ), WORK, INFO )
            IF( INFO.NE.0 ) THEN
               RETURN
            END IF
   40    CONTINUE
   50 CONTINUE
*
      RETURN
*
*     End of DLASD0
*
      END
      SUBROUTINE DLASD1( NL, NR, SQRE, D, ALPHA, BETA, U, LDU, VT, LDVT,
     $                   IDXQ, IWORK, WORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDU, LDVT, NL, NR, SQRE
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      INTEGER            IDXQ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), U( LDU, * ), VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASD1 computes the SVD of an upper bidiagonal N-by-M matrix B,
*  where N = NL + NR + 1 and M = N + SQRE. DLASD1 is called from DLASD0.
*
*  A related subroutine DLASD7 handles the case in which the singular
*  values (and the singular vectors in factored form) are desired.
*
*  DLASD1 computes the SVD as follows:
*
*                ( D1(in)  0    0     0 )
*    B = U(in) * (   Z1'   a   Z2'    b ) * VT(in)
*                (   0     0   D2(in) 0 )
*
*      = U(out) * ( D(out) 0) * VT(out)
*
*  where Z' = (Z1' a Z2' b) = u' VT', and u is a vector of dimension M
*  with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
*  elsewhere; and the entry b is empty if SQRE = 0.
*
*  The left singular vectors of the original matrix are stored in U, and
*  the transpose of the right singular vectors are stored in VT, and the
*  singular values are in D.  The algorithm consists of three stages:
*
*     The first stage consists of deflating the size of the problem
*     when there are multiple singular values or when there are zeros in
*     the Z vector.  For each such occurence the dimension of the
*     secular equation problem is reduced by one.  This stage is
*     performed by the routine DLASD2.
*
*     The second stage consists of calculating the updated
*     singular values. This is done by finding the square roots of the
*     roots of the secular equation via the routine DLASD4 (as called
*     by DLASD3). This routine also calculates the singular vectors of
*     the current problem.
*
*     The final stage consists of computing the updated singular vectors
*     directly using the updated singular values.  The singular vectors
*     for the current problem are multiplied with the singular vectors
*     from the overall problem.
*
*  Arguments
*  =========
*
*  NL     (input) INTEGER
*         The row dimension of the upper block.  NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block.  NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has row dimension N = NL + NR + 1,
*         and column dimension M = N + SQRE.
*
*  D      (input/output) DOUBLE PRECISION array,
*                        dimension (N = NL+NR+1).
*         On entry D(1:NL,1:NL) contains the singular values of the
*         upper block; and D(NL+2:N) contains the singular values of
*         the lower block. On exit D(1:N) contains the singular values
*         of the modified matrix.
*
*  ALPHA  (input/output) DOUBLE PRECISION
*         Contains the diagonal element associated with the added row.
*
*  BETA   (input/output) DOUBLE PRECISION
*         Contains the off-diagonal element associated with the added
*         row.
*
*  U      (input/output) DOUBLE PRECISION array, dimension(LDU,N)
*         On entry U(1:NL, 1:NL) contains the left singular vectors of
*         the upper block; U(NL+2:N, NL+2:N) contains the left singular
*         vectors of the lower block. On exit U contains the left
*         singular vectors of the bidiagonal matrix.
*
*  LDU    (input) INTEGER
*         The leading dimension of the array U.  LDU >= max( 1, N ).
*
*  VT     (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
*         where M = N + SQRE.
*         On entry VT(1:NL+1, 1:NL+1)' contains the right singular
*         vectors of the upper block; VT(NL+2:M, NL+2:M)' contains
*         the right singular vectors of the lower block. On exit
*         VT' contains the right singular vectors of the
*         bidiagonal matrix.
*
*  LDVT   (input) INTEGER
*         The leading dimension of the array VT.  LDVT >= max( 1, M ).
*
*  IDXQ  (output) INTEGER array, dimension(N)
*         This contains the permutation which will reintegrate the
*         subproblem just solved back into sorted order, i.e.
*         D( IDXQ( I = 1, N ) ) will be in ascending order.
*
*  IWORK  (workspace) INTEGER array, dimension( 4 * N )
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension( 3*M**2 + 2*M )
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
*
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            COLTYP, I, IDX, IDXC, IDXP, IQ, ISIGMA, IU2,
     $                   IVT2, IZ, K, LDQ, LDU2, LDVT2, M, N, N1, N2
      DOUBLE PRECISION   ORGNRM
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAMRG, DLASCL, DLASD2, DLASD3, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( NL.LT.1 ) THEN
         INFO = -1
      ELSE IF( NR.LT.1 ) THEN
         INFO = -2
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -3
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD1', -INFO )
         RETURN
      END IF
*
      N = NL + NR + 1
      M = N + SQRE
*
*     The following values are for bookkeeping purposes only.  They are
*     integer pointers which indicate the portion of the workspace
*     used by a particular array in DLASD2 and DLASD3.
*
      LDU2 = N
      LDVT2 = M
*
      IZ = 1
      ISIGMA = IZ + M
      IU2 = ISIGMA + N
      IVT2 = IU2 + LDU2*N
      IQ = IVT2 + LDVT2*M
*
      IDX = 1
      IDXC = IDX + N
      COLTYP = IDXC + N
      IDXP = COLTYP + N
*
*     Scale.
*
      ORGNRM = MAX( ABS( ALPHA ), ABS( BETA ) )
      D( NL+1 ) = ZERO
      DO 10 I = 1, N
         IF( ABS( D( I ) ).GT.ORGNRM ) THEN
            ORGNRM = ABS( D( I ) )
         END IF
   10 CONTINUE
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, N, 1, D, N, INFO )
      ALPHA = ALPHA / ORGNRM
      BETA = BETA / ORGNRM
*
*     Deflate singular values.
*
      CALL DLASD2( NL, NR, SQRE, K, D, WORK( IZ ), ALPHA, BETA, U, LDU,
     $             VT, LDVT, WORK( ISIGMA ), WORK( IU2 ), LDU2,
     $             WORK( IVT2 ), LDVT2, IWORK( IDXP ), IWORK( IDX ),
     $             IWORK( IDXC ), IDXQ, IWORK( COLTYP ), INFO )
*
*     Solve Secular Equation and update singular vectors.
*
      LDQ = K
      CALL DLASD3( NL, NR, SQRE, K, D, WORK( IQ ), LDQ, WORK( ISIGMA ),
     $             U, LDU, WORK( IU2 ), LDU2, VT, LDVT, WORK( IVT2 ),
     $             LDVT2, IWORK( IDXC ), IWORK( COLTYP ), WORK( IZ ),
     $             INFO )
      IF( INFO.NE.0 ) THEN
         RETURN
      END IF
*
*     Unscale.
*
      CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, N, 1, D, N, INFO )
*
*     Prepare the IDXQ sorting permutation.
*
      N1 = K
      N2 = N - K
      CALL DLAMRG( N1, N2, D, 1, -1, IDXQ )
*
      RETURN
*
*     End of DLASD1
*
      END
      SUBROUTINE DLASD2( NL, NR, SQRE, K, D, Z, ALPHA, BETA, U, LDU, VT,
     $                   LDVT, DSIGMA, U2, LDU2, VT2, LDVT2, IDXP, IDX,
     $                   IDXC, IDXQ, COLTYP, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDU, LDU2, LDVT, LDVT2, NL, NR, SQRE
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      INTEGER            COLTYP( * ), IDX( * ), IDXC( * ), IDXP( * ),
     $                   IDXQ( * )
      DOUBLE PRECISION   D( * ), DSIGMA( * ), U( LDU, * ),
     $                   U2( LDU2, * ), VT( LDVT, * ), VT2( LDVT2, * ),
     $                   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASD2 merges the two sets of singular values together into a single
*  sorted set.  Then it tries to deflate the size of the problem.
*  There are two ways in which deflation can occur:  when two or more
*  singular values are close together or if there is a tiny entry in the
*  Z vector.  For each such occurrence the order of the related secular
*  equation problem is reduced by one.
*
*  DLASD2 is called from DLASD1.
*
*  Arguments
*  =========
*
*  NL     (input) INTEGER
*         The row dimension of the upper block.  NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block.  NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has N = NL + NR + 1 rows and
*         M = N + SQRE >= N columns.
*
*  K      (output) INTEGER
*         Contains the dimension of the non-deflated matrix,
*         This is the order of the related secular equation. 1 <= K <=N.
*
*  D      (input/output) DOUBLE PRECISION array, dimension(N)
*         On entry D contains the singular values of the two submatrices
*         to be combined.  On exit D contains the trailing (N-K) updated
*         singular values (those which were deflated) sorted into
*         increasing order.
*
*  Z      (output) DOUBLE PRECISION array, dimension(N)
*         On exit Z contains the updating row vector in the secular
*         equation.
*
*  ALPHA  (input) DOUBLE PRECISION
*         Contains the diagonal element associated with the added row.
*
*  BETA   (input) DOUBLE PRECISION
*         Contains the off-diagonal element associated with the added
*         row.
*
*  U      (input/output) DOUBLE PRECISION array, dimension(LDU,N)
*         On entry U contains the left singular vectors of two
*         submatrices in the two square blocks with corners at (1,1),
*         (NL, NL), and (NL+2, NL+2), (N,N).
*         On exit U contains the trailing (N-K) updated left singular
*         vectors (those which were deflated) in its last N-K columns.
*
*  LDU    (input) INTEGER
*         The leading dimension of the array U.  LDU >= N.
*
*  VT     (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
*         On entry VT' contains the right singular vectors of two
*         submatrices in the two square blocks with corners at (1,1),
*         (NL+1, NL+1), and (NL+2, NL+2), (M,M).
*         On exit VT' contains the trailing (N-K) updated right singular
*         vectors (those which were deflated) in its last N-K columns.
*         In case SQRE =1, the last row of VT spans the right null
*         space.
*
*  LDVT   (input) INTEGER
*         The leading dimension of the array VT.  LDVT >= M.
*
*  DSIGMA (output) DOUBLE PRECISION array, dimension (N)
*         Contains a copy of the diagonal elements (K-1 singular values
*         and one zero) in the secular equation.
*
*  U2     (output) DOUBLE PRECISION array, dimension(LDU2,N)
*         Contains a copy of the first K-1 left singular vectors which
*         will be used by DLASD3 in a matrix multiply (DGEMM) to solve
*         for the new left singular vectors. U2 is arranged into four
*         blocks. The first block contains a column with 1 at NL+1 and
*         zero everywhere else; the second block contains non-zero
*         entries only at and above NL; the third contains non-zero
*         entries only below NL+1; and the fourth is dense.
*
*  LDU2   (input) INTEGER
*         The leading dimension of the array U2.  LDU2 >= N.
*
*  VT2    (output) DOUBLE PRECISION array, dimension(LDVT2,N)
*         VT2' contains a copy of the first K right singular vectors
*         which will be used by DLASD3 in a matrix multiply (DGEMM) to
*         solve for the new right singular vectors. VT2 is arranged into
*         three blocks. The first block contains a row that corresponds
*         to the special 0 diagonal element in SIGMA; the second block
*         contains non-zeros only at and before NL +1; the third block
*         contains non-zeros only at and after  NL +2.
*
*  LDVT2  (input) INTEGER
*         The leading dimension of the array VT2.  LDVT2 >= M.
*
*  IDXP   (workspace) INTEGER array dimension(N)
*         This will contain the permutation used to place deflated
*         values of D at the end of the array. On output IDXP(2:K)
*         points to the nondeflated D-values and IDXP(K+1:N)
*         points to the deflated singular values.
*
*  IDX    (workspace) INTEGER array dimension(N)
*         This will contain the permutation used to sort the contents of
*         D into ascending order.
*
*  IDXC   (output) INTEGER array dimension(N)
*         This will contain the permutation used to arrange the columns
*         of the deflated U matrix into three groups:  the first group
*         contains non-zero entries only at and above NL, the second
*         contains non-zero entries only below NL+2, and the third is
*         dense.
*
*  IDXQ   (input/output) INTEGER array dimension(N)
*         This contains the permutation which separately sorts the two
*         sub-problems in D into ascending order.  Note that entries in
*         the first hlaf of this permutation must first be moved one
*         position backward; and entries in the second half
*         must first have NL+1 added to their values.
*
*  COLTYP (workspace/output) INTEGER array dimension(N)
*         As workspace, this will contain a label which will indicate
*         which of the following types a column in the U2 matrix or a
*         row in the VT2 matrix is:
*         1 : non-zero in the upper half only
*         2 : non-zero in the lower half only
*         3 : dense
*         4 : deflated
*
*         On exit, it is an array of dimension 4, with COLTYP(I) being
*         the dimension of the I-th type columns.
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, EIGHT
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0,
     $                   EIGHT = 8.0D+0 )
*     ..
*     .. Local Arrays ..
      INTEGER            CTOT( 4 ), PSM( 4 )
*     ..
*     .. Local Scalars ..
      INTEGER            CT, I, IDXI, IDXJ, IDXJP, J, JP, JPREV, K2, M,
     $                   N, NLP1, NLP2
      DOUBLE PRECISION   C, EPS, HLFTOL, S, TAU, TOL, Z1
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DLAMRG, DLASET, DROT, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( NL.LT.1 ) THEN
         INFO = -1
      ELSE IF( NR.LT.1 ) THEN
         INFO = -2
      ELSE IF( ( SQRE.NE.1 ) .AND. ( SQRE.NE.0 ) ) THEN
         INFO = -3
      END IF
*
      N = NL + NR + 1
      M = N + SQRE
*
      IF( LDU.LT.N ) THEN
         INFO = -10
      ELSE IF( LDVT.LT.M ) THEN
         INFO = -12
      ELSE IF( LDU2.LT.N ) THEN
         INFO = -15
      ELSE IF( LDVT2.LT.M ) THEN
         INFO = -17
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD2', -INFO )
         RETURN
      END IF
*
      NLP1 = NL + 1
      NLP2 = NL + 2
*
*     Generate the first part of the vector Z; and move the singular
*     values in the first part of D one position backward.
*
      Z1 = ALPHA*VT( NLP1, NLP1 )
      Z( 1 ) = Z1
      DO 10 I = NL, 1, -1
         Z( I+1 ) = ALPHA*VT( I, NLP1 )
         D( I+1 ) = D( I )
         IDXQ( I+1 ) = IDXQ( I ) + 1
   10 CONTINUE
*
*     Generate the second part of the vector Z.
*
      DO 20 I = NLP2, M
         Z( I ) = BETA*VT( I, NLP2 )
   20 CONTINUE
*
*     Initialize some reference arrays.
*
      DO 30 I = 2, NLP1
         COLTYP( I ) = 1
   30 CONTINUE
      DO 40 I = NLP2, N
         COLTYP( I ) = 2
   40 CONTINUE
*
*     Sort the singular values into increasing order
*
      DO 50 I = NLP2, N
         IDXQ( I ) = IDXQ( I ) + NLP1
   50 CONTINUE
*
*     DSIGMA, IDXC, IDXC, and the first column of U2
*     are used as storage space.
*
      DO 60 I = 2, N
         DSIGMA( I ) = D( IDXQ( I ) )
         U2( I, 1 ) = Z( IDXQ( I ) )
         IDXC( I ) = COLTYP( IDXQ( I ) )
   60 CONTINUE
*
      CALL DLAMRG( NL, NR, DSIGMA( 2 ), 1, 1, IDX( 2 ) )
*
      DO 70 I = 2, N
         IDXI = 1 + IDX( I )
         D( I ) = DSIGMA( IDXI )
         Z( I ) = U2( IDXI, 1 )
         COLTYP( I ) = IDXC( IDXI )
   70 CONTINUE
*
*     Calculate the allowable deflation tolerance
*
      EPS = DLAMCH( 'Epsilon' )
      TOL = MAX( ABS( ALPHA ), ABS( BETA ) )
      TOL = EIGHT*EPS*MAX( ABS( D( N ) ), TOL )
*
*     There are 2 kinds of deflation -- first a value in the z-vector
*     is small, second two (or more) singular values are very close
*     together (their difference is small).
*
*     If the value in the z-vector is small, we simply permute the
*     array so that the corresponding singular value is moved to the
*     end.
*
*     If two values in the D-vector are close, we perform a two-sided
*     rotation designed to make one of the corresponding z-vector
*     entries zero, and then permute the array so that the deflated
*     singular value is moved to the end.
*
*     If there are multiple singular values then the problem deflates.
*     Here the number of equal singular values are found.  As each equal
*     singular value is found, an elementary reflector is computed to
*     rotate the corresponding singular subspace so that the
*     corresponding components of Z are zero in this new basis.
*
      K = 1
      K2 = N + 1
      DO 80 J = 2, N
         IF( ABS( Z( J ) ).LE.TOL ) THEN
*
*           Deflate due to small z component.
*
            K2 = K2 - 1
            IDXP( K2 ) = J
            COLTYP( J ) = 4
            IF( J.EQ.N )
     $         GO TO 120
         ELSE
            JPREV = J
            GO TO 90
         END IF
   80 CONTINUE
   90 CONTINUE
      J = JPREV
  100 CONTINUE
      J = J + 1
      IF( J.GT.N )
     $   GO TO 110
      IF( ABS( Z( J ) ).LE.TOL ) THEN
*
*        Deflate due to small z component.
*
         K2 = K2 - 1
         IDXP( K2 ) = J
         COLTYP( J ) = 4
      ELSE
*
*        Check if singular values are close enough to allow deflation.
*
         IF( ABS( D( J )-D( JPREV ) ).LE.TOL ) THEN
*
*           Deflation is possible.
*
            S = Z( JPREV )
            C = Z( J )
*
*           Find sqrt(a**2+b**2) without overflow or
*           destructive underflow.
*
            TAU = DLAPY2( C, S )
            C = C / TAU
            S = -S / TAU
            Z( J ) = TAU
            Z( JPREV ) = ZERO
*
*           Apply back the Givens rotation to the left and right
*           singular vector matrices.
*
            IDXJP = IDXQ( IDX( JPREV )+1 )
            IDXJ = IDXQ( IDX( J )+1 )
            IF( IDXJP.LE.NLP1 ) THEN
               IDXJP = IDXJP - 1
            END IF
            IF( IDXJ.LE.NLP1 ) THEN
               IDXJ = IDXJ - 1
            END IF
            CALL DROT( N, U( 1, IDXJP ), 1, U( 1, IDXJ ), 1, C, S )
            CALL DROT( M, VT( IDXJP, 1 ), LDVT, VT( IDXJ, 1 ), LDVT, C,
     $                 S )
            IF( COLTYP( J ).NE.COLTYP( JPREV ) ) THEN
               COLTYP( J ) = 3
            END IF
            COLTYP( JPREV ) = 4
            K2 = K2 - 1
            IDXP( K2 ) = JPREV
            JPREV = J
         ELSE
            K = K + 1
            U2( K, 1 ) = Z( JPREV )
            DSIGMA( K ) = D( JPREV )
            IDXP( K ) = JPREV
            JPREV = J
         END IF
      END IF
      GO TO 100
  110 CONTINUE
*
*     Record the last singular value.
*
      K = K + 1
      U2( K, 1 ) = Z( JPREV )
      DSIGMA( K ) = D( JPREV )
      IDXP( K ) = JPREV
*
  120 CONTINUE
*
*     Count up the total number of the various types of columns, then
*     form a permutation which positions the four column types into
*     four groups of uniform structure (although one or more of these
*     groups may be empty).
*
      DO 130 J = 1, 4
         CTOT( J ) = 0
  130 CONTINUE
      DO 140 J = 2, N
         CT = COLTYP( J )
         CTOT( CT ) = CTOT( CT ) + 1
  140 CONTINUE
*
*     PSM(*) = Position in SubMatrix (of types 1 through 4)
*
      PSM( 1 ) = 2
      PSM( 2 ) = 2 + CTOT( 1 )
      PSM( 3 ) = PSM( 2 ) + CTOT( 2 )
      PSM( 4 ) = PSM( 3 ) + CTOT( 3 )
*
*     Fill out the IDXC array so that the permutation which it induces
*     will place all type-1 columns first, all type-2 columns next,
*     then all type-3's, and finally all type-4's, starting from the
*     second column. This applies similarly to the rows of VT.
*
      DO 150 J = 2, N
         JP = IDXP( J )
         CT = COLTYP( JP )
         IDXC( PSM( CT ) ) = J
         PSM( CT ) = PSM( CT ) + 1
  150 CONTINUE
*
*     Sort the singular values and corresponding singular vectors into
*     DSIGMA, U2, and VT2 respectively.  The singular values/vectors
*     which were not deflated go into the first K slots of DSIGMA, U2,
*     and VT2 respectively, while those which were deflated go into the
*     last N - K slots, except that the first column/row will be treated
*     separately.
*
      DO 160 J = 2, N
         JP = IDXP( J )
         DSIGMA( J ) = D( JP )
         IDXJ = IDXQ( IDX( IDXP( IDXC( J ) ) )+1 )
         IF( IDXJ.LE.NLP1 ) THEN
            IDXJ = IDXJ - 1
         END IF
         CALL DCOPY( N, U( 1, IDXJ ), 1, U2( 1, J ), 1 )
         CALL DCOPY( M, VT( IDXJ, 1 ), LDVT, VT2( J, 1 ), LDVT2 )
  160 CONTINUE
*
*     Determine DSIGMA(1), DSIGMA(2) and Z(1)
*
      DSIGMA( 1 ) = ZERO
      HLFTOL = TOL / TWO
      IF( ABS( DSIGMA( 2 ) ).LE.HLFTOL )
     $   DSIGMA( 2 ) = HLFTOL
      IF( M.GT.N ) THEN
         Z( 1 ) = DLAPY2( Z1, Z( M ) )
         IF( Z( 1 ).LE.TOL ) THEN
            C = ONE
            S = ZERO
            Z( 1 ) = TOL
         ELSE
            C = Z1 / Z( 1 )
            S = Z( M ) / Z( 1 )
         END IF
      ELSE
         IF( ABS( Z1 ).LE.TOL ) THEN
            Z( 1 ) = TOL
         ELSE
            Z( 1 ) = Z1
         END IF
      END IF
*
*     Move the rest of the updating row to Z.
*
      CALL DCOPY( K-1, U2( 2, 1 ), 1, Z( 2 ), 1 )
*
*     Determine the first column of U2, the first row of VT2 and the
*     last row of VT.
*
      CALL DLASET( 'A', N, 1, ZERO, ZERO, U2, LDU2 )
      U2( NLP1, 1 ) = ONE
      IF( M.GT.N ) THEN
         DO 170 I = 1, NLP1
            VT( M, I ) = -S*VT( NLP1, I )
            VT2( 1, I ) = C*VT( NLP1, I )
  170    CONTINUE
         DO 180 I = NLP2, M
            VT2( 1, I ) = S*VT( M, I )
            VT( M, I ) = C*VT( M, I )
  180    CONTINUE
      ELSE
         CALL DCOPY( M, VT( NLP1, 1 ), LDVT, VT2( 1, 1 ), LDVT2 )
      END IF
      IF( M.GT.N ) THEN
         CALL DCOPY( M, VT( M, 1 ), LDVT, VT2( M, 1 ), LDVT2 )
      END IF
*
*     The deflated singular values and their corresponding vectors go
*     into the back of D, U, and V respectively.
*
      IF( N.GT.K ) THEN
         CALL DCOPY( N-K, DSIGMA( K+1 ), 1, D( K+1 ), 1 )
         CALL DLACPY( 'A', N, N-K, U2( 1, K+1 ), LDU2, U( 1, K+1 ),
     $                LDU )
         CALL DLACPY( 'A', N-K, M, VT2( K+1, 1 ), LDVT2, VT( K+1, 1 ),
     $                LDVT )
      END IF
*
*     Copy CTOT into COLTYP for referencing in DLASD3.
*
      DO 190 J = 1, 4
         COLTYP( J ) = CTOT( J )
  190 CONTINUE
*
      RETURN
*
*     End of DLASD2
*
      END
      SUBROUTINE DLASD3( NL, NR, SQRE, K, D, Q, LDQ, DSIGMA, U, LDU, U2,
     $                   LDU2, VT, LDVT, VT2, LDVT2, IDXC, CTOT, Z,
     $                   INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, K, LDQ, LDU, LDU2, LDVT, LDVT2, NL, NR,
     $                   SQRE
*     ..
*     .. Array Arguments ..
      INTEGER            CTOT( * ), IDXC( * )
      DOUBLE PRECISION   D( * ), DSIGMA( * ), Q( LDQ, * ), U( LDU, * ),
     $                   U2( LDU2, * ), VT( LDVT, * ), VT2( LDVT2, * ),
     $                   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASD3 finds all the square roots of the roots of the secular
*  equation, as defined by the values in D and Z.  It makes the
*  appropriate calls to DLASD4 and then updates the singular
*  vectors by matrix multiplication.
*
*  This code makes very mild assumptions about floating point
*  arithmetic. It will work on machines with a guard digit in
*  add/subtract, or on those binary machines without guard digits
*  which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
*  It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  DLASD3 is called from DLASD1.
*
*  Arguments
*  =========
*
*  NL     (input) INTEGER
*         The row dimension of the upper block.  NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block.  NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has N = NL + NR + 1 rows and
*         M = N + SQRE >= N columns.
*
*  K      (input) INTEGER
*         The size of the secular equation, 1 =< K = < N.
*
*  D      (output) DOUBLE PRECISION array, dimension(K)
*         On exit the square roots of the roots of the secular equation,
*         in ascending order.
*
*  Q      (workspace) DOUBLE PRECISION array,
*                     dimension at least (LDQ,K).
*
*  LDQ    (input) INTEGER
*         The leading dimension of the array Q.  LDQ >= K.
*
*  DSIGMA (input) DOUBLE PRECISION array, dimension(K)
*         The first K elements of this array contain the old roots
*         of the deflated updating problem.  These are the poles
*         of the secular equation.
*
*  U      (output) DOUBLE PRECISION array, dimension (LDU, N)
*         The last N - K columns of this matrix contain the deflated
*         left singular vectors.
*
*  LDU    (input) INTEGER
*         The leading dimension of the array U.  LDU >= N.
*
*  U2     (input/output) DOUBLE PRECISION array, dimension (LDU2, N)
*         The first K columns of this matrix contain the non-deflated
*         left singular vectors for the split problem.
*
*  LDU2   (input) INTEGER
*         The leading dimension of the array U2.  LDU2 >= N.
*
*  VT     (output) DOUBLE PRECISION array, dimension (LDVT, M)
*         The last M - K columns of VT' contain the deflated
*         right singular vectors.
*
*  LDVT   (input) INTEGER
*         The leading dimension of the array VT.  LDVT >= N.
*
*  VT2    (input/output) DOUBLE PRECISION array, dimension (LDVT2, N)
*         The first K columns of VT2' contain the non-deflated
*         right singular vectors for the split problem.
*
*  LDVT2  (input) INTEGER
*         The leading dimension of the array VT2.  LDVT2 >= N.
*
*  IDXC   (input) INTEGER array, dimension ( N )
*         The permutation used to arrange the columns of U (and rows of
*         VT) into three groups:  the first group contains non-zero
*         entries only at and above (or before) NL +1; the second
*         contains non-zero entries only at and below (or after) NL+2;
*         and the third is dense. The first column of U and the row of
*         VT are treated separately, however.
*
*         The rows of the singular vectors found by DLASD4
*         must be likewise permuted before the matrix multiplies can
*         take place.
*
*  CTOT   (input) INTEGER array, dimension ( 4 )
*         A count of the total number of the various types of columns
*         in U (or rows in VT), as described in IDXC. The fourth column
*         type is any column which has been deflated.
*
*  Z      (input) DOUBLE PRECISION array, dimension (K)
*         The first K elements of this array contain the components
*         of the deflation-adjusted updating row vector.
*
*  INFO   (output) INTEGER
*         = 0:  successful exit.
*         < 0:  if INFO = -i, the i-th argument had an illegal value.
*         > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO, NEGONE
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0,
     $                   NEGONE = -1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            CTEMP, I, J, JC, KTEMP, M, N, NLP1, NLP2, NRP1
      DOUBLE PRECISION   RHO, TEMP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMC3, DNRM2
      EXTERNAL           DLAMC3, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DLACPY, DLASCL, DLASD4, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( NL.LT.1 ) THEN
         INFO = -1
      ELSE IF( NR.LT.1 ) THEN
         INFO = -2
      ELSE IF( ( SQRE.NE.1 ) .AND. ( SQRE.NE.0 ) ) THEN
         INFO = -3
      END IF
*
      N = NL + NR + 1
      M = N + SQRE
      NLP1 = NL + 1
      NLP2 = NL + 2
*
      IF( ( K.LT.1 ) .OR. ( K.GT.N ) ) THEN
         INFO = -4
      ELSE IF( LDQ.LT.K ) THEN
         INFO = -7
      ELSE IF( LDU.LT.N ) THEN
         INFO = -10
      ELSE IF( LDU2.LT.N ) THEN
         INFO = -12
      ELSE IF( LDVT.LT.M ) THEN
         INFO = -14
      ELSE IF( LDVT2.LT.M ) THEN
         INFO = -16
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD3', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.1 ) THEN
         D( 1 ) = ABS( Z( 1 ) )
         CALL DCOPY( M, VT2( 1, 1 ), LDVT2, VT( 1, 1 ), LDVT )
         IF( Z( 1 ).GT.ZERO ) THEN
            CALL DCOPY( N, U2( 1, 1 ), 1, U( 1, 1 ), 1 )
         ELSE
            DO 10 I = 1, N
               U( I, 1 ) = -U2( I, 1 )
   10       CONTINUE
         END IF
         RETURN
      END IF
*
*     Modify values DSIGMA(i) to make sure all DSIGMA(i)-DSIGMA(j) can
*     be computed with high relative accuracy (barring over/underflow).
*     This is a problem on machines without a guard digit in
*     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
*     The following code replaces DSIGMA(I) by 2*DSIGMA(I)-DSIGMA(I),
*     which on any of these machines zeros out the bottommost
*     bit of DSIGMA(I) if it is 1; this makes the subsequent
*     subtractions DSIGMA(I)-DSIGMA(J) unproblematic when cancellation
*     occurs. On binary machines with a guard digit (almost all
*     machines) it does not change DSIGMA(I) at all. On hexadecimal
*     and decimal machines with a guard digit, it slightly
*     changes the bottommost bits of DSIGMA(I). It does not account
*     for hexadecimal or decimal machines without guard digits
*     (we know of none). We use a subroutine call to compute
*     2*DSIGMA(I) to prevent optimizing compilers from eliminating
*     this code.
*
      DO 20 I = 1, K
         DSIGMA( I ) = DLAMC3( DSIGMA( I ), DSIGMA( I ) ) - DSIGMA( I )
   20 CONTINUE
*
*     Keep a copy of Z.
*
      CALL DCOPY( K, Z, 1, Q, 1 )
*
*     Normalize Z.
*
      RHO = DNRM2( K, Z, 1 )
      CALL DLASCL( 'G', 0, 0, RHO, ONE, K, 1, Z, K, INFO )
      RHO = RHO*RHO
*
*     Find the new singular values.
*
      DO 30 J = 1, K
         CALL DLASD4( K, J, DSIGMA, Z, U( 1, J ), RHO, D( J ),
     $                VT( 1, J ), INFO )
*
*        If the zero finder fails, the computation is terminated.
*
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
   30 CONTINUE
*
*     Compute updated Z.
*
      DO 60 I = 1, K
         Z( I ) = U( I, K )*VT( I, K )
         DO 40 J = 1, I - 1
            Z( I ) = Z( I )*( U( I, J )*VT( I, J ) /
     $               ( DSIGMA( I )-DSIGMA( J ) ) /
     $               ( DSIGMA( I )+DSIGMA( J ) ) )
   40    CONTINUE
         DO 50 J = I, K - 1
            Z( I ) = Z( I )*( U( I, J )*VT( I, J ) /
     $               ( DSIGMA( I )-DSIGMA( J+1 ) ) /
     $               ( DSIGMA( I )+DSIGMA( J+1 ) ) )
   50    CONTINUE
         Z( I ) = SIGN( SQRT( ABS( Z( I ) ) ), Q( I, 1 ) )
   60 CONTINUE
*
*     Compute left singular vectors of the modified diagonal matrix,
*     and store related information for the right singular vectors.
*
      DO 90 I = 1, K
         VT( 1, I ) = Z( 1 ) / U( 1, I ) / VT( 1, I )
         U( 1, I ) = NEGONE
         DO 70 J = 2, K
            VT( J, I ) = Z( J ) / U( J, I ) / VT( J, I )
            U( J, I ) = DSIGMA( J )*VT( J, I )
   70    CONTINUE
         TEMP = DNRM2( K, U( 1, I ), 1 )
         Q( 1, I ) = U( 1, I ) / TEMP
         DO 80 J = 2, K
            JC = IDXC( J )
            Q( J, I ) = U( JC, I ) / TEMP
   80    CONTINUE
   90 CONTINUE
*
*     Update the left singular vector matrix.
*
      IF( K.EQ.2 ) THEN
         CALL DGEMM( 'N', 'N', N, K, K, ONE, U2, LDU2, Q, LDQ, ZERO, U,
     $               LDU )
         GO TO 100
      END IF
      IF( CTOT( 1 ).GT.0 ) THEN
         CALL DGEMM( 'N', 'N', NL, K, CTOT( 1 ), ONE, U2( 1, 2 ), LDU2,
     $               Q( 2, 1 ), LDQ, ZERO, U( 1, 1 ), LDU )
         IF( CTOT( 3 ).GT.0 ) THEN
            KTEMP = 2 + CTOT( 1 ) + CTOT( 2 )
            CALL DGEMM( 'N', 'N', NL, K, CTOT( 3 ), ONE, U2( 1, KTEMP ),
     $                  LDU2, Q( KTEMP, 1 ), LDQ, ONE, U( 1, 1 ), LDU )
         END IF
      ELSE IF( CTOT( 3 ).GT.0 ) THEN
         KTEMP = 2 + CTOT( 1 ) + CTOT( 2 )
         CALL DGEMM( 'N', 'N', NL, K, CTOT( 3 ), ONE, U2( 1, KTEMP ),
     $               LDU2, Q( KTEMP, 1 ), LDQ, ZERO, U( 1, 1 ), LDU )
      ELSE
         CALL DLACPY( 'F', NL, K, U2, LDU2, U, LDU )
      END IF
      CALL DCOPY( K, Q( 1, 1 ), LDQ, U( NLP1, 1 ), LDU )
      KTEMP = 2 + CTOT( 1 )
      CTEMP = CTOT( 2 ) + CTOT( 3 )
      CALL DGEMM( 'N', 'N', NR, K, CTEMP, ONE, U2( NLP2, KTEMP ), LDU2,
     $            Q( KTEMP, 1 ), LDQ, ZERO, U( NLP2, 1 ), LDU )
*
*     Generate the right singular vectors.
*
  100 CONTINUE
      DO 120 I = 1, K
         TEMP = DNRM2( K, VT( 1, I ), 1 )
         Q( I, 1 ) = VT( 1, I ) / TEMP
         DO 110 J = 2, K
            JC = IDXC( J )
            Q( I, J ) = VT( JC, I ) / TEMP
  110    CONTINUE
  120 CONTINUE
*
*     Update the right singular vector matrix.
*
      IF( K.EQ.2 ) THEN
         CALL DGEMM( 'N', 'N', K, M, K, ONE, Q, LDQ, VT2, LDVT2, ZERO,
     $               VT, LDVT )
         RETURN
      END IF
      KTEMP = 1 + CTOT( 1 )
      CALL DGEMM( 'N', 'N', K, NLP1, KTEMP, ONE, Q( 1, 1 ), LDQ,
     $            VT2( 1, 1 ), LDVT2, ZERO, VT( 1, 1 ), LDVT )
      KTEMP = 2 + CTOT( 1 ) + CTOT( 2 )
      IF( KTEMP.LE.LDVT2 )
     $   CALL DGEMM( 'N', 'N', K, NLP1, CTOT( 3 ), ONE, Q( 1, KTEMP ),
     $               LDQ, VT2( KTEMP, 1 ), LDVT2, ONE, VT( 1, 1 ),
     $               LDVT )
*
      KTEMP = CTOT( 1 ) + 1
      NRP1 = NR + SQRE
      IF( KTEMP.GT.1 ) THEN
         DO 130 I = 1, K
            Q( I, KTEMP ) = Q( I, 1 )
  130    CONTINUE
         DO 140 I = NLP2, M
            VT2( KTEMP, I ) = VT2( 1, I )
  140    CONTINUE
      END IF
      CTEMP = 1 + CTOT( 2 ) + CTOT( 3 )
      CALL DGEMM( 'N', 'N', K, NRP1, CTEMP, ONE, Q( 1, KTEMP ), LDQ,
     $            VT2( KTEMP, NLP2 ), LDVT2, ZERO, VT( 1, NLP2 ), LDVT )
*
      RETURN
*
*     End of DLASD3
*
      END
      SUBROUTINE DLASD4( N, I, D, Z, DELTA, RHO, SIGMA, WORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            I, INFO, N
      DOUBLE PRECISION   RHO, SIGMA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DELTA( * ), WORK( * ), Z( * )
*     ..
*
*  Purpose
*  =======
*
*  This subroutine computes the square root of the I-th updated
*  eigenvalue of a positive symmetric rank-one modification to
*  a positive diagonal matrix whose entries are given as the squares
*  of the corresponding entries in the array d, and that
*
*         0 <= D(i) < D(j)  for  i < j
*
*  and that RHO > 0. This is arranged by the calling routine, and is
*  no loss in generality.  The rank-one modified system is thus
*
*         diag( D ) * diag( D ) +  RHO *  Z * Z_transpose.
*
*  where we assume the Euclidean norm of Z is 1.
*
*  The method consists of approximating the rational functions in the
*  secular equation by simpler interpolating rational functions.
*
*  Arguments
*  =========
*
*  N      (input) INTEGER
*         The length of all arrays.
*
*  I      (input) INTEGER
*         The index of the eigenvalue to be computed.  1 <= I <= N.
*
*  D      (input) DOUBLE PRECISION array, dimension ( N )
*         The original eigenvalues.  It is assumed that they are in
*         order, 0 <= D(I) < D(J)  for I < J.
*
*  Z      (input) DOUBLE PRECISION array, dimension ( N )
*         The components of the updating vector.
*
*  DELTA  (output) DOUBLE PRECISION array, dimension ( N )
*         If N .ne. 1, DELTA contains (D(j) - sigma_I) in its  j-th
*         component.  If N = 1, then DELTA(1) = 1.  The vector DELTA
*         contains the information necessary to construct the
*         (singular) eigenvectors.
*
*  RHO    (input) DOUBLE PRECISION
*         The scalar in the symmetric updating formula.
*
*  SIGMA  (output) DOUBLE PRECISION
*         The computed sigma_I, the I-th updated eigenvalue.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension ( N )
*         If N .ne. 1, WORK contains (D(j) + sigma_I) in its  j-th
*         component.  If N = 1, then WORK( 1 ) = 1.
*
*  INFO   (output) INTEGER
*         = 0:  successful exit
*         > 0:  if INFO = 1, the updating process failed.
*
*  Internal Parameters
*  ===================
*
*  Logical variable ORGATI (origin-at-i?) is used for distinguishing
*  whether D(i) or D(i+1) is treated as the origin.
*
*            ORGATI = .true.    origin at i
*            ORGATI = .false.   origin at i+1
*
*  Logical variable SWTCH3 (switch-for-3-poles?) is for noting
*  if we are working with THREE poles!
*
*  MAXIT is the maximum number of iterations allowed for each
*  eigenvalue.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ren-Cang Li, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 20 )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR, EIGHT, TEN
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0,
     $                   THREE = 3.0D+0, FOUR = 4.0D+0, EIGHT = 8.0D+0,
     $                   TEN = 10.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ORGATI, SWTCH, SWTCH3
      INTEGER            II, IIM1, IIP1, IP1, ITER, J, NITER
      DOUBLE PRECISION   A, B, C, DELSQ, DELSQ2, DPHI, DPSI, DTIIM,
     $                   DTIIP, DTIPSQ, DTISQ, DTNSQ, DTNSQ1, DW, EPS,
     $                   ERRETM, ETA, PHI, PREW, PSI, RHOINV, SG2LB,
     $                   SG2UB, TAU, TEMP, TEMP1, TEMP2, W
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DD( 3 ), ZZ( 3 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLAED6, DLASD5
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Since this routine is called in an inner loop, we do no argument
*     checking.
*
*     Quick return for N=1 and 2.
*
      INFO = 0
      IF( N.EQ.1 ) THEN
*
*        Presumably, I=1 upon entry
*
         SIGMA = SQRT( D( 1 )*D( 1 )+RHO*Z( 1 )*Z( 1 ) )
         DELTA( 1 ) = ONE
         WORK( 1 ) = ONE
         RETURN
      END IF
      IF( N.EQ.2 ) THEN
         CALL DLASD5( I, D, Z, DELTA, RHO, SIGMA, WORK )
         RETURN
      END IF
*
*     Compute machine epsilon
*
      EPS = DLAMCH( 'Epsilon' )
      RHOINV = ONE / RHO
*
*     The case I = N
*
      IF( I.EQ.N ) THEN
*
*        Initialize some basic variables
*
         II = N - 1
         NITER = 1
*
*        Calculate initial guess
*
         TEMP = RHO / TWO
*
*        If ||Z||_2 is not one, then TEMP should be set to
*        RHO * ||Z||_2^2 / TWO
*
         TEMP1 = TEMP / ( D( N )+SQRT( D( N )*D( N )+TEMP ) )
         DO 10 J = 1, N
            WORK( J ) = D( J ) + D( N ) + TEMP1
            DELTA( J ) = ( D( J )-D( N ) ) - TEMP1
   10    CONTINUE
*
         PSI = ZERO
         DO 20 J = 1, N - 2
            PSI = PSI + Z( J )*Z( J ) / ( DELTA( J )*WORK( J ) )
   20    CONTINUE
*
         C = RHOINV + PSI
         W = C + Z( II )*Z( II ) / ( DELTA( II )*WORK( II ) ) +
     $       Z( N )*Z( N ) / ( DELTA( N )*WORK( N ) )
*
         IF( W.LE.ZERO ) THEN
            TEMP1 = SQRT( D( N )*D( N )+RHO )
            TEMP = Z( N-1 )*Z( N-1 ) / ( ( D( N-1 )+TEMP1 )*
     $             ( D( N )-D( N-1 )+RHO / ( D( N )+TEMP1 ) ) ) +
     $             Z( N )*Z( N ) / RHO
*
*           The following TAU is to approximate
*           SIGMA_n^2 - D( N )*D( N )
*
            IF( C.LE.TEMP ) THEN
               TAU = RHO
            ELSE
               DELSQ = ( D( N )-D( N-1 ) )*( D( N )+D( N-1 ) )
               A = -C*DELSQ + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
               B = Z( N )*Z( N )*DELSQ
               IF( A.LT.ZERO ) THEN
                  TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
               ELSE
                  TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
               END IF
            END IF
*
*           It can be proved that
*               D(N)^2+RHO/2 <= SIGMA_n^2 < D(N)^2+TAU <= D(N)^2+RHO
*
         ELSE
            DELSQ = ( D( N )-D( N-1 ) )*( D( N )+D( N-1 ) )
            A = -C*DELSQ + Z( N-1 )*Z( N-1 ) + Z( N )*Z( N )
            B = Z( N )*Z( N )*DELSQ
*
*           The following TAU is to approximate
*           SIGMA_n^2 - D( N )*D( N )
*
            IF( A.LT.ZERO ) THEN
               TAU = TWO*B / ( SQRT( A*A+FOUR*B*C )-A )
            ELSE
               TAU = ( A+SQRT( A*A+FOUR*B*C ) ) / ( TWO*C )
            END IF
*
*           It can be proved that
*           D(N)^2 < D(N)^2+TAU < SIGMA(N)^2 < D(N)^2+RHO/2
*
         END IF
*
*        The following ETA is to approximate SIGMA_n - D( N )
*
         ETA = TAU / ( D( N )+SQRT( D( N )*D( N )+TAU ) )
*
         SIGMA = D( N ) + ETA
         DO 30 J = 1, N
            DELTA( J ) = ( D( J )-D( I ) ) - ETA
            WORK( J ) = D( J ) + D( I ) + ETA
   30    CONTINUE
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 40 J = 1, II
            TEMP = Z( J ) / ( DELTA( J )*WORK( J ) )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
   40    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         TEMP = Z( N ) / ( DELTA( N )*WORK( N ) )
         PHI = Z( N )*TEMP
         DPHI = TEMP*TEMP
         ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $            ABS( TAU )*( DPSI+DPHI )
*
         W = RHOINV + PHI + PSI
*
*        Test for convergence
*
         IF( ABS( W ).LE.EPS*ERRETM ) THEN
            GO TO 240
         END IF
*
*        Calculate the new step
*
         NITER = NITER + 1
         DTNSQ1 = WORK( N-1 )*DELTA( N-1 )
         DTNSQ = WORK( N )*DELTA( N )
         C = W - DTNSQ1*DPSI - DTNSQ*DPHI
         A = ( DTNSQ+DTNSQ1 )*W - DTNSQ*DTNSQ1*( DPSI+DPHI )
         B = DTNSQ*DTNSQ1*W
         IF( C.LT.ZERO )
     $      C = ABS( C )
         IF( C.EQ.ZERO ) THEN
            ETA = RHO - SIGMA*SIGMA
         ELSE IF( A.GE.ZERO ) THEN
            ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
         ELSE
            ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
         END IF
*
*        Note, eta should be positive if w is negative, and
*        eta should be negative otherwise. However,
*        if for some reason caused by roundoff, eta*w > 0,
*        we simply use one Newton step instead. This way
*        will guarantee eta*w < 0.
*
         IF( W*ETA.GT.ZERO )
     $      ETA = -W / ( DPSI+DPHI )
         TEMP = ETA - DTNSQ
         IF( TEMP.GT.RHO )
     $      ETA = RHO + DTNSQ
*
         TAU = TAU + ETA
         ETA = ETA / ( SIGMA+SQRT( ETA+SIGMA*SIGMA ) )
         DO 50 J = 1, N
            DELTA( J ) = DELTA( J ) - ETA
            WORK( J ) = WORK( J ) + ETA
   50    CONTINUE
*
         SIGMA = SIGMA + ETA
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 60 J = 1, II
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
   60    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         TEMP = Z( N ) / ( WORK( N )*DELTA( N ) )
         PHI = Z( N )*TEMP
         DPHI = TEMP*TEMP
         ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $            ABS( TAU )*( DPSI+DPHI )
*
         W = RHOINV + PHI + PSI
*
*        Main loop to update the values of the array   DELTA
*
         ITER = NITER + 1
*
         DO 90 NITER = ITER, MAXIT
*
*           Test for convergence
*
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               GO TO 240
            END IF
*
*           Calculate the new step
*
            DTNSQ1 = WORK( N-1 )*DELTA( N-1 )
            DTNSQ = WORK( N )*DELTA( N )
            C = W - DTNSQ1*DPSI - DTNSQ*DPHI
            A = ( DTNSQ+DTNSQ1 )*W - DTNSQ1*DTNSQ*( DPSI+DPHI )
            B = DTNSQ1*DTNSQ*W
            IF( A.GE.ZERO ) THEN
               ETA = ( A+SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               ETA = TWO*B / ( A-SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
*
*           Note, eta should be positive if w is negative, and
*           eta should be negative otherwise. However,
*           if for some reason caused by roundoff, eta*w > 0,
*           we simply use one Newton step instead. This way
*           will guarantee eta*w < 0.
*
            IF( W*ETA.GT.ZERO )
     $         ETA = -W / ( DPSI+DPHI )
            TEMP = ETA - DTNSQ
            IF( TEMP.LE.ZERO )
     $         ETA = ETA / TWO
*
            TAU = TAU + ETA
            ETA = ETA / ( SIGMA+SQRT( ETA+SIGMA*SIGMA ) )
            DO 70 J = 1, N
               DELTA( J ) = DELTA( J ) - ETA
               WORK( J ) = WORK( J ) + ETA
   70       CONTINUE
*
            SIGMA = SIGMA + ETA
*
*           Evaluate PSI and the derivative DPSI
*
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 80 J = 1, II
               TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
   80       CONTINUE
            ERRETM = ABS( ERRETM )
*
*           Evaluate PHI and the derivative DPHI
*
            TEMP = Z( N ) / ( WORK( N )*DELTA( N ) )
            PHI = Z( N )*TEMP
            DPHI = TEMP*TEMP
            ERRETM = EIGHT*( -PHI-PSI ) + ERRETM - PHI + RHOINV +
     $               ABS( TAU )*( DPSI+DPHI )
*
            W = RHOINV + PHI + PSI
   90    CONTINUE
*
*        Return with INFO = 1, NITER = MAXIT and not converged
*
         INFO = 1
         GO TO 240
*
*        End for the case I = N
*
      ELSE
*
*        The case for I < N
*
         NITER = 1
         IP1 = I + 1
*
*        Calculate initial guess
*
         DELSQ = ( D( IP1 )-D( I ) )*( D( IP1 )+D( I ) )
         DELSQ2 = DELSQ / TWO
         TEMP = DELSQ2 / ( D( I )+SQRT( D( I )*D( I )+DELSQ2 ) )
         DO 100 J = 1, N
            WORK( J ) = D( J ) + D( I ) + TEMP
            DELTA( J ) = ( D( J )-D( I ) ) - TEMP
  100    CONTINUE
*
         PSI = ZERO
         DO 110 J = 1, I - 1
            PSI = PSI + Z( J )*Z( J ) / ( WORK( J )*DELTA( J ) )
  110    CONTINUE
*
         PHI = ZERO
         DO 120 J = N, I + 2, -1
            PHI = PHI + Z( J )*Z( J ) / ( WORK( J )*DELTA( J ) )
  120    CONTINUE
         C = RHOINV + PSI + PHI
         W = C + Z( I )*Z( I ) / ( WORK( I )*DELTA( I ) ) +
     $       Z( IP1 )*Z( IP1 ) / ( WORK( IP1 )*DELTA( IP1 ) )
*
         IF( W.GT.ZERO ) THEN
*
*           d(i)^2 < the ith sigma^2 < (d(i)^2+d(i+1)^2)/2
*
*           We choose d(i) as origin.
*
            ORGATI = .TRUE.
            SG2LB = ZERO
            SG2UB = DELSQ2
            A = C*DELSQ + Z( I )*Z( I ) + Z( IP1 )*Z( IP1 )
            B = Z( I )*Z( I )*DELSQ
            IF( A.GT.ZERO ) THEN
               TAU = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            ELSE
               TAU = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            END IF
*
*           TAU now is an estimation of SIGMA^2 - D( I )^2. The
*           following, however, is the corresponding estimation of
*           SIGMA - D( I ).
*
            ETA = TAU / ( D( I )+SQRT( D( I )*D( I )+TAU ) )
         ELSE
*
*           (d(i)^2+d(i+1)^2)/2 <= the ith sigma^2 < d(i+1)^2/2
*
*           We choose d(i+1) as origin.
*
            ORGATI = .FALSE.
            SG2LB = -DELSQ2
            SG2UB = ZERO
            A = C*DELSQ - Z( I )*Z( I ) - Z( IP1 )*Z( IP1 )
            B = Z( IP1 )*Z( IP1 )*DELSQ
            IF( A.LT.ZERO ) THEN
               TAU = TWO*B / ( A-SQRT( ABS( A*A+FOUR*B*C ) ) )
            ELSE
               TAU = -( A+SQRT( ABS( A*A+FOUR*B*C ) ) ) / ( TWO*C )
            END IF
*
*           TAU now is an estimation of SIGMA^2 - D( IP1 )^2. The
*           following, however, is the corresponding estimation of
*           SIGMA - D( IP1 ).
*
            ETA = TAU / ( D( IP1 )+SQRT( ABS( D( IP1 )*D( IP1 )+
     $            TAU ) ) )
         END IF
*
         IF( ORGATI ) THEN
            II = I
            SIGMA = D( I ) + ETA
            DO 130 J = 1, N
               WORK( J ) = D( J ) + D( I ) + ETA
               DELTA( J ) = ( D( J )-D( I ) ) - ETA
  130       CONTINUE
         ELSE
            II = I + 1
            SIGMA = D( IP1 ) + ETA
            DO 140 J = 1, N
               WORK( J ) = D( J ) + D( IP1 ) + ETA
               DELTA( J ) = ( D( J )-D( IP1 ) ) - ETA
  140       CONTINUE
         END IF
         IIM1 = II - 1
         IIP1 = II + 1
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 150 J = 1, IIM1
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
  150    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         DPHI = ZERO
         PHI = ZERO
         DO 160 J = N, IIP1, -1
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PHI = PHI + Z( J )*TEMP
            DPHI = DPHI + TEMP*TEMP
            ERRETM = ERRETM + PHI
  160    CONTINUE
*
         W = RHOINV + PHI + PSI
*
*        W is the value of the secular function with
*        its ii-th element removed.
*
         SWTCH3 = .FALSE.
         IF( ORGATI ) THEN
            IF( W.LT.ZERO )
     $         SWTCH3 = .TRUE.
         ELSE
            IF( W.GT.ZERO )
     $         SWTCH3 = .TRUE.
         END IF
         IF( II.EQ.1 .OR. II.EQ.N )
     $      SWTCH3 = .FALSE.
*
         TEMP = Z( II ) / ( WORK( II )*DELTA( II ) )
         DW = DPSI + DPHI + TEMP*TEMP
         TEMP = Z( II )*TEMP
         W = W + TEMP
         ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $            THREE*ABS( TEMP ) + ABS( TAU )*DW
*
*        Test for convergence
*
         IF( ABS( W ).LE.EPS*ERRETM ) THEN
            GO TO 240
         END IF
*
         IF( W.LE.ZERO ) THEN
            SG2LB = MAX( SG2LB, TAU )
         ELSE
            SG2UB = MIN( SG2UB, TAU )
         END IF
*
*        Calculate the new step
*
         NITER = NITER + 1
         IF( .NOT.SWTCH3 ) THEN
            DTIPSQ = WORK( IP1 )*DELTA( IP1 )
            DTISQ = WORK( I )*DELTA( I )
            IF( ORGATI ) THEN
               C = W - DTIPSQ*DW + DELSQ*( Z( I ) / DTISQ )**2
            ELSE
               C = W - DTISQ*DW - DELSQ*( Z( IP1 ) / DTIPSQ )**2
            END IF
            A = ( DTIPSQ+DTISQ )*W - DTIPSQ*DTISQ*DW
            B = DTIPSQ*DTISQ*W
            IF( C.EQ.ZERO ) THEN
               IF( A.EQ.ZERO ) THEN
                  IF( ORGATI ) THEN
                     A = Z( I )*Z( I ) + DTIPSQ*DTIPSQ*( DPSI+DPHI )
                  ELSE
                     A = Z( IP1 )*Z( IP1 ) + DTISQ*DTISQ*( DPSI+DPHI )
                  END IF
               END IF
               ETA = B / A
            ELSE IF( A.LE.ZERO ) THEN
               ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
            ELSE
               ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
            END IF
         ELSE
*
*           Interpolation using THREE most relevant poles
*
            DTIIM = WORK( IIM1 )*DELTA( IIM1 )
            DTIIP = WORK( IIP1 )*DELTA( IIP1 )
            TEMP = RHOINV + PSI + PHI
            IF( ORGATI ) THEN
               TEMP1 = Z( IIM1 ) / DTIIM
               TEMP1 = TEMP1*TEMP1
               C = ( TEMP - DTIIP*( DPSI+DPHI ) ) -
     $             ( D( IIM1 )-D( IIP1 ) )*( D( IIM1 )+D( IIP1 ) )*TEMP1
               ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
               IF( DPSI.LT.TEMP1 ) THEN
                  ZZ( 3 ) = DTIIP*DTIIP*DPHI
               ELSE
                  ZZ( 3 ) = DTIIP*DTIIP*( ( DPSI-TEMP1 )+DPHI )
               END IF
            ELSE
               TEMP1 = Z( IIP1 ) / DTIIP
               TEMP1 = TEMP1*TEMP1
               C = ( TEMP - DTIIM*( DPSI+DPHI ) ) -
     $             ( D( IIP1 )-D( IIM1 ) )*( D( IIM1 )+D( IIP1 ) )*TEMP1
               IF( DPHI.LT.TEMP1 ) THEN
                  ZZ( 1 ) = DTIIM*DTIIM*DPSI
               ELSE
                  ZZ( 1 ) = DTIIM*DTIIM*( DPSI+( DPHI-TEMP1 ) )
               END IF
               ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
            END IF
            ZZ( 2 ) = Z( II )*Z( II )
            DD( 1 ) = DTIIM
            DD( 2 ) = DELTA( II )*WORK( II )
            DD( 3 ) = DTIIP
            CALL DLAED6( NITER, ORGATI, C, DD, ZZ, W, ETA, INFO )
            IF( INFO.NE.0 )
     $         GO TO 240
         END IF
*
*        Note, eta should be positive if w is negative, and
*        eta should be negative otherwise. However,
*        if for some reason caused by roundoff, eta*w > 0,
*        we simply use one Newton step instead. This way
*        will guarantee eta*w < 0.
*
         IF( W*ETA.GE.ZERO )
     $      ETA = -W / DW
         IF( ORGATI ) THEN
            TEMP1 = WORK( I )*DELTA( I )
            TEMP = ETA - TEMP1
         ELSE
            TEMP1 = WORK( IP1 )*DELTA( IP1 )
            TEMP = ETA - TEMP1
         END IF
         IF( TEMP.GT.SG2UB .OR. TEMP.LT.SG2LB ) THEN
            IF( W.LT.ZERO ) THEN
               ETA = ( SG2UB-TAU ) / TWO
            ELSE
               ETA = ( SG2LB-TAU ) / TWO
            END IF
         END IF
*
         TAU = TAU + ETA
         ETA = ETA / ( SIGMA+SQRT( SIGMA*SIGMA+ETA ) )
*
         PREW = W
*
         SIGMA = SIGMA + ETA
         DO 170 J = 1, N
            WORK( J ) = WORK( J ) + ETA
            DELTA( J ) = DELTA( J ) - ETA
  170    CONTINUE
*
*        Evaluate PSI and the derivative DPSI
*
         DPSI = ZERO
         PSI = ZERO
         ERRETM = ZERO
         DO 180 J = 1, IIM1
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PSI = PSI + Z( J )*TEMP
            DPSI = DPSI + TEMP*TEMP
            ERRETM = ERRETM + PSI
  180    CONTINUE
         ERRETM = ABS( ERRETM )
*
*        Evaluate PHI and the derivative DPHI
*
         DPHI = ZERO
         PHI = ZERO
         DO 190 J = N, IIP1, -1
            TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
            PHI = PHI + Z( J )*TEMP
            DPHI = DPHI + TEMP*TEMP
            ERRETM = ERRETM + PHI
  190    CONTINUE
*
         TEMP = Z( II ) / ( WORK( II )*DELTA( II ) )
         DW = DPSI + DPHI + TEMP*TEMP
         TEMP = Z( II )*TEMP
         W = RHOINV + PHI + PSI + TEMP
         ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $            THREE*ABS( TEMP ) + ABS( TAU )*DW
*
         IF( W.LE.ZERO ) THEN
            SG2LB = MAX( SG2LB, TAU )
         ELSE
            SG2UB = MIN( SG2UB, TAU )
         END IF
*
         SWTCH = .FALSE.
         IF( ORGATI ) THEN
            IF( -W.GT.ABS( PREW ) / TEN )
     $         SWTCH = .TRUE.
         ELSE
            IF( W.GT.ABS( PREW ) / TEN )
     $         SWTCH = .TRUE.
         END IF
*
*        Main loop to update the values of the array   DELTA and WORK
*
         ITER = NITER + 1
*
         DO 230 NITER = ITER, MAXIT
*
*           Test for convergence
*
            IF( ABS( W ).LE.EPS*ERRETM ) THEN
               GO TO 240
            END IF
*
*           Calculate the new step
*
            IF( .NOT.SWTCH3 ) THEN
               DTIPSQ = WORK( IP1 )*DELTA( IP1 )
               DTISQ = WORK( I )*DELTA( I )
               IF( .NOT.SWTCH ) THEN
                  IF( ORGATI ) THEN
                     C = W - DTIPSQ*DW + DELSQ*( Z( I ) / DTISQ )**2
                  ELSE
                     C = W - DTISQ*DW - DELSQ*( Z( IP1 ) / DTIPSQ )**2
                  END IF
               ELSE
                  TEMP = Z( II ) / ( WORK( II )*DELTA( II ) )
                  IF( ORGATI ) THEN
                     DPSI = DPSI + TEMP*TEMP
                  ELSE
                     DPHI = DPHI + TEMP*TEMP
                  END IF
                  C = W - DTISQ*DPSI - DTIPSQ*DPHI
               END IF
               A = ( DTIPSQ+DTISQ )*W - DTIPSQ*DTISQ*DW
               B = DTIPSQ*DTISQ*W
               IF( C.EQ.ZERO ) THEN
                  IF( A.EQ.ZERO ) THEN
                     IF( .NOT.SWTCH ) THEN
                        IF( ORGATI ) THEN
                           A = Z( I )*Z( I ) + DTIPSQ*DTIPSQ*
     $                         ( DPSI+DPHI )
                        ELSE
                           A = Z( IP1 )*Z( IP1 ) +
     $                         DTISQ*DTISQ*( DPSI+DPHI )
                        END IF
                     ELSE
                        A = DTISQ*DTISQ*DPSI + DTIPSQ*DTIPSQ*DPHI
                     END IF
                  END IF
                  ETA = B / A
               ELSE IF( A.LE.ZERO ) THEN
                  ETA = ( A-SQRT( ABS( A*A-FOUR*B*C ) ) ) / ( TWO*C )
               ELSE
                  ETA = TWO*B / ( A+SQRT( ABS( A*A-FOUR*B*C ) ) )
               END IF
            ELSE
*
*              Interpolation using THREE most relevant poles
*
               DTIIM = WORK( IIM1 )*DELTA( IIM1 )
               DTIIP = WORK( IIP1 )*DELTA( IIP1 )
               TEMP = RHOINV + PSI + PHI
               IF( SWTCH ) THEN
                  C = TEMP - DTIIM*DPSI - DTIIP*DPHI
                  ZZ( 1 ) = DTIIM*DTIIM*DPSI
                  ZZ( 3 ) = DTIIP*DTIIP*DPHI
               ELSE
                  IF( ORGATI ) THEN
                     TEMP1 = Z( IIM1 ) / DTIIM
                     TEMP1 = TEMP1*TEMP1
                     TEMP2 = ( D( IIM1 )-D( IIP1 ) )*
     $                       ( D( IIM1 )+D( IIP1 ) )*TEMP1
                     C = TEMP - DTIIP*( DPSI+DPHI ) - TEMP2
                     ZZ( 1 ) = Z( IIM1 )*Z( IIM1 )
                     IF( DPSI.LT.TEMP1 ) THEN
                        ZZ( 3 ) = DTIIP*DTIIP*DPHI
                     ELSE
                        ZZ( 3 ) = DTIIP*DTIIP*( ( DPSI-TEMP1 )+DPHI )
                     END IF
                  ELSE
                     TEMP1 = Z( IIP1 ) / DTIIP
                     TEMP1 = TEMP1*TEMP1
                     TEMP2 = ( D( IIP1 )-D( IIM1 ) )*
     $                       ( D( IIM1 )+D( IIP1 ) )*TEMP1
                     C = TEMP - DTIIM*( DPSI+DPHI ) - TEMP2
                     IF( DPHI.LT.TEMP1 ) THEN
                        ZZ( 1 ) = DTIIM*DTIIM*DPSI
                     ELSE
                        ZZ( 1 ) = DTIIM*DTIIM*( DPSI+( DPHI-TEMP1 ) )
                     END IF
                     ZZ( 3 ) = Z( IIP1 )*Z( IIP1 )
                  END IF
               END IF
               DD( 1 ) = DTIIM
               DD( 2 ) = DELTA( II )*WORK( II )
               DD( 3 ) = DTIIP
               CALL DLAED6( NITER, ORGATI, C, DD, ZZ, W, ETA, INFO )
               IF( INFO.NE.0 )
     $            GO TO 240
            END IF
*
*           Note, eta should be positive if w is negative, and
*           eta should be negative otherwise. However,
*           if for some reason caused by roundoff, eta*w > 0,
*           we simply use one Newton step instead. This way
*           will guarantee eta*w < 0.
*
            IF( W*ETA.GE.ZERO )
     $         ETA = -W / DW
            IF( ORGATI ) THEN
               TEMP1 = WORK( I )*DELTA( I )
               TEMP = ETA - TEMP1
            ELSE
               TEMP1 = WORK( IP1 )*DELTA( IP1 )
               TEMP = ETA - TEMP1
            END IF
            IF( TEMP.GT.SG2UB .OR. TEMP.LT.SG2LB ) THEN
               IF( W.LT.ZERO ) THEN
                  ETA = ( SG2UB-TAU ) / TWO
               ELSE
                  ETA = ( SG2LB-TAU ) / TWO
               END IF
            END IF
*
            TAU = TAU + ETA
            ETA = ETA / ( SIGMA+SQRT( SIGMA*SIGMA+ETA ) )
*
            SIGMA = SIGMA + ETA
            DO 200 J = 1, N
               WORK( J ) = WORK( J ) + ETA
               DELTA( J ) = DELTA( J ) - ETA
  200       CONTINUE
*
            PREW = W
*
*           Evaluate PSI and the derivative DPSI
*
            DPSI = ZERO
            PSI = ZERO
            ERRETM = ZERO
            DO 210 J = 1, IIM1
               TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
               PSI = PSI + Z( J )*TEMP
               DPSI = DPSI + TEMP*TEMP
               ERRETM = ERRETM + PSI
  210       CONTINUE
            ERRETM = ABS( ERRETM )
*
*           Evaluate PHI and the derivative DPHI
*
            DPHI = ZERO
            PHI = ZERO
            DO 220 J = N, IIP1, -1
               TEMP = Z( J ) / ( WORK( J )*DELTA( J ) )
               PHI = PHI + Z( J )*TEMP
               DPHI = DPHI + TEMP*TEMP
               ERRETM = ERRETM + PHI
  220       CONTINUE
*
            TEMP = Z( II ) / ( WORK( II )*DELTA( II ) )
            DW = DPSI + DPHI + TEMP*TEMP
            TEMP = Z( II )*TEMP
            W = RHOINV + PHI + PSI + TEMP
            ERRETM = EIGHT*( PHI-PSI ) + ERRETM + TWO*RHOINV +
     $               THREE*ABS( TEMP ) + ABS( TAU )*DW
            IF( W*PREW.GT.ZERO .AND. ABS( W ).GT.ABS( PREW ) / TEN )
     $         SWTCH = .NOT.SWTCH
*
            IF( W.LE.ZERO ) THEN
               SG2LB = MAX( SG2LB, TAU )
            ELSE
               SG2UB = MIN( SG2UB, TAU )
            END IF
*
  230    CONTINUE
*
*        Return with INFO = 1, NITER = MAXIT and not converged
*
         INFO = 1
*
      END IF
*
  240 CONTINUE
      RETURN
*
*     End of DLASD4
*
      END
      SUBROUTINE DLASD5( I, D, Z, DELTA, RHO, DSIGMA, WORK )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            I
      DOUBLE PRECISION   DSIGMA, RHO
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( 2 ), DELTA( 2 ), WORK( 2 ), Z( 2 )
*     ..
*
*  Purpose
*  =======
*
*  This subroutine computes the square root of the I-th eigenvalue
*  of a positive symmetric rank-one modification of a 2-by-2 diagonal
*  matrix
*
*             diag( D ) * diag( D ) +  RHO *  Z * transpose(Z) .
*
*  The diagonal entries in the array D are assumed to satisfy
*
*             0 <= D(i) < D(j)  for  i < j .
*
*  We also assume RHO > 0 and that the Euclidean norm of the vector
*  Z is one.
*
*  Arguments
*  =========
*
*  I      (input) INTEGER
*         The index of the eigenvalue to be computed.  I = 1 or I = 2.
*
*  D      (input) DOUBLE PRECISION array, dimension ( 2 )
*         The original eigenvalues.  We assume 0 <= D(1) < D(2).
*
*  Z      (input) DOUBLE PRECISION array, dimension ( 2 )
*         The components of the updating vector.
*
*  DELTA  (output) DOUBLE PRECISION array, dimension ( 2 )
*         Contains (D(j) - sigma_I) in its  j-th component.
*         The vector DELTA contains the information necessary
*         to construct the eigenvectors.
*
*  RHO    (input) DOUBLE PRECISION
*         The scalar in the symmetric updating formula.
*
*  DSIGMA (output) DOUBLE PRECISION
*         The computed sigma_I, the I-th updated eigenvalue.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension ( 2 )
*         WORK contains (D(j) + sigma_I) in its  j-th component.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ren-Cang Li, Computer Science Division, University of California
*     at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0,
     $                   THREE = 3.0D+0, FOUR = 4.0D+0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   B, C, DEL, DELSQ, TAU, W
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
*     ..
*     .. Executable Statements ..
*
      DEL = D( 2 ) - D( 1 )
      DELSQ = DEL*( D( 2 )+D( 1 ) )
      IF( I.EQ.1 ) THEN
         W = ONE + FOUR*RHO*( Z( 2 )*Z( 2 ) / ( D( 1 )+THREE*D( 2 ) )-
     $       Z( 1 )*Z( 1 ) / ( THREE*D( 1 )+D( 2 ) ) ) / DEL
         IF( W.GT.ZERO ) THEN
            B = DELSQ + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 1 )*Z( 1 )*DELSQ
*
*           B > ZERO, always
*
*           The following TAU is DSIGMA * DSIGMA - D( 1 ) * D( 1 )
*
            TAU = TWO*C / ( B+SQRT( ABS( B*B-FOUR*C ) ) )
*
*           The following TAU is DSIGMA - D( 1 )
*
            TAU = TAU / ( D( 1 )+SQRT( D( 1 )*D( 1 )+TAU ) )
            DSIGMA = D( 1 ) + TAU
            DELTA( 1 ) = -TAU
            DELTA( 2 ) = DEL - TAU
            WORK( 1 ) = TWO*D( 1 ) + TAU
            WORK( 2 ) = ( D( 1 )+TAU ) + D( 2 )
*           DELTA( 1 ) = -Z( 1 ) / TAU
*           DELTA( 2 ) = Z( 2 ) / ( DEL-TAU )
         ELSE
            B = -DELSQ + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
            C = RHO*Z( 2 )*Z( 2 )*DELSQ
*
*           The following TAU is DSIGMA * DSIGMA - D( 2 ) * D( 2 )
*
            IF( B.GT.ZERO ) THEN
               TAU = -TWO*C / ( B+SQRT( B*B+FOUR*C ) )
            ELSE
               TAU = ( B-SQRT( B*B+FOUR*C ) ) / TWO
            END IF
*
*           The following TAU is DSIGMA - D( 2 )
*
            TAU = TAU / ( D( 2 )+SQRT( ABS( D( 2 )*D( 2 )+TAU ) ) )
            DSIGMA = D( 2 ) + TAU
            DELTA( 1 ) = -( DEL+TAU )
            DELTA( 2 ) = -TAU
            WORK( 1 ) = D( 1 ) + TAU + D( 2 )
            WORK( 2 ) = TWO*D( 2 ) + TAU
*           DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
*           DELTA( 2 ) = -Z( 2 ) / TAU
         END IF
*        TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
*        DELTA( 1 ) = DELTA( 1 ) / TEMP
*        DELTA( 2 ) = DELTA( 2 ) / TEMP
      ELSE
*
*        Now I=2
*
         B = -DELSQ + RHO*( Z( 1 )*Z( 1 )+Z( 2 )*Z( 2 ) )
         C = RHO*Z( 2 )*Z( 2 )*DELSQ
*
*        The following TAU is DSIGMA * DSIGMA - D( 2 ) * D( 2 )
*
         IF( B.GT.ZERO ) THEN
            TAU = ( B+SQRT( B*B+FOUR*C ) ) / TWO
         ELSE
            TAU = TWO*C / ( -B+SQRT( B*B+FOUR*C ) )
         END IF
*
*        The following TAU is DSIGMA - D( 2 )
*
         TAU = TAU / ( D( 2 )+SQRT( D( 2 )*D( 2 )+TAU ) )
         DSIGMA = D( 2 ) + TAU
         DELTA( 1 ) = -( DEL+TAU )
         DELTA( 2 ) = -TAU
         WORK( 1 ) = D( 1 ) + TAU + D( 2 )
         WORK( 2 ) = TWO*D( 2 ) + TAU
*        DELTA( 1 ) = -Z( 1 ) / ( DEL+TAU )
*        DELTA( 2 ) = -Z( 2 ) / TAU
*        TEMP = SQRT( DELTA( 1 )*DELTA( 1 )+DELTA( 2 )*DELTA( 2 ) )
*        DELTA( 1 ) = DELTA( 1 ) / TEMP
*        DELTA( 2 ) = DELTA( 2 ) / TEMP
      END IF
      RETURN
*
*     End of DLASD5
*
      END
      SUBROUTINE DLASD6( ICOMPQ, NL, NR, SQRE, D, VF, VL, ALPHA, BETA,
     $                   IDXQ, PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM,
     $                   LDGNUM, POLES, DIFL, DIFR, Z, K, C, S, WORK,
     $                   IWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            GIVPTR, ICOMPQ, INFO, K, LDGCOL, LDGNUM, NL,
     $                   NR, SQRE
      DOUBLE PRECISION   ALPHA, BETA, C, S
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( LDGCOL, * ), IDXQ( * ), IWORK( * ),
     $                   PERM( * )
      DOUBLE PRECISION   D( * ), DIFL( * ), DIFR( * ),
     $                   GIVNUM( LDGNUM, * ), POLES( LDGNUM, * ),
     $                   VF( * ), VL( * ), WORK( * ), Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASD6 computes the SVD of an updated upper bidiagonal matrix B
*  obtained by merging two smaller ones by appending a row. This
*  routine is used only for the problem which requires all singular
*  values and optionally singular vector matrices in factored form.
*  B is an N-by-M matrix with N = NL + NR + 1 and M = N + SQRE.
*  A related subroutine, DLASD1, handles the case in which all singular
*  values and singular vectors of the bidiagonal matrix are desired.
*
*  DLASD6 computes the SVD as follows:
*
*                ( D1(in)  0    0     0 )
*    B = U(in) * (   Z1'   a   Z2'    b ) * VT(in)
*                (   0     0   D2(in) 0 )
*
*      = U(out) * ( D(out) 0) * VT(out)
*
*  where Z' = (Z1' a Z2' b) = u' VT', and u is a vector of dimension M
*  with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
*  elsewhere; and the entry b is empty if SQRE = 0.
*
*  The singular values of B can be computed using D1, D2, the first
*  components of all the right singular vectors of the lower block, and
*  the last components of all the right singular vectors of the upper
*  block. These components are stored and updated in VF and VL,
*  respectively, in DLASD6. Hence U and VT are not explicitly
*  referenced.
*
*  The singular values are stored in D. The algorithm consists of two
*  stages:
*
*        The first stage consists of deflating the size of the problem
*        when there are multiple singular values or if there is a zero
*        in the Z vector. For each such occurence the dimension of the
*        secular equation problem is reduced by one. This stage is
*        performed by the routine DLASD7.
*
*        The second stage consists of calculating the updated
*        singular values. This is done by finding the roots of the
*        secular equation via the routine DLASD4 (as called by DLASD8).
*        This routine also updates VF and VL and computes the distances
*        between the updated singular values and the old singular
*        values.
*
*  DLASD6 is called from DLASDA.
*
*  Arguments
*  =========
*
*  ICOMPQ (input) INTEGER
*         Specifies whether singular vectors are to be computed in
*         factored form:
*         = 0: Compute singular values only.
*         = 1: Compute singular vectors in factored form as well.
*
*  NL     (input) INTEGER
*         The row dimension of the upper block.  NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block.  NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has row dimension N = NL + NR + 1,
*         and column dimension M = N + SQRE.
*
*  D      (input/output) DOUBLE PRECISION array, dimension ( NL+NR+1 ).
*         On entry D(1:NL,1:NL) contains the singular values of the
*         upper block, and D(NL+2:N) contains the singular values
*         of the lower block. On exit D(1:N) contains the singular
*         values of the modified matrix.
*
*  VF     (input/output) DOUBLE PRECISION array, dimension ( M )
*         On entry, VF(1:NL+1) contains the first components of all
*         right singular vectors of the upper block; and VF(NL+2:M)
*         contains the first components of all right singular vectors
*         of the lower block. On exit, VF contains the first components
*         of all right singular vectors of the bidiagonal matrix.
*
*  VL     (input/output) DOUBLE PRECISION array, dimension ( M )
*         On entry, VL(1:NL+1) contains the  last components of all
*         right singular vectors of the upper block; and VL(NL+2:M)
*         contains the last components of all right singular vectors of
*         the lower block. On exit, VL contains the last components of
*         all right singular vectors of the bidiagonal matrix.
*
*  ALPHA  (input/output) DOUBLE PRECISION
*         Contains the diagonal element associated with the added row.
*
*  BETA   (input/output) DOUBLE PRECISION
*         Contains the off-diagonal element associated with the added
*         row.
*
*  IDXQ   (output) INTEGER array, dimension ( N )
*         This contains the permutation which will reintegrate the
*         subproblem just solved back into sorted order, i.e.
*         D( IDXQ( I = 1, N ) ) will be in ascending order.
*
*  PERM   (output) INTEGER array, dimension ( N )
*         The permutations (from deflation and sorting) to be applied
*         to each block. Not referenced if ICOMPQ = 0.
*
*  GIVPTR (output) INTEGER
*         The number of Givens rotations which took place in this
*         subproblem. Not referenced if ICOMPQ = 0.
*
*  GIVCOL (output) INTEGER array, dimension ( LDGCOL, 2 )
*         Each pair of numbers indicates a pair of columns to take place
*         in a Givens rotation. Not referenced if ICOMPQ = 0.
*
*  LDGCOL (input) INTEGER
*         leading dimension of GIVCOL, must be at least N.
*
*  GIVNUM (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
*         Each number indicates the C or S value to be used in the
*         corresponding Givens rotation. Not referenced if ICOMPQ = 0.
*
*  LDGNUM (input) INTEGER
*         The leading dimension of GIVNUM and POLES, must be at least N.
*
*  POLES  (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
*         On exit, POLES(1,*) is an array containing the new singular
*         values obtained from solving the secular equation, and
*         POLES(2,*) is an array containing the poles in the secular
*         equation. Not referenced if ICOMPQ = 0.
*
*  DIFL   (output) DOUBLE PRECISION array, dimension ( N )
*         On exit, DIFL(I) is the distance between I-th updated
*         (undeflated) singular value and the I-th (undeflated) old
*         singular value.
*
*  DIFR   (output) DOUBLE PRECISION array,
*                  dimension ( LDGNUM, 2 ) if ICOMPQ = 1 and
*                  dimension ( N ) if ICOMPQ = 0.
*         On exit, DIFR(I, 1) is the distance between I-th updated
*         (undeflated) singular value and the I+1-th (undeflated) old
*         singular value.
*
*         If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
*         normalizing factors for the right singular vector matrix.
*
*         See DLASD8 for details on DIFL and DIFR.
*
*  Z      (output) DOUBLE PRECISION array, dimension ( M )
*         The first elements of this array contain the components
*         of the deflation-adjusted updating row vector.
*
*  K      (output) INTEGER
*         Contains the dimension of the non-deflated matrix,
*         This is the order of the related secular equation. 1 <= K <=N.
*
*  C      (output) DOUBLE PRECISION
*         C contains garbage if SQRE =0 and the C-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  S      (output) DOUBLE PRECISION
*         S contains garbage if SQRE =0 and the S-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension ( 4 * M )
*
*  IWORK  (workspace) INTEGER array, dimension ( 3 * N )
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IDX, IDXC, IDXP, ISIGMA, IVFW, IVLW, IW, M,
     $                   N, N1, N2
      DOUBLE PRECISION   ORGNRM
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAMRG, DLASCL, DLASD7, DLASD8, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      N = NL + NR + 1
      M = N + SQRE
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( NL.LT.1 ) THEN
         INFO = -2
      ELSE IF( NR.LT.1 ) THEN
         INFO = -3
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -4
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -14
      ELSE IF( LDGNUM.LT.N ) THEN
         INFO = -16
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD6', -INFO )
         RETURN
      END IF
*
*     The following values are for bookkeeping purposes only.  They are
*     integer pointers which indicate the portion of the workspace
*     used by a particular array in DLASD7 and DLASD8.
*
      ISIGMA = 1
      IW = ISIGMA + N
      IVFW = IW + M
      IVLW = IVFW + M
*
      IDX = 1
      IDXC = IDX + N
      IDXP = IDXC + N
*
*     Scale.
*
      ORGNRM = MAX( ABS( ALPHA ), ABS( BETA ) )
      D( NL+1 ) = ZERO
      DO 10 I = 1, N
         IF( ABS( D( I ) ).GT.ORGNRM ) THEN
            ORGNRM = ABS( D( I ) )
         END IF
   10 CONTINUE
      CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, N, 1, D, N, INFO )
      ALPHA = ALPHA / ORGNRM
      BETA = BETA / ORGNRM
*
*     Sort and Deflate singular values.
*
      CALL DLASD7( ICOMPQ, NL, NR, SQRE, K, D, Z, WORK( IW ), VF,
     $             WORK( IVFW ), VL, WORK( IVLW ), ALPHA, BETA,
     $             WORK( ISIGMA ), IWORK( IDX ), IWORK( IDXP ), IDXQ,
     $             PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM, C, S,
     $             INFO )
*
*     Solve Secular Equation, compute DIFL, DIFR, and update VF, VL.
*
      CALL DLASD8( ICOMPQ, K, D, Z, VF, VL, DIFL, DIFR, LDGNUM,
     $             WORK( ISIGMA ), WORK( IW ), INFO )
*
*     Save the poles if ICOMPQ = 1.
*
      IF( ICOMPQ.EQ.1 ) THEN
         CALL DCOPY( K, D, 1, POLES( 1, 1 ), 1 )
         CALL DCOPY( K, WORK( ISIGMA ), 1, POLES( 1, 2 ), 1 )
      END IF
*
*     Unscale.
*
      CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, N, 1, D, N, INFO )
*
*     Prepare the IDXQ sorting permutation.
*
      N1 = K
      N2 = N - K
      CALL DLAMRG( N1, N2, D, 1, -1, IDXQ )
*
      RETURN
*
*     End of DLASD6
*
      END
      SUBROUTINE DLASD7( ICOMPQ, NL, NR, SQRE, K, D, Z, ZW, VF, VFW, VL,
     $                   VLW, ALPHA, BETA, DSIGMA, IDX, IDXP, IDXQ,
     $                   PERM, GIVPTR, GIVCOL, LDGCOL, GIVNUM, LDGNUM,
     $                   C, S, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            GIVPTR, ICOMPQ, INFO, K, LDGCOL, LDGNUM, NL,
     $                   NR, SQRE
      DOUBLE PRECISION   ALPHA, BETA, C, S
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( LDGCOL, * ), IDX( * ), IDXP( * ),
     $                   IDXQ( * ), PERM( * )
      DOUBLE PRECISION   D( * ), DSIGMA( * ), GIVNUM( LDGNUM, * ),
     $                   VF( * ), VFW( * ), VL( * ), VLW( * ), Z( * ),
     $                   ZW( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASD7 merges the two sets of singular values together into a single
*  sorted set. Then it tries to deflate the size of the problem. There
*  are two ways in which deflation can occur:  when two or more singular
*  values are close together or if there is a tiny entry in the Z
*  vector. For each such occurrence the order of the related
*  secular equation problem is reduced by one.
*
*  DLASD7 is called from DLASD6.
*
*  Arguments
*  =========
*
*  ICOMPQ  (input) INTEGER
*          Specifies whether singular vectors are to be computed
*          in compact form, as follows:
*          = 0: Compute singular values only.
*          = 1: Compute singular vectors of upper
*               bidiagonal matrix in compact form.
*
*  NL     (input) INTEGER
*         The row dimension of the upper block. NL >= 1.
*
*  NR     (input) INTEGER
*         The row dimension of the lower block. NR >= 1.
*
*  SQRE   (input) INTEGER
*         = 0: the lower block is an NR-by-NR square matrix.
*         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
*
*         The bidiagonal matrix has
*         N = NL + NR + 1 rows and
*         M = N + SQRE >= N columns.
*
*  K      (output) INTEGER
*         Contains the dimension of the non-deflated matrix, this is
*         the order of the related secular equation. 1 <= K <=N.
*
*  D      (input/output) DOUBLE PRECISION array, dimension ( N )
*         On entry D contains the singular values of the two submatrices
*         to be combined. On exit D contains the trailing (N-K) updated
*         singular values (those which were deflated) sorted into
*         increasing order.
*
*  Z      (output) DOUBLE PRECISION array, dimension ( M )
*         On exit Z contains the updating row vector in the secular
*         equation.
*
*  ZW     (workspace) DOUBLE PRECISION array, dimension ( M )
*         Workspace for Z.
*
*  VF     (input/output) DOUBLE PRECISION array, dimension ( M )
*         On entry, VF(1:NL+1) contains the first components of all
*         right singular vectors of the upper block; and VF(NL+2:M)
*         contains the first components of all right singular vectors
*         of the lower block. On exit, VF contains the first components
*         of all right singular vectors of the bidiagonal matrix.
*
*  VFW    (workspace) DOUBLE PRECISION array, dimension ( M )
*         Workspace for VF.
*
*  VL     (input/output) DOUBLE PRECISION array, dimension ( M )
*         On entry, VL(1:NL+1) contains the  last components of all
*         right singular vectors of the upper block; and VL(NL+2:M)
*         contains the last components of all right singular vectors
*         of the lower block. On exit, VL contains the last components
*         of all right singular vectors of the bidiagonal matrix.
*
*  VLW    (workspace) DOUBLE PRECISION array, dimension ( M )
*         Workspace for VL.
*
*  ALPHA  (input) DOUBLE PRECISION
*         Contains the diagonal element associated with the added row.
*
*  BETA   (input) DOUBLE PRECISION
*         Contains the off-diagonal element associated with the added
*         row.
*
*  DSIGMA (output) DOUBLE PRECISION array, dimension ( N )
*         Contains a copy of the diagonal elements (K-1 singular values
*         and one zero) in the secular equation.
*
*  IDX    (workspace) INTEGER array, dimension ( N )
*         This will contain the permutation used to sort the contents of
*         D into ascending order.
*
*  IDXP   (workspace) INTEGER array, dimension ( N )
*         This will contain the permutation used to place deflated
*         values of D at the end of the array. On output IDXP(2:K)
*         points to the nondeflated D-values and IDXP(K+1:N)
*         points to the deflated singular values.
*
*  IDXQ   (input) INTEGER array, dimension ( N )
*         This contains the permutation which separately sorts the two
*         sub-problems in D into ascending order.  Note that entries in
*         the first half of this permutation must first be moved one
*         position backward; and entries in the second half
*         must first have NL+1 added to their values.
*
*  PERM   (output) INTEGER array, dimension ( N )
*         The permutations (from deflation and sorting) to be applied
*         to each singular block. Not referenced if ICOMPQ = 0.
*
*  GIVPTR (output) INTEGER
*         The number of Givens rotations which took place in this
*         subproblem. Not referenced if ICOMPQ = 0.
*
*  GIVCOL (output) INTEGER array, dimension ( LDGCOL, 2 )
*         Each pair of numbers indicates a pair of columns to take place
*         in a Givens rotation. Not referenced if ICOMPQ = 0.
*
*  LDGCOL (input) INTEGER
*         The leading dimension of GIVCOL, must be at least N.
*
*  GIVNUM (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
*         Each number indicates the C or S value to be used in the
*         corresponding Givens rotation. Not referenced if ICOMPQ = 0.
*
*  LDGNUM (input) INTEGER
*         The leading dimension of GIVNUM, must be at least N.
*
*  C      (output) DOUBLE PRECISION
*         C contains garbage if SQRE =0 and the C-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  S      (output) DOUBLE PRECISION
*         S contains garbage if SQRE =0 and the S-value of a Givens
*         rotation related to the right null space if SQRE = 1.
*
*  INFO   (output) INTEGER
*         = 0:  successful exit.
*         < 0:  if INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, EIGHT
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0,
     $                   EIGHT = 8.0D+0 )
*     ..
*     .. Local Scalars ..
*
      INTEGER            I, IDXI, IDXJ, IDXJP, J, JP, JPREV, K2, M, N,
     $                   NLP1, NLP2
      DOUBLE PRECISION   EPS, HLFTOL, TAU, TOL, Z1
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAMRG, DROT, XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      N = NL + NR + 1
      M = N + SQRE
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( NL.LT.1 ) THEN
         INFO = -2
      ELSE IF( NR.LT.1 ) THEN
         INFO = -3
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -4
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -22
      ELSE IF( LDGNUM.LT.N ) THEN
         INFO = -24
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD7', -INFO )
         RETURN
      END IF
*
      NLP1 = NL + 1
      NLP2 = NL + 2
      IF( ICOMPQ.EQ.1 ) THEN
         GIVPTR = 0
      END IF
*
*     Generate the first part of the vector Z and move the singular
*     values in the first part of D one position backward.
*
      Z1 = ALPHA*VL( NLP1 )
      VL( NLP1 ) = ZERO
      TAU = VF( NLP1 )
      DO 10 I = NL, 1, -1
         Z( I+1 ) = ALPHA*VL( I )
         VL( I ) = ZERO
         VF( I+1 ) = VF( I )
         D( I+1 ) = D( I )
         IDXQ( I+1 ) = IDXQ( I ) + 1
   10 CONTINUE
      VF( 1 ) = TAU
*
*     Generate the second part of the vector Z.
*
      DO 20 I = NLP2, M
         Z( I ) = BETA*VF( I )
         VF( I ) = ZERO
   20 CONTINUE
*
*     Sort the singular values into increasing order
*
      DO 30 I = NLP2, N
         IDXQ( I ) = IDXQ( I ) + NLP1
   30 CONTINUE
*
*     DSIGMA, IDXC, IDXC, and ZW are used as storage space.
*
      DO 40 I = 2, N
         DSIGMA( I ) = D( IDXQ( I ) )
         ZW( I ) = Z( IDXQ( I ) )
         VFW( I ) = VF( IDXQ( I ) )
         VLW( I ) = VL( IDXQ( I ) )
   40 CONTINUE
*
      CALL DLAMRG( NL, NR, DSIGMA( 2 ), 1, 1, IDX( 2 ) )
*
      DO 50 I = 2, N
         IDXI = 1 + IDX( I )
         D( I ) = DSIGMA( IDXI )
         Z( I ) = ZW( IDXI )
         VF( I ) = VFW( IDXI )
         VL( I ) = VLW( IDXI )
   50 CONTINUE
*
*     Calculate the allowable deflation tolerence
*
      EPS = DLAMCH( 'Epsilon' )
      TOL = MAX( ABS( ALPHA ), ABS( BETA ) )
      TOL = EIGHT*EIGHT*EPS*MAX( ABS( D( N ) ), TOL )
*
*     There are 2 kinds of deflation -- first a value in the z-vector
*     is small, second two (or more) singular values are very close
*     together (their difference is small).
*
*     If the value in the z-vector is small, we simply permute the
*     array so that the corresponding singular value is moved to the
*     end.
*
*     If two values in the D-vector are close, we perform a two-sided
*     rotation designed to make one of the corresponding z-vector
*     entries zero, and then permute the array so that the deflated
*     singular value is moved to the end.
*
*     If there are multiple singular values then the problem deflates.
*     Here the number of equal singular values are found.  As each equal
*     singular value is found, an elementary reflector is computed to
*     rotate the corresponding singular subspace so that the
*     corresponding components of Z are zero in this new basis.
*
      K = 1
      K2 = N + 1
      DO 60 J = 2, N
         IF( ABS( Z( J ) ).LE.TOL ) THEN
*
*           Deflate due to small z component.
*
            K2 = K2 - 1
            IDXP( K2 ) = J
            IF( J.EQ.N )
     $         GO TO 100
         ELSE
            JPREV = J
            GO TO 70
         END IF
   60 CONTINUE
   70 CONTINUE
      J = JPREV
   80 CONTINUE
      J = J + 1
      IF( J.GT.N )
     $   GO TO 90
      IF( ABS( Z( J ) ).LE.TOL ) THEN
*
*        Deflate due to small z component.
*
         K2 = K2 - 1
         IDXP( K2 ) = J
      ELSE
*
*        Check if singular values are close enough to allow deflation.
*
         IF( ABS( D( J )-D( JPREV ) ).LE.TOL ) THEN
*
*           Deflation is possible.
*
            S = Z( JPREV )
            C = Z( J )
*
*           Find sqrt(a**2+b**2) without overflow or
*           destructive underflow.
*
            TAU = DLAPY2( C, S )
            Z( J ) = TAU
            Z( JPREV ) = ZERO
            C = C / TAU
            S = -S / TAU
*
*           Record the appropriate Givens rotation
*
            IF( ICOMPQ.EQ.1 ) THEN
               GIVPTR = GIVPTR + 1
               IDXJP = IDXQ( IDX( JPREV )+1 )
               IDXJ = IDXQ( IDX( J )+1 )
               IF( IDXJP.LE.NLP1 ) THEN
                  IDXJP = IDXJP - 1
               END IF
               IF( IDXJ.LE.NLP1 ) THEN
                  IDXJ = IDXJ - 1
               END IF
               GIVCOL( GIVPTR, 2 ) = IDXJP
               GIVCOL( GIVPTR, 1 ) = IDXJ
               GIVNUM( GIVPTR, 2 ) = C
               GIVNUM( GIVPTR, 1 ) = S
            END IF
            CALL DROT( 1, VF( JPREV ), 1, VF( J ), 1, C, S )
            CALL DROT( 1, VL( JPREV ), 1, VL( J ), 1, C, S )
            K2 = K2 - 1
            IDXP( K2 ) = JPREV
            JPREV = J
         ELSE
            K = K + 1
            ZW( K ) = Z( JPREV )
            DSIGMA( K ) = D( JPREV )
            IDXP( K ) = JPREV
            JPREV = J
         END IF
      END IF
      GO TO 80
   90 CONTINUE
*
*     Record the last singular value.
*
      K = K + 1
      ZW( K ) = Z( JPREV )
      DSIGMA( K ) = D( JPREV )
      IDXP( K ) = JPREV
*
  100 CONTINUE
*
*     Sort the singular values into DSIGMA. The singular values which
*     were not deflated go into the first K slots of DSIGMA, except
*     that DSIGMA(1) is treated separately.
*
      DO 110 J = 2, N
         JP = IDXP( J )
         DSIGMA( J ) = D( JP )
         VFW( J ) = VF( JP )
         VLW( J ) = VL( JP )
  110 CONTINUE
      IF( ICOMPQ.EQ.1 ) THEN
         DO 120 J = 2, N
            JP = IDXP( J )
            PERM( J ) = IDXQ( IDX( JP )+1 )
            IF( PERM( J ).LE.NLP1 ) THEN
               PERM( J ) = PERM( J ) - 1
            END IF
  120    CONTINUE
      END IF
*
*     The deflated singular values go back into the last N - K slots of
*     D.
*
      CALL DCOPY( N-K, DSIGMA( K+1 ), 1, D( K+1 ), 1 )
*
*     Determine DSIGMA(1), DSIGMA(2), Z(1), VF(1), VL(1), VF(M), and
*     VL(M).
*
      DSIGMA( 1 ) = ZERO
      HLFTOL = TOL / TWO
      IF( ABS( DSIGMA( 2 ) ).LE.HLFTOL )
     $   DSIGMA( 2 ) = HLFTOL
      IF( M.GT.N ) THEN
         Z( 1 ) = DLAPY2( Z1, Z( M ) )
         IF( Z( 1 ).LE.TOL ) THEN
            C = ONE
            S = ZERO
            Z( 1 ) = TOL
         ELSE
            C = Z1 / Z( 1 )
            S = -Z( M ) / Z( 1 )
         END IF
         CALL DROT( 1, VF( M ), 1, VF( 1 ), 1, C, S )
         CALL DROT( 1, VL( M ), 1, VL( 1 ), 1, C, S )
      ELSE
         IF( ABS( Z1 ).LE.TOL ) THEN
            Z( 1 ) = TOL
         ELSE
            Z( 1 ) = Z1
         END IF
      END IF
*
*     Restore Z, VF, and VL.
*
      CALL DCOPY( K-1, ZW( 2 ), 1, Z( 2 ), 1 )
      CALL DCOPY( N-1, VFW( 2 ), 1, VF( 2 ), 1 )
      CALL DCOPY( N-1, VLW( 2 ), 1, VL( 2 ), 1 )
*
      RETURN
*
*     End of DLASD7
*
      END
      SUBROUTINE DLASD8( ICOMPQ, K, D, Z, VF, VL, DIFL, DIFR, LDDIFR,
     $                   DSIGMA, WORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            ICOMPQ, INFO, K, LDDIFR
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DIFL( * ), DIFR( LDDIFR, * ),
     $                   DSIGMA( * ), VF( * ), VL( * ), WORK( * ),
     $                   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASD8 finds the square roots of the roots of the secular equation,
*  as defined by the values in DSIGMA and Z. It makes the appropriate
*  calls to DLASD4, and stores, for each  element in D, the distance
*  to its two nearest poles (elements in DSIGMA). It also updates
*  the arrays VF and VL, the first and last components of all the
*  right singular vectors of the original bidiagonal matrix.
*
*  DLASD8 is called from DLASD6.
*
*  Arguments
*  =========
*
*  ICOMPQ  (input) INTEGER
*          Specifies whether singular vectors are to be computed in
*          factored form in the calling routine:
*          = 0: Compute singular values only.
*          = 1: Compute singular vectors in factored form as well.
*
*  K       (input) INTEGER
*          The number of terms in the rational function to be solved
*          by DLASD4.  K >= 1.
*
*  D       (output) DOUBLE PRECISION array, dimension ( K )
*          On output, D contains the updated singular values.
*
*  Z       (input) DOUBLE PRECISION array, dimension ( K )
*          The first K elements of this array contain the components
*          of the deflation-adjusted updating row vector.
*
*  VF      (input/output) DOUBLE PRECISION array, dimension ( K )
*          On entry, VF contains  information passed through DBEDE8.
*          On exit, VF contains the first K components of the first
*          components of all right singular vectors of the bidiagonal
*          matrix.
*
*  VL      (input/output) DOUBLE PRECISION array, dimension ( K )
*          On entry, VL contains  information passed through DBEDE8.
*          On exit, VL contains the first K components of the last
*          components of all right singular vectors of the bidiagonal
*          matrix.
*
*  DIFL    (output) DOUBLE PRECISION array, dimension ( K )
*          On exit, DIFL(I) = D(I) - DSIGMA(I).
*
*  DIFR    (output) DOUBLE PRECISION array,
*                   dimension ( LDDIFR, 2 ) if ICOMPQ = 1 and
*                   dimension ( K ) if ICOMPQ = 0.
*          On exit, DIFR(I,1) = D(I) - DSIGMA(I+1), DIFR(K,1) is not
*          defined and will not be referenced.
*
*          If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
*          normalizing factors for the right singular vector matrix.
*
*  LDDIFR  (input) INTEGER
*          The leading dimension of DIFR, must be at least K.
*
*  DSIGMA  (input) DOUBLE PRECISION array, dimension ( K )
*          The first K elements of this array contain the old roots
*          of the deflated updating problem.  These are the poles
*          of the secular equation.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension at least 3 * K
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IWK1, IWK2, IWK2I, IWK3, IWK3I, J
      DOUBLE PRECISION   DIFLJ, DIFRJ, DJ, DSIGJ, DSIGJP, RHO, TEMP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASCL, DLASD4, DLASET, XERBLA
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DLAMC3, DNRM2
      EXTERNAL           DDOT, DLAMC3, DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( K.LT.1 ) THEN
         INFO = -2
      ELSE IF( LDDIFR.LT.K ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD8', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.1 ) THEN
         D( 1 ) = ABS( Z( 1 ) )
         DIFL( 1 ) = D( 1 )
         IF( ICOMPQ.EQ.1 ) THEN
            DIFL( 2 ) = ONE
            DIFR( 1, 2 ) = ONE
         END IF
         RETURN
      END IF
*
*     Modify values DSIGMA(i) to make sure all DSIGMA(i)-DSIGMA(j) can
*     be computed with high relative accuracy (barring over/underflow).
*     This is a problem on machines without a guard digit in
*     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
*     The following code replaces DSIGMA(I) by 2*DSIGMA(I)-DSIGMA(I),
*     which on any of these machines zeros out the bottommost
*     bit of DSIGMA(I) if it is 1; this makes the subsequent
*     subtractions DSIGMA(I)-DSIGMA(J) unproblematic when cancellation
*     occurs. On binary machines with a guard digit (almost all
*     machines) it does not change DSIGMA(I) at all. On hexadecimal
*     and decimal machines with a guard digit, it slightly
*     changes the bottommost bits of DSIGMA(I). It does not account
*     for hexadecimal or decimal machines without guard digits
*     (we know of none). We use a subroutine call to compute
*     2*DSIGMA(I) to prevent optimizing compilers from eliminating
*     this code.
*
      DO 10 I = 1, K
         DSIGMA( I ) = DLAMC3( DSIGMA( I ), DSIGMA( I ) ) - DSIGMA( I )
   10 CONTINUE
*
*     Book keeping.
*
      IWK1 = 1
      IWK2 = IWK1 + K
      IWK3 = IWK2 + K
      IWK2I = IWK2 - 1
      IWK3I = IWK3 - 1
*
*     Normalize Z.
*
      RHO = DNRM2( K, Z, 1 )
      CALL DLASCL( 'G', 0, 0, RHO, ONE, K, 1, Z, K, INFO )
      RHO = RHO*RHO
*
*     Initialize WORK(IWK3).
*
      CALL DLASET( 'A', K, 1, ONE, ONE, WORK( IWK3 ), K )
*
*     Compute the updated singular values, the arrays DIFL, DIFR,
*     and the updated Z.
*
      DO 40 J = 1, K
         CALL DLASD4( K, J, DSIGMA, Z, WORK( IWK1 ), RHO, D( J ),
     $                WORK( IWK2 ), INFO )
*
*        If the root finder fails, the computation is terminated.
*
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         WORK( IWK3I+J ) = WORK( IWK3I+J )*WORK( J )*WORK( IWK2I+J )
         DIFL( J ) = -WORK( J )
         DIFR( J, 1 ) = -WORK( J+1 )
         DO 20 I = 1, J - 1
            WORK( IWK3I+I ) = WORK( IWK3I+I )*WORK( I )*
     $                        WORK( IWK2I+I ) / ( DSIGMA( I )-
     $                        DSIGMA( J ) ) / ( DSIGMA( I )+
     $                        DSIGMA( J ) )
   20    CONTINUE
         DO 30 I = J + 1, K
            WORK( IWK3I+I ) = WORK( IWK3I+I )*WORK( I )*
     $                        WORK( IWK2I+I ) / ( DSIGMA( I )-
     $                        DSIGMA( J ) ) / ( DSIGMA( I )+
     $                        DSIGMA( J ) )
   30    CONTINUE
   40 CONTINUE
*
*     Compute updated Z.
*
      DO 50 I = 1, K
         Z( I ) = SIGN( SQRT( ABS( WORK( IWK3I+I ) ) ), Z( I ) )
   50 CONTINUE
*
*     Update VF and VL.
*
      DO 80 J = 1, K
         DIFLJ = DIFL( J )
         DJ = D( J )
         DSIGJ = -DSIGMA( J )
         IF( J.LT.K ) THEN
            DIFRJ = -DIFR( J, 1 )
            DSIGJP = -DSIGMA( J+1 )
         END IF
         WORK( J ) = -Z( J ) / DIFLJ / ( DSIGMA( J )+DJ )
         DO 60 I = 1, J - 1
            WORK( I ) = Z( I ) / ( DLAMC3( DSIGMA( I ), DSIGJ )-DIFLJ )
     $                   / ( DSIGMA( I )+DJ )
   60    CONTINUE
         DO 70 I = J + 1, K
            WORK( I ) = Z( I ) / ( DLAMC3( DSIGMA( I ), DSIGJP )+DIFRJ )
     $                   / ( DSIGMA( I )+DJ )
   70    CONTINUE
         TEMP = DNRM2( K, WORK, 1 )
         WORK( IWK2I+J ) = DDOT( K, WORK, 1, VF, 1 ) / TEMP
         WORK( IWK3I+J ) = DDOT( K, WORK, 1, VL, 1 ) / TEMP
         IF( ICOMPQ.EQ.1 ) THEN
            DIFR( J, 2 ) = TEMP
         END IF
   80 CONTINUE
*
      CALL DCOPY( K, WORK( IWK2 ), 1, VF, 1 )
      CALL DCOPY( K, WORK( IWK3 ), 1, VL, 1 )
*
      RETURN
*
*     End of DLASD8
*
      END
      SUBROUTINE DLASD9( ICOMPQ, LDU, K, D, Z, VF, VL, DIFL, DIFR,
     $                   DSIGMA, WORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.0) --
*     Univ. of Tennessee, Oak Ridge National Lab, Argonne National Lab,
*     Courant Institute, NAG Ltd., and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      INTEGER            ICOMPQ, INFO, K, LDU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DIFL( * ), DIFR( LDU, * ), DSIGMA( * ),
     $                   VF( * ), VL( * ), WORK( * ), Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASD9 finds the square roots of the roots of the secular equation,
*  as defined by the values in DSIGMA and Z.  It makes the
*  appropriate calls to DLASD4, and stores, for each  element in D,
*  the distance to its two nearest poles (elements in DSIGMA). It also
*  updates the arrays VF and VL, the first and last components of all
*  the right singular vectors of the original bidiagonal matrix.
*
*  DLASD9 is called from DLASD7.
*
*  Arguments
*  =========
*
*  ICOMPQ  (input) INTEGER
*          Specifies whether singular vectors are to be computed in
*          factored form in the calling routine:
*
*             ICOMPQ = 0             Compute singular values only.
*
*             ICOMPQ = 1             Compute singular vector matrices in
*                                    factored form also.
*  K       (input) INTEGER
*          The number of terms in the rational function to be solved by
*          DLASD4.  K >= 1.
*
*  D       (output) DOUBLE PRECISION array, dimension(K)
*          D(I) contains the updated singular values.
*
*  DSIGMA  (input) DOUBLE PRECISION array, dimension(K)
*          The first K elements of this array contain the old roots
*          of the deflated updating problem.  These are the poles
*          of the secular equation.
*
*  Z       (input) DOUBLE PRECISION array, dimension (K)
*          The first K elements of this array contain the components
*          of the deflation-adjusted updating row vector.
*
*  VF      (input/output) DOUBLE PRECISION array, dimension(K)
*          On entry, VF contains  information passed through SBEDE8.f
*          On exit, VF contains the first K components of the first
*          components of all right singular vectors of the bidiagonal
*          matrix.
*
*  VL      (input/output) DOUBLE PRECISION array, dimension(K)
*          On entry, VL contains  information passed through SBEDE8.f
*          On exit, VL contains the first K components of the last
*          components of all right singular vectors of the bidiagonal
*          matrix.
*
*  DIFL    (output) DOUBLE PRECISION array, dimension (K).
*          On exit, DIFL(I) = D(I) - DSIGMA(I).
*
*  DIFR    (output) DOUBLE PRECISION array,
*                              dimension (LDU, 2) if ICOMPQ =1 and
*                              dimension (K) if ICOMPQ = 0.
*          On exit, DIFR(I, 1) = D(I) - DSIGMA(I+1), DIFR(K, 1) is not
*          defined and will not be referenced.
*
*          If ICOMPQ = 1, DIFR(1:K, 2) is an array containing the
*          normalizing factors for the right singular vector matrix.
*
*  WORK    (workspace) DOUBLE PRECISION array,
*                                 dimension at least (3 * K)
*          Workspace.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IWK1, IWK2, IWK2I, IWK3, IWK3I, J
      DOUBLE PRECISION   DIFLJ, DIFRJ, DJ, DJP1, DSIGJ, DSIGJP, RHO,
     $                   TEMP
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DDOT, DLAMC3, DNRM2
      EXTERNAL           DDOT, DLAMC3, DNRM2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASCL, DLASD4, DLASET, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( K.LT.1 ) THEN
         INFO = -3
      ELSE IF( LDU.LT.K ) THEN
         INFO = -2
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASD9', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( K.EQ.1 ) THEN
         D( 1 ) = ABS( Z( 1 ) )
         DIFL( 1 ) = D( 1 )
         IF( ICOMPQ.EQ.1 ) THEN
            DIFL( 2 ) = ONE
            DIFR( 1, 2 ) = ONE
         END IF
         RETURN
      END IF
*
*     Modify values DSIGMA(i) to make sure all DSIGMA(i)-DSIGMA(j) can
*     be computed with high relative accuracy (barring over/underflow).
*     This is a problem on machines without a guard digit in
*     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
*     The following code replaces DSIGMA(I) by 2*DSIGMA(I)-DSIGMA(I),
*     which on any of these machines zeros out the bottommost
*     bit of DSIGMA(I) if it is 1; this makes the subsequent
*     subtractions DSIGMA(I)-DSIGMA(J) unproblematic when cancellation
*     occurs. On binary machines with a guard digit (almost all
*     machines) it does not change DSIGMA(I) at all. On hexadecimal
*     and decimal machines with a guard digit, it slightly
*     changes the bottommost bits of DSIGMA(I). It does not account
*     for hexadecimal or decimal machines without guard digits
*     (we know of none). We use a subroutine call to compute
*     2*DLAMBDA(I) to prevent optimizing compilers from eliminating
*     this code.
*
      DO 10 I = 1, K
         DSIGMA( I ) = DLAMC3( DSIGMA( I ), DSIGMA( I ) ) - DSIGMA( I )
   10 CONTINUE
*
*     Book keeping.
*
      IWK1 = 1
      IWK2 = IWK1 + K
      IWK3 = IWK2 + K
      IWK2I = IWK2 - 1
      IWK3I = IWK3 - 1
*
*     Normalize Z.
*
      RHO = DNRM2( K, Z, 1 )
      CALL DLASCL( 'G', 0, 0, RHO, ONE, K, 1, Z, K, INFO )
      RHO = RHO*RHO
*
*     Initialize WORK(IWK3).
*
      CALL DLASET( 'A', K, 1, ONE, ONE, WORK( IWK3 ), K )
*
*     Compute the updated singular values, the arrays DIFL, DIFR,
*     and the updated Z.
*
      DO 40 J = 1, K
         CALL DLASD4( K, J, DSIGMA, Z, WORK( IWK1 ), RHO, D( J ),
     $                WORK( IWK2 ), INFO )
*
*        If the root finder fails, the computation is terminated.
*
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         WORK( IWK3I+J ) = WORK( IWK3I+J )*WORK( J )*WORK( IWK2I+J )
         DIFL( J ) = -WORK( J )
         DIFR( J, 1 ) = -WORK( J+1 )
         DO 20 I = 1, J - 1
            WORK( IWK3I+I ) = WORK( IWK3I+I )*WORK( I )*
     $                        WORK( IWK2I+I ) / ( DSIGMA( I )-
     $                        DSIGMA( J ) ) / ( DSIGMA( I )+
     $                        DSIGMA( J ) )
   20    CONTINUE
         DO 30 I = J + 1, K
            WORK( IWK3I+I ) = WORK( IWK3I+I )*WORK( I )*
     $                        WORK( IWK2I+I ) / ( DSIGMA( I )-
     $                        DSIGMA( J ) ) / ( DSIGMA( I )+
     $                        DSIGMA( J ) )
   30    CONTINUE
   40 CONTINUE
*
*     Compute updated Z.
*
      DO 50 I = 1, K
         Z( I ) = SIGN( SQRT( ABS( WORK( IWK3I+I ) ) ), Z( I ) )
   50 CONTINUE
*
*     Update VF and VL.
*
      DO 80 J = 1, K
         DIFLJ = DIFL( J )
         DJ = D( J )
         DSIGJ = -DSIGMA( J )
         IF( J.LT.K ) THEN
            DIFRJ = -DIFR( J, 1 )
            DJP1 = D( J+1 )
            DSIGJP = -DSIGMA( J+1 )
         END IF
         WORK( J ) = -Z( J ) / DIFLJ / ( DSIGMA( J )+DJ )
         DO 60 I = 1, J - 1
            WORK( I ) = Z( I ) / ( DLAMC3( DSIGMA( I ), DSIGJ )-DIFLJ )
     $                   / ( DSIGMA( I )+DJ )
   60    CONTINUE
         DO 70 I = J + 1, K
            WORK( I ) = Z( I ) / ( DLAMC3( DSIGMA( I ), DSIGJP )+DIFRJ )
     $                   / ( DSIGMA( I )+DJ )
   70    CONTINUE
         TEMP = DNRM2( K, WORK, 1 )
         WORK( IWK2I+J ) = DDOT( K, WORK, 1, VF, 1 ) / TEMP
         WORK( IWK3I+J ) = DDOT( K, WORK, 1, VL, 1 ) / TEMP
         IF( ICOMPQ.EQ.1 ) THEN
            DIFR( J, 2 ) = TEMP
         END IF
   80 CONTINUE
*
      CALL DCOPY( K, WORK( IWK2 ), 1, VF, 1 )
      CALL DCOPY( K, WORK( IWK3 ), 1, VL, 1 )
*
      RETURN
*
*     End of DLASD9
*
      END
      SUBROUTINE DLASDA( ICOMPQ, SMLSIZ, N, SQRE, D, E, U, LDU, VT, K,
     $                   DIFL, DIFR, Z, POLES, GIVPTR, GIVCOL, LDGCOL,
     $                   PERM, GIVNUM, C, S, WORK, IWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            ICOMPQ, INFO, LDGCOL, LDU, N, SMLSIZ, SQRE
*     ..
*     .. Array Arguments ..
      INTEGER            GIVCOL( LDGCOL, * ), GIVPTR( * ), IWORK( * ),
     $                   K( * ), PERM( LDGCOL, * )
      DOUBLE PRECISION   C( * ), D( * ), DIFL( LDU, * ), DIFR( LDU, * ),
     $                   E( * ), GIVNUM( LDU, * ), POLES( LDU, * ),
     $                   S( * ), U( LDU, * ), VT( LDU, * ), WORK( * ),
     $                   Z( LDU, * )
*     ..
*
*  Purpose
*  =======
*
*  Using a divide and conquer approach, DLASDA computes the singular
*  value decomposition (SVD) of a real upper bidiagonal N-by-M matrix
*  B with diagonal D and offdiagonal E, where M = N + SQRE. The
*  algorithm computes the singular values in the SVD B = U * S * VT.
*  The orthogonal matrices U and VT are optionally computed in
*  compact form.
*
*  A related subroutine, DLASD0, computes the singular values and
*  the singular vectors in explicit form.
*
*  Arguments
*  =========
*
*  ICOMPQ (input) INTEGER
*         Specifies whether singular vectors are to be computed
*         in compact form, as follows
*         = 0: Compute singular values only.
*         = 1: Compute singular vectors of upper bidiagonal
*              matrix in compact form.
*
*  SMLSIZ (input) INTEGER
*         The maximum size of the subproblems at the bottom of the
*         computation tree.
*
*  N      (input) INTEGER
*         The row dimension of the upper bidiagonal matrix. This is
*         also the dimension of the main diagonal array D.
*
*  SQRE   (input) INTEGER
*         Specifies the column dimension of the bidiagonal matrix.
*         = 0: The bidiagonal matrix has column dimension M = N;
*         = 1: The bidiagonal matrix has column dimension M = N + 1.
*
*  D      (input/output) DOUBLE PRECISION array, dimension ( N )
*         On entry D contains the main diagonal of the bidiagonal
*         matrix. On exit D, if INFO = 0, contains its singular values.
*
*  E      (input) DOUBLE PRECISION array, dimension ( M-1 )
*         Contains the subdiagonal entries of the bidiagonal matrix.
*         On exit, E has been destroyed.
*
*  U      (output) DOUBLE PRECISION array,
*         dimension ( LDU, SMLSIZ ) if ICOMPQ = 1, and not referenced
*         if ICOMPQ = 0. If ICOMPQ = 1, on exit, U contains the left
*         singular vector matrices of all subproblems at the bottom
*         level.
*
*  LDU    (input) INTEGER, LDU = > N.
*         The leading dimension of arrays U, VT, DIFL, DIFR, POLES,
*         GIVNUM, and Z.
*
*  VT     (output) DOUBLE PRECISION array,
*         dimension ( LDU, SMLSIZ+1 ) if ICOMPQ = 1, and not referenced
*         if ICOMPQ = 0. If ICOMPQ = 1, on exit, VT' contains the right
*         singular vector matrices of all subproblems at the bottom
*         level.
*
*  K      (output) INTEGER array,
*         dimension ( N ) if ICOMPQ = 1 and dimension 1 if ICOMPQ = 0.
*         If ICOMPQ = 1, on exit, K(I) is the dimension of the I-th
*         secular equation on the computation tree.
*
*  DIFL   (output) DOUBLE PRECISION array, dimension ( LDU, NLVL ),
*         where NLVL = floor(log_2 (N/SMLSIZ))).
*
*  DIFR   (output) DOUBLE PRECISION array,
*                  dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1 and
*                  dimension ( N ) if ICOMPQ = 0.
*         If ICOMPQ = 1, on exit, DIFL(1:N, I) and DIFR(1:N, 2 * I - 1)
*         record distances between singular values on the I-th
*         level and singular values on the (I -1)-th level, and
*         DIFR(1:N, 2 * I ) contains the normalizing factors for
*         the right singular vector matrix. See DLASD8 for details.
*
*  Z      (output) DOUBLE PRECISION array,
*                  dimension ( LDU, NLVL ) if ICOMPQ = 1 and
*                  dimension ( N ) if ICOMPQ = 0.
*         The first K elements of Z(1, I) contain the components of
*         the deflation-adjusted updating row vector for subproblems
*         on the I-th level.
*
*  POLES  (output) DOUBLE PRECISION array,
*         dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1, and not referenced
*         if ICOMPQ = 0. If ICOMPQ = 1, on exit, POLES(1, 2*I - 1) and
*         POLES(1, 2*I) contain  the new and old singular values
*         involved in the secular equations on the I-th level.
*
*  GIVPTR (output) INTEGER array,
*         dimension ( N ) if ICOMPQ = 1, and not referenced if
*         ICOMPQ = 0. If ICOMPQ = 1, on exit, GIVPTR( I ) records
*         the number of Givens rotations performed on the I-th
*         problem on the computation tree.
*
*  GIVCOL (output) INTEGER array,
*         dimension ( LDGCOL, 2 * NLVL ) if ICOMPQ = 1, and not
*         referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
*         GIVCOL(1, 2 *I - 1) and GIVCOL(1, 2 *I) record the locations
*         of Givens rotations performed on the I-th level on the
*         computation tree.
*
*  LDGCOL (input) INTEGER, LDGCOL = > N.
*         The leading dimension of arrays GIVCOL and PERM.
*
*  PERM   (output) INTEGER array,
*         dimension ( LDGCOL, NLVL ) if ICOMPQ = 1, and not referenced
*         if ICOMPQ = 0. If ICOMPQ = 1, on exit, PERM(1, I) records
*         permutations done on the I-th level of the computation tree.
*
*  GIVNUM (output) DOUBLE PRECISION array,
*         dimension ( LDU,  2 * NLVL ) if ICOMPQ = 1, and not
*         referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
*         GIVNUM(1, 2 *I - 1) and GIVNUM(1, 2 *I) record the C- and S-
*         values of Givens rotations performed on the I-th level on
*         the computation tree.
*
*  C      (output) DOUBLE PRECISION array,
*         dimension ( N ) if ICOMPQ = 1, and dimension 1 if ICOMPQ = 0.
*         If ICOMPQ = 1 and the I-th subproblem is not square, on exit,
*         C( I ) contains the C-value of a Givens rotation related to
*         the right null space of the I-th subproblem.
*
*  S      (output) DOUBLE PRECISION array, dimension ( N ) if
*         ICOMPQ = 1, and dimension 1 if ICOMPQ = 0. If ICOMPQ = 1
*         and the I-th subproblem is not square, on exit, S( I )
*         contains the S-value of a Givens rotation related to
*         the right null space of the I-th subproblem.
*
*  WORK   (workspace) DOUBLE PRECISION array, dimension
*         (6 * N + (SMLSIZ + 1)*(SMLSIZ + 1)).
*
*  IWORK  (workspace) INTEGER array.
*         Dimension must be at least (7 * N).
*
*  INFO   (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, an singular value did not converge
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, I1, IC, IDXQ, IDXQI, IM1, INODE, ITEMP, IWK,
     $                   J, LF, LL, LVL, LVL2, M, NCC, ND, NDB1, NDIML,
     $                   NDIMR, NL, NLF, NLP1, NLVL, NR, NRF, NRP1, NRU,
     $                   NWORK1, NWORK2, SMLSZP, SQREI, VF, VFI, VL, VLI
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLASD6, DLASDQ, DLASDT, DLASET, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
*
      IF( ( ICOMPQ.LT.0 ) .OR. ( ICOMPQ.GT.1 ) ) THEN
         INFO = -1
      ELSE IF( SMLSIZ.LT.3 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -4
      ELSE IF( LDU.LT.( N+SQRE ) ) THEN
         INFO = -8
      ELSE IF( LDGCOL.LT.N ) THEN
         INFO = -17
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASDA', -INFO )
         RETURN
      END IF
*
      M = N + SQRE
*
*     If the input matrix is too small, call DLASDQ to find the SVD.
*
      IF( N.LE.SMLSIZ ) THEN
         IF( ICOMPQ.EQ.0 ) THEN
            CALL DLASDQ( 'U', SQRE, N, 0, 0, 0, D, E, VT, LDU, U, LDU,
     $                   U, LDU, WORK, INFO )
         ELSE
            CALL DLASDQ( 'U', SQRE, N, M, N, 0, D, E, VT, LDU, U, LDU,
     $                   U, LDU, WORK, INFO )
         END IF
         RETURN
      END IF
*
*     Book-keeping and  set up the computation tree.
*
      INODE = 1
      NDIML = INODE + N
      NDIMR = NDIML + N
      IDXQ = NDIMR + N
      IWK = IDXQ + N
*
      NCC = 0
      NRU = 0
*
      SMLSZP = SMLSIZ + 1
      VF = 1
      VL = VF + M
      NWORK1 = VL + M
      NWORK2 = NWORK1 + SMLSZP*SMLSZP
*
      CALL DLASDT( N, NLVL, ND, IWORK( INODE ), IWORK( NDIML ),
     $             IWORK( NDIMR ), SMLSIZ )
*
*     for the nodes on bottom level of the tree, solve
*     their subproblems by DLASDQ.
*
      NDB1 = ( ND+1 ) / 2
      DO 30 I = NDB1, ND
*
*        IC : center row of each node
*        NL : number of rows of left  subproblem
*        NR : number of rows of right subproblem
*        NLF: starting row of the left   subproblem
*        NRF: starting row of the right  subproblem
*
         I1 = I - 1
         IC = IWORK( INODE+I1 )
         NL = IWORK( NDIML+I1 )
         NLP1 = NL + 1
         NR = IWORK( NDIMR+I1 )
         NLF = IC - NL
         NRF = IC + 1
         IDXQI = IDXQ + NLF - 2
         VFI = VF + NLF - 1
         VLI = VL + NLF - 1
         SQREI = 1
         IF( ICOMPQ.EQ.0 ) THEN
            CALL DLASET( 'A', NLP1, NLP1, ZERO, ONE, WORK( NWORK1 ),
     $                   SMLSZP )
            CALL DLASDQ( 'U', SQREI, NL, NLP1, NRU, NCC, D( NLF ),
     $                   E( NLF ), WORK( NWORK1 ), SMLSZP,
     $                   WORK( NWORK2 ), NL, WORK( NWORK2 ), NL,
     $                   WORK( NWORK2 ), INFO )
            ITEMP = NWORK1 + NL*SMLSZP
            CALL DCOPY( NLP1, WORK( NWORK1 ), 1, WORK( VFI ), 1 )
            CALL DCOPY( NLP1, WORK( ITEMP ), 1, WORK( VLI ), 1 )
         ELSE
            CALL DLASET( 'A', NL, NL, ZERO, ONE, U( NLF, 1 ), LDU )
            CALL DLASET( 'A', NLP1, NLP1, ZERO, ONE, VT( NLF, 1 ), LDU )
            CALL DLASDQ( 'U', SQREI, NL, NLP1, NL, NCC, D( NLF ),
     $                   E( NLF ), VT( NLF, 1 ), LDU, U( NLF, 1 ), LDU,
     $                   U( NLF, 1 ), LDU, WORK( NWORK1 ), INFO )
            CALL DCOPY( NLP1, VT( NLF, 1 ), 1, WORK( VFI ), 1 )
            CALL DCOPY( NLP1, VT( NLF, NLP1 ), 1, WORK( VLI ), 1 )
         END IF
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         DO 10 J = 1, NL
            IWORK( IDXQI+J ) = J
   10    CONTINUE
         IF( ( I.EQ.ND ) .AND. ( SQRE.EQ.0 ) ) THEN
            SQREI = 0
         ELSE
            SQREI = 1
         END IF
         IDXQI = IDXQI + NLP1
         VFI = VFI + NLP1
         VLI = VLI + NLP1
         NRP1 = NR + SQREI
         IF( ICOMPQ.EQ.0 ) THEN
            CALL DLASET( 'A', NRP1, NRP1, ZERO, ONE, WORK( NWORK1 ),
     $                   SMLSZP )
            CALL DLASDQ( 'U', SQREI, NR, NRP1, NRU, NCC, D( NRF ),
     $                   E( NRF ), WORK( NWORK1 ), SMLSZP,
     $                   WORK( NWORK2 ), NR, WORK( NWORK2 ), NR,
     $                   WORK( NWORK2 ), INFO )
            ITEMP = NWORK1 + ( NRP1-1 )*SMLSZP
            CALL DCOPY( NRP1, WORK( NWORK1 ), 1, WORK( VFI ), 1 )
            CALL DCOPY( NRP1, WORK( ITEMP ), 1, WORK( VLI ), 1 )
         ELSE
            CALL DLASET( 'A', NR, NR, ZERO, ONE, U( NRF, 1 ), LDU )
            CALL DLASET( 'A', NRP1, NRP1, ZERO, ONE, VT( NRF, 1 ), LDU )
            CALL DLASDQ( 'U', SQREI, NR, NRP1, NR, NCC, D( NRF ),
     $                   E( NRF ), VT( NRF, 1 ), LDU, U( NRF, 1 ), LDU,
     $                   U( NRF, 1 ), LDU, WORK( NWORK1 ), INFO )
            CALL DCOPY( NRP1, VT( NRF, 1 ), 1, WORK( VFI ), 1 )
            CALL DCOPY( NRP1, VT( NRF, NRP1 ), 1, WORK( VLI ), 1 )
         END IF
         IF( INFO.NE.0 ) THEN
            RETURN
         END IF
         DO 20 J = 1, NR
            IWORK( IDXQI+J ) = J
   20    CONTINUE
   30 CONTINUE
*
*     Now conquer each subproblem bottom-up.
*
      J = 2**NLVL
      DO 50 LVL = NLVL, 1, -1
         LVL2 = LVL*2 - 1
*
*        Find the first node LF and last node LL on
*        the current level LVL.
*
         IF( LVL.EQ.1 ) THEN
            LF = 1
            LL = 1
         ELSE
            LF = 2**( LVL-1 )
            LL = 2*LF - 1
         END IF
         DO 40 I = LF, LL
            IM1 = I - 1
            IC = IWORK( INODE+IM1 )
            NL = IWORK( NDIML+IM1 )
            NR = IWORK( NDIMR+IM1 )
            NLF = IC - NL
            NRF = IC + 1
            IF( I.EQ.LL ) THEN
               SQREI = SQRE
            ELSE
               SQREI = 1
            END IF
            VFI = VF + NLF - 1
            VLI = VL + NLF - 1
            IDXQI = IDXQ + NLF - 1
            ALPHA = D( IC )
            BETA = E( IC )
            IF( ICOMPQ.EQ.0 ) THEN
               CALL DLASD6( ICOMPQ, NL, NR, SQREI, D( NLF ),
     $                      WORK( VFI ), WORK( VLI ), ALPHA, BETA,
     $                      IWORK( IDXQI ), PERM, GIVPTR( 1 ), GIVCOL,
     $                      LDGCOL, GIVNUM, LDU, POLES, DIFL, DIFR, Z,
     $                      K( 1 ), C( 1 ), S( 1 ), WORK( NWORK1 ),
     $                      IWORK( IWK ), INFO )
            ELSE
               J = J - 1
               CALL DLASD6( ICOMPQ, NL, NR, SQREI, D( NLF ),
     $                      WORK( VFI ), WORK( VLI ), ALPHA, BETA,
     $                      IWORK( IDXQI ), PERM( NLF, LVL ),
     $                      GIVPTR( J ), GIVCOL( NLF, LVL2 ), LDGCOL,
     $                      GIVNUM( NLF, LVL2 ), LDU,
     $                      POLES( NLF, LVL2 ), DIFL( NLF, LVL ),
     $                      DIFR( NLF, LVL2 ), Z( NLF, LVL ), K( J ),
     $                      C( J ), S( J ), WORK( NWORK1 ),
     $                      IWORK( IWK ), INFO )
            END IF
            IF( INFO.NE.0 ) THEN
               RETURN
            END IF
   40    CONTINUE
   50 CONTINUE
*
      RETURN
*
*     End of DLASDA
*
      END
      SUBROUTINE DLASDQ( UPLO, SQRE, N, NCVT, NRU, NCC, D, E, VT, LDVT,
     $                   U, LDU, C, LDC, WORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDC, LDU, LDVT, N, NCC, NCVT, NRU, SQRE
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), D( * ), E( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASDQ computes the singular value decomposition (SVD) of a real
*  (upper or lower) bidiagonal matrix with diagonal D and offdiagonal
*  E, accumulating the transformations if desired. Letting B denote
*  the input bidiagonal matrix, the algorithm computes orthogonal
*  matrices Q and P such that B = Q * S * P' (P' denotes the transpose
*  of P). The singular values S are overwritten on D.
*
*  The input matrix U  is changed to U  * Q  if desired.
*  The input matrix VT is changed to P' * VT if desired.
*  The input matrix C  is changed to Q' * C  if desired.
*
*  See "Computing  Small Singular Values of Bidiagonal Matrices With
*  Guaranteed High Relative Accuracy," by J. Demmel and W. Kahan,
*  LAPACK Working Note #3, for a detailed description of the algorithm.
*
*  Arguments
*  =========
*
*  UPLO  (input) CHARACTER*1
*        On entry, UPLO specifies whether the input bidiagonal matrix
*        is upper or lower bidiagonal, and wether it is square are
*        not.
*           UPLO = 'U' or 'u'   B is upper bidiagonal.
*           UPLO = 'L' or 'l'   B is lower bidiagonal.
*
*  SQRE  (input) INTEGER
*        = 0: then the input matrix is N-by-N.
*        = 1: then the input matrix is N-by-(N+1) if UPLU = 'U' and
*             (N+1)-by-N if UPLU = 'L'.
*
*        The bidiagonal matrix has
*        N = NL + NR + 1 rows and
*        M = N + SQRE >= N columns.
*
*  N     (input) INTEGER
*        On entry, N specifies the number of rows and columns
*        in the matrix. N must be at least 0.
*
*  NCVT  (input) INTEGER
*        On entry, NCVT specifies the number of columns of
*        the matrix VT. NCVT must be at least 0.
*
*  NRU   (input) INTEGER
*        On entry, NRU specifies the number of rows of
*        the matrix U. NRU must be at least 0.
*
*  NCC   (input) INTEGER
*        On entry, NCC specifies the number of columns of
*        the matrix C. NCC must be at least 0.
*
*  D     (input/output) DOUBLE PRECISION array, dimension (N)
*        On entry, D contains the diagonal entries of the
*        bidiagonal matrix whose SVD is desired. On normal exit,
*        D contains the singular values in ascending order.
*
*  E     (input/output) DOUBLE PRECISION array.
*        dimension is (N-1) if SQRE = 0 and N if SQRE = 1.
*        On entry, the entries of E contain the offdiagonal entries
*        of the bidiagonal matrix whose SVD is desired. On normal
*        exit, E will contain 0. If the algorithm does not converge,
*        D and E will contain the diagonal and superdiagonal entries
*        of a bidiagonal matrix orthogonally equivalent to the one
*        given as input.
*
*  VT    (input/output) DOUBLE PRECISION array, dimension (LDVT, NCVT)
*        On entry, contains a matrix which on exit has been
*        premultiplied by P', dimension N-by-NCVT if SQRE = 0
*        and (N+1)-by-NCVT if SQRE = 1 (not referenced if NCVT=0).
*
*  LDVT  (input) INTEGER
*        On entry, LDVT specifies the leading dimension of VT as
*        declared in the calling (sub) program. LDVT must be at
*        least 1. If NCVT is nonzero LDVT must also be at least N.
*
*  U     (input/output) DOUBLE PRECISION array, dimension (LDU, N)
*        On entry, contains a  matrix which on exit has been
*        postmultiplied by Q, dimension NRU-by-N if SQRE = 0
*        and NRU-by-(N+1) if SQRE = 1 (not referenced if NRU=0).
*
*  LDU   (input) INTEGER
*        On entry, LDU  specifies the leading dimension of U as
*        declared in the calling (sub) program. LDU must be at
*        least max( 1, NRU ) .
*
*  C     (input/output) DOUBLE PRECISION array, dimension (LDC, NCC)
*        On entry, contains an N-by-NCC matrix which on exit
*        has been premultiplied by Q'  dimension N-by-NCC if SQRE = 0
*        and (N+1)-by-NCC if SQRE = 1 (not referenced if NCC=0).
*
*  LDC   (input) INTEGER
*        On entry, LDC  specifies the leading dimension of C as
*        declared in the calling (sub) program. LDC must be at
*        least 1. If NCC is nonzero, LDC must also be at least N.
*
*  WORK  (workspace) DOUBLE PRECISION array, dimension (4*N)
*        Workspace. Only referenced if one of NCVT, NRU, or NCC is
*        nonzero, and if N is at least 2.
*
*  INFO  (output) INTEGER
*        On exit, a value of 0 indicates a successful exit.
*        If INFO < 0, argument number -INFO is illegal.
*        If INFO > 0, the algorithm did not converge, and INFO
*        specifies how many superdiagonals did not converge.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ROTATE
      INTEGER            I, ISUB, IUPLO, J, NP1, SQRE1
      DOUBLE PRECISION   CS, R, SMIN, SN
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSQR, DLARTG, DLASR, DSWAP, XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IUPLO = 0
      IF( LSAME( UPLO, 'U' ) )
     $   IUPLO = 1
      IF( LSAME( UPLO, 'L' ) )
     $   IUPLO = 2
      IF( IUPLO.EQ.0 ) THEN
         INFO = -1
      ELSE IF( ( SQRE.LT.0 ) .OR. ( SQRE.GT.1 ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NCVT.LT.0 ) THEN
         INFO = -4
      ELSE IF( NRU.LT.0 ) THEN
         INFO = -5
      ELSE IF( NCC.LT.0 ) THEN
         INFO = -6
      ELSE IF( ( NCVT.EQ.0 .AND. LDVT.LT.1 ) .OR.
     $         ( NCVT.GT.0 .AND. LDVT.LT.MAX( 1, N ) ) ) THEN
         INFO = -10
      ELSE IF( LDU.LT.MAX( 1, NRU ) ) THEN
         INFO = -12
      ELSE IF( ( NCC.EQ.0 .AND. LDC.LT.1 ) .OR.
     $         ( NCC.GT.0 .AND. LDC.LT.MAX( 1, N ) ) ) THEN
         INFO = -14
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASDQ', -INFO )
         RETURN
      END IF
      IF( N.EQ.0 )
     $   RETURN
*
*     ROTATE is true if any singular vectors desired, false otherwise
*
      ROTATE = ( NCVT.GT.0 ) .OR. ( NRU.GT.0 ) .OR. ( NCC.GT.0 )
      NP1 = N + 1
      SQRE1 = SQRE
*
*     If matrix non-square upper bidiagonal, rotate to be lower
*     bidiagonal.  The rotations are on the right.
*
      IF( ( IUPLO.EQ.1 ) .AND. ( SQRE1.EQ.1 ) ) THEN
         DO 10 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            IF( ROTATE ) THEN
               WORK( I ) = CS
               WORK( N+I ) = SN
            END IF
   10    CONTINUE
         CALL DLARTG( D( N ), E( N ), CS, SN, R )
         D( N ) = R
         E( N ) = ZERO
         IF( ROTATE ) THEN
            WORK( N ) = CS
            WORK( N+N ) = SN
         END IF
         IUPLO = 2
         SQRE1 = 0
*
*        Update singular vectors if desired.
*
         IF( NCVT.GT.0 )
     $      CALL DLASR( 'L', 'V', 'F', NP1, NCVT, WORK( 1 ),
     $                  WORK( NP1 ), VT, LDVT )
      END IF
*
*     If matrix lower bidiagonal, rotate to be upper bidiagonal
*     by applying Givens rotations on the left.
*
      IF( IUPLO.EQ.2 ) THEN
         DO 20 I = 1, N - 1
            CALL DLARTG( D( I ), E( I ), CS, SN, R )
            D( I ) = R
            E( I ) = SN*D( I+1 )
            D( I+1 ) = CS*D( I+1 )
            IF( ROTATE ) THEN
               WORK( I ) = CS
               WORK( N+I ) = SN
            END IF
   20    CONTINUE
*
*        If matrix (N+1)-by-N lower bidiagonal, one additional
*        rotation is needed.
*
         IF( SQRE1.EQ.1 ) THEN
            CALL DLARTG( D( N ), E( N ), CS, SN, R )
            D( N ) = R
            IF( ROTATE ) THEN
               WORK( N ) = CS
               WORK( N+N ) = SN
            END IF
         END IF
*
*        Update singular vectors if desired.
*
         IF( NRU.GT.0 ) THEN
            IF( SQRE1.EQ.0 ) THEN
               CALL DLASR( 'R', 'V', 'F', NRU, N, WORK( 1 ),
     $                     WORK( NP1 ), U, LDU )
            ELSE
               CALL DLASR( 'R', 'V', 'F', NRU, NP1, WORK( 1 ),
     $                     WORK( NP1 ), U, LDU )
            END IF
         END IF
         IF( NCC.GT.0 ) THEN
            IF( SQRE1.EQ.0 ) THEN
               CALL DLASR( 'L', 'V', 'F', N, NCC, WORK( 1 ),
     $                     WORK( NP1 ), C, LDC )
            ELSE
               CALL DLASR( 'L', 'V', 'F', NP1, NCC, WORK( 1 ),
     $                     WORK( NP1 ), C, LDC )
            END IF
         END IF
      END IF
*
*     Call DBDSQR to compute the SVD of the reduced real
*     N-by-N upper bidiagonal matrix.
*
      CALL DBDSQR( 'U', N, NCVT, NRU, NCC, D, E, VT, LDVT, U, LDU, C,
     $             LDC, WORK, INFO )
*
*     Sort the singular values into ascending order (insertion sort on
*     singular values, but only one transposition per singular vector)
*
      DO 40 I = 1, N
*
*        Scan for smallest D(I).
*
         ISUB = I
         SMIN = D( I )
         DO 30 J = I + 1, N
            IF( D( J ).LT.SMIN ) THEN
               ISUB = J
               SMIN = D( J )
            END IF
   30    CONTINUE
         IF( ISUB.NE.I ) THEN
*
*           Swap singular values and vectors.
*
            D( ISUB ) = D( I )
            D( I ) = SMIN
            IF( NCVT.GT.0 )
     $         CALL DSWAP( NCVT, VT( ISUB, 1 ), LDVT, VT( I, 1 ), LDVT )
            IF( NRU.GT.0 )
     $         CALL DSWAP( NRU, U( 1, ISUB ), 1, U( 1, I ), 1 )
            IF( NCC.GT.0 )
     $         CALL DSWAP( NCC, C( ISUB, 1 ), LDC, C( I, 1 ), LDC )
         END IF
   40 CONTINUE
*
      RETURN
*
*     End of DLASDQ
*
      END
      SUBROUTINE DLASDT( N, LVL, ND, INODE, NDIML, NDIMR, MSUB )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            LVL, MSUB, N, ND
*     ..
*     .. Array Arguments ..
      INTEGER            INODE( * ), NDIML( * ), NDIMR( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASDT creates a tree of subproblems for bidiagonal divide and
*  conquer.
*
*  Arguments
*  =========
*
*   N      (input) INTEGER
*          On entry, the number of diagonal elements of the
*          bidiagonal matrix.
*
*   LVL    (output) INTEGER
*          On exit, the number of levels on the computation tree.
*
*   ND     (output) INTEGER
*          On exit, the number of nodes on the tree.
*
*   INODE  (output) INTEGER array, dimension ( N )
*          On exit, centers of subproblems.
*
*   NDIML  (output) INTEGER array, dimension ( N )
*          On exit, row dimensions of left children.
*
*   NDIMR  (output) INTEGER array, dimension ( N )
*          On exit, row dimensions of right children.
*
*   MSUB   (input) INTEGER.
*          On entry, the maximum row dimension each subproblem at the
*          bottom of the tree can be of.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IL, IR, LLST, MAXN, NCRNT, NLVL
      DOUBLE PRECISION   TEMP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, LOG, MAX
*     ..
*     .. Executable Statements ..
*
*     Find the number of levels on the tree.
*
      MAXN = MAX( 1, N )
      TEMP = LOG( DBLE( MAXN ) / DBLE( MSUB+1 ) ) / LOG( TWO )
      LVL = INT( TEMP ) + 1
*
      I = N / 2
      INODE( 1 ) = I + 1
      NDIML( 1 ) = I
      NDIMR( 1 ) = N - I - 1
      IL = 0
      IR = 1
      LLST = 1
      DO 20 NLVL = 1, LVL - 1
*
*        Constructing the tree at (NLVL+1)-st level. The number of
*        nodes created on this level is LLST * 2.
*
         DO 10 I = 0, LLST - 1
            IL = IL + 2
            IR = IR + 2
            NCRNT = LLST + I
            NDIML( IL ) = NDIML( NCRNT ) / 2
            NDIMR( IL ) = NDIML( NCRNT ) - NDIML( IL ) - 1
            INODE( IL ) = INODE( NCRNT ) - NDIMR( IL ) - 1
            NDIML( IR ) = NDIMR( NCRNT ) / 2
            NDIMR( IR ) = NDIMR( NCRNT ) - NDIML( IR ) - 1
            INODE( IR ) = INODE( NCRNT ) + NDIML( IR ) + 1
   10    CONTINUE
         LLST = LLST*2
   20 CONTINUE
      ND = LLST*2 - 1
*
      RETURN
*
*     End of DLASDT
*
      END
      SUBROUTINE DLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      DOUBLE PRECISION   ALPHA, BETA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASET initializes an m-by-n matrix A to BETA on the diagonal and
*  ALPHA on the offdiagonals.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies the part of the matrix A to be set.
*          = 'U':      Upper triangular part is set; the strictly lower
*                      triangular part of A is not changed.
*          = 'L':      Lower triangular part is set; the strictly upper
*                      triangular part of A is not changed.
*          Otherwise:  All of the matrix A is set.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  ALPHA   (input) DOUBLE PRECISION
*          The constant to which the offdiagonal elements are to be set.
*
*  BETA    (input) DOUBLE PRECISION
*          The constant to which the diagonal elements are to be set.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On exit, the leading m-by-n submatrix of A is set as follows:
*
*          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
*          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
*          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
*
*          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
* =====================================================================
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
*        Set the strictly upper triangular or trapezoidal part of the
*        array to ALPHA.
*
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
*
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
*
*        Set the strictly lower triangular or trapezoidal part of the
*        array to ALPHA.
*
         DO 40 J = 1, MIN( M, N )
            DO 30 I = J + 1, M
               A( I, J ) = ALPHA
   30       CONTINUE
   40    CONTINUE
*
      ELSE
*
*        Set the leading m-by-n submatrix to ALPHA.
*
         DO 60 J = 1, N
            DO 50 I = 1, M
               A( I, J ) = ALPHA
   50       CONTINUE
   60    CONTINUE
      END IF
*
*     Set the first min(M,N) diagonal elements to BETA.
*
      DO 70 I = 1, MIN( M, N )
         A( I, I ) = BETA
   70 CONTINUE
*
      RETURN
*
*     End of DLASET
*
      END
      SUBROUTINE DLASQ3( I0, N0, Z, PP, DMIN, SIGMA, DESIG, QMAX, NFAIL,
     $                   ITER, NDIV, IEEE )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            I0, ITER, N0, NDIV, NFAIL, PP
      DOUBLE PRECISION   DESIG, DMIN, QMAX, SIGMA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASQ3 checks for deflation, computes a shift (TAU) and calls dqds.
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
*  TTYPE  (output) INTEGER
*         Shift type.
*
*  IEEE   (input) LOGICAL
*         Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
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
      INTEGER            IPN4, J4, N0IN, NN, TTYPE
      DOUBLE PRECISION   DMIN1, DMIN2, DN, DN1, DN2, EPS, S, SAFMIN, T,
     $                   TAU, TEMP, TOL, TOL2
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASQ4, DLASQ5, DLASQ6
*     ..
*     .. External Function ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Save statement ..
      SAVE               TTYPE
      SAVE               DMIN1, DMIN2, DN, DN1, DN2, TAU
*     ..
*     .. Data statement ..
      DATA               TTYPE / 0 /
      DATA               DMIN1 / ZERO /, DMIN2 / ZERO /, DN / ZERO /,
     $                   DN1 / ZERO /, DN2 / ZERO /, TAU / ZERO /
*     ..
*     .. Executable Statements ..
*
      N0IN = N0
      EPS = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      TOL = EPS*HUNDRD
      TOL2 = TOL**2
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
         CALL DLASQ4( I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN, DN1,
     $                DN2, TAU, TTYPE )
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
*     End of DLASQ3
*
      END
      SUBROUTINE DLASQ4( I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN,
     $                   DN1, DN2, TAU, TTYPE )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            I0, N0, N0IN, PP, TTYPE
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DN1, DN2, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASQ4 computes an approximation TAU to the smallest eigenvalue 
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
*  Further Details
*  ===============
*  CNST1 = 9/16
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
      DOUBLE PRECISION   A2, B1, B2, G, GAM, GAP1, GAP2, S
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Save statement ..
      SAVE               G
*     ..
*     .. Data statement ..
      DATA               G / ZERO /
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
*     End of DLASQ4
*
      END
      SUBROUTINE DLASQ5( I0, N0, Z, PP, TAU, DMIN, DMIN1, DMIN2, DN,
     $                   DNM1, DNM2, IEEE )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            I0, N0, PP
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2, TAU
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASQ5 computes one dqds transform in ping-pong form, one
*  version for IEEE machines another for non IEEE machines.
*
*  Arguments
*  =========
*
*  I0    (input) INTEGER
*        First index.
*
*  N0    (input) INTEGER
*        Last index.
*
*  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
*        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
*        an extra argument.
*
*  PP    (input) INTEGER
*        PP=0 for ping, PP=1 for pong.
*
*  TAU   (input) DOUBLE PRECISION
*        This is the shift.
*
*  DMIN  (output) DOUBLE PRECISION
*        Minimum value of d.
*
*  DMIN1 (output) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ).
*
*  DMIN2 (output) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
*
*  DN    (output) DOUBLE PRECISION
*        d(N0), the last value of d.
*
*  DNM1  (output) DOUBLE PRECISION
*        d(N0-1).
*
*  DNM2  (output) DOUBLE PRECISION
*        d(N0-2).
*
*  IEEE  (input) LOGICAL
*        Flag for IEEE or non IEEE arithmetic.
*
*  =====================================================================
*
*     .. Parameter ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J4, J4P2
      DOUBLE PRECISION   D, EMIN, TEMP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( ( N0-I0-1 ).LE.0 )
     $   RETURN
*
      J4 = 4*I0 + PP - 3
      EMIN = Z( J4+4 )
      D = Z( J4 ) - TAU
      DMIN = D
      DMIN1 = -Z( J4 )
*
      IF( IEEE ) THEN
*
*        Code for IEEE arithmetic.
*
         IF( PP.EQ.0 ) THEN
            DO 10 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-2 ) = D + Z( J4-1 )
               TEMP = Z( J4+1 ) / Z( J4-2 )
               D = D*TEMP - TAU
               DMIN = MIN( DMIN, D )
               Z( J4 ) = Z( J4-1 )*TEMP
               EMIN = MIN( Z( J4 ), EMIN )
   10       CONTINUE
         ELSE
            DO 20 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-3 ) = D + Z( J4 )
               TEMP = Z( J4+2 ) / Z( J4-3 )
               D = D*TEMP - TAU
               DMIN = MIN( DMIN, D )
               Z( J4-1 ) = Z( J4 )*TEMP
               EMIN = MIN( Z( J4-1 ), EMIN )
   20       CONTINUE
         END IF
*
*        Unroll last two steps.
*
         DNM2 = D
         DMIN2 = DMIN
         J4 = 4*( N0-2 ) - PP
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM2 + Z( J4P2 )
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) ) - TAU
         DMIN = MIN( DMIN, DNM1 )
*
         DMIN1 = DMIN
         J4 = J4 + 4
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM1 + Z( J4P2 )
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) ) - TAU
         DMIN = MIN( DMIN, DN )
*
      ELSE
*
*        Code for non IEEE arithmetic.
*
         IF( PP.EQ.0 ) THEN
            DO 30 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-2 ) = D + Z( J4-1 )
               IF( D.LT.ZERO ) THEN
                  RETURN
               ELSE
                  Z( J4 ) = Z( J4+1 )*( Z( J4-1 ) / Z( J4-2 ) )
                  D = Z( J4+1 )*( D / Z( J4-2 ) ) - TAU
               END IF
               DMIN = MIN( DMIN, D )
               EMIN = MIN( EMIN, Z( J4 ) )
   30       CONTINUE
         ELSE
            DO 40 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-3 ) = D + Z( J4 )
               IF( D.LT.ZERO ) THEN
                  RETURN
               ELSE
                  Z( J4-1 ) = Z( J4+2 )*( Z( J4 ) / Z( J4-3 ) )
                  D = Z( J4+2 )*( D / Z( J4-3 ) ) - TAU
               END IF
               DMIN = MIN( DMIN, D )
               EMIN = MIN( EMIN, Z( J4-1 ) )
   40       CONTINUE
         END IF
*
*        Unroll last two steps.
*
         DNM2 = D
         DMIN2 = DMIN
         J4 = 4*( N0-2 ) - PP
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM2 + Z( J4P2 )
         IF( DNM2.LT.ZERO ) THEN
            RETURN
         ELSE
            Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
            DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) ) - TAU
         END IF
         DMIN = MIN( DMIN, DNM1 )
*
         DMIN1 = DMIN
         J4 = J4 + 4
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM1 + Z( J4P2 )
         IF( DNM1.LT.ZERO ) THEN
            RETURN
         ELSE
            Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
            DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) ) - TAU
         END IF
         DMIN = MIN( DMIN, DN )
*
      END IF
*
      Z( J4+2 ) = DN
      Z( 4*N0-PP ) = EMIN
      RETURN
*
*     End of DLASQ5
*
      END
      SUBROUTINE DLASQ6( I0, N0, Z, PP, DMIN, DMIN1, DMIN2, DN,
     $                   DNM1, DNM2 )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            I0, N0, PP
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASQ6 computes one dqd (shift equal to zero) transform in
*  ping-pong form, with protection against underflow and overflow.
*
*  Arguments
*  =========
*
*  I0    (input) INTEGER
*        First index.
*
*  N0    (input) INTEGER
*        Last index.
*
*  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
*        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
*        an extra argument.
*
*  PP    (input) INTEGER
*        PP=0 for ping, PP=1 for pong.
*
*  DMIN  (output) DOUBLE PRECISION
*        Minimum value of d.
*
*  DMIN1 (output) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ).
*
*  DMIN2 (output) DOUBLE PRECISION
*        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
*
*  DN    (output) DOUBLE PRECISION
*        d(N0), the last value of d.
*
*  DNM1  (output) DOUBLE PRECISION
*        d(N0-1).
*
*  DNM2  (output) DOUBLE PRECISION
*        d(N0-2).
*
*  =====================================================================
*
*     .. Parameter ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            J4, J4P2
      DOUBLE PRECISION   D, EMIN, SAFMIN, TEMP
*     ..
*     .. External Function ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
      IF( ( N0-I0-1 ).LE.0 )
     $   RETURN
*
      SAFMIN = DLAMCH( 'Safe minimum' )
      J4 = 4*I0 + PP - 3
      EMIN = Z( J4+4 ) 
      D = Z( J4 )
      DMIN = D
*
      IF( PP.EQ.0 ) THEN
         DO 10 J4 = 4*I0, 4*( N0-3 ), 4
            Z( J4-2 ) = D + Z( J4-1 ) 
            IF( Z( J4-2 ).EQ.ZERO ) THEN
               Z( J4 ) = ZERO
               D = Z( J4+1 )
               DMIN = D
               EMIN = ZERO
            ELSE IF( SAFMIN*Z( J4+1 ).LT.Z( J4-2 ) .AND.
     $               SAFMIN*Z( J4-2 ).LT.Z( J4+1 ) ) THEN
               TEMP = Z( J4+1 ) / Z( J4-2 )
               Z( J4 ) = Z( J4-1 )*TEMP
               D = D*TEMP
            ELSE 
               Z( J4 ) = Z( J4+1 )*( Z( J4-1 ) / Z( J4-2 ) )
               D = Z( J4+1 )*( D / Z( J4-2 ) )
            END IF
            DMIN = MIN( DMIN, D )
            EMIN = MIN( EMIN, Z( J4 ) )
   10    CONTINUE
      ELSE
         DO 20 J4 = 4*I0, 4*( N0-3 ), 4
            Z( J4-3 ) = D + Z( J4 ) 
            IF( Z( J4-3 ).EQ.ZERO ) THEN
               Z( J4-1 ) = ZERO
               D = Z( J4+2 )
               DMIN = D
               EMIN = ZERO
            ELSE IF( SAFMIN*Z( J4+2 ).LT.Z( J4-3 ) .AND.
     $               SAFMIN*Z( J4-3 ).LT.Z( J4+2 ) ) THEN
               TEMP = Z( J4+2 ) / Z( J4-3 )
               Z( J4-1 ) = Z( J4 )*TEMP
               D = D*TEMP
            ELSE 
               Z( J4-1 ) = Z( J4+2 )*( Z( J4 ) / Z( J4-3 ) )
               D = Z( J4+2 )*( D / Z( J4-3 ) )
            END IF
            DMIN = MIN( DMIN, D )
            EMIN = MIN( EMIN, Z( J4-1 ) )
   20    CONTINUE
      END IF
*
*     Unroll last two steps. 
*
      DNM2 = D
      DMIN2 = DMIN
      J4 = 4*( N0-2 ) - PP
      J4P2 = J4 + 2*PP - 1
      Z( J4-2 ) = DNM2 + Z( J4P2 )
      IF( Z( J4-2 ).EQ.ZERO ) THEN
         Z( J4 ) = ZERO
         DNM1 = Z( J4P2+2 )
         DMIN = DNM1
         EMIN = ZERO
      ELSE IF( SAFMIN*Z( J4P2+2 ).LT.Z( J4-2 ) .AND.
     $         SAFMIN*Z( J4-2 ).LT.Z( J4P2+2 ) ) THEN
         TEMP = Z( J4P2+2 ) / Z( J4-2 )
         Z( J4 ) = Z( J4P2 )*TEMP
         DNM1 = DNM2*TEMP
      ELSE
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) )
      END IF
      DMIN = MIN( DMIN, DNM1 )
*
      DMIN1 = DMIN
      J4 = J4 + 4
      J4P2 = J4 + 2*PP - 1
      Z( J4-2 ) = DNM1 + Z( J4P2 )
      IF( Z( J4-2 ).EQ.ZERO ) THEN
         Z( J4 ) = ZERO
         DN = Z( J4P2+2 )
         DMIN = DN
         EMIN = ZERO
      ELSE IF( SAFMIN*Z( J4P2+2 ).LT.Z( J4-2 ) .AND.
     $         SAFMIN*Z( J4-2 ).LT.Z( J4P2+2 ) ) THEN
         TEMP = Z( J4P2+2 ) / Z( J4-2 )
         Z( J4 ) = Z( J4P2 )*TEMP
         DN = DNM1*TEMP
      ELSE
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) )
      END IF
      DMIN = MIN( DMIN, DN )
*
      Z( J4+2 ) = DN
      Z( 4*N0-PP ) = EMIN
      RETURN
*
*     End of DLASQ6
*
      END
      SUBROUTINE DLASR( SIDE, PIVOT, DIRECT, M, N, C, S, A, LDA )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIRECT, PIVOT, SIDE
      INTEGER            LDA, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( * ), S( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASR applies a sequence of plane rotations to a real matrix A,
*  from either the left or the right.
*  
*  When SIDE = 'L', the transformation takes the form
*  
*     A := P*A
*  
*  and when SIDE = 'R', the transformation takes the form
*  
*     A := A*P**T
*  
*  where P is an orthogonal matrix consisting of a sequence of z plane
*  rotations, with z = M when SIDE = 'L' and z = N when SIDE = 'R',
*  and P**T is the transpose of P.
*  
*  When DIRECT = 'F' (Forward sequence), then
*  
*     P = P(z-1) * ... * P(2) * P(1)
*  
*  and when DIRECT = 'B' (Backward sequence), then
*  
*     P = P(1) * P(2) * ... * P(z-1)
*  
*  where P(k) is a plane rotation matrix defined by the 2-by-2 rotation
*  
*     R(k) = (  c(k)  s(k) )
*          = ( -s(k)  c(k) ).
*  
*  When PIVOT = 'V' (Variable pivot), the rotation is performed
*  for the plane (k,k+1), i.e., P(k) has the form
*  
*     P(k) = (  1                                            )
*            (       ...                                     )
*            (              1                                )
*            (                   c(k)  s(k)                  )
*            (                  -s(k)  c(k)                  )
*            (                                1              )
*            (                                     ...       )
*            (                                            1  )
*  
*  where R(k) appears as a rank-2 modification to the identity matrix in
*  rows and columns k and k+1.
*  
*  When PIVOT = 'T' (Top pivot), the rotation is performed for the
*  plane (1,k+1), so P(k) has the form
*  
*     P(k) = (  c(k)                    s(k)                 )
*            (         1                                     )
*            (              ...                              )
*            (                     1                         )
*            ( -s(k)                    c(k)                 )
*            (                                 1             )
*            (                                      ...      )
*            (                                             1 )
*  
*  where R(k) appears in rows and columns 1 and k+1.
*  
*  Similarly, when PIVOT = 'B' (Bottom pivot), the rotation is
*  performed for the plane (k,z), giving P(k) the form
*  
*     P(k) = ( 1                                             )
*            (      ...                                      )
*            (             1                                 )
*            (                  c(k)                    s(k) )
*            (                         1                     )
*            (                              ...              )
*            (                                     1         )
*            (                 -s(k)                    c(k) )
*  
*  where R(k) appears in rows and columns k and z.  The rotations are
*  performed without ever forming P(k) explicitly.
*
*  Arguments
*  =========
*
*  SIDE    (input) CHARACTER*1
*          Specifies whether the plane rotation matrix P is applied to
*          A on the left or the right.
*          = 'L':  Left, compute A := P*A
*          = 'R':  Right, compute A:= A*P**T
*
*  PIVOT   (input) CHARACTER*1
*          Specifies the plane for which P(k) is a plane rotation
*          matrix.
*          = 'V':  Variable pivot, the plane (k,k+1)
*          = 'T':  Top pivot, the plane (1,k+1)
*          = 'B':  Bottom pivot, the plane (k,z)
*
*  DIRECT  (input) CHARACTER*1
*          Specifies whether P is a forward or backward sequence of
*          plane rotations.
*          = 'F':  Forward, P = P(z-1)*...*P(2)*P(1)
*          = 'B':  Backward, P = P(1)*P(2)*...*P(z-1)
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  If m <= 1, an immediate
*          return is effected.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  If n <= 1, an
*          immediate return is effected.
*
*  C       (input) DOUBLE PRECISION array, dimension
*                  (M-1) if SIDE = 'L'
*                  (N-1) if SIDE = 'R'
*          The cosines c(k) of the plane rotations.
*
*  S       (input) DOUBLE PRECISION array, dimension
*                  (M-1) if SIDE = 'L'
*                  (N-1) if SIDE = 'R'
*          The sines s(k) of the plane rotations.  The 2-by-2 plane
*          rotation part of the matrix P(k), R(k), has the form
*          R(k) = (  c(k)  s(k) )
*                 ( -s(k)  c(k) ).
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          The M-by-N matrix A.  On exit, A is overwritten by P*A if
*          SIDE = 'R' or by A*P**T if SIDE = 'L'.
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
      DOUBLE PRECISION   CTEMP, STEMP, TEMP
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
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
         CALL XERBLA( 'DLASR ', INFO )
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
*     End of DLASR
*
      END
      SUBROUTINE DLASSQ( N, X, INCX, SCALE, SUMSQ )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SCALE, SUMSQ
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLASSQ  returns the values  scl  and  smsq  such that
*
*     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
*
*  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
*  assumed to be non-negative and  scl  returns the value
*
*     scl = max( scale, abs( x( i ) ) ).
*
*  scale and sumsq must be supplied in SCALE and SUMSQ and
*  scl and smsq are overwritten on SCALE and SUMSQ respectively.
*
*  The routine makes only one pass through the vector x.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of elements to be used from the vector X.
*
*  X       (input) DOUBLE PRECISION array, dimension (N)
*          The vector for which a scaled sum of squares is computed.
*             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
*
*  INCX    (input) INTEGER
*          The increment between successive values of the vector X.
*          INCX > 0.
*
*  SCALE   (input/output) DOUBLE PRECISION
*          On entry, the value  scale  in the equation above.
*          On exit, SCALE is overwritten with  scl , the scaling factor
*          for the sum of squares.
*
*  SUMSQ   (input/output) DOUBLE PRECISION
*          On entry, the value  sumsq  in the equation above.
*          On exit, SUMSQ is overwritten with  smsq , the basic sum of
*          squares from which  scl  has been factored out.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            IX
      DOUBLE PRECISION   ABSXI
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            IF( X( IX ).NE.ZERO ) THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SUMSQ = SUMSQ + ( ABSXI / SCALE )**2
               END IF
            END IF
   10    CONTINUE
      END IF
      RETURN
*
*     End of DLASSQ
*
      END
      SUBROUTINE DLASV2( F, G, H, SSMIN, SSMAX, SNR, CSR, SNL, CSL )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   CSL, CSR, F, G, H, SNL, SNR, SSMAX, SSMIN
*     ..
*
*  Purpose
*  =======
*
*  DLASV2 computes the singular value decomposition of a 2-by-2
*  triangular matrix
*     [  F   G  ]
*     [  0   H  ].
*  On return, abs(SSMAX) is the larger singular value, abs(SSMIN) is the
*  smaller singular value, and (CSL,SNL) and (CSR,SNR) are the left and
*  right singular vectors for abs(SSMAX), giving the decomposition
*
*     [ CSL  SNL ] [  F   G  ] [ CSR -SNR ]  =  [ SSMAX   0   ]
*     [-SNL  CSL ] [  0   H  ] [ SNR  CSR ]     [  0    SSMIN ].
*
*  Arguments
*  =========
*
*  F       (input) DOUBLE PRECISION
*          The (1,1) element of the 2-by-2 matrix.
*
*  G       (input) DOUBLE PRECISION
*          The (1,2) element of the 2-by-2 matrix.
*
*  H       (input) DOUBLE PRECISION
*          The (2,2) element of the 2-by-2 matrix.
*
*  SSMIN   (output) DOUBLE PRECISION
*          abs(SSMIN) is the smaller singular value.
*
*  SSMAX   (output) DOUBLE PRECISION
*          abs(SSMAX) is the larger singular value.
*
*  SNL     (output) DOUBLE PRECISION
*  CSL     (output) DOUBLE PRECISION
*          The vector (CSL, SNL) is a unit left singular vector for the
*          singular value abs(SSMAX).
*
*  SNR     (output) DOUBLE PRECISION
*  CSR     (output) DOUBLE PRECISION
*          The vector (CSR, SNR) is a unit right singular vector for the
*          singular value abs(SSMAX).
*
*  Further Details
*  ===============
*
*  Any input parameter may be aliased with any output parameter.
*
*  Barring over/underflow and assuming a guard digit in subtraction, all
*  output quantities are correct to within a few units in the last
*  place (ulps).
*
*  In IEEE arithmetic, the code works correctly if one matrix element is
*  infinite.
*
*  Overflow will not occur unless the largest singular value itself
*  overflows or is within a few ulps of overflow. (On machines with
*  partial overflow, like the Cray, overflow may occur if the largest
*  singular value is within a factor of 2 of overflow.)
*
*  Underflow is harmless if underflow is gradual. Otherwise, results
*  may correspond to a matrix modified by perturbations of size near
*  the underflow threshold.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   FOUR
      PARAMETER          ( FOUR = 4.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            GASMAL, SWAP
      INTEGER            PMAX
      DOUBLE PRECISION   A, CLT, CRT, D, FA, FT, GA, GT, HA, HT, L, M,
     $                   MM, R, S, SLT, SRT, T, TEMP, TSIGN, TT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. Executable Statements ..
*
      FT = F
      FA = ABS( FT )
      HT = H
      HA = ABS( H )
*
*     PMAX points to the maximum absolute element of matrix
*       PMAX = 1 if F largest in absolute values
*       PMAX = 2 if G largest in absolute values
*       PMAX = 3 if H largest in absolute values
*
      PMAX = 1
      SWAP = ( HA.GT.FA )
      IF( SWAP ) THEN
         PMAX = 3
         TEMP = FT
         FT = HT
         HT = TEMP
         TEMP = FA
         FA = HA
         HA = TEMP
*
*        Now FA .ge. HA
*
      END IF
      GT = G
      GA = ABS( GT )
      IF( GA.EQ.ZERO ) THEN
*
*        Diagonal matrix
*
         SSMIN = HA
         SSMAX = FA
         CLT = ONE
         CRT = ONE
         SLT = ZERO
         SRT = ZERO
      ELSE
         GASMAL = .TRUE.
         IF( GA.GT.FA ) THEN
            PMAX = 2
            IF( ( FA / GA ).LT.DLAMCH( 'EPS' ) ) THEN
*
*              Case of very large GA
*
               GASMAL = .FALSE.
               SSMAX = GA
               IF( HA.GT.ONE ) THEN
                  SSMIN = FA / ( GA / HA )
               ELSE
                  SSMIN = ( FA / GA )*HA
               END IF
               CLT = ONE
               SLT = HT / GT
               SRT = ONE
               CRT = FT / GT
            END IF
         END IF
         IF( GASMAL ) THEN
*
*           Normal case
*
            D = FA - HA
            IF( D.EQ.FA ) THEN
*
*              Copes with infinite F or H
*
               L = ONE
            ELSE
               L = D / FA
            END IF
*
*           Note that 0 .le. L .le. 1
*
            M = GT / FT
*
*           Note that abs(M) .le. 1/macheps
*
            T = TWO - L
*
*           Note that T .ge. 1
*
            MM = M*M
            TT = T*T
            S = SQRT( TT+MM )
*
*           Note that 1 .le. S .le. 1 + 1/macheps
*
            IF( L.EQ.ZERO ) THEN
               R = ABS( M )
            ELSE
               R = SQRT( L*L+MM )
            END IF
*
*           Note that 0 .le. R .le. 1 + 1/macheps
*
            A = HALF*( S+R )
*
*           Note that 1 .le. A .le. 1 + abs(M)
*
            SSMIN = HA / A
            SSMAX = FA*A
            IF( MM.EQ.ZERO ) THEN
*
*              Note that M is very tiny
*
               IF( L.EQ.ZERO ) THEN
                  T = SIGN( TWO, FT )*SIGN( ONE, GT )
               ELSE
                  T = GT / SIGN( D, FT ) + M / T
               END IF
            ELSE
               T = ( M / ( S+T )+M / ( R+L ) )*( ONE+A )
            END IF
            L = SQRT( T*T+FOUR )
            CRT = TWO / L
            SRT = T / L
            CLT = ( CRT+SRT*M ) / A
            SLT = ( HT / FT )*SRT / A
         END IF
      END IF
      IF( SWAP ) THEN
         CSL = SRT
         SNL = CRT
         CSR = SLT
         SNR = CLT
      ELSE
         CSL = CLT
         SNL = SLT
         CSR = CRT
         SNR = SRT
      END IF
*
*     Correct signs of SSMAX and SSMIN
*
      IF( PMAX.EQ.1 )
     $   TSIGN = SIGN( ONE, CSR )*SIGN( ONE, CSL )*SIGN( ONE, F )
      IF( PMAX.EQ.2 )
     $   TSIGN = SIGN( ONE, SNR )*SIGN( ONE, CSL )*SIGN( ONE, G )
      IF( PMAX.EQ.3 )
     $   TSIGN = SIGN( ONE, SNR )*SIGN( ONE, SNL )*SIGN( ONE, H )
      SSMAX = SIGN( SSMAX, TSIGN )
      SSMIN = SIGN( SSMIN, TSIGN*SIGN( ONE, F )*SIGN( ONE, H ) )
      RETURN
*
*     End of DLASV2
*
      END
      SUBROUTINE DLASWP( N, A, LDA, K1, K2, IPIV, INCX )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCX, K1, K2, LDA, N
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASWP performs a series of row interchanges on the matrix A.
*  One row interchange is initiated for each of rows K1 through K2 of A.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
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
*  IPIV    (input) INTEGER array, dimension (K2*abs(INCX))
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
      DOUBLE PRECISION   TEMP
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
*     End of DLASWP
*
      END
      SUBROUTINE DLASY2( LTRANL, LTRANR, ISGN, N1, N2, TL, LDTL, TR,
     $                   LDTR, B, LDB, SCALE, X, LDX, XNORM, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            LTRANL, LTRANR
      INTEGER            INFO, ISGN, LDB, LDTL, LDTR, LDX, N1, N2
      DOUBLE PRECISION   SCALE, XNORM
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   B( LDB, * ), TL( LDTL, * ), TR( LDTR, * ),
     $                   X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASY2 solves for the N1 by N2 matrix X, 1 <= N1,N2 <= 2, in
*
*         op(TL)*X + ISGN*X*op(TR) = SCALE*B,
*
*  where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or
*  -1.  op(T) = T or T', where T' denotes the transpose of T.
*
*  Arguments
*  =========
*
*  LTRANL  (input) LOGICAL
*          On entry, LTRANL specifies the op(TL):
*             = .FALSE., op(TL) = TL,
*             = .TRUE., op(TL) = TL'.
*
*  LTRANR  (input) LOGICAL
*          On entry, LTRANR specifies the op(TR):
*            = .FALSE., op(TR) = TR,
*            = .TRUE., op(TR) = TR'.
*
*  ISGN    (input) INTEGER
*          On entry, ISGN specifies the sign of the equation
*          as described before. ISGN may only be 1 or -1.
*
*  N1      (input) INTEGER
*          On entry, N1 specifies the order of matrix TL.
*          N1 may only be 0, 1 or 2.
*
*  N2      (input) INTEGER
*          On entry, N2 specifies the order of matrix TR.
*          N2 may only be 0, 1 or 2.
*
*  TL      (input) DOUBLE PRECISION array, dimension (LDTL,2)
*          On entry, TL contains an N1 by N1 matrix.
*
*  LDTL    (input) INTEGER
*          The leading dimension of the matrix TL. LDTL >= max(1,N1).
*
*  TR      (input) DOUBLE PRECISION array, dimension (LDTR,2)
*          On entry, TR contains an N2 by N2 matrix.
*
*  LDTR    (input) INTEGER
*          The leading dimension of the matrix TR. LDTR >= max(1,N2).
*
*  B       (input) DOUBLE PRECISION array, dimension (LDB,2)
*          On entry, the N1 by N2 matrix B contains the right-hand
*          side of the equation.
*
*  LDB     (input) INTEGER
*          The leading dimension of the matrix B. LDB >= max(1,N1).
*
*  SCALE   (output) DOUBLE PRECISION
*          On exit, SCALE contains the scale factor. SCALE is chosen
*          less than or equal to 1 to prevent the solution overflowing.
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,2)
*          On exit, X contains the N1 by N2 solution.
*
*  LDX     (input) INTEGER
*          The leading dimension of the matrix X. LDX >= max(1,N1).
*
*  XNORM   (output) DOUBLE PRECISION
*          On exit, XNORM is the infinity-norm of the solution.
*
*  INFO    (output) INTEGER
*          On exit, INFO is set to
*             0: successful exit.
*             1: TL and TR have too close eigenvalues, so TL or
*                TR is perturbed to get a nonsingular equation.
*          NOTE: In the interests of speed, this routine does not
*                check the inputs for errors.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   TWO, HALF, EIGHT
      PARAMETER          ( TWO = 2.0D+0, HALF = 0.5D+0, EIGHT = 8.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            BSWAP, XSWAP
      INTEGER            I, IP, IPIV, IPSV, J, JP, JPSV, K
      DOUBLE PRECISION   BET, EPS, GAM, L21, SGN, SMIN, SMLNUM, TAU1,
     $                   TEMP, U11, U12, U22, XMAX
*     ..
*     .. Local Arrays ..
      LOGICAL            BSWPIV( 4 ), XSWPIV( 4 )
      INTEGER            JPIV( 4 ), LOCL21( 4 ), LOCU12( 4 ),
     $                   LOCU22( 4 )
      DOUBLE PRECISION   BTMP( 4 ), T16( 4, 4 ), TMP( 4 ), X2( 2 )
*     ..
*     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           IDAMAX, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Data statements ..
      DATA               LOCU12 / 3, 4, 1, 2 / , LOCL21 / 2, 1, 4, 3 / ,
     $                   LOCU22 / 4, 3, 2, 1 /
      DATA               XSWPIV / .FALSE., .FALSE., .TRUE., .TRUE. /
      DATA               BSWPIV / .FALSE., .TRUE., .FALSE., .TRUE. /
*     ..
*     .. Executable Statements ..
*
*     Do not check the input parameters for errors
*
      INFO = 0
*
*     Quick return if possible
*
      IF( N1.EQ.0 .OR. N2.EQ.0 )
     $   RETURN
*
*     Set constants to control overflow
*
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      SGN = ISGN
*
      K = N1 + N1 + N2 - 2
      GO TO ( 10, 20, 30, 50 )K
*
*     1 by 1: TL11*X + SGN*X*TR11 = B11
*
   10 CONTINUE
      TAU1 = TL( 1, 1 ) + SGN*TR( 1, 1 )
      BET = ABS( TAU1 )
      IF( BET.LE.SMLNUM ) THEN
         TAU1 = SMLNUM
         BET = SMLNUM
         INFO = 1
      END IF
*
      SCALE = ONE
      GAM = ABS( B( 1, 1 ) )
      IF( SMLNUM*GAM.GT.BET )
     $   SCALE = ONE / GAM
*
      X( 1, 1 ) = ( B( 1, 1 )*SCALE ) / TAU1
      XNORM = ABS( X( 1, 1 ) )
      RETURN
*
*     1 by 2:
*     TL11*[X11 X12] + ISGN*[X11 X12]*op[TR11 TR12]  = [B11 B12]
*                                       [TR21 TR22]
*
   20 CONTINUE
*
      SMIN = MAX( EPS*MAX( ABS( TL( 1, 1 ) ), ABS( TR( 1, 1 ) ),
     $       ABS( TR( 1, 2 ) ), ABS( TR( 2, 1 ) ), ABS( TR( 2, 2 ) ) ),
     $       SMLNUM )
      TMP( 1 ) = TL( 1, 1 ) + SGN*TR( 1, 1 )
      TMP( 4 ) = TL( 1, 1 ) + SGN*TR( 2, 2 )
      IF( LTRANR ) THEN
         TMP( 2 ) = SGN*TR( 2, 1 )
         TMP( 3 ) = SGN*TR( 1, 2 )
      ELSE
         TMP( 2 ) = SGN*TR( 1, 2 )
         TMP( 3 ) = SGN*TR( 2, 1 )
      END IF
      BTMP( 1 ) = B( 1, 1 )
      BTMP( 2 ) = B( 1, 2 )
      GO TO 40
*
*     2 by 1:
*          op[TL11 TL12]*[X11] + ISGN* [X11]*TR11  = [B11]
*            [TL21 TL22] [X21]         [X21]         [B21]
*
   30 CONTINUE
      SMIN = MAX( EPS*MAX( ABS( TR( 1, 1 ) ), ABS( TL( 1, 1 ) ),
     $       ABS( TL( 1, 2 ) ), ABS( TL( 2, 1 ) ), ABS( TL( 2, 2 ) ) ),
     $       SMLNUM )
      TMP( 1 ) = TL( 1, 1 ) + SGN*TR( 1, 1 )
      TMP( 4 ) = TL( 2, 2 ) + SGN*TR( 1, 1 )
      IF( LTRANL ) THEN
         TMP( 2 ) = TL( 1, 2 )
         TMP( 3 ) = TL( 2, 1 )
      ELSE
         TMP( 2 ) = TL( 2, 1 )
         TMP( 3 ) = TL( 1, 2 )
      END IF
      BTMP( 1 ) = B( 1, 1 )
      BTMP( 2 ) = B( 2, 1 )
   40 CONTINUE
*
*     Solve 2 by 2 system using complete pivoting.
*     Set pivots less than SMIN to SMIN.
*
      IPIV = IDAMAX( 4, TMP, 1 )
      U11 = TMP( IPIV )
      IF( ABS( U11 ).LE.SMIN ) THEN
         INFO = 1
         U11 = SMIN
      END IF
      U12 = TMP( LOCU12( IPIV ) )
      L21 = TMP( LOCL21( IPIV ) ) / U11
      U22 = TMP( LOCU22( IPIV ) ) - U12*L21
      XSWAP = XSWPIV( IPIV )
      BSWAP = BSWPIV( IPIV )
      IF( ABS( U22 ).LE.SMIN ) THEN
         INFO = 1
         U22 = SMIN
      END IF
      IF( BSWAP ) THEN
         TEMP = BTMP( 2 )
         BTMP( 2 ) = BTMP( 1 ) - L21*TEMP
         BTMP( 1 ) = TEMP
      ELSE
         BTMP( 2 ) = BTMP( 2 ) - L21*BTMP( 1 )
      END IF
      SCALE = ONE
      IF( ( TWO*SMLNUM )*ABS( BTMP( 2 ) ).GT.ABS( U22 ) .OR.
     $    ( TWO*SMLNUM )*ABS( BTMP( 1 ) ).GT.ABS( U11 ) ) THEN
         SCALE = HALF / MAX( ABS( BTMP( 1 ) ), ABS( BTMP( 2 ) ) )
         BTMP( 1 ) = BTMP( 1 )*SCALE
         BTMP( 2 ) = BTMP( 2 )*SCALE
      END IF
      X2( 2 ) = BTMP( 2 ) / U22
      X2( 1 ) = BTMP( 1 ) / U11 - ( U12 / U11 )*X2( 2 )
      IF( XSWAP ) THEN
         TEMP = X2( 2 )
         X2( 2 ) = X2( 1 )
         X2( 1 ) = TEMP
      END IF
      X( 1, 1 ) = X2( 1 )
      IF( N1.EQ.1 ) THEN
         X( 1, 2 ) = X2( 2 )
         XNORM = ABS( X( 1, 1 ) ) + ABS( X( 1, 2 ) )
      ELSE
         X( 2, 1 ) = X2( 2 )
         XNORM = MAX( ABS( X( 1, 1 ) ), ABS( X( 2, 1 ) ) )
      END IF
      RETURN
*
*     2 by 2:
*     op[TL11 TL12]*[X11 X12] +ISGN* [X11 X12]*op[TR11 TR12] = [B11 B12]
*       [TL21 TL22] [X21 X22]        [X21 X22]   [TR21 TR22]   [B21 B22]
*
*     Solve equivalent 4 by 4 system using complete pivoting.
*     Set pivots less than SMIN to SMIN.
*
   50 CONTINUE
      SMIN = MAX( ABS( TR( 1, 1 ) ), ABS( TR( 1, 2 ) ),
     $       ABS( TR( 2, 1 ) ), ABS( TR( 2, 2 ) ) )
      SMIN = MAX( SMIN, ABS( TL( 1, 1 ) ), ABS( TL( 1, 2 ) ),
     $       ABS( TL( 2, 1 ) ), ABS( TL( 2, 2 ) ) )
      SMIN = MAX( EPS*SMIN, SMLNUM )
      BTMP( 1 ) = ZERO
      CALL DCOPY( 16, BTMP, 0, T16, 1 )
      T16( 1, 1 ) = TL( 1, 1 ) + SGN*TR( 1, 1 )
      T16( 2, 2 ) = TL( 2, 2 ) + SGN*TR( 1, 1 )
      T16( 3, 3 ) = TL( 1, 1 ) + SGN*TR( 2, 2 )
      T16( 4, 4 ) = TL( 2, 2 ) + SGN*TR( 2, 2 )
      IF( LTRANL ) THEN
         T16( 1, 2 ) = TL( 2, 1 )
         T16( 2, 1 ) = TL( 1, 2 )
         T16( 3, 4 ) = TL( 2, 1 )
         T16( 4, 3 ) = TL( 1, 2 )
      ELSE
         T16( 1, 2 ) = TL( 1, 2 )
         T16( 2, 1 ) = TL( 2, 1 )
         T16( 3, 4 ) = TL( 1, 2 )
         T16( 4, 3 ) = TL( 2, 1 )
      END IF
      IF( LTRANR ) THEN
         T16( 1, 3 ) = SGN*TR( 1, 2 )
         T16( 2, 4 ) = SGN*TR( 1, 2 )
         T16( 3, 1 ) = SGN*TR( 2, 1 )
         T16( 4, 2 ) = SGN*TR( 2, 1 )
      ELSE
         T16( 1, 3 ) = SGN*TR( 2, 1 )
         T16( 2, 4 ) = SGN*TR( 2, 1 )
         T16( 3, 1 ) = SGN*TR( 1, 2 )
         T16( 4, 2 ) = SGN*TR( 1, 2 )
      END IF
      BTMP( 1 ) = B( 1, 1 )
      BTMP( 2 ) = B( 2, 1 )
      BTMP( 3 ) = B( 1, 2 )
      BTMP( 4 ) = B( 2, 2 )
*
*     Perform elimination
*
      DO 100 I = 1, 3
         XMAX = ZERO
         DO 70 IP = I, 4
            DO 60 JP = I, 4
               IF( ABS( T16( IP, JP ) ).GE.XMAX ) THEN
                  XMAX = ABS( T16( IP, JP ) )
                  IPSV = IP
                  JPSV = JP
               END IF
   60       CONTINUE
   70    CONTINUE
         IF( IPSV.NE.I ) THEN
            CALL DSWAP( 4, T16( IPSV, 1 ), 4, T16( I, 1 ), 4 )
            TEMP = BTMP( I )
            BTMP( I ) = BTMP( IPSV )
            BTMP( IPSV ) = TEMP
         END IF
         IF( JPSV.NE.I )
     $      CALL DSWAP( 4, T16( 1, JPSV ), 1, T16( 1, I ), 1 )
         JPIV( I ) = JPSV
         IF( ABS( T16( I, I ) ).LT.SMIN ) THEN
            INFO = 1
            T16( I, I ) = SMIN
         END IF
         DO 90 J = I + 1, 4
            T16( J, I ) = T16( J, I ) / T16( I, I )
            BTMP( J ) = BTMP( J ) - T16( J, I )*BTMP( I )
            DO 80 K = I + 1, 4
               T16( J, K ) = T16( J, K ) - T16( J, I )*T16( I, K )
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
      IF( ABS( T16( 4, 4 ) ).LT.SMIN )
     $   T16( 4, 4 ) = SMIN
      SCALE = ONE
      IF( ( EIGHT*SMLNUM )*ABS( BTMP( 1 ) ).GT.ABS( T16( 1, 1 ) ) .OR.
     $    ( EIGHT*SMLNUM )*ABS( BTMP( 2 ) ).GT.ABS( T16( 2, 2 ) ) .OR.
     $    ( EIGHT*SMLNUM )*ABS( BTMP( 3 ) ).GT.ABS( T16( 3, 3 ) ) .OR.
     $    ( EIGHT*SMLNUM )*ABS( BTMP( 4 ) ).GT.ABS( T16( 4, 4 ) ) ) THEN
         SCALE = ( ONE / EIGHT ) / MAX( ABS( BTMP( 1 ) ),
     $           ABS( BTMP( 2 ) ), ABS( BTMP( 3 ) ), ABS( BTMP( 4 ) ) )
         BTMP( 1 ) = BTMP( 1 )*SCALE
         BTMP( 2 ) = BTMP( 2 )*SCALE
         BTMP( 3 ) = BTMP( 3 )*SCALE
         BTMP( 4 ) = BTMP( 4 )*SCALE
      END IF
      DO 120 I = 1, 4
         K = 5 - I
         TEMP = ONE / T16( K, K )
         TMP( K ) = BTMP( K )*TEMP
         DO 110 J = K + 1, 4
            TMP( K ) = TMP( K ) - ( TEMP*T16( K, J ) )*TMP( J )
  110    CONTINUE
  120 CONTINUE
      DO 130 I = 1, 3
         IF( JPIV( 4-I ).NE.4-I ) THEN
            TEMP = TMP( 4-I )
            TMP( 4-I ) = TMP( JPIV( 4-I ) )
            TMP( JPIV( 4-I ) ) = TEMP
         END IF
  130 CONTINUE
      X( 1, 1 ) = TMP( 1 )
      X( 2, 1 ) = TMP( 2 )
      X( 1, 2 ) = TMP( 3 )
      X( 2, 2 ) = TMP( 4 )
      XNORM = MAX( ABS( TMP( 1 ) )+ABS( TMP( 3 ) ),
     $        ABS( TMP( 2 ) )+ABS( TMP( 4 ) ) )
      RETURN
*
*     End of DLASY2
*
      END
      SUBROUTINE DLASYF( UPLO, N, NB, KB, A, LDA, IPIV, W, LDW, INFO )
*
*  -- LAPACK routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, KB, LDA, LDW, N, NB
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), W( LDW, * )
*     ..
*
*  Purpose
*  =======
*
*  DLASYF computes a partial factorization of a real symmetric matrix A
*  using the Bunch-Kaufman diagonal pivoting method. The partial
*  factorization has the form:
*
*  A  =  ( I  U12 ) ( A11  0  ) (  I    0   )  if UPLO = 'U', or:
*        ( 0  U22 ) (  0   D  ) ( U12' U22' )
*
*  A  =  ( L11  0 ) (  D   0  ) ( L11' L21' )  if UPLO = 'L'
*        ( L21  I ) (  0  A22 ) (  0    I   )
*
*  where the order of D is at most NB. The actual order is returned in
*  the argument KB, and is either NB or NB-1, or N if N <= NB.
*
*  DLASYF is an auxiliary routine called by DSYTRF. It uses blocked code
*  (calling Level 3 BLAS) to update the submatrix A11 (if UPLO = 'U') or
*  A22 (if UPLO = 'L').
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is stored:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  NB      (input) INTEGER
*          The maximum number of columns of the matrix A that should be
*          factored.  NB should be at least 2 to allow for 2-by-2 pivot
*          blocks.
*
*  KB      (output) INTEGER
*          The number of columns of A that were actually factored.
*          KB is either NB-1 or NB, or N if N <= NB.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
*          n-by-n upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading n-by-n lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*          On exit, A contains details of the partial factorization.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  IPIV    (output) INTEGER array, dimension (N)
*          Details of the interchanges and the block structure of D.
*          If UPLO = 'U', only the last KB elements of IPIV are set;
*          if UPLO = 'L', only the first KB elements are set.
*
*          If IPIV(k) > 0, then rows and columns k and IPIV(k) were
*          interchanged and D(k,k) is a 1-by-1 diagonal block.
*          If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and
*          columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
*          is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =
*          IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were
*          interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
*
*  W       (workspace) DOUBLE PRECISION array, dimension (LDW,NB)
*
*  LDW     (input) INTEGER
*          The leading dimension of the array W.  LDW >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          > 0: if INFO = k, D(k,k) is exactly zero.  The factorization
*               has been completed, but the block diagonal matrix D is
*               exactly singular.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   EIGHT, SEVTEN
      PARAMETER          ( EIGHT = 8.0D+0, SEVTEN = 17.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            IMAX, J, JB, JJ, JMAX, JP, K, KK, KKW, KP,
     $                   KSTEP, KW
      DOUBLE PRECISION   ABSAKK, ALPHA, COLMAX, D11, D21, D22, R1,
     $                   ROWMAX, T
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      EXTERNAL           LSAME, IDAMAX
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DGEMV, DSCAL, DSWAP
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
*     Initialize ALPHA for use in choosing pivot block size.
*
      ALPHA = ( ONE+SQRT( SEVTEN ) ) / EIGHT
*
      IF( LSAME( UPLO, 'U' ) ) THEN
*
*        Factorize the trailing columns of A using the upper triangle
*        of A and working backwards, and compute the matrix W = U12*D
*        for use in updating A11
*
*        K is the main loop index, decreasing from N in steps of 1 or 2
*
*        KW is the column of W which corresponds to column K of A
*
         K = N
   10    CONTINUE
         KW = NB + K - N
*
*        Exit from loop
*
         IF( ( K.LE.N-NB+1 .AND. NB.LT.N ) .OR. K.LT.1 )
     $      GO TO 30
*
*        Copy column K of A to column KW of W and update it
*
         CALL DCOPY( K, A( 1, K ), 1, W( 1, KW ), 1 )
         IF( K.LT.N )
     $      CALL DGEMV( 'No transpose', K, N-K, -ONE, A( 1, K+1 ), LDA,
     $                  W( K, KW+1 ), LDW, ONE, W( 1, KW ), 1 )
*
         KSTEP = 1
*
*        Determine rows and columns to be interchanged and whether
*        a 1-by-1 or 2-by-2 pivot block will be used
*
         ABSAKK = ABS( W( K, KW ) )
*
*        IMAX is the row-index of the largest off-diagonal element in
*        column K, and COLMAX is its absolute value
*
         IF( K.GT.1 ) THEN
            IMAX = IDAMAX( K-1, W( 1, KW ), 1 )
            COLMAX = ABS( W( IMAX, KW ) )
         ELSE
            COLMAX = ZERO
         END IF
*
         IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN
*
*           Column K is zero: set INFO and continue
*
            IF( INFO.EQ.0 )
     $         INFO = K
            KP = K
         ELSE
            IF( ABSAKK.GE.ALPHA*COLMAX ) THEN
*
*              no interchange, use 1-by-1 pivot block
*
               KP = K
            ELSE
*
*              Copy column IMAX to column KW-1 of W and update it
*
               CALL DCOPY( IMAX, A( 1, IMAX ), 1, W( 1, KW-1 ), 1 )
               CALL DCOPY( K-IMAX, A( IMAX, IMAX+1 ), LDA,
     $                     W( IMAX+1, KW-1 ), 1 )
               IF( K.LT.N )
     $            CALL DGEMV( 'No transpose', K, N-K, -ONE, A( 1, K+1 ),
     $                        LDA, W( IMAX, KW+1 ), LDW, ONE,
     $                        W( 1, KW-1 ), 1 )
*
*              JMAX is the column-index of the largest off-diagonal
*              element in row IMAX, and ROWMAX is its absolute value
*
               JMAX = IMAX + IDAMAX( K-IMAX, W( IMAX+1, KW-1 ), 1 )
               ROWMAX = ABS( W( JMAX, KW-1 ) )
               IF( IMAX.GT.1 ) THEN
                  JMAX = IDAMAX( IMAX-1, W( 1, KW-1 ), 1 )
                  ROWMAX = MAX( ROWMAX, ABS( W( JMAX, KW-1 ) ) )
               END IF
*
               IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN
*
*                 no interchange, use 1-by-1 pivot block
*
                  KP = K
               ELSE IF( ABS( W( IMAX, KW-1 ) ).GE.ALPHA*ROWMAX ) THEN
*
*                 interchange rows and columns K and IMAX, use 1-by-1
*                 pivot block
*
                  KP = IMAX
*
*                 copy column KW-1 of W to column KW
*
                  CALL DCOPY( K, W( 1, KW-1 ), 1, W( 1, KW ), 1 )
               ELSE
*
*                 interchange rows and columns K-1 and IMAX, use 2-by-2
*                 pivot block
*
                  KP = IMAX
                  KSTEP = 2
               END IF
            END IF
*
            KK = K - KSTEP + 1
            KKW = NB + KK - N
*
*           Updated column KP is already stored in column KKW of W
*
            IF( KP.NE.KK ) THEN
*
*              Copy non-updated column KK to column KP
*
               A( KP, K ) = A( KK, K )
               CALL DCOPY( K-1-KP, A( KP+1, KK ), 1, A( KP, KP+1 ),
     $                     LDA )
               CALL DCOPY( KP, A( 1, KK ), 1, A( 1, KP ), 1 )
*
*              Interchange rows KK and KP in last KK columns of A and W
*
               CALL DSWAP( N-KK+1, A( KK, KK ), LDA, A( KP, KK ), LDA )
               CALL DSWAP( N-KK+1, W( KK, KKW ), LDW, W( KP, KKW ),
     $                     LDW )
            END IF
*
            IF( KSTEP.EQ.1 ) THEN
*
*              1-by-1 pivot block D(k): column KW of W now holds
*
*              W(k) = U(k)*D(k)
*
*              where U(k) is the k-th column of U
*
*              Store U(k) in column k of A
*
               CALL DCOPY( K, W( 1, KW ), 1, A( 1, K ), 1 )
               R1 = ONE / A( K, K )
               CALL DSCAL( K-1, R1, A( 1, K ), 1 )
            ELSE
*
*              2-by-2 pivot block D(k): columns KW and KW-1 of W now
*              hold
*
*              ( W(k-1) W(k) ) = ( U(k-1) U(k) )*D(k)
*
*              where U(k) and U(k-1) are the k-th and (k-1)-th columns
*              of U
*
               IF( K.GT.2 ) THEN
*
*                 Store U(k) and U(k-1) in columns k and k-1 of A
*
                  D21 = W( K-1, KW )
                  D11 = W( K, KW ) / D21
                  D22 = W( K-1, KW-1 ) / D21
                  T = ONE / ( D11*D22-ONE )
                  D21 = T / D21
                  DO 20 J = 1, K - 2
                     A( J, K-1 ) = D21*( D11*W( J, KW-1 )-W( J, KW ) )
                     A( J, K ) = D21*( D22*W( J, KW )-W( J, KW-1 ) )
   20             CONTINUE
               END IF
*
*              Copy D(k) to A
*
               A( K-1, K-1 ) = W( K-1, KW-1 )
               A( K-1, K ) = W( K-1, KW )
               A( K, K ) = W( K, KW )
            END IF
         END IF
*
*        Store details of the interchanges in IPIV
*
         IF( KSTEP.EQ.1 ) THEN
            IPIV( K ) = KP
         ELSE
            IPIV( K ) = -KP
            IPIV( K-1 ) = -KP
         END IF
*
*        Decrease K and return to the start of the main loop
*
         K = K - KSTEP
         GO TO 10
*
   30    CONTINUE
*
*        Update the upper triangle of A11 (= A(1:k,1:k)) as
*
*        A11 := A11 - U12*D*U12' = A11 - U12*W'
*
*        computing blocks of NB columns at a time
*
         DO 50 J = ( ( K-1 ) / NB )*NB + 1, 1, -NB
            JB = MIN( NB, K-J+1 )
*
*           Update the upper triangle of the diagonal block
*
            DO 40 JJ = J, J + JB - 1
               CALL DGEMV( 'No transpose', JJ-J+1, N-K, -ONE,
     $                     A( J, K+1 ), LDA, W( JJ, KW+1 ), LDW, ONE,
     $                     A( J, JJ ), 1 )
   40       CONTINUE
*
*           Update the rectangular superdiagonal block
*
            CALL DGEMM( 'No transpose', 'Transpose', J-1, JB, N-K, -ONE,
     $                  A( 1, K+1 ), LDA, W( J, KW+1 ), LDW, ONE,
     $                  A( 1, J ), LDA )
   50    CONTINUE
*
*        Put U12 in standard form by partially undoing the interchanges
*        in columns k+1:n
*
         J = K + 1
   60    CONTINUE
         JJ = J
         JP = IPIV( J )
         IF( JP.LT.0 ) THEN
            JP = -JP
            J = J + 1
         END IF
         J = J + 1
         IF( JP.NE.JJ .AND. J.LE.N )
     $      CALL DSWAP( N-J+1, A( JP, J ), LDA, A( JJ, J ), LDA )
         IF( J.LE.N )
     $      GO TO 60
*
*        Set KB to the number of columns factorized
*
         KB = N - K
*
      ELSE
*
*        Factorize the leading columns of A using the lower triangle
*        of A and working forwards, and compute the matrix W = L21*D
*        for use in updating A22
*
*        K is the main loop index, increasing from 1 in steps of 1 or 2
*
         K = 1
   70    CONTINUE
*
*        Exit from loop
*
         IF( ( K.GE.NB .AND. NB.LT.N ) .OR. K.GT.N )
     $      GO TO 90
*
*        Copy column K of A to column K of W and update it
*
         CALL DCOPY( N-K+1, A( K, K ), 1, W( K, K ), 1 )
         CALL DGEMV( 'No transpose', N-K+1, K-1, -ONE, A( K, 1 ), LDA,
     $               W( K, 1 ), LDW, ONE, W( K, K ), 1 )
*
         KSTEP = 1
*
*        Determine rows and columns to be interchanged and whether
*        a 1-by-1 or 2-by-2 pivot block will be used
*
         ABSAKK = ABS( W( K, K ) )
*
*        IMAX is the row-index of the largest off-diagonal element in
*        column K, and COLMAX is its absolute value
*
         IF( K.LT.N ) THEN
            IMAX = K + IDAMAX( N-K, W( K+1, K ), 1 )
            COLMAX = ABS( W( IMAX, K ) )
         ELSE
            COLMAX = ZERO
         END IF
*
         IF( MAX( ABSAKK, COLMAX ).EQ.ZERO ) THEN
*
*           Column K is zero: set INFO and continue
*
            IF( INFO.EQ.0 )
     $         INFO = K
            KP = K
         ELSE
            IF( ABSAKK.GE.ALPHA*COLMAX ) THEN
*
*              no interchange, use 1-by-1 pivot block
*
               KP = K
            ELSE
*
*              Copy column IMAX to column K+1 of W and update it
*
               CALL DCOPY( IMAX-K, A( IMAX, K ), LDA, W( K, K+1 ), 1 )
               CALL DCOPY( N-IMAX+1, A( IMAX, IMAX ), 1, W( IMAX, K+1 ),
     $                     1 )
               CALL DGEMV( 'No transpose', N-K+1, K-1, -ONE, A( K, 1 ),
     $                     LDA, W( IMAX, 1 ), LDW, ONE, W( K, K+1 ), 1 )
*
*              JMAX is the column-index of the largest off-diagonal
*              element in row IMAX, and ROWMAX is its absolute value
*
               JMAX = K - 1 + IDAMAX( IMAX-K, W( K, K+1 ), 1 )
               ROWMAX = ABS( W( JMAX, K+1 ) )
               IF( IMAX.LT.N ) THEN
                  JMAX = IMAX + IDAMAX( N-IMAX, W( IMAX+1, K+1 ), 1 )
                  ROWMAX = MAX( ROWMAX, ABS( W( JMAX, K+1 ) ) )
               END IF
*
               IF( ABSAKK.GE.ALPHA*COLMAX*( COLMAX / ROWMAX ) ) THEN
*
*                 no interchange, use 1-by-1 pivot block
*
                  KP = K
               ELSE IF( ABS( W( IMAX, K+1 ) ).GE.ALPHA*ROWMAX ) THEN
*
*                 interchange rows and columns K and IMAX, use 1-by-1
*                 pivot block
*
                  KP = IMAX
*
*                 copy column K+1 of W to column K
*
                  CALL DCOPY( N-K+1, W( K, K+1 ), 1, W( K, K ), 1 )
               ELSE
*
*                 interchange rows and columns K+1 and IMAX, use 2-by-2
*                 pivot block
*
                  KP = IMAX
                  KSTEP = 2
               END IF
            END IF
*
            KK = K + KSTEP - 1
*
*           Updated column KP is already stored in column KK of W
*
            IF( KP.NE.KK ) THEN
*
*              Copy non-updated column KK to column KP
*
               A( KP, K ) = A( KK, K )
               CALL DCOPY( KP-K-1, A( K+1, KK ), 1, A( KP, K+1 ), LDA )
               CALL DCOPY( N-KP+1, A( KP, KK ), 1, A( KP, KP ), 1 )
*
*              Interchange rows KK and KP in first KK columns of A and W
*
               CALL DSWAP( KK, A( KK, 1 ), LDA, A( KP, 1 ), LDA )
               CALL DSWAP( KK, W( KK, 1 ), LDW, W( KP, 1 ), LDW )
            END IF
*
            IF( KSTEP.EQ.1 ) THEN
*
*              1-by-1 pivot block D(k): column k of W now holds
*
*              W(k) = L(k)*D(k)
*
*              where L(k) is the k-th column of L
*
*              Store L(k) in column k of A
*
               CALL DCOPY( N-K+1, W( K, K ), 1, A( K, K ), 1 )
               IF( K.LT.N ) THEN
                  R1 = ONE / A( K, K )
                  CALL DSCAL( N-K, R1, A( K+1, K ), 1 )
               END IF
            ELSE
*
*              2-by-2 pivot block D(k): columns k and k+1 of W now hold
*
*              ( W(k) W(k+1) ) = ( L(k) L(k+1) )*D(k)
*
*              where L(k) and L(k+1) are the k-th and (k+1)-th columns
*              of L
*
               IF( K.LT.N-1 ) THEN
*
*                 Store L(k) and L(k+1) in columns k and k+1 of A
*
                  D21 = W( K+1, K )
                  D11 = W( K+1, K+1 ) / D21
                  D22 = W( K, K ) / D21
                  T = ONE / ( D11*D22-ONE )
                  D21 = T / D21
                  DO 80 J = K + 2, N
                     A( J, K ) = D21*( D11*W( J, K )-W( J, K+1 ) )
                     A( J, K+1 ) = D21*( D22*W( J, K+1 )-W( J, K ) )
   80             CONTINUE
               END IF
*
*              Copy D(k) to A
*
               A( K, K ) = W( K, K )
               A( K+1, K ) = W( K+1, K )
               A( K+1, K+1 ) = W( K+1, K+1 )
            END IF
         END IF
*
*        Store details of the interchanges in IPIV
*
         IF( KSTEP.EQ.1 ) THEN
            IPIV( K ) = KP
         ELSE
            IPIV( K ) = -KP
            IPIV( K+1 ) = -KP
         END IF
*
*        Increase K and return to the start of the main loop
*
         K = K + KSTEP
         GO TO 70
*
   90    CONTINUE
*
*        Update the lower triangle of A22 (= A(k:n,k:n)) as
*
*        A22 := A22 - L21*D*L21' = A22 - L21*W'
*
*        computing blocks of NB columns at a time
*
         DO 110 J = K, N, NB
            JB = MIN( NB, N-J+1 )
*
*           Update the lower triangle of the diagonal block
*
            DO 100 JJ = J, J + JB - 1
               CALL DGEMV( 'No transpose', J+JB-JJ, K-1, -ONE,
     $                     A( JJ, 1 ), LDA, W( JJ, 1 ), LDW, ONE,
     $                     A( JJ, JJ ), 1 )
  100       CONTINUE
*
*           Update the rectangular subdiagonal block
*
            IF( J+JB.LE.N )
     $         CALL DGEMM( 'No transpose', 'Transpose', N-J-JB+1, JB,
     $                     K-1, -ONE, A( J+JB, 1 ), LDA, W( J, 1 ), LDW,
     $                     ONE, A( J+JB, J ), LDA )
  110    CONTINUE
*
*        Put L21 in standard form by partially undoing the interchanges
*        in columns 1:k-1
*
         J = K - 1
  120    CONTINUE
         JJ = J
         JP = IPIV( J )
         IF( JP.LT.0 ) THEN
            JP = -JP
            J = J - 1
         END IF
         J = J - 1
         IF( JP.NE.JJ .AND. J.GE.1 )
     $      CALL DSWAP( J, A( JP, 1 ), LDA, A( JJ, 1 ), LDA )
         IF( J.GE.1 )
     $      GO TO 120
*
*        Set KB to the number of columns factorized
*
         KB = K - 1
*
      END IF
      RETURN
*
*     End of DLASYF
*
      END
      SUBROUTINE DLATBS( UPLO, TRANS, DIAG, NORMIN, N, KD, AB, LDAB, X,
     $                   SCALE, CNORM, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, NORMIN, TRANS, UPLO
      INTEGER            INFO, KD, LDAB, N
      DOUBLE PRECISION   SCALE
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), CNORM( * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLATBS solves one of the triangular systems
*
*     A *x = s*b  or  A'*x = s*b
*
*  with scaling to prevent overflow, where A is an upper or lower
*  triangular band matrix.  Here A' denotes the transpose of A, x and b
*  are n-element vectors, and s is a scaling factor, usually less than
*  or equal to 1, chosen so that the components of x will be less than
*  the overflow threshold.  If the unscaled problem will not cause
*  overflow, the Level 2 BLAS routine DTBSV is called.  If the matrix A
*  is singular (A(j,j) = 0 for some j), then s is set to 0 and a
*  non-trivial solution to A*x = 0 is returned.
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
*          = 'N':  Solve A * x = s*b  (No transpose)
*          = 'T':  Solve A'* x = s*b  (Transpose)
*          = 'C':  Solve A'* x = s*b  (Conjugate transpose = Transpose)
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
*  KD      (input) INTEGER
*          The number of subdiagonals or superdiagonals in the
*          triangular matrix A.  KD >= 0.
*
*  AB      (input) DOUBLE PRECISION array, dimension (LDAB,N)
*          The upper or lower triangular band matrix A, stored in the
*          first KD+1 rows of the array. The j-th column of A is stored
*          in the j-th column of the array AB as follows:
*          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD+1.
*
*  X       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the right hand side b of the triangular system.
*          On exit, X is overwritten by the solution vector x.
*
*  SCALE   (output) DOUBLE PRECISION
*          The scaling factor s for the triangular system
*             A * x = s*b  or  A'* x = s*b.
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
*  A rough bound on x is computed; if that is less than overflow, DTBSV
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
*  Since |x(j)| <= M(j), we use the Level 2 BLAS routine DTBSV if the
*  reciprocal of the largest M(j), j=1,..,n, is larger than
*  max(underflow, 1/overflow).
*
*  The bound on x(j) is also used to determine when a step in the
*  columnwise method can be performed without fear of overflow.  If
*  the computed bound is greater than a large constant, x is scaled to
*  prevent overflow, but if the bound overflows, x is set to 0, x(j) to
*  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.
*
*  Similarly, a row-wise scheme is used to solve A'*x = b.  The basic
*  algorithm for A upper triangular is
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
*  and we can safely call DTBSV if 1/M(n) and 1/G(n) are both greater
*  than max(underflow, 1/overflow).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN, NOUNIT, UPPER
      INTEGER            I, IMAX, J, JFIRST, JINC, JLAST, JLEN, MAIND
      DOUBLE PRECISION   BIGNUM, GROW, REC, SMLNUM, SUMJ, TJJ, TJJS,
     $                   TMAX, TSCAL, USCAL, XBND, XJ, XMAX
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM, DDOT, DLAMCH
      EXTERNAL           LSAME, IDAMAX, DASUM, DDOT, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DSCAL, DTBSV, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
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
      ELSE IF( KD.LT.0 ) THEN
         INFO = -6
      ELSE IF( LDAB.LT.KD+1 ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLATBS', -INFO )
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
      SMLNUM = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' )
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
               JLEN = MIN( KD, J-1 )
               CNORM( J ) = DASUM( JLEN, AB( KD+1-JLEN, J ), 1 )
   10       CONTINUE
         ELSE
*
*           A is lower triangular.
*
            DO 20 J = 1, N
               JLEN = MIN( KD, N-J )
               IF( JLEN.GT.0 ) THEN
                  CNORM( J ) = DASUM( JLEN, AB( 2, J ), 1 )
               ELSE
                  CNORM( J ) = ZERO
               END IF
   20       CONTINUE
         END IF
      END IF
*
*     Scale the column norms by TSCAL if the maximum element in CNORM is
*     greater than BIGNUM.
*
      IMAX = IDAMAX( N, CNORM, 1 )
      TMAX = CNORM( IMAX )
      IF( TMAX.LE.BIGNUM ) THEN
         TSCAL = ONE
      ELSE
         TSCAL = ONE / ( SMLNUM*TMAX )
         CALL DSCAL( N, TSCAL, CNORM, 1 )
      END IF
*
*     Compute a bound on the computed solution vector to see if the
*     Level 2 BLAS routine DTBSV can be used.
*
      J = IDAMAX( N, X, 1 )
      XMAX = ABS( X( J ) )
      XBND = XMAX
      IF( NOTRAN ) THEN
*
*        Compute the growth in A * x = b.
*
         IF( UPPER ) THEN
            JFIRST = N
            JLAST = 1
            JINC = -1
            MAIND = KD + 1
         ELSE
            JFIRST = 1
            JLAST = N
            JINC = 1
            MAIND = 1
         END IF
*
         IF( TSCAL.NE.ONE ) THEN
            GROW = ZERO
            GO TO 50
         END IF
*
         IF( NOUNIT ) THEN
*
*           A is non-unit triangular.
*
*           Compute GROW = 1/G(j) and XBND = 1/M(j).
*           Initially, G(0) = max{x(i), i=1,...,n}.
*
            GROW = ONE / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 30 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 50
*
*              M(j) = G(j-1) / abs(A(j,j))
*
               TJJ = ABS( AB( MAIND, J ) )
               XBND = MIN( XBND, MIN( ONE, TJJ )*GROW )
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
   30       CONTINUE
            GROW = XBND
         ELSE
*
*           A is unit triangular.
*
*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
*
            GROW = MIN( ONE, ONE / MAX( XBND, SMLNUM ) )
            DO 40 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 50
*
*              G(j) = G(j-1)*( 1 + CNORM(j) )
*
               GROW = GROW*( ONE / ( ONE+CNORM( J ) ) )
   40       CONTINUE
         END IF
   50    CONTINUE
*
      ELSE
*
*        Compute the growth in A' * x = b.
*
         IF( UPPER ) THEN
            JFIRST = 1
            JLAST = N
            JINC = 1
            MAIND = KD + 1
         ELSE
            JFIRST = N
            JLAST = 1
            JINC = -1
            MAIND = 1
         END IF
*
         IF( TSCAL.NE.ONE ) THEN
            GROW = ZERO
            GO TO 80
         END IF
*
         IF( NOUNIT ) THEN
*
*           A is non-unit triangular.
*
*           Compute GROW = 1/G(j) and XBND = 1/M(j).
*           Initially, M(0) = max{x(i), i=1,...,n}.
*
            GROW = ONE / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 60 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 80
*
*              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) )
*
               XJ = ONE + CNORM( J )
               GROW = MIN( GROW, XBND / XJ )
*
*              M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j))
*
               TJJ = ABS( AB( MAIND, J ) )
               IF( XJ.GT.TJJ )
     $            XBND = XBND*( TJJ / XJ )
   60       CONTINUE
            GROW = MIN( GROW, XBND )
         ELSE
*
*           A is unit triangular.
*
*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
*
            GROW = MIN( ONE, ONE / MAX( XBND, SMLNUM ) )
            DO 70 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 80
*
*              G(j) = ( 1 + CNORM(j) )*G(j-1)
*
               XJ = ONE + CNORM( J )
               GROW = GROW / XJ
   70       CONTINUE
         END IF
   80    CONTINUE
      END IF
*
      IF( ( GROW*TSCAL ).GT.SMLNUM ) THEN
*
*        Use the Level 2 BLAS solve if the reciprocal of the bound on
*        elements of X is not too small.
*
         CALL DTBSV( UPLO, TRANS, DIAG, N, KD, AB, LDAB, X, 1 )
      ELSE
*
*        Use a Level 1 BLAS solve, scaling intermediate results.
*
         IF( XMAX.GT.BIGNUM ) THEN
*
*           Scale X so that its components are less than or equal to
*           BIGNUM in absolute value.
*
            SCALE = BIGNUM / XMAX
            CALL DSCAL( N, SCALE, X, 1 )
            XMAX = BIGNUM
         END IF
*
         IF( NOTRAN ) THEN
*
*           Solve A * x = b
*
            DO 110 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) / A(j,j), scaling x if necessary.
*
               XJ = ABS( X( J ) )
               IF( NOUNIT ) THEN
                  TJJS = AB( MAIND, J )*TSCAL
               ELSE
                  TJJS = TSCAL
                  IF( TSCAL.EQ.ONE )
     $               GO TO 100
               END IF
               TJJ = ABS( TJJS )
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
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  X( J ) = X( J ) / TJJS
                  XJ = ABS( X( J ) )
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
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
                  X( J ) = X( J ) / TJJS
                  XJ = ABS( X( J ) )
               ELSE
*
*                    A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                    scale = 0, and compute a solution to A*x = 0.
*
                  DO 90 I = 1, N
                     X( I ) = ZERO
   90             CONTINUE
                  X( J ) = ONE
                  XJ = ONE
                  SCALE = ZERO
                  XMAX = ZERO
               END IF
  100          CONTINUE
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
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                  END IF
               ELSE IF( XJ*CNORM( J ).GT.( BIGNUM-XMAX ) ) THEN
*
*                 Scale x by 1/2.
*
                  CALL DSCAL( N, HALF, X, 1 )
                  SCALE = SCALE*HALF
               END IF
*
               IF( UPPER ) THEN
                  IF( J.GT.1 ) THEN
*
*                    Compute the update
*                       x(max(1,j-kd):j-1) := x(max(1,j-kd):j-1) -
*                                             x(j)* A(max(1,j-kd):j-1,j)
*
                     JLEN = MIN( KD, J-1 )
                     CALL DAXPY( JLEN, -X( J )*TSCAL,
     $                           AB( KD+1-JLEN, J ), 1, X( J-JLEN ), 1 )
                     I = IDAMAX( J-1, X, 1 )
                     XMAX = ABS( X( I ) )
                  END IF
               ELSE IF( J.LT.N ) THEN
*
*                 Compute the update
*                    x(j+1:min(j+kd,n)) := x(j+1:min(j+kd,n)) -
*                                          x(j) * A(j+1:min(j+kd,n),j)
*
                  JLEN = MIN( KD, N-J )
                  IF( JLEN.GT.0 )
     $               CALL DAXPY( JLEN, -X( J )*TSCAL, AB( 2, J ), 1,
     $                           X( J+1 ), 1 )
                  I = J + IDAMAX( N-J, X( J+1 ), 1 )
                  XMAX = ABS( X( I ) )
               END IF
  110       CONTINUE
*
         ELSE
*
*           Solve A' * x = b
*
            DO 160 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) - sum A(k,j)*x(k).
*                                    k<>j
*
               XJ = ABS( X( J ) )
               USCAL = TSCAL
               REC = ONE / MAX( XMAX, ONE )
               IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN
*
*                 If x(j) could overflow, scale x by 1/(2*XMAX).
*
                  REC = REC*HALF
                  IF( NOUNIT ) THEN
                     TJJS = AB( MAIND, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                  END IF
                  TJJ = ABS( TJJS )
                  IF( TJJ.GT.ONE ) THEN
*
*                       Divide by A(j,j) when scaling x if A(j,j) > 1.
*
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = USCAL / TJJS
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
*
               SUMJ = ZERO
               IF( USCAL.EQ.ONE ) THEN
*
*                 If the scaling needed for A in the dot product is 1,
*                 call DDOT to perform the dot product.
*
                  IF( UPPER ) THEN
                     JLEN = MIN( KD, J-1 )
                     SUMJ = DDOT( JLEN, AB( KD+1-JLEN, J ), 1,
     $                      X( J-JLEN ), 1 )
                  ELSE
                     JLEN = MIN( KD, N-J )
                     IF( JLEN.GT.0 )
     $                  SUMJ = DDOT( JLEN, AB( 2, J ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
*
*                 Otherwise, use in-line code for the dot product.
*
                  IF( UPPER ) THEN
                     JLEN = MIN( KD, J-1 )
                     DO 120 I = 1, JLEN
                        SUMJ = SUMJ + ( AB( KD+I-JLEN, J )*USCAL )*
     $                         X( J-JLEN-1+I )
  120                CONTINUE
                  ELSE
                     JLEN = MIN( KD, N-J )
                     DO 130 I = 1, JLEN
                        SUMJ = SUMJ + ( AB( I+1, J )*USCAL )*X( J+I )
  130                CONTINUE
                  END IF
               END IF
*
               IF( USCAL.EQ.TSCAL ) THEN
*
*                 Compute x(j) := ( x(j) - sumj ) / A(j,j) if 1/A(j,j)
*                 was not used to scale the dotproduct.
*
                  X( J ) = X( J ) - SUMJ
                  XJ = ABS( X( J ) )
                  IF( NOUNIT ) THEN
*
*                    Compute x(j) = x(j) / A(j,j), scaling if necessary.
*
                     TJJS = AB( MAIND, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     $                  GO TO 150
                  END IF
                  TJJ = ABS( TJJS )
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
                           CALL DSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = X( J ) / TJJS
                  ELSE IF( TJJ.GT.ZERO ) THEN
*
*                       0 < abs(A(j,j)) <= SMLNUM:
*
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM.
*
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = X( J ) / TJJS
                  ELSE
*
*                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                       scale = 0, and compute a solution to A'*x = 0.
*
                     DO 140 I = 1, N
                        X( I ) = ZERO
  140                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  150             CONTINUE
               ELSE
*
*                 Compute x(j) := x(j) / A(j,j) - sumj if the dot
*                 product has already been divided by 1/A(j,j).
*
                  X( J ) = X( J ) / TJJS - SUMJ
               END IF
               XMAX = MAX( XMAX, ABS( X( J ) ) )
  160       CONTINUE
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
*     End of DLATBS
*
      END
      SUBROUTINE DLATDF( IJOB, N, Z, LDZ, RHS, RDSUM, RDSCAL, IPIV,
     $                   JPIV )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            IJOB, LDZ, N
      DOUBLE PRECISION   RDSCAL, RDSUM
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * ), JPIV( * )
      DOUBLE PRECISION   RHS( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DLATDF uses the LU factorization of the n-by-n matrix Z computed by
*  DGETC2 and computes a contribution to the reciprocal Dif-estimate
*  by solving Z * x = b for x, and choosing the r.h.s. b such that
*  the norm of x is as large as possible. On entry RHS = b holds the
*  contribution from earlier solved sub-systems, and on return RHS = x.
*
*  The factorization of Z returned by DGETC2 has the form Z = P*L*U*Q,
*  where P and Q are permutation matrices. L is lower triangular with
*  unit diagonal elements and U is upper triangular.
*
*  Arguments
*  =========
*
*  IJOB    (input) INTEGER
*          IJOB = 2: First compute an approximative null-vector e
*              of Z using DGECON, e is normalized and solve for
*              Zx = +-e - f with the sign giving the greater value
*              of 2-norm(x). About 5 times as expensive as Default.
*          IJOB .ne. 2: Local look ahead strategy where all entries of
*              the r.h.s. b is choosen as either +1 or -1 (Default).
*
*  N       (input) INTEGER
*          The number of columns of the matrix Z.
*
*  Z       (input) DOUBLE PRECISION array, dimension (LDZ, N)
*          On entry, the LU part of the factorization of the n-by-n
*          matrix Z computed by DGETC2:  Z = P * L * U * Q
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDA >= max(1, N).
*
*  RHS     (input/output) DOUBLE PRECISION array, dimension N.
*          On entry, RHS contains contributions from other subsystems.
*          On exit, RHS contains the solution of the subsystem with
*          entries acoording to the value of IJOB (see above).
*
*  RDSUM   (input/output) DOUBLE PRECISION
*          On entry, the sum of squares of computed contributions to
*          the Dif-estimate under computation by DTGSYL, where the
*          scaling factor RDSCAL (see below) has been factored out.
*          On exit, the corresponding sum of squares updated with the
*          contributions from the current sub-system.
*          If TRANS = 'T' RDSUM is not touched.
*          NOTE: RDSUM only makes sense when DTGSY2 is called by STGSYL.
*
*  RDSCAL  (input/output) DOUBLE PRECISION
*          On entry, scaling factor used to prevent overflow in RDSUM.
*          On exit, RDSCAL is updated w.r.t. the current contributions
*          in RDSUM.
*          If TRANS = 'T', RDSCAL is not touched.
*          NOTE: RDSCAL only makes sense when DTGSY2 is called by
*                DTGSYL.
*
*  IPIV    (input) INTEGER array, dimension (N).
*          The pivot indices; for 1 <= i <= N, row i of the
*          matrix has been interchanged with row IPIV(i).
*
*  JPIV    (input) INTEGER array, dimension (N).
*          The pivot indices; for 1 <= j <= N, column j of the
*          matrix has been interchanged with column JPIV(j).
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
*     Umea University, S-901 87 Umea, Sweden.
*
*  This routine is a further developed implementation of algorithm
*  BSOLVE in [1] using complete pivoting in the LU factorization.
*
*  [1] Bo Kagstrom and Lars Westin,
*      Generalized Schur Methods with Condition Estimators for
*      Solving the Generalized Sylvester Equation, IEEE Transactions
*      on Automatic Control, Vol. 34, No. 7, July 1989, pp 745-751.
*
*  [2] Peter Poromaa,
*      On Efficient and Robust Estimators for the Separation
*      between two Regular Matrix Pairs with Applications in
*      Condition Estimation. Report IMINF-95.05, Departement of
*      Computing Science, Umea University, S-901 87 Umea, Sweden, 1995.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            MAXDIM
      PARAMETER          ( MAXDIM = 8 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J, K
      DOUBLE PRECISION   BM, BP, PMONE, SMINU, SPLUS, TEMP
*     ..
*     .. Local Arrays ..
      INTEGER            IWORK( MAXDIM )
      DOUBLE PRECISION   WORK( 4*MAXDIM ), XM( MAXDIM ), XP( MAXDIM )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGECON, DGESC2, DLASSQ, DLASWP,
     $                   DSCAL
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DASUM, DDOT
      EXTERNAL           DASUM, DDOT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
*     ..
*     .. Executable Statements ..
*
      IF( IJOB.NE.2 ) THEN
*
*        Apply permutations IPIV to RHS
*
         CALL DLASWP( 1, RHS, LDZ, 1, N-1, IPIV, 1 )
*
*        Solve for L-part choosing RHS either to +1 or -1.
*
         PMONE = -ONE
*
         DO 10 J = 1, N - 1
            BP = RHS( J ) + ONE
            BM = RHS( J ) - ONE
            SPLUS = ONE
*
*           Look-ahead for L-part RHS(1:N-1) = + or -1, SPLUS and
*           SMIN computed more efficiently than in BSOLVE [1].
*
            SPLUS = SPLUS + DDOT( N-J, Z( J+1, J ), 1, Z( J+1, J ), 1 )
            SMINU = DDOT( N-J, Z( J+1, J ), 1, RHS( J+1 ), 1 )
            SPLUS = SPLUS*RHS( J )
            IF( SPLUS.GT.SMINU ) THEN
               RHS( J ) = BP
            ELSE IF( SMINU.GT.SPLUS ) THEN
               RHS( J ) = BM
            ELSE
*
*              In this case the updating sums are equal and we can
*              choose RHS(J) +1 or -1. The first time this happens
*              we choose -1, thereafter +1. This is a simple way to
*              get good estimates of matrices like Byers well-known
*              example (see [1]). (Not done in BSOLVE.)
*
               RHS( J ) = RHS( J ) + PMONE
               PMONE = ONE
            END IF
*
*           Compute the remaining r.h.s.
*
            TEMP = -RHS( J )
            CALL DAXPY( N-J, TEMP, Z( J+1, J ), 1, RHS( J+1 ), 1 )
*
   10    CONTINUE
*
*        Solve for U-part, look-ahead for RHS(N) = +-1. This is not done
*        in BSOLVE and will hopefully give us a better estimate because
*        any ill-conditioning of the original matrix is transfered to U
*        and not to L. U(N, N) is an approximation to sigma_min(LU).
*
         CALL DCOPY( N-1, RHS, 1, XP, 1 )
         XP( N ) = RHS( N ) + ONE
         RHS( N ) = RHS( N ) - ONE
         SPLUS = ZERO
         SMINU = ZERO
         DO 30 I = N, 1, -1
            TEMP = ONE / Z( I, I )
            XP( I ) = XP( I )*TEMP
            RHS( I ) = RHS( I )*TEMP
            DO 20 K = I + 1, N
               XP( I ) = XP( I ) - XP( K )*( Z( I, K )*TEMP )
               RHS( I ) = RHS( I ) - RHS( K )*( Z( I, K )*TEMP )
   20       CONTINUE
            SPLUS = SPLUS + ABS( XP( I ) )
            SMINU = SMINU + ABS( RHS( I ) )
   30    CONTINUE
         IF( SPLUS.GT.SMINU )
     $      CALL DCOPY( N, XP, 1, RHS, 1 )
*
*        Apply the permutations JPIV to the computed solution (RHS)
*
         CALL DLASWP( 1, RHS, LDZ, 1, N-1, JPIV, -1 )
*
*        Compute the sum of squares
*
         CALL DLASSQ( N, RHS, 1, RDSCAL, RDSUM )
*
      ELSE
*
*        IJOB = 2, Compute approximate nullvector XM of Z
*
         CALL DGECON( 'I', N, Z, LDZ, ONE, TEMP, WORK, IWORK, INFO )
         CALL DCOPY( N, WORK( N+1 ), 1, XM, 1 )
*
*        Compute RHS
*
         CALL DLASWP( 1, XM, LDZ, 1, N-1, IPIV, -1 )
         TEMP = ONE / SQRT( DDOT( N, XM, 1, XM, 1 ) )
         CALL DSCAL( N, TEMP, XM, 1 )
         CALL DCOPY( N, XM, 1, XP, 1 )
         CALL DAXPY( N, ONE, RHS, 1, XP, 1 )
         CALL DAXPY( N, -ONE, XM, 1, RHS, 1 )
         CALL DGESC2( N, Z, LDZ, RHS, IPIV, JPIV, TEMP )
         CALL DGESC2( N, Z, LDZ, XP, IPIV, JPIV, TEMP )
         IF( DASUM( N, XP, 1 ).GT.DASUM( N, RHS, 1 ) )
     $      CALL DCOPY( N, XP, 1, RHS, 1 )
*
*        Compute the sum of squares
*
         CALL DLASSQ( N, RHS, 1, RDSCAL, RDSUM )
*
      END IF
*
      RETURN
*
*     End of DLATDF
*
      END
      SUBROUTINE DLATPS( UPLO, TRANS, DIAG, NORMIN, N, AP, X, SCALE,
     $                   CNORM, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, NORMIN, TRANS, UPLO
      INTEGER            INFO, N
      DOUBLE PRECISION   SCALE
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AP( * ), CNORM( * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLATPS solves one of the triangular systems
*
*     A *x = s*b  or  A'*x = s*b
*
*  with scaling to prevent overflow, where A is an upper or lower
*  triangular matrix stored in packed form.  Here A' denotes the
*  transpose of A, x and b are n-element vectors, and s is a scaling
*  factor, usually less than or equal to 1, chosen so that the
*  components of x will be less than the overflow threshold.  If the
*  unscaled problem will not cause overflow, the Level 2 BLAS routine
*  DTPSV is called. If the matrix A is singular (A(j,j) = 0 for some j),
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
*          = 'N':  Solve A * x = s*b  (No transpose)
*          = 'T':  Solve A'* x = s*b  (Transpose)
*          = 'C':  Solve A'* x = s*b  (Conjugate transpose = Transpose)
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
*  AP      (input) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          The upper or lower triangular matrix A, packed columnwise in
*          a linear array.  The j-th column of A is stored in the array
*          AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*
*  X       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the right hand side b of the triangular system.
*          On exit, X is overwritten by the solution vector x.
*
*  SCALE   (output) DOUBLE PRECISION
*          The scaling factor s for the triangular system
*             A * x = s*b  or  A'* x = s*b.
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
*  A rough bound on x is computed; if that is less than overflow, DTPSV
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
*  Since |x(j)| <= M(j), we use the Level 2 BLAS routine DTPSV if the
*  reciprocal of the largest M(j), j=1,..,n, is larger than
*  max(underflow, 1/overflow).
*
*  The bound on x(j) is also used to determine when a step in the
*  columnwise method can be performed without fear of overflow.  If
*  the computed bound is greater than a large constant, x is scaled to
*  prevent overflow, but if the bound overflows, x is set to 0, x(j) to
*  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.
*
*  Similarly, a row-wise scheme is used to solve A'*x = b.  The basic
*  algorithm for A upper triangular is
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
*  and we can safely call DTPSV if 1/M(n) and 1/G(n) are both greater
*  than max(underflow, 1/overflow).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN, NOUNIT, UPPER
      INTEGER            I, IMAX, IP, J, JFIRST, JINC, JLAST, JLEN
      DOUBLE PRECISION   BIGNUM, GROW, REC, SMLNUM, SUMJ, TJJ, TJJS,
     $                   TMAX, TSCAL, USCAL, XBND, XJ, XMAX
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM, DDOT, DLAMCH
      EXTERNAL           LSAME, IDAMAX, DASUM, DDOT, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DSCAL, DTPSV, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
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
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLATPS', -INFO )
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
      SMLNUM = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' )
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
            IP = 1
            DO 10 J = 1, N
               CNORM( J ) = DASUM( J-1, AP( IP ), 1 )
               IP = IP + J
   10       CONTINUE
         ELSE
*
*           A is lower triangular.
*
            IP = 1
            DO 20 J = 1, N - 1
               CNORM( J ) = DASUM( N-J, AP( IP+1 ), 1 )
               IP = IP + N - J + 1
   20       CONTINUE
            CNORM( N ) = ZERO
         END IF
      END IF
*
*     Scale the column norms by TSCAL if the maximum element in CNORM is
*     greater than BIGNUM.
*
      IMAX = IDAMAX( N, CNORM, 1 )
      TMAX = CNORM( IMAX )
      IF( TMAX.LE.BIGNUM ) THEN
         TSCAL = ONE
      ELSE
         TSCAL = ONE / ( SMLNUM*TMAX )
         CALL DSCAL( N, TSCAL, CNORM, 1 )
      END IF
*
*     Compute a bound on the computed solution vector to see if the
*     Level 2 BLAS routine DTPSV can be used.
*
      J = IDAMAX( N, X, 1 )
      XMAX = ABS( X( J ) )
      XBND = XMAX
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
            GO TO 50
         END IF
*
         IF( NOUNIT ) THEN
*
*           A is non-unit triangular.
*
*           Compute GROW = 1/G(j) and XBND = 1/M(j).
*           Initially, G(0) = max{x(i), i=1,...,n}.
*
            GROW = ONE / MAX( XBND, SMLNUM )
            XBND = GROW
            IP = JFIRST*( JFIRST+1 ) / 2
            JLEN = N
            DO 30 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 50
*
*              M(j) = G(j-1) / abs(A(j,j))
*
               TJJ = ABS( AP( IP ) )
               XBND = MIN( XBND, MIN( ONE, TJJ )*GROW )
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
               IP = IP + JINC*JLEN
               JLEN = JLEN - 1
   30       CONTINUE
            GROW = XBND
         ELSE
*
*           A is unit triangular.
*
*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
*
            GROW = MIN( ONE, ONE / MAX( XBND, SMLNUM ) )
            DO 40 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 50
*
*              G(j) = G(j-1)*( 1 + CNORM(j) )
*
               GROW = GROW*( ONE / ( ONE+CNORM( J ) ) )
   40       CONTINUE
         END IF
   50    CONTINUE
*
      ELSE
*
*        Compute the growth in A' * x = b.
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
            GO TO 80
         END IF
*
         IF( NOUNIT ) THEN
*
*           A is non-unit triangular.
*
*           Compute GROW = 1/G(j) and XBND = 1/M(j).
*           Initially, M(0) = max{x(i), i=1,...,n}.
*
            GROW = ONE / MAX( XBND, SMLNUM )
            XBND = GROW
            IP = JFIRST*( JFIRST+1 ) / 2
            JLEN = 1
            DO 60 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 80
*
*              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) )
*
               XJ = ONE + CNORM( J )
               GROW = MIN( GROW, XBND / XJ )
*
*              M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j))
*
               TJJ = ABS( AP( IP ) )
               IF( XJ.GT.TJJ )
     $            XBND = XBND*( TJJ / XJ )
               JLEN = JLEN + 1
               IP = IP + JINC*JLEN
   60       CONTINUE
            GROW = MIN( GROW, XBND )
         ELSE
*
*           A is unit triangular.
*
*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
*
            GROW = MIN( ONE, ONE / MAX( XBND, SMLNUM ) )
            DO 70 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 80
*
*              G(j) = ( 1 + CNORM(j) )*G(j-1)
*
               XJ = ONE + CNORM( J )
               GROW = GROW / XJ
   70       CONTINUE
         END IF
   80    CONTINUE
      END IF
*
      IF( ( GROW*TSCAL ).GT.SMLNUM ) THEN
*
*        Use the Level 2 BLAS solve if the reciprocal of the bound on
*        elements of X is not too small.
*
         CALL DTPSV( UPLO, TRANS, DIAG, N, AP, X, 1 )
      ELSE
*
*        Use a Level 1 BLAS solve, scaling intermediate results.
*
         IF( XMAX.GT.BIGNUM ) THEN
*
*           Scale X so that its components are less than or equal to
*           BIGNUM in absolute value.
*
            SCALE = BIGNUM / XMAX
            CALL DSCAL( N, SCALE, X, 1 )
            XMAX = BIGNUM
         END IF
*
         IF( NOTRAN ) THEN
*
*           Solve A * x = b
*
            IP = JFIRST*( JFIRST+1 ) / 2
            DO 110 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) / A(j,j), scaling x if necessary.
*
               XJ = ABS( X( J ) )
               IF( NOUNIT ) THEN
                  TJJS = AP( IP )*TSCAL
               ELSE
                  TJJS = TSCAL
                  IF( TSCAL.EQ.ONE )
     $               GO TO 100
               END IF
               TJJ = ABS( TJJS )
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
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  X( J ) = X( J ) / TJJS
                  XJ = ABS( X( J ) )
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
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
                  X( J ) = X( J ) / TJJS
                  XJ = ABS( X( J ) )
               ELSE
*
*                    A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                    scale = 0, and compute a solution to A*x = 0.
*
                  DO 90 I = 1, N
                     X( I ) = ZERO
   90             CONTINUE
                  X( J ) = ONE
                  XJ = ONE
                  SCALE = ZERO
                  XMAX = ZERO
               END IF
  100          CONTINUE
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
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                  END IF
               ELSE IF( XJ*CNORM( J ).GT.( BIGNUM-XMAX ) ) THEN
*
*                 Scale x by 1/2.
*
                  CALL DSCAL( N, HALF, X, 1 )
                  SCALE = SCALE*HALF
               END IF
*
               IF( UPPER ) THEN
                  IF( J.GT.1 ) THEN
*
*                    Compute the update
*                       x(1:j-1) := x(1:j-1) - x(j) * A(1:j-1,j)
*
                     CALL DAXPY( J-1, -X( J )*TSCAL, AP( IP-J+1 ), 1, X,
     $                           1 )
                     I = IDAMAX( J-1, X, 1 )
                     XMAX = ABS( X( I ) )
                  END IF
                  IP = IP - J
               ELSE
                  IF( J.LT.N ) THEN
*
*                    Compute the update
*                       x(j+1:n) := x(j+1:n) - x(j) * A(j+1:n,j)
*
                     CALL DAXPY( N-J, -X( J )*TSCAL, AP( IP+1 ), 1,
     $                           X( J+1 ), 1 )
                     I = J + IDAMAX( N-J, X( J+1 ), 1 )
                     XMAX = ABS( X( I ) )
                  END IF
                  IP = IP + N - J + 1
               END IF
  110       CONTINUE
*
         ELSE
*
*           Solve A' * x = b
*
            IP = JFIRST*( JFIRST+1 ) / 2
            JLEN = 1
            DO 160 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) - sum A(k,j)*x(k).
*                                    k<>j
*
               XJ = ABS( X( J ) )
               USCAL = TSCAL
               REC = ONE / MAX( XMAX, ONE )
               IF( CNORM( J ).GT.( BIGNUM-XJ )*REC ) THEN
*
*                 If x(j) could overflow, scale x by 1/(2*XMAX).
*
                  REC = REC*HALF
                  IF( NOUNIT ) THEN
                     TJJS = AP( IP )*TSCAL
                  ELSE
                     TJJS = TSCAL
                  END IF
                  TJJ = ABS( TJJS )
                  IF( TJJ.GT.ONE ) THEN
*
*                       Divide by A(j,j) when scaling x if A(j,j) > 1.
*
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = USCAL / TJJS
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
*
               SUMJ = ZERO
               IF( USCAL.EQ.ONE ) THEN
*
*                 If the scaling needed for A in the dot product is 1,
*                 call DDOT to perform the dot product.
*
                  IF( UPPER ) THEN
                     SUMJ = DDOT( J-1, AP( IP-J+1 ), 1, X, 1 )
                  ELSE IF( J.LT.N ) THEN
                     SUMJ = DDOT( N-J, AP( IP+1 ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
*
*                 Otherwise, use in-line code for the dot product.
*
                  IF( UPPER ) THEN
                     DO 120 I = 1, J - 1
                        SUMJ = SUMJ + ( AP( IP-J+I )*USCAL )*X( I )
  120                CONTINUE
                  ELSE IF( J.LT.N ) THEN
                     DO 130 I = 1, N - J
                        SUMJ = SUMJ + ( AP( IP+I )*USCAL )*X( J+I )
  130                CONTINUE
                  END IF
               END IF
*
               IF( USCAL.EQ.TSCAL ) THEN
*
*                 Compute x(j) := ( x(j) - sumj ) / A(j,j) if 1/A(j,j)
*                 was not used to scale the dotproduct.
*
                  X( J ) = X( J ) - SUMJ
                  XJ = ABS( X( J ) )
                  IF( NOUNIT ) THEN
*
*                    Compute x(j) = x(j) / A(j,j), scaling if necessary.
*
                     TJJS = AP( IP )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     $                  GO TO 150
                  END IF
                  TJJ = ABS( TJJS )
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
                           CALL DSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = X( J ) / TJJS
                  ELSE IF( TJJ.GT.ZERO ) THEN
*
*                       0 < abs(A(j,j)) <= SMLNUM:
*
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM.
*
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = X( J ) / TJJS
                  ELSE
*
*                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                       scale = 0, and compute a solution to A'*x = 0.
*
                     DO 140 I = 1, N
                        X( I ) = ZERO
  140                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  150             CONTINUE
               ELSE
*
*                 Compute x(j) := x(j) / A(j,j)  - sumj if the dot
*                 product has already been divided by 1/A(j,j).
*
                  X( J ) = X( J ) / TJJS - SUMJ
               END IF
               XMAX = MAX( XMAX, ABS( X( J ) ) )
               JLEN = JLEN + 1
               IP = IP + JINC*JLEN
  160       CONTINUE
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
*     End of DLATPS
*
      END
      SUBROUTINE DLATRD( UPLO, N, NB, A, LDA, E, TAU, W, LDW )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDW, N, NB
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), E( * ), TAU( * ), W( LDW, * )
*     ..
*
*  Purpose
*  =======
*
*  DLATRD reduces NB rows and columns of a real symmetric matrix A to
*  symmetric tridiagonal form by an orthogonal similarity
*  transformation Q' * A * Q, and returns the matrices V and W which are
*  needed to apply the transformation to the unreduced part of A.
*
*  If UPLO = 'U', DLATRD reduces the last NB rows and columns of a
*  matrix, of which the upper triangle is supplied;
*  if UPLO = 'L', DLATRD reduces the first NB rows and columns of a
*  matrix, of which the lower triangle is supplied.
*
*  This is an auxiliary routine called by DSYTRD.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          symmetric matrix A is stored:
*          = 'U': Upper triangular
*          = 'L': Lower triangular
*
*  N       (input) INTEGER
*          The order of the matrix A.
*
*  NB      (input) INTEGER
*          The number of rows and columns to be reduced.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
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
*            with the array TAU, represent the orthogonal matrix Q as a
*            product of elementary reflectors;
*          if UPLO = 'L', the first NB columns have been reduced to
*            tridiagonal form, with the diagonal elements overwriting
*            the diagonal elements of A; the elements below the diagonal
*            with the array TAU, represent the  orthogonal matrix Q as a
*            product of elementary reflectors.
*          See Further Details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= (1,N).
*
*  E       (output) DOUBLE PRECISION array, dimension (N-1)
*          If UPLO = 'U', E(n-nb:n-1) contains the superdiagonal
*          elements of the last NB columns of the reduced matrix;
*          if UPLO = 'L', E(1:nb) contains the subdiagonal elements of
*          the first NB columns of the reduced matrix.
*
*  TAU     (output) DOUBLE PRECISION array, dimension (N-1)
*          The scalar factors of the elementary reflectors, stored in
*          TAU(n-nb:n-1) if UPLO = 'U', and in TAU(1:nb) if UPLO = 'L'.
*          See Further Details.
*
*  W       (output) DOUBLE PRECISION array, dimension (LDW,NB)
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
*  where tau is a real scalar, and v is a real vector with
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
*  where tau is a real scalar, and v is a real vector with
*  v(1:i) = 0 and v(i+1) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
*  and tau in TAU(i).
*
*  The elements of the vectors v together form the n-by-nb matrix V
*  which is needed, with W, to apply the transformation to the unreduced
*  part of the matrix, using a symmetric rank-2k update of the form:
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
      DOUBLE PRECISION   ZERO, ONE, HALF
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HALF = 0.5D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IW
      DOUBLE PRECISION   ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DGEMV, DLARFG, DSCAL, DSYMV
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT
      EXTERNAL           LSAME, DDOT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
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
               CALL DGEMV( 'No transpose', I, N-I, -ONE, A( 1, I+1 ),
     $                     LDA, W( I, IW+1 ), LDW, ONE, A( 1, I ), 1 )
               CALL DGEMV( 'No transpose', I, N-I, -ONE, W( 1, IW+1 ),
     $                     LDW, A( I, I+1 ), LDA, ONE, A( 1, I ), 1 )
            END IF
            IF( I.GT.1 ) THEN
*
*              Generate elementary reflector H(i) to annihilate
*              A(1:i-2,i)
*
               CALL DLARFG( I-1, A( I-1, I ), A( 1, I ), 1, TAU( I-1 ) )
               E( I-1 ) = A( I-1, I )
               A( I-1, I ) = ONE
*
*              Compute W(1:i-1,i)
*
               CALL DSYMV( 'Upper', I-1, ONE, A, LDA, A( 1, I ), 1,
     $                     ZERO, W( 1, IW ), 1 )
               IF( I.LT.N ) THEN
                  CALL DGEMV( 'Transpose', I-1, N-I, ONE, W( 1, IW+1 ),
     $                        LDW, A( 1, I ), 1, ZERO, W( I+1, IW ), 1 )
                  CALL DGEMV( 'No transpose', I-1, N-I, -ONE,
     $                        A( 1, I+1 ), LDA, W( I+1, IW ), 1, ONE,
     $                        W( 1, IW ), 1 )
                  CALL DGEMV( 'Transpose', I-1, N-I, ONE, A( 1, I+1 ),
     $                        LDA, A( 1, I ), 1, ZERO, W( I+1, IW ), 1 )
                  CALL DGEMV( 'No transpose', I-1, N-I, -ONE,
     $                        W( 1, IW+1 ), LDW, W( I+1, IW ), 1, ONE,
     $                        W( 1, IW ), 1 )
               END IF
               CALL DSCAL( I-1, TAU( I-1 ), W( 1, IW ), 1 )
               ALPHA = -HALF*TAU( I-1 )*DDOT( I-1, W( 1, IW ), 1,
     $                 A( 1, I ), 1 )
               CALL DAXPY( I-1, ALPHA, A( 1, I ), 1, W( 1, IW ), 1 )
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
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, A( I, 1 ),
     $                  LDA, W( I, 1 ), LDW, ONE, A( I, I ), 1 )
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, W( I, 1 ),
     $                  LDW, A( I, 1 ), LDA, ONE, A( I, I ), 1 )
            IF( I.LT.N ) THEN
*
*              Generate elementary reflector H(i) to annihilate
*              A(i+2:n,i)
*
               CALL DLARFG( N-I, A( I+1, I ), A( MIN( I+2, N ), I ), 1,
     $                      TAU( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
*
*              Compute W(i+1:n,i)
*
               CALL DSYMV( 'Lower', N-I, ONE, A( I+1, I+1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, W( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I-1, ONE, W( I+1, 1 ), LDW,
     $                     A( I+1, I ), 1, ZERO, W( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, A( I+1, 1 ),
     $                     LDA, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I-1, ONE, A( I+1, 1 ), LDA,
     $                     A( I+1, I ), 1, ZERO, W( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, W( I+1, 1 ),
     $                     LDW, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
               CALL DSCAL( N-I, TAU( I ), W( I+1, I ), 1 )
               ALPHA = -HALF*TAU( I )*DDOT( N-I, W( I+1, I ), 1,
     $                 A( I+1, I ), 1 )
               CALL DAXPY( N-I, ALPHA, A( I+1, I ), 1, W( I+1, I ), 1 )
            END IF
*
   20    CONTINUE
      END IF
*
      RETURN
*
*     End of DLATRD
*
      END
      SUBROUTINE DLATRS( UPLO, TRANS, DIAG, NORMIN, N, A, LDA, X, SCALE,
     $                   CNORM, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG, NORMIN, TRANS, UPLO
      INTEGER            INFO, LDA, N
      DOUBLE PRECISION   SCALE
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), CNORM( * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DLATRS solves one of the triangular systems
*
*     A *x = s*b  or  A'*x = s*b
*
*  with scaling to prevent overflow.  Here A is an upper or lower
*  triangular matrix, A' denotes the transpose of A, x and b are
*  n-element vectors, and s is a scaling factor, usually less than
*  or equal to 1, chosen so that the components of x will be less than
*  the overflow threshold.  If the unscaled problem will not cause
*  overflow, the Level 2 BLAS routine DTRSV is called.  If the matrix A
*  is singular (A(j,j) = 0 for some j), then s is set to 0 and a
*  non-trivial solution to A*x = 0 is returned.
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
*          = 'N':  Solve A * x = s*b  (No transpose)
*          = 'T':  Solve A'* x = s*b  (Transpose)
*          = 'C':  Solve A'* x = s*b  (Conjugate transpose = Transpose)
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
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
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
*  X       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the right hand side b of the triangular system.
*          On exit, X is overwritten by the solution vector x.
*
*  SCALE   (output) DOUBLE PRECISION
*          The scaling factor s for the triangular system
*             A * x = s*b  or  A'* x = s*b.
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
*  A rough bound on x is computed; if that is less than overflow, DTRSV
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
*  Since |x(j)| <= M(j), we use the Level 2 BLAS routine DTRSV if the
*  reciprocal of the largest M(j), j=1,..,n, is larger than
*  max(underflow, 1/overflow).
*
*  The bound on x(j) is also used to determine when a step in the
*  columnwise method can be performed without fear of overflow.  If
*  the computed bound is greater than a large constant, x is scaled to
*  prevent overflow, but if the bound overflows, x is set to 0, x(j) to
*  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.
*
*  Similarly, a row-wise scheme is used to solve A'*x = b.  The basic
*  algorithm for A upper triangular is
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
*  and we can safely call DTRSV if 1/M(n) and 1/G(n) are both greater
*  than max(underflow, 1/overflow).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN, NOUNIT, UPPER
      INTEGER            I, IMAX, J, JFIRST, JINC, JLAST
      DOUBLE PRECISION   BIGNUM, GROW, REC, SMLNUM, SUMJ, TJJ, TJJS,
     $                   TMAX, TSCAL, USCAL, XBND, XJ, XMAX
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM, DDOT, DLAMCH
      EXTERNAL           LSAME, IDAMAX, DASUM, DDOT, DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DSCAL, DTRSV, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
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
         CALL XERBLA( 'DLATRS', -INFO )
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
      SMLNUM = DLAMCH( 'Safe minimum' ) / DLAMCH( 'Precision' )
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
               CNORM( J ) = DASUM( J-1, A( 1, J ), 1 )
   10       CONTINUE
         ELSE
*
*           A is lower triangular.
*
            DO 20 J = 1, N - 1
               CNORM( J ) = DASUM( N-J, A( J+1, J ), 1 )
   20       CONTINUE
            CNORM( N ) = ZERO
         END IF
      END IF
*
*     Scale the column norms by TSCAL if the maximum element in CNORM is
*     greater than BIGNUM.
*
      IMAX = IDAMAX( N, CNORM, 1 )
      TMAX = CNORM( IMAX )
      IF( TMAX.LE.BIGNUM ) THEN
         TSCAL = ONE
      ELSE
         TSCAL = ONE / ( SMLNUM*TMAX )
         CALL DSCAL( N, TSCAL, CNORM, 1 )
      END IF
*
*     Compute a bound on the computed solution vector to see if the
*     Level 2 BLAS routine DTRSV can be used.
*
      J = IDAMAX( N, X, 1 )
      XMAX = ABS( X( J ) )
      XBND = XMAX
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
            GO TO 50
         END IF
*
         IF( NOUNIT ) THEN
*
*           A is non-unit triangular.
*
*           Compute GROW = 1/G(j) and XBND = 1/M(j).
*           Initially, G(0) = max{x(i), i=1,...,n}.
*
            GROW = ONE / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 30 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 50
*
*              M(j) = G(j-1) / abs(A(j,j))
*
               TJJ = ABS( A( J, J ) )
               XBND = MIN( XBND, MIN( ONE, TJJ )*GROW )
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
   30       CONTINUE
            GROW = XBND
         ELSE
*
*           A is unit triangular.
*
*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
*
            GROW = MIN( ONE, ONE / MAX( XBND, SMLNUM ) )
            DO 40 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 50
*
*              G(j) = G(j-1)*( 1 + CNORM(j) )
*
               GROW = GROW*( ONE / ( ONE+CNORM( J ) ) )
   40       CONTINUE
         END IF
   50    CONTINUE
*
      ELSE
*
*        Compute the growth in A' * x = b.
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
            GO TO 80
         END IF
*
         IF( NOUNIT ) THEN
*
*           A is non-unit triangular.
*
*           Compute GROW = 1/G(j) and XBND = 1/M(j).
*           Initially, M(0) = max{x(i), i=1,...,n}.
*
            GROW = ONE / MAX( XBND, SMLNUM )
            XBND = GROW
            DO 60 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 80
*
*              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) )
*
               XJ = ONE + CNORM( J )
               GROW = MIN( GROW, XBND / XJ )
*
*              M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j))
*
               TJJ = ABS( A( J, J ) )
               IF( XJ.GT.TJJ )
     $            XBND = XBND*( TJJ / XJ )
   60       CONTINUE
            GROW = MIN( GROW, XBND )
         ELSE
*
*           A is unit triangular.
*
*           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
*
            GROW = MIN( ONE, ONE / MAX( XBND, SMLNUM ) )
            DO 70 J = JFIRST, JLAST, JINC
*
*              Exit the loop if the growth factor is too small.
*
               IF( GROW.LE.SMLNUM )
     $            GO TO 80
*
*              G(j) = ( 1 + CNORM(j) )*G(j-1)
*
               XJ = ONE + CNORM( J )
               GROW = GROW / XJ
   70       CONTINUE
         END IF
   80    CONTINUE
      END IF
*
      IF( ( GROW*TSCAL ).GT.SMLNUM ) THEN
*
*        Use the Level 2 BLAS solve if the reciprocal of the bound on
*        elements of X is not too small.
*
         CALL DTRSV( UPLO, TRANS, DIAG, N, A, LDA, X, 1 )
      ELSE
*
*        Use a Level 1 BLAS solve, scaling intermediate results.
*
         IF( XMAX.GT.BIGNUM ) THEN
*
*           Scale X so that its components are less than or equal to
*           BIGNUM in absolute value.
*
            SCALE = BIGNUM / XMAX
            CALL DSCAL( N, SCALE, X, 1 )
            XMAX = BIGNUM
         END IF
*
         IF( NOTRAN ) THEN
*
*           Solve A * x = b
*
            DO 110 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) / A(j,j), scaling x if necessary.
*
               XJ = ABS( X( J ) )
               IF( NOUNIT ) THEN
                  TJJS = A( J, J )*TSCAL
               ELSE
                  TJJS = TSCAL
                  IF( TSCAL.EQ.ONE )
     $               GO TO 100
               END IF
               TJJ = ABS( TJJS )
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
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                  END IF
                  X( J ) = X( J ) / TJJS
                  XJ = ABS( X( J ) )
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
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
                  X( J ) = X( J ) / TJJS
                  XJ = ABS( X( J ) )
               ELSE
*
*                    A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                    scale = 0, and compute a solution to A*x = 0.
*
                  DO 90 I = 1, N
                     X( I ) = ZERO
   90             CONTINUE
                  X( J ) = ONE
                  XJ = ONE
                  SCALE = ZERO
                  XMAX = ZERO
               END IF
  100          CONTINUE
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
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                  END IF
               ELSE IF( XJ*CNORM( J ).GT.( BIGNUM-XMAX ) ) THEN
*
*                 Scale x by 1/2.
*
                  CALL DSCAL( N, HALF, X, 1 )
                  SCALE = SCALE*HALF
               END IF
*
               IF( UPPER ) THEN
                  IF( J.GT.1 ) THEN
*
*                    Compute the update
*                       x(1:j-1) := x(1:j-1) - x(j) * A(1:j-1,j)
*
                     CALL DAXPY( J-1, -X( J )*TSCAL, A( 1, J ), 1, X,
     $                           1 )
                     I = IDAMAX( J-1, X, 1 )
                     XMAX = ABS( X( I ) )
                  END IF
               ELSE
                  IF( J.LT.N ) THEN
*
*                    Compute the update
*                       x(j+1:n) := x(j+1:n) - x(j) * A(j+1:n,j)
*
                     CALL DAXPY( N-J, -X( J )*TSCAL, A( J+1, J ), 1,
     $                           X( J+1 ), 1 )
                     I = J + IDAMAX( N-J, X( J+1 ), 1 )
                     XMAX = ABS( X( I ) )
                  END IF
               END IF
  110       CONTINUE
*
         ELSE
*
*           Solve A' * x = b
*
            DO 160 J = JFIRST, JLAST, JINC
*
*              Compute x(j) = b(j) - sum A(k,j)*x(k).
*                                    k<>j
*
               XJ = ABS( X( J ) )
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
                  TJJ = ABS( TJJS )
                  IF( TJJ.GT.ONE ) THEN
*
*                       Divide by A(j,j) when scaling x if A(j,j) > 1.
*
                     REC = MIN( ONE, REC*TJJ )
                     USCAL = USCAL / TJJS
                  END IF
                  IF( REC.LT.ONE ) THEN
                     CALL DSCAL( N, REC, X, 1 )
                     SCALE = SCALE*REC
                     XMAX = XMAX*REC
                  END IF
               END IF
*
               SUMJ = ZERO
               IF( USCAL.EQ.ONE ) THEN
*
*                 If the scaling needed for A in the dot product is 1,
*                 call DDOT to perform the dot product.
*
                  IF( UPPER ) THEN
                     SUMJ = DDOT( J-1, A( 1, J ), 1, X, 1 )
                  ELSE IF( J.LT.N ) THEN
                     SUMJ = DDOT( N-J, A( J+1, J ), 1, X( J+1 ), 1 )
                  END IF
               ELSE
*
*                 Otherwise, use in-line code for the dot product.
*
                  IF( UPPER ) THEN
                     DO 120 I = 1, J - 1
                        SUMJ = SUMJ + ( A( I, J )*USCAL )*X( I )
  120                CONTINUE
                  ELSE IF( J.LT.N ) THEN
                     DO 130 I = J + 1, N
                        SUMJ = SUMJ + ( A( I, J )*USCAL )*X( I )
  130                CONTINUE
                  END IF
               END IF
*
               IF( USCAL.EQ.TSCAL ) THEN
*
*                 Compute x(j) := ( x(j) - sumj ) / A(j,j) if 1/A(j,j)
*                 was not used to scale the dotproduct.
*
                  X( J ) = X( J ) - SUMJ
                  XJ = ABS( X( J ) )
                  IF( NOUNIT ) THEN
                     TJJS = A( J, J )*TSCAL
                  ELSE
                     TJJS = TSCAL
                     IF( TSCAL.EQ.ONE )
     $                  GO TO 150
                  END IF
*
*                    Compute x(j) = x(j) / A(j,j), scaling if necessary.
*
                  TJJ = ABS( TJJS )
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
                           CALL DSCAL( N, REC, X, 1 )
                           SCALE = SCALE*REC
                           XMAX = XMAX*REC
                        END IF
                     END IF
                     X( J ) = X( J ) / TJJS
                  ELSE IF( TJJ.GT.ZERO ) THEN
*
*                       0 < abs(A(j,j)) <= SMLNUM:
*
                     IF( XJ.GT.TJJ*BIGNUM ) THEN
*
*                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM.
*
                        REC = ( TJJ*BIGNUM ) / XJ
                        CALL DSCAL( N, REC, X, 1 )
                        SCALE = SCALE*REC
                        XMAX = XMAX*REC
                     END IF
                     X( J ) = X( J ) / TJJS
                  ELSE
*
*                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
*                       scale = 0, and compute a solution to A'*x = 0.
*
                     DO 140 I = 1, N
                        X( I ) = ZERO
  140                CONTINUE
                     X( J ) = ONE
                     SCALE = ZERO
                     XMAX = ZERO
                  END IF
  150             CONTINUE
               ELSE
*
*                 Compute x(j) := x(j) / A(j,j)  - sumj if the dot
*                 product has already been divided by 1/A(j,j).
*
                  X( J ) = X( J ) / TJJS - SUMJ
               END IF
               XMAX = MAX( XMAX, ABS( X( J ) ) )
  160       CONTINUE
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
*     End of DLATRS
*
      END
      SUBROUTINE DLAUU2( UPLO, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLAUU2 computes the product U * U' or L' * L, where the triangular
*  factor U or L is stored in the upper or lower triangular part of
*  the array A.
*
*  If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
*  overwriting the factor U in A.
*  If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
*  overwriting the factor L in A.
*
*  This is the unblocked form of the algorithm, calling Level 2 BLAS.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the triangular factor stored in the array A
*          is upper or lower triangular:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the triangular factor U or L.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the triangular factor U or L.
*          On exit, if UPLO = 'U', the upper triangle of A is
*          overwritten with the upper triangle of the product U * U';
*          if UPLO = 'L', the lower triangle of A is overwritten with
*          the lower triangle of the product L' * L.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I
      DOUBLE PRECISION   AII
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT
      EXTERNAL           LSAME, DDOT
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMV, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
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
         CALL XERBLA( 'DLAUU2', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( UPPER ) THEN
*
*        Compute the product U * U'.
*
         DO 10 I = 1, N
            AII = A( I, I )
            IF( I.LT.N ) THEN
               A( I, I ) = DDOT( N-I+1, A( I, I ), LDA, A( I, I ), LDA )
               CALL DGEMV( 'No transpose', I-1, N-I, ONE, A( 1, I+1 ),
     $                     LDA, A( I, I+1 ), LDA, AII, A( 1, I ), 1 )
            ELSE
               CALL DSCAL( I, AII, A( 1, I ), 1 )
            END IF
   10    CONTINUE
*
      ELSE
*
*        Compute the product L' * L.
*
         DO 20 I = 1, N
            AII = A( I, I )
            IF( I.LT.N ) THEN
               A( I, I ) = DDOT( N-I+1, A( I, I ), 1, A( I, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I-1, ONE, A( I+1, 1 ), LDA,
     $                     A( I+1, I ), 1, AII, A( I, 1 ), LDA )
            ELSE
               CALL DSCAL( I, AII, A( I, 1 ), LDA )
            END IF
   20    CONTINUE
      END IF
*
      RETURN
*
*     End of DLAUU2
*
      END
      SUBROUTINE DLAUUM( UPLO, N, A, LDA, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
*     ..
*
*  Purpose
*  =======
*
*  DLAUUM computes the product U * U' or L' * L, where the triangular
*  factor U or L is stored in the upper or lower triangular part of
*  the array A.
*
*  If UPLO = 'U' or 'u' then the upper triangle of the result is stored,
*  overwriting the factor U in A.
*  If UPLO = 'L' or 'l' then the lower triangle of the result is stored,
*  overwriting the factor L in A.
*
*  This is the blocked form of the algorithm, calling Level 3 BLAS.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the triangular factor stored in the array A
*          is upper or lower triangular:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The order of the triangular factor U or L.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the triangular factor U or L.
*          On exit, if UPLO = 'U', the upper triangle of A is
*          overwritten with the upper triangle of the product U * U';
*          if UPLO = 'L', the lower triangle of A is overwritten with
*          the lower triangle of the product L' * L.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -k, the k-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I, IB, NB
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DLAUU2, DSYRK, DTRMM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
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
         CALL XERBLA( 'DLAUUM', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Determine the block size for this environment.
*
      NB = ILAENV( 1, 'DLAUUM', UPLO, N, -1, -1, -1 )
*
      IF( NB.LE.1 .OR. NB.GE.N ) THEN
*
*        Use unblocked code
*
         CALL DLAUU2( UPLO, N, A, LDA, INFO )
      ELSE
*
*        Use blocked code
*
         IF( UPPER ) THEN
*
*           Compute the product U * U'.
*
            DO 10 I = 1, N, NB
               IB = MIN( NB, N-I+1 )
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Non-unit',
     $                     I-1, IB, ONE, A( I, I ), LDA, A( 1, I ),
     $                     LDA )
               CALL DLAUU2( 'Upper', IB, A( I, I ), LDA, INFO )
               IF( I+IB.LE.N ) THEN
                  CALL DGEMM( 'No transpose', 'Transpose', I-1, IB,
     $                        N-I-IB+1, ONE, A( 1, I+IB ), LDA,
     $                        A( I, I+IB ), LDA, ONE, A( 1, I ), LDA )
                  CALL DSYRK( 'Upper', 'No transpose', IB, N-I-IB+1,
     $                        ONE, A( I, I+IB ), LDA, ONE, A( I, I ),
     $                        LDA )
               END IF
   10       CONTINUE
         ELSE
*
*           Compute the product L' * L.
*
            DO 20 I = 1, N, NB
               IB = MIN( NB, N-I+1 )
               CALL DTRMM( 'Left', 'Lower', 'Transpose', 'Non-unit', IB,
     $                     I-1, ONE, A( I, I ), LDA, A( I, 1 ), LDA )
               CALL DLAUU2( 'Lower', IB, A( I, I ), LDA, INFO )
               IF( I+IB.LE.N ) THEN
                  CALL DGEMM( 'Transpose', 'No transpose', IB, I-1,
     $                        N-I-IB+1, ONE, A( I+IB, I ), LDA,
     $                        A( I+IB, 1 ), LDA, ONE, A( I, 1 ), LDA )
                  CALL DSYRK( 'Lower', 'Transpose', IB, N-I-IB+1, ONE,
     $                        A( I+IB, I ), LDA, ONE, A( I, I ), LDA )
               END IF
   20       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DLAUUM
*
      END
      SUBROUTINE DRSCL( N, SA, SX, INCX )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SA
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   SX( * )
*     ..
*
*  Purpose
*  =======
*
*  DRSCL multiplies an n-element real vector x by the real scalar 1/a.
*  This is done without overflow or underflow as long as
*  the final result x/a does not overflow or underflow.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of components of the vector x.
*
*  SA      (input) DOUBLE PRECISION
*          The scalar a which is used to divide each component of x.
*          SA must be >= 0, or the subroutine will divide by zero.
*
*  SX      (input/output) DOUBLE PRECISION array, dimension
*                         (1+(N-1)*abs(INCX))
*          The n-element vector x.
*
*  INCX    (input) INTEGER
*          The increment between successive values of the vector SX.
*          > 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            DONE
      DOUBLE PRECISION   BIGNUM, CDEN, CDEN1, CNUM, CNUM1, MUL, SMLNUM
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSCAL
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
*     Quick return if possible
*
      IF( N.LE.0 )
     $   RETURN
*
*     Get machine parameters
*
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Initialize the denominator to SA and the numerator to 1.
*
      CDEN = SA
      CNUM = ONE
*
   10 CONTINUE
      CDEN1 = CDEN*SMLNUM
      CNUM1 = CNUM / BIGNUM
      IF( ABS( CDEN1 ).GT.ABS( CNUM ) .AND. CNUM.NE.ZERO ) THEN
*
*        Pre-multiply X by SMLNUM if CDEN is large compared to CNUM.
*
         MUL = SMLNUM
         DONE = .FALSE.
         CDEN = CDEN1
      ELSE IF( ABS( CNUM1 ).GT.ABS( CDEN ) ) THEN
*
*        Pre-multiply X by BIGNUM if CDEN is small compared to CNUM.
*
         MUL = BIGNUM
         DONE = .FALSE.
         CNUM = CNUM1
      ELSE
*
*        Multiply X by CNUM / CDEN and return.
*
         MUL = CNUM / CDEN
         DONE = .TRUE.
      END IF
*
*     Scale the vector X by MUL
*
      CALL DSCAL( N, MUL, SX, INCX )
*
      IF( .NOT.DONE )
     $   GO TO 10
*
      RETURN
*
*     End of DRSCL
*
      END
      SUBROUTINE DTGEX2( WANTQ, WANTZ, N, A, LDA, B, LDB, Q, LDQ, Z,
     $                   LDZ, J1, N1, N2, WORK, LWORK, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      LOGICAL            WANTQ, WANTZ
      INTEGER            INFO, J1, LDA, LDB, LDQ, LDZ, LWORK, N, N1, N2
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), Q( LDQ, * ),
     $                   WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DTGEX2 swaps adjacent diagonal blocks (A11, B11) and (A22, B22)
*  of size 1-by-1 or 2-by-2 in an upper (quasi) triangular matrix pair
*  (A, B) by an orthogonal equivalence transformation.
*
*  (A, B) must be in generalized real Schur canonical form (as returned
*  by DGGES), i.e. A is block upper triangular with 1-by-1 and 2-by-2
*  diagonal blocks. B is upper triangular.
*
*  Optionally, the matrices Q and Z of generalized Schur vectors are
*  updated.
*
*         Q(in) * A(in) * Z(in)' = Q(out) * A(out) * Z(out)'
*         Q(in) * B(in) * Z(in)' = Q(out) * B(out) * Z(out)'
*
*
*  Arguments
*  =========
*
*  WANTQ   (input) LOGICAL
*          .TRUE. : update the left transformation matrix Q;
*          .FALSE.: do not update Q.
*
*  WANTZ   (input) LOGICAL
*          .TRUE. : update the right transformation matrix Z;
*          .FALSE.: do not update Z.
*
*  N       (input) INTEGER
*          The order of the matrices A and B. N >= 0.
*
*  A      (input/output) DOUBLE PRECISION arrays, dimensions (LDA,N)
*          On entry, the matrix A in the pair (A, B).
*          On exit, the updated matrix A.
*
*  LDA     (input)  INTEGER
*          The leading dimension of the array A. LDA >= max(1,N).
*
*  B      (input/output) DOUBLE PRECISION arrays, dimensions (LDB,N)
*          On entry, the matrix B in the pair (A, B).
*          On exit, the updated matrix B.
*
*  LDB     (input)  INTEGER
*          The leading dimension of the array B. LDB >= max(1,N).
*
*  Q       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
*          On entry, if WANTQ = .TRUE., the orthogonal matrix Q.
*          On exit, the updated matrix Q.
*          Not referenced if WANTQ = .FALSE..
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q. LDQ >= 1.
*          If WANTQ = .TRUE., LDQ >= N.
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
*          On entry, if WANTZ =.TRUE., the orthogonal matrix Z.
*          On exit, the updated matrix Z.
*          Not referenced if WANTZ = .FALSE..
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z. LDZ >= 1.
*          If WANTZ = .TRUE., LDZ >= N.
*
*  J1      (input) INTEGER
*          The index to the first block (A11, B11). 1 <= J1 <= N.
*
*  N1      (input) INTEGER
*          The order of the first block (A11, B11). N1 = 0, 1 or 2.
*
*  N2      (input) INTEGER
*          The order of the second block (A22, B22). N2 = 0, 1 or 2.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)).
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          LWORK >=  MAX( 1, N*(N2+N1), (N2+N1)*(N2+N1)*2 )
*
*  INFO    (output) INTEGER
*            =0: Successful exit
*            >0: If INFO = 1, the transformed matrix (A, B) would be
*                too far from generalized Schur form; the blocks are
*                not swapped and (A, B) and (Q, Z) are unchanged.
*                The problem of swapping is too ill-conditioned.
*            <0: If INFO = -16: LWORK is too small. Appropriate value
*                for LWORK is returned in WORK(1).
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
*     Umea University, S-901 87 Umea, Sweden.
*
*  In the current code both weak and strong stability tests are
*  performed. The user can omit the strong stability test by changing
*  the internal logical parameter WANDS to .FALSE.. See ref. [2] for
*  details.
*
*  [1] B. Kagstrom; A Direct Method for Reordering Eigenvalues in the
*      Generalized Real Schur Form of a Regular Matrix Pair (A, B), in
*      M.S. Moonen et al (eds), Linear Algebra for Large Scale and
*      Real-Time Applications, Kluwer Academic Publ. 1993, pp 195-218.
*
*  [2] B. Kagstrom and P. Poromaa; Computing Eigenspaces with Specified
*      Eigenvalues of a Regular Matrix Pair (A, B) and Condition
*      Estimation: Theory, Algorithms and Software,
*      Report UMINF - 94.04, Department of Computing Science, Umea
*      University, S-901 87 Umea, Sweden, 1994. Also as LAPACK Working
*      Note 87. To appear in Numerical Algorithms, 1996.
*
*  =====================================================================
*  Replaced various illegal calls to DCOPY by calls to DLASET, or by DO
*  loops. Sven Hammarling, 1/5/02.
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   TEN
      PARAMETER          ( TEN = 1.0D+01 )
      INTEGER            LDST
      PARAMETER          ( LDST = 4 )
      LOGICAL            WANDS
      PARAMETER          ( WANDS = .TRUE. )
*     ..
*     .. Local Scalars ..
      LOGICAL            DTRONG, WEAK
      INTEGER            I, IDUM, LINFO, M
      DOUBLE PRECISION   BQRA21, BRQA21, DDUM, DNORM, DSCALE, DSUM, EPS,
     $                   F, G, SA, SB, SCALE, SMLNUM, SS, THRESH, WS
*     ..
*     .. Local Arrays ..
      INTEGER            IWORK( LDST )
      DOUBLE PRECISION   AI( 2 ), AR( 2 ), BE( 2 ), IR( LDST, LDST ),
     $                   IRCOP( LDST, LDST ), LI( LDST, LDST ),
     $                   LICOP( LDST, LDST ), S( LDST, LDST ),
     $                   SCPY( LDST, LDST ), T( LDST, LDST ),
     $                   TAUL( LDST ), TAUR( LDST ), TCPY( LDST, LDST )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DGEQR2, DGERQ2, DLACPY, DLAGV2, DLARTG,
     $                   DLASET, DLASSQ, DORG2R, DORGR2, DORM2R, DORMR2,
     $                   DROT, DSCAL, DTGSY2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
*     Quick return if possible
*
      IF( N.LE.1 .OR. N1.LE.0 .OR. N2.LE.0 )
     $   RETURN
      IF( N1.GT.N .OR. ( J1+N1 ).GT.N )
     $   RETURN
      M = N1 + N2
      IF( LWORK.LT.MAX( 1, N*M, M*M*2 ) ) THEN
         INFO = -16
         WORK( 1 ) = MAX( 1, N*M, M*M*2 )
         RETURN
      END IF
*
      WEAK = .FALSE.
      DTRONG = .FALSE.
*
*     Make a local copy of selected block
*
      CALL DLASET( 'Full', LDST, LDST, ZERO, ZERO, LI, LDST )
      CALL DLASET( 'Full', LDST, LDST, ZERO, ZERO, IR, LDST )
      CALL DLACPY( 'Full', M, M, A( J1, J1 ), LDA, S, LDST )
      CALL DLACPY( 'Full', M, M, B( J1, J1 ), LDB, T, LDST )
*
*     Compute threshold for testing acceptance of swapping.
*
      EPS = DLAMCH( 'P' )
      SMLNUM = DLAMCH( 'S' ) / EPS
      DSCALE = ZERO
      DSUM = ONE
      CALL DLACPY( 'Full', M, M, S, LDST, WORK, M )
      CALL DLASSQ( M*M, WORK, 1, DSCALE, DSUM )
      CALL DLACPY( 'Full', M, M, T, LDST, WORK, M )
      CALL DLASSQ( M*M, WORK, 1, DSCALE, DSUM )
      DNORM = DSCALE*SQRT( DSUM )
      THRESH = MAX( TEN*EPS*DNORM, SMLNUM )
*
      IF( M.EQ.2 ) THEN
*
*        CASE 1: Swap 1-by-1 and 1-by-1 blocks.
*
*        Compute orthogonal QL and RQ that swap 1-by-1 and 1-by-1 blocks
*        using Givens rotations and perform the swap tentatively.
*
         F = S( 2, 2 )*T( 1, 1 ) - T( 2, 2 )*S( 1, 1 )
         G = S( 2, 2 )*T( 1, 2 ) - T( 2, 2 )*S( 1, 2 )
         SB = ABS( T( 2, 2 ) )
         SA = ABS( S( 2, 2 ) )
         CALL DLARTG( F, G, IR( 1, 2 ), IR( 1, 1 ), DDUM )
         IR( 2, 1 ) = -IR( 1, 2 )
         IR( 2, 2 ) = IR( 1, 1 )
         CALL DROT( 2, S( 1, 1 ), 1, S( 1, 2 ), 1, IR( 1, 1 ),
     $              IR( 2, 1 ) )
         CALL DROT( 2, T( 1, 1 ), 1, T( 1, 2 ), 1, IR( 1, 1 ),
     $              IR( 2, 1 ) )
         IF( SA.GE.SB ) THEN
            CALL DLARTG( S( 1, 1 ), S( 2, 1 ), LI( 1, 1 ), LI( 2, 1 ),
     $                   DDUM )
         ELSE
            CALL DLARTG( T( 1, 1 ), T( 2, 1 ), LI( 1, 1 ), LI( 2, 1 ),
     $                   DDUM )
         END IF
         CALL DROT( 2, S( 1, 1 ), LDST, S( 2, 1 ), LDST, LI( 1, 1 ),
     $              LI( 2, 1 ) )
         CALL DROT( 2, T( 1, 1 ), LDST, T( 2, 1 ), LDST, LI( 1, 1 ),
     $              LI( 2, 1 ) )
         LI( 2, 2 ) = LI( 1, 1 )
         LI( 1, 2 ) = -LI( 2, 1 )
*
*        Weak stability test:
*           |S21| + |T21| <= O(EPS * F-norm((S, T)))
*
         WS = ABS( S( 2, 1 ) ) + ABS( T( 2, 1 ) )
         WEAK = WS.LE.THRESH
         IF( .NOT.WEAK )
     $      GO TO 70
*
         IF( WANDS ) THEN
*
*           Strong stability test:
*             F-norm((A-QL'*S*QR, B-QL'*T*QR)) <= O(EPS*F-norm((A,B)))
*
            CALL DLACPY( 'Full', M, M, A( J1, J1 ), LDA, WORK( M*M+1 ),
     $                   M )
            CALL DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, S, LDST, ZERO,
     $                  WORK, M )
            CALL DGEMM( 'N', 'T', M, M, M, -ONE, WORK, M, IR, LDST, ONE,
     $                  WORK( M*M+1 ), M )
            DSCALE = ZERO
            DSUM = ONE
            CALL DLASSQ( M*M, WORK( M*M+1 ), 1, DSCALE, DSUM )
*
            CALL DLACPY( 'Full', M, M, B( J1, J1 ), LDB, WORK( M*M+1 ),
     $                   M )
            CALL DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, T, LDST, ZERO,
     $                  WORK, M )
            CALL DGEMM( 'N', 'T', M, M, M, -ONE, WORK, M, IR, LDST, ONE,
     $                  WORK( M*M+1 ), M )
            CALL DLASSQ( M*M, WORK( M*M+1 ), 1, DSCALE, DSUM )
            SS = DSCALE*SQRT( DSUM )
            DTRONG = SS.LE.THRESH
            IF( .NOT.DTRONG )
     $         GO TO 70
         END IF
*
*        Update (A(J1:J1+M-1, M+J1:N), B(J1:J1+M-1, M+J1:N)) and
*               (A(1:J1-1, J1:J1+M), B(1:J1-1, J1:J1+M)).
*
         CALL DROT( J1+1, A( 1, J1 ), 1, A( 1, J1+1 ), 1, IR( 1, 1 ),
     $              IR( 2, 1 ) )
         CALL DROT( J1+1, B( 1, J1 ), 1, B( 1, J1+1 ), 1, IR( 1, 1 ),
     $              IR( 2, 1 ) )
         CALL DROT( N-J1+1, A( J1, J1 ), LDA, A( J1+1, J1 ), LDA,
     $              LI( 1, 1 ), LI( 2, 1 ) )
         CALL DROT( N-J1+1, B( J1, J1 ), LDB, B( J1+1, J1 ), LDB,
     $              LI( 1, 1 ), LI( 2, 1 ) )
*
*        Set  N1-by-N2 (2,1) - blocks to ZERO.
*
         A( J1+1, J1 ) = ZERO
         B( J1+1, J1 ) = ZERO
*
*        Accumulate transformations into Q and Z if requested.
*
         IF( WANTZ )
     $      CALL DROT( N, Z( 1, J1 ), 1, Z( 1, J1+1 ), 1, IR( 1, 1 ),
     $                 IR( 2, 1 ) )
         IF( WANTQ )
     $      CALL DROT( N, Q( 1, J1 ), 1, Q( 1, J1+1 ), 1, LI( 1, 1 ),
     $                 LI( 2, 1 ) )
*
*        Exit with INFO = 0 if swap was successfully performed.
*
         RETURN
*
      ELSE
*
*        CASE 2: Swap 1-by-1 and 2-by-2 blocks, or 2-by-2
*                and 2-by-2 blocks.
*
*        Solve the generalized Sylvester equation
*                 S11 * R - L * S22 = SCALE * S12
*                 T11 * R - L * T22 = SCALE * T12
*        for R and L. Solutions in LI and IR.
*
         CALL DLACPY( 'Full', N1, N2, T( 1, N1+1 ), LDST, LI, LDST )
         CALL DLACPY( 'Full', N1, N2, S( 1, N1+1 ), LDST,
     $                IR( N2+1, N1+1 ), LDST )
         CALL DTGSY2( 'N', 0, N1, N2, S, LDST, S( N1+1, N1+1 ), LDST,
     $                IR( N2+1, N1+1 ), LDST, T, LDST, T( N1+1, N1+1 ),
     $                LDST, LI, LDST, SCALE, DSUM, DSCALE, IWORK, IDUM,
     $                LINFO )
*
*        Compute orthogonal matrix QL:
*
*                    QL' * LI = [ TL ]
*                               [ 0  ]
*        where
*                    LI =  [      -L              ]
*                          [ SCALE * identity(N2) ]
*
         DO 10 I = 1, N2
            CALL DSCAL( N1, -ONE, LI( 1, I ), 1 )
            LI( N1+I, I ) = SCALE
   10    CONTINUE
         CALL DGEQR2( M, N2, LI, LDST, TAUL, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL DORG2R( M, M, N2, LI, LDST, TAUL, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
*
*        Compute orthogonal matrix RQ:
*
*                    IR * RQ' =   [ 0  TR],
*
*         where IR = [ SCALE * identity(N1), R ]
*
         DO 20 I = 1, N1
            IR( N2+I, I ) = SCALE
   20    CONTINUE
         CALL DGERQ2( N1, M, IR( N2+1, 1 ), LDST, TAUR, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL DORGR2( M, M, N1, IR, LDST, TAUR, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
*
*        Perform the swapping tentatively:
*
         CALL DGEMM( 'T', 'N', M, M, M, ONE, LI, LDST, S, LDST, ZERO,
     $               WORK, M )
         CALL DGEMM( 'N', 'T', M, M, M, ONE, WORK, M, IR, LDST, ZERO, S,
     $               LDST )
         CALL DGEMM( 'T', 'N', M, M, M, ONE, LI, LDST, T, LDST, ZERO,
     $               WORK, M )
         CALL DGEMM( 'N', 'T', M, M, M, ONE, WORK, M, IR, LDST, ZERO, T,
     $               LDST )
         CALL DLACPY( 'F', M, M, S, LDST, SCPY, LDST )
         CALL DLACPY( 'F', M, M, T, LDST, TCPY, LDST )
         CALL DLACPY( 'F', M, M, IR, LDST, IRCOP, LDST )
         CALL DLACPY( 'F', M, M, LI, LDST, LICOP, LDST )
*
*        Triangularize the B-part by an RQ factorization.
*        Apply transformation (from left) to A-part, giving S.
*
         CALL DGERQ2( M, M, T, LDST, TAUR, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL DORMR2( 'R', 'T', M, M, M, T, LDST, TAUR, S, LDST, WORK,
     $                LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL DORMR2( 'L', 'N', M, M, M, T, LDST, TAUR, IR, LDST, WORK,
     $                LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
*
*        Compute F-norm(S21) in BRQA21. (T21 is 0.)
*
         DSCALE = ZERO
         DSUM = ONE
         DO 30 I = 1, N2
            CALL DLASSQ( N1, S( N2+1, I ), 1, DSCALE, DSUM )
   30    CONTINUE
         BRQA21 = DSCALE*SQRT( DSUM )
*
*        Triangularize the B-part by a QR factorization.
*        Apply transformation (from right) to A-part, giving S.
*
         CALL DGEQR2( M, M, TCPY, LDST, TAUL, WORK, LINFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
         CALL DORM2R( 'L', 'T', M, M, M, TCPY, LDST, TAUL, SCPY, LDST,
     $                WORK, INFO )
         CALL DORM2R( 'R', 'N', M, M, M, TCPY, LDST, TAUL, LICOP, LDST,
     $                WORK, INFO )
         IF( LINFO.NE.0 )
     $      GO TO 70
*
*        Compute F-norm(S21) in BQRA21. (T21 is 0.)
*
         DSCALE = ZERO
         DSUM = ONE
         DO 40 I = 1, N2
            CALL DLASSQ( N1, SCPY( N2+1, I ), 1, DSCALE, DSUM )
   40    CONTINUE
         BQRA21 = DSCALE*SQRT( DSUM )
*
*        Decide which method to use.
*          Weak stability test:
*             F-norm(S21) <= O(EPS * F-norm((S, T)))
*
         IF( BQRA21.LE.BRQA21 .AND. BQRA21.LE.THRESH ) THEN
            CALL DLACPY( 'F', M, M, SCPY, LDST, S, LDST )
            CALL DLACPY( 'F', M, M, TCPY, LDST, T, LDST )
            CALL DLACPY( 'F', M, M, IRCOP, LDST, IR, LDST )
            CALL DLACPY( 'F', M, M, LICOP, LDST, LI, LDST )
         ELSE IF( BRQA21.GE.THRESH ) THEN
            GO TO 70
         END IF
*
*        Set lower triangle of B-part to zero
*
         CALL DLASET( 'Lower', M-1, M-1, ZERO, ZERO, T(2,1), LDST )
*
         IF( WANDS ) THEN
*
*           Strong stability test:
*              F-norm((A-QL*S*QR', B-QL*T*QR')) <= O(EPS*F-norm((A,B)))
*
            CALL DLACPY( 'Full', M, M, A( J1, J1 ), LDA, WORK( M*M+1 ),
     $                   M )
            CALL DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, S, LDST, ZERO,
     $                  WORK, M )
            CALL DGEMM( 'N', 'N', M, M, M, -ONE, WORK, M, IR, LDST, ONE,
     $                  WORK( M*M+1 ), M )
            DSCALE = ZERO
            DSUM = ONE
            CALL DLASSQ( M*M, WORK( M*M+1 ), 1, DSCALE, DSUM )
*
            CALL DLACPY( 'Full', M, M, B( J1, J1 ), LDB, WORK( M*M+1 ),
     $                   M )
            CALL DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, T, LDST, ZERO,
     $                  WORK, M )
            CALL DGEMM( 'N', 'N', M, M, M, -ONE, WORK, M, IR, LDST, ONE,
     $                  WORK( M*M+1 ), M )
            CALL DLASSQ( M*M, WORK( M*M+1 ), 1, DSCALE, DSUM )
            SS = DSCALE*SQRT( DSUM )
            DTRONG = ( SS.LE.THRESH )
            IF( .NOT.DTRONG )
     $         GO TO 70
*
         END IF
*
*        If the swap is accepted ("weakly" and "strongly"), apply the
*        transformations and set N1-by-N2 (2,1)-block to zero.
*
         CALL DLASET( 'Full', N1, N2, ZERO, ZERO, S(N2+1,1), LDST )
*
*        copy back M-by-M diagonal block starting at index J1 of (A, B)
*
         CALL DLACPY( 'F', M, M, S, LDST, A( J1, J1 ), LDA )
         CALL DLACPY( 'F', M, M, T, LDST, B( J1, J1 ), LDB )
         CALL DLASET( 'Full', LDST, LDST, ZERO, ZERO, T, LDST )
*
*        Standardize existing 2-by-2 blocks.
*
         DO 50 I = 1, M*M
            WORK(I) = ZERO
   50    CONTINUE
         WORK( 1 ) = ONE
         T( 1, 1 ) = ONE
         IDUM = LWORK - M*M - 2
         IF( N2.GT.1 ) THEN
            CALL DLAGV2( A( J1, J1 ), LDA, B( J1, J1 ), LDB, AR, AI, BE,
     $                   WORK( 1 ), WORK( 2 ), T( 1, 1 ), T( 2, 1 ) )
            WORK( M+1 ) = -WORK( 2 )
            WORK( M+2 ) = WORK( 1 )
            T( N2, N2 ) = T( 1, 1 )
            T( 1, 2 ) = -T( 2, 1 )
         END IF
         WORK( M*M ) = ONE
         T( M, M ) = ONE
*
         IF( N1.GT.1 ) THEN
            CALL DLAGV2( A( J1+N2, J1+N2 ), LDA, B( J1+N2, J1+N2 ), LDB,
     $                   TAUR, TAUL, WORK( M*M+1 ), WORK( N2*M+N2+1 ),
     $                   WORK( N2*M+N2+2 ), T( N2+1, N2+1 ),
     $                   T( M, M-1 ) )
            WORK( M*M ) = WORK( N2*M+N2+1 )
            WORK( M*M-1 ) = -WORK( N2*M+N2+2 )
            T( M, M ) = T( N2+1, N2+1 )
            T( M-1, M ) = -T( M, M-1 )
         END IF
         CALL DGEMM( 'T', 'N', N2, N1, N2, ONE, WORK, M, A( J1, J1+N2 ),
     $               LDA, ZERO, WORK( M*M+1 ), N2 )
         CALL DLACPY( 'Full', N2, N1, WORK( M*M+1 ), N2, A( J1, J1+N2 ),
     $                LDA )
         CALL DGEMM( 'T', 'N', N2, N1, N2, ONE, WORK, M, B( J1, J1+N2 ),
     $               LDB, ZERO, WORK( M*M+1 ), N2 )
         CALL DLACPY( 'Full', N2, N1, WORK( M*M+1 ), N2, B( J1, J1+N2 ),
     $                LDB )
         CALL DGEMM( 'N', 'N', M, M, M, ONE, LI, LDST, WORK, M, ZERO,
     $               WORK( M*M+1 ), M )
         CALL DLACPY( 'Full', M, M, WORK( M*M+1 ), M, LI, LDST )
         CALL DGEMM( 'N', 'N', N2, N1, N1, ONE, A( J1, J1+N2 ), LDA,
     $               T( N2+1, N2+1 ), LDST, ZERO, WORK, N2 )
         CALL DLACPY( 'Full', N2, N1, WORK, N2, A( J1, J1+N2 ), LDA )
         CALL DGEMM( 'N', 'N', N2, N1, N1, ONE, B( J1, J1+N2 ), LDB,
     $               T( N2+1, N2+1 ), LDST, ZERO, WORK, N2 )
         CALL DLACPY( 'Full', N2, N1, WORK, N2, B( J1, J1+N2 ), LDB )
         CALL DGEMM( 'T', 'N', M, M, M, ONE, IR, LDST, T, LDST, ZERO,
     $               WORK, M )
         CALL DLACPY( 'Full', M, M, WORK, M, IR, LDST )
*
*        Accumulate transformations into Q and Z if requested.
*
         IF( WANTQ ) THEN
            CALL DGEMM( 'N', 'N', N, M, M, ONE, Q( 1, J1 ), LDQ, LI,
     $                  LDST, ZERO, WORK, N )
            CALL DLACPY( 'Full', N, M, WORK, N, Q( 1, J1 ), LDQ )
*
         END IF
*
         IF( WANTZ ) THEN
            CALL DGEMM( 'N', 'N', N, M, M, ONE, Z( 1, J1 ), LDZ, IR,
     $                  LDST, ZERO, WORK, N )
            CALL DLACPY( 'Full', N, M, WORK, N, Z( 1, J1 ), LDZ )
*
         END IF
*
*        Update (A(J1:J1+M-1, M+J1:N), B(J1:J1+M-1, M+J1:N)) and
*                (A(1:J1-1, J1:J1+M), B(1:J1-1, J1:J1+M)).
*
         I = J1 + M
         IF( I.LE.N ) THEN
            CALL DGEMM( 'T', 'N', M, N-I+1, M, ONE, LI, LDST,
     $                  A( J1, I ), LDA, ZERO, WORK, M )
            CALL DLACPY( 'Full', M, N-I+1, WORK, M, A( J1, I ), LDA )
            CALL DGEMM( 'T', 'N', M, N-I+1, M, ONE, LI, LDST,
     $                  B( J1, I ), LDA, ZERO, WORK, M )
            CALL DLACPY( 'Full', M, N-I+1, WORK, M, B( J1, I ), LDB )
         END IF
         I = J1 - 1
         IF( I.GT.0 ) THEN
            CALL DGEMM( 'N', 'N', I, M, M, ONE, A( 1, J1 ), LDA, IR,
     $                  LDST, ZERO, WORK, I )
            CALL DLACPY( 'Full', I, M, WORK, I, A( 1, J1 ), LDA )
            CALL DGEMM( 'N', 'N', I, M, M, ONE, B( 1, J1 ), LDB, IR,
     $                  LDST, ZERO, WORK, I )
            CALL DLACPY( 'Full', I, M, WORK, I, B( 1, J1 ), LDB )
         END IF
*
*        Exit with INFO = 0 if swap was successfully performed.
*
         RETURN
*
      END IF
*
*     Exit with INFO = 1 if swap was rejected.
*
   70 CONTINUE
*
      INFO = 1
      RETURN
*
*     End of DTGEX2
*
      END
      SUBROUTINE DTGSY2( TRANS, IJOB, M, N, A, LDA, B, LDB, C, LDC, D,
     $                   LDD, E, LDE, F, LDF, SCALE, RDSUM, RDSCAL,
     $                   IWORK, PQ, INFO )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            IJOB, INFO, LDA, LDB, LDC, LDD, LDE, LDF, M, N,
     $                   PQ
      DOUBLE PRECISION   RDSCAL, RDSUM, SCALE
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * ),
     $                   D( LDD, * ), E( LDE, * ), F( LDF, * )
*     ..
*
*  Purpose
*  =======
*
*  DTGSY2 solves the generalized Sylvester equation:
*
*              A * R - L * B = scale * C                (1)
*              D * R - L * E = scale * F,
*
*  using Level 1 and 2 BLAS. where R and L are unknown M-by-N matrices,
*  (A, D), (B, E) and (C, F) are given matrix pairs of size M-by-M,
*  N-by-N and M-by-N, respectively, with real entries. (A, D) and (B, E)
*  must be in generalized Schur canonical form, i.e. A, B are upper
*  quasi triangular and D, E are upper triangular. The solution (R, L)
*  overwrites (C, F). 0 <= SCALE <= 1 is an output scaling factor
*  chosen to avoid overflow.
*
*  In matrix notation solving equation (1) corresponds to solve
*  Z*x = scale*b, where Z is defined as
*
*         Z = [ kron(In, A)  -kron(B', Im) ]             (2)
*             [ kron(In, D)  -kron(E', Im) ],
*
*  Ik is the identity matrix of size k and X' is the transpose of X.
*  kron(X, Y) is the Kronecker product between the matrices X and Y.
*  In the process of solving (1), we solve a number of such systems
*  where Dim(In), Dim(In) = 1 or 2.
*
*  If TRANS = 'T', solve the transposed system Z'*y = scale*b for y,
*  which is equivalent to solve for R and L in
*
*              A' * R  + D' * L   = scale *  C           (3)
*              R  * B' + L  * E'  = scale * -F
*
*  This case is used to compute an estimate of Dif[(A, D), (B, E)] =
*  sigma_min(Z) using reverse communicaton with DLACON.
*
*  DTGSY2 also (IJOB >= 1) contributes to the computation in STGSYL
*  of an upper bound on the separation between to matrix pairs. Then
*  the input (A, D), (B, E) are sub-pencils of the matrix pair in
*  DTGSYL. See STGSYL for details.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          = 'N', solve the generalized Sylvester equation (1).
*          = 'T': solve the 'transposed' system (3).
*
*  IJOB    (input) INTEGER
*          Specifies what kind of functionality to be performed.
*          = 0: solve (1) only.
*          = 1: A contribution from this subsystem to a Frobenius
*               norm-based estimate of the separation between two matrix
*               pairs is computed. (look ahead strategy is used).
*          = 2: A contribution from this subsystem to a Frobenius
*               norm-based estimate of the separation between two matrix
*               pairs is computed. (DGECON on sub-systems is used.)
*          Not referenced if TRANS = 'T'.
*
*  M       (input) INTEGER
*          On entry, M specifies the order of A and D, and the row
*          dimension of C, F, R and L.
*
*  N       (input) INTEGER
*          On entry, N specifies the order of B and E, and the column
*          dimension of C, F, R and L.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA, M)
*          On entry, A contains an upper quasi triangular matrix.
*
*  LDA     (input) INTEGER
*          The leading dimension of the matrix A. LDA >= max(1, M).
*
*  B       (input) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, B contains an upper quasi triangular matrix.
*
*  LDB     (input) INTEGER
*          The leading dimension of the matrix B. LDB >= max(1, N).
*
*  C       (input/output) DOUBLE PRECISION array, dimension (LDC, N)
*          On entry, C contains the right-hand-side of the first matrix
*          equation in (1).
*          On exit, if IJOB = 0, C has been overwritten by the
*          solution R.
*
*  LDC     (input) INTEGER
*          The leading dimension of the matrix C. LDC >= max(1, M).
*
*  D       (input) DOUBLE PRECISION array, dimension (LDD, M)
*          On entry, D contains an upper triangular matrix.
*
*  LDD     (input) INTEGER
*          The leading dimension of the matrix D. LDD >= max(1, M).
*
*  E       (input) DOUBLE PRECISION array, dimension (LDE, N)
*          On entry, E contains an upper triangular matrix.
*
*  LDE     (input) INTEGER
*          The leading dimension of the matrix E. LDE >= max(1, N).
*
*  F       (input/output) DOUBLE PRECISION array, dimension (LDF, N)
*          On entry, F contains the right-hand-side of the second matrix
*          equation in (1).
*          On exit, if IJOB = 0, F has been overwritten by the
*          solution L.
*
*  LDF     (input) INTEGER
*          The leading dimension of the matrix F. LDF >= max(1, M).
*
*  SCALE   (output) DOUBLE PRECISION
*          On exit, 0 <= SCALE <= 1. If 0 < SCALE < 1, the solutions
*          R and L (C and F on entry) will hold the solutions to a
*          slightly perturbed system but the input matrices A, B, D and
*          E have not been changed. If SCALE = 0, R and L will hold the
*          solutions to the homogeneous system with C = F = 0. Normally,
*          SCALE = 1.
*
*  RDSUM   (input/output) DOUBLE PRECISION
*          On entry, the sum of squares of computed contributions to
*          the Dif-estimate under computation by DTGSYL, where the
*          scaling factor RDSCAL (see below) has been factored out.
*          On exit, the corresponding sum of squares updated with the
*          contributions from the current sub-system.
*          If TRANS = 'T' RDSUM is not touched.
*          NOTE: RDSUM only makes sense when DTGSY2 is called by STGSYL.
*
*  RDSCAL  (input/output) DOUBLE PRECISION
*          On entry, scaling factor used to prevent overflow in RDSUM.
*          On exit, RDSCAL is updated w.r.t. the current contributions
*          in RDSUM.
*          If TRANS = 'T', RDSCAL is not touched.
*          NOTE: RDSCAL only makes sense when DTGSY2 is called by
*                DTGSYL.
*
*  IWORK   (workspace) INTEGER array, dimension (M+N+2)
*
*  PQ      (output) INTEGER
*          On exit, the number of subsystems (of size 2-by-2, 4-by-4 and
*          8-by-8) solved by this routine.
*
*  INFO    (output) INTEGER
*          On exit, if INFO is set to
*            =0: Successful exit
*            <0: If INFO = -i, the i-th argument had an illegal value.
*            >0: The matrix pairs (A, D) and (B, E) have common or very
*                close eigenvalues.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Bo Kagstrom and Peter Poromaa, Department of Computing Science,
*     Umea University, S-901 87 Umea, Sweden.
*
*  =====================================================================
*  Replaced various illegal calls to DCOPY by calls to DLASET.
*  Sven Hammarling, 27/5/02.
*
*     .. Parameters ..
      INTEGER            LDZ
      PARAMETER          ( LDZ = 8 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOTRAN
      INTEGER            I, IE, IERR, II, IS, ISP1, J, JE, JJ, JS, JSP1,
     $                   K, MB, NB, P, Q, ZDIM
      DOUBLE PRECISION   ALPHA, SCALOC
*     ..
*     .. Local Arrays ..
      INTEGER            IPIV( LDZ ), JPIV( LDZ )
      DOUBLE PRECISION   RHS( LDZ ), Z( LDZ, LDZ )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMM, DGEMV, DGER, DGESC2,
     $                   DGETC2, DLASET, DLATDF, DSCAL, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Decode and test input parameters
*
      INFO = 0
      IERR = 0
      NOTRAN = LSAME( TRANS, 'N' )
      IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -1
      ELSE IF( NOTRAN ) THEN
         IF( ( IJOB.LT.0 ) .OR. ( IJOB.GT.2 ) ) THEN
            INFO = -2
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( M.LE.0 ) THEN
            INFO = -3
         ELSE IF( N.LE.0 ) THEN
            INFO = -4
         ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
            INFO = -5
         ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
            INFO = -8
         ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
            INFO = -10
         ELSE IF( LDD.LT.MAX( 1, M ) ) THEN
            INFO = -12
         ELSE IF( LDE.LT.MAX( 1, N ) ) THEN
            INFO = -14
         ELSE IF( LDF.LT.MAX( 1, M ) ) THEN
            INFO = -16
         END IF
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DTGSY2', -INFO )
         RETURN
      END IF
*
*     Determine block structure of A
*
      PQ = 0
      P = 0
      I = 1
   10 CONTINUE
      IF( I.GT.M )
     $   GO TO 20
      P = P + 1
      IWORK( P ) = I
      IF( I.EQ.M )
     $   GO TO 20
      IF( A( I+1, I ).NE.ZERO ) THEN
         I = I + 2
      ELSE
         I = I + 1
      END IF
      GO TO 10
   20 CONTINUE
      IWORK( P+1 ) = M + 1
*
*     Determine block structure of B
*
      Q = P + 1
      J = 1
   30 CONTINUE
      IF( J.GT.N )
     $   GO TO 40
      Q = Q + 1
      IWORK( Q ) = J
      IF( J.EQ.N )
     $   GO TO 40
      IF( B( J+1, J ).NE.ZERO ) THEN
         J = J + 2
      ELSE
         J = J + 1
      END IF
      GO TO 30
   40 CONTINUE
      IWORK( Q+1 ) = N + 1
      PQ = P*( Q-P-1 )
*
      IF( NOTRAN ) THEN
*
*        Solve (I, J) - subsystem
*           A(I, I) * R(I, J) - L(I, J) * B(J, J) = C(I, J)
*           D(I, I) * R(I, J) - L(I, J) * E(J, J) = F(I, J)
*        for I = P, P - 1, ..., 1; J = 1, 2, ..., Q
*
         SCALE = ONE
         SCALOC = ONE
         DO 120 J = P + 2, Q
            JS = IWORK( J )
            JSP1 = JS + 1
            JE = IWORK( J+1 ) - 1
            NB = JE - JS + 1
            DO 110 I = P, 1, -1
*
               IS = IWORK( I )
               ISP1 = IS + 1
               IE = IWORK( I+1 ) - 1
               MB = IE - IS + 1
               ZDIM = MB*NB*2
*
               IF( ( MB.EQ.1 ) .AND. ( NB.EQ.1 ) ) THEN
*
*                 Build a 2-by-2 system Z * x = RHS
*
                  Z( 1, 1 ) = A( IS, IS )
                  Z( 2, 1 ) = D( IS, IS )
                  Z( 1, 2 ) = -B( JS, JS )
                  Z( 2, 2 ) = -E( JS, JS )
*
*                 Set up right hand side(s)
*
                  RHS( 1 ) = C( IS, JS )
                  RHS( 2 ) = F( IS, JS )
*
*                 Solve Z * x = RHS
*
                  CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.GT.0 )
     $               INFO = IERR
*
                  IF( IJOB.EQ.0 ) THEN
                     CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV,
     $                            SCALOC )
                     IF( SCALOC.NE.ONE ) THEN
                        DO 50 K = 1, N
                           CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                           CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
   50                   CONTINUE
                        SCALE = SCALE*SCALOC
                     END IF
                  ELSE
                     CALL DLATDF( IJOB, ZDIM, Z, LDZ, RHS, RDSUM,
     $                            RDSCAL, IPIV, JPIV )
                  END IF
*
*                 Unpack solution vector(s)
*
                  C( IS, JS ) = RHS( 1 )
                  F( IS, JS ) = RHS( 2 )
*
*                 Substitute R(I, J) and L(I, J) into remaining
*                 equation.
*
                  IF( I.GT.1 ) THEN
                     ALPHA = -RHS( 1 )
                     CALL DAXPY( IS-1, ALPHA, A( 1, IS ), 1, C( 1, JS ),
     $                           1 )
                     CALL DAXPY( IS-1, ALPHA, D( 1, IS ), 1, F( 1, JS ),
     $                           1 )
                  END IF
                  IF( J.LT.Q ) THEN
                     CALL DAXPY( N-JE, RHS( 2 ), B( JS, JE+1 ), LDB,
     $                           C( IS, JE+1 ), LDC )
                     CALL DAXPY( N-JE, RHS( 2 ), E( JS, JE+1 ), LDE,
     $                           F( IS, JE+1 ), LDF )
                  END IF
*
               ELSE IF( ( MB.EQ.1 ) .AND. ( NB.EQ.2 ) ) THEN
*
*                 Build a 4-by-4 system Z * x = RHS
*
                  Z( 1, 1 ) = A( IS, IS )
                  Z( 2, 1 ) = ZERO
                  Z( 3, 1 ) = D( IS, IS )
                  Z( 4, 1 ) = ZERO
*
                  Z( 1, 2 ) = ZERO
                  Z( 2, 2 ) = A( IS, IS )
                  Z( 3, 2 ) = ZERO
                  Z( 4, 2 ) = D( IS, IS )
*
                  Z( 1, 3 ) = -B( JS, JS )
                  Z( 2, 3 ) = -B( JS, JSP1 )
                  Z( 3, 3 ) = -E( JS, JS )
                  Z( 4, 3 ) = -E( JS, JSP1 )
*
                  Z( 1, 4 ) = -B( JSP1, JS )
                  Z( 2, 4 ) = -B( JSP1, JSP1 )
                  Z( 3, 4 ) = ZERO
                  Z( 4, 4 ) = -E( JSP1, JSP1 )
*
*                 Set up right hand side(s)
*
                  RHS( 1 ) = C( IS, JS )
                  RHS( 2 ) = C( IS, JSP1 )
                  RHS( 3 ) = F( IS, JS )
                  RHS( 4 ) = F( IS, JSP1 )
*
*                 Solve Z * x = RHS
*
                  CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.GT.0 )
     $               INFO = IERR
*
                  IF( IJOB.EQ.0 ) THEN
                     CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV,
     $                            SCALOC )
                     IF( SCALOC.NE.ONE ) THEN
                        DO 60 K = 1, N
                           CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                           CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
   60                   CONTINUE
                        SCALE = SCALE*SCALOC
                     END IF
                  ELSE
                     CALL DLATDF( IJOB, ZDIM, Z, LDZ, RHS, RDSUM,
     $                            RDSCAL, IPIV, JPIV )
                  END IF
*
*                 Unpack solution vector(s)
*
                  C( IS, JS ) = RHS( 1 )
                  C( IS, JSP1 ) = RHS( 2 )
                  F( IS, JS ) = RHS( 3 )
                  F( IS, JSP1 ) = RHS( 4 )
*
*                 Substitute R(I, J) and L(I, J) into remaining
*                 equation.
*
                  IF( I.GT.1 ) THEN
                     CALL DGER( IS-1, NB, -ONE, A( 1, IS ), 1, RHS( 1 ),
     $                          1, C( 1, JS ), LDC )
                     CALL DGER( IS-1, NB, -ONE, D( 1, IS ), 1, RHS( 1 ),
     $                          1, F( 1, JS ), LDF )
                  END IF
                  IF( J.LT.Q ) THEN
                     CALL DAXPY( N-JE, RHS( 3 ), B( JS, JE+1 ), LDB,
     $                           C( IS, JE+1 ), LDC )
                     CALL DAXPY( N-JE, RHS( 3 ), E( JS, JE+1 ), LDE,
     $                           F( IS, JE+1 ), LDF )
                     CALL DAXPY( N-JE, RHS( 4 ), B( JSP1, JE+1 ), LDB,
     $                           C( IS, JE+1 ), LDC )
                     CALL DAXPY( N-JE, RHS( 4 ), E( JSP1, JE+1 ), LDE,
     $                           F( IS, JE+1 ), LDF )
                  END IF
*
               ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.1 ) ) THEN
*
*                 Build a 4-by-4 system Z * x = RHS
*
                  Z( 1, 1 ) = A( IS, IS )
                  Z( 2, 1 ) = A( ISP1, IS )
                  Z( 3, 1 ) = D( IS, IS )
                  Z( 4, 1 ) = ZERO
*
                  Z( 1, 2 ) = A( IS, ISP1 )
                  Z( 2, 2 ) = A( ISP1, ISP1 )
                  Z( 3, 2 ) = D( IS, ISP1 )
                  Z( 4, 2 ) = D( ISP1, ISP1 )
*
                  Z( 1, 3 ) = -B( JS, JS )
                  Z( 2, 3 ) = ZERO
                  Z( 3, 3 ) = -E( JS, JS )
                  Z( 4, 3 ) = ZERO
*
                  Z( 1, 4 ) = ZERO
                  Z( 2, 4 ) = -B( JS, JS )
                  Z( 3, 4 ) = ZERO
                  Z( 4, 4 ) = -E( JS, JS )
*
*                 Set up right hand side(s)
*
                  RHS( 1 ) = C( IS, JS )
                  RHS( 2 ) = C( ISP1, JS )
                  RHS( 3 ) = F( IS, JS )
                  RHS( 4 ) = F( ISP1, JS )
*
*                 Solve Z * x = RHS
*
                  CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.GT.0 )
     $               INFO = IERR
                  IF( IJOB.EQ.0 ) THEN
                     CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV,
     $                            SCALOC )
                     IF( SCALOC.NE.ONE ) THEN
                        DO 70 K = 1, N
                           CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                           CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
   70                   CONTINUE
                        SCALE = SCALE*SCALOC
                     END IF
                  ELSE
                     CALL DLATDF( IJOB, ZDIM, Z, LDZ, RHS, RDSUM,
     $                            RDSCAL, IPIV, JPIV )
                  END IF
*
*                 Unpack solution vector(s)
*
                  C( IS, JS ) = RHS( 1 )
                  C( ISP1, JS ) = RHS( 2 )
                  F( IS, JS ) = RHS( 3 )
                  F( ISP1, JS ) = RHS( 4 )
*
*                 Substitute R(I, J) and L(I, J) into remaining
*                 equation.
*
                  IF( I.GT.1 ) THEN
                     CALL DGEMV( 'N', IS-1, MB, -ONE, A( 1, IS ), LDA,
     $                           RHS( 1 ), 1, ONE, C( 1, JS ), 1 )
                     CALL DGEMV( 'N', IS-1, MB, -ONE, D( 1, IS ), LDD,
     $                           RHS( 1 ), 1, ONE, F( 1, JS ), 1 )
                  END IF
                  IF( J.LT.Q ) THEN
                     CALL DGER( MB, N-JE, ONE, RHS( 3 ), 1,
     $                          B( JS, JE+1 ), LDB, C( IS, JE+1 ), LDC )
                     CALL DGER( MB, N-JE, ONE, RHS( 3 ), 1,
     $                          E( JS, JE+1 ), LDB, F( IS, JE+1 ), LDC )
                  END IF
*
               ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.2 ) ) THEN
*
*                 Build an 8-by-8 system Z * x = RHS
*
                  CALL DLASET( 'F', LDZ, LDZ, ZERO, ZERO, Z, LDZ )
*
                  Z( 1, 1 ) = A( IS, IS )
                  Z( 2, 1 ) = A( ISP1, IS )
                  Z( 5, 1 ) = D( IS, IS )
*
                  Z( 1, 2 ) = A( IS, ISP1 )
                  Z( 2, 2 ) = A( ISP1, ISP1 )
                  Z( 5, 2 ) = D( IS, ISP1 )
                  Z( 6, 2 ) = D( ISP1, ISP1 )
*
                  Z( 3, 3 ) = A( IS, IS )
                  Z( 4, 3 ) = A( ISP1, IS )
                  Z( 7, 3 ) = D( IS, IS )
*
                  Z( 3, 4 ) = A( IS, ISP1 )
                  Z( 4, 4 ) = A( ISP1, ISP1 )
                  Z( 7, 4 ) = D( IS, ISP1 )
                  Z( 8, 4 ) = D( ISP1, ISP1 )
*
                  Z( 1, 5 ) = -B( JS, JS )
                  Z( 3, 5 ) = -B( JS, JSP1 )
                  Z( 5, 5 ) = -E( JS, JS )
                  Z( 7, 5 ) = -E( JS, JSP1 )
*
                  Z( 2, 6 ) = -B( JS, JS )
                  Z( 4, 6 ) = -B( JS, JSP1 )
                  Z( 6, 6 ) = -E( JS, JS )
                  Z( 8, 6 ) = -E( JS, JSP1 )
*
                  Z( 1, 7 ) = -B( JSP1, JS )
                  Z( 3, 7 ) = -B( JSP1, JSP1 )
                  Z( 7, 7 ) = -E( JSP1, JSP1 )
*
                  Z( 2, 8 ) = -B( JSP1, JS )
                  Z( 4, 8 ) = -B( JSP1, JSP1 )
                  Z( 8, 8 ) = -E( JSP1, JSP1 )
*
*                 Set up right hand side(s)
*
                  K = 1
                  II = MB*NB + 1
                  DO 80 JJ = 0, NB - 1
                     CALL DCOPY( MB, C( IS, JS+JJ ), 1, RHS( K ), 1 )
                     CALL DCOPY( MB, F( IS, JS+JJ ), 1, RHS( II ), 1 )
                     K = K + MB
                     II = II + MB
   80             CONTINUE
*
*                 Solve Z * x = RHS
*
                  CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.GT.0 )
     $               INFO = IERR
                  IF( IJOB.EQ.0 ) THEN
                     CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV,
     $                            SCALOC )
                     IF( SCALOC.NE.ONE ) THEN
                        DO 90 K = 1, N
                           CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                           CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
   90                   CONTINUE
                        SCALE = SCALE*SCALOC
                     END IF
                  ELSE
                     CALL DLATDF( IJOB, ZDIM, Z, LDZ, RHS, RDSUM,
     $                            RDSCAL, IPIV, JPIV )
                  END IF
*
*                 Unpack solution vector(s)
*
                  K = 1
                  II = MB*NB + 1
                  DO 100 JJ = 0, NB - 1
                     CALL DCOPY( MB, RHS( K ), 1, C( IS, JS+JJ ), 1 )
                     CALL DCOPY( MB, RHS( II ), 1, F( IS, JS+JJ ), 1 )
                     K = K + MB
                     II = II + MB
  100             CONTINUE
*
*                 Substitute R(I, J) and L(I, J) into remaining
*                 equation.
*
                  IF( I.GT.1 ) THEN
                     CALL DGEMM( 'N', 'N', IS-1, NB, MB, -ONE,
     $                           A( 1, IS ), LDA, RHS( 1 ), MB, ONE,
     $                           C( 1, JS ), LDC )
                     CALL DGEMM( 'N', 'N', IS-1, NB, MB, -ONE,
     $                           D( 1, IS ), LDD, RHS( 1 ), MB, ONE,
     $                           F( 1, JS ), LDF )
                  END IF
                  IF( J.LT.Q ) THEN
                     K = MB*NB + 1
                     CALL DGEMM( 'N', 'N', MB, N-JE, NB, ONE, RHS( K ),
     $                           MB, B( JS, JE+1 ), LDB, ONE,
     $                           C( IS, JE+1 ), LDC )
                     CALL DGEMM( 'N', 'N', MB, N-JE, NB, ONE, RHS( K ),
     $                           MB, E( JS, JE+1 ), LDE, ONE,
     $                           F( IS, JE+1 ), LDF )
                  END IF
*
               END IF
*
  110       CONTINUE
  120    CONTINUE
      ELSE
*
*        Solve (I, J) - subsystem
*             A(I, I)' * R(I, J) + D(I, I)' * L(J, J)  =  C(I, J)
*             R(I, I)  * B(J, J) + L(I, J)  * E(J, J)  = -F(I, J)
*        for I = 1, 2, ..., P, J = Q, Q - 1, ..., 1
*
         SCALE = ONE
         SCALOC = ONE
         DO 200 I = 1, P
*
            IS = IWORK( I )
            ISP1 = IS + 1
            IE = ( I+1 ) - 1
            MB = IE - IS + 1
            DO 190 J = Q, P + 2, -1
*
               JS = IWORK( J )
               JSP1 = JS + 1
               JE = IWORK( J+1 ) - 1
               NB = JE - JS + 1
               ZDIM = MB*NB*2
               IF( ( MB.EQ.1 ) .AND. ( NB.EQ.1 ) ) THEN
*
*                 Build a 2-by-2 system Z' * x = RHS
*
                  Z( 1, 1 ) = A( IS, IS )
                  Z( 2, 1 ) = -B( JS, JS )
                  Z( 1, 2 ) = D( IS, IS )
                  Z( 2, 2 ) = -E( JS, JS )
*
*                 Set up right hand side(s)
*
                  RHS( 1 ) = C( IS, JS )
                  RHS( 2 ) = F( IS, JS )
*
*                 Solve Z' * x = RHS
*
                  CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.GT.0 )
     $               INFO = IERR
*
                  CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
                  IF( SCALOC.NE.ONE ) THEN
                     DO 130 K = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                        CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
  130                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
*
*                 Unpack solution vector(s)
*
                  C( IS, JS ) = RHS( 1 )
                  F( IS, JS ) = RHS( 2 )
*
*                 Substitute R(I, J) and L(I, J) into remaining
*                 equation.
*
                  IF( J.GT.P+2 ) THEN
                     ALPHA = RHS( 1 )
                     CALL DAXPY( JS-1, ALPHA, B( 1, JS ), 1, F( IS, 1 ),
     $                           LDF )
                     ALPHA = RHS( 2 )
                     CALL DAXPY( JS-1, ALPHA, E( 1, JS ), 1, F( IS, 1 ),
     $                           LDF )
                  END IF
                  IF( I.LT.P ) THEN
                     ALPHA = -RHS( 1 )
                     CALL DAXPY( M-IE, ALPHA, A( IS, IE+1 ), LDA,
     $                           C( IE+1, JS ), 1 )
                     ALPHA = -RHS( 2 )
                     CALL DAXPY( M-IE, ALPHA, D( IS, IE+1 ), LDD,
     $                           C( IE+1, JS ), 1 )
                  END IF
*
               ELSE IF( ( MB.EQ.1 ) .AND. ( NB.EQ.2 ) ) THEN
*
*                 Build a 4-by-4 system Z' * x = RHS
*
                  Z( 1, 1 ) = A( IS, IS )
                  Z( 2, 1 ) = ZERO
                  Z( 3, 1 ) = -B( JS, JS )
                  Z( 4, 1 ) = -B( JSP1, JS )
*
                  Z( 1, 2 ) = ZERO
                  Z( 2, 2 ) = A( IS, IS )
                  Z( 3, 2 ) = -B( JS, JSP1 )
                  Z( 4, 2 ) = -B( JSP1, JSP1 )
*
                  Z( 1, 3 ) = D( IS, IS )
                  Z( 2, 3 ) = ZERO
                  Z( 3, 3 ) = -E( JS, JS )
                  Z( 4, 3 ) = ZERO
*
                  Z( 1, 4 ) = ZERO
                  Z( 2, 4 ) = D( IS, IS )
                  Z( 3, 4 ) = -E( JS, JSP1 )
                  Z( 4, 4 ) = -E( JSP1, JSP1 )
*
*                 Set up right hand side(s)
*
                  RHS( 1 ) = C( IS, JS )
                  RHS( 2 ) = C( IS, JSP1 )
                  RHS( 3 ) = F( IS, JS )
                  RHS( 4 ) = F( IS, JSP1 )
*
*                 Solve Z' * x = RHS
*
                  CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.GT.0 )
     $               INFO = IERR
                  CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
                  IF( SCALOC.NE.ONE ) THEN
                     DO 140 K = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                        CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
  140                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
*
*                 Unpack solution vector(s)
*
                  C( IS, JS ) = RHS( 1 )
                  C( IS, JSP1 ) = RHS( 2 )
                  F( IS, JS ) = RHS( 3 )
                  F( IS, JSP1 ) = RHS( 4 )
*
*                 Substitute R(I, J) and L(I, J) into remaining
*                 equation.
*
                  IF( J.GT.P+2 ) THEN
                     CALL DAXPY( JS-1, RHS( 1 ), B( 1, JS ), 1,
     $                           F( IS, 1 ), LDF )
                     CALL DAXPY( JS-1, RHS( 2 ), B( 1, JSP1 ), 1,
     $                           F( IS, 1 ), LDF )
                     CALL DAXPY( JS-1, RHS( 3 ), E( 1, JS ), 1,
     $                           F( IS, 1 ), LDF )
                     CALL DAXPY( JS-1, RHS( 4 ), E( 1, JSP1 ), 1,
     $                           F( IS, 1 ), LDF )
                  END IF
                  IF( I.LT.P ) THEN
                     CALL DGER( M-IE, NB, -ONE, A( IS, IE+1 ), LDA,
     $                          RHS( 1 ), 1, C( IE+1, JS ), LDC )
                     CALL DGER( M-IE, NB, -ONE, D( IS, IE+1 ), LDD,
     $                          RHS( 3 ), 1, C( IE+1, JS ), LDC )
                  END IF
*
               ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.1 ) ) THEN
*
*                 Build a 4-by-4 system Z' * x = RHS
*
                  Z( 1, 1 ) = A( IS, IS )
                  Z( 2, 1 ) = A( IS, ISP1 )
                  Z( 3, 1 ) = -B( JS, JS )
                  Z( 4, 1 ) = ZERO
*
                  Z( 1, 2 ) = A( ISP1, IS )
                  Z( 2, 2 ) = A( ISP1, ISP1 )
                  Z( 3, 2 ) = ZERO
                  Z( 4, 2 ) = -B( JS, JS )
*
                  Z( 1, 3 ) = D( IS, IS )
                  Z( 2, 3 ) = D( IS, ISP1 )
                  Z( 3, 3 ) = -E( JS, JS )
                  Z( 4, 3 ) = ZERO
*
                  Z( 1, 4 ) = ZERO
                  Z( 2, 4 ) = D( ISP1, ISP1 )
                  Z( 3, 4 ) = ZERO
                  Z( 4, 4 ) = -E( JS, JS )
*
*                 Set up right hand side(s)
*
                  RHS( 1 ) = C( IS, JS )
                  RHS( 2 ) = C( ISP1, JS )
                  RHS( 3 ) = F( IS, JS )
                  RHS( 4 ) = F( ISP1, JS )
*
*                 Solve Z' * x = RHS
*
                  CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.GT.0 )
     $               INFO = IERR
*
                  CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
                  IF( SCALOC.NE.ONE ) THEN
                     DO 150 K = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                        CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
  150                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
*
*                 Unpack solution vector(s)
*
                  C( IS, JS ) = RHS( 1 )
                  C( ISP1, JS ) = RHS( 2 )
                  F( IS, JS ) = RHS( 3 )
                  F( ISP1, JS ) = RHS( 4 )
*
*                 Substitute R(I, J) and L(I, J) into remaining
*                 equation.
*
                  IF( J.GT.P+2 ) THEN
                     CALL DGER( MB, JS-1, ONE, RHS( 1 ), 1, B( 1, JS ),
     $                          1, F( IS, 1 ), LDF )
                     CALL DGER( MB, JS-1, ONE, RHS( 3 ), 1, E( 1, JS ),
     $                          1, F( IS, 1 ), LDF )
                  END IF
                  IF( I.LT.P ) THEN
                     CALL DGEMV( 'T', MB, M-IE, -ONE, A( IS, IE+1 ),
     $                           LDA, RHS( 1 ), 1, ONE, C( IE+1, JS ),
     $                           1 )
                     CALL DGEMV( 'T', MB, M-IE, -ONE, D( IS, IE+1 ),
     $                           LDD, RHS( 3 ), 1, ONE, C( IE+1, JS ),
     $                           1 )
                  END IF
*
               ELSE IF( ( MB.EQ.2 ) .AND. ( NB.EQ.2 ) ) THEN
*
*                 Build an 8-by-8 system Z' * x = RHS
*
                  CALL DLASET( 'F', LDZ, LDZ, ZERO, ZERO, Z, LDZ )
*
                  Z( 1, 1 ) = A( IS, IS )
                  Z( 2, 1 ) = A( IS, ISP1 )
                  Z( 5, 1 ) = -B( JS, JS )
                  Z( 7, 1 ) = -B( JSP1, JS )
*
                  Z( 1, 2 ) = A( ISP1, IS )
                  Z( 2, 2 ) = A( ISP1, ISP1 )
                  Z( 6, 2 ) = -B( JS, JS )
                  Z( 8, 2 ) = -B( JSP1, JS )
*
                  Z( 3, 3 ) = A( IS, IS )
                  Z( 4, 3 ) = A( IS, ISP1 )
                  Z( 5, 3 ) = -B( JS, JSP1 )
                  Z( 7, 3 ) = -B( JSP1, JSP1 )
*
                  Z( 3, 4 ) = A( ISP1, IS )
                  Z( 4, 4 ) = A( ISP1, ISP1 )
                  Z( 6, 4 ) = -B( JS, JSP1 )
                  Z( 8, 4 ) = -B( JSP1, JSP1 )
*
                  Z( 1, 5 ) = D( IS, IS )
                  Z( 2, 5 ) = D( IS, ISP1 )
                  Z( 5, 5 ) = -E( JS, JS )
*
                  Z( 2, 6 ) = D( ISP1, ISP1 )
                  Z( 6, 6 ) = -E( JS, JS )
*
                  Z( 3, 7 ) = D( IS, IS )
                  Z( 4, 7 ) = D( IS, ISP1 )
                  Z( 5, 7 ) = -E( JS, JSP1 )
                  Z( 7, 7 ) = -E( JSP1, JSP1 )
*
                  Z( 4, 8 ) = D( ISP1, ISP1 )
                  Z( 6, 8 ) = -E( JS, JSP1 )
                  Z( 8, 8 ) = -E( JSP1, JSP1 )
*
*                 Set up right hand side(s)
*
                  K = 1
                  II = MB*NB + 1
                  DO 160 JJ = 0, NB - 1
                     CALL DCOPY( MB, C( IS, JS+JJ ), 1, RHS( K ), 1 )
                     CALL DCOPY( MB, F( IS, JS+JJ ), 1, RHS( II ), 1 )
                     K = K + MB
                     II = II + MB
  160             CONTINUE
*
*
*                 Solve Z' * x = RHS
*
                  CALL DGETC2( ZDIM, Z, LDZ, IPIV, JPIV, IERR )
                  IF( IERR.GT.0 )
     $               INFO = IERR
*
                  CALL DGESC2( ZDIM, Z, LDZ, RHS, IPIV, JPIV, SCALOC )
                  IF( SCALOC.NE.ONE ) THEN
                     DO 170 K = 1, N
                        CALL DSCAL( M, SCALOC, C( 1, K ), 1 )
                        CALL DSCAL( M, SCALOC, F( 1, K ), 1 )
  170                CONTINUE
                     SCALE = SCALE*SCALOC
                  END IF
*
*                 Unpack solution vector(s)
*
                  K = 1
                  II = MB*NB + 1
                  DO 180 JJ = 0, NB - 1
                     CALL DCOPY( MB, RHS( K ), 1, C( IS, JS+JJ ), 1 )
                     CALL DCOPY( MB, RHS( II ), 1, F( IS, JS+JJ ), 1 )
                     K = K + MB
                     II = II + MB
  180             CONTINUE
*
*                 Substitute R(I, J) and L(I, J) into remaining
*                 equation.
*
                  IF( J.GT.P+2 ) THEN
                     CALL DGEMM( 'N', 'T', MB, JS-1, NB, ONE,
     $                           C( IS, JS ), LDC, B( 1, JS ), LDB, ONE,
     $                           F( IS, 1 ), LDF )
                     CALL DGEMM( 'N', 'T', MB, JS-1, NB, ONE,
     $                           F( IS, JS ), LDF, E( 1, JS ), LDE, ONE,
     $                           F( IS, 1 ), LDF )
                  END IF
                  IF( I.LT.P ) THEN
                     CALL DGEMM( 'T', 'N', M-IE, NB, MB, -ONE,
     $                           A( IS, IE+1 ), LDA, C( IS, JS ), LDC,
     $                           ONE, C( IE+1, JS ), LDC )
                     CALL DGEMM( 'T', 'N', M-IE, NB, MB, -ONE,
     $                           D( IS, IE+1 ), LDD, F( IS, JS ), LDF,
     $                           ONE, C( IE+1, JS ), LDC )
                  END IF
*
               END IF
*
  190       CONTINUE
  200    CONTINUE
*
      END IF
      RETURN
*
*     End of DTGSY2
*
      END
      INTEGER          FUNCTION IEEECK( ISPEC, ZERO, ONE )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            ISPEC
      REAL               ONE, ZERO
*     ..
*
*  Purpose
*  =======
*
*  IEEECK is called from the ILAENV to verify that Infinity and
*  possibly NaN arithmetic is safe (i.e. will not trap).
*
*  Arguments
*  =========
*
*  ISPEC   (input) INTEGER
*          Specifies whether to test just for inifinity arithmetic
*          or whether to test for infinity and NaN arithmetic.
*          = 0: Verify infinity arithmetic only.
*          = 1: Verify infinity and NaN arithmetic.
*
*  ZERO    (input) REAL
*          Must contain the value 0.0
*          This is passed to prevent the compiler from optimizing
*          away this code.
*
*  ONE     (input) REAL
*          Must contain the value 1.0
*          This is passed to prevent the compiler from optimizing
*          away this code.
*
*  RETURN VALUE:  INTEGER
*          = 0:  Arithmetic failed to produce the correct answers
*          = 1:  Arithmetic produced the correct answers
*
*     .. Local Scalars ..
      REAL               NAN1, NAN2, NAN3, NAN4, NAN5, NAN6, NEGINF,
     $                   NEGZRO, NEWZRO, POSINF
*     ..
*     .. Executable Statements ..
      IEEECK = 1
*
      POSINF = ONE / ZERO
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      NEGINF = -ONE / ZERO
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      NEGZRO = ONE / ( NEGINF+ONE )
      IF( NEGZRO.NE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      NEGINF = ONE / NEGZRO
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      NEWZRO = NEGZRO + ZERO
      IF( NEWZRO.NE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      POSINF = ONE / NEWZRO
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      NEGINF = NEGINF*POSINF
      IF( NEGINF.GE.ZERO ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      POSINF = POSINF*POSINF
      IF( POSINF.LE.ONE ) THEN
         IEEECK = 0
         RETURN
      END IF
*
*
*
*
*     Return if we were only asked to check infinity arithmetic
*
      IF( ISPEC.EQ.0 )
     $   RETURN
*
      NAN1 = POSINF + NEGINF
*
      NAN2 = POSINF / NEGINF
*
      NAN3 = POSINF / POSINF
*
      NAN4 = POSINF*ZERO
*
      NAN5 = NEGINF*NEGZRO
*
      NAN6 = NAN5*0.0
*
      IF( NAN1.EQ.NAN1 ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      IF( NAN2.EQ.NAN2 ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      IF( NAN3.EQ.NAN3 ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      IF( NAN4.EQ.NAN4 ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      IF( NAN5.EQ.NAN5 ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      IF( NAN6.EQ.NAN6 ) THEN
         IEEECK = 0
         RETURN
      END IF
*
      RETURN
      END
      INTEGER FUNCTION ILAENV( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
*
*  -- LAPACK auxiliary routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER*( * )    NAME, OPTS
      INTEGER            ISPEC, N1, N2, N3, N4
*     ..
*
*  Purpose
*  =======
*
*  ILAENV is called from the LAPACK routines to choose problem-dependent
*  parameters for the local environment.  See ISPEC for a description of
*  the parameters.
*
*  This version provides a set of parameters which should give good,
*  but not optimal, performance on many of the currently available
*  computers.  Users are encouraged to modify this subroutine to set
*  the tuning parameters for their particular machine using the option
*  and problem size information in the arguments.
*
*  This routine will not function correctly if it is converted to all
*  lower case.  Converting it to all upper case is allowed.
*
*  Arguments
*  =========
*
*  ISPEC   (input) INTEGER
*          Specifies the parameter to be returned as the value of
*          ILAENV.
*          = 1: the optimal blocksize; if this value is 1, an unblocked
*               algorithm will give the best performance.
*          = 2: the minimum block size for which the block routine
*               should be used; if the usable block size is less than
*               this value, an unblocked routine should be used.
*          = 3: the crossover point (in a block routine, for N less
*               than this value, an unblocked routine should be used)
*          = 4: the number of shifts, used in the nonsymmetric
*               eigenvalue routines (DEPRECATED)
*          = 5: the minimum column dimension for blocking to be used;
*               rectangular blocks must have dimension at least k by m,
*               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
*          = 6: the crossover point for the SVD (when reducing an m by n
*               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
*               this value, a QR factorization is used first to reduce
*               the matrix to a triangular form.)
*          = 7: the number of processors
*          = 8: the crossover point for the multishift QR method
*               for nonsymmetric eigenvalue problems (DEPRECATED)
*          = 9: maximum size of the subproblems at the bottom of the
*               computation tree in the divide-and-conquer algorithm
*               (used by xGELSD and xGESDD)
*          =10: ieee NaN arithmetic can be trusted not to trap
*          =11: infinity arithmetic can be trusted not to trap
*          12 <= ISPEC <= 16:
*               xHSEQR or one of its subroutines,
*               see IPARMQ for detailed explanation
*
*  NAME    (input) CHARACTER*(*)
*          The name of the calling subroutine, in either upper case or
*          lower case.
*
*  OPTS    (input) CHARACTER*(*)
*          The character options to the subroutine NAME, concatenated
*          into a single character string.  For example, UPLO = 'U',
*          TRANS = 'T', and DIAG = 'N' for a triangular routine would
*          be specified as OPTS = 'UTN'.
*
*  N1      (input) INTEGER
*  N2      (input) INTEGER
*  N3      (input) INTEGER
*  N4      (input) INTEGER
*          Problem dimensions for the subroutine NAME; these may not all
*          be required.
*
* (ILAENV) (output) INTEGER
*          >= 0: the value of the parameter specified by ISPEC
*          < 0:  if ILAENV = -k, the k-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  The following conventions have been used when calling ILAENV from the
*  LAPACK routines:
*  1)  OPTS is a concatenation of all of the character options to
*      subroutine NAME, in the same order that they appear in the
*      argument list for NAME, even if they are not used in determining
*      the value of the parameter specified by ISPEC.
*  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
*      that they appear in the argument list for NAME.  N1 is used
*      first, N2 second, and so on, and unused problem dimensions are
*      passed a value of -1.
*  3)  The parameter value returned by ILAENV is checked for validity in
*      the calling subroutine.  For example, ILAENV is used to retrieve
*      the optimal blocksize for STRTRI as follows:
*
*      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
*      IF( NB.LE.1 ) NB = MAX( 1, N )
*
*  =====================================================================
*
*     .. Local Scalars ..
      INTEGER            I, IC, IZ, NB, NBMIN, NX
      LOGICAL            CNAME, SNAME
      CHARACTER          C1*1, C2*2, C4*2, C3*3, SUBNAM*6
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL
*     ..
*     .. External Functions ..
      INTEGER            IEEECK, IPARMQ
      EXTERNAL           IEEECK, IPARMQ
*     ..
*     .. Executable Statements ..
*
      GO TO ( 10, 10, 10, 80, 90, 100, 110, 120,
     $        130, 140, 150, 160, 160, 160, 160, 160 )ISPEC
*
*     Invalid value for ISPEC
*
      ILAENV = -1
      RETURN
*
   10 CONTINUE
*
*     Convert NAME to upper case if the first character is lower case.
*
      ILAENV = 1
      SUBNAM = NAME
      IC = ICHAR( SUBNAM( 1: 1 ) )
      IZ = ICHAR( 'Z' )
      IF( IZ.EQ.90 .OR. IZ.EQ.122 ) THEN
*
*        ASCII character set
*
         IF( IC.GE.97 .AND. IC.LE.122 ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC-32 )
            DO 20 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( IC.GE.97 .AND. IC.LE.122 )
     $            SUBNAM( I: I ) = CHAR( IC-32 )
   20       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.233 .OR. IZ.EQ.169 ) THEN
*
*        EBCDIC character set
*
         IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $       ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $       ( IC.GE.162 .AND. IC.LE.169 ) ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC+64 )
            DO 30 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( ( IC.GE.129 .AND. IC.LE.137 ) .OR.
     $             ( IC.GE.145 .AND. IC.LE.153 ) .OR.
     $             ( IC.GE.162 .AND. IC.LE.169 ) )SUBNAM( I:
     $             I ) = CHAR( IC+64 )
   30       CONTINUE
         END IF
*
      ELSE IF( IZ.EQ.218 .OR. IZ.EQ.250 ) THEN
*
*        Prime machines:  ASCII+128
*
         IF( IC.GE.225 .AND. IC.LE.250 ) THEN
            SUBNAM( 1: 1 ) = CHAR( IC-32 )
            DO 40 I = 2, 6
               IC = ICHAR( SUBNAM( I: I ) )
               IF( IC.GE.225 .AND. IC.LE.250 )
     $            SUBNAM( I: I ) = CHAR( IC-32 )
   40       CONTINUE
         END IF
      END IF
*
      C1 = SUBNAM( 1: 1 )
      SNAME = C1.EQ.'S' .OR. C1.EQ.'D'
      CNAME = C1.EQ.'C' .OR. C1.EQ.'Z'
      IF( .NOT.( CNAME .OR. SNAME ) )
     $   RETURN
      C2 = SUBNAM( 2: 3 )
      C3 = SUBNAM( 4: 6 )
      C4 = C3( 2: 3 )
*
      GO TO ( 50, 60, 70 )ISPEC
*
   50 CONTINUE
*
*     ISPEC = 1:  block size
*
*     In these examples, separate code is provided for setting NB for
*     real and complex.  We assume that NB will take the same value in
*     single or double precision.
*
      NB = 1
*
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR.
     $            C3.EQ.'QLF' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NB = 32
            ELSE
               NB = 32
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'PO' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NB = 32
         ELSE IF( SNAME .AND. C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            NB = 64
         ELSE IF( C3.EQ.'TRD' ) THEN
            NB = 32
         ELSE IF( C3.EQ.'GST' ) THEN
            NB = 64
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NB = 32
            END IF
         END IF
      ELSE IF( C2.EQ.'GB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N4.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'PB' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            ELSE
               IF( N2.LE.64 ) THEN
                  NB = 1
               ELSE
                  NB = 32
               END IF
            END IF
         END IF
      ELSE IF( C2.EQ.'TR' ) THEN
         IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( C2.EQ.'LA' ) THEN
         IF( C3.EQ.'UUM' ) THEN
            IF( SNAME ) THEN
               NB = 64
            ELSE
               NB = 64
            END IF
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'ST' ) THEN
         IF( C3.EQ.'EBZ' ) THEN
            NB = 1
         END IF
      END IF
      ILAENV = NB
      RETURN
*
   60 CONTINUE
*
*     ISPEC = 2:  minimum block size
*
      NBMIN = 2
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR. C3.EQ.
     $       'QLF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         ELSE IF( C3.EQ.'TRI' ) THEN
            IF( SNAME ) THEN
               NBMIN = 2
            ELSE
               NBMIN = 2
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( C3.EQ.'TRF' ) THEN
            IF( SNAME ) THEN
               NBMIN = 8
            ELSE
               NBMIN = 8
            END IF
         ELSE IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NBMIN = 2
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         ELSE IF( C3( 1: 1 ).EQ.'M' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NBMIN = 2
            END IF
         END IF
      END IF
      ILAENV = NBMIN
      RETURN
*
   70 CONTINUE
*
*     ISPEC = 3:  crossover point
*
      NX = 0
      IF( C2.EQ.'GE' ) THEN
         IF( C3.EQ.'QRF' .OR. C3.EQ.'RQF' .OR. C3.EQ.'LQF' .OR. C3.EQ.
     $       'QLF' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'HRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         ELSE IF( C3.EQ.'BRD' ) THEN
            IF( SNAME ) THEN
               NX = 128
            ELSE
               NX = 128
            END IF
         END IF
      ELSE IF( C2.EQ.'SY' ) THEN
         IF( SNAME .AND. C3.EQ.'TRD' ) THEN
            NX = 32
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'HE' ) THEN
         IF( C3.EQ.'TRD' ) THEN
            NX = 32
         END IF
      ELSE IF( SNAME .AND. C2.EQ.'OR' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NX = 128
            END IF
         END IF
      ELSE IF( CNAME .AND. C2.EQ.'UN' ) THEN
         IF( C3( 1: 1 ).EQ.'G' ) THEN
            IF( C4.EQ.'QR' .OR. C4.EQ.'RQ' .OR. C4.EQ.'LQ' .OR. C4.EQ.
     $          'QL' .OR. C4.EQ.'HR' .OR. C4.EQ.'TR' .OR. C4.EQ.'BR' )
     $           THEN
               NX = 128
            END IF
         END IF
      END IF
      ILAENV = NX
      RETURN
*
   80 CONTINUE
*
*     ISPEC = 4:  number of shifts (used by xHSEQR)
*
      ILAENV = 6
      RETURN
*
   90 CONTINUE
*
*     ISPEC = 5:  minimum column dimension (not used)
*
      ILAENV = 2
      RETURN
*
  100 CONTINUE
*
*     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
*
      ILAENV = INT( REAL( MIN( N1, N2 ) )*1.6E0 )
      RETURN
*
  110 CONTINUE
*
*     ISPEC = 7:  number of processors (not used)
*
      ILAENV = 1
      RETURN
*
  120 CONTINUE
*
*     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
*
      ILAENV = 50
      RETURN
*
  130 CONTINUE
*
*     ISPEC = 9:  maximum size of the subproblems at the bottom of the
*                 computation tree in the divide-and-conquer algorithm
*                 (used by xGELSD and xGESDD)
*
      ILAENV = 25
      RETURN
*
  140 CONTINUE
*
*     ISPEC = 10: ieee NaN arithmetic can be trusted not to trap
*
*     ILAENV = 0
      ILAENV = 1
      IF( ILAENV.EQ.1 ) THEN
         ILAENV = IEEECK( 0, 0.0, 1.0 )
      END IF
      RETURN
*
  150 CONTINUE
*
*     ISPEC = 11: infinity arithmetic can be trusted not to trap
*
*     ILAENV = 0
      ILAENV = 1
      IF( ILAENV.EQ.1 ) THEN
         ILAENV = IEEECK( 1, 0.0, 1.0 )
      END IF
      RETURN
*
  160 CONTINUE
*
*     12 <= ISPEC <= 16: xHSEQR or one of its subroutines. 
*
      ILAENV = IPARMQ( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
      RETURN
*
*     End of ILAENV
*
      END
