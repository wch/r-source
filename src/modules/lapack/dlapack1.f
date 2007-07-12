      SUBROUTINE DGBSV( N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, KL, KU, LDAB, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   AB( LDAB, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DGBSV computes the solution to a real system of linear equations
*  A * X = B, where A is a band matrix of order N with KL subdiagonals
*  and KU superdiagonals, and X and B are N-by-NRHS matrices.
*
*  The LU decomposition with partial pivoting and row interchanges is
*  used to factor A as A = L * U, where L is a product of permutation
*  and unit lower triangular matrices with KL subdiagonals, and U is
*  upper triangular with KL+KU superdiagonals.  The factored form of A
*  is then used to solve the system of equations A * X = B.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  KL      (input) INTEGER
*          The number of subdiagonals within the band of A.  KL >= 0.
*
*  KU      (input) INTEGER
*          The number of superdiagonals within the band of A.  KU >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
*          On entry, the matrix A in band storage, in rows KL+1 to
*          2*KL+KU+1; rows 1 to KL of the array need not be set.
*          The j-th column of A is stored in the j-th column of the
*          array AB as follows:
*          AB(KL+KU+1+i-j,j) = A(i,j) for max(1,j-KU)<=i<=min(N,j+KL)
*          On exit, details of the factorization: U is stored as an
*          upper triangular band matrix with KL+KU superdiagonals in
*          rows 1 to KL+KU+1, and the multipliers used during the
*          factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
*          See below for further details.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
*
*  IPIV    (output) INTEGER array, dimension (N)
*          The pivot indices that define the permutation matrix P;
*          row i of the matrix was interchanged with row IPIV(i).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
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
*                singular, and the solution has not been computed.
*
*  Further Details
*  ===============
*
*  The band storage scheme is illustrated by the following example, when
*  M = N = 6, KL = 2, KU = 1:
*
*  On entry:                       On exit:
*
*      *    *    *    +    +    +       *    *    *   u14  u25  u36
*      *    *    +    +    +    +       *    *   u13  u24  u35  u46
*      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
*     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
*     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
*     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
*
*  Array elements marked * are not used by the routine; elements marked
*  + need not be set on entry, but are required by the routine to store
*  elements of U because of fill-in resulting from the row interchanges.
*
*  =====================================================================
*
*     .. External Subroutines ..
      EXTERNAL           DGBTRF, DGBTRS, XERBLA
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
      ELSE IF( KL.LT.0 ) THEN
         INFO = -2
      ELSE IF( KU.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDAB.LT.2*KL+KU+1 ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( N, 1 ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGBSV ', -INFO )
         RETURN
      END IF
*
*     Compute the LU factorization of the band matrix A.
*
      CALL DGBTRF( N, N, KL, KU, AB, LDAB, IPIV, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL DGBTRS( 'No transpose', N, KL, KU, NRHS, AB, LDAB, IPIV,
     $                B, LDB, INFO )
      END IF
      RETURN
*
*     End of DGBSV
*
      END
      SUBROUTINE DGBSVX( FACT, TRANS, N, KL, KU, NRHS, AB, LDAB, AFB,
     $                   LDAFB, IPIV, EQUED, R, C, B, LDB, X, LDX,
     $                   RCOND, FERR, BERR, WORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED, FACT, TRANS
      INTEGER            INFO, KL, KU, LDAB, LDAFB, LDB, LDX, N, NRHS
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * ), IWORK( * )
      DOUBLE PRECISION   AB( LDAB, * ), AFB( LDAFB, * ), B( LDB, * ),
     $                   BERR( * ), C( * ), FERR( * ), R( * ),
     $                   WORK( * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DGBSVX uses the LU factorization to compute the solution to a real
*  system of linear equations A * X = B, A**T * X = B, or A**H * X = B,
*  where A is a band matrix of order N with KL subdiagonals and KU
*  superdiagonals, and X and B are N-by-NRHS matrices.
*
*  Error bounds on the solution and a condition estimate are also
*  provided.
*
*  Description
*  ===========
*
*  The following steps are performed by this subroutine:
*
*  1. If FACT = 'E', real scaling factors are computed to equilibrate
*     the system:
*        TRANS = 'N':  diag(R)*A*diag(C)     *inv(diag(C))*X = diag(R)*B
*        TRANS = 'T': (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B
*        TRANS = 'C': (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B
*     Whether or not the system will be equilibrated depends on the
*     scaling of the matrix A, but if equilibration is used, A is
*     overwritten by diag(R)*A*diag(C) and B by diag(R)*B (if TRANS='N')
*     or diag(C)*B (if TRANS = 'T' or 'C').
*
*  2. If FACT = 'N' or 'E', the LU decomposition is used to factor the
*     matrix A (after equilibration if FACT = 'E') as
*        A = L * U,
*     where L is a product of permutation and unit lower triangular
*     matrices with KL subdiagonals, and U is upper triangular with
*     KL+KU superdiagonals.
*
*  3. If some U(i,i)=0, so that U is exactly singular, then the routine
*     returns with INFO = i. Otherwise, the factored form of A is used
*     to estimate the condition number of the matrix A.  If the
*     reciprocal of the condition number is less than machine precision,
*     INFO = N+1 is returned as a warning, but the routine still goes on
*     to solve for X and compute error bounds as described below.
*
*  4. The system of equations is solved for X using the factored form
*     of A.
*
*  5. Iterative refinement is applied to improve the computed solution
*     matrix and calculate error bounds and backward error estimates
*     for it.
*
*  6. If equilibration was used, the matrix X is premultiplied by
*     diag(C) (if TRANS = 'N') or diag(R) (if TRANS = 'T' or 'C') so
*     that it solves the original system before equilibration.
*
*  Arguments
*  =========
*
*  FACT    (input) CHARACTER*1
*          Specifies whether or not the factored form of the matrix A is
*          supplied on entry, and if not, whether the matrix A should be
*          equilibrated before it is factored.
*          = 'F':  On entry, AFB and IPIV contain the factored form of
*                  A.  If EQUED is not 'N', the matrix A has been
*                  equilibrated with scaling factors given by R and C.
*                  AB, AFB, and IPIV are not modified.
*          = 'N':  The matrix A will be copied to AFB and factored.
*          = 'E':  The matrix A will be equilibrated if necessary, then
*                  copied to AFB and factored.
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations.
*          = 'N':  A * X = B     (No transpose)
*          = 'T':  A**T * X = B  (Transpose)
*          = 'C':  A**H * X = B  (Transpose)
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  KL      (input) INTEGER
*          The number of subdiagonals within the band of A.  KL >= 0.
*
*  KU      (input) INTEGER
*          The number of superdiagonals within the band of A.  KU >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X.  NRHS >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
*          On entry, the matrix A in band storage, in rows 1 to KL+KU+1.
*          The j-th column of A is stored in the j-th column of the
*          array AB as follows:
*          AB(KU+1+i-j,j) = A(i,j) for max(1,j-KU)<=i<=min(N,j+kl)
*
*          If FACT = 'F' and EQUED is not 'N', then A must have been
*          equilibrated by the scaling factors in R and/or C.  AB is not
*          modified if FACT = 'F' or 'N', or if FACT = 'E' and
*          EQUED = 'N' on exit.
*
*          On exit, if EQUED .ne. 'N', A is scaled as follows:
*          EQUED = 'R':  A := diag(R) * A
*          EQUED = 'C':  A := A * diag(C)
*          EQUED = 'B':  A := diag(R) * A * diag(C).
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KL+KU+1.
*
*  AFB     (input or output) DOUBLE PRECISION array, dimension (LDAFB,N)
*          If FACT = 'F', then AFB is an input argument and on entry
*          contains details of the LU factorization of the band matrix
*          A, as computed by DGBTRF.  U is stored as an upper triangular
*          band matrix with KL+KU superdiagonals in rows 1 to KL+KU+1,
*          and the multipliers used during the factorization are stored
*          in rows KL+KU+2 to 2*KL+KU+1.  If EQUED .ne. 'N', then AFB is
*          the factored form of the equilibrated matrix A.
*
*          If FACT = 'N', then AFB is an output argument and on exit
*          returns details of the LU factorization of A.
*
*          If FACT = 'E', then AFB is an output argument and on exit
*          returns details of the LU factorization of the equilibrated
*          matrix A (see the description of AB for the form of the
*          equilibrated matrix).
*
*  LDAFB   (input) INTEGER
*          The leading dimension of the array AFB.  LDAFB >= 2*KL+KU+1.
*
*  IPIV    (input or output) INTEGER array, dimension (N)
*          If FACT = 'F', then IPIV is an input argument and on entry
*          contains the pivot indices from the factorization A = L*U
*          as computed by DGBTRF; row i of the matrix was interchanged
*          with row IPIV(i).
*
*          If FACT = 'N', then IPIV is an output argument and on exit
*          contains the pivot indices from the factorization A = L*U
*          of the original matrix A.
*
*          If FACT = 'E', then IPIV is an output argument and on exit
*          contains the pivot indices from the factorization A = L*U
*          of the equilibrated matrix A.
*
*  EQUED   (input or output) CHARACTER*1
*          Specifies the form of equilibration that was done.
*          = 'N':  No equilibration (always true if FACT = 'N').
*          = 'R':  Row equilibration, i.e., A has been premultiplied by
*                  diag(R).
*          = 'C':  Column equilibration, i.e., A has been postmultiplied
*                  by diag(C).
*          = 'B':  Both row and column equilibration, i.e., A has been
*                  replaced by diag(R) * A * diag(C).
*          EQUED is an input argument if FACT = 'F'; otherwise, it is an
*          output argument.
*
*  R       (input or output) DOUBLE PRECISION array, dimension (N)
*          The row scale factors for A.  If EQUED = 'R' or 'B', A is
*          multiplied on the left by diag(R); if EQUED = 'N' or 'C', R
*          is not accessed.  R is an input argument if FACT = 'F';
*          otherwise, R is an output argument.  If FACT = 'F' and
*          EQUED = 'R' or 'B', each element of R must be positive.
*
*  C       (input or output) DOUBLE PRECISION array, dimension (N)
*          The column scale factors for A.  If EQUED = 'C' or 'B', A is
*          multiplied on the right by diag(C); if EQUED = 'N' or 'R', C
*          is not accessed.  C is an input argument if FACT = 'F';
*          otherwise, C is an output argument.  If FACT = 'F' and
*          EQUED = 'C' or 'B', each element of C must be positive.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the right hand side matrix B.
*          On exit,
*          if EQUED = 'N', B is not modified;
*          if TRANS = 'N' and EQUED = 'R' or 'B', B is overwritten by
*          diag(R)*B;
*          if TRANS = 'T' or 'C' and EQUED = 'C' or 'B', B is
*          overwritten by diag(C)*B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X
*          to the original system of equations.  Note that A and B are
*          modified on exit if EQUED .ne. 'N', and the solution to the
*          equilibrated system is inv(diag(C))*X if TRANS = 'N' and
*          EQUED = 'C' or 'B', or inv(diag(R))*X if TRANS = 'T' or 'C'
*          and EQUED = 'R' or 'B'.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  RCOND   (output) DOUBLE PRECISION
*          The estimate of the reciprocal condition number of the matrix
*          A after equilibration (if done).  If RCOND is less than the
*          machine precision (in particular, if RCOND = 0), the matrix
*          is singular to working precision.  This condition is
*          indicated by a return code of INFO > 0.
*
*  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bound for each solution vector
*          X(j) (the j-th column of the solution matrix X).
*          If XTRUE is the true solution corresponding to X(j), FERR(j)
*          is an estimated upper bound for the magnitude of the largest
*          element in (X(j) - XTRUE) divided by the magnitude of the
*          largest element in X(j).  The estimate is as reliable as
*          the estimate for RCOND, and is almost always a slight
*          overestimate of the true error.
*
*  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector X(j) (i.e., the smallest relative change in
*          any element of A or B that makes X(j) an exact solution).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (3*N)
*          On exit, WORK(1) contains the reciprocal pivot growth
*          factor norm(A)/norm(U). The "max absolute element" norm is
*          used. If WORK(1) is much less than 1, then the stability
*          of the LU factorization of the (equilibrated) matrix A
*          could be poor. This also means that the solution X, condition
*          estimator RCOND, and forward error bound FERR could be
*          unreliable. If factorization fails with 0<INFO<=N, then
*          WORK(1) contains the reciprocal pivot growth factor for the
*          leading INFO columns of A.
*
*  IWORK   (workspace) INTEGER array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, and i is
*                <= N:  U(i,i) is exactly zero.  The factorization
*                       has been completed, but the factor U is exactly
*                       singular, so the solution and error bounds
*                       could not be computed. RCOND = 0 is returned.
*                = N+1: U is nonsingular, but RCOND is less than machine
*                       precision, meaning that the matrix is singular
*                       to working precision.  Nevertheless, the
*                       solution and error bounds are computed because
*                       there are a number of situations where the
*                       computed solution can be more accurate than the
*                       value of RCOND would suggest.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            COLEQU, EQUIL, NOFACT, NOTRAN, ROWEQU
      CHARACTER          NORM
      INTEGER            I, INFEQU, J, J1, J2
      DOUBLE PRECISION   AMAX, ANORM, BIGNUM, COLCND, RCMAX, RCMIN,
     $                   ROWCND, RPVGRW, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANGB, DLANTB
      EXTERNAL           LSAME, DLAMCH, DLANGB, DLANTB
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGBCON, DGBEQU, DGBRFS, DGBTRF, DGBTRS,
     $                   DLACPY, DLAQGB, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      NOFACT = LSAME( FACT, 'N' )
      EQUIL = LSAME( FACT, 'E' )
      NOTRAN = LSAME( TRANS, 'N' )
      IF( NOFACT .OR. EQUIL ) THEN
         EQUED = 'N'
         ROWEQU = .FALSE.
         COLEQU = .FALSE.
      ELSE
         ROWEQU = LSAME( EQUED, 'R' ) .OR. LSAME( EQUED, 'B' )
         COLEQU = LSAME( EQUED, 'C' ) .OR. LSAME( EQUED, 'B' )
         SMLNUM = DLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
      END IF
*
*     Test the input parameters.
*
      IF( .NOT.NOFACT .AND. .NOT.EQUIL .AND. .NOT.LSAME( FACT, 'F' ) )
     $     THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $         LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( KL.LT.0 ) THEN
         INFO = -4
      ELSE IF( KU.LT.0 ) THEN
         INFO = -5
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -6
      ELSE IF( LDAB.LT.KL+KU+1 ) THEN
         INFO = -8
      ELSE IF( LDAFB.LT.2*KL+KU+1 ) THEN
         INFO = -10
      ELSE IF( LSAME( FACT, 'F' ) .AND. .NOT.
     $         ( ROWEQU .OR. COLEQU .OR. LSAME( EQUED, 'N' ) ) ) THEN
         INFO = -12
      ELSE
         IF( ROWEQU ) THEN
            RCMIN = BIGNUM
            RCMAX = ZERO
            DO 10 J = 1, N
               RCMIN = MIN( RCMIN, R( J ) )
               RCMAX = MAX( RCMAX, R( J ) )
   10       CONTINUE
            IF( RCMIN.LE.ZERO ) THEN
               INFO = -13
            ELSE IF( N.GT.0 ) THEN
               ROWCND = MAX( RCMIN, SMLNUM ) / MIN( RCMAX, BIGNUM )
            ELSE
               ROWCND = ONE
            END IF
         END IF
         IF( COLEQU .AND. INFO.EQ.0 ) THEN
            RCMIN = BIGNUM
            RCMAX = ZERO
            DO 20 J = 1, N
               RCMIN = MIN( RCMIN, C( J ) )
               RCMAX = MAX( RCMAX, C( J ) )
   20       CONTINUE
            IF( RCMIN.LE.ZERO ) THEN
               INFO = -14
            ELSE IF( N.GT.0 ) THEN
               COLCND = MAX( RCMIN, SMLNUM ) / MIN( RCMAX, BIGNUM )
            ELSE
               COLCND = ONE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            IF( LDB.LT.MAX( 1, N ) ) THEN
               INFO = -16
            ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
               INFO = -18
            END IF
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGBSVX', -INFO )
         RETURN
      END IF
*
      IF( EQUIL ) THEN
*
*        Compute row and column scalings to equilibrate the matrix A.
*
         CALL DGBEQU( N, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,
     $                AMAX, INFEQU )
         IF( INFEQU.EQ.0 ) THEN
*
*           Equilibrate the matrix.
*
            CALL DLAQGB( N, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND,
     $                   AMAX, EQUED )
            ROWEQU = LSAME( EQUED, 'R' ) .OR. LSAME( EQUED, 'B' )
            COLEQU = LSAME( EQUED, 'C' ) .OR. LSAME( EQUED, 'B' )
         END IF
      END IF
*
*     Scale the right hand side.
*
      IF( NOTRAN ) THEN
         IF( ROWEQU ) THEN
            DO 40 J = 1, NRHS
               DO 30 I = 1, N
                  B( I, J ) = R( I )*B( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
      ELSE IF( COLEQU ) THEN
         DO 60 J = 1, NRHS
            DO 50 I = 1, N
               B( I, J ) = C( I )*B( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
*
      IF( NOFACT .OR. EQUIL ) THEN
*
*        Compute the LU factorization of the band matrix A.
*
         DO 70 J = 1, N
            J1 = MAX( J-KU, 1 )
            J2 = MIN( J+KL, N )
            CALL DCOPY( J2-J1+1, AB( KU+1-J+J1, J ), 1,
     $                  AFB( KL+KU+1-J+J1, J ), 1 )
   70    CONTINUE
*
         CALL DGBTRF( N, N, KL, KU, AFB, LDAFB, IPIV, INFO )
*
*        Return if INFO is non-zero.
*
         IF( INFO.GT.0 ) THEN
*
*           Compute the reciprocal pivot growth factor of the
*           leading rank-deficient INFO columns of A.
*
            ANORM = ZERO
            DO 90 J = 1, INFO
               DO 80 I = MAX( KU+2-J, 1 ), MIN( N+KU+1-J, KL+KU+1 )
                  ANORM = MAX( ANORM, ABS( AB( I, J ) ) )
   80          CONTINUE
   90       CONTINUE
            RPVGRW = DLANTB( 'M', 'U', 'N', INFO, MIN( INFO-1, KL+KU ),
     $                       AFB( MAX( 1, KL+KU+2-INFO ), 1 ), LDAFB,
     $                       WORK )
            IF( RPVGRW.EQ.ZERO ) THEN
               RPVGRW = ONE
            ELSE
               RPVGRW = ANORM / RPVGRW
            END IF
            WORK( 1 ) = RPVGRW
            RCOND = ZERO
            RETURN
         END IF
      END IF
*
*     Compute the norm of the matrix A and the
*     reciprocal pivot growth factor RPVGRW.
*
      IF( NOTRAN ) THEN
         NORM = '1'
      ELSE
         NORM = 'I'
      END IF
      ANORM = DLANGB( NORM, N, KL, KU, AB, LDAB, WORK )
      RPVGRW = DLANTB( 'M', 'U', 'N', N, KL+KU, AFB, LDAFB, WORK )
      IF( RPVGRW.EQ.ZERO ) THEN
         RPVGRW = ONE
      ELSE
         RPVGRW = DLANGB( 'M', N, KL, KU, AB, LDAB, WORK ) / RPVGRW
      END IF
*
*     Compute the reciprocal of the condition number of A.
*
      CALL DGBCON( NORM, N, KL, KU, AFB, LDAFB, IPIV, ANORM, RCOND,
     $             WORK, IWORK, INFO )
*
*     Compute the solution matrix X.
*
      CALL DLACPY( 'Full', N, NRHS, B, LDB, X, LDX )
      CALL DGBTRS( TRANS, N, KL, KU, NRHS, AFB, LDAFB, IPIV, X, LDX,
     $             INFO )
*
*     Use iterative refinement to improve the computed solution and
*     compute error bounds and backward error estimates for it.
*
      CALL DGBRFS( TRANS, N, KL, KU, NRHS, AB, LDAB, AFB, LDAFB, IPIV,
     $             B, LDB, X, LDX, FERR, BERR, WORK, IWORK, INFO )
*
*     Transform the solution matrix X to a solution of the original
*     system.
*
      IF( NOTRAN ) THEN
         IF( COLEQU ) THEN
            DO 110 J = 1, NRHS
               DO 100 I = 1, N
                  X( I, J ) = C( I )*X( I, J )
  100          CONTINUE
  110       CONTINUE
            DO 120 J = 1, NRHS
               FERR( J ) = FERR( J ) / COLCND
  120       CONTINUE
         END IF
      ELSE IF( ROWEQU ) THEN
         DO 140 J = 1, NRHS
            DO 130 I = 1, N
               X( I, J ) = R( I )*X( I, J )
  130       CONTINUE
  140    CONTINUE
         DO 150 J = 1, NRHS
            FERR( J ) = FERR( J ) / ROWCND
  150    CONTINUE
      END IF
*
*     Set INFO = N+1 if the matrix is singular to working precision.
*
      IF( RCOND.LT.DLAMCH( 'Epsilon' ) )
     $   INFO = N + 1
*
      WORK( 1 ) = RPVGRW
      RETURN
*
*     End of DGBSVX
*
      END
      SUBROUTINE DGEES( JOBVS, SORT, SELECT, N, A, LDA, SDIM, WR, WI,
     $                  VS, LDVS, WORK, LWORK, BWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVS, SORT
      INTEGER            INFO, LDA, LDVS, LWORK, N, SDIM
*     ..
*     .. Array Arguments ..
      LOGICAL            BWORK( * )
      DOUBLE PRECISION   A( LDA, * ), VS( LDVS, * ), WI( * ), WORK( * ),
     $                   WR( * )
*     ..
*     .. Function Arguments ..
      LOGICAL            SELECT
      EXTERNAL           SELECT
*     ..
*
*  Purpose
*  =======
*
*  DGEES computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues, the real Schur form T, and, optionally, the matrix of
*  Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
*
*  Optionally, it also orders the eigenvalues on the diagonal of the
*  real Schur form so that selected eigenvalues are at the top left.
*  The leading columns of Z then form an orthonormal basis for the
*  invariant subspace corresponding to the selected eigenvalues.
*
*  A matrix is in real Schur form if it is upper quasi-triangular with
*  1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in the
*  form
*          [  a  b  ]
*          [  c  a  ]
*
*  where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
*
*  Arguments
*  =========
*
*  JOBVS   (input) CHARACTER*1
*          = 'N': Schur vectors are not computed;
*          = 'V': Schur vectors are computed.
*
*  SORT    (input) CHARACTER*1
*          Specifies whether or not to order the eigenvalues on the
*          diagonal of the Schur form.
*          = 'N': Eigenvalues are not ordered;
*          = 'S': Eigenvalues are ordered (see SELECT).
*
*  SELECT  (external procedure) LOGICAL FUNCTION of two DOUBLE PRECISION arguments
*          SELECT must be declared EXTERNAL in the calling subroutine.
*          If SORT = 'S', SELECT is used to select eigenvalues to sort
*          to the top left of the Schur form.
*          If SORT = 'N', SELECT is not referenced.
*          An eigenvalue WR(j)+sqrt(-1)*WI(j) is selected if
*          SELECT(WR(j),WI(j)) is true; i.e., if either one of a complex
*          conjugate pair of eigenvalues is selected, then both complex
*          eigenvalues are selected.
*          Note that a selected complex eigenvalue may no longer
*          satisfy SELECT(WR(j),WI(j)) = .TRUE. after ordering, since
*          ordering may change the value of complex eigenvalues
*          (especially if the eigenvalue is ill-conditioned); in this
*          case INFO is set to N+2 (see INFO below).
*
*  N       (input) INTEGER
*          The order of the matrix A. N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the N-by-N matrix A.
*          On exit, A has been overwritten by its real Schur form T.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  SDIM    (output) INTEGER
*          If SORT = 'N', SDIM = 0.
*          If SORT = 'S', SDIM = number of eigenvalues (after sorting)
*                         for which SELECT is true. (Complex conjugate
*                         pairs for which SELECT is true for either
*                         eigenvalue count as 2.)
*
*  WR      (output) DOUBLE PRECISION array, dimension (N)
*  WI      (output) DOUBLE PRECISION array, dimension (N)
*          WR and WI contain the real and imaginary parts,
*          respectively, of the computed eigenvalues in the same order
*          that they appear on the diagonal of the output Schur form T.
*          Complex conjugate pairs of eigenvalues will appear
*          consecutively with the eigenvalue having the positive
*          imaginary part first.
*
*  VS      (output) DOUBLE PRECISION array, dimension (LDVS,N)
*          If JOBVS = 'V', VS contains the orthogonal matrix Z of Schur
*          vectors.
*          If JOBVS = 'N', VS is not referenced.
*
*  LDVS    (input) INTEGER
*          The leading dimension of the array VS.  LDVS >= 1; if
*          JOBVS = 'V', LDVS >= N.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) contains the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,3*N).
*          For good performance, LWORK must generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  BWORK   (workspace) LOGICAL array, dimension (N)
*          Not referenced if SORT = 'N'.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*          > 0: if INFO = i, and i is
*             <= N: the QR algorithm failed to compute all the
*                   eigenvalues; elements 1:ILO-1 and i+1:N of WR and WI
*                   contain those eigenvalues which have converged; if
*                   JOBVS = 'V', VS contains the matrix which reduces A
*                   to its partially converged Schur form.
*             = N+1: the eigenvalues could not be reordered because some
*                   eigenvalues were too close to separate (the problem
*                   is very ill-conditioned);
*             = N+2: after reordering, roundoff changed values of some
*                   complex eigenvalues so that leading eigenvalues in
*                   the Schur form no longer satisfy SELECT=.TRUE.  This
*                   could also be caused by underflow due to scaling.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            CURSL, LASTSL, LQUERY, LST2SL, SCALEA, WANTST,
     $                   WANTVS
      INTEGER            HSWORK, I, I1, I2, IBAL, ICOND, IERR, IEVAL,
     $                   IHI, ILO, INXT, IP, ITAU, IWRK, MAXWRK, MINWRK
      DOUBLE PRECISION   ANRM, BIGNUM, CSCALE, EPS, S, SEP, SMLNUM
*     ..
*     .. Local Arrays ..
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEBAK, DGEBAL, DGEHRD, DHSEQR, DLACPY,
     $                   DLABAD, DLASCL, DORGHR, DSWAP, DTRSEN, XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      WANTVS = LSAME( JOBVS, 'V' )
      WANTST = LSAME( SORT, 'S' )
      IF( ( .NOT.WANTVS ) .AND. ( .NOT.LSAME( JOBVS, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( ( .NOT.WANTST ) .AND. ( .NOT.LSAME( SORT, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDVS.LT.1 .OR. ( WANTVS .AND. LDVS.LT.N ) ) THEN
         INFO = -11
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.
*       HSWORK refers to the workspace preferred by DHSEQR, as
*       calculated below. HSWORK is computed assuming ILO=1 and IHI=N,
*       the worst case.)
*
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            MINWRK = 1
            MAXWRK = 1
         ELSE
            MAXWRK = 2*N + N*ILAENV( 1, 'DGEHRD', ' ', N, 1, N, 0 )
            MINWRK = 3*N
*
            CALL DHSEQR( 'S', JOBVS, N, 1, N, A, LDA, WR, WI, VS, LDVS,
     $             WORK, -1, IEVAL )
            HSWORK = WORK( 1 )
*
            IF( .NOT.WANTVS ) THEN
               MAXWRK = MAX( MAXWRK, N + HSWORK )
            ELSE
               MAXWRK = MAX( MAXWRK, 2*N + ( N - 1 )*ILAENV( 1,
     $                       'DORGHR', ' ', N, 1, N, -1 ) )
               MAXWRK = MAX( MAXWRK, N + HSWORK )
            END IF
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -13
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEES ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         SDIM = 0
         RETURN
      END IF
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
      ANRM = DLANGE( 'M', N, N, A, LDA, DUM )
      SCALEA = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = SMLNUM
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = BIGNUM
      END IF
      IF( SCALEA )
     $   CALL DLASCL( 'G', 0, 0, ANRM, CSCALE, N, N, A, LDA, IERR )
*
*     Permute the matrix to make it more nearly triangular
*     (Workspace: need N)
*
      IBAL = 1
      CALL DGEBAL( 'P', N, A, LDA, ILO, IHI, WORK( IBAL ), IERR )
*
*     Reduce to upper Hessenberg form
*     (Workspace: need 3*N, prefer 2*N+N*NB)
*
      ITAU = N + IBAL
      IWRK = N + ITAU
      CALL DGEHRD( N, ILO, IHI, A, LDA, WORK( ITAU ), WORK( IWRK ),
     $             LWORK-IWRK+1, IERR )
*
      IF( WANTVS ) THEN
*
*        Copy Householder vectors to VS
*
         CALL DLACPY( 'L', N, N, A, LDA, VS, LDVS )
*
*        Generate orthogonal matrix in VS
*        (Workspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*
         CALL DORGHR( N, ILO, IHI, VS, LDVS, WORK( ITAU ), WORK( IWRK ),
     $                LWORK-IWRK+1, IERR )
      END IF
*
      SDIM = 0
*
*     Perform QR iteration, accumulating Schur vectors in VS if desired
*     (Workspace: need N+1, prefer N+HSWORK (see comments) )
*
      IWRK = ITAU
      CALL DHSEQR( 'S', JOBVS, N, ILO, IHI, A, LDA, WR, WI, VS, LDVS,
     $             WORK( IWRK ), LWORK-IWRK+1, IEVAL )
      IF( IEVAL.GT.0 )
     $   INFO = IEVAL
*
*     Sort eigenvalues if desired
*
      IF( WANTST .AND. INFO.EQ.0 ) THEN
         IF( SCALEA ) THEN
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N, 1, WR, N, IERR )
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N, 1, WI, N, IERR )
         END IF
         DO 10 I = 1, N
            BWORK( I ) = SELECT( WR( I ), WI( I ) )
   10    CONTINUE
*
*        Reorder eigenvalues and transform Schur vectors
*        (Workspace: none needed)
*
         CALL DTRSEN( 'N', JOBVS, BWORK, N, A, LDA, VS, LDVS, WR, WI,
     $                SDIM, S, SEP, WORK( IWRK ), LWORK-IWRK+1, IDUM, 1,
     $                ICOND )
         IF( ICOND.GT.0 )
     $      INFO = N + ICOND
      END IF
*
      IF( WANTVS ) THEN
*
*        Undo balancing
*        (Workspace: need N)
*
         CALL DGEBAK( 'P', 'R', N, ILO, IHI, WORK( IBAL ), N, VS, LDVS,
     $                IERR )
      END IF
*
      IF( SCALEA ) THEN
*
*        Undo scaling for the Schur form of A
*
         CALL DLASCL( 'H', 0, 0, CSCALE, ANRM, N, N, A, LDA, IERR )
         CALL DCOPY( N, A, LDA+1, WR, 1 )
         IF( CSCALE.EQ.SMLNUM ) THEN
*
*           If scaling back towards underflow, adjust WI if an
*           offdiagonal element of a 2-by-2 block in the Schur form
*           underflows.
*
            IF( IEVAL.GT.0 ) THEN
               I1 = IEVAL + 1
               I2 = IHI - 1
               CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, ILO-1, 1, WI,
     $                      MAX( ILO-1, 1 ), IERR )
            ELSE IF( WANTST ) THEN
               I1 = 1
               I2 = N - 1
            ELSE
               I1 = ILO
               I2 = IHI - 1
            END IF
            INXT = I1 - 1
            DO 20 I = I1, I2
               IF( I.LT.INXT )
     $            GO TO 20
               IF( WI( I ).EQ.ZERO ) THEN
                  INXT = I + 1
               ELSE
                  IF( A( I+1, I ).EQ.ZERO ) THEN
                     WI( I ) = ZERO
                     WI( I+1 ) = ZERO
                  ELSE IF( A( I+1, I ).NE.ZERO .AND. A( I, I+1 ).EQ.
     $                     ZERO ) THEN
                     WI( I ) = ZERO
                     WI( I+1 ) = ZERO
                     IF( I.GT.1 )
     $                  CALL DSWAP( I-1, A( 1, I ), 1, A( 1, I+1 ), 1 )
                     IF( N.GT.I+1 )
     $                  CALL DSWAP( N-I-1, A( I, I+2 ), LDA,
     $                              A( I+1, I+2 ), LDA )
                     IF( WANTVS ) THEN
                        CALL DSWAP( N, VS( 1, I ), 1, VS( 1, I+1 ), 1 )
                     END IF
                     A( I, I+1 ) = A( I+1, I )
                     A( I+1, I ) = ZERO
                  END IF
                  INXT = I + 2
               END IF
   20       CONTINUE
         END IF
*
*        Undo scaling for the imaginary part of the eigenvalues
*
         CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N-IEVAL, 1,
     $                WI( IEVAL+1 ), MAX( N-IEVAL, 1 ), IERR )
      END IF
*
      IF( WANTST .AND. INFO.EQ.0 ) THEN
*
*        Check if reordering successful
*
         LASTSL = .TRUE.
         LST2SL = .TRUE.
         SDIM = 0
         IP = 0
         DO 30 I = 1, N
            CURSL = SELECT( WR( I ), WI( I ) )
            IF( WI( I ).EQ.ZERO ) THEN
               IF( CURSL )
     $            SDIM = SDIM + 1
               IP = 0
               IF( CURSL .AND. .NOT.LASTSL )
     $            INFO = N + 2
            ELSE
               IF( IP.EQ.1 ) THEN
*
*                 Last eigenvalue of conjugate pair
*
                  CURSL = CURSL .OR. LASTSL
                  LASTSL = CURSL
                  IF( CURSL )
     $               SDIM = SDIM + 2
                  IP = -1
                  IF( CURSL .AND. .NOT.LST2SL )
     $               INFO = N + 2
               ELSE
*
*                 First eigenvalue of conjugate pair
*
                  IP = 1
               END IF
            END IF
            LST2SL = LASTSL
            LASTSL = CURSL
   30    CONTINUE
      END IF
*
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of DGEES
*
      END
      SUBROUTINE DGEESX( JOBVS, SORT, SELECT, SENSE, N, A, LDA, SDIM,
     $                   WR, WI, VS, LDVS, RCONDE, RCONDV, WORK, LWORK,
     $                   IWORK, LIWORK, BWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVS, SENSE, SORT
      INTEGER            INFO, LDA, LDVS, LIWORK, LWORK, N, SDIM
      DOUBLE PRECISION   RCONDE, RCONDV
*     ..
*     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), VS( LDVS, * ), WI( * ), WORK( * ),
     $                   WR( * )
*     ..
*     .. Function Arguments ..
      LOGICAL            SELECT
      EXTERNAL           SELECT
*     ..
*
*  Purpose
*  =======
*
*  DGEESX computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues, the real Schur form T, and, optionally, the matrix of
*  Schur vectors Z.  This gives the Schur factorization A = Z*T*(Z**T).
*
*  Optionally, it also orders the eigenvalues on the diagonal of the
*  real Schur form so that selected eigenvalues are at the top left;
*  computes a reciprocal condition number for the average of the
*  selected eigenvalues (RCONDE); and computes a reciprocal condition
*  number for the right invariant subspace corresponding to the
*  selected eigenvalues (RCONDV).  The leading columns of Z form an
*  orthonormal basis for this invariant subspace.
*
*  For further explanation of the reciprocal condition numbers RCONDE
*  and RCONDV, see Section 4.10 of the LAPACK Users' Guide (where
*  these quantities are called s and sep respectively).
*
*  A real matrix is in real Schur form if it is upper quasi-triangular
*  with 1-by-1 and 2-by-2 blocks. 2-by-2 blocks will be standardized in
*  the form
*            [  a  b  ]
*            [  c  a  ]
*
*  where b*c < 0. The eigenvalues of such a block are a +- sqrt(bc).
*
*  Arguments
*  =========
*
*  JOBVS   (input) CHARACTER*1
*          = 'N': Schur vectors are not computed;
*          = 'V': Schur vectors are computed.
*
*  SORT    (input) CHARACTER*1
*          Specifies whether or not to order the eigenvalues on the
*          diagonal of the Schur form.
*          = 'N': Eigenvalues are not ordered;
*          = 'S': Eigenvalues are ordered (see SELECT).
*
*  SELECT  (external procedure) LOGICAL FUNCTION of two DOUBLE PRECISION arguments
*          SELECT must be declared EXTERNAL in the calling subroutine.
*          If SORT = 'S', SELECT is used to select eigenvalues to sort
*          to the top left of the Schur form.
*          If SORT = 'N', SELECT is not referenced.
*          An eigenvalue WR(j)+sqrt(-1)*WI(j) is selected if
*          SELECT(WR(j),WI(j)) is true; i.e., if either one of a
*          complex conjugate pair of eigenvalues is selected, then both
*          are.  Note that a selected complex eigenvalue may no longer
*          satisfy SELECT(WR(j),WI(j)) = .TRUE. after ordering, since
*          ordering may change the value of complex eigenvalues
*          (especially if the eigenvalue is ill-conditioned); in this
*          case INFO may be set to N+3 (see INFO below).
*
*  SENSE   (input) CHARACTER*1
*          Determines which reciprocal condition numbers are computed.
*          = 'N': None are computed;
*          = 'E': Computed for average of selected eigenvalues only;
*          = 'V': Computed for selected right invariant subspace only;
*          = 'B': Computed for both.
*          If SENSE = 'E', 'V' or 'B', SORT must equal 'S'.
*
*  N       (input) INTEGER
*          The order of the matrix A. N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the N-by-N matrix A.
*          On exit, A is overwritten by its real Schur form T.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  SDIM    (output) INTEGER
*          If SORT = 'N', SDIM = 0.
*          If SORT = 'S', SDIM = number of eigenvalues (after sorting)
*                         for which SELECT is true. (Complex conjugate
*                         pairs for which SELECT is true for either
*                         eigenvalue count as 2.)
*
*  WR      (output) DOUBLE PRECISION array, dimension (N)
*  WI      (output) DOUBLE PRECISION array, dimension (N)
*          WR and WI contain the real and imaginary parts, respectively,
*          of the computed eigenvalues, in the same order that they
*          appear on the diagonal of the output Schur form T.  Complex
*          conjugate pairs of eigenvalues appear consecutively with the
*          eigenvalue having the positive imaginary part first.
*
*  VS      (output) DOUBLE PRECISION array, dimension (LDVS,N)
*          If JOBVS = 'V', VS contains the orthogonal matrix Z of Schur
*          vectors.
*          If JOBVS = 'N', VS is not referenced.
*
*  LDVS    (input) INTEGER
*          The leading dimension of the array VS.  LDVS >= 1, and if
*          JOBVS = 'V', LDVS >= N.
*
*  RCONDE  (output) DOUBLE PRECISION
*          If SENSE = 'E' or 'B', RCONDE contains the reciprocal
*          condition number for the average of the selected eigenvalues.
*          Not referenced if SENSE = 'N' or 'V'.
*
*  RCONDV  (output) DOUBLE PRECISION
*          If SENSE = 'V' or 'B', RCONDV contains the reciprocal
*          condition number for the selected right invariant subspace.
*          Not referenced if SENSE = 'N' or 'E'.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,3*N).
*          Also, if SENSE = 'E' or 'V' or 'B',
*          LWORK >= N+2*SDIM*(N-SDIM), where SDIM is the number of
*          selected eigenvalues computed by this routine.  Note that
*          N+2*SDIM*(N-SDIM) <= N+N*N/2. Note also that an error is only
*          returned if LWORK < max(1,3*N), but if SENSE = 'E' or 'V' or
*          'B' this may not be large enough.
*          For good performance, LWORK must generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates upper bounds on the optimal sizes of the
*          arrays WORK and IWORK, returns these values as the first
*          entries of the WORK and IWORK arrays, and no error messages
*          related to LWORK or LIWORK are issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          LIWORK >= 1; if SENSE = 'V' or 'B', LIWORK >= SDIM*(N-SDIM).
*          Note that SDIM*(N-SDIM) <= N*N/4. Note also that an error is
*          only returned if LIWORK < 1, but if SENSE = 'V' or 'B' this
*          may not be large enough.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates upper bounds on the optimal sizes of
*          the arrays WORK and IWORK, returns these values as the first
*          entries of the WORK and IWORK arrays, and no error messages
*          related to LWORK or LIWORK are issued by XERBLA.
*
*  BWORK   (workspace) LOGICAL array, dimension (N)
*          Not referenced if SORT = 'N'.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value.
*          > 0: if INFO = i, and i is
*             <= N: the QR algorithm failed to compute all the
*                   eigenvalues; elements 1:ILO-1 and i+1:N of WR and WI
*                   contain those eigenvalues which have converged; if
*                   JOBVS = 'V', VS contains the transformation which
*                   reduces A to its partially converged Schur form.
*             = N+1: the eigenvalues could not be reordered because some
*                   eigenvalues were too close to separate (the problem
*                   is very ill-conditioned);
*             = N+2: after reordering, roundoff changed values of some
*                   complex eigenvalues so that leading eigenvalues in
*                   the Schur form no longer satisfy SELECT=.TRUE.  This
*                   could also be caused by underflow due to scaling.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            CURSL, LASTSL, LQUERY, LST2SL, SCALEA, WANTSB,
     $                   WANTSE, WANTSN, WANTST, WANTSV, WANTVS
      INTEGER            HSWORK, I, I1, I2, IBAL, ICOND, IERR, IEVAL,
     $                   IHI, ILO, INXT, IP, ITAU, IWRK, LIWRK, LWRK,
     $                   MAXWRK, MINWRK
      DOUBLE PRECISION   ANRM, BIGNUM, CSCALE, EPS, SMLNUM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEBAK, DGEBAL, DGEHRD, DHSEQR, DLACPY,
     $                   DLASCL, DORGHR, DSWAP, DTRSEN, XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLABAD, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      WANTVS = LSAME( JOBVS, 'V' )
      WANTST = LSAME( SORT, 'S' )
      WANTSN = LSAME( SENSE, 'N' )
      WANTSE = LSAME( SENSE, 'E' )
      WANTSV = LSAME( SENSE, 'V' )
      WANTSB = LSAME( SENSE, 'B' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
      IF( ( .NOT.WANTVS ) .AND. ( .NOT.LSAME( JOBVS, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( ( .NOT.WANTST ) .AND. ( .NOT.LSAME( SORT, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( WANTSN .OR. WANTSE .OR. WANTSV .OR. WANTSB ) .OR.
     $         ( .NOT.WANTST .AND. .NOT.WANTSN ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDVS.LT.1 .OR. ( WANTVS .AND. LDVS.LT.N ) ) THEN
         INFO = -12
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "RWorkspace:" describe the
*       minimal amount of real workspace needed at that point in the
*       code, as well as the preferred amount for good performance.
*       IWorkspace refers to integer workspace.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.
*       HSWORK refers to the workspace preferred by DHSEQR, as
*       calculated below. HSWORK is computed assuming ILO=1 and IHI=N,
*       the worst case.
*       If SENSE = 'E', 'V' or 'B', then the amount of workspace needed
*       depends on SDIM, which is computed by the routine DTRSEN later
*       in the code.)
*
      IF( INFO.EQ.0 ) THEN
         LIWRK = 1
         IF( N.EQ.0 ) THEN
            MINWRK = 1
            LWRK = 1
         ELSE
            MAXWRK = 2*N + N*ILAENV( 1, 'DGEHRD', ' ', N, 1, N, 0 )
            MINWRK = 3*N
*
            CALL DHSEQR( 'S', JOBVS, N, 1, N, A, LDA, WR, WI, VS, LDVS,
     $             WORK, -1, IEVAL )
            HSWORK = WORK( 1 )
*
            IF( .NOT.WANTVS ) THEN
               MAXWRK = MAX( MAXWRK, N + HSWORK )
            ELSE
               MAXWRK = MAX( MAXWRK, 2*N + ( N - 1 )*ILAENV( 1,
     $                       'DORGHR', ' ', N, 1, N, -1 ) )
               MAXWRK = MAX( MAXWRK, N + HSWORK )
            END IF
            LWRK = MAXWRK
            IF( .NOT.WANTSN )
     $         LWRK = MAX( LWRK, N + ( N*N )/2 )
            IF( WANTSV .OR. WANTSB )
     $         LIWRK = ( N*N )/4
         END IF
         IWORK( 1 ) = LIWRK
         WORK( 1 ) = LWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -16
         ELSE IF( LIWORK.LT.1 .AND. .NOT.LQUERY ) THEN
            INFO = -18
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEESX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         SDIM = 0
         RETURN
      END IF
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
      ANRM = DLANGE( 'M', N, N, A, LDA, DUM )
      SCALEA = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = SMLNUM
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = BIGNUM
      END IF
      IF( SCALEA )
     $   CALL DLASCL( 'G', 0, 0, ANRM, CSCALE, N, N, A, LDA, IERR )
*
*     Permute the matrix to make it more nearly triangular
*     (RWorkspace: need N)
*
      IBAL = 1
      CALL DGEBAL( 'P', N, A, LDA, ILO, IHI, WORK( IBAL ), IERR )
*
*     Reduce to upper Hessenberg form
*     (RWorkspace: need 3*N, prefer 2*N+N*NB)
*
      ITAU = N + IBAL
      IWRK = N + ITAU
      CALL DGEHRD( N, ILO, IHI, A, LDA, WORK( ITAU ), WORK( IWRK ),
     $             LWORK-IWRK+1, IERR )
*
      IF( WANTVS ) THEN
*
*        Copy Householder vectors to VS
*
         CALL DLACPY( 'L', N, N, A, LDA, VS, LDVS )
*
*        Generate orthogonal matrix in VS
*        (RWorkspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*
         CALL DORGHR( N, ILO, IHI, VS, LDVS, WORK( ITAU ), WORK( IWRK ),
     $                LWORK-IWRK+1, IERR )
      END IF
*
      SDIM = 0
*
*     Perform QR iteration, accumulating Schur vectors in VS if desired
*     (RWorkspace: need N+1, prefer N+HSWORK (see comments) )
*
      IWRK = ITAU
      CALL DHSEQR( 'S', JOBVS, N, ILO, IHI, A, LDA, WR, WI, VS, LDVS,
     $             WORK( IWRK ), LWORK-IWRK+1, IEVAL )
      IF( IEVAL.GT.0 )
     $   INFO = IEVAL
*
*     Sort eigenvalues if desired
*
      IF( WANTST .AND. INFO.EQ.0 ) THEN
         IF( SCALEA ) THEN
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N, 1, WR, N, IERR )
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N, 1, WI, N, IERR )
         END IF
         DO 10 I = 1, N
            BWORK( I ) = SELECT( WR( I ), WI( I ) )
   10    CONTINUE
*
*        Reorder eigenvalues, transform Schur vectors, and compute
*        reciprocal condition numbers
*        (RWorkspace: if SENSE is not 'N', need N+2*SDIM*(N-SDIM)
*                     otherwise, need N )
*        (IWorkspace: if SENSE is 'V' or 'B', need SDIM*(N-SDIM)
*                     otherwise, need 0 )
*
         CALL DTRSEN( SENSE, JOBVS, BWORK, N, A, LDA, VS, LDVS, WR, WI,
     $                SDIM, RCONDE, RCONDV, WORK( IWRK ), LWORK-IWRK+1,
     $                IWORK, LIWORK, ICOND )
         IF( .NOT.WANTSN )
     $      MAXWRK = MAX( MAXWRK, N+2*SDIM*( N-SDIM ) )
         IF( ICOND.EQ.-15 ) THEN
*
*           Not enough real workspace
*
            INFO = -16
         ELSE IF( ICOND.EQ.-17 ) THEN
*
*           Not enough integer workspace
*
            INFO = -18
         ELSE IF( ICOND.GT.0 ) THEN
*
*           DTRSEN failed to reorder or to restore standard Schur form
*
            INFO = ICOND + N
         END IF
      END IF
*
      IF( WANTVS ) THEN
*
*        Undo balancing
*        (RWorkspace: need N)
*
         CALL DGEBAK( 'P', 'R', N, ILO, IHI, WORK( IBAL ), N, VS, LDVS,
     $                IERR )
      END IF
*
      IF( SCALEA ) THEN
*
*        Undo scaling for the Schur form of A
*
         CALL DLASCL( 'H', 0, 0, CSCALE, ANRM, N, N, A, LDA, IERR )
         CALL DCOPY( N, A, LDA+1, WR, 1 )
         IF( ( WANTSV .OR. WANTSB ) .AND. INFO.EQ.0 ) THEN
            DUM( 1 ) = RCONDV
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, 1, 1, DUM, 1, IERR )
            RCONDV = DUM( 1 )
         END IF
         IF( CSCALE.EQ.SMLNUM ) THEN
*
*           If scaling back towards underflow, adjust WI if an
*           offdiagonal element of a 2-by-2 block in the Schur form
*           underflows.
*
            IF( IEVAL.GT.0 ) THEN
               I1 = IEVAL + 1
               I2 = IHI - 1
               CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, ILO-1, 1, WI, N,
     $                      IERR )
            ELSE IF( WANTST ) THEN
               I1 = 1
               I2 = N - 1
            ELSE
               I1 = ILO
               I2 = IHI - 1
            END IF
            INXT = I1 - 1
            DO 20 I = I1, I2
               IF( I.LT.INXT )
     $            GO TO 20
               IF( WI( I ).EQ.ZERO ) THEN
                  INXT = I + 1
               ELSE
                  IF( A( I+1, I ).EQ.ZERO ) THEN
                     WI( I ) = ZERO
                     WI( I+1 ) = ZERO
                  ELSE IF( A( I+1, I ).NE.ZERO .AND. A( I, I+1 ).EQ.
     $                     ZERO ) THEN
                     WI( I ) = ZERO
                     WI( I+1 ) = ZERO
                     IF( I.GT.1 )
     $                  CALL DSWAP( I-1, A( 1, I ), 1, A( 1, I+1 ), 1 )
                     IF( N.GT.I+1 )
     $                  CALL DSWAP( N-I-1, A( I, I+2 ), LDA,
     $                              A( I+1, I+2 ), LDA )
                     CALL DSWAP( N, VS( 1, I ), 1, VS( 1, I+1 ), 1 )
                     A( I, I+1 ) = A( I+1, I )
                     A( I+1, I ) = ZERO
                  END IF
                  INXT = I + 2
               END IF
   20       CONTINUE
         END IF
         CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N-IEVAL, 1,
     $                WI( IEVAL+1 ), MAX( N-IEVAL, 1 ), IERR )
      END IF
*
      IF( WANTST .AND. INFO.EQ.0 ) THEN
*
*        Check if reordering successful
*
         LASTSL = .TRUE.
         LST2SL = .TRUE.
         SDIM = 0
         IP = 0
         DO 30 I = 1, N
            CURSL = SELECT( WR( I ), WI( I ) )
            IF( WI( I ).EQ.ZERO ) THEN
               IF( CURSL )
     $            SDIM = SDIM + 1
               IP = 0
               IF( CURSL .AND. .NOT.LASTSL )
     $            INFO = N + 2
            ELSE
               IF( IP.EQ.1 ) THEN
*
*                 Last eigenvalue of conjugate pair
*
                  CURSL = CURSL .OR. LASTSL
                  LASTSL = CURSL
                  IF( CURSL )
     $               SDIM = SDIM + 2
                  IP = -1
                  IF( CURSL .AND. .NOT.LST2SL )
     $               INFO = N + 2
               ELSE
*
*                 First eigenvalue of conjugate pair
*
                  IP = 1
               END IF
            END IF
            LST2SL = LASTSL
            LASTSL = CURSL
   30    CONTINUE
      END IF
*
      WORK( 1 ) = MAXWRK
      IF( WANTSV .OR. WANTSB ) THEN
         IWORK( 1 ) = MAX( 1, SDIM*( N-SDIM ) )
      ELSE
         IWORK( 1 ) = 1
      END IF
*
      RETURN
*
*     End of DGEESX
*
      END
      SUBROUTINE DGEEV( JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR,
     $                  LDVR, WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVL, JOBVR
      INTEGER            INFO, LDA, LDVL, LDVR, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   WI( * ), WORK( * ), WR( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEEV computes for an N-by-N real nonsymmetric matrix A, the
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
*          = 'V': left eigenvectors of A are computed.
*
*  JOBVR   (input) CHARACTER*1
*          = 'N': right eigenvectors of A are not computed;
*          = 'V': right eigenvectors of A are computed.
*
*  N       (input) INTEGER
*          The order of the matrix A. N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the N-by-N matrix A.
*          On exit, A has been overwritten.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  WR      (output) DOUBLE PRECISION array, dimension (N)
*  WI      (output) DOUBLE PRECISION array, dimension (N)
*          WR and WI contain the real and imaginary parts,
*          respectively, of the computed eigenvalues.  Complex
*          conjugate pairs of eigenvalues appear consecutively
*          with the eigenvalue having the positive imaginary part
*          first.
*
*  VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
*          If JOBVL = 'V', the left eigenvectors u(j) are stored one
*          after another in the columns of VL, in the same order
*          as their eigenvalues.
*          If JOBVL = 'N', VL is not referenced.
*          If the j-th eigenvalue is real, then u(j) = VL(:,j),
*          the j-th column of VL.
*          If the j-th and (j+1)-st eigenvalues form a complex
*          conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
*          u(j+1) = VL(:,j) - i*VL(:,j+1).
*
*  LDVL    (input) INTEGER
*          The leading dimension of the array VL.  LDVL >= 1; if
*          JOBVL = 'V', LDVL >= N.
*
*  VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
*          If JOBVR = 'V', the right eigenvectors v(j) are stored one
*          after another in the columns of VR, in the same order
*          as their eigenvalues.
*          If JOBVR = 'N', VR is not referenced.
*          If the j-th eigenvalue is real, then v(j) = VR(:,j),
*          the j-th column of VR.
*          If the j-th and (j+1)-st eigenvalues form a complex
*          conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
*          v(j+1) = VR(:,j) - i*VR(:,j+1).
*
*  LDVR    (input) INTEGER
*          The leading dimension of the array VR.  LDVR >= 1; if
*          JOBVR = 'V', LDVR >= N.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,3*N), and
*          if JOBVL = 'V' or JOBVR = 'V', LWORK >= 4*N.  For good
*          performance, LWORK must generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = i, the QR algorithm failed to compute all the
*                eigenvalues, and no eigenvectors have been computed;
*                elements i+1:N of WR and WI contain eigenvalues which
*                have converged.
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
      INTEGER            HSWORK, I, IBAL, IERR, IHI, ILO, ITAU, IWRK, K,
     $                   MAXWRK, MINWRK, NOUT
      DOUBLE PRECISION   ANRM, BIGNUM, CS, CSCALE, EPS, R, SCL, SMLNUM,
     $                   SN
*     ..
*     .. Local Arrays ..
      LOGICAL            SELECT( 1 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEBAK, DGEBAL, DGEHRD, DHSEQR, DLABAD, DLACPY,
     $                   DLARTG, DLASCL, DORGHR, DROT, DSCAL, DTREVC,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX, ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE, DLAPY2, DNRM2
      EXTERNAL           LSAME, IDAMAX, ILAENV, DLAMCH, DLANGE, DLAPY2,
     $                   DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
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
         INFO = -9
      ELSE IF( LDVR.LT.1 .OR. ( WANTVR .AND. LDVR.LT.N ) ) THEN
         INFO = -11
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.
*       HSWORK refers to the workspace preferred by DHSEQR, as
*       calculated below. HSWORK is computed assuming ILO=1 and IHI=N,
*       the worst case.)
*
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            MINWRK = 1
            MAXWRK = 1
         ELSE
            MAXWRK = 2*N + N*ILAENV( 1, 'DGEHRD', ' ', N, 1, N, 0 )
            IF( WANTVL ) THEN
               MINWRK = 4*N
               MAXWRK = MAX( MAXWRK, 2*N + ( N - 1 )*ILAENV( 1,
     $                       'DORGHR', ' ', N, 1, N, -1 ) )
               CALL DHSEQR( 'S', 'V', N, 1, N, A, LDA, WR, WI, VL, LDVL,
     $                WORK, -1, INFO )
               HSWORK = WORK( 1 )
               MAXWRK = MAX( MAXWRK, N + 1, N + HSWORK )
               MAXWRK = MAX( MAXWRK, 4*N )
            ELSE IF( WANTVR ) THEN
               MINWRK = 4*N
               MAXWRK = MAX( MAXWRK, 2*N + ( N - 1 )*ILAENV( 1,
     $                       'DORGHR', ' ', N, 1, N, -1 ) )
               CALL DHSEQR( 'S', 'V', N, 1, N, A, LDA, WR, WI, VR, LDVR,
     $                WORK, -1, INFO )
               HSWORK = WORK( 1 )
               MAXWRK = MAX( MAXWRK, N + 1, N + HSWORK )
               MAXWRK = MAX( MAXWRK, 4*N )
            ELSE 
               MINWRK = 3*N
               CALL DHSEQR( 'E', 'N', N, 1, N, A, LDA, WR, WI, VR, LDVR,
     $                WORK, -1, INFO )
               HSWORK = WORK( 1 )
               MAXWRK = MAX( MAXWRK, N + 1, N + HSWORK )
            END IF
            MAXWRK = MAX( MAXWRK, MINWRK )
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -13
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEEV ', -INFO )
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
      ANRM = DLANGE( 'M', N, N, A, LDA, DUM )
      SCALEA = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = SMLNUM
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = BIGNUM
      END IF
      IF( SCALEA )
     $   CALL DLASCL( 'G', 0, 0, ANRM, CSCALE, N, N, A, LDA, IERR )
*
*     Balance the matrix
*     (Workspace: need N)
*
      IBAL = 1
      CALL DGEBAL( 'B', N, A, LDA, ILO, IHI, WORK( IBAL ), IERR )
*
*     Reduce to upper Hessenberg form
*     (Workspace: need 3*N, prefer 2*N+N*NB)
*
      ITAU = IBAL + N
      IWRK = ITAU + N
      CALL DGEHRD( N, ILO, IHI, A, LDA, WORK( ITAU ), WORK( IWRK ),
     $             LWORK-IWRK+1, IERR )
*
      IF( WANTVL ) THEN
*
*        Want left eigenvectors
*        Copy Householder vectors to VL
*
         SIDE = 'L'
         CALL DLACPY( 'L', N, N, A, LDA, VL, LDVL )
*
*        Generate orthogonal matrix in VL
*        (Workspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*
         CALL DORGHR( N, ILO, IHI, VL, LDVL, WORK( ITAU ), WORK( IWRK ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VL
*        (Workspace: need N+1, prefer N+HSWORK (see comments) )
*
         IWRK = ITAU
         CALL DHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, WR, WI, VL, LDVL,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
         IF( WANTVR ) THEN
*
*           Want left and right eigenvectors
*           Copy Schur vectors to VR
*
            SIDE = 'B'
            CALL DLACPY( 'F', N, N, VL, LDVL, VR, LDVR )
         END IF
*
      ELSE IF( WANTVR ) THEN
*
*        Want right eigenvectors
*        Copy Householder vectors to VR
*
         SIDE = 'R'
         CALL DLACPY( 'L', N, N, A, LDA, VR, LDVR )
*
*        Generate orthogonal matrix in VR
*        (Workspace: need 3*N-1, prefer 2*N+(N-1)*NB)
*
         CALL DORGHR( N, ILO, IHI, VR, LDVR, WORK( ITAU ), WORK( IWRK ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VR
*        (Workspace: need N+1, prefer N+HSWORK (see comments) )
*
         IWRK = ITAU
         CALL DHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, WR, WI, VR, LDVR,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
      ELSE
*
*        Compute eigenvalues only
*        (Workspace: need N+1, prefer N+HSWORK (see comments) )
*
         IWRK = ITAU
         CALL DHSEQR( 'E', 'N', N, ILO, IHI, A, LDA, WR, WI, VR, LDVR,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
      END IF
*
*     If INFO > 0 from DHSEQR, then quit
*
      IF( INFO.GT.0 )
     $   GO TO 50
*
      IF( WANTVL .OR. WANTVR ) THEN
*
*        Compute left and/or right eigenvectors
*        (Workspace: need 4*N)
*
         CALL DTREVC( SIDE, 'B', SELECT, N, A, LDA, VL, LDVL, VR, LDVR,
     $                N, NOUT, WORK( IWRK ), IERR )
      END IF
*
      IF( WANTVL ) THEN
*
*        Undo balancing of left eigenvectors
*        (Workspace: need N)
*
         CALL DGEBAK( 'B', 'L', N, ILO, IHI, WORK( IBAL ), N, VL, LDVL,
     $                IERR )
*
*        Normalize left eigenvectors and make largest component real
*
         DO 20 I = 1, N
            IF( WI( I ).EQ.ZERO ) THEN
               SCL = ONE / DNRM2( N, VL( 1, I ), 1 )
               CALL DSCAL( N, SCL, VL( 1, I ), 1 )
            ELSE IF( WI( I ).GT.ZERO ) THEN
               SCL = ONE / DLAPY2( DNRM2( N, VL( 1, I ), 1 ),
     $               DNRM2( N, VL( 1, I+1 ), 1 ) )
               CALL DSCAL( N, SCL, VL( 1, I ), 1 )
               CALL DSCAL( N, SCL, VL( 1, I+1 ), 1 )
               DO 10 K = 1, N
                  WORK( IWRK+K-1 ) = VL( K, I )**2 + VL( K, I+1 )**2
   10          CONTINUE
               K = IDAMAX( N, WORK( IWRK ), 1 )
               CALL DLARTG( VL( K, I ), VL( K, I+1 ), CS, SN, R )
               CALL DROT( N, VL( 1, I ), 1, VL( 1, I+1 ), 1, CS, SN )
               VL( K, I+1 ) = ZERO
            END IF
   20    CONTINUE
      END IF
*
      IF( WANTVR ) THEN
*
*        Undo balancing of right eigenvectors
*        (Workspace: need N)
*
         CALL DGEBAK( 'B', 'R', N, ILO, IHI, WORK( IBAL ), N, VR, LDVR,
     $                IERR )
*
*        Normalize right eigenvectors and make largest component real
*
         DO 40 I = 1, N
            IF( WI( I ).EQ.ZERO ) THEN
               SCL = ONE / DNRM2( N, VR( 1, I ), 1 )
               CALL DSCAL( N, SCL, VR( 1, I ), 1 )
            ELSE IF( WI( I ).GT.ZERO ) THEN
               SCL = ONE / DLAPY2( DNRM2( N, VR( 1, I ), 1 ),
     $               DNRM2( N, VR( 1, I+1 ), 1 ) )
               CALL DSCAL( N, SCL, VR( 1, I ), 1 )
               CALL DSCAL( N, SCL, VR( 1, I+1 ), 1 )
               DO 30 K = 1, N
                  WORK( IWRK+K-1 ) = VR( K, I )**2 + VR( K, I+1 )**2
   30          CONTINUE
               K = IDAMAX( N, WORK( IWRK ), 1 )
               CALL DLARTG( VR( K, I ), VR( K, I+1 ), CS, SN, R )
               CALL DROT( N, VR( 1, I ), 1, VR( 1, I+1 ), 1, CS, SN )
               VR( K, I+1 ) = ZERO
            END IF
   40    CONTINUE
      END IF
*
*     Undo scaling if necessary
*
   50 CONTINUE
      IF( SCALEA ) THEN
         CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N-INFO, 1, WR( INFO+1 ),
     $                MAX( N-INFO, 1 ), IERR )
         CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N-INFO, 1, WI( INFO+1 ),
     $                MAX( N-INFO, 1 ), IERR )
         IF( INFO.GT.0 ) THEN
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, ILO-1, 1, WR, N,
     $                   IERR )
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, ILO-1, 1, WI, N,
     $                   IERR )
         END IF
      END IF
*
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of DGEEV
*
      END
      SUBROUTINE DGEEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, WR, WI,
     $                   VL, LDVL, VR, LDVR, ILO, IHI, SCALE, ABNRM,
     $                   RCONDE, RCONDV, WORK, LWORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          BALANC, JOBVL, JOBVR, SENSE
      INTEGER            IHI, ILO, INFO, LDA, LDVL, LDVR, LWORK, N
      DOUBLE PRECISION   ABNRM
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), RCONDE( * ), RCONDV( * ),
     $                   SCALE( * ), VL( LDVL, * ), VR( LDVR, * ),
     $                   WI( * ), WORK( * ), WR( * )
*     ..
*
*  Purpose
*  =======
*
*  DGEEVX computes for an N-by-N real nonsymmetric matrix A, the
*  eigenvalues and, optionally, the left and/or right eigenvectors.
*
*  Optionally also, it computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  SCALE, and ABNRM), reciprocal condition numbers for the eigenvalues
*  (RCONDE), and reciprocal condition numbers for the right
*  eigenvectors (RCONDV).
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
*  Balancing a matrix means permuting the rows and columns to make it
*  more nearly upper triangular, and applying a diagonal similarity
*  transformation D * A * D**(-1), where D is a diagonal matrix, to
*  make its rows and columns closer in norm and the condition numbers
*  of its eigenvalues and eigenvectors smaller.  The computed
*  reciprocal condition numbers correspond to the balanced matrix.
*  Permuting rows and columns will not change the condition numbers
*  (in exact arithmetic) but diagonal scaling will.  For further
*  explanation of balancing, see section 4.10.2 of the LAPACK
*  Users' Guide.
*
*  Arguments
*  =========
*
*  BALANC  (input) CHARACTER*1
*          Indicates how the input matrix should be diagonally scaled
*          and/or permuted to improve the conditioning of its
*          eigenvalues.
*          = 'N': Do not diagonally scale or permute;
*          = 'P': Perform permutations to make the matrix more nearly
*                 upper triangular. Do not diagonally scale;
*          = 'S': Diagonally scale the matrix, i.e. replace A by
*                 D*A*D**(-1), where D is a diagonal matrix chosen
*                 to make the rows and columns of A more equal in
*                 norm. Do not permute;
*          = 'B': Both diagonally scale and permute A.
*
*          Computed reciprocal condition numbers will be for the matrix
*          after balancing and/or permuting. Permuting does not change
*          condition numbers (in exact arithmetic), but balancing does.
*
*  JOBVL   (input) CHARACTER*1
*          = 'N': left eigenvectors of A are not computed;
*          = 'V': left eigenvectors of A are computed.
*          If SENSE = 'E' or 'B', JOBVL must = 'V'.
*
*  JOBVR   (input) CHARACTER*1
*          = 'N': right eigenvectors of A are not computed;
*          = 'V': right eigenvectors of A are computed.
*          If SENSE = 'E' or 'B', JOBVR must = 'V'.
*
*  SENSE   (input) CHARACTER*1
*          Determines which reciprocal condition numbers are computed.
*          = 'N': None are computed;
*          = 'E': Computed for eigenvalues only;
*          = 'V': Computed for right eigenvectors only;
*          = 'B': Computed for eigenvalues and right eigenvectors.
*
*          If SENSE = 'E' or 'B', both left and right eigenvectors
*          must also be computed (JOBVL = 'V' and JOBVR = 'V').
*
*  N       (input) INTEGER
*          The order of the matrix A. N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the N-by-N matrix A.
*          On exit, A has been overwritten.  If JOBVL = 'V' or
*          JOBVR = 'V', A contains the real Schur form of the balanced
*          version of the input matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  WR      (output) DOUBLE PRECISION array, dimension (N)
*  WI      (output) DOUBLE PRECISION array, dimension (N)
*          WR and WI contain the real and imaginary parts,
*          respectively, of the computed eigenvalues.  Complex
*          conjugate pairs of eigenvalues will appear consecutively
*          with the eigenvalue having the positive imaginary part
*          first.
*
*  VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
*          If JOBVL = 'V', the left eigenvectors u(j) are stored one
*          after another in the columns of VL, in the same order
*          as their eigenvalues.
*          If JOBVL = 'N', VL is not referenced.
*          If the j-th eigenvalue is real, then u(j) = VL(:,j),
*          the j-th column of VL.
*          If the j-th and (j+1)-st eigenvalues form a complex
*          conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
*          u(j+1) = VL(:,j) - i*VL(:,j+1).
*
*  LDVL    (input) INTEGER
*          The leading dimension of the array VL.  LDVL >= 1; if
*          JOBVL = 'V', LDVL >= N.
*
*  VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
*          If JOBVR = 'V', the right eigenvectors v(j) are stored one
*          after another in the columns of VR, in the same order
*          as their eigenvalues.
*          If JOBVR = 'N', VR is not referenced.
*          If the j-th eigenvalue is real, then v(j) = VR(:,j),
*          the j-th column of VR.
*          If the j-th and (j+1)-st eigenvalues form a complex
*          conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
*          v(j+1) = VR(:,j) - i*VR(:,j+1).
*
*  LDVR    (input) INTEGER
*          The leading dimension of the array VR.  LDVR >= 1, and if
*          JOBVR = 'V', LDVR >= N.
*
*  ILO     (output) INTEGER
*  IHI     (output) INTEGER
*          ILO and IHI are integer values determined when A was
*          balanced.  The balanced A(i,j) = 0 if I > J and
*          J = 1,...,ILO-1 or I = IHI+1,...,N.
*
*  SCALE   (output) DOUBLE PRECISION array, dimension (N)
*          Details of the permutations and scaling factors applied
*          when balancing A.  If P(j) is the index of the row and column
*          interchanged with row and column j, and D(j) is the scaling
*          factor applied to row and column j, then
*          SCALE(J) = P(J),    for J = 1,...,ILO-1
*                   = D(J),    for J = ILO,...,IHI
*                   = P(J)     for J = IHI+1,...,N.
*          The order in which the interchanges are made is N to IHI+1,
*          then 1 to ILO-1.
*
*  ABNRM   (output) DOUBLE PRECISION
*          The one-norm of the balanced matrix (the maximum
*          of the sum of absolute values of elements of any column).
*
*  RCONDE  (output) DOUBLE PRECISION array, dimension (N)
*          RCONDE(j) is the reciprocal condition number of the j-th
*          eigenvalue.
*
*  RCONDV  (output) DOUBLE PRECISION array, dimension (N)
*          RCONDV(j) is the reciprocal condition number of the j-th
*          right eigenvector.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.   If SENSE = 'N' or 'E',
*          LWORK >= max(1,2*N), and if JOBVL = 'V' or JOBVR = 'V',
*          LWORK >= 3*N.  If SENSE = 'V' or 'B', LWORK >= N*(N+6).
*          For good performance, LWORK must generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace) INTEGER array, dimension (2*N-2)
*          If SENSE = 'N' or 'E', not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = i, the QR algorithm failed to compute all the
*                eigenvalues, and no eigenvectors or condition numbers
*                have been computed; elements 1:ILO-1 and i+1:N of WR
*                and WI contain eigenvalues which have converged.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, SCALEA, WANTVL, WANTVR, WNTSNB, WNTSNE,
     $                   WNTSNN, WNTSNV
      CHARACTER          JOB, SIDE
      INTEGER            HSWORK, I, ICOND, IERR, ITAU, IWRK, K, MAXWRK,
     $                   MINWRK, NOUT
      DOUBLE PRECISION   ANRM, BIGNUM, CS, CSCALE, EPS, R, SCL, SMLNUM,
     $                   SN
*     ..
*     .. Local Arrays ..
      LOGICAL            SELECT( 1 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEBAK, DGEBAL, DGEHRD, DHSEQR, DLABAD, DLACPY,
     $                   DLARTG, DLASCL, DORGHR, DROT, DSCAL, DTREVC,
     $                   DTRSNA, XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            IDAMAX, ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE, DLAPY2, DNRM2
      EXTERNAL           LSAME, IDAMAX, ILAENV, DLAMCH, DLANGE, DLAPY2,
     $                   DNRM2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      WANTVL = LSAME( JOBVL, 'V' )
      WANTVR = LSAME( JOBVR, 'V' )
      WNTSNN = LSAME( SENSE, 'N' )
      WNTSNE = LSAME( SENSE, 'E' )
      WNTSNV = LSAME( SENSE, 'V' )
      WNTSNB = LSAME( SENSE, 'B' )
      IF( .NOT.( LSAME( BALANC, 'N' ) .OR. LSAME( BALANC,
     $    'S' ) .OR. LSAME( BALANC, 'P' ) .OR. LSAME( BALANC, 'B' ) ) )
     $     THEN
         INFO = -1
      ELSE IF( ( .NOT.WANTVL ) .AND. ( .NOT.LSAME( JOBVL, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( ( .NOT.WANTVR ) .AND. ( .NOT.LSAME( JOBVR, 'N' ) ) ) THEN
         INFO = -3
      ELSE IF( .NOT.( WNTSNN .OR. WNTSNE .OR. WNTSNB .OR. WNTSNV ) .OR.
     $         ( ( WNTSNE .OR. WNTSNB ) .AND. .NOT.( WANTVL .AND.
     $         WANTVR ) ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDVL.LT.1 .OR. ( WANTVL .AND. LDVL.LT.N ) ) THEN
         INFO = -11
      ELSE IF( LDVR.LT.1 .OR. ( WANTVR .AND. LDVR.LT.N ) ) THEN
         INFO = -13
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.
*       HSWORK refers to the workspace preferred by DHSEQR, as
*       calculated below. HSWORK is computed assuming ILO=1 and IHI=N,
*       the worst case.)
*
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            MINWRK = 1
            MAXWRK = 1
         ELSE
            MAXWRK = N + N*ILAENV( 1, 'DGEHRD', ' ', N, 1, N, 0 )
*
            IF( WANTVL ) THEN
               CALL DHSEQR( 'S', 'V', N, 1, N, A, LDA, WR, WI, VL, LDVL,
     $                WORK, -1, INFO )
            ELSE IF( WANTVR ) THEN
               CALL DHSEQR( 'S', 'V', N, 1, N, A, LDA, WR, WI, VR, LDVR,
     $                WORK, -1, INFO )
            ELSE
               IF( WNTSNN ) THEN
                  CALL DHSEQR( 'E', 'N', N, 1, N, A, LDA, WR, WI, VR,
     $                LDVR, WORK, -1, INFO )
               ELSE
                  CALL DHSEQR( 'S', 'N', N, 1, N, A, LDA, WR, WI, VR,
     $                LDVR, WORK, -1, INFO )
               END IF
            END IF
            HSWORK = WORK( 1 )
*
            IF( ( .NOT.WANTVL ) .AND. ( .NOT.WANTVR ) ) THEN
               MINWRK = 2*N
               IF( .NOT.WNTSNN )
     $            MINWRK = MAX( MINWRK, N*N+6*N )
               MAXWRK = MAX( MAXWRK, HSWORK )
               IF( .NOT.WNTSNN )
     $            MAXWRK = MAX( MAXWRK, N*N + 6*N )
            ELSE
               MINWRK = 3*N
               IF( ( .NOT.WNTSNN ) .AND. ( .NOT.WNTSNE ) )
     $            MINWRK = MAX( MINWRK, N*N + 6*N )
               MAXWRK = MAX( MAXWRK, HSWORK )
               MAXWRK = MAX( MAXWRK, N + ( N - 1 )*ILAENV( 1, 'DORGHR',
     $                       ' ', N, 1, N, -1 ) )
               IF( ( .NOT.WNTSNN ) .AND. ( .NOT.WNTSNE ) )
     $            MAXWRK = MAX( MAXWRK, N*N + 6*N )
               MAXWRK = MAX( MAXWRK, 3*N )
            END IF
            MAXWRK = MAX( MAXWRK, MINWRK )
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -21
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEEVX', -INFO )
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
      ICOND = 0
      ANRM = DLANGE( 'M', N, N, A, LDA, DUM )
      SCALEA = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = SMLNUM
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         SCALEA = .TRUE.
         CSCALE = BIGNUM
      END IF
      IF( SCALEA )
     $   CALL DLASCL( 'G', 0, 0, ANRM, CSCALE, N, N, A, LDA, IERR )
*
*     Balance the matrix and compute ABNRM
*
      CALL DGEBAL( BALANC, N, A, LDA, ILO, IHI, SCALE, IERR )
      ABNRM = DLANGE( '1', N, N, A, LDA, DUM )
      IF( SCALEA ) THEN
         DUM( 1 ) = ABNRM
         CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, 1, 1, DUM, 1, IERR )
         ABNRM = DUM( 1 )
      END IF
*
*     Reduce to upper Hessenberg form
*     (Workspace: need 2*N, prefer N+N*NB)
*
      ITAU = 1
      IWRK = ITAU + N
      CALL DGEHRD( N, ILO, IHI, A, LDA, WORK( ITAU ), WORK( IWRK ),
     $             LWORK-IWRK+1, IERR )
*
      IF( WANTVL ) THEN
*
*        Want left eigenvectors
*        Copy Householder vectors to VL
*
         SIDE = 'L'
         CALL DLACPY( 'L', N, N, A, LDA, VL, LDVL )
*
*        Generate orthogonal matrix in VL
*        (Workspace: need 2*N-1, prefer N+(N-1)*NB)
*
         CALL DORGHR( N, ILO, IHI, VL, LDVL, WORK( ITAU ), WORK( IWRK ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VL
*        (Workspace: need 1, prefer HSWORK (see comments) )
*
         IWRK = ITAU
         CALL DHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, WR, WI, VL, LDVL,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
         IF( WANTVR ) THEN
*
*           Want left and right eigenvectors
*           Copy Schur vectors to VR
*
            SIDE = 'B'
            CALL DLACPY( 'F', N, N, VL, LDVL, VR, LDVR )
         END IF
*
      ELSE IF( WANTVR ) THEN
*
*        Want right eigenvectors
*        Copy Householder vectors to VR
*
         SIDE = 'R'
         CALL DLACPY( 'L', N, N, A, LDA, VR, LDVR )
*
*        Generate orthogonal matrix in VR
*        (Workspace: need 2*N-1, prefer N+(N-1)*NB)
*
         CALL DORGHR( N, ILO, IHI, VR, LDVR, WORK( ITAU ), WORK( IWRK ),
     $                LWORK-IWRK+1, IERR )
*
*        Perform QR iteration, accumulating Schur vectors in VR
*        (Workspace: need 1, prefer HSWORK (see comments) )
*
         IWRK = ITAU
         CALL DHSEQR( 'S', 'V', N, ILO, IHI, A, LDA, WR, WI, VR, LDVR,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
*
      ELSE
*
*        Compute eigenvalues only
*        If condition numbers desired, compute Schur form
*
         IF( WNTSNN ) THEN
            JOB = 'E'
         ELSE
            JOB = 'S'
         END IF
*
*        (Workspace: need 1, prefer HSWORK (see comments) )
*
         IWRK = ITAU
         CALL DHSEQR( JOB, 'N', N, ILO, IHI, A, LDA, WR, WI, VR, LDVR,
     $                WORK( IWRK ), LWORK-IWRK+1, INFO )
      END IF
*
*     If INFO > 0 from DHSEQR, then quit
*
      IF( INFO.GT.0 )
     $   GO TO 50
*
      IF( WANTVL .OR. WANTVR ) THEN
*
*        Compute left and/or right eigenvectors
*        (Workspace: need 3*N)
*
         CALL DTREVC( SIDE, 'B', SELECT, N, A, LDA, VL, LDVL, VR, LDVR,
     $                N, NOUT, WORK( IWRK ), IERR )
      END IF
*
*     Compute condition numbers if desired
*     (Workspace: need N*N+6*N unless SENSE = 'E')
*
      IF( .NOT.WNTSNN ) THEN
         CALL DTRSNA( SENSE, 'A', SELECT, N, A, LDA, VL, LDVL, VR, LDVR,
     $                RCONDE, RCONDV, N, NOUT, WORK( IWRK ), N, IWORK,
     $                ICOND )
      END IF
*
      IF( WANTVL ) THEN
*
*        Undo balancing of left eigenvectors
*
         CALL DGEBAK( BALANC, 'L', N, ILO, IHI, SCALE, N, VL, LDVL,
     $                IERR )
*
*        Normalize left eigenvectors and make largest component real
*
         DO 20 I = 1, N
            IF( WI( I ).EQ.ZERO ) THEN
               SCL = ONE / DNRM2( N, VL( 1, I ), 1 )
               CALL DSCAL( N, SCL, VL( 1, I ), 1 )
            ELSE IF( WI( I ).GT.ZERO ) THEN
               SCL = ONE / DLAPY2( DNRM2( N, VL( 1, I ), 1 ),
     $               DNRM2( N, VL( 1, I+1 ), 1 ) )
               CALL DSCAL( N, SCL, VL( 1, I ), 1 )
               CALL DSCAL( N, SCL, VL( 1, I+1 ), 1 )
               DO 10 K = 1, N
                  WORK( K ) = VL( K, I )**2 + VL( K, I+1 )**2
   10          CONTINUE
               K = IDAMAX( N, WORK, 1 )
               CALL DLARTG( VL( K, I ), VL( K, I+1 ), CS, SN, R )
               CALL DROT( N, VL( 1, I ), 1, VL( 1, I+1 ), 1, CS, SN )
               VL( K, I+1 ) = ZERO
            END IF
   20    CONTINUE
      END IF
*
      IF( WANTVR ) THEN
*
*        Undo balancing of right eigenvectors
*
         CALL DGEBAK( BALANC, 'R', N, ILO, IHI, SCALE, N, VR, LDVR,
     $                IERR )
*
*        Normalize right eigenvectors and make largest component real
*
         DO 40 I = 1, N
            IF( WI( I ).EQ.ZERO ) THEN
               SCL = ONE / DNRM2( N, VR( 1, I ), 1 )
               CALL DSCAL( N, SCL, VR( 1, I ), 1 )
            ELSE IF( WI( I ).GT.ZERO ) THEN
               SCL = ONE / DLAPY2( DNRM2( N, VR( 1, I ), 1 ),
     $               DNRM2( N, VR( 1, I+1 ), 1 ) )
               CALL DSCAL( N, SCL, VR( 1, I ), 1 )
               CALL DSCAL( N, SCL, VR( 1, I+1 ), 1 )
               DO 30 K = 1, N
                  WORK( K ) = VR( K, I )**2 + VR( K, I+1 )**2
   30          CONTINUE
               K = IDAMAX( N, WORK, 1 )
               CALL DLARTG( VR( K, I ), VR( K, I+1 ), CS, SN, R )
               CALL DROT( N, VR( 1, I ), 1, VR( 1, I+1 ), 1, CS, SN )
               VR( K, I+1 ) = ZERO
            END IF
   40    CONTINUE
      END IF
*
*     Undo scaling if necessary
*
   50 CONTINUE
      IF( SCALEA ) THEN
         CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N-INFO, 1, WR( INFO+1 ),
     $                MAX( N-INFO, 1 ), IERR )
         CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N-INFO, 1, WI( INFO+1 ),
     $                MAX( N-INFO, 1 ), IERR )
         IF( INFO.EQ.0 ) THEN
            IF( ( WNTSNV .OR. WNTSNB ) .AND. ICOND.EQ.0 )
     $         CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, N, 1, RCONDV, N,
     $                      IERR )
         ELSE
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, ILO-1, 1, WR, N,
     $                   IERR )
            CALL DLASCL( 'G', 0, 0, CSCALE, ANRM, ILO-1, 1, WI, N,
     $                   IERR )
         END IF
      END IF
*
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of DGEEVX
*
      END
      SUBROUTINE DGEGS( JOBVSL, JOBVSR, N, A, LDA, B, LDB, ALPHAR,
     $                  ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR, WORK,
     $                  LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVSL, JOBVSR
      INTEGER            INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), VSL( LDVSL, * ),
     $                   VSR( LDVSR, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  This routine is deprecated and has been replaced by routine DGGES.
*
*  DGEGS computes the eigenvalues, real Schur form, and, optionally,
*  left and or/right Schur vectors of a real matrix pair (A,B).
*  Given two square matrices A and B, the generalized real Schur
*  factorization has the form
*
*    A = Q*S*Z**T,  B = Q*T*Z**T
*
*  where Q and Z are orthogonal matrices, T is upper triangular, and S
*  is an upper quasi-triangular matrix with 1-by-1 and 2-by-2 diagonal
*  blocks, the 2-by-2 blocks corresponding to complex conjugate pairs
*  of eigenvalues of (A,B).  The columns of Q are the left Schur vectors
*  and the columns of Z are the right Schur vectors.
*
*  If only the eigenvalues of (A,B) are needed, the driver routine
*  DGEGV should be used instead.  See DGEGV for a description of the
*  eigenvalues of the generalized nonsymmetric eigenvalue problem
*  (GNEP).
*
*  Arguments
*  =========
*
*  JOBVSL  (input) CHARACTER*1
*          = 'N':  do not compute the left Schur vectors;
*          = 'V':  compute the left Schur vectors (returned in VSL).
*
*  JOBVSR  (input) CHARACTER*1
*          = 'N':  do not compute the right Schur vectors;
*          = 'V':  compute the right Schur vectors (returned in VSR).
*
*  N       (input) INTEGER
*          The order of the matrices A, B, VSL, and VSR.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the matrix A.
*          On exit, the upper quasi-triangular matrix S from the
*          generalized real Schur factorization.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the matrix B.
*          On exit, the upper triangular matrix T from the generalized
*          real Schur factorization.
*
*  LDB     (input) INTEGER
*          The leading dimension of B.  LDB >= max(1,N).
*
*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
*          The real parts of each scalar alpha defining an eigenvalue
*          of GNEP.
*
*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
*          The imaginary parts of each scalar alpha defining an
*          eigenvalue of GNEP.  If ALPHAI(j) is zero, then the j-th
*          eigenvalue is real; if positive, then the j-th and (j+1)-st
*          eigenvalues are a complex conjugate pair, with
*          ALPHAI(j+1) = -ALPHAI(j).
*
*  BETA    (output) DOUBLE PRECISION array, dimension (N)
*          The scalars beta that define the eigenvalues of GNEP.
*          Together, the quantities alpha = (ALPHAR(j),ALPHAI(j)) and
*          beta = BETA(j) represent the j-th eigenvalue of the matrix
*          pair (A,B), in one of the forms lambda = alpha/beta or
*          mu = beta/alpha.  Since either lambda or mu may overflow,
*          they should not, in general, be computed.
*
*  VSL     (output) DOUBLE PRECISION array, dimension (LDVSL,N)
*          If JOBVSL = 'V', the matrix of left Schur vectors Q.
*          Not referenced if JOBVSL = 'N'.
*
*  LDVSL   (input) INTEGER
*          The leading dimension of the matrix VSL. LDVSL >=1, and
*          if JOBVSL = 'V', LDVSL >= N.
*
*  VSR     (output) DOUBLE PRECISION array, dimension (LDVSR,N)
*          If JOBVSR = 'V', the matrix of right Schur vectors Z.
*          Not referenced if JOBVSR = 'N'.
*
*  LDVSR   (input) INTEGER
*          The leading dimension of the matrix VSR. LDVSR >= 1, and
*          if JOBVSR = 'V', LDVSR >= N.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,4*N).
*          For good performance, LWORK must generally be larger.
*          To compute the optimal value of LWORK, call ILAENV to get
*          blocksizes (for DGEQRF, DORMQR, and DORGQR.)  Then compute:
*          NB  -- MAX of the blocksizes for DGEQRF, DORMQR, and DORGQR
*          The optimal LWORK is  2*N + N*(NB+1).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          = 1,...,N:
*                The QZ iteration failed.  (A,B) are not in Schur
*                form, but ALPHAR(j), ALPHAI(j), and BETA(j) should
*                be correct for j=INFO+1,...,N.
*          > N:  errors that usually indicate LAPACK problems:
*                =N+1: error return from DGGBAL
*                =N+2: error return from DGEQRF
*                =N+3: error return from DORMQR
*                =N+4: error return from DORGQR
*                =N+5: error return from DGGHRD
*                =N+6: error return from DHGEQZ (other than failed
*                                                iteration)
*                =N+7: error return from DGGBAK (computing VSL)
*                =N+8: error return from DGGBAK (computing VSR)
*                =N+9: error return from DLASCL (various places)
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ILASCL, ILBSCL, ILVSL, ILVSR, LQUERY
      INTEGER            ICOLS, IHI, IINFO, IJOBVL, IJOBVR, ILEFT, ILO,
     $                   IRIGHT, IROWS, ITAU, IWORK, LOPT, LWKMIN,
     $                   LWKOPT, NB, NB1, NB2, NB3
      DOUBLE PRECISION   ANRM, ANRMTO, BIGNUM, BNRM, BNRMTO, EPS,
     $                   SAFMIN, SMLNUM
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DGGBAK, DGGBAL, DGGHRD, DHGEQZ, DLACPY,
     $                   DLASCL, DLASET, DORGQR, DORMQR, XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX
*     ..
*     .. Executable Statements ..
*
*     Decode the input arguments
*
      IF( LSAME( JOBVSL, 'N' ) ) THEN
         IJOBVL = 1
         ILVSL = .FALSE.
      ELSE IF( LSAME( JOBVSL, 'V' ) ) THEN
         IJOBVL = 2
         ILVSL = .TRUE.
      ELSE
         IJOBVL = -1
         ILVSL = .FALSE.
      END IF
*
      IF( LSAME( JOBVSR, 'N' ) ) THEN
         IJOBVR = 1
         ILVSR = .FALSE.
      ELSE IF( LSAME( JOBVSR, 'V' ) ) THEN
         IJOBVR = 2
         ILVSR = .TRUE.
      ELSE
         IJOBVR = -1
         ILVSR = .FALSE.
      END IF
*
*     Test the input arguments
*
      LWKMIN = MAX( 4*N, 1 )
      LWKOPT = LWKMIN
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      INFO = 0
      IF( IJOBVL.LE.0 ) THEN
         INFO = -1
      ELSE IF( IJOBVR.LE.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDVSL.LT.1 .OR. ( ILVSL .AND. LDVSL.LT.N ) ) THEN
         INFO = -12
      ELSE IF( LDVSR.LT.1 .OR. ( ILVSR .AND. LDVSR.LT.N ) ) THEN
         INFO = -14
      ELSE IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
         INFO = -16
      END IF
*
      IF( INFO.EQ.0 ) THEN
         NB1 = ILAENV( 1, 'DGEQRF', ' ', N, N, -1, -1 )
         NB2 = ILAENV( 1, 'DORMQR', ' ', N, N, N, -1 )
         NB3 = ILAENV( 1, 'DORGQR', ' ', N, N, N, -1 )
         NB = MAX( NB1, NB2, NB3 )
         LOPT = 2*N + N*( NB+1 )
         WORK( 1 ) = LOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEGS ', -INFO )
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
      EPS = DLAMCH( 'E' )*DLAMCH( 'B' )
      SAFMIN = DLAMCH( 'S' )
      SMLNUM = N*SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', N, N, A, LDA, WORK )
      ILASCL = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ANRMTO = SMLNUM
         ILASCL = .TRUE.
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ANRMTO = BIGNUM
         ILASCL = .TRUE.
      END IF
*
      IF( ILASCL ) THEN
         CALL DLASCL( 'G', -1, -1, ANRM, ANRMTO, N, N, A, LDA, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 9
            RETURN
         END IF
      END IF
*
*     Scale B if max element outside range [SMLNUM,BIGNUM]
*
      BNRM = DLANGE( 'M', N, N, B, LDB, WORK )
      ILBSCL = .FALSE.
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
         BNRMTO = SMLNUM
         ILBSCL = .TRUE.
      ELSE IF( BNRM.GT.BIGNUM ) THEN
         BNRMTO = BIGNUM
         ILBSCL = .TRUE.
      END IF
*
      IF( ILBSCL ) THEN
         CALL DLASCL( 'G', -1, -1, BNRM, BNRMTO, N, N, B, LDB, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 9
            RETURN
         END IF
      END IF
*
*     Permute the matrix to make it more nearly triangular
*     Workspace layout:  (2*N words -- "work..." not actually used)
*        left_permutation, right_permutation, work...
*
      ILEFT = 1
      IRIGHT = N + 1
      IWORK = IRIGHT + N
      CALL DGGBAL( 'P', N, A, LDA, B, LDB, ILO, IHI, WORK( ILEFT ),
     $             WORK( IRIGHT ), WORK( IWORK ), IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = N + 1
         GO TO 10
      END IF
*
*     Reduce B to triangular form, and initialize VSL and/or VSR
*     Workspace layout:  ("work..." must have at least N words)
*        left_permutation, right_permutation, tau, work...
*
      IROWS = IHI + 1 - ILO
      ICOLS = N + 1 - ILO
      ITAU = IWORK
      IWORK = ITAU + IROWS
      CALL DGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK( ITAU ),
     $             WORK( IWORK ), LWORK+1-IWORK, IINFO )
      IF( IINFO.GE.0 )
     $   LWKOPT = MAX( LWKOPT, INT( WORK( IWORK ) )+IWORK-1 )
      IF( IINFO.NE.0 ) THEN
         INFO = N + 2
         GO TO 10
      END IF
*
      CALL DORMQR( 'L', 'T', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     $             WORK( ITAU ), A( ILO, ILO ), LDA, WORK( IWORK ),
     $             LWORK+1-IWORK, IINFO )
      IF( IINFO.GE.0 )
     $   LWKOPT = MAX( LWKOPT, INT( WORK( IWORK ) )+IWORK-1 )
      IF( IINFO.NE.0 ) THEN
         INFO = N + 3
         GO TO 10
      END IF
*
      IF( ILVSL ) THEN
         CALL DLASET( 'Full', N, N, ZERO, ONE, VSL, LDVSL )
         CALL DLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     $                VSL( ILO+1, ILO ), LDVSL )
         CALL DORGQR( IROWS, IROWS, IROWS, VSL( ILO, ILO ), LDVSL,
     $                WORK( ITAU ), WORK( IWORK ), LWORK+1-IWORK,
     $                IINFO )
         IF( IINFO.GE.0 )
     $      LWKOPT = MAX( LWKOPT, INT( WORK( IWORK ) )+IWORK-1 )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 4
            GO TO 10
         END IF
      END IF
*
      IF( ILVSR )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, VSR, LDVSR )
*
*     Reduce to generalized Hessenberg form
*
      CALL DGGHRD( JOBVSL, JOBVSR, N, ILO, IHI, A, LDA, B, LDB, VSL,
     $             LDVSL, VSR, LDVSR, IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = N + 5
         GO TO 10
      END IF
*
*     Perform QZ algorithm, computing Schur vectors if desired
*     Workspace layout:  ("work..." must have at least 1 word)
*        left_permutation, right_permutation, work...
*
      IWORK = ITAU
      CALL DHGEQZ( 'S', JOBVSL, JOBVSR, N, ILO, IHI, A, LDA, B, LDB,
     $             ALPHAR, ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR,
     $             WORK( IWORK ), LWORK+1-IWORK, IINFO )
      IF( IINFO.GE.0 )
     $   LWKOPT = MAX( LWKOPT, INT( WORK( IWORK ) )+IWORK-1 )
      IF( IINFO.NE.0 ) THEN
         IF( IINFO.GT.0 .AND. IINFO.LE.N ) THEN
            INFO = IINFO
         ELSE IF( IINFO.GT.N .AND. IINFO.LE.2*N ) THEN
            INFO = IINFO - N
         ELSE
            INFO = N + 6
         END IF
         GO TO 10
      END IF
*
*     Apply permutation to VSL and VSR
*
      IF( ILVSL ) THEN
         CALL DGGBAK( 'P', 'L', N, ILO, IHI, WORK( ILEFT ),
     $                WORK( IRIGHT ), N, VSL, LDVSL, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 7
            GO TO 10
         END IF
      END IF
      IF( ILVSR ) THEN
         CALL DGGBAK( 'P', 'R', N, ILO, IHI, WORK( ILEFT ),
     $                WORK( IRIGHT ), N, VSR, LDVSR, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 8
            GO TO 10
         END IF
      END IF
*
*     Undo scaling
*
      IF( ILASCL ) THEN
         CALL DLASCL( 'H', -1, -1, ANRMTO, ANRM, N, N, A, LDA, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 9
            RETURN
         END IF
         CALL DLASCL( 'G', -1, -1, ANRMTO, ANRM, N, 1, ALPHAR, N,
     $                IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 9
            RETURN
         END IF
         CALL DLASCL( 'G', -1, -1, ANRMTO, ANRM, N, 1, ALPHAI, N,
     $                IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 9
            RETURN
         END IF
      END IF
*
      IF( ILBSCL ) THEN
         CALL DLASCL( 'U', -1, -1, BNRMTO, BNRM, N, N, B, LDB, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 9
            RETURN
         END IF
         CALL DLASCL( 'G', -1, -1, BNRMTO, BNRM, N, 1, BETA, N, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 9
            RETURN
         END IF
      END IF
*
   10 CONTINUE
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DGEGS
*
      END
      SUBROUTINE DGEGV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR, ALPHAI,
     $                  BETA, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVL, JOBVR
      INTEGER            INFO, LDA, LDB, LDVL, LDVR, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), VL( LDVL, * ),
     $                   VR( LDVR, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  This routine is deprecated and has been replaced by routine DGGEV.
*
*  DGEGV computes the eigenvalues and, optionally, the left and/or right
*  eigenvectors of a real matrix pair (A,B).
*  Given two square matrices A and B,
*  the generalized nonsymmetric eigenvalue problem (GNEP) is to find the
*  eigenvalues lambda and corresponding (non-zero) eigenvectors x such
*  that
*
*     A*x = lambda*B*x.
*
*  An alternate form is to find the eigenvalues mu and corresponding
*  eigenvectors y such that
*
*     mu*A*y = B*y.
*
*  These two forms are equivalent with mu = 1/lambda and x = y if
*  neither lambda nor mu is zero.  In order to deal with the case that
*  lambda or mu is zero or small, two values alpha and beta are returned
*  for each eigenvalue, such that lambda = alpha/beta and
*  mu = beta/alpha.
*
*  The vectors x and y in the above equations are right eigenvectors of
*  the matrix pair (A,B).  Vectors u and v satisfying
*
*     u**H*A = lambda*u**H*B  or  mu*v**H*A = v**H*B
*
*  are left eigenvectors of (A,B).
*
*  Note: this routine performs "full balancing" on A and B -- see
*  "Further Details", below.
*
*  Arguments
*  =========
*
*  JOBVL   (input) CHARACTER*1
*          = 'N':  do not compute the left generalized eigenvectors;
*          = 'V':  compute the left generalized eigenvectors (returned
*                  in VL).
*
*  JOBVR   (input) CHARACTER*1
*          = 'N':  do not compute the right generalized eigenvectors;
*          = 'V':  compute the right generalized eigenvectors (returned
*                  in VR).
*
*  N       (input) INTEGER
*          The order of the matrices A, B, VL, and VR.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the matrix A.
*          If JOBVL = 'V' or JOBVR = 'V', then on exit A
*          contains the real Schur form of A from the generalized Schur
*          factorization of the pair (A,B) after balancing.
*          If no eigenvectors were computed, then only the diagonal
*          blocks from the Schur form will be correct.  See DGGHRD and
*          DHGEQZ for details.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the matrix B.
*          If JOBVL = 'V' or JOBVR = 'V', then on exit B contains the
*          upper triangular matrix obtained from B in the generalized
*          Schur factorization of the pair (A,B) after balancing.
*          If no eigenvectors were computed, then only those elements of
*          B corresponding to the diagonal blocks from the Schur form of
*          A will be correct.  See DGGHRD and DHGEQZ for details.
*
*  LDB     (input) INTEGER
*          The leading dimension of B.  LDB >= max(1,N).
*
*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
*          The real parts of each scalar alpha defining an eigenvalue of
*          GNEP.
*
*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
*          The imaginary parts of each scalar alpha defining an
*          eigenvalue of GNEP.  If ALPHAI(j) is zero, then the j-th
*          eigenvalue is real; if positive, then the j-th and
*          (j+1)-st eigenvalues are a complex conjugate pair, with
*          ALPHAI(j+1) = -ALPHAI(j).
*
*  BETA    (output) DOUBLE PRECISION array, dimension (N)
*          The scalars beta that define the eigenvalues of GNEP.
*          
*          Together, the quantities alpha = (ALPHAR(j),ALPHAI(j)) and
*          beta = BETA(j) represent the j-th eigenvalue of the matrix
*          pair (A,B), in one of the forms lambda = alpha/beta or
*          mu = beta/alpha.  Since either lambda or mu may overflow,
*          they should not, in general, be computed.
*
*  VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
*          If JOBVL = 'V', the left eigenvectors u(j) are stored
*          in the columns of VL, in the same order as their eigenvalues.
*          If the j-th eigenvalue is real, then u(j) = VL(:,j).
*          If the j-th and (j+1)-st eigenvalues form a complex conjugate
*          pair, then
*             u(j) = VL(:,j) + i*VL(:,j+1)
*          and
*            u(j+1) = VL(:,j) - i*VL(:,j+1).
*
*          Each eigenvector is scaled so that its largest component has
*          abs(real part) + abs(imag. part) = 1, except for eigenvectors
*          corresponding to an eigenvalue with alpha = beta = 0, which
*          are set to zero.
*          Not referenced if JOBVL = 'N'.
*
*  LDVL    (input) INTEGER
*          The leading dimension of the matrix VL. LDVL >= 1, and
*          if JOBVL = 'V', LDVL >= N.
*
*  VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
*          If JOBVR = 'V', the right eigenvectors x(j) are stored
*          in the columns of VR, in the same order as their eigenvalues.
*          If the j-th eigenvalue is real, then x(j) = VR(:,j).
*          If the j-th and (j+1)-st eigenvalues form a complex conjugate
*          pair, then
*            x(j) = VR(:,j) + i*VR(:,j+1)
*          and
*            x(j+1) = VR(:,j) - i*VR(:,j+1).
*
*          Each eigenvector is scaled so that its largest component has
*          abs(real part) + abs(imag. part) = 1, except for eigenvalues
*          corresponding to an eigenvalue with alpha = beta = 0, which
*          are set to zero.
*          Not referenced if JOBVR = 'N'.
*
*  LDVR    (input) INTEGER
*          The leading dimension of the matrix VR. LDVR >= 1, and
*          if JOBVR = 'V', LDVR >= N.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,8*N).
*          For good performance, LWORK must generally be larger.
*          To compute the optimal value of LWORK, call ILAENV to get
*          blocksizes (for DGEQRF, DORMQR, and DORGQR.)  Then compute:
*          NB  -- MAX of the blocksizes for DGEQRF, DORMQR, and DORGQR;
*          The optimal LWORK is:
*              2*N + MAX( 6*N, N*(NB+1) ).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          = 1,...,N:
*                The QZ iteration failed.  No eigenvectors have been
*                calculated, but ALPHAR(j), ALPHAI(j), and BETA(j)
*                should be correct for j=INFO+1,...,N.
*          > N:  errors that usually indicate LAPACK problems:
*                =N+1: error return from DGGBAL
*                =N+2: error return from DGEQRF
*                =N+3: error return from DORMQR
*                =N+4: error return from DORGQR
*                =N+5: error return from DGGHRD
*                =N+6: error return from DHGEQZ (other than failed
*                                                iteration)
*                =N+7: error return from DTGEVC
*                =N+8: error return from DGGBAK (computing VL)
*                =N+9: error return from DGGBAK (computing VR)
*                =N+10: error return from DLASCL (various calls)
*
*  Further Details
*  ===============
*
*  Balancing
*  ---------
*
*  This driver calls DGGBAL to both permute and scale rows and columns
*  of A and B.  The permutations PL and PR are chosen so that PL*A*PR
*  and PL*B*R will be upper triangular except for the diagonal blocks
*  A(i:j,i:j) and B(i:j,i:j), with i and j as close together as
*  possible.  The diagonal scaling matrices DL and DR are chosen so
*  that the pair  DL*PL*A*PR*DR, DL*PL*B*PR*DR have elements close to
*  one (except for the elements that start out zero.)
*
*  After the eigenvalues and eigenvectors of the balanced matrices
*  have been computed, DGGBAK transforms the eigenvectors back to what
*  they would have been (in perfect arithmetic) if they had not been
*  balanced.
*
*  Contents of A and B on Exit
*  -------- -- - --- - -- ----
*
*  If any eigenvectors are computed (either JOBVL='V' or JOBVR='V' or
*  both), then on exit the arrays A and B will contain the real Schur
*  form[*] of the "balanced" versions of A and B.  If no eigenvectors
*  are computed, then only the diagonal blocks will be correct.
*
*  [*] See DHGEQZ, DGEGS, or read the book "Matrix Computations",
*      by Golub & van Loan, pub. by Johns Hopkins U. Press.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ILIMIT, ILV, ILVL, ILVR, LQUERY
      CHARACTER          CHTEMP
      INTEGER            ICOLS, IHI, IINFO, IJOBVL, IJOBVR, ILEFT, ILO,
     $                   IN, IRIGHT, IROWS, ITAU, IWORK, JC, JR, LOPT,
     $                   LWKMIN, LWKOPT, NB, NB1, NB2, NB3
      DOUBLE PRECISION   ABSAI, ABSAR, ABSB, ANRM, ANRM1, ANRM2, BNRM,
     $                   BNRM1, BNRM2, EPS, ONEPLS, SAFMAX, SAFMIN,
     $                   SALFAI, SALFAR, SBETA, SCALE, TEMP
*     ..
*     .. Local Arrays ..
      LOGICAL            LDUMMA( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DGGBAK, DGGBAL, DGGHRD, DHGEQZ, DLACPY,
     $                   DLASCL, DLASET, DORGQR, DORMQR, DTGEVC, XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, MAX
*     ..
*     .. Executable Statements ..
*
*     Decode the input arguments
*
      IF( LSAME( JOBVL, 'N' ) ) THEN
         IJOBVL = 1
         ILVL = .FALSE.
      ELSE IF( LSAME( JOBVL, 'V' ) ) THEN
         IJOBVL = 2
         ILVL = .TRUE.
      ELSE
         IJOBVL = -1
         ILVL = .FALSE.
      END IF
*
      IF( LSAME( JOBVR, 'N' ) ) THEN
         IJOBVR = 1
         ILVR = .FALSE.
      ELSE IF( LSAME( JOBVR, 'V' ) ) THEN
         IJOBVR = 2
         ILVR = .TRUE.
      ELSE
         IJOBVR = -1
         ILVR = .FALSE.
      END IF
      ILV = ILVL .OR. ILVR
*
*     Test the input arguments
*
      LWKMIN = MAX( 8*N, 1 )
      LWKOPT = LWKMIN
      WORK( 1 ) = LWKOPT
      LQUERY = ( LWORK.EQ.-1 )
      INFO = 0
      IF( IJOBVL.LE.0 ) THEN
         INFO = -1
      ELSE IF( IJOBVR.LE.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDVL.LT.1 .OR. ( ILVL .AND. LDVL.LT.N ) ) THEN
         INFO = -12
      ELSE IF( LDVR.LT.1 .OR. ( ILVR .AND. LDVR.LT.N ) ) THEN
         INFO = -14
      ELSE IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
         INFO = -16
      END IF
*
      IF( INFO.EQ.0 ) THEN
         NB1 = ILAENV( 1, 'DGEQRF', ' ', N, N, -1, -1 )
         NB2 = ILAENV( 1, 'DORMQR', ' ', N, N, N, -1 )
         NB3 = ILAENV( 1, 'DORGQR', ' ', N, N, N, -1 )
         NB = MAX( NB1, NB2, NB3 )
         LOPT = 2*N + MAX( 6*N, N*( NB+1 ) )
         WORK( 1 ) = LOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGEGV ', -INFO )
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
      EPS = DLAMCH( 'E' )*DLAMCH( 'B' )
      SAFMIN = DLAMCH( 'S' )
      SAFMIN = SAFMIN + SAFMIN
      SAFMAX = ONE / SAFMIN
      ONEPLS = ONE + ( 4*EPS )
*
*     Scale A
*
      ANRM = DLANGE( 'M', N, N, A, LDA, WORK )
      ANRM1 = ANRM
      ANRM2 = ONE
      IF( ANRM.LT.ONE ) THEN
         IF( SAFMAX*ANRM.LT.ONE ) THEN
            ANRM1 = SAFMIN
            ANRM2 = SAFMAX*ANRM
         END IF
      END IF
*
      IF( ANRM.GT.ZERO ) THEN
         CALL DLASCL( 'G', -1, -1, ANRM, ONE, N, N, A, LDA, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 10
            RETURN
         END IF
      END IF
*
*     Scale B
*
      BNRM = DLANGE( 'M', N, N, B, LDB, WORK )
      BNRM1 = BNRM
      BNRM2 = ONE
      IF( BNRM.LT.ONE ) THEN
         IF( SAFMAX*BNRM.LT.ONE ) THEN
            BNRM1 = SAFMIN
            BNRM2 = SAFMAX*BNRM
         END IF
      END IF
*
      IF( BNRM.GT.ZERO ) THEN
         CALL DLASCL( 'G', -1, -1, BNRM, ONE, N, N, B, LDB, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 10
            RETURN
         END IF
      END IF
*
*     Permute the matrix to make it more nearly triangular
*     Workspace layout:  (8*N words -- "work" requires 6*N words)
*        left_permutation, right_permutation, work...
*
      ILEFT = 1
      IRIGHT = N + 1
      IWORK = IRIGHT + N
      CALL DGGBAL( 'P', N, A, LDA, B, LDB, ILO, IHI, WORK( ILEFT ),
     $             WORK( IRIGHT ), WORK( IWORK ), IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = N + 1
         GO TO 120
      END IF
*
*     Reduce B to triangular form, and initialize VL and/or VR
*     Workspace layout:  ("work..." must have at least N words)
*        left_permutation, right_permutation, tau, work...
*
      IROWS = IHI + 1 - ILO
      IF( ILV ) THEN
         ICOLS = N + 1 - ILO
      ELSE
         ICOLS = IROWS
      END IF
      ITAU = IWORK
      IWORK = ITAU + IROWS
      CALL DGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK( ITAU ),
     $             WORK( IWORK ), LWORK+1-IWORK, IINFO )
      IF( IINFO.GE.0 )
     $   LWKOPT = MAX( LWKOPT, INT( WORK( IWORK ) )+IWORK-1 )
      IF( IINFO.NE.0 ) THEN
         INFO = N + 2
         GO TO 120
      END IF
*
      CALL DORMQR( 'L', 'T', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     $             WORK( ITAU ), A( ILO, ILO ), LDA, WORK( IWORK ),
     $             LWORK+1-IWORK, IINFO )
      IF( IINFO.GE.0 )
     $   LWKOPT = MAX( LWKOPT, INT( WORK( IWORK ) )+IWORK-1 )
      IF( IINFO.NE.0 ) THEN
         INFO = N + 3
         GO TO 120
      END IF
*
      IF( ILVL ) THEN
         CALL DLASET( 'Full', N, N, ZERO, ONE, VL, LDVL )
         CALL DLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     $                VL( ILO+1, ILO ), LDVL )
         CALL DORGQR( IROWS, IROWS, IROWS, VL( ILO, ILO ), LDVL,
     $                WORK( ITAU ), WORK( IWORK ), LWORK+1-IWORK,
     $                IINFO )
         IF( IINFO.GE.0 )
     $      LWKOPT = MAX( LWKOPT, INT( WORK( IWORK ) )+IWORK-1 )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 4
            GO TO 120
         END IF
      END IF
*
      IF( ILVR )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, VR, LDVR )
*
*     Reduce to generalized Hessenberg form
*
      IF( ILV ) THEN
*
*        Eigenvectors requested -- work on whole matrix.
*
         CALL DGGHRD( JOBVL, JOBVR, N, ILO, IHI, A, LDA, B, LDB, VL,
     $                LDVL, VR, LDVR, IINFO )
      ELSE
         CALL DGGHRD( 'N', 'N', IROWS, 1, IROWS, A( ILO, ILO ), LDA,
     $                B( ILO, ILO ), LDB, VL, LDVL, VR, LDVR, IINFO )
      END IF
      IF( IINFO.NE.0 ) THEN
         INFO = N + 5
         GO TO 120
      END IF
*
*     Perform QZ algorithm
*     Workspace layout:  ("work..." must have at least 1 word)
*        left_permutation, right_permutation, work...
*
      IWORK = ITAU
      IF( ILV ) THEN
         CHTEMP = 'S'
      ELSE
         CHTEMP = 'E'
      END IF
      CALL DHGEQZ( CHTEMP, JOBVL, JOBVR, N, ILO, IHI, A, LDA, B, LDB,
     $             ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR,
     $             WORK( IWORK ), LWORK+1-IWORK, IINFO )
      IF( IINFO.GE.0 )
     $   LWKOPT = MAX( LWKOPT, INT( WORK( IWORK ) )+IWORK-1 )
      IF( IINFO.NE.0 ) THEN
         IF( IINFO.GT.0 .AND. IINFO.LE.N ) THEN
            INFO = IINFO
         ELSE IF( IINFO.GT.N .AND. IINFO.LE.2*N ) THEN
            INFO = IINFO - N
         ELSE
            INFO = N + 6
         END IF
         GO TO 120
      END IF
*
      IF( ILV ) THEN
*
*        Compute Eigenvectors  (DTGEVC requires 6*N words of workspace)
*
         IF( ILVL ) THEN
            IF( ILVR ) THEN
               CHTEMP = 'B'
            ELSE
               CHTEMP = 'L'
            END IF
         ELSE
            CHTEMP = 'R'
         END IF
*
         CALL DTGEVC( CHTEMP, 'B', LDUMMA, N, A, LDA, B, LDB, VL, LDVL,
     $                VR, LDVR, N, IN, WORK( IWORK ), IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = N + 7
            GO TO 120
         END IF
*
*        Undo balancing on VL and VR, rescale
*
         IF( ILVL ) THEN
            CALL DGGBAK( 'P', 'L', N, ILO, IHI, WORK( ILEFT ),
     $                   WORK( IRIGHT ), N, VL, LDVL, IINFO )
            IF( IINFO.NE.0 ) THEN
               INFO = N + 8
               GO TO 120
            END IF
            DO 50 JC = 1, N
               IF( ALPHAI( JC ).LT.ZERO )
     $            GO TO 50
               TEMP = ZERO
               IF( ALPHAI( JC ).EQ.ZERO ) THEN
                  DO 10 JR = 1, N
                     TEMP = MAX( TEMP, ABS( VL( JR, JC ) ) )
   10             CONTINUE
               ELSE
                  DO 20 JR = 1, N
                     TEMP = MAX( TEMP, ABS( VL( JR, JC ) )+
     $                      ABS( VL( JR, JC+1 ) ) )
   20             CONTINUE
               END IF
               IF( TEMP.LT.SAFMIN )
     $            GO TO 50
               TEMP = ONE / TEMP
               IF( ALPHAI( JC ).EQ.ZERO ) THEN
                  DO 30 JR = 1, N
                     VL( JR, JC ) = VL( JR, JC )*TEMP
   30             CONTINUE
               ELSE
                  DO 40 JR = 1, N
                     VL( JR, JC ) = VL( JR, JC )*TEMP
                     VL( JR, JC+1 ) = VL( JR, JC+1 )*TEMP
   40             CONTINUE
               END IF
   50       CONTINUE
         END IF
         IF( ILVR ) THEN
            CALL DGGBAK( 'P', 'R', N, ILO, IHI, WORK( ILEFT ),
     $                   WORK( IRIGHT ), N, VR, LDVR, IINFO )
            IF( IINFO.NE.0 ) THEN
               INFO = N + 9
               GO TO 120
            END IF
            DO 100 JC = 1, N
               IF( ALPHAI( JC ).LT.ZERO )
     $            GO TO 100
               TEMP = ZERO
               IF( ALPHAI( JC ).EQ.ZERO ) THEN
                  DO 60 JR = 1, N
                     TEMP = MAX( TEMP, ABS( VR( JR, JC ) ) )
   60             CONTINUE
               ELSE
                  DO 70 JR = 1, N
                     TEMP = MAX( TEMP, ABS( VR( JR, JC ) )+
     $                      ABS( VR( JR, JC+1 ) ) )
   70             CONTINUE
               END IF
               IF( TEMP.LT.SAFMIN )
     $            GO TO 100
               TEMP = ONE / TEMP
               IF( ALPHAI( JC ).EQ.ZERO ) THEN
                  DO 80 JR = 1, N
                     VR( JR, JC ) = VR( JR, JC )*TEMP
   80             CONTINUE
               ELSE
                  DO 90 JR = 1, N
                     VR( JR, JC ) = VR( JR, JC )*TEMP
                     VR( JR, JC+1 ) = VR( JR, JC+1 )*TEMP
   90             CONTINUE
               END IF
  100       CONTINUE
         END IF
*
*        End of eigenvector calculation
*
      END IF
*
*     Undo scaling in alpha, beta
*
*     Note: this does not give the alpha and beta for the unscaled
*     problem.
*
*     Un-scaling is limited to avoid underflow in alpha and beta
*     if they are significant.
*
      DO 110 JC = 1, N
         ABSAR = ABS( ALPHAR( JC ) )
         ABSAI = ABS( ALPHAI( JC ) )
         ABSB = ABS( BETA( JC ) )
         SALFAR = ANRM*ALPHAR( JC )
         SALFAI = ANRM*ALPHAI( JC )
         SBETA = BNRM*BETA( JC )
         ILIMIT = .FALSE.
         SCALE = ONE
*
*        Check for significant underflow in ALPHAI
*
         IF( ABS( SALFAI ).LT.SAFMIN .AND. ABSAI.GE.
     $       MAX( SAFMIN, EPS*ABSAR, EPS*ABSB ) ) THEN
            ILIMIT = .TRUE.
            SCALE = ( ONEPLS*SAFMIN / ANRM1 ) /
     $              MAX( ONEPLS*SAFMIN, ANRM2*ABSAI )
*
         ELSE IF( SALFAI.EQ.ZERO ) THEN
*
*           If insignificant underflow in ALPHAI, then make the
*           conjugate eigenvalue real.
*
            IF( ALPHAI( JC ).LT.ZERO .AND. JC.GT.1 ) THEN
               ALPHAI( JC-1 ) = ZERO
            ELSE IF( ALPHAI( JC ).GT.ZERO .AND. JC.LT.N ) THEN
               ALPHAI( JC+1 ) = ZERO
            END IF
         END IF
*
*        Check for significant underflow in ALPHAR
*
         IF( ABS( SALFAR ).LT.SAFMIN .AND. ABSAR.GE.
     $       MAX( SAFMIN, EPS*ABSAI, EPS*ABSB ) ) THEN
            ILIMIT = .TRUE.
            SCALE = MAX( SCALE, ( ONEPLS*SAFMIN / ANRM1 ) /
     $              MAX( ONEPLS*SAFMIN, ANRM2*ABSAR ) )
         END IF
*
*        Check for significant underflow in BETA
*
         IF( ABS( SBETA ).LT.SAFMIN .AND. ABSB.GE.
     $       MAX( SAFMIN, EPS*ABSAR, EPS*ABSAI ) ) THEN
            ILIMIT = .TRUE.
            SCALE = MAX( SCALE, ( ONEPLS*SAFMIN / BNRM1 ) /
     $              MAX( ONEPLS*SAFMIN, BNRM2*ABSB ) )
         END IF
*
*        Check for possible overflow when limiting scaling
*
         IF( ILIMIT ) THEN
            TEMP = ( SCALE*SAFMIN )*MAX( ABS( SALFAR ), ABS( SALFAI ),
     $             ABS( SBETA ) )
            IF( TEMP.GT.ONE )
     $         SCALE = SCALE / TEMP
            IF( SCALE.LT.ONE )
     $         ILIMIT = .FALSE.
         END IF
*
*        Recompute un-scaled ALPHAR, ALPHAI, BETA if necessary.
*
         IF( ILIMIT ) THEN
            SALFAR = ( SCALE*ALPHAR( JC ) )*ANRM
            SALFAI = ( SCALE*ALPHAI( JC ) )*ANRM
            SBETA = ( SCALE*BETA( JC ) )*BNRM
         END IF
         ALPHAR( JC ) = SALFAR
         ALPHAI( JC ) = SALFAI
         BETA( JC ) = SBETA
  110 CONTINUE
*
  120 CONTINUE
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DGEGV
*
      END
      SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK,
     $                  INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGELS solves overdetermined or underdetermined real linear systems
*  involving an M-by-N matrix A, or its transpose, using a QR or LQ
*  factorization of A.  It is assumed that A has full rank.
*
*  The following options are provided:
*
*  1. If TRANS = 'N' and m >= n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A*X ||.
*
*  2. If TRANS = 'N' and m < n:  find the minimum norm solution of
*     an underdetermined system A * X = B.
*
*  3. If TRANS = 'T' and m >= n:  find the minimum norm solution of
*     an undetermined system A**T * X = B.
*
*  4. If TRANS = 'T' and m < n:  find the least squares solution of
*     an overdetermined system, i.e., solve the least squares problem
*                  minimize || B - A**T * X ||.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  Arguments
*  =========
*
*  TRANS   (input) CHARACTER*1
*          = 'N': the linear system involves A;
*          = 'T': the linear system involves A**T.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of
*          columns of the matrices B and X. NRHS >=0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*            if M >= N, A is overwritten by details of its QR
*                       factorization as returned by DGEQRF;
*            if M <  N, A is overwritten by details of its LQ
*                       factorization as returned by DGELQF.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the matrix B of right hand side vectors, stored
*          columnwise; B is M-by-NRHS if TRANS = 'N', or N-by-NRHS
*          if TRANS = 'T'.
*          On exit, if INFO = 0, B is overwritten by the solution
*          vectors, stored columnwise:
*          if TRANS = 'N' and m >= n, rows 1 to n of B contain the least
*          squares solution vectors; the residual sum of squares for the
*          solution in each column is given by the sum of squares of
*          elements N+1 to M in that column;
*          if TRANS = 'N' and m < n, rows 1 to N of B contain the
*          minimum norm solution vectors;
*          if TRANS = 'T' and m >= n, rows 1 to M of B contain the
*          minimum norm solution vectors;
*          if TRANS = 'T' and m < n, rows 1 to M of B contain the
*          least squares solution vectors; the residual sum of squares
*          for the solution in each column is given by the sum of
*          squares of elements M+1 to N in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= MAX(1,M,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          LWORK >= max( 1, MN + max( MN, NRHS ) ).
*          For optimal performance,
*          LWORK >= max( 1, MN + max( MN, NRHS )*NB ).
*          where MN = min(M,N) and NB is the optimum block size.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO =  i, the i-th diagonal element of the
*                triangular factor of A is zero, so that A does not have
*                full rank; the least squares solution could not be
*                computed.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, TPSD
      INTEGER            BROW, I, IASCL, IBSCL, J, MN, NB, SCLLEN, WSIZE
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, SMLNUM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   RWORK( 1 )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLABAD, DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGELQF, DGEQRF, DLASCL, DLASET, DORMLQ, DORMQR,
     $                   DTRTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments.
*
      INFO = 0
      MN = MIN( M, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.( LSAME( TRANS, 'N' ) .OR. LSAME( TRANS, 'T' ) ) ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, M, N ) ) THEN
         INFO = -8
      ELSE IF( LWORK.LT.MAX( 1, MN+MAX( MN, NRHS ) ) .AND. .NOT.LQUERY )
     $          THEN
         INFO = -10
      END IF
*
*     Figure out optimal block size
*
      IF( INFO.EQ.0 .OR. INFO.EQ.-10 ) THEN
*
         TPSD = .TRUE.
         IF( LSAME( TRANS, 'N' ) )
     $      TPSD = .FALSE.
*
         IF( M.GE.N ) THEN
            NB = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
            IF( TPSD ) THEN
               NB = MAX( NB, ILAENV( 1, 'DORMQR', 'LN', M, NRHS, N,
     $              -1 ) )
            ELSE
               NB = MAX( NB, ILAENV( 1, 'DORMQR', 'LT', M, NRHS, N,
     $              -1 ) )
            END IF
         ELSE
            NB = ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
            IF( TPSD ) THEN
               NB = MAX( NB, ILAENV( 1, 'DORMLQ', 'LT', N, NRHS, M,
     $              -1 ) )
            ELSE
               NB = MAX( NB, ILAENV( 1, 'DORMLQ', 'LN', N, NRHS, M,
     $              -1 ) )
            END IF
         END IF
*
         WSIZE = MAX( 1, MN+MAX( MN, NRHS )*NB )
         WORK( 1 ) = DBLE( WSIZE )
*
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELS ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( MIN( M, N, NRHS ).EQ.0 ) THEN
         CALL DLASET( 'Full', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         RETURN
      END IF
*
*     Get machine parameters
*
      SMLNUM = DLAMCH( 'S' ) / DLAMCH( 'P' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A, B if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, RWORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         GO TO 50
      END IF
*
      BROW = M
      IF( TPSD )
     $   BROW = N
      BNRM = DLANGE( 'M', BROW, NRHS, B, LDB, RWORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, BROW, NRHS, B, LDB,
     $                INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, BROW, NRHS, B, LDB,
     $                INFO )
         IBSCL = 2
      END IF
*
      IF( M.GE.N ) THEN
*
*        compute QR factorization of A
*
         CALL DGEQRF( M, N, A, LDA, WORK( 1 ), WORK( MN+1 ), LWORK-MN,
     $                INFO )
*
*        workspace at least N, optimally N*NB
*
         IF( .NOT.TPSD ) THEN
*
*           Least-Squares Problem min || A * X - B ||
*
*           B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
*
            CALL DORMQR( 'Left', 'Transpose', M, NRHS, N, A, LDA,
     $                   WORK( 1 ), B, LDB, WORK( MN+1 ), LWORK-MN,
     $                   INFO )
*
*           workspace at least NRHS, optimally NRHS*NB
*
*           B(1:N,1:NRHS) := inv(R) * B(1:N,1:NRHS)
*
            CALL DTRTRS( 'Upper', 'No transpose', 'Non-unit', N, NRHS,
     $                   A, LDA, B, LDB, INFO )
*
            IF( INFO.GT.0 ) THEN
               RETURN
            END IF
*
            SCLLEN = N
*
         ELSE
*
*           Overdetermined system of equations A' * X = B
*
*           B(1:N,1:NRHS) := inv(R') * B(1:N,1:NRHS)
*
            CALL DTRTRS( 'Upper', 'Transpose', 'Non-unit', N, NRHS,
     $                   A, LDA, B, LDB, INFO )
*
            IF( INFO.GT.0 ) THEN
               RETURN
            END IF
*
*           B(N+1:M,1:NRHS) = ZERO
*
            DO 20 J = 1, NRHS
               DO 10 I = N + 1, M
                  B( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
*
*           B(1:M,1:NRHS) := Q(1:N,:) * B(1:N,1:NRHS)
*
            CALL DORMQR( 'Left', 'No transpose', M, NRHS, N, A, LDA,
     $                   WORK( 1 ), B, LDB, WORK( MN+1 ), LWORK-MN,
     $                   INFO )
*
*           workspace at least NRHS, optimally NRHS*NB
*
            SCLLEN = M
*
         END IF
*
      ELSE
*
*        Compute LQ factorization of A
*
         CALL DGELQF( M, N, A, LDA, WORK( 1 ), WORK( MN+1 ), LWORK-MN,
     $                INFO )
*
*        workspace at least M, optimally M*NB.
*
         IF( .NOT.TPSD ) THEN
*
*           underdetermined system of equations A * X = B
*
*           B(1:M,1:NRHS) := inv(L) * B(1:M,1:NRHS)
*
            CALL DTRTRS( 'Lower', 'No transpose', 'Non-unit', M, NRHS,
     $                   A, LDA, B, LDB, INFO )
*
            IF( INFO.GT.0 ) THEN
               RETURN
            END IF
*
*           B(M+1:N,1:NRHS) = 0
*
            DO 40 J = 1, NRHS
               DO 30 I = M + 1, N
                  B( I, J ) = ZERO
   30          CONTINUE
   40       CONTINUE
*
*           B(1:N,1:NRHS) := Q(1:N,:)' * B(1:M,1:NRHS)
*
            CALL DORMLQ( 'Left', 'Transpose', N, NRHS, M, A, LDA,
     $                   WORK( 1 ), B, LDB, WORK( MN+1 ), LWORK-MN,
     $                   INFO )
*
*           workspace at least NRHS, optimally NRHS*NB
*
            SCLLEN = N
*
         ELSE
*
*           overdetermined system min || A' * X - B ||
*
*           B(1:N,1:NRHS) := Q * B(1:N,1:NRHS)
*
            CALL DORMLQ( 'Left', 'No transpose', N, NRHS, M, A, LDA,
     $                   WORK( 1 ), B, LDB, WORK( MN+1 ), LWORK-MN,
     $                   INFO )
*
*           workspace at least NRHS, optimally NRHS*NB
*
*           B(1:M,1:NRHS) := inv(L') * B(1:M,1:NRHS)
*
            CALL DTRTRS( 'Lower', 'Transpose', 'Non-unit', M, NRHS,
     $                   A, LDA, B, LDB, INFO )
*
            IF( INFO.GT.0 ) THEN
               RETURN
            END IF
*
            SCLLEN = M
*
         END IF
*
      END IF
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, SCLLEN, NRHS, B, LDB,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, SCLLEN, NRHS, B, LDB,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, SCLLEN, NRHS, B, LDB,
     $                INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, SCLLEN, NRHS, B, LDB,
     $                INFO )
      END IF
*
   50 CONTINUE
      WORK( 1 ) = DBLE( WSIZE )
*
      RETURN
*
*     End of DGELS
*
      END
      SUBROUTINE DGELSD( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
     $                   WORK, LWORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGELSD computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize 2-norm(| b - A*x |)
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  The problem is solved in three steps:
*  (1) Reduce the coefficient matrix A to bidiagonal form with
*      Householder transformations, reducing the original problem
*      into a "bidiagonal least squares problem" (BLS)
*  (2) Solve the BLS using a divide and conquer approach.
*  (3) Apply back all the Householder tranformations to solve
*      the original least squares problem.
*
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of A. M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of A. N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X. NRHS >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, A has been destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, B is overwritten by the N-by-NRHS solution
*          matrix X.  If m >= n and RANK = n, the residual
*          sum-of-squares for the solution in the i-th column is given
*          by the sum of squares of elements n+1:m in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,max(M,N)).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A in decreasing order.
*          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A.
*          Singular values S(i) <= RCOND*S(1) are treated as zero.
*          If RCOND < 0, machine precision is used instead.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the number of singular values
*          which are greater than RCOND*S(1).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK must be at least 1.
*          The exact minimum amount of workspace needed depends on M,
*          N and NRHS. As long as LWORK is at least
*              12*N + 2*N*SMLSIZ + 8*N*NLVL + N*NRHS + (SMLSIZ+1)**2,
*          if M is greater than or equal to N or
*              12*M + 2*M*SMLSIZ + 8*M*NLVL + M*NRHS + (SMLSIZ+1)**2,
*          if M is less than N, the code will execute correctly.
*          SMLSIZ is returned by ILAENV and is equal to the maximum
*          size of the subproblems at the bottom of the computation
*          tree (usually about 25), and
*             NLVL = MAX( 0, INT( LOG_2( MIN( M,N )/(SMLSIZ+1) ) ) + 1 )
*          For good performance, LWORK should generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace) INTEGER array, dimension (MAX(1,LIWORK))
*          LIWORK >= 3 * MINMN * NLVL + 11 * MINMN,
*          where MINMN = MIN( M,N ).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  the algorithm for computing the SVD failed to converge;
*                if INFO = i, i off-diagonal elements of an intermediate
*                bidiagonal form did not converge to zero.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Ming Gu and Ren-Cang Li, Computer Science Division, University of
*       California at Berkeley, USA
*     Osni Marques, LBNL/NERSC, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            IASCL, IBSCL, IE, IL, ITAU, ITAUP, ITAUQ,
     $                   LDWORK, MAXMN, MAXWRK, MINMN, MINWRK, MM,
     $                   MNTHR, NLVL, NWORK, SMLSIZ, WLALSD
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, EPS, SFMIN, SMLNUM
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEBRD, DGELQF, DGEQRF, DLABAD, DLACPY, DLALSD,
     $                   DLASCL, DLASET, DORMBR, DORMLQ, DORMQR, XERBLA
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, INT, LOG, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments.
*
      INFO = 0
      MINMN = MIN( M, N )
      MAXMN = MAX( M, N )
      MNTHR = ILAENV( 6, 'DGELSD', ' ', M, N, NRHS, -1 )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, MAXMN ) ) THEN
         INFO = -7
      END IF
*
      SMLSIZ = ILAENV( 9, 'DGELSD', ' ', 0, 0, 0, 0 )
*
*     Compute workspace.
*     (Note: Comments in the code beginning "Workspace:" describe the
*     minimal amount of workspace needed at that point in the code,
*     as well as the preferred amount for good performance.
*     NB refers to the optimal block size for the immediately
*     following subroutine, as returned by ILAENV.)
*
      MINWRK = 1
      MINMN = MAX( 1, MINMN )
      NLVL = MAX( INT( LOG( DBLE( MINMN ) / DBLE( SMLSIZ+1 ) ) /
     $       LOG( TWO ) ) + 1, 0 )
*
      IF( INFO.EQ.0 ) THEN
         MAXWRK = 0
         MM = M
         IF( M.GE.N .AND. M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns.
*
            MM = N
            MAXWRK = MAX( MAXWRK, N+N*ILAENV( 1, 'DGEQRF', ' ', M, N,
     $               -1, -1 ) )
            MAXWRK = MAX( MAXWRK, N+NRHS*
     $               ILAENV( 1, 'DORMQR', 'LT', M, NRHS, N, -1 ) )
         END IF
         IF( M.GE.N ) THEN
*
*           Path 1 - overdetermined or exactly determined.
*
            MAXWRK = MAX( MAXWRK, 3*N+( MM+N )*
     $               ILAENV( 1, 'DGEBRD', ' ', MM, N, -1, -1 ) )
            MAXWRK = MAX( MAXWRK, 3*N+NRHS*
     $               ILAENV( 1, 'DORMBR', 'QLT', MM, NRHS, N, -1 ) )
            MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $               ILAENV( 1, 'DORMBR', 'PLN', N, NRHS, N, -1 ) )
            WLALSD = 9*N+2*N*SMLSIZ+8*N*NLVL+N*NRHS+(SMLSIZ+1)**2
            MAXWRK = MAX( MAXWRK, 3*N+WLALSD )
            MINWRK = MAX( 3*N+MM, 3*N+NRHS, 3*N+WLALSD )
         END IF
         IF( N.GT.M ) THEN
            WLALSD = 9*M+2*M*SMLSIZ+8*M*NLVL+M*NRHS+(SMLSIZ+1)**2
            IF( N.GE.MNTHR ) THEN
*
*              Path 2a - underdetermined, with many more columns
*              than rows.
*
               MAXWRK = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
               MAXWRK = MAX( MAXWRK, M*M+4*M+2*M*
     $                  ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+NRHS*
     $                  ILAENV( 1, 'DORMBR', 'QLT', M, NRHS, M, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+( M-1 )*
     $                  ILAENV( 1, 'DORMBR', 'PLN', M, NRHS, M, -1 ) )
               IF( NRHS.GT.1 ) THEN
                  MAXWRK = MAX( MAXWRK, M*M+M+M*NRHS )
               ELSE
                  MAXWRK = MAX( MAXWRK, M*M+2*M )
               END IF
               MAXWRK = MAX( MAXWRK, M+NRHS*
     $                  ILAENV( 1, 'DORMLQ', 'LT', N, NRHS, M, -1 ) )
               MAXWRK = MAX( MAXWRK, M*M+4*M+WLALSD )
            ELSE
*
*              Path 2 - remaining underdetermined cases.
*
               MAXWRK = 3*M + ( N+M )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $                  -1, -1 )
               MAXWRK = MAX( MAXWRK, 3*M+NRHS*
     $                  ILAENV( 1, 'DORMBR', 'QLT', M, NRHS, N, -1 ) )
               MAXWRK = MAX( MAXWRK, 3*M+M*
     $                  ILAENV( 1, 'DORMBR', 'PLN', N, NRHS, M, -1 ) )
               MAXWRK = MAX( MAXWRK, 3*M+WLALSD )
            END IF
            MINWRK = MAX( 3*M+NRHS, 3*M+M, 3*M+WLALSD )
         END IF
         MINWRK = MIN( MINWRK, MAXWRK )
         WORK( 1 ) = MAXWRK
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         GO TO 10
      END IF
*
*     Quick return if possible.
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters.
*
      EPS = DLAMCH( 'P' )
      SFMIN = DLAMCH( 'S' )
      SMLNUM = SFMIN / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A if max entry outside range [SMLNUM,BIGNUM].
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM.
*
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM.
*
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         CALL DLASET( 'F', MINMN, 1, ZERO, ZERO, S, 1 )
         RANK = 0
         GO TO 10
      END IF
*
*     Scale B if max entry outside range [SMLNUM,BIGNUM].
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM.
*
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM.
*
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     If M < N make sure certain entries of B are zero.
*
      IF( M.LT.N )
     $   CALL DLASET( 'F', N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
*
*     Overdetermined case.
*
      IF( M.GE.N ) THEN
*
*        Path 1 - overdetermined or exactly determined.
*
         MM = M
         IF( M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns.
*
            MM = N
            ITAU = 1
            NWORK = ITAU + N
*
*           Compute A=Q*R.
*           (Workspace: need 2*N, prefer N+N*NB)
*
            CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                   LWORK-NWORK+1, INFO )
*
*           Multiply B by transpose(Q).
*           (Workspace: need N+NRHS, prefer N+NRHS*NB)
*
            CALL DORMQR( 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAU ), B,
     $                   LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*           Zero out below R.
*
            IF( N.GT.1 ) THEN
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
            END IF
         END IF
*
         IE = 1
         ITAUQ = IE + N
         ITAUP = ITAUQ + N
         NWORK = ITAUP + N
*
*        Bidiagonalize R in A.
*        (Workspace: need 3*N+MM, prefer 3*N+(MM+N)*NB)
*
         CALL DGEBRD( MM, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors of R.
*        (Workspace: need 3*N+NRHS, prefer 3*N+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', MM, NRHS, N, A, LDA, WORK( ITAUQ ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL DLALSD( 'U', SMLSIZ, N, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of R.
*
         CALL DORMBR( 'P', 'L', 'N', N, NRHS, N, A, LDA, WORK( ITAUP ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      ELSE IF( N.GE.MNTHR .AND. LWORK.GE.4*M+M*M+
     $         MAX( M, 2*M-4, NRHS, N-3*M, WLALSD ) ) THEN
*
*        Path 2a - underdetermined, with many more columns than rows
*        and sufficient workspace for an efficient algorithm.
*
         LDWORK = M
         IF( LWORK.GE.MAX( 4*M+M*LDA+MAX( M, 2*M-4, NRHS, N-3*M ),
     $       M*LDA+M+M*NRHS, 4*M+M*LDA+WLALSD ) )LDWORK = LDA
         ITAU = 1
         NWORK = M + 1
*
*        Compute A=L*Q.
*        (Workspace: need 2*M, prefer M+M*NB)
*
         CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
         IL = NWORK
*
*        Copy L to WORK(IL), zeroing out above its diagonal.
*
         CALL DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWORK )
         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, WORK( IL+LDWORK ),
     $                LDWORK )
         IE = IL + LDWORK*M
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         NWORK = ITAUP + M
*
*        Bidiagonalize L in WORK(IL).
*        (Workspace: need M*M+5*M, prefer M*M+4*M+2*M*NB)
*
         CALL DGEBRD( M, M, WORK( IL ), LDWORK, S, WORK( IE ),
     $                WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors of L.
*        (Workspace: need M*M+4*M+NRHS, prefer M*M+4*M+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', M, NRHS, M, WORK( IL ), LDWORK,
     $                WORK( ITAUQ ), B, LDB, WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL DLALSD( 'U', SMLSIZ, M, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of L.
*
         CALL DORMBR( 'P', 'L', 'N', M, NRHS, M, WORK( IL ), LDWORK,
     $                WORK( ITAUP ), B, LDB, WORK( NWORK ),
     $                LWORK-NWORK+1, INFO )
*
*        Zero out below first M rows of B.
*
         CALL DLASET( 'F', N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
         NWORK = ITAU + M
*
*        Multiply transpose(Q) by B.
*        (Workspace: need M+NRHS, prefer M+NRHS*NB)
*
         CALL DORMLQ( 'L', 'T', N, NRHS, M, A, LDA, WORK( ITAU ), B,
     $                LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      ELSE
*
*        Path 2 - remaining underdetermined cases.
*
         IE = 1
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         NWORK = ITAUP + M
*
*        Bidiagonalize A.
*        (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
         CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors.
*        (Workspace: need 3*M+NRHS, prefer 3*M+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAUQ ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
*        Solve the bidiagonal least squares problem.
*
         CALL DLALSD( 'L', SMLSIZ, M, NRHS, S, WORK( IE ), B, LDB,
     $                RCOND, RANK, WORK( NWORK ), IWORK, INFO )
         IF( INFO.NE.0 ) THEN
            GO TO 10
         END IF
*
*        Multiply B by right bidiagonalizing vectors of A.
*
         CALL DORMBR( 'P', 'L', 'N', N, NRHS, M, A, LDA, WORK( ITAUP ),
     $                B, LDB, WORK( NWORK ), LWORK-NWORK+1, INFO )
*
      END IF
*
*     Undo scaling.
*
      IF( IASCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
   10 CONTINUE
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of DGELSD
*
      END
      SUBROUTINE DGELSS( M, N, NRHS, A, LDA, B, LDB, S, RCOND, RANK,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), S( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGELSS computes the minimum norm solution to a real linear least
*  squares problem:
*
*  Minimize 2-norm(| b - A*x |).
*
*  using the singular value decomposition (SVD) of A. A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
*  X.
*
*  The effective rank of A is determined by treating as zero those
*  singular values which are less than RCOND times the largest singular
*  value.
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
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X. NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the first min(m,n) rows of A are overwritten with
*          its right singular vectors, stored rowwise.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, B is overwritten by the N-by-NRHS solution
*          matrix X.  If m >= n and RANK = n, the residual
*          sum-of-squares for the solution in the i-th column is given
*          by the sum of squares of elements n+1:m in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,max(M,N)).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A in decreasing order.
*          The condition number of A in the 2-norm = S(1)/S(min(m,n)).
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A.
*          Singular values S(i) <= RCOND*S(1) are treated as zero.
*          If RCOND < 0, machine precision is used instead.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the number of singular values
*          which are greater than RCOND*S(1).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 1, and also:
*          LWORK >= 3*min(M,N) + max( 2*min(M,N), max(M,N), NRHS )
*          For good performance, LWORK should generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  the algorithm for computing the SVD failed to converge;
*                if INFO = i, i off-diagonal elements of an intermediate
*                bidiagonal form did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            BDSPAC, BL, CHUNK, I, IASCL, IBSCL, IE, IL,
     $                   ITAU, ITAUP, ITAUQ, IWORK, LDWORK, MAXMN,
     $                   MAXWRK, MINMN, MINWRK, MM, MNTHR
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, EPS, SFMIN, SMLNUM, THR
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   VDUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSQR, DCOPY, DGEBRD, DGELQF, DGEMM, DGEMV,
     $                   DGEQRF, DLABAD, DLACPY, DLASCL, DLASET, DORGBR,
     $                   DORMBR, DORMLQ, DORMQR, DRSCL, XERBLA
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      MINMN = MIN( M, N )
      MAXMN = MAX( M, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, MAXMN ) ) THEN
         INFO = -7
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 ) THEN
         MINWRK = 1
         MAXWRK = 1
         IF( MINMN.GT.0 ) THEN
            MM = M
            MNTHR = ILAENV( 6, 'DGELSS', ' ', M, N, NRHS, -1 )
            IF( M.GE.N .AND. M.GE.MNTHR ) THEN
*
*              Path 1a - overdetermined, with many more rows than
*                        columns
*
               MM = N
               MAXWRK = MAX( MAXWRK, N + N*ILAENV( 1, 'DGEQRF', ' ', M,
     $                       N, -1, -1 ) )
               MAXWRK = MAX( MAXWRK, N + NRHS*ILAENV( 1, 'DORMQR', 'LT',
     $                       M, NRHS, N, -1 ) )
            END IF
            IF( M.GE.N ) THEN
*
*              Path 1 - overdetermined or exactly determined
*
*              Compute workspace needed for DBDSQR
*
               BDSPAC = MAX( 1, 5*N )
               MAXWRK = MAX( MAXWRK, 3*N + ( MM + N )*ILAENV( 1,
     $                       'DGEBRD', ' ', MM, N, -1, -1 ) )
               MAXWRK = MAX( MAXWRK, 3*N + NRHS*ILAENV( 1, 'DORMBR',
     $                       'QLT', MM, NRHS, N, -1 ) )
               MAXWRK = MAX( MAXWRK, 3*N + ( N - 1 )*ILAENV( 1,
     $                       'DORGBR', 'P', N, N, N, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MAXWRK = MAX( MAXWRK, N*NRHS )
               MINWRK = MAX( 3*N + MM, 3*N + NRHS, BDSPAC )
               MAXWRK = MAX( MINWRK, MAXWRK )
            END IF
            IF( N.GT.M ) THEN
*
*              Compute workspace needed for DBDSQR
*
               BDSPAC = MAX( 1, 5*M )
               MINWRK = MAX( 3*M+NRHS, 3*M+N, BDSPAC )
               IF( N.GE.MNTHR ) THEN
*
*                 Path 2a - underdetermined, with many more columns
*                 than rows
*
                  MAXWRK = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1,
     $                                  -1 )
                  MAXWRK = MAX( MAXWRK, M*M + 4*M + 2*M*ILAENV( 1,
     $                          'DGEBRD', ' ', M, M, -1, -1 ) )
                  MAXWRK = MAX( MAXWRK, M*M + 4*M + NRHS*ILAENV( 1,
     $                          'DORMBR', 'QLT', M, NRHS, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, M*M + 4*M +
     $                          ( M - 1 )*ILAENV( 1, 'DORGBR', 'P', M,
     $                          M, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, M*M + M + BDSPAC )
                  IF( NRHS.GT.1 ) THEN
                     MAXWRK = MAX( MAXWRK, M*M + M + M*NRHS )
                  ELSE
                     MAXWRK = MAX( MAXWRK, M*M + 2*M )
                  END IF
                  MAXWRK = MAX( MAXWRK, M + NRHS*ILAENV( 1, 'DORMLQ',
     $                          'LT', N, NRHS, M, -1 ) )
               ELSE
*
*                 Path 2 - underdetermined
*
                  MAXWRK = 3*M + ( N + M )*ILAENV( 1, 'DGEBRD', ' ', M,
     $                     N, -1, -1 )
                  MAXWRK = MAX( MAXWRK, 3*M + NRHS*ILAENV( 1, 'DORMBR',
     $                          'QLT', M, NRHS, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, 3*M + M*ILAENV( 1, 'DORGBR',
     $                          'P', M, N, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MAXWRK = MAX( MAXWRK, N*NRHS )
               END IF
            END IF
            MAXWRK = MAX( MINWRK, MAXWRK )
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY )
     $      INFO = -12
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSS', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters
*
      EPS = DLAMCH( 'P' )
      SFMIN = DLAMCH( 'S' )
      SMLNUM = SFMIN / EPS
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         CALL DLASET( 'F', MINMN, 1, ZERO, ZERO, S, 1 )
         RANK = 0
         GO TO 70
      END IF
*
*     Scale B if max element outside range [SMLNUM,BIGNUM]
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     Overdetermined case
*
      IF( M.GE.N ) THEN
*
*        Path 1 - overdetermined or exactly determined
*
         MM = M
         IF( M.GE.MNTHR ) THEN
*
*           Path 1a - overdetermined, with many more rows than columns
*
            MM = N
            ITAU = 1
            IWORK = ITAU + N
*
*           Compute A=Q*R
*           (Workspace: need 2*N, prefer N+N*NB)
*
            CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                   LWORK-IWORK+1, INFO )
*
*           Multiply B by transpose(Q)
*           (Workspace: need N+NRHS, prefer N+NRHS*NB)
*
            CALL DORMQR( 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAU ), B,
     $                   LDB, WORK( IWORK ), LWORK-IWORK+1, INFO )
*
*           Zero out below R
*
            IF( N.GT.1 )
     $         CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
         END IF
*
         IE = 1
         ITAUQ = IE + N
         ITAUP = ITAUQ + N
         IWORK = ITAUP + N
*
*        Bidiagonalize R in A
*        (Workspace: need 3*N+MM, prefer 3*N+(MM+N)*NB)
*
         CALL DGEBRD( MM, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors of R
*        (Workspace: need 3*N+NRHS, prefer 3*N+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', MM, NRHS, N, A, LDA, WORK( ITAUQ ),
     $                B, LDB, WORK( IWORK ), LWORK-IWORK+1, INFO )
*
*        Generate right bidiagonalizing vectors of R in A
*        (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
         CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                WORK( IWORK ), LWORK-IWORK+1, INFO )
         IWORK = IE + N
*
*        Perform bidiagonal QR iteration
*          multiply B by transpose of left singular vectors
*          compute right singular vectors in A
*        (Workspace: need BDSPAC)
*
         CALL DBDSQR( 'U', N, N, 0, NRHS, S, WORK( IE ), A, LDA, VDUM,
     $                1, B, LDB, WORK( IWORK ), INFO )
         IF( INFO.NE.0 )
     $      GO TO 70
*
*        Multiply B by reciprocals of singular values
*
         THR = MAX( RCOND*S( 1 ), SFMIN )
         IF( RCOND.LT.ZERO )
     $      THR = MAX( EPS*S( 1 ), SFMIN )
         RANK = 0
         DO 10 I = 1, N
            IF( S( I ).GT.THR ) THEN
               CALL DRSCL( NRHS, S( I ), B( I, 1 ), LDB )
               RANK = RANK + 1
            ELSE
               CALL DLASET( 'F', 1, NRHS, ZERO, ZERO, B( I, 1 ), LDB )
            END IF
   10    CONTINUE
*
*        Multiply B by right singular vectors
*        (Workspace: need N, prefer N*NRHS)
*
         IF( LWORK.GE.LDB*NRHS .AND. NRHS.GT.1 ) THEN
            CALL DGEMM( 'T', 'N', N, NRHS, N, ONE, A, LDA, B, LDB, ZERO,
     $                  WORK, LDB )
            CALL DLACPY( 'G', N, NRHS, WORK, LDB, B, LDB )
         ELSE IF( NRHS.GT.1 ) THEN
            CHUNK = LWORK / N
            DO 20 I = 1, NRHS, CHUNK
               BL = MIN( NRHS-I+1, CHUNK )
               CALL DGEMM( 'T', 'N', N, BL, N, ONE, A, LDA, B( 1, I ),
     $                     LDB, ZERO, WORK, N )
               CALL DLACPY( 'G', N, BL, WORK, N, B( 1, I ), LDB )
   20       CONTINUE
         ELSE
            CALL DGEMV( 'T', N, N, ONE, A, LDA, B, 1, ZERO, WORK, 1 )
            CALL DCOPY( N, WORK, 1, B, 1 )
         END IF
*
      ELSE IF( N.GE.MNTHR .AND. LWORK.GE.4*M+M*M+
     $         MAX( M, 2*M-4, NRHS, N-3*M ) ) THEN
*
*        Path 2a - underdetermined, with many more columns than rows
*        and sufficient workspace for an efficient algorithm
*
         LDWORK = M
         IF( LWORK.GE.MAX( 4*M+M*LDA+MAX( M, 2*M-4, NRHS, N-3*M ),
     $       M*LDA+M+M*NRHS ) )LDWORK = LDA
         ITAU = 1
         IWORK = M + 1
*
*        Compute A=L*Q
*        (Workspace: need 2*M, prefer M+M*NB)
*
         CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                LWORK-IWORK+1, INFO )
         IL = IWORK
*
*        Copy L to WORK(IL), zeroing out above it
*
         CALL DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWORK )
         CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, WORK( IL+LDWORK ),
     $                LDWORK )
         IE = IL + LDWORK*M
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         IWORK = ITAUP + M
*
*        Bidiagonalize L in WORK(IL)
*        (Workspace: need M*M+5*M, prefer M*M+4*M+2*M*NB)
*
         CALL DGEBRD( M, M, WORK( IL ), LDWORK, S, WORK( IE ),
     $                WORK( ITAUQ ), WORK( ITAUP ), WORK( IWORK ),
     $                LWORK-IWORK+1, INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors of L
*        (Workspace: need M*M+4*M+NRHS, prefer M*M+4*M+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', M, NRHS, M, WORK( IL ), LDWORK,
     $                WORK( ITAUQ ), B, LDB, WORK( IWORK ),
     $                LWORK-IWORK+1, INFO )
*
*        Generate right bidiagonalizing vectors of R in WORK(IL)
*        (Workspace: need M*M+5*M-1, prefer M*M+4*M+(M-1)*NB)
*
         CALL DORGBR( 'P', M, M, M, WORK( IL ), LDWORK, WORK( ITAUP ),
     $                WORK( IWORK ), LWORK-IWORK+1, INFO )
         IWORK = IE + M
*
*        Perform bidiagonal QR iteration,
*           computing right singular vectors of L in WORK(IL) and
*           multiplying B by transpose of left singular vectors
*        (Workspace: need M*M+M+BDSPAC)
*
         CALL DBDSQR( 'U', M, M, 0, NRHS, S, WORK( IE ), WORK( IL ),
     $                LDWORK, A, LDA, B, LDB, WORK( IWORK ), INFO )
         IF( INFO.NE.0 )
     $      GO TO 70
*
*        Multiply B by reciprocals of singular values
*
         THR = MAX( RCOND*S( 1 ), SFMIN )
         IF( RCOND.LT.ZERO )
     $      THR = MAX( EPS*S( 1 ), SFMIN )
         RANK = 0
         DO 30 I = 1, M
            IF( S( I ).GT.THR ) THEN
               CALL DRSCL( NRHS, S( I ), B( I, 1 ), LDB )
               RANK = RANK + 1
            ELSE
               CALL DLASET( 'F', 1, NRHS, ZERO, ZERO, B( I, 1 ), LDB )
            END IF
   30    CONTINUE
         IWORK = IE
*
*        Multiply B by right singular vectors of L in WORK(IL)
*        (Workspace: need M*M+2*M, prefer M*M+M+M*NRHS)
*
         IF( LWORK.GE.LDB*NRHS+IWORK-1 .AND. NRHS.GT.1 ) THEN
            CALL DGEMM( 'T', 'N', M, NRHS, M, ONE, WORK( IL ), LDWORK,
     $                  B, LDB, ZERO, WORK( IWORK ), LDB )
            CALL DLACPY( 'G', M, NRHS, WORK( IWORK ), LDB, B, LDB )
         ELSE IF( NRHS.GT.1 ) THEN
            CHUNK = ( LWORK-IWORK+1 ) / M
            DO 40 I = 1, NRHS, CHUNK
               BL = MIN( NRHS-I+1, CHUNK )
               CALL DGEMM( 'T', 'N', M, BL, M, ONE, WORK( IL ), LDWORK,
     $                     B( 1, I ), LDB, ZERO, WORK( IWORK ), M )
               CALL DLACPY( 'G', M, BL, WORK( IWORK ), M, B( 1, I ),
     $                      LDB )
   40       CONTINUE
         ELSE
            CALL DGEMV( 'T', M, M, ONE, WORK( IL ), LDWORK, B( 1, 1 ),
     $                  1, ZERO, WORK( IWORK ), 1 )
            CALL DCOPY( M, WORK( IWORK ), 1, B( 1, 1 ), 1 )
         END IF
*
*        Zero out below first M rows of B
*
         CALL DLASET( 'F', N-M, NRHS, ZERO, ZERO, B( M+1, 1 ), LDB )
         IWORK = ITAU + M
*
*        Multiply transpose(Q) by B
*        (Workspace: need M+NRHS, prefer M+NRHS*NB)
*
         CALL DORMLQ( 'L', 'T', N, NRHS, M, A, LDA, WORK( ITAU ), B,
     $                LDB, WORK( IWORK ), LWORK-IWORK+1, INFO )
*
      ELSE
*
*        Path 2 - remaining underdetermined cases
*
         IE = 1
         ITAUQ = IE + M
         ITAUP = ITAUQ + M
         IWORK = ITAUP + M
*
*        Bidiagonalize A
*        (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
         CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                INFO )
*
*        Multiply B by transpose of left bidiagonalizing vectors
*        (Workspace: need 3*M+NRHS, prefer 3*M+NRHS*NB)
*
         CALL DORMBR( 'Q', 'L', 'T', M, NRHS, N, A, LDA, WORK( ITAUQ ),
     $                B, LDB, WORK( IWORK ), LWORK-IWORK+1, INFO )
*
*        Generate right bidiagonalizing vectors in A
*        (Workspace: need 4*M, prefer 3*M+M*NB)
*
         CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                WORK( IWORK ), LWORK-IWORK+1, INFO )
         IWORK = IE + M
*
*        Perform bidiagonal QR iteration,
*           computing right singular vectors of A in A and
*           multiplying B by transpose of left singular vectors
*        (Workspace: need BDSPAC)
*
         CALL DBDSQR( 'L', M, N, 0, NRHS, S, WORK( IE ), A, LDA, VDUM,
     $                1, B, LDB, WORK( IWORK ), INFO )
         IF( INFO.NE.0 )
     $      GO TO 70
*
*        Multiply B by reciprocals of singular values
*
         THR = MAX( RCOND*S( 1 ), SFMIN )
         IF( RCOND.LT.ZERO )
     $      THR = MAX( EPS*S( 1 ), SFMIN )
         RANK = 0
         DO 50 I = 1, M
            IF( S( I ).GT.THR ) THEN
               CALL DRSCL( NRHS, S( I ), B( I, 1 ), LDB )
               RANK = RANK + 1
            ELSE
               CALL DLASET( 'F', 1, NRHS, ZERO, ZERO, B( I, 1 ), LDB )
            END IF
   50    CONTINUE
*
*        Multiply B by right singular vectors of A
*        (Workspace: need N, prefer N*NRHS)
*
         IF( LWORK.GE.LDB*NRHS .AND. NRHS.GT.1 ) THEN
            CALL DGEMM( 'T', 'N', N, NRHS, M, ONE, A, LDA, B, LDB, ZERO,
     $                  WORK, LDB )
            CALL DLACPY( 'F', N, NRHS, WORK, LDB, B, LDB )
         ELSE IF( NRHS.GT.1 ) THEN
            CHUNK = LWORK / N
            DO 60 I = 1, NRHS, CHUNK
               BL = MIN( NRHS-I+1, CHUNK )
               CALL DGEMM( 'T', 'N', N, BL, M, ONE, A, LDA, B( 1, I ),
     $                     LDB, ZERO, WORK, N )
               CALL DLACPY( 'F', N, BL, WORK, N, B( 1, I ), LDB )
   60       CONTINUE
         ELSE
            CALL DGEMV( 'T', M, N, ONE, A, LDA, B, 1, ZERO, WORK, 1 )
            CALL DCOPY( N, WORK, 1, B, 1 )
         END IF
      END IF
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
   70 CONTINUE
      WORK( 1 ) = MAXWRK
      RETURN
*
*     End of DGELSS
*
      END
      SUBROUTINE DGELSX( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
     $                   WORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  This routine is deprecated and has been replaced by routine DGELSY.
*
*  DGELSX computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
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
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of
*          columns of matrices B and X. NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, A has been overwritten by details of its
*          complete orthogonal factorization.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, the N-by-NRHS solution matrix X.
*          If m >= n and RANK = n, the residual sum-of-squares for
*          the solution in the i-th column is given by the sum of
*          squares of elements N+1:M in that column.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,M,N).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(i) .ne. 0, the i-th column of A is an
*          initial column, otherwise it is a free column.  Before
*          the QR factorization of A, all initial columns are
*          permuted to the leading positions; only the remaining
*          free columns are moved as a result of column pivoting
*          during the factorization.
*          On exit, if JPVT(i) = k, then the i-th column of A*P
*          was the k-th column of A.
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A, which
*          is defined as the order of the largest leading triangular
*          submatrix R11 in the QR factorization with pivoting of A,
*          whose estimated condition number < 1/RCOND.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the order of the submatrix
*          R11.  This is the same as the order of the submatrix T11
*          in the complete orthogonal factorization of A.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                      (max( min(M,N)+3*N, 2*min(M,N)+NRHS )),
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            IMAX, IMIN
      PARAMETER          ( IMAX = 1, IMIN = 2 )
      DOUBLE PRECISION   ZERO, ONE, DONE, NTDONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, DONE = ZERO,
     $                   NTDONE = ONE )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IASCL, IBSCL, ISMAX, ISMIN, J, K, MN
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, C1, C2, S1, S2, SMAX,
     $                   SMAXPR, SMIN, SMINPR, SMLNUM, T1, T2
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQPF, DLAIC1, DLASCL, DLASET, DLATZM, DORM2R,
     $                   DTRSM, DTZRQF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M, N )
      ISMIN = MN + 1
      ISMAX = 2*MN + 1
*
*     Test the input arguments.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, M, N ) ) THEN
         INFO = -7
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( MIN( M, N, NRHS ).EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters
*
      SMLNUM = DLAMCH( 'S' ) / DLAMCH( 'P' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A, B if max elements outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         RANK = 0
         GO TO 100
      END IF
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     Compute QR factorization with column pivoting of A:
*        A * P = Q * R
*
      CALL DGEQPF( M, N, A, LDA, JPVT, WORK( 1 ), WORK( MN+1 ), INFO )
*
*     workspace 3*N. Details of Householder rotations stored
*     in WORK(1:MN).
*
*     Determine RANK using incremental condition estimation
*
      WORK( ISMIN ) = ONE
      WORK( ISMAX ) = ONE
      SMAX = ABS( A( 1, 1 ) )
      SMIN = SMAX
      IF( ABS( A( 1, 1 ) ).EQ.ZERO ) THEN
         RANK = 0
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         GO TO 100
      ELSE
         RANK = 1
      END IF
*
   10 CONTINUE
      IF( RANK.LT.MN ) THEN
         I = RANK + 1
         CALL DLAIC1( IMIN, RANK, WORK( ISMIN ), SMIN, A( 1, I ),
     $                A( I, I ), SMINPR, S1, C1 )
         CALL DLAIC1( IMAX, RANK, WORK( ISMAX ), SMAX, A( 1, I ),
     $                A( I, I ), SMAXPR, S2, C2 )
*
         IF( SMAXPR*RCOND.LE.SMINPR ) THEN
            DO 20 I = 1, RANK
               WORK( ISMIN+I-1 ) = S1*WORK( ISMIN+I-1 )
               WORK( ISMAX+I-1 ) = S2*WORK( ISMAX+I-1 )
   20       CONTINUE
            WORK( ISMIN+RANK ) = C1
            WORK( ISMAX+RANK ) = C2
            SMIN = SMINPR
            SMAX = SMAXPR
            RANK = RANK + 1
            GO TO 10
         END IF
      END IF
*
*     Logically partition R = [ R11 R12 ]
*                             [  0  R22 ]
*     where R11 = R(1:RANK,1:RANK)
*
*     [R11,R12] = [ T11, 0 ] * Y
*
      IF( RANK.LT.N )
     $   CALL DTZRQF( RANK, N, A, LDA, WORK( MN+1 ), INFO )
*
*     Details of Householder rotations stored in WORK(MN+1:2*MN)
*
*     B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
*
      CALL DORM2R( 'Left', 'Transpose', M, NRHS, MN, A, LDA, WORK( 1 ),
     $             B, LDB, WORK( 2*MN+1 ), INFO )
*
*     workspace NRHS
*
*     B(1:RANK,1:NRHS) := inv(T11) * B(1:RANK,1:NRHS)
*
      CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', RANK,
     $            NRHS, ONE, A, LDA, B, LDB )
*
      DO 40 I = RANK + 1, N
         DO 30 J = 1, NRHS
            B( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
*
*     B(1:N,1:NRHS) := Y' * B(1:N,1:NRHS)
*
      IF( RANK.LT.N ) THEN
         DO 50 I = 1, RANK
            CALL DLATZM( 'Left', N-RANK+1, NRHS, A( I, RANK+1 ), LDA,
     $                   WORK( MN+I ), B( I, 1 ), B( RANK+1, 1 ), LDB,
     $                   WORK( 2*MN+1 ) )
   50    CONTINUE
      END IF
*
*     workspace NRHS
*
*     B(1:N,1:NRHS) := P * B(1:N,1:NRHS)
*
      DO 90 J = 1, NRHS
         DO 60 I = 1, N
            WORK( 2*MN+I ) = NTDONE
   60    CONTINUE
         DO 80 I = 1, N
            IF( WORK( 2*MN+I ).EQ.NTDONE ) THEN
               IF( JPVT( I ).NE.I ) THEN
                  K = I
                  T1 = B( K, J )
                  T2 = B( JPVT( K ), J )
   70             CONTINUE
                  B( JPVT( K ), J ) = T1
                  WORK( 2*MN+K ) = DONE
                  T1 = T2
                  K = JPVT( K )
                  T2 = B( JPVT( K ), J )
                  IF( JPVT( K ).NE.I )
     $               GO TO 70
                  B( I, J ) = T1
                  WORK( 2*MN+K ) = DONE
               END IF
            END IF
   80    CONTINUE
   90 CONTINUE
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'U', 0, 0, SMLNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'U', 0, 0, BIGNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
  100 CONTINUE
*
      RETURN
*
*     End of DGELSX
*
      END
      SUBROUTINE DGELSY( M, N, NRHS, A, LDA, B, LDB, JPVT, RCOND, RANK,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS, RANK
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGELSY computes the minimum-norm solution to a real linear least
*  squares problem:
*      minimize || A * X - B ||
*  using a complete orthogonal factorization of A.  A is an M-by-N
*  matrix which may be rank-deficient.
*
*  Several right hand side vectors b and solution vectors x can be
*  handled in a single call; they are stored as the columns of the
*  M-by-NRHS right hand side matrix B and the N-by-NRHS solution
*  matrix X.
*
*  The routine first computes a QR factorization with column pivoting:
*      A * P = Q * [ R11 R12 ]
*                  [  0  R22 ]
*  with R11 defined as the largest leading submatrix whose estimated
*  condition number is less than 1/RCOND.  The order of R11, RANK,
*  is the effective rank of A.
*
*  Then, R22 is considered to be negligible, and R12 is annihilated
*  by orthogonal transformations from the right, arriving at the
*  complete orthogonal factorization:
*     A * P = Q * [ T11 0 ] * Z
*                 [  0  0 ]
*  The minimum-norm solution is then
*     X = P * Z' [ inv(T11)*Q1'*B ]
*                [        0       ]
*  where Q1 consists of the first RANK columns of Q.
*
*  This routine is basically identical to the original xGELSX except
*  three differences:
*    o The call to the subroutine xGEQPF has been substituted by the
*      the call to the subroutine xGEQP3. This subroutine is a Blas-3
*      version of the QR factorization with column pivoting.
*    o Matrix B (the right hand side) is updated with Blas-3.
*    o The permutation of matrix B (the right hand side) is faster and
*      more simple.
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
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of
*          columns of matrices B and X. NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, A has been overwritten by details of its
*          complete orthogonal factorization.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the M-by-NRHS right hand side matrix B.
*          On exit, the N-by-NRHS solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,M,N).
*
*  JPVT    (input/output) INTEGER array, dimension (N)
*          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
*          to the front of AP, otherwise column i is a free column.
*          On exit, if JPVT(i) = k, then the i-th column of AP
*          was the k-th column of A.
*
*  RCOND   (input) DOUBLE PRECISION
*          RCOND is used to determine the effective rank of A, which
*          is defined as the order of the largest leading triangular
*          submatrix R11 in the QR factorization with pivoting of A,
*          whose estimated condition number < 1/RCOND.
*
*  RANK    (output) INTEGER
*          The effective rank of A, i.e., the order of the submatrix
*          R11.  This is the same as the order of the submatrix T11
*          in the complete orthogonal factorization of A.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          The unblocked strategy requires that:
*             LWORK >= MAX( MN+3*N+1, 2*MN+NRHS ),
*          where MN = min( M, N ).
*          The block algorithm requires that:
*             LWORK >= MAX( MN+2*N+NB*(N+1), 2*MN+NB*NRHS ),
*          where NB is an upper bound on the blocksize returned
*          by ILAENV for the routines DGEQP3, DTZRZF, STZRQF, DORMQR,
*          and DORMRZ.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: If INFO = -i, the i-th argument had an illegal value.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
*    E. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            IMAX, IMIN
      PARAMETER          ( IMAX = 1, IMIN = 2 )
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, IASCL, IBSCL, ISMAX, ISMIN, J, LWKMIN,
     $                   LWKOPT, MN, NB, NB1, NB2, NB3, NB4
      DOUBLE PRECISION   ANRM, BIGNUM, BNRM, C1, C2, S1, S2, SMAX,
     $                   SMAXPR, SMIN, SMINPR, SMLNUM, WSIZE
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           ILAENV, DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEQP3, DLABAD, DLAIC1, DLASCL, DLASET,
     $                   DORMQR, DORMRZ, DTRSM, DTZRZF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
*     ..
*     .. Executable Statements ..
*
      MN = MIN( M, N )
      ISMIN = MN + 1
      ISMAX = 2*MN + 1
*
*     Test the input arguments.
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, M, N ) ) THEN
         INFO = -7
      END IF
*
*     Figure out optimal block size
*
      IF( INFO.EQ.0 ) THEN
         IF( MN.EQ.0 .OR. NRHS.EQ.0 ) THEN
            LWKMIN = 1
            LWKOPT = 1
         ELSE
            NB1 = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
            NB2 = ILAENV( 1, 'DGERQF', ' ', M, N, -1, -1 )
            NB3 = ILAENV( 1, 'DORMQR', ' ', M, N, NRHS, -1 )
            NB4 = ILAENV( 1, 'DORMRQ', ' ', M, N, NRHS, -1 )
            NB = MAX( NB1, NB2, NB3, NB4 )
            LWKMIN = MN + MAX( 2*MN, N + 1, MN + NRHS )
            LWKOPT = MAX( LWKMIN,
     $                    MN + 2*N + NB*( N + 1 ), 2*MN + NB*NRHS )
         END IF
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
            INFO = -12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGELSY', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( MN.EQ.0 .OR. NRHS.EQ.0 ) THEN
         RANK = 0
         RETURN
      END IF
*
*     Get machine parameters
*
      SMLNUM = DLAMCH( 'S' ) / DLAMCH( 'P' )
      BIGNUM = ONE / SMLNUM
      CALL DLABAD( SMLNUM, BIGNUM )
*
*     Scale A, B if max entries outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', M, N, A, LDA, WORK )
      IASCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, INFO )
         IASCL = 1
      ELSE IF( ANRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, INFO )
         IASCL = 2
      ELSE IF( ANRM.EQ.ZERO ) THEN
*
*        Matrix all zero. Return zero solution.
*
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         RANK = 0
         GO TO 70
      END IF
*
      BNRM = DLANGE( 'M', M, NRHS, B, LDB, WORK )
      IBSCL = 0
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
*
*        Scale matrix norm up to SMLNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, SMLNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 1
      ELSE IF( BNRM.GT.BIGNUM ) THEN
*
*        Scale matrix norm down to BIGNUM
*
         CALL DLASCL( 'G', 0, 0, BNRM, BIGNUM, M, NRHS, B, LDB, INFO )
         IBSCL = 2
      END IF
*
*     Compute QR factorization with column pivoting of A:
*        A * P = Q * R
*
      CALL DGEQP3( M, N, A, LDA, JPVT, WORK( 1 ), WORK( MN+1 ),
     $             LWORK-MN, INFO )
      WSIZE = MN + WORK( MN+1 )
*
*     workspace: MN+2*N+NB*(N+1).
*     Details of Householder rotations stored in WORK(1:MN).
*
*     Determine RANK using incremental condition estimation
*
      WORK( ISMIN ) = ONE
      WORK( ISMAX ) = ONE
      SMAX = ABS( A( 1, 1 ) )
      SMIN = SMAX
      IF( ABS( A( 1, 1 ) ).EQ.ZERO ) THEN
         RANK = 0
         CALL DLASET( 'F', MAX( M, N ), NRHS, ZERO, ZERO, B, LDB )
         GO TO 70
      ELSE
         RANK = 1
      END IF
*
   10 CONTINUE
      IF( RANK.LT.MN ) THEN
         I = RANK + 1
         CALL DLAIC1( IMIN, RANK, WORK( ISMIN ), SMIN, A( 1, I ),
     $                A( I, I ), SMINPR, S1, C1 )
         CALL DLAIC1( IMAX, RANK, WORK( ISMAX ), SMAX, A( 1, I ),
     $                A( I, I ), SMAXPR, S2, C2 )
*
         IF( SMAXPR*RCOND.LE.SMINPR ) THEN
            DO 20 I = 1, RANK
               WORK( ISMIN+I-1 ) = S1*WORK( ISMIN+I-1 )
               WORK( ISMAX+I-1 ) = S2*WORK( ISMAX+I-1 )
   20       CONTINUE
            WORK( ISMIN+RANK ) = C1
            WORK( ISMAX+RANK ) = C2
            SMIN = SMINPR
            SMAX = SMAXPR
            RANK = RANK + 1
            GO TO 10
         END IF
      END IF
*
*     workspace: 3*MN.
*
*     Logically partition R = [ R11 R12 ]
*                             [  0  R22 ]
*     where R11 = R(1:RANK,1:RANK)
*
*     [R11,R12] = [ T11, 0 ] * Y
*
      IF( RANK.LT.N )
     $   CALL DTZRZF( RANK, N, A, LDA, WORK( MN+1 ), WORK( 2*MN+1 ),
     $                LWORK-2*MN, INFO )
*
*     workspace: 2*MN.
*     Details of Householder rotations stored in WORK(MN+1:2*MN)
*
*     B(1:M,1:NRHS) := Q' * B(1:M,1:NRHS)
*
      CALL DORMQR( 'Left', 'Transpose', M, NRHS, MN, A, LDA, WORK( 1 ),
     $             B, LDB, WORK( 2*MN+1 ), LWORK-2*MN, INFO )
      WSIZE = MAX( WSIZE, 2*MN+WORK( 2*MN+1 ) )
*
*     workspace: 2*MN+NB*NRHS.
*
*     B(1:RANK,1:NRHS) := inv(T11) * B(1:RANK,1:NRHS)
*
      CALL DTRSM( 'Left', 'Upper', 'No transpose', 'Non-unit', RANK,
     $            NRHS, ONE, A, LDA, B, LDB )
*
      DO 40 J = 1, NRHS
         DO 30 I = RANK + 1, N
            B( I, J ) = ZERO
   30    CONTINUE
   40 CONTINUE
*
*     B(1:N,1:NRHS) := Y' * B(1:N,1:NRHS)
*
      IF( RANK.LT.N ) THEN
         CALL DORMRZ( 'Left', 'Transpose', N, NRHS, RANK, N-RANK, A,
     $                LDA, WORK( MN+1 ), B, LDB, WORK( 2*MN+1 ),
     $                LWORK-2*MN, INFO )
      END IF
*
*     workspace: 2*MN+NRHS.
*
*     B(1:N,1:NRHS) := P * B(1:N,1:NRHS)
*
      DO 60 J = 1, NRHS
         DO 50 I = 1, N
            WORK( JPVT( I ) ) = B( I, J )
   50    CONTINUE
         CALL DCOPY( N, WORK( 1 ), 1, B( 1, J ), 1 )
   60 CONTINUE
*
*     workspace: N.
*
*     Undo scaling
*
      IF( IASCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'U', 0, 0, SMLNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      ELSE IF( IASCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, N, NRHS, B, LDB, INFO )
         CALL DLASCL( 'U', 0, 0, BIGNUM, ANRM, RANK, RANK, A, LDA,
     $                INFO )
      END IF
      IF( IBSCL.EQ.1 ) THEN
         CALL DLASCL( 'G', 0, 0, SMLNUM, BNRM, N, NRHS, B, LDB, INFO )
      ELSE IF( IBSCL.EQ.2 ) THEN
         CALL DLASCL( 'G', 0, 0, BIGNUM, BNRM, N, NRHS, B, LDB, INFO )
      END IF
*
   70 CONTINUE
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DGELSY
*
      END
      SUBROUTINE DGESDD( JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK,
     $                   LWORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGESDD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and right singular
*  vectors.  If singular vectors are desired, it uses a
*  divide-and-conquer algorithm.
*
*  The SVD is written
*
*       A = U * SIGMA * transpose(V)
*
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*
*  Note that the routine returns VT = V**T, not V.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          Specifies options for computing all or part of the matrix U:
*          = 'A':  all M columns of U and all N rows of V**T are
*                  returned in the arrays U and VT;
*          = 'S':  the first min(M,N) columns of U and the first
*                  min(M,N) rows of V**T are returned in the arrays U
*                  and VT;
*          = 'O':  If M >= N, the first N columns of U are overwritten
*                  on the array A and all rows of V**T are returned in
*                  the array VT;
*                  otherwise, all columns of U are returned in the
*                  array U and the first M rows of V**T are overwritten
*                  in the array A;
*          = 'N':  no columns of U or rows of V**T are computed.
*
*  M       (input) INTEGER
*          The number of rows of the input matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the input matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*          if JOBZ = 'O',  A is overwritten with the first N columns
*                          of U (the left singular vectors, stored
*                          columnwise) if M >= N;
*                          A is overwritten with the first M rows
*                          of V**T (the right singular vectors, stored
*                          rowwise) otherwise.
*          if JOBZ .ne. 'O', the contents of A are destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  S       (output) DOUBLE PRECISION array, dimension (min(M,N))
*          The singular values of A, sorted so that S(i) >= S(i+1).
*
*  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
*          UCOL = M if JOBZ = 'A' or JOBZ = 'O' and M < N;
*          UCOL = min(M,N) if JOBZ = 'S'.
*          If JOBZ = 'A' or JOBZ = 'O' and M < N, U contains the M-by-M
*          orthogonal matrix U;
*          if JOBZ = 'S', U contains the first min(M,N) columns of U
*          (the left singular vectors, stored columnwise);
*          if JOBZ = 'O' and M >= N, or JOBZ = 'N', U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= 1; if
*          JOBZ = 'S' or 'A' or JOBZ = 'O' and M < N, LDU >= M.
*
*  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
*          If JOBZ = 'A' or JOBZ = 'O' and M >= N, VT contains the
*          N-by-N orthogonal matrix V**T;
*          if JOBZ = 'S', VT contains the first min(M,N) rows of
*          V**T (the right singular vectors, stored rowwise);
*          if JOBZ = 'O' and M < N, or JOBZ = 'N', VT is not referenced.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= 1; if
*          JOBZ = 'A' or JOBZ = 'O' and M >= N, LDVT >= N;
*          if JOBZ = 'S', LDVT >= min(M,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= 1.
*          If JOBZ = 'N',
*            LWORK >= 3*min(M,N) + max(max(M,N),7*min(M,N)).
*          If JOBZ = 'O',
*            LWORK >= 3*min(M,N)*min(M,N) + 
*                     max(max(M,N),5*min(M,N)*min(M,N)+4*min(M,N)).
*          If JOBZ = 'S' or 'A'
*            LWORK >= 3*min(M,N)*min(M,N) +
*                     max(max(M,N),4*min(M,N)*min(M,N)+4*min(M,N)).
*          For good performance, LWORK should generally be larger.
*          If LWORK = -1 but other input arguments are legal, WORK(1)
*          returns the optimal LWORK.
*
*  IWORK   (workspace) INTEGER array, dimension (8*min(M,N))
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  DBDSDC did not converge, updating process failed.
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
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, WNTQA, WNTQAS, WNTQN, WNTQO, WNTQS
      INTEGER            BDSPAC, BLK, CHUNK, I, IE, IERR, IL,
     $                   IR, ISCL, ITAU, ITAUP, ITAUQ, IU, IVT, LDWKVT,
     $                   LDWRKL, LDWRKR, LDWRKU, MAXWRK, MINMN, MINWRK,
     $                   MNTHR, NWORK, WRKBL
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, SMLNUM
*     ..
*     .. Local Arrays ..
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSDC, DGEBRD, DGELQF, DGEMM, DGEQRF, DLACPY,
     $                   DLASCL, DLASET, DORGBR, DORGLQ, DORGQR, DORMBR,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           DLAMCH, DLANGE, ILAENV, LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments
*
      INFO = 0
      MINMN = MIN( M, N )
      WNTQA = LSAME( JOBZ, 'A' )
      WNTQS = LSAME( JOBZ, 'S' )
      WNTQAS = WNTQA .OR. WNTQS
      WNTQO = LSAME( JOBZ, 'O' )
      WNTQN = LSAME( JOBZ, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
*
      IF( .NOT.( WNTQA .OR. WNTQS .OR. WNTQO .OR. WNTQN ) ) THEN
         INFO = -1
      ELSE IF( M.LT.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDU.LT.1 .OR. ( WNTQAS .AND. LDU.LT.M ) .OR.
     $         ( WNTQO .AND. M.LT.N .AND. LDU.LT.M ) ) THEN
         INFO = -8
      ELSE IF( LDVT.LT.1 .OR. ( WNTQA .AND. LDVT.LT.N ) .OR.
     $         ( WNTQS .AND. LDVT.LT.MINMN ) .OR.
     $         ( WNTQO .AND. M.GE.N .AND. LDVT.LT.N ) ) THEN
         INFO = -10
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 ) THEN
         MINWRK = 1
         MAXWRK = 1
         IF( M.GE.N .AND. MINMN.GT.0 ) THEN
*
*           Compute space needed for DBDSDC
*
            MNTHR = INT( MINMN*11.0D0 / 6.0D0 )
            IF( WNTQN ) THEN
               BDSPAC = 7*N
            ELSE
               BDSPAC = 3*N*N + 4*N
            END IF
            IF( M.GE.MNTHR ) THEN
               IF( WNTQN ) THEN
*
*                 Path 1 (M much larger than N, JOBZ='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1,
     $                    -1 )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+N )
                  MINWRK = BDSPAC + N
               ELSE IF( WNTQO ) THEN
*
*                 Path 2 (M much larger than N, JOBZ='O')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*N )
                  MAXWRK = WRKBL + 2*N*N
                  MINWRK = BDSPAC + 2*N*N + 3*N
               ELSE IF( WNTQS ) THEN
*
*                 Path 3 (M much larger than N, JOBZ='S')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*N )
                  MAXWRK = WRKBL + N*N
                  MINWRK = BDSPAC + N*N + 3*N
               ELSE IF( WNTQA ) THEN
*
*                 Path 4 (M much larger than N, JOBZ='A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*N )
                  MAXWRK = WRKBL + N*N
                  MINWRK = BDSPAC + N*N + 3*N
               END IF
            ELSE
*
*              Path 5 (M at least N, but not much larger)
*
               WRKBL = 3*N + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N, -1,
     $                 -1 )
               IF( WNTQN ) THEN
                  MAXWRK = MAX( WRKBL, BDSPAC+3*N )
                  MINWRK = 3*N + MAX( M, BDSPAC )
               ELSE IF( WNTQO ) THEN
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*N )
                  MAXWRK = WRKBL + M*N
                  MINWRK = 3*N + MAX( M, N*N+BDSPAC )
               ELSE IF( WNTQS ) THEN
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+3*N )
                  MINWRK = 3*N + MAX( M, BDSPAC )
               ELSE IF( WNTQA ) THEN
                  WRKBL = MAX( WRKBL, 3*N+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, N, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC+3*N )
                  MINWRK = 3*N + MAX( M, BDSPAC )
               END IF
            END IF
         ELSE IF( MINMN.GT.0 ) THEN
*
*           Compute space needed for DBDSDC
*
            MNTHR = INT( MINMN*11.0D0 / 6.0D0 )
            IF( WNTQN ) THEN
               BDSPAC = 7*M
            ELSE
               BDSPAC = 3*M*M + 4*M
            END IF
            IF( N.GE.MNTHR ) THEN
               IF( WNTQN ) THEN
*
*                 Path 1t (N much larger than M, JOBZ='N')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1,
     $                    -1 )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+M )
                  MINWRK = BDSPAC + M
               ELSE IF( WNTQO ) THEN
*
*                 Path 2t (N much larger than M, JOBZ='O')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*M )
                  MAXWRK = WRKBL + 2*M*M
                  MINWRK = BDSPAC + 2*M*M + 3*M
               ELSE IF( WNTQS ) THEN
*
*                 Path 3t (N much larger than M, JOBZ='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*M )
                  MAXWRK = WRKBL + M*M
                  MINWRK = BDSPAC + M*M + 3*M
               ELSE IF( WNTQA ) THEN
*
*                 Path 4t (N much larger than M, JOBZ='A')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*M )
                  MAXWRK = WRKBL + M*M
                  MINWRK = BDSPAC + M*M + 3*M
               END IF
            ELSE
*
*              Path 5t (N greater than M, but not much larger)
*
               WRKBL = 3*M + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N, -1,
     $                 -1 )
               IF( WNTQN ) THEN
                  MAXWRK = MAX( WRKBL, BDSPAC+3*M )
                  MINWRK = 3*M + MAX( N, BDSPAC )
               ELSE IF( WNTQO ) THEN
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, N, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC+3*M )
                  MAXWRK = WRKBL + M*N
                  MINWRK = 3*M + MAX( N, M*M+BDSPAC )
               ELSE IF( WNTQS ) THEN
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', M, N, M, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+3*M )
                  MINWRK = 3*M + MAX( N, BDSPAC )
               ELSE IF( WNTQA ) THEN
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'QLN', M, M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORMBR', 'PRT', N, N, M, -1 ) )
                  MAXWRK = MAX( WRKBL, BDSPAC+3*M )
                  MINWRK = 3*M + MAX( N, BDSPAC )
               END IF
            END IF
         END IF
         MAXWRK = MAX( MAXWRK, MINWRK )
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGESDD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
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
      ANRM = DLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
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
            IF( WNTQN ) THEN
*
*              Path 1 (M much larger than N, JOBZ='N')
*              No singular vectors to be computed
*
               ITAU = 1
               NWORK = ITAU + N
*
*              Compute A=Q*R
*              (Workspace: need 2*N, prefer N+N*NB)
*
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Zero out below R
*
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = 1
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               NWORK = ITAUP + N
*
*              Bidiagonalize R in A
*              (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
               CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                      IERR )
               NWORK = IE + N
*
*              Perform bidiagonal SVD, computing singular values only
*              (Workspace: need N+BDSPAC)
*
               CALL DBDSDC( 'U', 'N', N, S, WORK( IE ), DUM, 1, DUM, 1,
     $                      DUM, IDUM, WORK( NWORK ), IWORK, INFO )
*
            ELSE IF( WNTQO ) THEN
*
*              Path 2 (M much larger than N, JOBZ = 'O')
*              N left singular vectors to be overwritten on A and
*              N right singular vectors to be computed in VT
*
               IR = 1
*
*              WORK(IR) is LDWRKR by N
*
               IF( LWORK.GE.LDA*N+N*N+3*N+BDSPAC ) THEN
                  LDWRKR = LDA
               ELSE
                  LDWRKR = ( LWORK-N*N-3*N-BDSPAC ) / N
               END IF
               ITAU = IR + LDWRKR*N
               NWORK = ITAU + N
*
*              Compute A=Q*R
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Copy R to WORK(IR), zeroing out below it
*
               CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ),
     $                      LDWRKR )
*
*              Generate Q in A
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               IE = ITAU
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               NWORK = ITAUP + N
*
*              Bidiagonalize R in VT, copying result to WORK(IR)
*              (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
               CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                      WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              WORK(IU) is N by N
*
               IU = NWORK
               NWORK = IU + N*N
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in WORK(IU) and computing right
*              singular vectors of bidiagonal matrix in VT
*              (Workspace: need N+N*N+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), WORK( IU ), N,
     $                      VT, LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite WORK(IU) by left singular vectors of R
*              and VT by right singular vectors of R
*              (Workspace: need 2*N*N+3*N, prefer 2*N*N+2*N+N*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', N, N, N, WORK( IR ), LDWRKR,
     $                      WORK( ITAUQ ), WORK( IU ), N, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', N, N, N, WORK( IR ), LDWRKR,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Multiply Q in A by left singular vectors of R in
*              WORK(IU), storing result in WORK(IR) and copying to A
*              (Workspace: need 2*N*N, prefer N*N+M*N)
*
               DO 10 I = 1, M, LDWRKR
                  CHUNK = MIN( M-I+1, LDWRKR )
                  CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                        LDA, WORK( IU ), N, ZERO, WORK( IR ),
     $                        LDWRKR )
                  CALL DLACPY( 'F', CHUNK, N, WORK( IR ), LDWRKR,
     $                         A( I, 1 ), LDA )
   10          CONTINUE
*
            ELSE IF( WNTQS ) THEN
*
*              Path 3 (M much larger than N, JOBZ='S')
*              N left singular vectors to be computed in U and
*              N right singular vectors to be computed in VT
*
               IR = 1
*
*              WORK(IR) is N by N
*
               LDWRKR = N
               ITAU = IR + LDWRKR*N
               NWORK = ITAU + N
*
*              Compute A=Q*R
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Copy R to WORK(IR), zeroing out below it
*
               CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ),
     $                      LDWRKR )
*
*              Generate Q in A
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               IE = ITAU
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               NWORK = ITAUP + N
*
*              Bidiagonalize R in WORK(IR)
*              (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
               CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                      WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagoal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need N+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite U by left singular vectors of R and VT
*              by right singular vectors of R
*              (Workspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', N, N, N, WORK( IR ), LDWRKR,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
               CALL DORMBR( 'P', 'R', 'T', N, N, N, WORK( IR ), LDWRKR,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Multiply Q in A by left singular vectors of R in
*              WORK(IR), storing result in U
*              (Workspace: need N*N)
*
               CALL DLACPY( 'F', N, N, U, LDU, WORK( IR ), LDWRKR )
               CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA, WORK( IR ),
     $                     LDWRKR, ZERO, U, LDU )
*
            ELSE IF( WNTQA ) THEN
*
*              Path 4 (M much larger than N, JOBZ='A')
*              M left singular vectors to be computed in U and
*              N right singular vectors to be computed in VT
*
               IU = 1
*
*              WORK(IU) is N by N
*
               LDWRKU = N
               ITAU = IU + LDWRKU*N
               NWORK = ITAU + N
*
*              Compute A=Q*R, copying result to U
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*              Generate Q in U
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
               CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*              Produce R in A, zeroing out other entries
*
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = ITAU
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               NWORK = ITAUP + N
*
*              Bidiagonalize R in A
*              (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
               CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                      IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in WORK(IU) and computing right
*              singular vectors of bidiagonal matrix in VT
*              (Workspace: need N+N*N+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), WORK( IU ), N,
     $                      VT, LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite WORK(IU) by left singular vectors of R and VT
*              by right singular vectors of R
*              (Workspace: need N*N+3*N, prefer N*N+2*N+N*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', N, N, N, A, LDA,
     $                      WORK( ITAUQ ), WORK( IU ), LDWRKU,
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', N, N, N, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Multiply Q in U by left singular vectors of R in
*              WORK(IU), storing result in A
*              (Workspace: need N*N)
*
               CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU, WORK( IU ),
     $                     LDWRKU, ZERO, A, LDA )
*
*              Copy left singular vectors of A from A to U
*
               CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
            END IF
*
         ELSE
*
*           M .LT. MNTHR
*
*           Path 5 (M at least N, but not much larger)
*           Reduce to bidiagonal form without QR decomposition
*
            IE = 1
            ITAUQ = IE + N
            ITAUP = ITAUQ + N
            NWORK = ITAUP + N
*
*           Bidiagonalize A
*           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
*
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                   IERR )
            IF( WNTQN ) THEN
*
*              Perform bidiagonal SVD, only computing singular values
*              (Workspace: need N+BDSPAC)
*
               CALL DBDSDC( 'U', 'N', N, S, WORK( IE ), DUM, 1, DUM, 1,
     $                      DUM, IDUM, WORK( NWORK ), IWORK, INFO )
            ELSE IF( WNTQO ) THEN
               IU = NWORK
               IF( LWORK.GE.M*N+3*N+BDSPAC ) THEN
*
*                 WORK( IU ) is M by N
*
                  LDWRKU = M
                  NWORK = IU + LDWRKU*N
                  CALL DLASET( 'F', M, N, ZERO, ZERO, WORK( IU ),
     $                         LDWRKU )
               ELSE
*
*                 WORK( IU ) is N by N
*
                  LDWRKU = N
                  NWORK = IU + LDWRKU*N
*
*                 WORK(IR) is LDWRKR by N
*
                  IR = NWORK
                  LDWRKR = ( LWORK-N*N-3*N ) / N
               END IF
               NWORK = IU + LDWRKU*N
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in WORK(IU) and computing right
*              singular vectors of bidiagonal matrix in VT
*              (Workspace: need N+N*N+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), WORK( IU ),
     $                      LDWRKU, VT, LDVT, DUM, IDUM, WORK( NWORK ),
     $                      IWORK, INFO )
*
*              Overwrite VT by right singular vectors of A
*              (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
               CALL DORMBR( 'P', 'R', 'T', N, N, N, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
               IF( LWORK.GE.M*N+3*N+BDSPAC ) THEN
*
*                 Overwrite WORK(IU) by left singular vectors of A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DORMBR( 'Q', 'L', 'N', M, N, N, A, LDA,
     $                         WORK( ITAUQ ), WORK( IU ), LDWRKU,
     $                         WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*                 Copy left singular vectors of A from WORK(IU) to A
*
                  CALL DLACPY( 'F', M, N, WORK( IU ), LDWRKU, A, LDA )
               ELSE
*
*                 Generate Q in A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                         WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*                 Multiply Q in A by left singular vectors of
*                 bidiagonal matrix in WORK(IU), storing result in
*                 WORK(IR) and copying to A
*                 (Workspace: need 2*N*N, prefer N*N+M*N)
*
                  DO 20 I = 1, M, LDWRKR
                     CHUNK = MIN( M-I+1, LDWRKR )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IU ), LDWRKU, ZERO,
     $                           WORK( IR ), LDWRKR )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IR ), LDWRKR,
     $                            A( I, 1 ), LDA )
   20             CONTINUE
               END IF
*
            ELSE IF( WNTQS ) THEN
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need N+BDSPAC)
*
               CALL DLASET( 'F', M, N, ZERO, ZERO, U, LDU )
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite U by left singular vectors of A and VT
*              by right singular vectors of A
*              (Workspace: need 3*N, prefer 2*N+N*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', M, N, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', N, N, N, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
            ELSE IF( WNTQA ) THEN
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need N+BDSPAC)
*
               CALL DLASET( 'F', M, M, ZERO, ZERO, U, LDU )
               CALL DBDSDC( 'U', 'I', N, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Set the right corner of U to identity matrix
*
               IF( M.GT.N ) THEN
                  CALL DLASET( 'F', M-N, M-N, ZERO, ONE, U( N+1, N+1 ),
     $                         LDU )
               END IF
*
*              Overwrite U by left singular vectors of A and VT
*              by right singular vectors of A
*              (Workspace: need N*N+2*N+M, prefer N*N+2*N+M*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', M, M, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', N, N, M, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
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
            IF( WNTQN ) THEN
*
*              Path 1t (N much larger than M, JOBZ='N')
*              No singular vectors to be computed
*
               ITAU = 1
               NWORK = ITAU + M
*
*              Compute A=L*Q
*              (Workspace: need 2*M, prefer M+M*NB)
*
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Zero out above L
*
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = 1
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               NWORK = ITAUP + M
*
*              Bidiagonalize L in A
*              (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
               CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                      IERR )
               NWORK = IE + M
*
*              Perform bidiagonal SVD, computing singular values only
*              (Workspace: need M+BDSPAC)
*
               CALL DBDSDC( 'U', 'N', M, S, WORK( IE ), DUM, 1, DUM, 1,
     $                      DUM, IDUM, WORK( NWORK ), IWORK, INFO )
*
            ELSE IF( WNTQO ) THEN
*
*              Path 2t (N much larger than M, JOBZ='O')
*              M right singular vectors to be overwritten on A and
*              M left singular vectors to be computed in U
*
               IVT = 1
*
*              IVT is M by M
*
               IL = IVT + M*M
               IF( LWORK.GE.M*N+M*M+3*M+BDSPAC ) THEN
*
*                 WORK(IL) is M by N
*
                  LDWRKL = M
                  CHUNK = N
               ELSE
                  LDWRKL = M
                  CHUNK = ( LWORK-M*M ) / M
               END IF
               ITAU = IL + LDWRKL*M
               NWORK = ITAU + M
*
*              Compute A=L*Q
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Copy L to WORK(IL), zeroing about above it
*
               CALL DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWRKL )
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                      WORK( IL+LDWRKL ), LDWRKL )
*
*              Generate Q in A
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               IE = ITAU
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               NWORK = ITAUP + M
*
*              Bidiagonalize L in WORK(IL)
*              (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
               CALL DGEBRD( M, M, WORK( IL ), LDWRKL, S, WORK( IE ),
     $                      WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U, and computing right singular
*              vectors of bidiagonal matrix in WORK(IVT)
*              (Workspace: need M+M*M+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', M, S, WORK( IE ), U, LDU,
     $                      WORK( IVT ), M, DUM, IDUM, WORK( NWORK ),
     $                      IWORK, INFO )
*
*              Overwrite U by left singular vectors of L and WORK(IVT)
*              by right singular vectors of L
*              (Workspace: need 2*M*M+3*M, prefer 2*M*M+2*M+M*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', M, M, M, WORK( IL ), LDWRKL,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', M, M, M, WORK( IL ), LDWRKL,
     $                      WORK( ITAUP ), WORK( IVT ), M,
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*              Multiply right singular vectors of L in WORK(IVT) by Q
*              in A, storing result in WORK(IL) and copying to A
*              (Workspace: need 2*M*M, prefer M*M+M*N)
*
               DO 30 I = 1, N, CHUNK
                  BLK = MIN( N-I+1, CHUNK )
                  CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IVT ), M,
     $                        A( 1, I ), LDA, ZERO, WORK( IL ), LDWRKL )
                  CALL DLACPY( 'F', M, BLK, WORK( IL ), LDWRKL,
     $                         A( 1, I ), LDA )
   30          CONTINUE
*
            ELSE IF( WNTQS ) THEN
*
*              Path 3t (N much larger than M, JOBZ='S')
*              M right singular vectors to be computed in VT and
*              M left singular vectors to be computed in U
*
               IL = 1
*
*              WORK(IL) is M by M
*
               LDWRKL = M
               ITAU = IL + LDWRKL*M
               NWORK = ITAU + M
*
*              Compute A=L*Q
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Copy L to WORK(IL), zeroing out above it
*
               CALL DLACPY( 'L', M, M, A, LDA, WORK( IL ), LDWRKL )
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                      WORK( IL+LDWRKL ), LDWRKL )
*
*              Generate Q in A
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
               IE = ITAU
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               NWORK = ITAUP + M
*
*              Bidiagonalize L in WORK(IU), copying result to U
*              (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
               CALL DGEBRD( M, M, WORK( IL ), LDWRKL, S, WORK( IE ),
     $                      WORK( ITAUQ ), WORK( ITAUP ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need M+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', M, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite U by left singular vectors of L and VT
*              by right singular vectors of L
*              (Workspace: need M*M+3*M, prefer M*M+2*M+M*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', M, M, M, WORK( IL ), LDWRKL,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', M, M, M, WORK( IL ), LDWRKL,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
*              Multiply right singular vectors of L in WORK(IL) by
*              Q in A, storing result in VT
*              (Workspace: need M*M)
*
               CALL DLACPY( 'F', M, M, VT, LDVT, WORK( IL ), LDWRKL )
               CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IL ), LDWRKL,
     $                     A, LDA, ZERO, VT, LDVT )
*
            ELSE IF( WNTQA ) THEN
*
*              Path 4t (N much larger than M, JOBZ='A')
*              N right singular vectors to be computed in VT and
*              M left singular vectors to be computed in U
*
               IVT = 1
*
*              WORK(IVT) is M by M
*
               LDWKVT = M
               ITAU = IVT + LDWKVT*M
               NWORK = ITAU + M
*
*              Compute A=L*Q, copying result to VT
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*              Generate Q in VT
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*              Produce L in A, zeroing out other entries
*
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = ITAU
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               NWORK = ITAUP + M
*
*              Bidiagonalize L in A
*              (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
               CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                      IERR )
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in WORK(IVT)
*              (Workspace: need M+M*M+BDSPAC)
*
               CALL DBDSDC( 'U', 'I', M, S, WORK( IE ), U, LDU,
     $                      WORK( IVT ), LDWKVT, DUM, IDUM,
     $                      WORK( NWORK ), IWORK, INFO )
*
*              Overwrite U by left singular vectors of L and WORK(IVT)
*              by right singular vectors of L
*              (Workspace: need M*M+3*M, prefer M*M+2*M+M*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', M, M, M, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', M, M, M, A, LDA,
     $                      WORK( ITAUP ), WORK( IVT ), LDWKVT,
     $                      WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*              Multiply right singular vectors of L in WORK(IVT) by
*              Q in VT, storing result in A
*              (Workspace: need M*M)
*
               CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IVT ), LDWKVT,
     $                     VT, LDVT, ZERO, A, LDA )
*
*              Copy right singular vectors of A from A to VT
*
               CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
            END IF
*
         ELSE
*
*           N .LT. MNTHR
*
*           Path 5t (N greater than M, but not much larger)
*           Reduce to bidiagonal form without LQ decomposition
*
            IE = 1
            ITAUQ = IE + M
            ITAUP = ITAUQ + M
            NWORK = ITAUP + M
*
*           Bidiagonalize A
*           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( NWORK ), LWORK-NWORK+1,
     $                   IERR )
            IF( WNTQN ) THEN
*
*              Perform bidiagonal SVD, only computing singular values
*              (Workspace: need M+BDSPAC)
*
               CALL DBDSDC( 'L', 'N', M, S, WORK( IE ), DUM, 1, DUM, 1,
     $                      DUM, IDUM, WORK( NWORK ), IWORK, INFO )
            ELSE IF( WNTQO ) THEN
               LDWKVT = M
               IVT = NWORK
               IF( LWORK.GE.M*N+3*M+BDSPAC ) THEN
*
*                 WORK( IVT ) is M by N
*
                  CALL DLASET( 'F', M, N, ZERO, ZERO, WORK( IVT ),
     $                         LDWKVT )
                  NWORK = IVT + LDWKVT*N
               ELSE
*
*                 WORK( IVT ) is M by M
*
                  NWORK = IVT + LDWKVT*M
                  IL = NWORK
*
*                 WORK(IL) is M by CHUNK
*
                  CHUNK = ( LWORK-M*M-3*M ) / M
               END IF
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in WORK(IVT)
*              (Workspace: need M*M+BDSPAC)
*
               CALL DBDSDC( 'L', 'I', M, S, WORK( IE ), U, LDU,
     $                      WORK( IVT ), LDWKVT, DUM, IDUM,
     $                      WORK( NWORK ), IWORK, INFO )
*
*              Overwrite U by left singular vectors of A
*              (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', M, M, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
*
               IF( LWORK.GE.M*N+3*M+BDSPAC ) THEN
*
*                 Overwrite WORK(IVT) by left singular vectors of A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DORMBR( 'P', 'R', 'T', M, N, M, A, LDA,
     $                         WORK( ITAUP ), WORK( IVT ), LDWKVT,
     $                         WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*                 Copy right singular vectors of A from WORK(IVT) to A
*
                  CALL DLACPY( 'F', M, N, WORK( IVT ), LDWKVT, A, LDA )
               ELSE
*
*                 Generate P**T in A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                         WORK( NWORK ), LWORK-NWORK+1, IERR )
*
*                 Multiply Q in A by right singular vectors of
*                 bidiagonal matrix in WORK(IVT), storing result in
*                 WORK(IL) and copying to A
*                 (Workspace: need 2*M*M, prefer M*M+M*N)
*
                  DO 40 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IVT ),
     $                           LDWKVT, A( 1, I ), LDA, ZERO,
     $                           WORK( IL ), M )
                     CALL DLACPY( 'F', M, BLK, WORK( IL ), M, A( 1, I ),
     $                            LDA )
   40             CONTINUE
               END IF
            ELSE IF( WNTQS ) THEN
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need M+BDSPAC)
*
               CALL DLASET( 'F', M, N, ZERO, ZERO, VT, LDVT )
               CALL DBDSDC( 'L', 'I', M, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Overwrite U by left singular vectors of A and VT
*              by right singular vectors of A
*              (Workspace: need 3*M, prefer 2*M+M*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', M, M, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', M, N, M, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
            ELSE IF( WNTQA ) THEN
*
*              Perform bidiagonal SVD, computing left singular vectors
*              of bidiagonal matrix in U and computing right singular
*              vectors of bidiagonal matrix in VT
*              (Workspace: need M+BDSPAC)
*
               CALL DLASET( 'F', N, N, ZERO, ZERO, VT, LDVT )
               CALL DBDSDC( 'L', 'I', M, S, WORK( IE ), U, LDU, VT,
     $                      LDVT, DUM, IDUM, WORK( NWORK ), IWORK,
     $                      INFO )
*
*              Set the right corner of VT to identity matrix
*
               IF( N.GT.M ) THEN
                  CALL DLASET( 'F', N-M, N-M, ZERO, ONE, VT( M+1, M+1 ),
     $                         LDVT )
               END IF
*
*              Overwrite U by left singular vectors of A and VT
*              by right singular vectors of A
*              (Workspace: need 2*M+N, prefer 2*M+N*NB)
*
               CALL DORMBR( 'Q', 'L', 'N', M, M, N, A, LDA,
     $                      WORK( ITAUQ ), U, LDU, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
               CALL DORMBR( 'P', 'R', 'T', N, N, M, A, LDA,
     $                      WORK( ITAUP ), VT, LDVT, WORK( NWORK ),
     $                      LWORK-NWORK+1, IERR )
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
         IF( ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
      END IF
*
*     Return optimal workspace in WORK(1)
*
      WORK( 1 ) = MAXWRK
*
      RETURN
*
*     End of DGESDD
*
      END
      SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DGESV computes the solution to a real system of linear equations
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
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
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
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
      EXTERNAL           DGETRF, DGETRS, XERBLA
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
         CALL XERBLA( 'DGESV ', -INFO )
         RETURN
      END IF
*
*     Compute the LU factorization of A.
*
      CALL DGETRF( N, N, A, LDA, IPIV, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL DGETRS( 'No transpose', N, NRHS, A, LDA, IPIV, B, LDB,
     $                INFO )
      END IF
      RETURN
*
*     End of DGESV
*
      END
      SUBROUTINE DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT,
     $                   WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBU, JOBVT
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, M, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), S( * ), U( LDU, * ),
     $                   VT( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGESVD computes the singular value decomposition (SVD) of a real
*  M-by-N matrix A, optionally computing the left and/or right singular
*  vectors. The SVD is written
*
*       A = U * SIGMA * transpose(V)
*
*  where SIGMA is an M-by-N matrix which is zero except for its
*  min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
*  V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
*  are the singular values of A; they are real and non-negative, and
*  are returned in descending order.  The first min(m,n) columns of
*  U and V are the left and right singular vectors of A.
*
*  Note that the routine returns V**T, not V.
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
*          V**T:
*          = 'A':  all N rows of V**T are returned in the array VT;
*          = 'S':  the first min(m,n) rows of V**T (the right singular
*                  vectors) are returned in the array VT;
*          = 'O':  the first min(m,n) rows of V**T (the right singular
*                  vectors) are overwritten on the array A;
*          = 'N':  no rows of V**T (no right singular vectors) are
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit,
*          if JOBU = 'O',  A is overwritten with the first min(m,n)
*                          columns of U (the left singular vectors,
*                          stored columnwise);
*          if JOBVT = 'O', A is overwritten with the first min(m,n)
*                          rows of V**T (the right singular vectors,
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
*  U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
*          (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
*          If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
*          if JOBU = 'S', U contains the first min(m,n) columns of U
*          (the left singular vectors, stored columnwise);
*          if JOBU = 'N' or 'O', U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= 1; if
*          JOBU = 'S' or 'A', LDU >= M.
*
*  VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
*          If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
*          V**T;
*          if JOBVT = 'S', VT contains the first min(m,n) rows of
*          V**T (the right singular vectors, stored rowwise);
*          if JOBVT = 'N' or 'O', VT is not referenced.
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= 1; if
*          JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
*          if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
*          superdiagonal elements of an upper bidiagonal matrix B
*          whose diagonal is in S (not necessarily sorted). B
*          satisfies A = U * B * VT, so it has the same singular values
*          as A, and singular vectors related by U and VT.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          LWORK >= MAX(1,3*MIN(M,N)+MAX(M,N),5*MIN(M,N)).
*          For good performance, LWORK should generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if DBDSQR did not converge, INFO specifies how many
*                superdiagonals of an intermediate bidiagonal form B
*                did not converge to zero. See the description of WORK
*                above for details.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, WNTUA, WNTUAS, WNTUN, WNTUO, WNTUS,
     $                   WNTVA, WNTVAS, WNTVN, WNTVO, WNTVS
      INTEGER            BDSPAC, BLK, CHUNK, I, IE, IERR, IR, ISCL,
     $                   ITAU, ITAUP, ITAUQ, IU, IWORK, LDWRKR, LDWRKU,
     $                   MAXWRK, MINMN, MINWRK, MNTHR, NCU, NCVT, NRU,
     $                   NRVT, WRKBL
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, SMLNUM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DUM( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DBDSQR, DGEBRD, DGELQF, DGEMM, DGEQRF, DLACPY,
     $                   DLASCL, DLASET, DORGBR, DORGLQ, DORGQR, DORMBR,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
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
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 ) THEN
         MINWRK = 1
         MAXWRK = 1
         IF( M.GE.N .AND. MINMN.GT.0 ) THEN
*
*           Compute space needed for DBDSQR
*
            MNTHR = ILAENV( 6, 'DGESVD', JOBU // JOBVT, M, N, 0, 0 )
            BDSPAC = 5*N
            IF( M.GE.MNTHR ) THEN
               IF( WNTUN ) THEN
*
*                 Path 1 (M much larger than N, JOBU='N')
*
                  MAXWRK = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1,
     $                     -1 )
                  MAXWRK = MAX( MAXWRK, 3*N+2*N*
     $                     ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  IF( WNTVO .OR. WNTVAS )
     $               MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $                        ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*N, BDSPAC )
               ELSE IF( WNTUO .AND. WNTVN ) THEN
*
*                 Path 2 (M much larger than N, JOBU='O', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
               ELSE IF( WNTUO .AND. WNTVAS ) THEN
*
*                 Path 3 (M much larger than N, JOBU='O', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( N*N+WRKBL, N*N+M*N+N )
                  MINWRK = MAX( 3*N+M, BDSPAC )
               ELSE IF( WNTUS .AND. WNTVN ) THEN
*
*                 Path 4 (M much larger than N, JOBU='S', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
               ELSE IF( WNTUS .AND. WNTVO ) THEN
*
*                 Path 5 (M much larger than N, JOBU='S', JOBVT='O')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
               ELSE IF( WNTUS .AND. WNTVAS ) THEN
*
*                 Path 6 (M much larger than N, JOBU='S', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+N*ILAENV( 1, 'DORGQR', ' ', M,
     $                    N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
               ELSE IF( WNTUA .AND. WNTVN ) THEN
*
*                 Path 7 (M much larger than N, JOBU='A', JOBVT='N')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
               ELSE IF( WNTUA .AND. WNTVO ) THEN
*
*                 Path 8 (M much larger than N, JOBU='A', JOBVT='O')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
               ELSE IF( WNTUA .AND. WNTVAS ) THEN
*
*                 Path 9 (M much larger than N, JOBU='A', JOBVT='S' or
*                 'A')
*
                  WRKBL = N + N*ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, N+M*ILAENV( 1, 'DORGQR', ' ', M,
     $                    M, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+2*N*
     $                    ILAENV( 1, 'DGEBRD', ' ', N, N, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+N*
     $                    ILAENV( 1, 'DORGBR', 'Q', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, 3*N+( N-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = N*N + WRKBL
                  MINWRK = MAX( 3*N+M, BDSPAC )
               END IF
            ELSE
*
*              Path 10 (M at least N, but not much larger)
*
               MAXWRK = 3*N + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $                  -1, -1 )
               IF( WNTUS .OR. WNTUO )
     $            MAXWRK = MAX( MAXWRK, 3*N+N*
     $                     ILAENV( 1, 'DORGBR', 'Q', M, N, N, -1 ) )
               IF( WNTUA )
     $            MAXWRK = MAX( MAXWRK, 3*N+M*
     $                     ILAENV( 1, 'DORGBR', 'Q', M, M, N, -1 ) )
               IF( .NOT.WNTVN )
     $            MAXWRK = MAX( MAXWRK, 3*N+( N-1 )*
     $                     ILAENV( 1, 'DORGBR', 'P', N, N, N, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*N+M, BDSPAC )
            END IF
         ELSE IF( MINMN.GT.0 ) THEN
*
*           Compute space needed for DBDSQR
*
            MNTHR = ILAENV( 6, 'DGESVD', JOBU // JOBVT, M, N, 0, 0 )
            BDSPAC = 5*M
            IF( N.GE.MNTHR ) THEN
               IF( WNTVN ) THEN
*
*                 Path 1t(N much larger than M, JOBVT='N')
*
                  MAXWRK = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1,
     $                     -1 )
                  MAXWRK = MAX( MAXWRK, 3*M+2*M*
     $                     ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  IF( WNTUO .OR. WNTUAS )
     $               MAXWRK = MAX( MAXWRK, 3*M+M*
     $                        ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  MAXWRK = MAX( MAXWRK, BDSPAC )
                  MINWRK = MAX( 4*M, BDSPAC )
               ELSE IF( WNTVO .AND. WNTUN ) THEN
*
*                 Path 2t(N much larger than M, JOBU='N', JOBVT='O')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
               ELSE IF( WNTVO .AND. WNTUAS ) THEN
*
*                 Path 3t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='O')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = MAX( M*M+WRKBL, M*M+M*N+M )
                  MINWRK = MAX( 3*M+N, BDSPAC )
               ELSE IF( WNTVS .AND. WNTUN ) THEN
*
*                 Path 4t(N much larger than M, JOBU='N', JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
               ELSE IF( WNTVS .AND. WNTUO ) THEN
*
*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
               ELSE IF( WNTVS .AND. WNTUAS ) THEN
*
*                 Path 6t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='S')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+M*ILAENV( 1, 'DORGLQ', ' ', M,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
               ELSE IF( WNTVA .AND. WNTUN ) THEN
*
*                 Path 7t(N much larger than M, JOBU='N', JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
               ELSE IF( WNTVA .AND. WNTUO ) THEN
*
*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = 2*M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
               ELSE IF( WNTVA .AND. WNTUAS ) THEN
*
*                 Path 9t(N much larger than M, JOBU='S' or 'A',
*                 JOBVT='A')
*
                  WRKBL = M + M*ILAENV( 1, 'DGELQF', ' ', M, N, -1, -1 )
                  WRKBL = MAX( WRKBL, M+N*ILAENV( 1, 'DORGLQ', ' ', N,
     $                    N, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+2*M*
     $                    ILAENV( 1, 'DGEBRD', ' ', M, M, -1, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+( M-1 )*
     $                    ILAENV( 1, 'DORGBR', 'P', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, 3*M+M*
     $                    ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
                  WRKBL = MAX( WRKBL, BDSPAC )
                  MAXWRK = M*M + WRKBL
                  MINWRK = MAX( 3*M+N, BDSPAC )
               END IF
            ELSE
*
*              Path 10t(N greater than M, but not much larger)
*
               MAXWRK = 3*M + ( M+N )*ILAENV( 1, 'DGEBRD', ' ', M, N,
     $                  -1, -1 )
               IF( WNTVS .OR. WNTVO )
     $            MAXWRK = MAX( MAXWRK, 3*M+M*
     $                     ILAENV( 1, 'DORGBR', 'P', M, N, M, -1 ) )
               IF( WNTVA )
     $            MAXWRK = MAX( MAXWRK, 3*M+N*
     $                     ILAENV( 1, 'DORGBR', 'P', N, N, M, -1 ) )
               IF( .NOT.WNTUN )
     $            MAXWRK = MAX( MAXWRK, 3*M+( M-1 )*
     $                     ILAENV( 1, 'DORGBR', 'Q', M, M, M, -1 ) )
               MAXWRK = MAX( MAXWRK, BDSPAC )
               MINWRK = MAX( 3*M+N, BDSPAC )
            END IF
         END IF
         MAXWRK = MAX( MAXWRK, MINWRK )
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -13
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGESVD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
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
      ANRM = DLANGE( 'M', M, N, A, LDA, DUM )
      ISCL = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, SMLNUM, M, N, A, LDA, IERR )
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ISCL = 1
         CALL DLASCL( 'G', 0, 0, ANRM, BIGNUM, M, N, A, LDA, IERR )
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
*              (Workspace: need 2*N, prefer N+N*NB)
*
               CALL DGEQRF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
*
*              Zero out below R
*
               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ), LDA )
               IE = 1
               ITAUQ = IE + N
               ITAUP = ITAUQ + N
               IWORK = ITAUP + N
*
*              Bidiagonalize R in A
*              (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
               CALL DGEBRD( N, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               NCVT = 0
               IF( WNTVO .OR. WNTVAS ) THEN
*
*                 If right singular vectors desired, generate P'.
*                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  NCVT = N
               END IF
               IWORK = IE + N
*
*              Perform bidiagonal QR iteration, computing right
*              singular vectors of A in A if desired
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, 0, 0, S, WORK( IE ), A, LDA,
     $                      DUM, 1, DUM, 1, WORK( IWORK ), INFO )
*
*              If right singular vectors desired in VT, copy them there
*
               IF( WNTVAS )
     $            CALL DLACPY( 'F', N, N, A, LDA, VT, LDVT )
*
            ELSE IF( WNTUO .AND. WNTVN ) THEN
*
*              Path 2 (M much larger than N, JOBU='O', JOBVT='N')
*              N left singular vectors to be overwritten on A and
*              no right singular vectors to be computed
*
               IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) THEN
*
*                    WORK(IU) is LDA by N, WORK(IR) is LDA by N
*
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) THEN
*
*                    WORK(IU) is LDA by N, WORK(IR) is N by N
*
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
*
*                    WORK(IU) is LDWRKU by N, WORK(IR) is N by N
*
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to WORK(IR) and zero out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ), LDWRKR )
                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, WORK( IR+1 ),
     $                         LDWRKR )
*
*                 Generate Q in A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing R
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                  CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of R in WORK(IR)
*                 (Workspace: need N*N+BDSPAC)
*
                  CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM, 1,
     $                         WORK( IR ), LDWRKR, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + N
*
*                 Multiply Q in A by left singular vectors of R in
*                 WORK(IR), storing result in WORK(IU) and copying to A
*                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
*
                  DO 10 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
     $                            A( I, 1 ), LDA )
   10             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  IE = 1
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize A
*                 (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
*
                  CALL DGEBRD( M, N, A, LDA, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing A
*                 (Workspace: need 4*N, prefer 3*N+N*NB)
*
                  CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM, 1,
     $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTUO .AND. WNTVAS ) THEN
*
*              Path 3 (M much larger than N, JOBU='O', JOBVT='S' or 'A')
*              N left singular vectors to be overwritten on A and
*              N right singular vectors to be computed in VT
*
               IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+LDA*N ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by N
*
                     LDWRKU = LDA
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+N )+N*N ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is N by N
*
                     LDWRKU = LDA
                     LDWRKR = N
                  ELSE
*
*                    WORK(IU) is LDWRKU by N and WORK(IR) is N by N
*
                     LDWRKU = ( LWORK-N*N-N ) / N
                     LDWRKR = N
                  END IF
                  ITAU = IR + LDWRKR*N
                  IWORK = ITAU + N
*
*                 Compute A=Q*R
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to VT, zeroing out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  IF( N.GT.1 )
     $               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            VT( 2, 1 ), LDVT )
*
*                 Generate Q in A
*                 (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in VT, copying result to WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL DLACPY( 'L', N, N, VT, LDVT, WORK( IR ), LDWRKR )
*
*                 Generate left vectors bidiagonalizing R in WORK(IR)
*                 (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                  CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                         WORK( ITAUQ ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing R in VT
*                 (Workspace: need N*N+4*N-1, prefer N*N+3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of R in WORK(IR) and computing right
*                 singular vectors of R in VT
*                 (Workspace: need N*N+BDSPAC)
*
                  CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT, LDVT,
     $                         WORK( IR ), LDWRKR, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + N
*
*                 Multiply Q in A by left singular vectors of R in
*                 WORK(IR), storing result in WORK(IU) and copying to A
*                 (Workspace: need N*N+2*N, prefer N*N+M*N+N)
*
                  DO 20 I = 1, M, LDWRKU
                     CHUNK = MIN( M-I+1, LDWRKU )
                     CALL DGEMM( 'N', 'N', CHUNK, N, N, ONE, A( I, 1 ),
     $                           LDA, WORK( IR ), LDWRKR, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', CHUNK, N, WORK( IU ), LDWRKU,
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
*                 (Workspace: need 2*N, prefer N+N*NB)
*
                  CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy R to VT, zeroing out below it
*
                  CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                  IF( N.GT.1 )
     $               CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            VT( 2, 1 ), LDVT )
*
*                 Generate Q in A
*                 (Workspace: need 2*N, prefer N+N*NB)
*
                  CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + N
                  ITAUP = ITAUQ + N
                  IWORK = ITAUP + N
*
*                 Bidiagonalize R in VT
*                 (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                  CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Multiply Q in A by left vectors bidiagonalizing R
*                 (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                  CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                         WORK( ITAUQ ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing R in VT
*                 (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                  CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + N
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in A and computing right
*                 singular vectors of A in VT
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT, LDVT,
     $                         A, LDA, DUM, 1, WORK( IWORK ), INFO )
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
                  IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
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
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IR), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IR+1 ), LDWRKR )
*
*                    Generate Q in A
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left vectors bidiagonalizing R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IR)
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
     $                            1, WORK( IR ), LDWRKR, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IR), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IR ), LDWRKR, ZERO, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left vectors bidiagonalizing R
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
     $                            1, U, LDU, DUM, 1, WORK( IWORK ),
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
                  IF( LWORK.GE.2*N*N+MAX( 4*N, BDSPAC ) ) THEN
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
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*N*N+4*N,
*                                prefer 2*N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*N*N+4*N-1,
*                                prefer 2*N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in WORK(IR)
*                    (Workspace: need 2*N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IU), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
*
*                    Copy right singular vectors of R to A
*                    (Workspace: need N*N)
*
                     CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
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
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left vectors bidiagonalizing R
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right vectors bidiagonalizing R in A
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in A
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
     $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
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
                  IF( LWORK.GE.N*N+MAX( 4*N, BDSPAC ) ) THEN
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
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DORGQR( M, N, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to VT
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need N*N+4*N-1,
*                                prefer N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in VT
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in A by left singular vectors of R in
*                    WORK(IU), storing result in U
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, A, LDA,
     $                           WORK( IU ), LDWRKU, ZERO, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DORGQR( M, N, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to VT, zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     IF( N.GT.1 )
     $                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                               VT( 2, 1 ), LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in VT
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in VT
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
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
                  IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
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
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Copy R to WORK(IR), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IR+1 ), LDWRKR )
*
*                    Generate Q in U
*                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IR)
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, N, 0, S, WORK( IE ), DUM,
     $                            1, WORK( IR ), LDWRKR, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IR), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IR ), LDWRKR, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in A
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, 0, M, 0, S, WORK( IE ), DUM,
     $                            1, U, LDU, DUM, 1, WORK( IWORK ),
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
                  IF( LWORK.GE.2*N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
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
*                    (Workspace: need 2*N*N+2*N, prefer 2*N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need 2*N*N+N+M, prefer 2*N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*N*N+4*N,
*                                prefer 2*N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*N*N+4*N, prefer 2*N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*N*N+4*N-1,
*                                prefer 2*N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in WORK(IR)
*                    (Workspace: need 2*N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, WORK( IU ),
     $                            LDWRKU, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IU), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
*                    Copy right singular vectors of R from WORK(IR) to A
*
                     CALL DLACPY( 'F', N, N, WORK( IR ), LDWRKR, A,
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
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Zero out below R in A
*
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO, A( 2, 1 ),
     $                            LDA )
*
*                    Bidiagonalize R in A
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in A
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, A, LDA,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in A
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in A
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), A,
     $                            LDA, U, LDU, DUM, 1, WORK( IWORK ),
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
                  IF( LWORK.GE.N*N+MAX( N+M, 4*N, BDSPAC ) ) THEN
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
*                    (Workspace: need N*N+2*N, prefer N*N+N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N*N+N+M, prefer N*N+N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                            WORK( IU+1 ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in WORK(IU), copying result to VT
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', N, N, WORK( IU ), LDWRKU, VT,
     $                            LDVT )
*
*                    Generate left bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need N*N+4*N, prefer N*N+3*N+N*NB)
*
                     CALL DORGBR( 'Q', N, N, N, WORK( IU ), LDWRKU,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need N*N+4*N-1,
*                                prefer N*N+3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of R in WORK(IU) and computing
*                    right singular vectors of R in VT
*                    (Workspace: need N*N+BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, N, 0, S, WORK( IE ), VT,
     $                            LDVT, WORK( IU ), LDWRKU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply Q in U by left singular vectors of R in
*                    WORK(IU), storing result in A
*                    (Workspace: need N*N)
*
                     CALL DGEMM( 'N', 'N', M, N, N, ONE, U, LDU,
     $                           WORK( IU ), LDWRKU, ZERO, A, LDA )
*
*                    Copy left singular vectors of A from A to U
*
                     CALL DLACPY( 'F', M, N, A, LDA, U, LDU )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + N
*
*                    Compute A=Q*R, copying result to U
*                    (Workspace: need 2*N, prefer N+N*NB)
*
                     CALL DGEQRF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
*
*                    Generate Q in U
*                    (Workspace: need N+M, prefer N+M*NB)
*
                     CALL DORGQR( M, M, N, U, LDU, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy R from A to VT, zeroing out below it
*
                     CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
                     IF( N.GT.1 )
     $                  CALL DLASET( 'L', N-1, N-1, ZERO, ZERO,
     $                               VT( 2, 1 ), LDVT )
                     IE = ITAU
                     ITAUQ = IE + N
                     ITAUP = ITAUQ + N
                     IWORK = ITAUP + N
*
*                    Bidiagonalize R in VT
*                    (Workspace: need 4*N, prefer 3*N+2*N*NB)
*
                     CALL DGEBRD( N, N, VT, LDVT, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply Q in U by left bidiagonalizing vectors
*                    in VT
*                    (Workspace: need 3*N+M, prefer 3*N+M*NB)
*
                     CALL DORMBR( 'Q', 'R', 'N', M, N, N, VT, LDVT,
     $                            WORK( ITAUQ ), U, LDU, WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in VT
*                    (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
                     CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + N
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', N, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
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
            ITAUQ = IE + N
            ITAUP = ITAUQ + N
            IWORK = ITAUP + N
*
*           Bidiagonalize A
*           (Workspace: need 3*N+M, prefer 3*N+(M+N)*NB)
*
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
*
*              If left singular vectors desired in U, copy result to U
*              and generate left bidiagonalizing vectors in U
*              (Workspace: need 3*N+NCU, prefer 3*N+NCU*NB)
*
               CALL DLACPY( 'L', M, N, A, LDA, U, LDU )
               IF( WNTUS )
     $            NCU = N
               IF( WNTUA )
     $            NCU = M
               CALL DORGBR( 'Q', M, NCU, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
*
*              If right singular vectors desired in VT, copy result to
*              VT and generate right bidiagonalizing vectors in VT
*              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
               CALL DLACPY( 'U', N, N, A, LDA, VT, LDVT )
               CALL DORGBR( 'P', N, N, N, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
*
*              If left singular vectors desired in A, generate left
*              bidiagonalizing vectors in A
*              (Workspace: need 4*N, prefer 3*N+N*NB)
*
               CALL DORGBR( 'Q', M, N, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
*
*              If right singular vectors desired in A, generate right
*              bidiagonalizing vectors in A
*              (Workspace: need 4*N-1, prefer 3*N+(N-1)*NB)
*
               CALL DORGBR( 'P', N, N, N, A, LDA, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IWORK = IE + N
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
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in A
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
     $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in A and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', N, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
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
*              (Workspace: need 2*M, prefer M+M*NB)
*
               CALL DGELQF( M, N, A, LDA, WORK( ITAU ), WORK( IWORK ),
     $                      LWORK-IWORK+1, IERR )
*
*              Zero out above L
*
               CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ), LDA )
               IE = 1
               ITAUQ = IE + M
               ITAUP = ITAUQ + M
               IWORK = ITAUP + M
*
*              Bidiagonalize L in A
*              (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
               CALL DGEBRD( M, M, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                      WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                      IERR )
               IF( WNTUO .OR. WNTUAS ) THEN
*
*                 If left singular vectors desired, generate Q
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
               END IF
               IWORK = IE + M
               NRU = 0
               IF( WNTUO .OR. WNTUAS )
     $            NRU = M
*
*              Perform bidiagonal QR iteration, computing left singular
*              vectors of A in A if desired
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'U', M, 0, NRU, 0, S, WORK( IE ), DUM, 1, A,
     $                      LDA, DUM, 1, WORK( IWORK ), INFO )
*
*              If left singular vectors desired in U, copy them there
*
               IF( WNTUAS )
     $            CALL DLACPY( 'F', M, M, A, LDA, U, LDU )
*
            ELSE IF( WNTVO .AND. WNTUN ) THEN
*
*              Path 2t(N much larger than M, JOBU='N', JOBVT='O')
*              M right singular vectors to be overwritten on A and
*              no left singular vectors to be computed
*
               IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
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
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to WORK(IR) and zero out above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ), LDWRKR )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                         WORK( IR+LDWRKR ), LDWRKR )
*
*                 Generate Q in A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in WORK(IR)
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing L
*                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
*
                  CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing right
*                 singular vectors of L in WORK(IR)
*                 (Workspace: need M*M+BDSPAC)
*
                  CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                         WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + M
*
*                 Multiply right singular vectors of L in WORK(IR) by Q
*                 in A, storing result in WORK(IU) and copying to A
*                 (Workspace: need M*M+2*M, prefer M*M+M*N+M)
*
                  DO 30 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
     $                            A( 1, I ), LDA )
   30             CONTINUE
*
               ELSE
*
*                 Insufficient workspace for a fast algorithm
*
                  IE = 1
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize A
*                 (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
                  CALL DGEBRD( M, N, A, LDA, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Generate right vectors bidiagonalizing A
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing right
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'L', M, N, 0, 0, S, WORK( IE ), A, LDA,
     $                         DUM, 1, DUM, 1, WORK( IWORK ), INFO )
*
               END IF
*
            ELSE IF( WNTVO .AND. WNTUAS ) THEN
*
*              Path 3t(N much larger than M, JOBU='S' or 'A', JOBVT='O')
*              M right singular vectors to be overwritten on A and
*              M left singular vectors to be computed in U
*
               IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
*
*                 Sufficient workspace for a fast algorithm
*
                  IR = 1
                  IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+LDA*M ) THEN
*
*                    WORK(IU) is LDA by N and WORK(IR) is LDA by M
*
                     LDWRKU = LDA
                     CHUNK = N
                     LDWRKR = LDA
                  ELSE IF( LWORK.GE.MAX( WRKBL, LDA*N+M )+M*M ) THEN
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
                     CHUNK = ( LWORK-M*M-M ) / M
                     LDWRKR = M
                  END IF
                  ITAU = IR + LDWRKR*M
                  IWORK = ITAU + M
*
*                 Compute A=L*Q
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to U, zeroing about above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                         LDU )
*
*                 Generate Q in A
*                 (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in U, copying result to WORK(IR)
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  CALL DLACPY( 'U', M, M, U, LDU, WORK( IR ), LDWRKR )
*
*                 Generate right vectors bidiagonalizing L in WORK(IR)
*                 (Workspace: need M*M+4*M-1, prefer M*M+3*M+(M-1)*NB)
*
                  CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                         WORK( ITAUP ), WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing L in U
*                 (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of L in U, and computing right
*                 singular vectors of L in WORK(IR)
*                 (Workspace: need M*M+BDSPAC)
*
                  CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                         WORK( IR ), LDWRKR, U, LDU, DUM, 1,
     $                         WORK( IWORK ), INFO )
                  IU = IE + M
*
*                 Multiply right singular vectors of L in WORK(IR) by Q
*                 in A, storing result in WORK(IU) and copying to A
*                 (Workspace: need M*M+2*M, prefer M*M+M*N+M))
*
                  DO 40 I = 1, N, CHUNK
                     BLK = MIN( N-I+1, CHUNK )
                     CALL DGEMM( 'N', 'N', M, BLK, M, ONE, WORK( IR ),
     $                           LDWRKR, A( 1, I ), LDA, ZERO,
     $                           WORK( IU ), LDWRKU )
                     CALL DLACPY( 'F', M, BLK, WORK( IU ), LDWRKU,
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
*                 (Workspace: need 2*M, prefer M+M*NB)
*
                  CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Copy L to U, zeroing out above it
*
                  CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                  CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                         LDU )
*
*                 Generate Q in A
*                 (Workspace: need 2*M, prefer M+M*NB)
*
                  CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IE = ITAU
                  ITAUQ = IE + M
                  ITAUP = ITAUQ + M
                  IWORK = ITAUP + M
*
*                 Bidiagonalize L in U
*                 (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                  CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                         WORK( ITAUQ ), WORK( ITAUP ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                 Multiply right vectors bidiagonalizing L by Q in A
*                 (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                  CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                         WORK( ITAUP ), A, LDA, WORK( IWORK ),
     $                         LWORK-IWORK+1, IERR )
*
*                 Generate left vectors bidiagonalizing L in U
*                 (Workspace: need 4*M, prefer 3*M+M*NB)
*
                  CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                         WORK( IWORK ), LWORK-IWORK+1, IERR )
                  IWORK = IE + M
*
*                 Perform bidiagonal QR iteration, computing left
*                 singular vectors of A in U and computing right
*                 singular vectors of A in A
*                 (Workspace: need BDSPAC)
*
                  CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), A, LDA,
     $                         U, LDU, DUM, 1, WORK( IWORK ), INFO )
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
                  IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
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
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IR), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
*
*                    Generate Q in A
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right vectors bidiagonalizing L in
*                    WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of L in WORK(IR)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IR) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
     $                           LDWRKR, A, LDA, ZERO, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy result to VT
*
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right vectors bidiagonalizing L by Q in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
     $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUO ) THEN
*
*                 Path 5t(N much larger than M, JOBU='O', JOBVT='S')
*                 M right singular vectors to be computed in VT and
*                 M left singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*M*M+MAX( 4*M, BDSPAC ) ) THEN
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
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out below it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*M*M+4*M,
*                                prefer 2*M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*M*M+4*M-1,
*                                prefer 2*M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in WORK(IR) and computing
*                    right singular vectors of L in WORK(IU)
*                    (Workspace: need 2*M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
*
*                    Copy left singular vectors of L to A
*                    (Workspace: need M*M)
*
                     CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
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
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right vectors bidiagonalizing L by Q in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors of L in A
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, compute left
*                    singular vectors of A in A and compute right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
     $                            INFO )
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
                  IF( LWORK.GE.M*M+MAX( 4*M, BDSPAC ) ) THEN
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
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
*
*                    Generate Q in A
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DORGLQ( M, N, M, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need M*M+4*M-1,
*                                prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in U and computing right
*                    singular vectors of L in WORK(IU)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in A, storing result in VT
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, A, LDA, ZERO, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DORGLQ( M, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to U, zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                            LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in U
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in U by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
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
                  IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
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
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Copy L to WORK(IR), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IR ),
     $                            LDWRKR )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IR+LDWRKR ), LDWRKR )
*
*                    Generate Q in VT
*                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IR)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IR ), LDWRKR, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate right bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need M*M+4*M-1,
*                                prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of L in WORK(IR)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, 0, 0, S, WORK( IE ),
     $                            WORK( IR ), LDWRKR, DUM, 1, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IR) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IR ),
     $                           LDWRKR, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in A by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, 0, 0, S, WORK( IE ), VT,
     $                            LDVT, DUM, 1, DUM, 1, WORK( IWORK ),
     $                            INFO )
*
                  END IF
*
               ELSE IF( WNTUO ) THEN
*
*                 Path 8t(N much larger than M, JOBU='O', JOBVT='A')
*                 N right singular vectors to be computed in VT and
*                 M left singular vectors to be overwritten on A
*
                  IF( LWORK.GE.2*M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
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
*                    (Workspace: need 2*M*M+2*M, prefer 2*M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need 2*M*M+M+N, prefer 2*M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to
*                    WORK(IR)
*                    (Workspace: need 2*M*M+4*M,
*                                prefer 2*M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU,
     $                            WORK( IR ), LDWRKR )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need 2*M*M+4*M-1,
*                                prefer 2*M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in WORK(IR)
*                    (Workspace: need 2*M*M+4*M, prefer 2*M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, WORK( IR ), LDWRKR,
     $                            WORK( ITAUQ ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in WORK(IR) and computing
*                    right singular vectors of L in WORK(IU)
*                    (Workspace: need 2*M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, WORK( IR ),
     $                            LDWRKR, DUM, 1, WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
*                    Copy left singular vectors of A from WORK(IR) to A
*
                     CALL DLACPY( 'F', M, M, WORK( IR ), LDWRKR, A,
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
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Zero out above L in A
*
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, A( 1, 2 ),
     $                            LDA )
*
*                    Bidiagonalize L in A
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, A, LDA, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in A by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, A, LDA,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in A
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, A, LDA, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in A and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, A, LDA, DUM, 1, WORK( IWORK ),
     $                            INFO )
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
                  IF( LWORK.GE.M*M+MAX( N+M, 4*M, BDSPAC ) ) THEN
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
*                    (Workspace: need M*M+2*M, prefer M*M+M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M*M+M+N, prefer M*M+M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to WORK(IU), zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, WORK( IU ),
     $                            LDWRKU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO,
     $                            WORK( IU+LDWRKU ), LDWRKU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in WORK(IU), copying result to U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, WORK( IU ), LDWRKU, S,
     $                            WORK( IE ), WORK( ITAUQ ),
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'L', M, M, WORK( IU ), LDWRKU, U,
     $                            LDU )
*
*                    Generate right bidiagonalizing vectors in WORK(IU)
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+(M-1)*NB)
*
                     CALL DORGBR( 'P', M, M, M, WORK( IU ), LDWRKU,
     $                            WORK( ITAUP ), WORK( IWORK ),
     $                            LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need M*M+4*M, prefer M*M+3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of L in U and computing right
*                    singular vectors of L in WORK(IU)
*                    (Workspace: need M*M+BDSPAC)
*
                     CALL DBDSQR( 'U', M, M, M, 0, S, WORK( IE ),
     $                            WORK( IU ), LDWRKU, U, LDU, DUM, 1,
     $                            WORK( IWORK ), INFO )
*
*                    Multiply right singular vectors of L in WORK(IU) by
*                    Q in VT, storing result in A
*                    (Workspace: need M*M)
*
                     CALL DGEMM( 'N', 'N', M, N, M, ONE, WORK( IU ),
     $                           LDWRKU, VT, LDVT, ZERO, A, LDA )
*
*                    Copy right singular vectors of A from A to VT
*
                     CALL DLACPY( 'F', M, N, A, LDA, VT, LDVT )
*
                  ELSE
*
*                    Insufficient workspace for a fast algorithm
*
                     ITAU = 1
                     IWORK = ITAU + M
*
*                    Compute A=L*Q, copying result to VT
*                    (Workspace: need 2*M, prefer M+M*NB)
*
                     CALL DGELQF( M, N, A, LDA, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
*
*                    Generate Q in VT
*                    (Workspace: need M+N, prefer M+N*NB)
*
                     CALL DORGLQ( N, N, M, VT, LDVT, WORK( ITAU ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Copy L to U, zeroing out above it
*
                     CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
                     CALL DLASET( 'U', M-1, M-1, ZERO, ZERO, U( 1, 2 ),
     $                            LDU )
                     IE = ITAU
                     ITAUQ = IE + M
                     ITAUP = ITAUQ + M
                     IWORK = ITAUP + M
*
*                    Bidiagonalize L in U
*                    (Workspace: need 4*M, prefer 3*M+2*M*NB)
*
                     CALL DGEBRD( M, M, U, LDU, S, WORK( IE ),
     $                            WORK( ITAUQ ), WORK( ITAUP ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Multiply right bidiagonalizing vectors in U by Q
*                    in VT
*                    (Workspace: need 3*M+N, prefer 3*M+N*NB)
*
                     CALL DORMBR( 'P', 'L', 'T', M, N, M, U, LDU,
     $                            WORK( ITAUP ), VT, LDVT,
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
*
*                    Generate left bidiagonalizing vectors in U
*                    (Workspace: need 4*M, prefer 3*M+M*NB)
*
                     CALL DORGBR( 'Q', M, M, M, U, LDU, WORK( ITAUQ ),
     $                            WORK( IWORK ), LWORK-IWORK+1, IERR )
                     IWORK = IE + M
*
*                    Perform bidiagonal QR iteration, computing left
*                    singular vectors of A in U and computing right
*                    singular vectors of A in VT
*                    (Workspace: need BDSPAC)
*
                     CALL DBDSQR( 'U', M, N, M, 0, S, WORK( IE ), VT,
     $                            LDVT, U, LDU, DUM, 1, WORK( IWORK ),
     $                            INFO )
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
            ITAUQ = IE + M
            ITAUP = ITAUQ + M
            IWORK = ITAUP + M
*
*           Bidiagonalize A
*           (Workspace: need 3*M+N, prefer 3*M+(M+N)*NB)
*
            CALL DGEBRD( M, N, A, LDA, S, WORK( IE ), WORK( ITAUQ ),
     $                   WORK( ITAUP ), WORK( IWORK ), LWORK-IWORK+1,
     $                   IERR )
            IF( WNTUAS ) THEN
*
*              If left singular vectors desired in U, copy result to U
*              and generate left bidiagonalizing vectors in U
*              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
*
               CALL DLACPY( 'L', M, M, A, LDA, U, LDU )
               CALL DORGBR( 'Q', M, M, N, U, LDU, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVAS ) THEN
*
*              If right singular vectors desired in VT, copy result to
*              VT and generate right bidiagonalizing vectors in VT
*              (Workspace: need 3*M+NRVT, prefer 3*M+NRVT*NB)
*
               CALL DLACPY( 'U', M, N, A, LDA, VT, LDVT )
               IF( WNTVA )
     $            NRVT = N
               IF( WNTVS )
     $            NRVT = M
               CALL DORGBR( 'P', NRVT, N, M, VT, LDVT, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTUO ) THEN
*
*              If left singular vectors desired in A, generate left
*              bidiagonalizing vectors in A
*              (Workspace: need 4*M-1, prefer 3*M+(M-1)*NB)
*
               CALL DORGBR( 'Q', M, M, N, A, LDA, WORK( ITAUQ ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IF( WNTVO ) THEN
*
*              If right singular vectors desired in A, generate right
*              bidiagonalizing vectors in A
*              (Workspace: need 4*M, prefer 3*M+M*NB)
*
               CALL DORGBR( 'P', M, N, M, A, LDA, WORK( ITAUP ),
     $                      WORK( IWORK ), LWORK-IWORK+1, IERR )
            END IF
            IWORK = IE + M
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
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE IF( ( .NOT.WNTUO ) .AND. WNTVO ) THEN
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in U and computing right singular
*              vectors in A
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), A, LDA,
     $                      U, LDU, DUM, 1, WORK( IWORK ), INFO )
            ELSE
*
*              Perform bidiagonal QR iteration, if desired, computing
*              left singular vectors in A and computing right singular
*              vectors in VT
*              (Workspace: need BDSPAC)
*
               CALL DBDSQR( 'L', M, NCVT, NRU, 0, S, WORK( IE ), VT,
     $                      LDVT, A, LDA, DUM, 1, WORK( IWORK ), INFO )
            END IF
*
         END IF
*
      END IF
*
*     If DBDSQR failed to converge, copy unconverged superdiagonals
*     to WORK( 2:MINMN )
*
      IF( INFO.NE.0 ) THEN
         IF( IE.GT.2 ) THEN
            DO 50 I = 1, MINMN - 1
               WORK( I+1 ) = WORK( I+IE-1 )
   50       CONTINUE
         END IF
         IF( IE.LT.2 ) THEN
            DO 60 I = MINMN - 1, 1, -1
               WORK( I+1 ) = WORK( I+IE-1 )
   60       CONTINUE
         END IF
      END IF
*
*     Undo scaling if necessary
*
      IF( ISCL.EQ.1 ) THEN
         IF( ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.GT.BIGNUM )
     $      CALL DLASCL( 'G', 0, 0, BIGNUM, ANRM, MINMN-1, 1, WORK( 2 ),
     $                   MINMN, IERR )
         IF( ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN, 1, S, MINMN,
     $                   IERR )
         IF( INFO.NE.0 .AND. ANRM.LT.SMLNUM )
     $      CALL DLASCL( 'G', 0, 0, SMLNUM, ANRM, MINMN-1, 1, WORK( 2 ),
     $                   MINMN, IERR )
      END IF
*
*     Return optimal workspace in WORK(1)
*
      WORK( 1 ) = MAXWRK
*
      RETURN
*
*     End of DGESVD
*
      END
      SUBROUTINE DGESVX( FACT, TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV,
     $                   EQUED, R, C, B, LDB, X, LDX, RCOND, FERR, BERR,
     $                   WORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED, FACT, TRANS
      INTEGER            INFO, LDA, LDAF, LDB, LDX, N, NRHS
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
     $                   BERR( * ), C( * ), FERR( * ), R( * ),
     $                   WORK( * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DGESVX uses the LU factorization to compute the solution to a real
*  system of linear equations
*     A * X = B,
*  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
*
*  Error bounds on the solution and a condition estimate are also
*  provided.
*
*  Description
*  ===========
*
*  The following steps are performed:
*
*  1. If FACT = 'E', real scaling factors are computed to equilibrate
*     the system:
*        TRANS = 'N':  diag(R)*A*diag(C)     *inv(diag(C))*X = diag(R)*B
*        TRANS = 'T': (diag(R)*A*diag(C))**T *inv(diag(R))*X = diag(C)*B
*        TRANS = 'C': (diag(R)*A*diag(C))**H *inv(diag(R))*X = diag(C)*B
*     Whether or not the system will be equilibrated depends on the
*     scaling of the matrix A, but if equilibration is used, A is
*     overwritten by diag(R)*A*diag(C) and B by diag(R)*B (if TRANS='N')
*     or diag(C)*B (if TRANS = 'T' or 'C').
*
*  2. If FACT = 'N' or 'E', the LU decomposition is used to factor the
*     matrix A (after equilibration if FACT = 'E') as
*        A = P * L * U,
*     where P is a permutation matrix, L is a unit lower triangular
*     matrix, and U is upper triangular.
*
*  3. If some U(i,i)=0, so that U is exactly singular, then the routine
*     returns with INFO = i. Otherwise, the factored form of A is used
*     to estimate the condition number of the matrix A.  If the
*     reciprocal of the condition number is less than machine precision,
*     INFO = N+1 is returned as a warning, but the routine still goes on
*     to solve for X and compute error bounds as described below.
*
*  4. The system of equations is solved for X using the factored form
*     of A.
*
*  5. Iterative refinement is applied to improve the computed solution
*     matrix and calculate error bounds and backward error estimates
*     for it.
*
*  6. If equilibration was used, the matrix X is premultiplied by
*     diag(C) (if TRANS = 'N') or diag(R) (if TRANS = 'T' or 'C') so
*     that it solves the original system before equilibration.
*
*  Arguments
*  =========
*
*  FACT    (input) CHARACTER*1
*          Specifies whether or not the factored form of the matrix A is
*          supplied on entry, and if not, whether the matrix A should be
*          equilibrated before it is factored.
*          = 'F':  On entry, AF and IPIV contain the factored form of A.
*                  If EQUED is not 'N', the matrix A has been
*                  equilibrated with scaling factors given by R and C.
*                  A, AF, and IPIV are not modified.
*          = 'N':  The matrix A will be copied to AF and factored.
*          = 'E':  The matrix A will be equilibrated if necessary, then
*                  copied to AF and factored.
*
*  TRANS   (input) CHARACTER*1
*          Specifies the form of the system of equations:
*          = 'N':  A * X = B     (No transpose)
*          = 'T':  A**T * X = B  (Transpose)
*          = 'C':  A**H * X = B  (Transpose)
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X.  NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the N-by-N matrix A.  If FACT = 'F' and EQUED is
*          not 'N', then A must have been equilibrated by the scaling
*          factors in R and/or C.  A is not modified if FACT = 'F' or
*          'N', or if FACT = 'E' and EQUED = 'N' on exit.
*
*          On exit, if EQUED .ne. 'N', A is scaled as follows:
*          EQUED = 'R':  A := diag(R) * A
*          EQUED = 'C':  A := A * diag(C)
*          EQUED = 'B':  A := diag(R) * A * diag(C).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  AF      (input or output) DOUBLE PRECISION array, dimension (LDAF,N)
*          If FACT = 'F', then AF is an input argument and on entry
*          contains the factors L and U from the factorization
*          A = P*L*U as computed by DGETRF.  If EQUED .ne. 'N', then
*          AF is the factored form of the equilibrated matrix A.
*
*          If FACT = 'N', then AF is an output argument and on exit
*          returns the factors L and U from the factorization A = P*L*U
*          of the original matrix A.
*
*          If FACT = 'E', then AF is an output argument and on exit
*          returns the factors L and U from the factorization A = P*L*U
*          of the equilibrated matrix A (see the description of A for
*          the form of the equilibrated matrix).
*
*  LDAF    (input) INTEGER
*          The leading dimension of the array AF.  LDAF >= max(1,N).
*
*  IPIV    (input or output) INTEGER array, dimension (N)
*          If FACT = 'F', then IPIV is an input argument and on entry
*          contains the pivot indices from the factorization A = P*L*U
*          as computed by DGETRF; row i of the matrix was interchanged
*          with row IPIV(i).
*
*          If FACT = 'N', then IPIV is an output argument and on exit
*          contains the pivot indices from the factorization A = P*L*U
*          of the original matrix A.
*
*          If FACT = 'E', then IPIV is an output argument and on exit
*          contains the pivot indices from the factorization A = P*L*U
*          of the equilibrated matrix A.
*
*  EQUED   (input or output) CHARACTER*1
*          Specifies the form of equilibration that was done.
*          = 'N':  No equilibration (always true if FACT = 'N').
*          = 'R':  Row equilibration, i.e., A has been premultiplied by
*                  diag(R).
*          = 'C':  Column equilibration, i.e., A has been postmultiplied
*                  by diag(C).
*          = 'B':  Both row and column equilibration, i.e., A has been
*                  replaced by diag(R) * A * diag(C).
*          EQUED is an input argument if FACT = 'F'; otherwise, it is an
*          output argument.
*
*  R       (input or output) DOUBLE PRECISION array, dimension (N)
*          The row scale factors for A.  If EQUED = 'R' or 'B', A is
*          multiplied on the left by diag(R); if EQUED = 'N' or 'C', R
*          is not accessed.  R is an input argument if FACT = 'F';
*          otherwise, R is an output argument.  If FACT = 'F' and
*          EQUED = 'R' or 'B', each element of R must be positive.
*
*  C       (input or output) DOUBLE PRECISION array, dimension (N)
*          The column scale factors for A.  If EQUED = 'C' or 'B', A is
*          multiplied on the right by diag(C); if EQUED = 'N' or 'R', C
*          is not accessed.  C is an input argument if FACT = 'F';
*          otherwise, C is an output argument.  If FACT = 'F' and
*          EQUED = 'C' or 'B', each element of C must be positive.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit,
*          if EQUED = 'N', B is not modified;
*          if TRANS = 'N' and EQUED = 'R' or 'B', B is overwritten by
*          diag(R)*B;
*          if TRANS = 'T' or 'C' and EQUED = 'C' or 'B', B is
*          overwritten by diag(C)*B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X
*          to the original system of equations.  Note that A and B are
*          modified on exit if EQUED .ne. 'N', and the solution to the
*          equilibrated system is inv(diag(C))*X if TRANS = 'N' and
*          EQUED = 'C' or 'B', or inv(diag(R))*X if TRANS = 'T' or 'C'
*          and EQUED = 'R' or 'B'.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  RCOND   (output) DOUBLE PRECISION
*          The estimate of the reciprocal condition number of the matrix
*          A after equilibration (if done).  If RCOND is less than the
*          machine precision (in particular, if RCOND = 0), the matrix
*          is singular to working precision.  This condition is
*          indicated by a return code of INFO > 0.
*
*  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bound for each solution vector
*          X(j) (the j-th column of the solution matrix X).
*          If XTRUE is the true solution corresponding to X(j), FERR(j)
*          is an estimated upper bound for the magnitude of the largest
*          element in (X(j) - XTRUE) divided by the magnitude of the
*          largest element in X(j).  The estimate is as reliable as
*          the estimate for RCOND, and is almost always a slight
*          overestimate of the true error.
*
*  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector X(j) (i.e., the smallest relative change in
*          any element of A or B that makes X(j) an exact solution).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (4*N)
*          On exit, WORK(1) contains the reciprocal pivot growth
*          factor norm(A)/norm(U). The "max absolute element" norm is
*          used. If WORK(1) is much less than 1, then the stability
*          of the LU factorization of the (equilibrated) matrix A
*          could be poor. This also means that the solution X, condition
*          estimator RCOND, and forward error bound FERR could be
*          unreliable. If factorization fails with 0<INFO<=N, then
*          WORK(1) contains the reciprocal pivot growth factor for the
*          leading INFO columns of A.
*
*  IWORK   (workspace) INTEGER array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, and i is
*                <= N:  U(i,i) is exactly zero.  The factorization has
*                       been completed, but the factor U is exactly
*                       singular, so the solution and error bounds
*                       could not be computed. RCOND = 0 is returned.
*                = N+1: U is nonsingular, but RCOND is less than machine
*                       precision, meaning that the matrix is singular
*                       to working precision.  Nevertheless, the
*                       solution and error bounds are computed because
*                       there are a number of situations where the
*                       computed solution can be more accurate than the
*                       value of RCOND would suggest.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            COLEQU, EQUIL, NOFACT, NOTRAN, ROWEQU
      CHARACTER          NORM
      INTEGER            I, INFEQU, J
      DOUBLE PRECISION   AMAX, ANORM, BIGNUM, COLCND, RCMAX, RCMIN,
     $                   ROWCND, RPVGRW, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANGE, DLANTR
      EXTERNAL           LSAME, DLAMCH, DLANGE, DLANTR
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGECON, DGEEQU, DGERFS, DGETRF, DGETRS, DLACPY,
     $                   DLAQGE, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      NOFACT = LSAME( FACT, 'N' )
      EQUIL = LSAME( FACT, 'E' )
      NOTRAN = LSAME( TRANS, 'N' )
      IF( NOFACT .OR. EQUIL ) THEN
         EQUED = 'N'
         ROWEQU = .FALSE.
         COLEQU = .FALSE.
      ELSE
         ROWEQU = LSAME( EQUED, 'R' ) .OR. LSAME( EQUED, 'B' )
         COLEQU = LSAME( EQUED, 'C' ) .OR. LSAME( EQUED, 'B' )
         SMLNUM = DLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
      END IF
*
*     Test the input parameters.
*
      IF( .NOT.NOFACT .AND. .NOT.EQUIL .AND. .NOT.LSAME( FACT, 'F' ) )
     $     THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) .AND. .NOT.
     $         LSAME( TRANS, 'C' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDAF.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LSAME( FACT, 'F' ) .AND. .NOT.
     $         ( ROWEQU .OR. COLEQU .OR. LSAME( EQUED, 'N' ) ) ) THEN
         INFO = -10
      ELSE
         IF( ROWEQU ) THEN
            RCMIN = BIGNUM
            RCMAX = ZERO
            DO 10 J = 1, N
               RCMIN = MIN( RCMIN, R( J ) )
               RCMAX = MAX( RCMAX, R( J ) )
   10       CONTINUE
            IF( RCMIN.LE.ZERO ) THEN
               INFO = -11
            ELSE IF( N.GT.0 ) THEN
               ROWCND = MAX( RCMIN, SMLNUM ) / MIN( RCMAX, BIGNUM )
            ELSE
               ROWCND = ONE
            END IF
         END IF
         IF( COLEQU .AND. INFO.EQ.0 ) THEN
            RCMIN = BIGNUM
            RCMAX = ZERO
            DO 20 J = 1, N
               RCMIN = MIN( RCMIN, C( J ) )
               RCMAX = MAX( RCMAX, C( J ) )
   20       CONTINUE
            IF( RCMIN.LE.ZERO ) THEN
               INFO = -12
            ELSE IF( N.GT.0 ) THEN
               COLCND = MAX( RCMIN, SMLNUM ) / MIN( RCMAX, BIGNUM )
            ELSE
               COLCND = ONE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            IF( LDB.LT.MAX( 1, N ) ) THEN
               INFO = -14
            ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
               INFO = -16
            END IF
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGESVX', -INFO )
         RETURN
      END IF
*
      IF( EQUIL ) THEN
*
*        Compute row and column scalings to equilibrate the matrix A.
*
         CALL DGEEQU( N, N, A, LDA, R, C, ROWCND, COLCND, AMAX, INFEQU )
         IF( INFEQU.EQ.0 ) THEN
*
*           Equilibrate the matrix.
*
            CALL DLAQGE( N, N, A, LDA, R, C, ROWCND, COLCND, AMAX,
     $                   EQUED )
            ROWEQU = LSAME( EQUED, 'R' ) .OR. LSAME( EQUED, 'B' )
            COLEQU = LSAME( EQUED, 'C' ) .OR. LSAME( EQUED, 'B' )
         END IF
      END IF
*
*     Scale the right hand side.
*
      IF( NOTRAN ) THEN
         IF( ROWEQU ) THEN
            DO 40 J = 1, NRHS
               DO 30 I = 1, N
                  B( I, J ) = R( I )*B( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
      ELSE IF( COLEQU ) THEN
         DO 60 J = 1, NRHS
            DO 50 I = 1, N
               B( I, J ) = C( I )*B( I, J )
   50       CONTINUE
   60    CONTINUE
      END IF
*
      IF( NOFACT .OR. EQUIL ) THEN
*
*        Compute the LU factorization of A.
*
         CALL DLACPY( 'Full', N, N, A, LDA, AF, LDAF )
         CALL DGETRF( N, N, AF, LDAF, IPIV, INFO )
*
*        Return if INFO is non-zero.
*
         IF( INFO.GT.0 ) THEN
*
*           Compute the reciprocal pivot growth factor of the
*           leading rank-deficient INFO columns of A.
*
            RPVGRW = DLANTR( 'M', 'U', 'N', INFO, INFO, AF, LDAF,
     $               WORK )
            IF( RPVGRW.EQ.ZERO ) THEN
               RPVGRW = ONE
            ELSE
               RPVGRW = DLANGE( 'M', N, INFO, A, LDA, WORK ) / RPVGRW
            END IF
            WORK( 1 ) = RPVGRW
            RCOND = ZERO
            RETURN
         END IF
      END IF
*
*     Compute the norm of the matrix A and the
*     reciprocal pivot growth factor RPVGRW.
*
      IF( NOTRAN ) THEN
         NORM = '1'
      ELSE
         NORM = 'I'
      END IF
      ANORM = DLANGE( NORM, N, N, A, LDA, WORK )
      RPVGRW = DLANTR( 'M', 'U', 'N', N, N, AF, LDAF, WORK )
      IF( RPVGRW.EQ.ZERO ) THEN
         RPVGRW = ONE
      ELSE
         RPVGRW = DLANGE( 'M', N, N, A, LDA, WORK ) / RPVGRW
      END IF
*
*     Compute the reciprocal of the condition number of A.
*
      CALL DGECON( NORM, N, AF, LDAF, ANORM, RCOND, WORK, IWORK, INFO )
*
*     Compute the solution matrix X.
*
      CALL DLACPY( 'Full', N, NRHS, B, LDB, X, LDX )
      CALL DGETRS( TRANS, N, NRHS, AF, LDAF, IPIV, X, LDX, INFO )
*
*     Use iterative refinement to improve the computed solution and
*     compute error bounds and backward error estimates for it.
*
      CALL DGERFS( TRANS, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X,
     $             LDX, FERR, BERR, WORK, IWORK, INFO )
*
*     Transform the solution matrix X to a solution of the original
*     system.
*
      IF( NOTRAN ) THEN
         IF( COLEQU ) THEN
            DO 80 J = 1, NRHS
               DO 70 I = 1, N
                  X( I, J ) = C( I )*X( I, J )
   70          CONTINUE
   80       CONTINUE
            DO 90 J = 1, NRHS
               FERR( J ) = FERR( J ) / COLCND
   90       CONTINUE
         END IF
      ELSE IF( ROWEQU ) THEN
         DO 110 J = 1, NRHS
            DO 100 I = 1, N
               X( I, J ) = R( I )*X( I, J )
  100       CONTINUE
  110    CONTINUE
         DO 120 J = 1, NRHS
            FERR( J ) = FERR( J ) / ROWCND
  120    CONTINUE
      END IF
*
      WORK( 1 ) = RPVGRW
*
*     Set INFO = N+1 if the matrix is singular to working precision.
*
      IF( RCOND.LT.DLAMCH( 'Epsilon' ) )
     $   INFO = N + 1
      RETURN
*
*     End of DGESVX
*
      END
      SUBROUTINE DGGES( JOBVSL, JOBVSR, SORT, SELCTG, N, A, LDA, B, LDB,
     $                  SDIM, ALPHAR, ALPHAI, BETA, VSL, LDVSL, VSR,
     $                  LDVSR, WORK, LWORK, BWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVSL, JOBVSR, SORT
      INTEGER            INFO, LDA, LDB, LDVSL, LDVSR, LWORK, N, SDIM
*     ..
*     .. Array Arguments ..
      LOGICAL            BWORK( * )
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), VSL( LDVSL, * ),
     $                   VSR( LDVSR, * ), WORK( * )
*     ..
*     .. Function Arguments ..
      LOGICAL            SELCTG
      EXTERNAL           SELCTG
*     ..
*
*  Purpose
*  =======
*
*  DGGES computes for a pair of N-by-N real nonsymmetric matrices (A,B),
*  the generalized eigenvalues, the generalized real Schur form (S,T),
*  optionally, the left and/or right matrices of Schur vectors (VSL and
*  VSR). This gives the generalized Schur factorization
*
*           (A,B) = ( (VSL)*S*(VSR)**T, (VSL)*T*(VSR)**T )
*
*  Optionally, it also orders the eigenvalues so that a selected cluster
*  of eigenvalues appears in the leading diagonal blocks of the upper
*  quasi-triangular matrix S and the upper triangular matrix T.The
*  leading columns of VSL and VSR then form an orthonormal basis for the
*  corresponding left and right eigenspaces (deflating subspaces).
*
*  (If only the generalized eigenvalues are needed, use the driver
*  DGGEV instead, which is faster.)
*
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
*  or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
*  usually represented as the pair (alpha,beta), as there is a
*  reasonable interpretation for beta=0 or both being zero.
*
*  A pair of matrices (S,T) is in generalized real Schur form if T is
*  upper triangular with non-negative diagonal and S is block upper
*  triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
*  to real generalized eigenvalues, while 2-by-2 blocks of S will be
*  "standardized" by making the corresponding elements of T have the
*  form:
*          [  a  0  ]
*          [  0  b  ]
*
*  and the pair of corresponding 2-by-2 blocks in S and T will have a
*  complex conjugate pair of generalized eigenvalues.
*
*
*  Arguments
*  =========
*
*  JOBVSL  (input) CHARACTER*1
*          = 'N':  do not compute the left Schur vectors;
*          = 'V':  compute the left Schur vectors.
*
*  JOBVSR  (input) CHARACTER*1
*          = 'N':  do not compute the right Schur vectors;
*          = 'V':  compute the right Schur vectors.
*
*  SORT    (input) CHARACTER*1
*          Specifies whether or not to order the eigenvalues on the
*          diagonal of the generalized Schur form.
*          = 'N':  Eigenvalues are not ordered;
*          = 'S':  Eigenvalues are ordered (see SELCTG);
*
*  SELCTG  (external procedure) LOGICAL FUNCTION of three DOUBLE PRECISION arguments
*          SELCTG must be declared EXTERNAL in the calling subroutine.
*          If SORT = 'N', SELCTG is not referenced.
*          If SORT = 'S', SELCTG is used to select eigenvalues to sort
*          to the top left of the Schur form.
*          An eigenvalue (ALPHAR(j)+ALPHAI(j))/BETA(j) is selected if
*          SELCTG(ALPHAR(j),ALPHAI(j),BETA(j)) is true; i.e. if either
*          one of a complex conjugate pair of eigenvalues is selected,
*          then both complex eigenvalues are selected.
*
*          Note that in the ill-conditioned case, a selected complex
*          eigenvalue may no longer satisfy SELCTG(ALPHAR(j),ALPHAI(j),
*          BETA(j)) = .TRUE. after ordering. INFO is to be set to N+2
*          in this case.
*
*  N       (input) INTEGER
*          The order of the matrices A, B, VSL, and VSR.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the first of the pair of matrices.
*          On exit, A has been overwritten by its generalized Schur
*          form S.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the second of the pair of matrices.
*          On exit, B has been overwritten by its generalized Schur
*          form T.
*
*  LDB     (input) INTEGER
*          The leading dimension of B.  LDB >= max(1,N).
*
*  SDIM    (output) INTEGER
*          If SORT = 'N', SDIM = 0.
*          If SORT = 'S', SDIM = number of eigenvalues (after sorting)
*          for which SELCTG is true.  (Complex conjugate pairs for which
*          SELCTG is true for either eigenvalue count as 2.)
*
*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
*  BETA    (output) DOUBLE PRECISION array, dimension (N)
*          On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
*          be the generalized eigenvalues.  ALPHAR(j) + ALPHAI(j)*i,
*          and  BETA(j),j=1,...,N are the diagonals of the complex Schur
*          form (S,T) that would result if the 2-by-2 diagonal blocks of
*          the real Schur form of (A,B) were further reduced to
*          triangular form using 2-by-2 complex unitary transformations.
*          If ALPHAI(j) is zero, then the j-th eigenvalue is real; if
*          positive, then the j-th and (j+1)-st eigenvalues are a
*          complex conjugate pair, with ALPHAI(j+1) negative.
*
*          Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
*          may easily over- or underflow, and BETA(j) may even be zero.
*          Thus, the user should avoid naively computing the ratio.
*          However, ALPHAR and ALPHAI will be always less than and
*          usually comparable with norm(A) in magnitude, and BETA always
*          less than and usually comparable with norm(B).
*
*  VSL     (output) DOUBLE PRECISION array, dimension (LDVSL,N)
*          If JOBVSL = 'V', VSL will contain the left Schur vectors.
*          Not referenced if JOBVSL = 'N'.
*
*  LDVSL   (input) INTEGER
*          The leading dimension of the matrix VSL. LDVSL >=1, and
*          if JOBVSL = 'V', LDVSL >= N.
*
*  VSR     (output) DOUBLE PRECISION array, dimension (LDVSR,N)
*          If JOBVSR = 'V', VSR will contain the right Schur vectors.
*          Not referenced if JOBVSR = 'N'.
*
*  LDVSR   (input) INTEGER
*          The leading dimension of the matrix VSR. LDVSR >= 1, and
*          if JOBVSR = 'V', LDVSR >= N.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If N = 0, LWORK >= 1, else LWORK >= 8*N+16.
*          For good performance , LWORK must generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  BWORK   (workspace) LOGICAL array, dimension (N)
*          Not referenced if SORT = 'N'.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          = 1,...,N:
*                The QZ iteration failed.  (A,B) are not in Schur
*                form, but ALPHAR(j), ALPHAI(j), and BETA(j) should
*                be correct for j=INFO+1,...,N.
*          > N:  =N+1: other than QZ iteration failed in DHGEQZ.
*                =N+2: after reordering, roundoff changed values of
*                      some complex eigenvalues so that leading
*                      eigenvalues in the Generalized Schur form no
*                      longer satisfy SELCTG=.TRUE.  This could also
*                      be caused due to scaling.
*                =N+3: reordering failed in DTGSEN.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            CURSL, ILASCL, ILBSCL, ILVSL, ILVSR, LASTSL,
     $                   LQUERY, LST2SL, WANTST
      INTEGER            I, ICOLS, IERR, IHI, IJOBVL, IJOBVR, ILEFT,
     $                   ILO, IP, IRIGHT, IROWS, ITAU, IWRK, MAXWRK,
     $                   MINWRK
      DOUBLE PRECISION   ANRM, ANRMTO, BIGNUM, BNRM, BNRMTO, EPS, PVSL,
     $                   PVSR, SAFMAX, SAFMIN, SMLNUM
*     ..
*     .. Local Arrays ..
      INTEGER            IDUM( 1 )
      DOUBLE PRECISION   DIF( 2 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DGGBAK, DGGBAL, DGGHRD, DHGEQZ, DLABAD,
     $                   DLACPY, DLASCL, DLASET, DORGQR, DORMQR, DTGSEN,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Decode the input arguments
*
      IF( LSAME( JOBVSL, 'N' ) ) THEN
         IJOBVL = 1
         ILVSL = .FALSE.
      ELSE IF( LSAME( JOBVSL, 'V' ) ) THEN
         IJOBVL = 2
         ILVSL = .TRUE.
      ELSE
         IJOBVL = -1
         ILVSL = .FALSE.
      END IF
*
      IF( LSAME( JOBVSR, 'N' ) ) THEN
         IJOBVR = 1
         ILVSR = .FALSE.
      ELSE IF( LSAME( JOBVSR, 'V' ) ) THEN
         IJOBVR = 2
         ILVSR = .TRUE.
      ELSE
         IJOBVR = -1
         ILVSR = .FALSE.
      END IF
*
      WANTST = LSAME( SORT, 'S' )
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( IJOBVL.LE.0 ) THEN
         INFO = -1
      ELSE IF( IJOBVR.LE.0 ) THEN
         INFO = -2
      ELSE IF( ( .NOT.WANTST ) .AND. ( .NOT.LSAME( SORT, 'N' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDVSL.LT.1 .OR. ( ILVSL .AND. LDVSL.LT.N ) ) THEN
         INFO = -15
      ELSE IF( LDVSR.LT.1 .OR. ( ILVSR .AND. LDVSR.LT.N ) ) THEN
         INFO = -17
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 ) THEN
         IF( N.GT.0 )THEN
            MINWRK = MAX( 8*N, 6*N + 16 )
            MAXWRK = MINWRK - N +
     $               N*ILAENV( 1, 'DGEQRF', ' ', N, 1, N, 0 )
            MAXWRK = MAX( MAXWRK, MINWRK - N +
     $                    N*ILAENV( 1, 'DORMQR', ' ', N, 1, N, -1 ) )
            IF( ILVSL ) THEN
               MAXWRK = MAX( MAXWRK, MINWRK - N +
     $                       N*ILAENV( 1, 'DORGQR', ' ', N, 1, N, -1 ) )
            END IF
         ELSE
            MINWRK = 1
            MAXWRK = 1
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY )
     $      INFO = -19
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGGES ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         SDIM = 0
         RETURN
      END IF
*
*     Get machine constants
*
      EPS = DLAMCH( 'P' )
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      CALL DLABAD( SAFMIN, SAFMAX )
      SMLNUM = SQRT( SAFMIN ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', N, N, A, LDA, WORK )
      ILASCL = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ANRMTO = SMLNUM
         ILASCL = .TRUE.
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ANRMTO = BIGNUM
         ILASCL = .TRUE.
      END IF
      IF( ILASCL )
     $   CALL DLASCL( 'G', 0, 0, ANRM, ANRMTO, N, N, A, LDA, IERR )
*
*     Scale B if max element outside range [SMLNUM,BIGNUM]
*
      BNRM = DLANGE( 'M', N, N, B, LDB, WORK )
      ILBSCL = .FALSE.
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
         BNRMTO = SMLNUM
         ILBSCL = .TRUE.
      ELSE IF( BNRM.GT.BIGNUM ) THEN
         BNRMTO = BIGNUM
         ILBSCL = .TRUE.
      END IF
      IF( ILBSCL )
     $   CALL DLASCL( 'G', 0, 0, BNRM, BNRMTO, N, N, B, LDB, IERR )
*
*     Permute the matrix to make it more nearly triangular
*     (Workspace: need 6*N + 2*N space for storing balancing factors)
*
      ILEFT = 1
      IRIGHT = N + 1
      IWRK = IRIGHT + N
      CALL DGGBAL( 'P', N, A, LDA, B, LDB, ILO, IHI, WORK( ILEFT ),
     $             WORK( IRIGHT ), WORK( IWRK ), IERR )
*
*     Reduce B to triangular form (QR decomposition of B)
*     (Workspace: need N, prefer N*NB)
*
      IROWS = IHI + 1 - ILO
      ICOLS = N + 1 - ILO
      ITAU = IWRK
      IWRK = ITAU + IROWS
      CALL DGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK( ITAU ),
     $             WORK( IWRK ), LWORK+1-IWRK, IERR )
*
*     Apply the orthogonal transformation to matrix A
*     (Workspace: need N, prefer N*NB)
*
      CALL DORMQR( 'L', 'T', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     $             WORK( ITAU ), A( ILO, ILO ), LDA, WORK( IWRK ),
     $             LWORK+1-IWRK, IERR )
*
*     Initialize VSL
*     (Workspace: need N, prefer N*NB)
*
      IF( ILVSL ) THEN
         CALL DLASET( 'Full', N, N, ZERO, ONE, VSL, LDVSL )
         IF( IROWS.GT.1 ) THEN
            CALL DLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     $                   VSL( ILO+1, ILO ), LDVSL )
         END IF
         CALL DORGQR( IROWS, IROWS, IROWS, VSL( ILO, ILO ), LDVSL,
     $                WORK( ITAU ), WORK( IWRK ), LWORK+1-IWRK, IERR )
      END IF
*
*     Initialize VSR
*
      IF( ILVSR )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, VSR, LDVSR )
*
*     Reduce to generalized Hessenberg form
*     (Workspace: none needed)
*
      CALL DGGHRD( JOBVSL, JOBVSR, N, ILO, IHI, A, LDA, B, LDB, VSL,
     $             LDVSL, VSR, LDVSR, IERR )
*
*     Perform QZ algorithm, computing Schur vectors if desired
*     (Workspace: need N)
*
      IWRK = ITAU
      CALL DHGEQZ( 'S', JOBVSL, JOBVSR, N, ILO, IHI, A, LDA, B, LDB,
     $             ALPHAR, ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR,
     $             WORK( IWRK ), LWORK+1-IWRK, IERR )
      IF( IERR.NE.0 ) THEN
         IF( IERR.GT.0 .AND. IERR.LE.N ) THEN
            INFO = IERR
         ELSE IF( IERR.GT.N .AND. IERR.LE.2*N ) THEN
            INFO = IERR - N
         ELSE
            INFO = N + 1
         END IF
         GO TO 50
      END IF
*
*     Sort eigenvalues ALPHA/BETA if desired
*     (Workspace: need 4*N+16 )
*
      SDIM = 0
      IF( WANTST ) THEN
*
*        Undo scaling on eigenvalues before SELCTGing
*
         IF( ILASCL ) THEN
            CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAR, N,
     $                   IERR )
            CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAI, N,
     $                   IERR )
         END IF
         IF( ILBSCL )
     $      CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR )
*
*        Select eigenvalues
*
         DO 10 I = 1, N
            BWORK( I ) = SELCTG( ALPHAR( I ), ALPHAI( I ), BETA( I ) )
   10    CONTINUE
*
         CALL DTGSEN( 0, ILVSL, ILVSR, BWORK, N, A, LDA, B, LDB, ALPHAR,
     $                ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR, SDIM, PVSL,
     $                PVSR, DIF, WORK( IWRK ), LWORK-IWRK+1, IDUM, 1,
     $                IERR )
         IF( IERR.EQ.1 )
     $      INFO = N + 3
*
      END IF
*
*     Apply back-permutation to VSL and VSR
*     (Workspace: none needed)
*
      IF( ILVSL )
     $   CALL DGGBAK( 'P', 'L', N, ILO, IHI, WORK( ILEFT ),
     $                WORK( IRIGHT ), N, VSL, LDVSL, IERR )
*
      IF( ILVSR )
     $   CALL DGGBAK( 'P', 'R', N, ILO, IHI, WORK( ILEFT ),
     $                WORK( IRIGHT ), N, VSR, LDVSR, IERR )
*
*     Check if unscaling would cause over/underflow, if so, rescale
*     (ALPHAR(I),ALPHAI(I),BETA(I)) so BETA(I) is on the order of
*     B(I,I) and ALPHAR(I) and ALPHAI(I) are on the order of A(I,I)
*
      IF( ILASCL ) THEN
         DO 20 I = 1, N
            IF( ALPHAI( I ).NE.ZERO ) THEN
               IF( ( ALPHAR( I ) / SAFMAX ).GT.( ANRMTO / ANRM ) .OR.
     $             ( SAFMIN / ALPHAR( I ) ).GT.( ANRM / ANRMTO ) ) THEN
                  WORK( 1 ) = ABS( A( I, I ) / ALPHAR( I ) )
                  BETA( I ) = BETA( I )*WORK( 1 )
                  ALPHAR( I ) = ALPHAR( I )*WORK( 1 )
                  ALPHAI( I ) = ALPHAI( I )*WORK( 1 )
               ELSE IF( ( ALPHAI( I ) / SAFMAX ).GT.
     $                  ( ANRMTO / ANRM ) .OR.
     $                  ( SAFMIN / ALPHAI( I ) ).GT.( ANRM / ANRMTO ) )
     $                   THEN
                  WORK( 1 ) = ABS( A( I, I+1 ) / ALPHAI( I ) )
                  BETA( I ) = BETA( I )*WORK( 1 )
                  ALPHAR( I ) = ALPHAR( I )*WORK( 1 )
                  ALPHAI( I ) = ALPHAI( I )*WORK( 1 )
               END IF
            END IF
   20    CONTINUE
      END IF
*
      IF( ILBSCL ) THEN
         DO 30 I = 1, N
            IF( ALPHAI( I ).NE.ZERO ) THEN
               IF( ( BETA( I ) / SAFMAX ).GT.( BNRMTO / BNRM ) .OR.
     $             ( SAFMIN / BETA( I ) ).GT.( BNRM / BNRMTO ) ) THEN
                  WORK( 1 ) = ABS( B( I, I ) / BETA( I ) )
                  BETA( I ) = BETA( I )*WORK( 1 )
                  ALPHAR( I ) = ALPHAR( I )*WORK( 1 )
                  ALPHAI( I ) = ALPHAI( I )*WORK( 1 )
               END IF
            END IF
   30    CONTINUE
      END IF
*
*     Undo scaling
*
      IF( ILASCL ) THEN
         CALL DLASCL( 'H', 0, 0, ANRMTO, ANRM, N, N, A, LDA, IERR )
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAR, N, IERR )
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAI, N, IERR )
      END IF
*
      IF( ILBSCL ) THEN
         CALL DLASCL( 'U', 0, 0, BNRMTO, BNRM, N, N, B, LDB, IERR )
         CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR )
      END IF
*
      IF( WANTST ) THEN
*
*        Check if reordering is correct
*
         LASTSL = .TRUE.
         LST2SL = .TRUE.
         SDIM = 0
         IP = 0
         DO 40 I = 1, N
            CURSL = SELCTG( ALPHAR( I ), ALPHAI( I ), BETA( I ) )
            IF( ALPHAI( I ).EQ.ZERO ) THEN
               IF( CURSL )
     $            SDIM = SDIM + 1
               IP = 0
               IF( CURSL .AND. .NOT.LASTSL )
     $            INFO = N + 2
            ELSE
               IF( IP.EQ.1 ) THEN
*
*                 Last eigenvalue of conjugate pair
*
                  CURSL = CURSL .OR. LASTSL
                  LASTSL = CURSL
                  IF( CURSL )
     $               SDIM = SDIM + 2
                  IP = -1
                  IF( CURSL .AND. .NOT.LST2SL )
     $               INFO = N + 2
               ELSE
*
*                 First eigenvalue of conjugate pair
*
                  IP = 1
               END IF
            END IF
            LST2SL = LASTSL
            LASTSL = CURSL
   40    CONTINUE
*
      END IF
*
   50 CONTINUE
*
      WORK( 1 ) = MAXWRK
*
      RETURN
*
*     End of DGGES
*
      END
      SUBROUTINE DGGESX( JOBVSL, JOBVSR, SORT, SELCTG, SENSE, N, A, LDA,
     $                   B, LDB, SDIM, ALPHAR, ALPHAI, BETA, VSL, LDVSL,
     $                   VSR, LDVSR, RCONDE, RCONDV, WORK, LWORK, IWORK,
     $                   LIWORK, BWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVSL, JOBVSR, SENSE, SORT
      INTEGER            INFO, LDA, LDB, LDVSL, LDVSR, LIWORK, LWORK, N,
     $                   SDIM
*     ..
*     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), RCONDE( 2 ),
     $                   RCONDV( 2 ), VSL( LDVSL, * ), VSR( LDVSR, * ),
     $                   WORK( * )
*     ..
*     .. Function Arguments ..
      LOGICAL            SELCTG
      EXTERNAL           SELCTG
*     ..
*
*  Purpose
*  =======
*
*  DGGESX computes for a pair of N-by-N real nonsymmetric matrices
*  (A,B), the generalized eigenvalues, the real Schur form (S,T), and,
*  optionally, the left and/or right matrices of Schur vectors (VSL and
*  VSR).  This gives the generalized Schur factorization
*
*       (A,B) = ( (VSL) S (VSR)**T, (VSL) T (VSR)**T )
*
*  Optionally, it also orders the eigenvalues so that a selected cluster
*  of eigenvalues appears in the leading diagonal blocks of the upper
*  quasi-triangular matrix S and the upper triangular matrix T; computes
*  a reciprocal condition number for the average of the selected
*  eigenvalues (RCONDE); and computes a reciprocal condition number for
*  the right and left deflating subspaces corresponding to the selected
*  eigenvalues (RCONDV). The leading columns of VSL and VSR then form
*  an orthonormal basis for the corresponding left and right eigenspaces
*  (deflating subspaces).
*
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar w
*  or a ratio alpha/beta = w, such that  A - w*B is singular.  It is
*  usually represented as the pair (alpha,beta), as there is a
*  reasonable interpretation for beta=0 or for both being zero.
*
*  A pair of matrices (S,T) is in generalized real Schur form if T is
*  upper triangular with non-negative diagonal and S is block upper
*  triangular with 1-by-1 and 2-by-2 blocks.  1-by-1 blocks correspond
*  to real generalized eigenvalues, while 2-by-2 blocks of S will be
*  "standardized" by making the corresponding elements of T have the
*  form:
*          [  a  0  ]
*          [  0  b  ]
*
*  and the pair of corresponding 2-by-2 blocks in S and T will have a
*  complex conjugate pair of generalized eigenvalues.
*
*
*  Arguments
*  =========
*
*  JOBVSL  (input) CHARACTER*1
*          = 'N':  do not compute the left Schur vectors;
*          = 'V':  compute the left Schur vectors.
*
*  JOBVSR  (input) CHARACTER*1
*          = 'N':  do not compute the right Schur vectors;
*          = 'V':  compute the right Schur vectors.
*
*  SORT    (input) CHARACTER*1
*          Specifies whether or not to order the eigenvalues on the
*          diagonal of the generalized Schur form.
*          = 'N':  Eigenvalues are not ordered;
*          = 'S':  Eigenvalues are ordered (see SELCTG).
*
*  SELCTG  (external procedure) LOGICAL FUNCTION of three DOUBLE PRECISION arguments
*          SELCTG must be declared EXTERNAL in the calling subroutine.
*          If SORT = 'N', SELCTG is not referenced.
*          If SORT = 'S', SELCTG is used to select eigenvalues to sort
*          to the top left of the Schur form.
*          An eigenvalue (ALPHAR(j)+ALPHAI(j))/BETA(j) is selected if
*          SELCTG(ALPHAR(j),ALPHAI(j),BETA(j)) is true; i.e. if either
*          one of a complex conjugate pair of eigenvalues is selected,
*          then both complex eigenvalues are selected.
*          Note that a selected complex eigenvalue may no longer satisfy
*          SELCTG(ALPHAR(j),ALPHAI(j),BETA(j)) = .TRUE. after ordering,
*          since ordering may change the value of complex eigenvalues
*          (especially if the eigenvalue is ill-conditioned), in this
*          case INFO is set to N+3.
*
*  SENSE   (input) CHARACTER*1
*          Determines which reciprocal condition numbers are computed.
*          = 'N' : None are computed;
*          = 'E' : Computed for average of selected eigenvalues only;
*          = 'V' : Computed for selected deflating subspaces only;
*          = 'B' : Computed for both.
*          If SENSE = 'E', 'V', or 'B', SORT must equal 'S'.
*
*  N       (input) INTEGER
*          The order of the matrices A, B, VSL, and VSR.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the first of the pair of matrices.
*          On exit, A has been overwritten by its generalized Schur
*          form S.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the second of the pair of matrices.
*          On exit, B has been overwritten by its generalized Schur
*          form T.
*
*  LDB     (input) INTEGER
*          The leading dimension of B.  LDB >= max(1,N).
*
*  SDIM    (output) INTEGER
*          If SORT = 'N', SDIM = 0.
*          If SORT = 'S', SDIM = number of eigenvalues (after sorting)
*          for which SELCTG is true.  (Complex conjugate pairs for which
*          SELCTG is true for either eigenvalue count as 2.)
*
*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
*  BETA    (output) DOUBLE PRECISION array, dimension (N)
*          On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
*          be the generalized eigenvalues.  ALPHAR(j) + ALPHAI(j)*i
*          and BETA(j),j=1,...,N  are the diagonals of the complex Schur
*          form (S,T) that would result if the 2-by-2 diagonal blocks of
*          the real Schur form of (A,B) were further reduced to
*          triangular form using 2-by-2 complex unitary transformations.
*          If ALPHAI(j) is zero, then the j-th eigenvalue is real; if
*          positive, then the j-th and (j+1)-st eigenvalues are a
*          complex conjugate pair, with ALPHAI(j+1) negative.
*
*          Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
*          may easily over- or underflow, and BETA(j) may even be zero.
*          Thus, the user should avoid naively computing the ratio.
*          However, ALPHAR and ALPHAI will be always less than and
*          usually comparable with norm(A) in magnitude, and BETA always
*          less than and usually comparable with norm(B).
*
*  VSL     (output) DOUBLE PRECISION array, dimension (LDVSL,N)
*          If JOBVSL = 'V', VSL will contain the left Schur vectors.
*          Not referenced if JOBVSL = 'N'.
*
*  LDVSL   (input) INTEGER
*          The leading dimension of the matrix VSL. LDVSL >=1, and
*          if JOBVSL = 'V', LDVSL >= N.
*
*  VSR     (output) DOUBLE PRECISION array, dimension (LDVSR,N)
*          If JOBVSR = 'V', VSR will contain the right Schur vectors.
*          Not referenced if JOBVSR = 'N'.
*
*  LDVSR   (input) INTEGER
*          The leading dimension of the matrix VSR. LDVSR >= 1, and
*          if JOBVSR = 'V', LDVSR >= N.
*
*  RCONDE  (output) DOUBLE PRECISION array, dimension ( 2 )
*          If SENSE = 'E' or 'B', RCONDE(1) and RCONDE(2) contain the
*          reciprocal condition numbers for the average of the selected
*          eigenvalues.
*          Not referenced if SENSE = 'N' or 'V'.
*
*  RCONDV  (output) DOUBLE PRECISION array, dimension ( 2 )
*          If SENSE = 'V' or 'B', RCONDV(1) and RCONDV(2) contain the
*          reciprocal condition numbers for the selected deflating
*          subspaces.
*          Not referenced if SENSE = 'N' or 'E'.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If N = 0, LWORK >= 1, else if SENSE = 'E', 'V', or 'B',
*          LWORK >= max( 8*N, 6*N+16, 2*SDIM*(N-SDIM) ), else
*          LWORK >= max( 8*N, 6*N+16 ).
*          Note that 2*SDIM*(N-SDIM) <= N*N/2.
*          Note also that an error is only returned if
*          LWORK < max( 8*N, 6*N+16), but if SENSE = 'E' or 'V' or 'B'
*          this may not be large enough.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the bound on the optimal size of the WORK
*          array and the minimum size of the IWORK array, returns these
*          values as the first entries of the WORK and IWORK arrays, and
*          no error message related to LWORK or LIWORK is issued by
*          XERBLA.
*
*  IWORK   (workspace) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the minimum LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If SENSE = 'N' or N = 0, LIWORK >= 1, otherwise
*          LIWORK >= N+6.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the bound on the optimal size of the
*          WORK array and the minimum size of the IWORK array, returns
*          these values as the first entries of the WORK and IWORK
*          arrays, and no error message related to LWORK or LIWORK is
*          issued by XERBLA.
*
*  BWORK   (workspace) LOGICAL array, dimension (N)
*          Not referenced if SORT = 'N'.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          = 1,...,N:
*                The QZ iteration failed.  (A,B) are not in Schur
*                form, but ALPHAR(j), ALPHAI(j), and BETA(j) should
*                be correct for j=INFO+1,...,N.
*          > N:  =N+1: other than QZ iteration failed in DHGEQZ
*                =N+2: after reordering, roundoff changed values of
*                      some complex eigenvalues so that leading
*                      eigenvalues in the Generalized Schur form no
*                      longer satisfy SELCTG=.TRUE.  This could also
*                      be caused due to scaling.
*                =N+3: reordering failed in DTGSEN.
*
*  Further details
*  ===============
*
*  An approximate (asymptotic) bound on the average absolute error of
*  the selected eigenvalues is
*
*       EPS * norm((A, B)) / RCONDE( 1 ).
*
*  An approximate (asymptotic) bound on the maximum angular error in
*  the computed deflating subspaces is
*
*       EPS * norm((A, B)) / RCONDV( 2 ).
*
*  See LAPACK User's Guide, section 4.11 for more information.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            CURSL, ILASCL, ILBSCL, ILVSL, ILVSR, LASTSL,
     $                   LQUERY, LST2SL, WANTSB, WANTSE, WANTSN, WANTST,
     $                   WANTSV
      INTEGER            I, ICOLS, IERR, IHI, IJOB, IJOBVL, IJOBVR,
     $                   ILEFT, ILO, IP, IRIGHT, IROWS, ITAU, IWRK,
     $                   LIWMIN, LWRK, MAXWRK, MINWRK
      DOUBLE PRECISION   ANRM, ANRMTO, BIGNUM, BNRM, BNRMTO, EPS, PL,
     $                   PR, SAFMAX, SAFMIN, SMLNUM
*     ..
*     .. Local Arrays ..
      DOUBLE PRECISION   DIF( 2 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DGGBAK, DGGBAL, DGGHRD, DHGEQZ, DLABAD,
     $                   DLACPY, DLASCL, DLASET, DORGQR, DORMQR, DTGSEN,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Decode the input arguments
*
      IF( LSAME( JOBVSL, 'N' ) ) THEN
         IJOBVL = 1
         ILVSL = .FALSE.
      ELSE IF( LSAME( JOBVSL, 'V' ) ) THEN
         IJOBVL = 2
         ILVSL = .TRUE.
      ELSE
         IJOBVL = -1
         ILVSL = .FALSE.
      END IF
*
      IF( LSAME( JOBVSR, 'N' ) ) THEN
         IJOBVR = 1
         ILVSR = .FALSE.
      ELSE IF( LSAME( JOBVSR, 'V' ) ) THEN
         IJOBVR = 2
         ILVSR = .TRUE.
      ELSE
         IJOBVR = -1
         ILVSR = .FALSE.
      END IF
*
      WANTST = LSAME( SORT, 'S' )
      WANTSN = LSAME( SENSE, 'N' )
      WANTSE = LSAME( SENSE, 'E' )
      WANTSV = LSAME( SENSE, 'V' )
      WANTSB = LSAME( SENSE, 'B' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
      IF( WANTSN ) THEN
         IJOB = 0
      ELSE IF( WANTSE ) THEN
         IJOB = 1
      ELSE IF( WANTSV ) THEN
         IJOB = 2
      ELSE IF( WANTSB ) THEN
         IJOB = 4
      END IF
*
*     Test the input arguments
*
      INFO = 0
      IF( IJOBVL.LE.0 ) THEN
         INFO = -1
      ELSE IF( IJOBVR.LE.0 ) THEN
         INFO = -2
      ELSE IF( ( .NOT.WANTST ) .AND. ( .NOT.LSAME( SORT, 'N' ) ) ) THEN
         INFO = -3
      ELSE IF( .NOT.( WANTSN .OR. WANTSE .OR. WANTSV .OR. WANTSB ) .OR.
     $         ( .NOT.WANTST .AND. .NOT.WANTSN ) ) THEN
         INFO = -5
      ELSE IF( N.LT.0 ) THEN
         INFO = -6
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -10
      ELSE IF( LDVSL.LT.1 .OR. ( ILVSL .AND. LDVSL.LT.N ) ) THEN
         INFO = -16
      ELSE IF( LDVSR.LT.1 .OR. ( ILVSR .AND. LDVSR.LT.N ) ) THEN
         INFO = -18
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV.)
*
      IF( INFO.EQ.0 ) THEN
         IF( N.GT.0) THEN
            MINWRK = MAX( 8*N, 6*N + 16 )
            MAXWRK = MINWRK - N +
     $               N*ILAENV( 1, 'DGEQRF', ' ', N, 1, N, 0 )
            MAXWRK = MAX( MAXWRK, MINWRK - N +
     $               N*ILAENV( 1, 'DORMQR', ' ', N, 1, N, -1 ) )
            IF( ILVSL ) THEN
               MAXWRK = MAX( MAXWRK, MINWRK - N +
     $                  N*ILAENV( 1, 'DORGQR', ' ', N, 1, N, -1 ) )
            END IF
            LWRK = MAXWRK
            IF( IJOB.GE.1 )
     $         LWRK = MAX( LWRK, N*N/2 )
         ELSE
            MINWRK = 1
            MAXWRK = 1
            LWRK   = 1
         END IF
         WORK( 1 ) = LWRK
         IF( WANTSN .OR. N.EQ.0 ) THEN
            LIWMIN = 1
         ELSE
            LIWMIN = N + 6
         END IF
         IWORK( 1 ) = LIWMIN
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -22
         ELSE IF( LIWORK.LT.LIWMIN  .AND. .NOT.LQUERY ) THEN
            INFO = -24
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGGESX', -INFO )
         RETURN
      ELSE IF (LQUERY) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         SDIM = 0
         RETURN
      END IF
*
*     Get machine constants
*
      EPS = DLAMCH( 'P' )
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      CALL DLABAD( SAFMIN, SAFMAX )
      SMLNUM = SQRT( SAFMIN ) / EPS
      BIGNUM = ONE / SMLNUM
*
*     Scale A if max element outside range [SMLNUM,BIGNUM]
*
      ANRM = DLANGE( 'M', N, N, A, LDA, WORK )
      ILASCL = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ANRMTO = SMLNUM
         ILASCL = .TRUE.
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ANRMTO = BIGNUM
         ILASCL = .TRUE.
      END IF
      IF( ILASCL )
     $   CALL DLASCL( 'G', 0, 0, ANRM, ANRMTO, N, N, A, LDA, IERR )
*
*     Scale B if max element outside range [SMLNUM,BIGNUM]
*
      BNRM = DLANGE( 'M', N, N, B, LDB, WORK )
      ILBSCL = .FALSE.
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
         BNRMTO = SMLNUM
         ILBSCL = .TRUE.
      ELSE IF( BNRM.GT.BIGNUM ) THEN
         BNRMTO = BIGNUM
         ILBSCL = .TRUE.
      END IF
      IF( ILBSCL )
     $   CALL DLASCL( 'G', 0, 0, BNRM, BNRMTO, N, N, B, LDB, IERR )
*
*     Permute the matrix to make it more nearly triangular
*     (Workspace: need 6*N + 2*N for permutation parameters)
*
      ILEFT = 1
      IRIGHT = N + 1
      IWRK = IRIGHT + N
      CALL DGGBAL( 'P', N, A, LDA, B, LDB, ILO, IHI, WORK( ILEFT ),
     $             WORK( IRIGHT ), WORK( IWRK ), IERR )
*
*     Reduce B to triangular form (QR decomposition of B)
*     (Workspace: need N, prefer N*NB)
*
      IROWS = IHI + 1 - ILO
      ICOLS = N + 1 - ILO
      ITAU = IWRK
      IWRK = ITAU + IROWS
      CALL DGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK( ITAU ),
     $             WORK( IWRK ), LWORK+1-IWRK, IERR )
*
*     Apply the orthogonal transformation to matrix A
*     (Workspace: need N, prefer N*NB)
*
      CALL DORMQR( 'L', 'T', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     $             WORK( ITAU ), A( ILO, ILO ), LDA, WORK( IWRK ),
     $             LWORK+1-IWRK, IERR )
*
*     Initialize VSL
*     (Workspace: need N, prefer N*NB)
*
      IF( ILVSL ) THEN
         CALL DLASET( 'Full', N, N, ZERO, ONE, VSL, LDVSL )
         IF( IROWS.GT.1 ) THEN
            CALL DLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     $                   VSL( ILO+1, ILO ), LDVSL )
         END IF
         CALL DORGQR( IROWS, IROWS, IROWS, VSL( ILO, ILO ), LDVSL,
     $                WORK( ITAU ), WORK( IWRK ), LWORK+1-IWRK, IERR )
      END IF
*
*     Initialize VSR
*
      IF( ILVSR )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, VSR, LDVSR )
*
*     Reduce to generalized Hessenberg form
*     (Workspace: none needed)
*
      CALL DGGHRD( JOBVSL, JOBVSR, N, ILO, IHI, A, LDA, B, LDB, VSL,
     $             LDVSL, VSR, LDVSR, IERR )
*
      SDIM = 0
*
*     Perform QZ algorithm, computing Schur vectors if desired
*     (Workspace: need N)
*
      IWRK = ITAU
      CALL DHGEQZ( 'S', JOBVSL, JOBVSR, N, ILO, IHI, A, LDA, B, LDB,
     $             ALPHAR, ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR,
     $             WORK( IWRK ), LWORK+1-IWRK, IERR )
      IF( IERR.NE.0 ) THEN
         IF( IERR.GT.0 .AND. IERR.LE.N ) THEN
            INFO = IERR
         ELSE IF( IERR.GT.N .AND. IERR.LE.2*N ) THEN
            INFO = IERR - N
         ELSE
            INFO = N + 1
         END IF
         GO TO 60
      END IF
*
*     Sort eigenvalues ALPHA/BETA and compute the reciprocal of
*     condition number(s)
*     (Workspace: If IJOB >= 1, need MAX( 8*(N+1), 2*SDIM*(N-SDIM) )
*                 otherwise, need 8*(N+1) )
*
      IF( WANTST ) THEN
*
*        Undo scaling on eigenvalues before SELCTGing
*
         IF( ILASCL ) THEN
            CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAR, N,
     $                   IERR )
            CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAI, N,
     $                   IERR )
         END IF
         IF( ILBSCL )
     $      CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR )
*
*        Select eigenvalues
*
         DO 10 I = 1, N
            BWORK( I ) = SELCTG( ALPHAR( I ), ALPHAI( I ), BETA( I ) )
   10    CONTINUE
*
*        Reorder eigenvalues, transform Generalized Schur vectors, and
*        compute reciprocal condition numbers
*
         CALL DTGSEN( IJOB, ILVSL, ILVSR, BWORK, N, A, LDA, B, LDB,
     $                ALPHAR, ALPHAI, BETA, VSL, LDVSL, VSR, LDVSR,
     $                SDIM, PL, PR, DIF, WORK( IWRK ), LWORK-IWRK+1,
     $                IWORK, LIWORK, IERR )
*
         IF( IJOB.GE.1 )
     $      MAXWRK = MAX( MAXWRK, 2*SDIM*( N-SDIM ) )
         IF( IERR.EQ.-22 ) THEN
*
*            not enough real workspace
*
            INFO = -22
         ELSE
            IF( IJOB.EQ.1 .OR. IJOB.EQ.4 ) THEN
               RCONDE( 1 ) = PL
               RCONDE( 2 ) = PR
            END IF
            IF( IJOB.EQ.2 .OR. IJOB.EQ.4 ) THEN
               RCONDV( 1 ) = DIF( 1 )
               RCONDV( 2 ) = DIF( 2 )
            END IF
            IF( IERR.EQ.1 )
     $         INFO = N + 3
         END IF
*
      END IF
*
*     Apply permutation to VSL and VSR
*     (Workspace: none needed)
*
      IF( ILVSL )
     $   CALL DGGBAK( 'P', 'L', N, ILO, IHI, WORK( ILEFT ),
     $                WORK( IRIGHT ), N, VSL, LDVSL, IERR )
*
      IF( ILVSR )
     $   CALL DGGBAK( 'P', 'R', N, ILO, IHI, WORK( ILEFT ),
     $                WORK( IRIGHT ), N, VSR, LDVSR, IERR )
*
*     Check if unscaling would cause over/underflow, if so, rescale
*     (ALPHAR(I),ALPHAI(I),BETA(I)) so BETA(I) is on the order of
*     B(I,I) and ALPHAR(I) and ALPHAI(I) are on the order of A(I,I)
*
      IF( ILASCL ) THEN
         DO 20 I = 1, N
            IF( ALPHAI( I ).NE.ZERO ) THEN
               IF( ( ALPHAR( I ) / SAFMAX ).GT.( ANRMTO / ANRM ) .OR.
     $             ( SAFMIN / ALPHAR( I ) ).GT.( ANRM / ANRMTO ) ) THEN
                  WORK( 1 ) = ABS( A( I, I ) / ALPHAR( I ) )
                  BETA( I ) = BETA( I )*WORK( 1 )
                  ALPHAR( I ) = ALPHAR( I )*WORK( 1 )
                  ALPHAI( I ) = ALPHAI( I )*WORK( 1 )
               ELSE IF( ( ALPHAI( I ) / SAFMAX ).GT.
     $                  ( ANRMTO / ANRM ) .OR.
     $                  ( SAFMIN / ALPHAI( I ) ).GT.( ANRM / ANRMTO ) )
     $                   THEN
                  WORK( 1 ) = ABS( A( I, I+1 ) / ALPHAI( I ) )
                  BETA( I ) = BETA( I )*WORK( 1 )
                  ALPHAR( I ) = ALPHAR( I )*WORK( 1 )
                  ALPHAI( I ) = ALPHAI( I )*WORK( 1 )
               END IF
            END IF
   20    CONTINUE
      END IF
*
      IF( ILBSCL ) THEN
         DO 30 I = 1, N
            IF( ALPHAI( I ).NE.ZERO ) THEN
               IF( ( BETA( I ) / SAFMAX ).GT.( BNRMTO / BNRM ) .OR.
     $             ( SAFMIN / BETA( I ) ).GT.( BNRM / BNRMTO ) ) THEN
                  WORK( 1 ) = ABS( B( I, I ) / BETA( I ) )
                  BETA( I ) = BETA( I )*WORK( 1 )
                  ALPHAR( I ) = ALPHAR( I )*WORK( 1 )
                  ALPHAI( I ) = ALPHAI( I )*WORK( 1 )
               END IF
            END IF
   30    CONTINUE
      END IF
*
*     Undo scaling
*
      IF( ILASCL ) THEN
         CALL DLASCL( 'H', 0, 0, ANRMTO, ANRM, N, N, A, LDA, IERR )
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAR, N, IERR )
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAI, N, IERR )
      END IF
*
      IF( ILBSCL ) THEN
         CALL DLASCL( 'U', 0, 0, BNRMTO, BNRM, N, N, B, LDB, IERR )
         CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR )
      END IF
*
      IF( WANTST ) THEN
*
*        Check if reordering is correct
*
         LASTSL = .TRUE.
         LST2SL = .TRUE.
         SDIM = 0
         IP = 0
         DO 50 I = 1, N
            CURSL = SELCTG( ALPHAR( I ), ALPHAI( I ), BETA( I ) )
            IF( ALPHAI( I ).EQ.ZERO ) THEN
               IF( CURSL )
     $            SDIM = SDIM + 1
               IP = 0
               IF( CURSL .AND. .NOT.LASTSL )
     $            INFO = N + 2
            ELSE
               IF( IP.EQ.1 ) THEN
*
*                 Last eigenvalue of conjugate pair
*
                  CURSL = CURSL .OR. LASTSL
                  LASTSL = CURSL
                  IF( CURSL )
     $               SDIM = SDIM + 2
                  IP = -1
                  IF( CURSL .AND. .NOT.LST2SL )
     $               INFO = N + 2
               ELSE
*
*                 First eigenvalue of conjugate pair
*
                  IP = 1
               END IF
            END IF
            LST2SL = LASTSL
            LASTSL = CURSL
   50    CONTINUE
*
      END IF
*
   60 CONTINUE
*
      WORK( 1 ) = MAXWRK
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of DGGESX
*
      END
      SUBROUTINE DGGEV( JOBVL, JOBVR, N, A, LDA, B, LDB, ALPHAR, ALPHAI,
     $                  BETA, VL, LDVL, VR, LDVR, WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBVL, JOBVR
      INTEGER            INFO, LDA, LDB, LDVL, LDVR, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), VL( LDVL, * ),
     $                   VR( LDVR, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGGEV computes for a pair of N-by-N real nonsymmetric matrices (A,B)
*  the generalized eigenvalues, and optionally, the left and/or right
*  generalized eigenvectors.
*
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*
*                   A * v(j) = lambda(j) * B * v(j).
*
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*
*                   u(j)**H * A  = lambda(j) * u(j)**H * B .
*
*  where u(j)**H is the conjugate-transpose of u(j).
*
*
*  Arguments
*  =========
*
*  JOBVL   (input) CHARACTER*1
*          = 'N':  do not compute the left generalized eigenvectors;
*          = 'V':  compute the left generalized eigenvectors.
*
*  JOBVR   (input) CHARACTER*1
*          = 'N':  do not compute the right generalized eigenvectors;
*          = 'V':  compute the right generalized eigenvectors.
*
*  N       (input) INTEGER
*          The order of the matrices A, B, VL, and VR.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the matrix A in the pair (A,B).
*          On exit, A has been overwritten.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the matrix B in the pair (A,B).
*          On exit, B has been overwritten.
*
*  LDB     (input) INTEGER
*          The leading dimension of B.  LDB >= max(1,N).
*
*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
*  BETA    (output) DOUBLE PRECISION array, dimension (N)
*          On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
*          be the generalized eigenvalues.  If ALPHAI(j) is zero, then
*          the j-th eigenvalue is real; if positive, then the j-th and
*          (j+1)-st eigenvalues are a complex conjugate pair, with
*          ALPHAI(j+1) negative.
*
*          Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
*          may easily over- or underflow, and BETA(j) may even be zero.
*          Thus, the user should avoid naively computing the ratio
*          alpha/beta.  However, ALPHAR and ALPHAI will be always less
*          than and usually comparable with norm(A) in magnitude, and
*          BETA always less than and usually comparable with norm(B).
*
*  VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
*          If JOBVL = 'V', the left eigenvectors u(j) are stored one
*          after another in the columns of VL, in the same order as
*          their eigenvalues. If the j-th eigenvalue is real, then
*          u(j) = VL(:,j), the j-th column of VL. If the j-th and
*          (j+1)-th eigenvalues form a complex conjugate pair, then
*          u(j) = VL(:,j)+i*VL(:,j+1) and u(j+1) = VL(:,j)-i*VL(:,j+1).
*          Each eigenvector is scaled so the largest component has
*          abs(real part)+abs(imag. part)=1.
*          Not referenced if JOBVL = 'N'.
*
*  LDVL    (input) INTEGER
*          The leading dimension of the matrix VL. LDVL >= 1, and
*          if JOBVL = 'V', LDVL >= N.
*
*  VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
*          If JOBVR = 'V', the right eigenvectors v(j) are stored one
*          after another in the columns of VR, in the same order as
*          their eigenvalues. If the j-th eigenvalue is real, then
*          v(j) = VR(:,j), the j-th column of VR. If the j-th and
*          (j+1)-th eigenvalues form a complex conjugate pair, then
*          v(j) = VR(:,j)+i*VR(:,j+1) and v(j+1) = VR(:,j)-i*VR(:,j+1).
*          Each eigenvector is scaled so the largest component has
*          abs(real part)+abs(imag. part)=1.
*          Not referenced if JOBVR = 'N'.
*
*  LDVR    (input) INTEGER
*          The leading dimension of the matrix VR. LDVR >= 1, and
*          if JOBVR = 'V', LDVR >= N.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,8*N).
*          For good performance, LWORK must generally be larger.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          = 1,...,N:
*                The QZ iteration failed.  No eigenvectors have been
*                calculated, but ALPHAR(j), ALPHAI(j), and BETA(j)
*                should be correct for j=INFO+1,...,N.
*          > N:  =N+1: other than QZ iteration failed in DHGEQZ.
*                =N+2: error return from DTGEVC.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ILASCL, ILBSCL, ILV, ILVL, ILVR, LQUERY
      CHARACTER          CHTEMP
      INTEGER            ICOLS, IERR, IHI, IJOBVL, IJOBVR, ILEFT, ILO,
     $                   IN, IRIGHT, IROWS, ITAU, IWRK, JC, JR, MAXWRK,
     $                   MINWRK
      DOUBLE PRECISION   ANRM, ANRMTO, BIGNUM, BNRM, BNRMTO, EPS,
     $                   SMLNUM, TEMP
*     ..
*     .. Local Arrays ..
      LOGICAL            LDUMMA( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DGGBAK, DGGBAL, DGGHRD, DHGEQZ, DLABAD,
     $                   DLACPY,DLASCL, DLASET, DORGQR, DORMQR, DTGEVC,
     $                   XERBLA
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Decode the input arguments
*
      IF( LSAME( JOBVL, 'N' ) ) THEN
         IJOBVL = 1
         ILVL = .FALSE.
      ELSE IF( LSAME( JOBVL, 'V' ) ) THEN
         IJOBVL = 2
         ILVL = .TRUE.
      ELSE
         IJOBVL = -1
         ILVL = .FALSE.
      END IF
*
      IF( LSAME( JOBVR, 'N' ) ) THEN
         IJOBVR = 1
         ILVR = .FALSE.
      ELSE IF( LSAME( JOBVR, 'V' ) ) THEN
         IJOBVR = 2
         ILVR = .TRUE.
      ELSE
         IJOBVR = -1
         ILVR = .FALSE.
      END IF
      ILV = ILVL .OR. ILVR
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( IJOBVL.LE.0 ) THEN
         INFO = -1
      ELSE IF( IJOBVR.LE.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDVL.LT.1 .OR. ( ILVL .AND. LDVL.LT.N ) ) THEN
         INFO = -12
      ELSE IF( LDVR.LT.1 .OR. ( ILVR .AND. LDVR.LT.N ) ) THEN
         INFO = -14
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV. The workspace is
*       computed assuming ILO = 1 and IHI = N, the worst case.)
*
      IF( INFO.EQ.0 ) THEN
         MINWRK = MAX( 1, 8*N )
         MAXWRK = MAX( 1, N*( 7 +
     $                 ILAENV( 1, 'DGEQRF', ' ', N, 1, N, 0 ) ) )
         MAXWRK = MAX( MAXWRK, N*( 7 +
     $                 ILAENV( 1, 'DORMQR', ' ', N, 1, N, 0 ) ) )
         IF( ILVL ) THEN
            MAXWRK = MAX( MAXWRK, N*( 7 +
     $                 ILAENV( 1, 'DORGQR', ' ', N, 1, N, -1 ) ) )
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY )
     $      INFO = -16
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGGEV ', -INFO )
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
      ANRM = DLANGE( 'M', N, N, A, LDA, WORK )
      ILASCL = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ANRMTO = SMLNUM
         ILASCL = .TRUE.
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ANRMTO = BIGNUM
         ILASCL = .TRUE.
      END IF
      IF( ILASCL )
     $   CALL DLASCL( 'G', 0, 0, ANRM, ANRMTO, N, N, A, LDA, IERR )
*
*     Scale B if max element outside range [SMLNUM,BIGNUM]
*
      BNRM = DLANGE( 'M', N, N, B, LDB, WORK )
      ILBSCL = .FALSE.
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
         BNRMTO = SMLNUM
         ILBSCL = .TRUE.
      ELSE IF( BNRM.GT.BIGNUM ) THEN
         BNRMTO = BIGNUM
         ILBSCL = .TRUE.
      END IF
      IF( ILBSCL )
     $   CALL DLASCL( 'G', 0, 0, BNRM, BNRMTO, N, N, B, LDB, IERR )
*
*     Permute the matrices A, B to isolate eigenvalues if possible
*     (Workspace: need 6*N)
*
      ILEFT = 1
      IRIGHT = N + 1
      IWRK = IRIGHT + N
      CALL DGGBAL( 'P', N, A, LDA, B, LDB, ILO, IHI, WORK( ILEFT ),
     $             WORK( IRIGHT ), WORK( IWRK ), IERR )
*
*     Reduce B to triangular form (QR decomposition of B)
*     (Workspace: need N, prefer N*NB)
*
      IROWS = IHI + 1 - ILO
      IF( ILV ) THEN
         ICOLS = N + 1 - ILO
      ELSE
         ICOLS = IROWS
      END IF
      ITAU = IWRK
      IWRK = ITAU + IROWS
      CALL DGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK( ITAU ),
     $             WORK( IWRK ), LWORK+1-IWRK, IERR )
*
*     Apply the orthogonal transformation to matrix A
*     (Workspace: need N, prefer N*NB)
*
      CALL DORMQR( 'L', 'T', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     $             WORK( ITAU ), A( ILO, ILO ), LDA, WORK( IWRK ),
     $             LWORK+1-IWRK, IERR )
*
*     Initialize VL
*     (Workspace: need N, prefer N*NB)
*
      IF( ILVL ) THEN
         CALL DLASET( 'Full', N, N, ZERO, ONE, VL, LDVL )
         IF( IROWS.GT.1 ) THEN
            CALL DLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     $                   VL( ILO+1, ILO ), LDVL )
         END IF
         CALL DORGQR( IROWS, IROWS, IROWS, VL( ILO, ILO ), LDVL,
     $                WORK( ITAU ), WORK( IWRK ), LWORK+1-IWRK, IERR )
      END IF
*
*     Initialize VR
*
      IF( ILVR )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, VR, LDVR )
*
*     Reduce to generalized Hessenberg form
*     (Workspace: none needed)
*
      IF( ILV ) THEN
*
*        Eigenvectors requested -- work on whole matrix.
*
         CALL DGGHRD( JOBVL, JOBVR, N, ILO, IHI, A, LDA, B, LDB, VL,
     $                LDVL, VR, LDVR, IERR )
      ELSE
         CALL DGGHRD( 'N', 'N', IROWS, 1, IROWS, A( ILO, ILO ), LDA,
     $                B( ILO, ILO ), LDB, VL, LDVL, VR, LDVR, IERR )
      END IF
*
*     Perform QZ algorithm (Compute eigenvalues, and optionally, the
*     Schur forms and Schur vectors)
*     (Workspace: need N)
*
      IWRK = ITAU
      IF( ILV ) THEN
         CHTEMP = 'S'
      ELSE
         CHTEMP = 'E'
      END IF
      CALL DHGEQZ( CHTEMP, JOBVL, JOBVR, N, ILO, IHI, A, LDA, B, LDB,
     $             ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR,
     $             WORK( IWRK ), LWORK+1-IWRK, IERR )
      IF( IERR.NE.0 ) THEN
         IF( IERR.GT.0 .AND. IERR.LE.N ) THEN
            INFO = IERR
         ELSE IF( IERR.GT.N .AND. IERR.LE.2*N ) THEN
            INFO = IERR - N
         ELSE
            INFO = N + 1
         END IF
         GO TO 110
      END IF
*
*     Compute Eigenvectors
*     (Workspace: need 6*N)
*
      IF( ILV ) THEN
         IF( ILVL ) THEN
            IF( ILVR ) THEN
               CHTEMP = 'B'
            ELSE
               CHTEMP = 'L'
            END IF
         ELSE
            CHTEMP = 'R'
         END IF
         CALL DTGEVC( CHTEMP, 'B', LDUMMA, N, A, LDA, B, LDB, VL, LDVL,
     $                VR, LDVR, N, IN, WORK( IWRK ), IERR )
         IF( IERR.NE.0 ) THEN
            INFO = N + 2
            GO TO 110
         END IF
*
*        Undo balancing on VL and VR and normalization
*        (Workspace: none needed)
*
         IF( ILVL ) THEN
            CALL DGGBAK( 'P', 'L', N, ILO, IHI, WORK( ILEFT ),
     $                   WORK( IRIGHT ), N, VL, LDVL, IERR )
            DO 50 JC = 1, N
               IF( ALPHAI( JC ).LT.ZERO )
     $            GO TO 50
               TEMP = ZERO
               IF( ALPHAI( JC ).EQ.ZERO ) THEN
                  DO 10 JR = 1, N
                     TEMP = MAX( TEMP, ABS( VL( JR, JC ) ) )
   10             CONTINUE
               ELSE
                  DO 20 JR = 1, N
                     TEMP = MAX( TEMP, ABS( VL( JR, JC ) )+
     $                      ABS( VL( JR, JC+1 ) ) )
   20             CONTINUE
               END IF
               IF( TEMP.LT.SMLNUM )
     $            GO TO 50
               TEMP = ONE / TEMP
               IF( ALPHAI( JC ).EQ.ZERO ) THEN
                  DO 30 JR = 1, N
                     VL( JR, JC ) = VL( JR, JC )*TEMP
   30             CONTINUE
               ELSE
                  DO 40 JR = 1, N
                     VL( JR, JC ) = VL( JR, JC )*TEMP
                     VL( JR, JC+1 ) = VL( JR, JC+1 )*TEMP
   40             CONTINUE
               END IF
   50       CONTINUE
         END IF
         IF( ILVR ) THEN
            CALL DGGBAK( 'P', 'R', N, ILO, IHI, WORK( ILEFT ),
     $                   WORK( IRIGHT ), N, VR, LDVR, IERR )
            DO 100 JC = 1, N
               IF( ALPHAI( JC ).LT.ZERO )
     $            GO TO 100
               TEMP = ZERO
               IF( ALPHAI( JC ).EQ.ZERO ) THEN
                  DO 60 JR = 1, N
                     TEMP = MAX( TEMP, ABS( VR( JR, JC ) ) )
   60             CONTINUE
               ELSE
                  DO 70 JR = 1, N
                     TEMP = MAX( TEMP, ABS( VR( JR, JC ) )+
     $                      ABS( VR( JR, JC+1 ) ) )
   70             CONTINUE
               END IF
               IF( TEMP.LT.SMLNUM )
     $            GO TO 100
               TEMP = ONE / TEMP
               IF( ALPHAI( JC ).EQ.ZERO ) THEN
                  DO 80 JR = 1, N
                     VR( JR, JC ) = VR( JR, JC )*TEMP
   80             CONTINUE
               ELSE
                  DO 90 JR = 1, N
                     VR( JR, JC ) = VR( JR, JC )*TEMP
                     VR( JR, JC+1 ) = VR( JR, JC+1 )*TEMP
   90             CONTINUE
               END IF
  100       CONTINUE
         END IF
*
*        End of eigenvector calculation
*
      END IF
*
*     Undo scaling if necessary
*
      IF( ILASCL ) THEN
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAR, N, IERR )
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAI, N, IERR )
      END IF
*
      IF( ILBSCL ) THEN
         CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR )
      END IF
*
  110 CONTINUE
*
      WORK( 1 ) = MAXWRK
*
      RETURN
*
*     End of DGGEV
*
      END
      SUBROUTINE DGGEVX( BALANC, JOBVL, JOBVR, SENSE, N, A, LDA, B, LDB,
     $                   ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR, ILO,
     $                   IHI, LSCALE, RSCALE, ABNRM, BBNRM, RCONDE,
     $                   RCONDV, WORK, LWORK, IWORK, BWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          BALANC, JOBVL, JOBVR, SENSE
      INTEGER            IHI, ILO, INFO, LDA, LDB, LDVL, LDVR, LWORK, N
      DOUBLE PRECISION   ABNRM, BBNRM
*     ..
*     .. Array Arguments ..
      LOGICAL            BWORK( * )
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), LSCALE( * ),
     $                   RCONDE( * ), RCONDV( * ), RSCALE( * ),
     $                   VL( LDVL, * ), VR( LDVR, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGGEVX computes for a pair of N-by-N real nonsymmetric matrices (A,B)
*  the generalized eigenvalues, and optionally, the left and/or right
*  generalized eigenvectors.
*
*  Optionally also, it computes a balancing transformation to improve
*  the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
*  LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
*  the eigenvalues (RCONDE), and reciprocal condition numbers for the
*  right eigenvectors (RCONDV).
*
*  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
*  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
*  singular. It is usually represented as the pair (alpha,beta), as
*  there is a reasonable interpretation for beta=0, and even for both
*  being zero.
*
*  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*
*                   A * v(j) = lambda(j) * B * v(j) .
*
*  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
*  of (A,B) satisfies
*
*                   u(j)**H * A  = lambda(j) * u(j)**H * B.
*
*  where u(j)**H is the conjugate-transpose of u(j).
*
*
*  Arguments
*  =========
*
*  BALANC  (input) CHARACTER*1
*          Specifies the balance option to be performed.
*          = 'N':  do not diagonally scale or permute;
*          = 'P':  permute only;
*          = 'S':  scale only;
*          = 'B':  both permute and scale.
*          Computed reciprocal condition numbers will be for the
*          matrices after permuting and/or balancing. Permuting does
*          not change condition numbers (in exact arithmetic), but
*          balancing does.
*
*  JOBVL   (input) CHARACTER*1
*          = 'N':  do not compute the left generalized eigenvectors;
*          = 'V':  compute the left generalized eigenvectors.
*
*  JOBVR   (input) CHARACTER*1
*          = 'N':  do not compute the right generalized eigenvectors;
*          = 'V':  compute the right generalized eigenvectors.
*
*  SENSE   (input) CHARACTER*1
*          Determines which reciprocal condition numbers are computed.
*          = 'N': none are computed;
*          = 'E': computed for eigenvalues only;
*          = 'V': computed for eigenvectors only;
*          = 'B': computed for eigenvalues and eigenvectors.
*
*  N       (input) INTEGER
*          The order of the matrices A, B, VL, and VR.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the matrix A in the pair (A,B).
*          On exit, A has been overwritten. If JOBVL='V' or JOBVR='V'
*          or both, then A contains the first part of the real Schur
*          form of the "balanced" versions of the input A and B.
*
*  LDA     (input) INTEGER
*          The leading dimension of A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the matrix B in the pair (A,B).
*          On exit, B has been overwritten. If JOBVL='V' or JOBVR='V'
*          or both, then B contains the second part of the real Schur
*          form of the "balanced" versions of the input A and B.
*
*  LDB     (input) INTEGER
*          The leading dimension of B.  LDB >= max(1,N).
*
*  ALPHAR  (output) DOUBLE PRECISION array, dimension (N)
*  ALPHAI  (output) DOUBLE PRECISION array, dimension (N)
*  BETA    (output) DOUBLE PRECISION array, dimension (N)
*          On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
*          be the generalized eigenvalues.  If ALPHAI(j) is zero, then
*          the j-th eigenvalue is real; if positive, then the j-th and
*          (j+1)-st eigenvalues are a complex conjugate pair, with
*          ALPHAI(j+1) negative.
*
*          Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
*          may easily over- or underflow, and BETA(j) may even be zero.
*          Thus, the user should avoid naively computing the ratio
*          ALPHA/BETA. However, ALPHAR and ALPHAI will be always less
*          than and usually comparable with norm(A) in magnitude, and
*          BETA always less than and usually comparable with norm(B).
*
*  VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
*          If JOBVL = 'V', the left eigenvectors u(j) are stored one
*          after another in the columns of VL, in the same order as
*          their eigenvalues. If the j-th eigenvalue is real, then
*          u(j) = VL(:,j), the j-th column of VL. If the j-th and
*          (j+1)-th eigenvalues form a complex conjugate pair, then
*          u(j) = VL(:,j)+i*VL(:,j+1) and u(j+1) = VL(:,j)-i*VL(:,j+1).
*          Each eigenvector will be scaled so the largest component have
*          abs(real part) + abs(imag. part) = 1.
*          Not referenced if JOBVL = 'N'.
*
*  LDVL    (input) INTEGER
*          The leading dimension of the matrix VL. LDVL >= 1, and
*          if JOBVL = 'V', LDVL >= N.
*
*  VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
*          If JOBVR = 'V', the right eigenvectors v(j) are stored one
*          after another in the columns of VR, in the same order as
*          their eigenvalues. If the j-th eigenvalue is real, then
*          v(j) = VR(:,j), the j-th column of VR. If the j-th and
*          (j+1)-th eigenvalues form a complex conjugate pair, then
*          v(j) = VR(:,j)+i*VR(:,j+1) and v(j+1) = VR(:,j)-i*VR(:,j+1).
*          Each eigenvector will be scaled so the largest component have
*          abs(real part) + abs(imag. part) = 1.
*          Not referenced if JOBVR = 'N'.
*
*  LDVR    (input) INTEGER
*          The leading dimension of the matrix VR. LDVR >= 1, and
*          if JOBVR = 'V', LDVR >= N.
*
*  ILO     (output) INTEGER
*  IHI     (output) INTEGER
*          ILO and IHI are integer values such that on exit
*          A(i,j) = 0 and B(i,j) = 0 if i > j and
*          j = 1,...,ILO-1 or i = IHI+1,...,N.
*          If BALANC = 'N' or 'S', ILO = 1 and IHI = N.
*
*  LSCALE  (output) DOUBLE PRECISION array, dimension (N)
*          Details of the permutations and scaling factors applied
*          to the left side of A and B.  If PL(j) is the index of the
*          row interchanged with row j, and DL(j) is the scaling
*          factor applied to row j, then
*            LSCALE(j) = PL(j)  for j = 1,...,ILO-1
*                      = DL(j)  for j = ILO,...,IHI
*                      = PL(j)  for j = IHI+1,...,N.
*          The order in which the interchanges are made is N to IHI+1,
*          then 1 to ILO-1.
*
*  RSCALE  (output) DOUBLE PRECISION array, dimension (N)
*          Details of the permutations and scaling factors applied
*          to the right side of A and B.  If PR(j) is the index of the
*          column interchanged with column j, and DR(j) is the scaling
*          factor applied to column j, then
*            RSCALE(j) = PR(j)  for j = 1,...,ILO-1
*                      = DR(j)  for j = ILO,...,IHI
*                      = PR(j)  for j = IHI+1,...,N
*          The order in which the interchanges are made is N to IHI+1,
*          then 1 to ILO-1.
*
*  ABNRM   (output) DOUBLE PRECISION
*          The one-norm of the balanced matrix A.
*
*  BBNRM   (output) DOUBLE PRECISION
*          The one-norm of the balanced matrix B.
*
*  RCONDE  (output) DOUBLE PRECISION array, dimension (N)
*          If SENSE = 'E' or 'B', the reciprocal condition numbers of
*          the eigenvalues, stored in consecutive elements of the array.
*          For a complex conjugate pair of eigenvalues two consecutive
*          elements of RCONDE are set to the same value. Thus RCONDE(j),
*          RCONDV(j), and the j-th columns of VL and VR all correspond
*          to the j-th eigenpair.
*          If SENSE = 'N or 'V', RCONDE is not referenced.
*
*  RCONDV  (output) DOUBLE PRECISION array, dimension (N)
*          If SENSE = 'V' or 'B', the estimated reciprocal condition
*          numbers of the eigenvectors, stored in consecutive elements
*          of the array. For a complex eigenvector two consecutive
*          elements of RCONDV are set to the same value. If the
*          eigenvalues cannot be reordered to compute RCONDV(j),
*          RCONDV(j) is set to 0; this can only occur when the true
*          value would be very small anyway.
*          If SENSE = 'N' or 'E', RCONDV is not referenced.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,2*N).
*          If BALANC = 'S' or 'B', or JOBVL = 'V', or JOBVR = 'V',
*          LWORK >= max(1,6*N).
*          If SENSE = 'E' or 'B', LWORK >= max(1,10*N).
*          If SENSE = 'V' or 'B', LWORK >= 2*N*N+8*N+16.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace) INTEGER array, dimension (N+6)
*          If SENSE = 'E', IWORK is not referenced.
*
*  BWORK   (workspace) LOGICAL array, dimension (N)
*          If SENSE = 'N', BWORK is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          = 1,...,N:
*                The QZ iteration failed.  No eigenvectors have been
*                calculated, but ALPHAR(j), ALPHAI(j), and BETA(j)
*                should be correct for j=INFO+1,...,N.
*          > N:  =N+1: other than QZ iteration failed in DHGEQZ.
*                =N+2: error return from DTGEVC.
*
*  Further Details
*  ===============
*
*  Balancing a matrix pair (A,B) includes, first, permuting rows and
*  columns to isolate eigenvalues, second, applying diagonal similarity
*  transformation to the rows and columns to make the rows and columns
*  as close in norm as possible. The computed reciprocal condition
*  numbers correspond to the balanced matrix. Permuting rows and columns
*  will not change the condition numbers (in exact arithmetic) but
*  diagonal scaling will.  For further explanation of balancing, see
*  section 4.11.1.2 of LAPACK Users' Guide.
*
*  An approximate error bound on the chordal distance between the i-th
*  computed generalized eigenvalue w and the corresponding exact
*  eigenvalue lambda is
*
*       chord(w, lambda) <= EPS * norm(ABNRM, BBNRM) / RCONDE(I)
*
*  An approximate error bound for the angle between the i-th computed
*  eigenvector VL(i) or VR(i) is given by
*
*       EPS * norm(ABNRM, BBNRM) / DIF(i).
*
*  For further explanation of the reciprocal condition numbers RCONDE
*  and RCONDV, see section 4.11 of LAPACK User's Guide.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ILASCL, ILBSCL, ILV, ILVL, ILVR, LQUERY, NOSCL,
     $                   PAIR, WANTSB, WANTSE, WANTSN, WANTSV
      CHARACTER          CHTEMP
      INTEGER            I, ICOLS, IERR, IJOBVL, IJOBVR, IN, IROWS,
     $                   ITAU, IWRK, IWRK1, J, JC, JR, M, MAXWRK,
     $                   MINWRK, MM
      DOUBLE PRECISION   ANRM, ANRMTO, BIGNUM, BNRM, BNRMTO, EPS,
     $                   SMLNUM, TEMP
*     ..
*     .. Local Arrays ..
      LOGICAL            LDUMMA( 1 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEQRF, DGGBAK, DGGBAL, DGGHRD, DHGEQZ, DLABAD,
     $                   DLACPY, DLASCL, DLASET, DORGQR, DORMQR, DTGEVC,
     $                   DTGSNA, XERBLA 
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANGE
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
*     ..
*     .. Executable Statements ..
*
*     Decode the input arguments
*
      IF( LSAME( JOBVL, 'N' ) ) THEN
         IJOBVL = 1
         ILVL = .FALSE.
      ELSE IF( LSAME( JOBVL, 'V' ) ) THEN
         IJOBVL = 2
         ILVL = .TRUE.
      ELSE
         IJOBVL = -1
         ILVL = .FALSE.
      END IF
*
      IF( LSAME( JOBVR, 'N' ) ) THEN
         IJOBVR = 1
         ILVR = .FALSE.
      ELSE IF( LSAME( JOBVR, 'V' ) ) THEN
         IJOBVR = 2
         ILVR = .TRUE.
      ELSE
         IJOBVR = -1
         ILVR = .FALSE.
      END IF
      ILV = ILVL .OR. ILVR
*
      NOSCL  = LSAME( BALANC, 'N' ) .OR. LSAME( BALANC, 'P' )
      WANTSN = LSAME( SENSE, 'N' )
      WANTSE = LSAME( SENSE, 'E' )
      WANTSV = LSAME( SENSE, 'V' )
      WANTSB = LSAME( SENSE, 'B' )
*
*     Test the input arguments
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.( LSAME( BALANC, 'N' ) .OR. LSAME( BALANC,
     $    'S' ) .OR. LSAME( BALANC, 'P' ) .OR. LSAME( BALANC, 'B' ) ) )
     $     THEN
         INFO = -1
      ELSE IF( IJOBVL.LE.0 ) THEN
         INFO = -2
      ELSE IF( IJOBVR.LE.0 ) THEN
         INFO = -3
      ELSE IF( .NOT.( WANTSN .OR. WANTSE .OR. WANTSB .OR. WANTSV ) )
     $          THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDVL.LT.1 .OR. ( ILVL .AND. LDVL.LT.N ) ) THEN
         INFO = -14
      ELSE IF( LDVR.LT.1 .OR. ( ILVR .AND. LDVR.LT.N ) ) THEN
         INFO = -16
      END IF
*
*     Compute workspace
*      (Note: Comments in the code beginning "Workspace:" describe the
*       minimal amount of workspace needed at that point in the code,
*       as well as the preferred amount for good performance.
*       NB refers to the optimal block size for the immediately
*       following subroutine, as returned by ILAENV. The workspace is
*       computed assuming ILO = 1 and IHI = N, the worst case.)
*
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            MINWRK = 1
            MAXWRK = 1
         ELSE
            IF( NOSCL .AND. .NOT.ILV ) THEN
               MINWRK = 2*N
            ELSE
               MINWRK = 6*N
            END IF
            IF( WANTSE .OR. WANTSB ) THEN
               MINWRK = 10*N
            END IF
            IF( WANTSV .OR. WANTSB ) THEN
               MINWRK = MAX( MINWRK, 2*N*( N + 4 ) + 16 )
            END IF
            MAXWRK = MINWRK
            MAXWRK = MAX( MAXWRK,
     $                    N + N*ILAENV( 1, 'DGEQRF', ' ', N, 1, N, 0 ) )
            MAXWRK = MAX( MAXWRK,
     $                    N + N*ILAENV( 1, 'DORMQR', ' ', N, 1, N, 0 ) )
            IF( ILVL ) THEN
               MAXWRK = MAX( MAXWRK, N +
     $                       N*ILAENV( 1, 'DORGQR', ' ', N, 1, N, 0 ) )
            END IF
         END IF
         WORK( 1 ) = MAXWRK
*
         IF( LWORK.LT.MINWRK .AND. .NOT.LQUERY ) THEN
            INFO = -26
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGGEVX', -INFO )
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
      ANRM = DLANGE( 'M', N, N, A, LDA, WORK )
      ILASCL = .FALSE.
      IF( ANRM.GT.ZERO .AND. ANRM.LT.SMLNUM ) THEN
         ANRMTO = SMLNUM
         ILASCL = .TRUE.
      ELSE IF( ANRM.GT.BIGNUM ) THEN
         ANRMTO = BIGNUM
         ILASCL = .TRUE.
      END IF
      IF( ILASCL )
     $   CALL DLASCL( 'G', 0, 0, ANRM, ANRMTO, N, N, A, LDA, IERR )
*
*     Scale B if max element outside range [SMLNUM,BIGNUM]
*
      BNRM = DLANGE( 'M', N, N, B, LDB, WORK )
      ILBSCL = .FALSE.
      IF( BNRM.GT.ZERO .AND. BNRM.LT.SMLNUM ) THEN
         BNRMTO = SMLNUM
         ILBSCL = .TRUE.
      ELSE IF( BNRM.GT.BIGNUM ) THEN
         BNRMTO = BIGNUM
         ILBSCL = .TRUE.
      END IF
      IF( ILBSCL )
     $   CALL DLASCL( 'G', 0, 0, BNRM, BNRMTO, N, N, B, LDB, IERR )
*
*     Permute and/or balance the matrix pair (A,B)
*     (Workspace: need 6*N if BALANC = 'S' or 'B', 1 otherwise)
*
      CALL DGGBAL( BALANC, N, A, LDA, B, LDB, ILO, IHI, LSCALE, RSCALE,
     $             WORK, IERR )
*
*     Compute ABNRM and BBNRM
*
      ABNRM = DLANGE( '1', N, N, A, LDA, WORK( 1 ) )
      IF( ILASCL ) THEN
         WORK( 1 ) = ABNRM
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, 1, 1, WORK( 1 ), 1,
     $                IERR )
         ABNRM = WORK( 1 )
      END IF
*
      BBNRM = DLANGE( '1', N, N, B, LDB, WORK( 1 ) )
      IF( ILBSCL ) THEN
         WORK( 1 ) = BBNRM
         CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, 1, 1, WORK( 1 ), 1,
     $                IERR )
         BBNRM = WORK( 1 )
      END IF
*
*     Reduce B to triangular form (QR decomposition of B)
*     (Workspace: need N, prefer N*NB )
*
      IROWS = IHI + 1 - ILO
      IF( ILV .OR. .NOT.WANTSN ) THEN
         ICOLS = N + 1 - ILO
      ELSE
         ICOLS = IROWS
      END IF
      ITAU = 1
      IWRK = ITAU + IROWS
      CALL DGEQRF( IROWS, ICOLS, B( ILO, ILO ), LDB, WORK( ITAU ),
     $             WORK( IWRK ), LWORK+1-IWRK, IERR )
*
*     Apply the orthogonal transformation to A
*     (Workspace: need N, prefer N*NB)
*
      CALL DORMQR( 'L', 'T', IROWS, ICOLS, IROWS, B( ILO, ILO ), LDB,
     $             WORK( ITAU ), A( ILO, ILO ), LDA, WORK( IWRK ),
     $             LWORK+1-IWRK, IERR )
*
*     Initialize VL and/or VR
*     (Workspace: need N, prefer N*NB)
*
      IF( ILVL ) THEN
         CALL DLASET( 'Full', N, N, ZERO, ONE, VL, LDVL )
         IF( IROWS.GT.1 ) THEN
            CALL DLACPY( 'L', IROWS-1, IROWS-1, B( ILO+1, ILO ), LDB,
     $                   VL( ILO+1, ILO ), LDVL )
         END IF
         CALL DORGQR( IROWS, IROWS, IROWS, VL( ILO, ILO ), LDVL,
     $                WORK( ITAU ), WORK( IWRK ), LWORK+1-IWRK, IERR )
      END IF
*
      IF( ILVR )
     $   CALL DLASET( 'Full', N, N, ZERO, ONE, VR, LDVR )
*
*     Reduce to generalized Hessenberg form
*     (Workspace: none needed)
*
      IF( ILV .OR. .NOT.WANTSN ) THEN
*
*        Eigenvectors requested -- work on whole matrix.
*
         CALL DGGHRD( JOBVL, JOBVR, N, ILO, IHI, A, LDA, B, LDB, VL,
     $                LDVL, VR, LDVR, IERR )
      ELSE
         CALL DGGHRD( 'N', 'N', IROWS, 1, IROWS, A( ILO, ILO ), LDA,
     $                B( ILO, ILO ), LDB, VL, LDVL, VR, LDVR, IERR )
      END IF
*
*     Perform QZ algorithm (Compute eigenvalues, and optionally, the
*     Schur forms and Schur vectors)
*     (Workspace: need N)
*
      IF( ILV .OR. .NOT.WANTSN ) THEN
         CHTEMP = 'S'
      ELSE
         CHTEMP = 'E'
      END IF
*
      CALL DHGEQZ( CHTEMP, JOBVL, JOBVR, N, ILO, IHI, A, LDA, B, LDB,
     $             ALPHAR, ALPHAI, BETA, VL, LDVL, VR, LDVR, WORK,
     $             LWORK, IERR )
      IF( IERR.NE.0 ) THEN
         IF( IERR.GT.0 .AND. IERR.LE.N ) THEN
            INFO = IERR
         ELSE IF( IERR.GT.N .AND. IERR.LE.2*N ) THEN
            INFO = IERR - N
         ELSE
            INFO = N + 1
         END IF
         GO TO 130
      END IF
*
*     Compute Eigenvectors and estimate condition numbers if desired
*     (Workspace: DTGEVC: need 6*N
*                 DTGSNA: need 2*N*(N+2)+16 if SENSE = 'V' or 'B',
*                         need N otherwise )
*
      IF( ILV .OR. .NOT.WANTSN ) THEN
         IF( ILV ) THEN
            IF( ILVL ) THEN
               IF( ILVR ) THEN
                  CHTEMP = 'B'
               ELSE
                  CHTEMP = 'L'
               END IF
            ELSE
               CHTEMP = 'R'
            END IF
*
            CALL DTGEVC( CHTEMP, 'B', LDUMMA, N, A, LDA, B, LDB, VL,
     $                   LDVL, VR, LDVR, N, IN, WORK, IERR )
            IF( IERR.NE.0 ) THEN
               INFO = N + 2
               GO TO 130
            END IF
         END IF
*
         IF( .NOT.WANTSN ) THEN
*
*           compute eigenvectors (DTGEVC) and estimate condition
*           numbers (DTGSNA). Note that the definition of the condition
*           number is not invariant under transformation (u,v) to
*           (Q*u, Z*v), where (u,v) are eigenvectors of the generalized
*           Schur form (S,T), Q and Z are orthogonal matrices. In order
*           to avoid using extra 2*N*N workspace, we have to recalculate
*           eigenvectors and estimate one condition numbers at a time.
*
            PAIR = .FALSE.
            DO 20 I = 1, N
*
               IF( PAIR ) THEN
                  PAIR = .FALSE.
                  GO TO 20
               END IF
               MM = 1
               IF( I.LT.N ) THEN
                  IF( A( I+1, I ).NE.ZERO ) THEN
                     PAIR = .TRUE.
                     MM = 2
                  END IF
               END IF
*
               DO 10 J = 1, N
                  BWORK( J ) = .FALSE.
   10          CONTINUE
               IF( MM.EQ.1 ) THEN
                  BWORK( I ) = .TRUE.
               ELSE IF( MM.EQ.2 ) THEN
                  BWORK( I ) = .TRUE.
                  BWORK( I+1 ) = .TRUE.
               END IF
*
               IWRK = MM*N + 1
               IWRK1 = IWRK + MM*N
*
*              Compute a pair of left and right eigenvectors.
*              (compute workspace: need up to 4*N + 6*N)
*
               IF( WANTSE .OR. WANTSB ) THEN
                  CALL DTGEVC( 'B', 'S', BWORK, N, A, LDA, B, LDB,
     $                         WORK( 1 ), N, WORK( IWRK ), N, MM, M,
     $                         WORK( IWRK1 ), IERR )
                  IF( IERR.NE.0 ) THEN
                     INFO = N + 2
                     GO TO 130
                  END IF
               END IF
*
               CALL DTGSNA( SENSE, 'S', BWORK, N, A, LDA, B, LDB,
     $                      WORK( 1 ), N, WORK( IWRK ), N, RCONDE( I ),
     $                      RCONDV( I ), MM, M, WORK( IWRK1 ),
     $                      LWORK-IWRK1+1, IWORK, IERR )
*
   20       CONTINUE
         END IF
      END IF
*
*     Undo balancing on VL and VR and normalization
*     (Workspace: none needed)
*
      IF( ILVL ) THEN
         CALL DGGBAK( BALANC, 'L', N, ILO, IHI, LSCALE, RSCALE, N, VL,
     $                LDVL, IERR )
*
         DO 70 JC = 1, N
            IF( ALPHAI( JC ).LT.ZERO )
     $         GO TO 70
            TEMP = ZERO
            IF( ALPHAI( JC ).EQ.ZERO ) THEN
               DO 30 JR = 1, N
                  TEMP = MAX( TEMP, ABS( VL( JR, JC ) ) )
   30          CONTINUE
            ELSE
               DO 40 JR = 1, N
                  TEMP = MAX( TEMP, ABS( VL( JR, JC ) )+
     $                   ABS( VL( JR, JC+1 ) ) )
   40          CONTINUE
            END IF
            IF( TEMP.LT.SMLNUM )
     $         GO TO 70
            TEMP = ONE / TEMP
            IF( ALPHAI( JC ).EQ.ZERO ) THEN
               DO 50 JR = 1, N
                  VL( JR, JC ) = VL( JR, JC )*TEMP
   50          CONTINUE
            ELSE
               DO 60 JR = 1, N
                  VL( JR, JC ) = VL( JR, JC )*TEMP
                  VL( JR, JC+1 ) = VL( JR, JC+1 )*TEMP
   60          CONTINUE
            END IF
   70    CONTINUE
      END IF
      IF( ILVR ) THEN
         CALL DGGBAK( BALANC, 'R', N, ILO, IHI, LSCALE, RSCALE, N, VR,
     $                LDVR, IERR )
         DO 120 JC = 1, N
            IF( ALPHAI( JC ).LT.ZERO )
     $         GO TO 120
            TEMP = ZERO
            IF( ALPHAI( JC ).EQ.ZERO ) THEN
               DO 80 JR = 1, N
                  TEMP = MAX( TEMP, ABS( VR( JR, JC ) ) )
   80          CONTINUE
            ELSE
               DO 90 JR = 1, N
                  TEMP = MAX( TEMP, ABS( VR( JR, JC ) )+
     $                   ABS( VR( JR, JC+1 ) ) )
   90          CONTINUE
            END IF
            IF( TEMP.LT.SMLNUM )
     $         GO TO 120
            TEMP = ONE / TEMP
            IF( ALPHAI( JC ).EQ.ZERO ) THEN
               DO 100 JR = 1, N
                  VR( JR, JC ) = VR( JR, JC )*TEMP
  100          CONTINUE
            ELSE
               DO 110 JR = 1, N
                  VR( JR, JC ) = VR( JR, JC )*TEMP
                  VR( JR, JC+1 ) = VR( JR, JC+1 )*TEMP
  110          CONTINUE
            END IF
  120    CONTINUE
      END IF
*
*     Undo scaling if necessary
*
      IF( ILASCL ) THEN
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAR, N, IERR )
         CALL DLASCL( 'G', 0, 0, ANRMTO, ANRM, N, 1, ALPHAI, N, IERR )
      END IF
*
      IF( ILBSCL ) THEN
         CALL DLASCL( 'G', 0, 0, BNRMTO, BNRM, N, 1, BETA, N, IERR )
      END IF
*
  130 CONTINUE
      WORK( 1 ) = MAXWRK
*
      RETURN
*
*     End of DGGEVX
*
      END
      SUBROUTINE DGGGLM( N, M, P, A, LDA, B, LDB, D, X, Y, WORK, LWORK,
     $                   INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, P
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), D( * ), WORK( * ),
     $                   X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  DGGGLM solves a general Gauss-Markov linear model (GLM) problem:
*
*          minimize || y ||_2   subject to   d = A*x + B*y
*              x
*
*  where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
*  given N-vector. It is assumed that M <= N <= M+P, and
*
*             rank(A) = M    and    rank( A B ) = N.
*
*  Under these assumptions, the constrained equation is always
*  consistent, and there is a unique solution x and a minimal 2-norm
*  solution y, which is obtained using a generalized QR factorization
*  of the matrices (A, B) given by
*
*     A = Q*(R),   B = Q*T*Z.
*           (0)
*
*  In particular, if matrix B is square nonsingular, then the problem
*  GLM is equivalent to the following weighted linear least squares
*  problem
*
*               minimize || inv(B)*(d-A*x) ||_2
*                   x
*
*  where inv(B) denotes the inverse of B.
*
*  Arguments
*  =========
*
*  N       (input) INTEGER
*          The number of rows of the matrices A and B.  N >= 0.
*
*  M       (input) INTEGER
*          The number of columns of the matrix A.  0 <= M <= N.
*
*  P       (input) INTEGER
*          The number of columns of the matrix B.  P >= N-M.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,M)
*          On entry, the N-by-M matrix A.
*          On exit, the upper triangular part of the array A contains
*          the M-by-M upper triangular matrix R.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,P)
*          On entry, the N-by-P matrix B.
*          On exit, if N <= P, the upper triangle of the subarray
*          B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
*          if N > P, the elements on and above the (N-P)th subdiagonal
*          contain the N-by-P upper trapezoidal matrix T.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,N).
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, D is the left hand side of the GLM equation.
*          On exit, D is destroyed.
*
*  X       (output) DOUBLE PRECISION array, dimension (M)
*  Y       (output) DOUBLE PRECISION array, dimension (P)
*          On exit, X and Y are the solutions of the GLM problem.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,N+M+P).
*          For optimum performance, LWORK >= M+min(N,P)+max(N,P)*NB,
*          where NB is an upper bound for the optimal blocksizes for
*          DGEQRF, SGERQF, DORMQR and SORMRQ.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          = 1:  the upper triangular factor R associated with A in the
*                generalized QR factorization of the pair (A, B) is
*                singular, so that rank(A) < M; the least squares
*                solution could not be computed.
*          = 2:  the bottom (N-M) by (N-M) part of the upper trapezoidal
*                factor T associated with B in the generalized QR
*                factorization of the pair (A, B) is singular, so that
*                rank( A B ) < N; the least squares solution could not
*                be computed.
*
*  ===================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            I, LOPT, LWKMIN, LWKOPT, NB, NB1, NB2, NB3,
     $                   NB4, NP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMV, DGGQRF, DORMQR, DORMRQ, DTRTRS,
     $                   XERBLA
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      NP = MIN( N, P )
      LQUERY = ( LWORK.EQ.-1 )
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( M.LT.0 .OR. M.GT.N ) THEN
         INFO = -2
      ELSE IF( P.LT.0 .OR. P.LT.N-M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
*
*     Calculate workspace
*
      IF( INFO.EQ.0) THEN
         IF( N.EQ.0 ) THEN
            LWKMIN = 1
            LWKOPT = 1
         ELSE
            NB1 = ILAENV( 1, 'DGEQRF', ' ', N, M, -1, -1 )
            NB2 = ILAENV( 1, 'DGERQF', ' ', N, M, -1, -1 )
            NB3 = ILAENV( 1, 'DORMQR', ' ', N, M, P, -1 )
            NB4 = ILAENV( 1, 'DORMRQ', ' ', N, M, P, -1 )
            NB = MAX( NB1, NB2, NB3, NB4 )
            LWKMIN = M + N + P
            LWKOPT = M + NP + MAX( N, P )*NB
         END IF
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
            INFO = -12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGGGLM', -INFO )
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
*     Compute the GQR factorization of matrices A and B:
*
*            Q'*A = ( R11 ) M,    Q'*B*Z' = ( T11   T12 ) M
*                   (  0  ) N-M             (  0    T22 ) N-M
*                      M                     M+P-N  N-M
*
*     where R11 and T22 are upper triangular, and Q and Z are
*     orthogonal.
*
      CALL DGGQRF( N, M, P, A, LDA, WORK, B, LDB, WORK( M+1 ),
     $             WORK( M+NP+1 ), LWORK-M-NP, INFO )
      LOPT = WORK( M+NP+1 )
*
*     Update left-hand-side vector d = Q'*d = ( d1 ) M
*                                             ( d2 ) N-M
*
      CALL DORMQR( 'Left', 'Transpose', N, 1, M, A, LDA, WORK, D,
     $             MAX( 1, N ), WORK( M+NP+1 ), LWORK-M-NP, INFO )
      LOPT = MAX( LOPT, INT( WORK( M+NP+1 ) ) )
*
*     Solve T22*y2 = d2 for y2
*
      IF( N.GT.M ) THEN
         CALL DTRTRS( 'Upper', 'No transpose', 'Non unit', N-M, 1,
     $                B( M+1, M+P-N+1 ), LDB, D( M+1 ), N-M, INFO )
*
         IF( INFO.GT.0 ) THEN
            INFO = 1
            RETURN
         END IF
*
         CALL DCOPY( N-M, D( M+1 ), 1, Y( M+P-N+1 ), 1 )
      END IF
*
*     Set y1 = 0
*
      DO 10 I = 1, M + P - N
         Y( I ) = ZERO
   10 CONTINUE
*
*     Update d1 = d1 - T12*y2
*
      CALL DGEMV( 'No transpose', M, N-M, -ONE, B( 1, M+P-N+1 ), LDB,
     $            Y( M+P-N+1 ), 1, ONE, D, 1 )
*
*     Solve triangular system: R11*x = d1
*
      IF( M.GT.0 ) THEN
         CALL DTRTRS( 'Upper', 'No Transpose', 'Non unit', M, 1, A, LDA,
     $                D, M, INFO )
*
         IF( INFO.GT.0 ) THEN
            INFO = 2
            RETURN
         END IF
*
*        Copy D to X
*
         CALL DCOPY( M, D, 1, X, 1 )
      END IF
*
*     Backward transformation y = Z'*y
*
      CALL DORMRQ( 'Left', 'Transpose', P, 1, NP,
     $             B( MAX( 1, N-P+1 ), 1 ), LDB, WORK( M+1 ), Y,
     $             MAX( 1, P ), WORK( M+NP+1 ), LWORK-M-NP, INFO )
      WORK( 1 ) = M + NP + MAX( LOPT, INT( WORK( M+NP+1 ) ) )
*
      RETURN
*
*     End of DGGGLM
*
      END
      SUBROUTINE DGGLSE( M, N, P, A, LDA, B, LDB, C, D, X, WORK, LWORK,
     $                   INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, P
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( * ), D( * ),
     $                   WORK( * ), X( * )
*     ..
*
*  Purpose
*  =======
*
*  DGGLSE solves the linear equality-constrained least squares (LSE)
*  problem:
*
*          minimize || c - A*x ||_2   subject to   B*x = d
*
*  where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
*  M-vector, and d is a given P-vector. It is assumed that
*  P <= N <= M+P, and
*
*           rank(B) = P and  rank( (A) ) = N.
*                                ( (B) )
*
*  These conditions ensure that the LSE problem has a unique solution,
*  which is obtained using a generalized RQ factorization of the
*  matrices (B, A) given by
*
*     B = (0 R)*Q,   A = Z*T*Q.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrices A and B. N >= 0.
*
*  P       (input) INTEGER
*          The number of rows of the matrix B. 0 <= P <= N <= M+P.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, the elements on and above the diagonal of the array
*          contain the min(M,N)-by-N upper trapezoidal matrix T.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
*          On entry, the P-by-N matrix B.
*          On exit, the upper triangle of the subarray B(1:P,N-P+1:N)
*          contains the P-by-P upper triangular matrix R.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,P).
*
*  C       (input/output) DOUBLE PRECISION array, dimension (M)
*          On entry, C contains the right hand side vector for the
*          least squares part of the LSE problem.
*          On exit, the residual sum of squares for the solution
*          is given by the sum of squares of elements N-P+1 to M of
*          vector C.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (P)
*          On entry, D contains the right hand side vector for the
*          constrained equation.
*          On exit, D is destroyed.
*
*  X       (output) DOUBLE PRECISION array, dimension (N)
*          On exit, X is the solution of the LSE problem.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK. LWORK >= max(1,M+N+P).
*          For optimum performance LWORK >= P+min(M,N)+max(M,N)*NB,
*          where NB is an upper bound for the optimal blocksizes for
*          DGEQRF, SGERQF, DORMQR and SORMRQ.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          = 1:  the upper triangular factor R associated with B in the
*                generalized RQ factorization of the pair (B, A) is
*                singular, so that rank(B) < P; the least squares
*                solution could not be computed.
*          = 2:  the (N-P) by (N-P) part of the upper trapezoidal factor
*                T associated with A in the generalized RQ factorization
*                of the pair (B, A) is singular, so that
*                rank( (A) ) < N; the least squares solution could not
*                    ( (B) )
*                be computed.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            LOPT, LWKMIN, LWKOPT, MN, NB, NB1, NB2, NB3,
     $                   NB4, NR
*     ..
*     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DGEMV, DGGRQF, DORMQR, DORMRQ,
     $                   DTRMV, DTRTRS, XERBLA
*     ..
*     .. External Functions ..
      INTEGER            ILAENV
      EXTERNAL           ILAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          INT, MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      INFO = 0
      MN = MIN( M, N )
      LQUERY = ( LWORK.EQ.-1 )
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( P.LT.0 .OR. P.GT.N .OR. P.LT.N-M ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, P ) ) THEN
         INFO = -7
      END IF
*
*     Calculate workspace
*
      IF( INFO.EQ.0) THEN
         IF( N.EQ.0 ) THEN
            LWKMIN = 1
            LWKOPT = 1
         ELSE
            NB1 = ILAENV( 1, 'DGEQRF', ' ', M, N, -1, -1 )
            NB2 = ILAENV( 1, 'DGERQF', ' ', M, N, -1, -1 )
            NB3 = ILAENV( 1, 'DORMQR', ' ', M, N, P, -1 )
            NB4 = ILAENV( 1, 'DORMRQ', ' ', M, N, P, -1 )
            NB = MAX( NB1, NB2, NB3, NB4 )
            LWKMIN = M + N + P
            LWKOPT = P + MN + MAX( M, N )*NB
         END IF
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
            INFO = -12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGGLSE', -INFO )
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
*     Compute the GRQ factorization of matrices B and A:
*
*            B*Q' = (  0  T12 ) P   Z'*A*Q' = ( R11 R12 ) N-P
*                     N-P  P                  (  0  R22 ) M+P-N
*                                               N-P  P
*
*     where T12 and R11 are upper triangular, and Q and Z are
*     orthogonal.
*
      CALL DGGRQF( P, M, N, B, LDB, WORK, A, LDA, WORK( P+1 ),
     $             WORK( P+MN+1 ), LWORK-P-MN, INFO )
      LOPT = WORK( P+MN+1 )
*
*     Update c = Z'*c = ( c1 ) N-P
*                       ( c2 ) M+P-N
*
      CALL DORMQR( 'Left', 'Transpose', M, 1, MN, A, LDA, WORK( P+1 ),
     $             C, MAX( 1, M ), WORK( P+MN+1 ), LWORK-P-MN, INFO )
      LOPT = MAX( LOPT, INT( WORK( P+MN+1 ) ) )
*
*     Solve T12*x2 = d for x2
*
      IF( P.GT.0 ) THEN
         CALL DTRTRS( 'Upper', 'No transpose', 'Non-unit', P, 1,
     $                B( 1, N-P+1 ), LDB, D, P, INFO )
*
         IF( INFO.GT.0 ) THEN
            INFO = 1
            RETURN
         END IF
*
*        Put the solution in X
*
         CALL DCOPY( P, D, 1, X( N-P+1 ), 1 )
*
*        Update c1
*
         CALL DGEMV( 'No transpose', N-P, P, -ONE, A( 1, N-P+1 ), LDA,
     $               D, 1, ONE, C, 1 )
      END IF
*
*     Solve R11*x1 = c1 for x1
*
      IF( N.GT.P ) THEN
         CALL DTRTRS( 'Upper', 'No transpose', 'Non-unit', N-P, 1,
     $                A, LDA, C, N-P, INFO )
*
         IF( INFO.GT.0 ) THEN
            INFO = 2
            RETURN
         END IF
*
*        Put the solutions in X
*
         CALL DCOPY( N-P, C, 1, X, 1 )
      END IF
*
*     Compute the residual vector:
*
      IF( M.LT.N ) THEN
         NR = M + P - N
         IF( NR.GT.0 )
     $      CALL DGEMV( 'No transpose', NR, N-M, -ONE, A( N-P+1, M+1 ),
     $                  LDA, D( NR+1 ), 1, ONE, C( N-P+1 ), 1 )
      ELSE
         NR = P
      END IF
      IF( NR.GT.0 ) THEN
         CALL DTRMV( 'Upper', 'No transpose', 'Non unit', NR,
     $               A( N-P+1, N-P+1 ), LDA, D, 1 )
         CALL DAXPY( NR, -ONE, D, 1, C( N-P+1 ), 1 )
      END IF
*
*     Backward transformation x = Q'*x
*
      CALL DORMRQ( 'Left', 'Transpose', N, 1, P, B, LDB, WORK( 1 ), X,
     $             N, WORK( P+MN+1 ), LWORK-P-MN, INFO )
      WORK( 1 ) = P + MN + MAX( LOPT, INT( WORK( P+MN+1 ) ) )
*
      RETURN
*
*     End of DGGLSE
*
      END
      SUBROUTINE DGGSVD( JOBU, JOBV, JOBQ, M, N, P, K, L, A, LDA, B,
     $                   LDB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ, WORK,
     $                   IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBQ, JOBU, JOBV
      INTEGER            INFO, K, L, LDA, LDB, LDQ, LDU, LDV, M, N, P
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), ALPHA( * ), B( LDB, * ),
     $                   BETA( * ), Q( LDQ, * ), U( LDU, * ),
     $                   V( LDV, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DGGSVD computes the generalized singular value decomposition (GSVD)
*  of an M-by-N real matrix A and P-by-N real matrix B:
*
*      U'*A*Q = D1*( 0 R ),    V'*B*Q = D2*( 0 R )
*
*  where U, V and Q are orthogonal matrices, and Z' is the transpose
*  of Z.  Let K+L = the effective numerical rank of the matrix (A',B')',
*  then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and
*  D2 are M-by-(K+L) and P-by-(K+L) "diagonal" matrices and of the
*  following structures, respectively:
*
*  If M-K-L >= 0,
*
*                      K  L
*         D1 =     K ( I  0 )
*                  L ( 0  C )
*              M-K-L ( 0  0 )
*
*                    K  L
*         D2 =   L ( 0  S )
*              P-L ( 0  0 )
*
*                  N-K-L  K    L
*    ( 0 R ) = K (  0   R11  R12 )
*              L (  0    0   R22 )
*
*  where
*
*    C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
*    S = diag( BETA(K+1),  ... , BETA(K+L) ),
*    C**2 + S**2 = I.
*
*    R is stored in A(1:K+L,N-K-L+1:N) on exit.
*
*  If M-K-L < 0,
*
*                    K M-K K+L-M
*         D1 =   K ( I  0    0   )
*              M-K ( 0  C    0   )
*
*                      K M-K K+L-M
*         D2 =   M-K ( 0  S    0  )
*              K+L-M ( 0  0    I  )
*                P-L ( 0  0    0  )
*
*                     N-K-L  K   M-K  K+L-M
*    ( 0 R ) =     K ( 0    R11  R12  R13  )
*                M-K ( 0     0   R22  R23  )
*              K+L-M ( 0     0    0   R33  )
*
*  where
*
*    C = diag( ALPHA(K+1), ... , ALPHA(M) ),
*    S = diag( BETA(K+1),  ... , BETA(M) ),
*    C**2 + S**2 = I.
*
*    (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
*    ( 0  R22 R23 )
*    in B(M-K+1:L,N+M-K-L+1:N) on exit.
*
*  The routine computes C, S, R, and optionally the orthogonal
*  transformation matrices U, V and Q.
*
*  In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
*  A and B implicitly gives the SVD of A*inv(B):
*                       A*inv(B) = U*(D1*inv(D2))*V'.
*  If ( A',B')' has orthonormal columns, then the GSVD of A and B is
*  also equal to the CS decomposition of A and B. Furthermore, the GSVD
*  can be used to derive the solution of the eigenvalue problem:
*                       A'*A x = lambda* B'*B x.
*  In some literature, the GSVD of A and B is presented in the form
*                   U'*A*X = ( 0 D1 ),   V'*B*X = ( 0 D2 )
*  where U and V are orthogonal and X is nonsingular, D1 and D2 are
*  ``diagonal''.  The former GSVD form can be converted to the latter
*  form by taking the nonsingular matrix X as
*
*                       X = Q*( I   0    )
*                             ( 0 inv(R) ).
*
*  Arguments
*  =========
*
*  JOBU    (input) CHARACTER*1
*          = 'U':  Orthogonal matrix U is computed;
*          = 'N':  U is not computed.
*
*  JOBV    (input) CHARACTER*1
*          = 'V':  Orthogonal matrix V is computed;
*          = 'N':  V is not computed.
*
*  JOBQ    (input) CHARACTER*1
*          = 'Q':  Orthogonal matrix Q is computed;
*          = 'N':  Q is not computed.
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrices A and B.  N >= 0.
*
*  P       (input) INTEGER
*          The number of rows of the matrix B.  P >= 0.
*
*  K       (output) INTEGER
*  L       (output) INTEGER
*          On exit, K and L specify the dimension of the subblocks
*          described in the Purpose section.
*          K + L = effective numerical rank of (A',B')'.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the M-by-N matrix A.
*          On exit, A contains the triangular matrix R, or part of R.
*          See Purpose for details.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A. LDA >= max(1,M).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,N)
*          On entry, the P-by-N matrix B.
*          On exit, B contains the triangular matrix R if M-K-L < 0.
*          See Purpose for details.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= max(1,P).
*
*  ALPHA   (output) DOUBLE PRECISION array, dimension (N)
*  BETA    (output) DOUBLE PRECISION array, dimension (N)
*          On exit, ALPHA and BETA contain the generalized singular
*          value pairs of A and B;
*            ALPHA(1:K) = 1,
*            BETA(1:K)  = 0,
*          and if M-K-L >= 0,
*            ALPHA(K+1:K+L) = C,
*            BETA(K+1:K+L)  = S,
*          or if M-K-L < 0,
*            ALPHA(K+1:M)=C, ALPHA(M+1:K+L)=0
*            BETA(K+1:M) =S, BETA(M+1:K+L) =1
*          and
*            ALPHA(K+L+1:N) = 0
*            BETA(K+L+1:N)  = 0
*
*  U       (output) DOUBLE PRECISION array, dimension (LDU,M)
*          If JOBU = 'U', U contains the M-by-M orthogonal matrix U.
*          If JOBU = 'N', U is not referenced.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U. LDU >= max(1,M) if
*          JOBU = 'U'; LDU >= 1 otherwise.
*
*  V       (output) DOUBLE PRECISION array, dimension (LDV,P)
*          If JOBV = 'V', V contains the P-by-P orthogonal matrix V.
*          If JOBV = 'N', V is not referenced.
*
*  LDV     (input) INTEGER
*          The leading dimension of the array V. LDV >= max(1,P) if
*          JOBV = 'V'; LDV >= 1 otherwise.
*
*  Q       (output) DOUBLE PRECISION array, dimension (LDQ,N)
*          If JOBQ = 'Q', Q contains the N-by-N orthogonal matrix Q.
*          If JOBQ = 'N', Q is not referenced.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q. LDQ >= max(1,N) if
*          JOBQ = 'Q'; LDQ >= 1 otherwise.
*
*  WORK    (workspace) DOUBLE PRECISION array,
*                      dimension (max(3*N,M,P)+N)
*
*  IWORK   (workspace/output) INTEGER array, dimension (N)
*          On exit, IWORK stores the sorting information. More
*          precisely, the following loop will sort ALPHA
*             for I = K+1, min(M,K+L)
*                 swap ALPHA(I) and ALPHA(IWORK(I))
*             endfor
*          such that ALPHA(1) >= ALPHA(2) >= ... >= ALPHA(N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = 1, the Jacobi-type procedure failed to
*                converge.  For further details, see subroutine DTGSJA.
*
*  Internal Parameters
*  ===================
*
*  TOLA    DOUBLE PRECISION
*  TOLB    DOUBLE PRECISION
*          TOLA and TOLB are the thresholds to determine the effective
*          rank of (A',B')'. Generally, they are set to
*                   TOLA = MAX(M,N)*norm(A)*MAZHEPS,
*                   TOLB = MAX(P,N)*norm(B)*MAZHEPS.
*          The size of TOLA and TOLB may affect the size of backward
*          errors of the decomposition.
*
*  Further Details
*  ===============
*
*  2-96 Based on modifications by
*     Ming Gu and Huan Ren, Computer Science Division, University of
*     California at Berkeley, USA
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            WANTQ, WANTU, WANTV
      INTEGER            I, IBND, ISUB, J, NCYCLE
      DOUBLE PRECISION   ANORM, BNORM, SMAX, TEMP, TOLA, TOLB, ULP, UNFL
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANGE
      EXTERNAL           LSAME, DLAMCH, DLANGE
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGGSVP, DTGSJA, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters
*
      WANTU = LSAME( JOBU, 'U' )
      WANTV = LSAME( JOBV, 'V' )
      WANTQ = LSAME( JOBQ, 'Q' )
*
      INFO = 0
      IF( .NOT.( WANTU .OR. LSAME( JOBU, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( WANTV .OR. LSAME( JOBV, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( WANTQ .OR. LSAME( JOBQ, 'N' ) ) ) THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( P.LT.0 ) THEN
         INFO = -6
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LDB.LT.MAX( 1, P ) ) THEN
         INFO = -12
      ELSE IF( LDU.LT.1 .OR. ( WANTU .AND. LDU.LT.M ) ) THEN
         INFO = -16
      ELSE IF( LDV.LT.1 .OR. ( WANTV .AND. LDV.LT.P ) ) THEN
         INFO = -18
      ELSE IF( LDQ.LT.1 .OR. ( WANTQ .AND. LDQ.LT.N ) ) THEN
         INFO = -20
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DGGSVD', -INFO )
         RETURN
      END IF
*
*     Compute the Frobenius norm of matrices A and B
*
      ANORM = DLANGE( '1', M, N, A, LDA, WORK )
      BNORM = DLANGE( '1', P, N, B, LDB, WORK )
*
*     Get machine precision and set up threshold for determining
*     the effective numerical rank of the matrices A and B.
*
      ULP = DLAMCH( 'Precision' )
      UNFL = DLAMCH( 'Safe Minimum' )
      TOLA = MAX( M, N )*MAX( ANORM, UNFL )*ULP
      TOLB = MAX( P, N )*MAX( BNORM, UNFL )*ULP
*
*     Preprocessing
*
      CALL DGGSVP( JOBU, JOBV, JOBQ, M, P, N, A, LDA, B, LDB, TOLA,
     $             TOLB, K, L, U, LDU, V, LDV, Q, LDQ, IWORK, WORK,
     $             WORK( N+1 ), INFO )
*
*     Compute the GSVD of two upper "triangular" matrices
*
      CALL DTGSJA( JOBU, JOBV, JOBQ, M, P, N, K, L, A, LDA, B, LDB,
     $             TOLA, TOLB, ALPHA, BETA, U, LDU, V, LDV, Q, LDQ,
     $             WORK, NCYCLE, INFO )
*
*     Sort the singular values and store the pivot indices in IWORK
*     Copy ALPHA to WORK, then sort ALPHA in WORK
*
      CALL DCOPY( N, ALPHA, 1, WORK, 1 )
      IBND = MIN( L, M-K )
      DO 20 I = 1, IBND
*
*        Scan for largest ALPHA(K+I)
*
         ISUB = I
         SMAX = WORK( K+I )
         DO 10 J = I + 1, IBND
            TEMP = WORK( K+J )
            IF( TEMP.GT.SMAX ) THEN
               ISUB = J
               SMAX = TEMP
            END IF
   10    CONTINUE
         IF( ISUB.NE.I ) THEN
            WORK( K+ISUB ) = WORK( K+I )
            WORK( K+I ) = SMAX
            IWORK( K+I ) = K + ISUB
         ELSE
            IWORK( K+I ) = K + I
         END IF
   20 CONTINUE
*
      RETURN
*
*     End of DGGSVD
*
      END
      SUBROUTINE DLANV2( A, B, C, D, RT1R, RT1I, RT2R, RT2I, CS, SN )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, CS, D, RT1I, RT1R, RT2I, RT2R, SN
*     ..
*
*  Purpose
*  =======
*
*  DLANV2 computes the Schur factorization of a real 2-by-2 nonsymmetric
*  matrix in standard form:
*
*       [ A  B ] = [ CS -SN ] [ AA  BB ] [ CS  SN ]
*       [ C  D ]   [ SN  CS ] [ CC  DD ] [-SN  CS ]
*
*  where either
*  1) CC = 0 so that AA and DD are real eigenvalues of the matrix, or
*  2) AA = DD and BB*CC < 0, so that AA + or - sqrt(BB*CC) are complex
*  conjugate eigenvalues.
*
*  Arguments
*  =========
*
*  A       (input/output) DOUBLE PRECISION
*  B       (input/output) DOUBLE PRECISION
*  C       (input/output) DOUBLE PRECISION
*  D       (input/output) DOUBLE PRECISION
*          On entry, the elements of the input matrix.
*          On exit, they are overwritten by the elements of the
*          standardised Schur form.
*
*  RT1R    (output) DOUBLE PRECISION
*  RT1I    (output) DOUBLE PRECISION
*  RT2R    (output) DOUBLE PRECISION
*  RT2I    (output) DOUBLE PRECISION
*          The real and imaginary parts of the eigenvalues. If the
*          eigenvalues are a complex conjugate pair, RT1I > 0.
*
*  CS      (output) DOUBLE PRECISION
*  SN      (output) DOUBLE PRECISION
*          Parameters of the rotation matrix.
*
*  Further Details
*  ===============
*
*  Modified by V. Sima, Research Institute for Informatics, Bucharest,
*  Romania, to reduce the risk of cancellation errors,
*  when computing real eigenvalues, and to ensure, if possible, that
*  abs(RT1R) >= abs(RT2R).
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, HALF, ONE
      PARAMETER          ( ZERO = 0.0D+0, HALF = 0.5D+0, ONE = 1.0D+0 )
      DOUBLE PRECISION   MULTPL
      PARAMETER          ( MULTPL = 4.0D+0 )
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION   AA, BB, BCMAX, BCMIS, CC, CS1, DD, EPS, P, SAB,
     $                   SAC, SCALE, SIGMA, SN1, TAU, TEMP, Z
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2
      EXTERNAL           DLAMCH, DLAPY2
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SIGN, SQRT
*     ..
*     .. Executable Statements ..
*
      EPS = DLAMCH( 'P' )
      IF( C.EQ.ZERO ) THEN
         CS = ONE
         SN = ZERO
         GO TO 10
*
      ELSE IF( B.EQ.ZERO ) THEN
*
*        Swap rows and columns
*
         CS = ZERO
         SN = ONE
         TEMP = D
         D = A
         A = TEMP
         B = -C
         C = ZERO
         GO TO 10
      ELSE IF( ( A-D ).EQ.ZERO .AND. SIGN( ONE, B ).NE.SIGN( ONE, C ) )
     $          THEN
         CS = ONE
         SN = ZERO
         GO TO 10
      ELSE
*
         TEMP = A - D
         P = HALF*TEMP
         BCMAX = MAX( ABS( B ), ABS( C ) )
         BCMIS = MIN( ABS( B ), ABS( C ) )*SIGN( ONE, B )*SIGN( ONE, C )
         SCALE = MAX( ABS( P ), BCMAX )
         Z = ( P / SCALE )*P + ( BCMAX / SCALE )*BCMIS
*
*        If Z is of the order of the machine accuracy, postpone the
*        decision on the nature of eigenvalues
*
         IF( Z.GE.MULTPL*EPS ) THEN
*
*           Real eigenvalues. Compute A and D.
*
            Z = P + SIGN( SQRT( SCALE )*SQRT( Z ), P )
            A = D + Z
            D = D - ( BCMAX / Z )*BCMIS
*
*           Compute B and the rotation matrix
*
            TAU = DLAPY2( C, Z )
            CS = Z / TAU
            SN = C / TAU
            B = B - C
            C = ZERO
         ELSE
*
*           Complex eigenvalues, or real (almost) equal eigenvalues.
*           Make diagonal elements equal.
*
            SIGMA = B + C
            TAU = DLAPY2( SIGMA, TEMP )
            CS = SQRT( HALF*( ONE+ABS( SIGMA ) / TAU ) )
            SN = -( P / ( TAU*CS ) )*SIGN( ONE, SIGMA )
*
*           Compute [ AA  BB ] = [ A  B ] [ CS -SN ]
*                   [ CC  DD ]   [ C  D ] [ SN  CS ]
*
            AA = A*CS + B*SN
            BB = -A*SN + B*CS
            CC = C*CS + D*SN
            DD = -C*SN + D*CS
*
*           Compute [ A  B ] = [ CS  SN ] [ AA  BB ]
*                   [ C  D ]   [-SN  CS ] [ CC  DD ]
*
            A = AA*CS + CC*SN
            B = BB*CS + DD*SN
            C = -AA*SN + CC*CS
            D = -BB*SN + DD*CS
*
            TEMP = HALF*( A+D )
            A = TEMP
            D = TEMP
*
            IF( C.NE.ZERO ) THEN
               IF( B.NE.ZERO ) THEN
                  IF( SIGN( ONE, B ).EQ.SIGN( ONE, C ) ) THEN
*
*                    Real eigenvalues: reduce to upper triangular form
*
                     SAB = SQRT( ABS( B ) )
                     SAC = SQRT( ABS( C ) )
                     P = SIGN( SAB*SAC, C )
                     TAU = ONE / SQRT( ABS( B+C ) )
                     A = TEMP + P
                     D = TEMP - P
                     B = B - C
                     C = ZERO
                     CS1 = SAB*TAU
                     SN1 = SAC*TAU
                     TEMP = CS*CS1 - SN*SN1
                     SN = CS*SN1 + SN*CS1
                     CS = TEMP
                  END IF
               ELSE
                  B = -C
                  C = ZERO
                  TEMP = CS
                  CS = -SN
                  SN = TEMP
               END IF
            END IF
         END IF
*
      END IF
*
   10 CONTINUE
*
*     Store eigenvalues in (RT1R,RT1I) and (RT2R,RT2I).
*
      RT1R = A
      RT2R = D
      IF( C.EQ.ZERO ) THEN
         RT1I = ZERO
         RT2I = ZERO
      ELSE
         RT1I = SQRT( ABS( B ) )*SQRT( ABS( C ) )
         RT2I = -RT1I
      END IF
      RETURN
*
*     End of DLANV2
*
      END
      SUBROUTINE DPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, KD, LDAB, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DPBSV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric positive definite band matrix and X
*  and B are N-by-NRHS matrices.
*
*  The Cholesky decomposition is used to factor A as
*     A = U**T * U,  if UPLO = 'U', or
*     A = L * L**T,  if UPLO = 'L',
*  where U is an upper triangular band matrix, and L is a lower
*  triangular band matrix, with the same number of superdiagonals or
*  subdiagonals as A.  The factored form of A is then used to solve the
*  system of equations A * X = B.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  KD      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first KD+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(KD+1+i-j,j) = A(i,j) for max(1,j-KD)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(N,j+KD).
*          See below for further details.
*
*          On exit, if INFO = 0, the triangular factor U or L from the
*          Cholesky factorization A = U**T*U or A = L*L**T of the band
*          matrix A, in the same storage format as A.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD+1.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the leading minor of order i of A is not
*                positive definite, so the factorization could not be
*                completed, and the solution has not been computed.
*
*  Further Details
*  ===============
*
*  The band storage scheme is illustrated by the following example, when
*  N = 6, KD = 2, and UPLO = 'U':
*
*  On entry:                       On exit:
*
*      *    *   a13  a24  a35  a46      *    *   u13  u24  u35  u46
*      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
*     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
*
*  Similarly, if UPLO = 'L' the format of A is as follows:
*
*  On entry:                       On exit:
*
*     a11  a22  a33  a44  a55  a66     l11  l22  l33  l44  l55  l66
*     a21  a32  a43  a54  a65   *      l21  l32  l43  l54  l65   *
*     a31  a42  a53  a64   *    *      l31  l42  l53  l64   *    *
*
*  Array elements marked * are not used by the routine.
*
*  =====================================================================
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPBTRF, DPBTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( KD.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDAB.LT.KD+1 ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPBSV ', -INFO )
         RETURN
      END IF
*
*     Compute the Cholesky factorization A = U'*U or A = L*L'.
*
      CALL DPBTRF( UPLO, N, KD, AB, LDAB, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL DPBTRS( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )
*
      END IF
      RETURN
*
*     End of DPBSV
*
      END
      SUBROUTINE DPBSVX( FACT, UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB,
     $                   EQUED, S, B, LDB, X, LDX, RCOND, FERR, BERR,
     $                   WORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED, FACT, UPLO
      INTEGER            INFO, KD, LDAB, LDAFB, LDB, LDX, N, NRHS
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   AB( LDAB, * ), AFB( LDAFB, * ), B( LDB, * ),
     $                   BERR( * ), FERR( * ), S( * ), WORK( * ),
     $                   X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DPBSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
*  compute the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric positive definite band matrix and X
*  and B are N-by-NRHS matrices.
*
*  Error bounds on the solution and a condition estimate are also
*  provided.
*
*  Description
*  ===========
*
*  The following steps are performed:
*
*  1. If FACT = 'E', real scaling factors are computed to equilibrate
*     the system:
*        diag(S) * A * diag(S) * inv(diag(S)) * X = diag(S) * B
*     Whether or not the system will be equilibrated depends on the
*     scaling of the matrix A, but if equilibration is used, A is
*     overwritten by diag(S)*A*diag(S) and B by diag(S)*B.
*
*  2. If FACT = 'N' or 'E', the Cholesky decomposition is used to
*     factor the matrix A (after equilibration if FACT = 'E') as
*        A = U**T * U,  if UPLO = 'U', or
*        A = L * L**T,  if UPLO = 'L',
*     where U is an upper triangular band matrix, and L is a lower
*     triangular band matrix.
*
*  3. If the leading i-by-i principal minor is not positive definite,
*     then the routine returns with INFO = i. Otherwise, the factored
*     form of A is used to estimate the condition number of the matrix
*     A.  If the reciprocal of the condition number is less than machine
*     precision, INFO = N+1 is returned as a warning, but the routine
*     still goes on to solve for X and compute error bounds as
*     described below.
*
*  4. The system of equations is solved for X using the factored form
*     of A.
*
*  5. Iterative refinement is applied to improve the computed solution
*     matrix and calculate error bounds and backward error estimates
*     for it.
*
*  6. If equilibration was used, the matrix X is premultiplied by
*     diag(S) so that it solves the original system before
*     equilibration.
*
*  Arguments
*  =========
*
*  FACT    (input) CHARACTER*1
*          Specifies whether or not the factored form of the matrix A is
*          supplied on entry, and if not, whether the matrix A should be
*          equilibrated before it is factored.
*          = 'F':  On entry, AFB contains the factored form of A.
*                  If EQUED = 'Y', the matrix A has been equilibrated
*                  with scaling factors given by S.  AB and AFB will not
*                  be modified.
*          = 'N':  The matrix A will be copied to AFB and factored.
*          = 'E':  The matrix A will be equilibrated if necessary, then
*                  copied to AFB and factored.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  KD      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right-hand sides, i.e., the number of columns
*          of the matrices B and X.  NRHS >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first KD+1 rows of the array, except
*          if FACT = 'F' and EQUED = 'Y', then A must contain the
*          equilibrated matrix diag(S)*A*diag(S).  The j-th column of A
*          is stored in the j-th column of the array AB as follows:
*          if UPLO = 'U', AB(KD+1+i-j,j) = A(i,j) for max(1,j-KD)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(N,j+KD).
*          See below for further details.
*
*          On exit, if FACT = 'E' and EQUED = 'Y', A is overwritten by
*          diag(S)*A*diag(S).
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array A.  LDAB >= KD+1.
*
*  AFB     (input or output) DOUBLE PRECISION array, dimension (LDAFB,N)
*          If FACT = 'F', then AFB is an input argument and on entry
*          contains the triangular factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T of the band matrix
*          A, in the same storage format as A (see AB).  If EQUED = 'Y',
*          then AFB is the factored form of the equilibrated matrix A.
*
*          If FACT = 'N', then AFB is an output argument and on exit
*          returns the triangular factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T.
*
*          If FACT = 'E', then AFB is an output argument and on exit
*          returns the triangular factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T of the equilibrated
*          matrix A (see the description of A for the form of the
*          equilibrated matrix).
*
*  LDAFB   (input) INTEGER
*          The leading dimension of the array AFB.  LDAFB >= KD+1.
*
*  EQUED   (input or output) CHARACTER*1
*          Specifies the form of equilibration that was done.
*          = 'N':  No equilibration (always true if FACT = 'N').
*          = 'Y':  Equilibration was done, i.e., A has been replaced by
*                  diag(S) * A * diag(S).
*          EQUED is an input argument if FACT = 'F'; otherwise, it is an
*          output argument.
*
*  S       (input or output) DOUBLE PRECISION array, dimension (N)
*          The scale factors for A; not accessed if EQUED = 'N'.  S is
*          an input argument if FACT = 'F'; otherwise, S is an output
*          argument.  If FACT = 'F' and EQUED = 'Y', each element of S
*          must be positive.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit, if EQUED = 'N', B is not modified; if EQUED = 'Y',
*          B is overwritten by diag(S) * B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X to
*          the original system of equations.  Note that if EQUED = 'Y',
*          A and B are modified on exit, and the solution to the
*          equilibrated system is inv(diag(S))*X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  RCOND   (output) DOUBLE PRECISION
*          The estimate of the reciprocal condition number of the matrix
*          A after equilibration (if done).  If RCOND is less than the
*          machine precision (in particular, if RCOND = 0), the matrix
*          is singular to working precision.  This condition is
*          indicated by a return code of INFO > 0.
*
*  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bound for each solution vector
*          X(j) (the j-th column of the solution matrix X).
*          If XTRUE is the true solution corresponding to X(j), FERR(j)
*          is an estimated upper bound for the magnitude of the largest
*          element in (X(j) - XTRUE) divided by the magnitude of the
*          largest element in X(j).  The estimate is as reliable as
*          the estimate for RCOND, and is almost always a slight
*          overestimate of the true error.
*
*  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector X(j) (i.e., the smallest relative change in
*          any element of A or B that makes X(j) an exact solution).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
*
*  IWORK   (workspace) INTEGER array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, and i is
*                <= N:  the leading minor of order i of A is
*                       not positive definite, so the factorization
*                       could not be completed, and the solution has not
*                       been computed. RCOND = 0 is returned.
*                = N+1: U is nonsingular, but RCOND is less than machine
*                       precision, meaning that the matrix is singular
*                       to working precision.  Nevertheless, the
*                       solution and error bounds are computed because
*                       there are a number of situations where the
*                       computed solution can be more accurate than the
*                       value of RCOND would suggest.
*
*  Further Details
*  ===============
*
*  The band storage scheme is illustrated by the following example, when
*  N = 6, KD = 2, and UPLO = 'U':
*
*  Two-dimensional storage of the symmetric matrix A:
*
*     a11  a12  a13
*          a22  a23  a24
*               a33  a34  a35
*                    a44  a45  a46
*                         a55  a56
*     (aij=conjg(aji))         a66
*
*  Band storage of the upper triangle of A:
*
*      *    *   a13  a24  a35  a46
*      *   a12  a23  a34  a45  a56
*     a11  a22  a33  a44  a55  a66
*
*  Similarly, if UPLO = 'L' the format of A is as follows:
*
*     a11  a22  a33  a44  a55  a66
*     a21  a32  a43  a54  a65   *
*     a31  a42  a53  a64   *    *
*
*  Array elements marked * are not used by the routine.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            EQUIL, NOFACT, RCEQU, UPPER
      INTEGER            I, INFEQU, J, J1, J2
      DOUBLE PRECISION   AMAX, ANORM, BIGNUM, SCOND, SMAX, SMIN, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSB
      EXTERNAL           LSAME, DLAMCH, DLANSB
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DLAQSB, DPBCON, DPBEQU, DPBRFS,
     $                   DPBTRF, DPBTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      NOFACT = LSAME( FACT, 'N' )
      EQUIL = LSAME( FACT, 'E' )
      UPPER = LSAME( UPLO, 'U' )
      IF( NOFACT .OR. EQUIL ) THEN
         EQUED = 'N'
         RCEQU = .FALSE.
      ELSE
         RCEQU = LSAME( EQUED, 'Y' )
         SMLNUM = DLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
      END IF
*
*     Test the input parameters.
*
      IF( .NOT.NOFACT .AND. .NOT.EQUIL .AND. .NOT.LSAME( FACT, 'F' ) )
     $     THEN
         INFO = -1
      ELSE IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( KD.LT.0 ) THEN
         INFO = -4
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.KD+1 ) THEN
         INFO = -7
      ELSE IF( LDAFB.LT.KD+1 ) THEN
         INFO = -9
      ELSE IF( LSAME( FACT, 'F' ) .AND. .NOT.
     $         ( RCEQU .OR. LSAME( EQUED, 'N' ) ) ) THEN
         INFO = -10
      ELSE
         IF( RCEQU ) THEN
            SMIN = BIGNUM
            SMAX = ZERO
            DO 10 J = 1, N
               SMIN = MIN( SMIN, S( J ) )
               SMAX = MAX( SMAX, S( J ) )
   10       CONTINUE
            IF( SMIN.LE.ZERO ) THEN
               INFO = -11
            ELSE IF( N.GT.0 ) THEN
               SCOND = MAX( SMIN, SMLNUM ) / MIN( SMAX, BIGNUM )
            ELSE
               SCOND = ONE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            IF( LDB.LT.MAX( 1, N ) ) THEN
               INFO = -13
            ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
               INFO = -15
            END IF
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPBSVX', -INFO )
         RETURN
      END IF
*
      IF( EQUIL ) THEN
*
*        Compute row and column scalings to equilibrate the matrix A.
*
         CALL DPBEQU( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, INFEQU )
         IF( INFEQU.EQ.0 ) THEN
*
*           Equilibrate the matrix.
*
            CALL DLAQSB( UPLO, N, KD, AB, LDAB, S, SCOND, AMAX, EQUED )
            RCEQU = LSAME( EQUED, 'Y' )
         END IF
      END IF
*
*     Scale the right-hand side.
*
      IF( RCEQU ) THEN
         DO 30 J = 1, NRHS
            DO 20 I = 1, N
               B( I, J ) = S( I )*B( I, J )
   20       CONTINUE
   30    CONTINUE
      END IF
*
      IF( NOFACT .OR. EQUIL ) THEN
*
*        Compute the Cholesky factorization A = U'*U or A = L*L'.
*
         IF( UPPER ) THEN
            DO 40 J = 1, N
               J1 = MAX( J-KD, 1 )
               CALL DCOPY( J-J1+1, AB( KD+1-J+J1, J ), 1,
     $                     AFB( KD+1-J+J1, J ), 1 )
   40       CONTINUE
         ELSE
            DO 50 J = 1, N
               J2 = MIN( J+KD, N )
               CALL DCOPY( J2-J+1, AB( 1, J ), 1, AFB( 1, J ), 1 )
   50       CONTINUE
         END IF
*
         CALL DPBTRF( UPLO, N, KD, AFB, LDAFB, INFO )
*
*        Return if INFO is non-zero.
*
         IF( INFO.GT.0 )THEN
            RCOND = ZERO
            RETURN
         END IF
      END IF
*
*     Compute the norm of the matrix A.
*
      ANORM = DLANSB( '1', UPLO, N, KD, AB, LDAB, WORK )
*
*     Compute the reciprocal of the condition number of A.
*
      CALL DPBCON( UPLO, N, KD, AFB, LDAFB, ANORM, RCOND, WORK, IWORK,
     $             INFO )
*
*     Compute the solution matrix X.
*
      CALL DLACPY( 'Full', N, NRHS, B, LDB, X, LDX )
      CALL DPBTRS( UPLO, N, KD, NRHS, AFB, LDAFB, X, LDX, INFO )
*
*     Use iterative refinement to improve the computed solution and
*     compute error bounds and backward error estimates for it.
*
      CALL DPBRFS( UPLO, N, KD, NRHS, AB, LDAB, AFB, LDAFB, B, LDB, X,
     $             LDX, FERR, BERR, WORK, IWORK, INFO )
*
*     Transform the solution matrix X to a solution of the original
*     system.
*
      IF( RCEQU ) THEN
         DO 70 J = 1, NRHS
            DO 60 I = 1, N
               X( I, J ) = S( I )*X( I, J )
   60       CONTINUE
   70    CONTINUE
         DO 80 J = 1, NRHS
            FERR( J ) = FERR( J ) / SCOND
   80    CONTINUE
      END IF
*
*     Set INFO = N+1 if the matrix is singular to working precision.
*
      IF( RCOND.LT.DLAMCH( 'Epsilon' ) )
     $   INFO = N + 1
*
      RETURN
*
*     End of DPBSVX
*
      END
      SUBROUTINE DPOSV( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DPOSV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric positive definite matrix and X and B
*  are N-by-NRHS matrices.
*
*  The Cholesky decomposition is used to factor A as
*     A = U**T* U,  if UPLO = 'U', or
*     A = L * L**T,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is a lower triangular
*  matrix.  The factored form of A is then used to solve the system of
*  equations A * X = B.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
*          N-by-N upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading N-by-N lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*
*          On exit, if INFO = 0, the factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the leading minor of order i of A is not
*                positive definite, so the factorization could not be
*                completed, and the solution has not been computed.
*
*  =====================================================================
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPOTRF, DPOTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPOSV ', -INFO )
         RETURN
      END IF
*
*     Compute the Cholesky factorization A = U'*U or A = L*L'.
*
      CALL DPOTRF( UPLO, N, A, LDA, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL DPOTRS( UPLO, N, NRHS, A, LDA, B, LDB, INFO )
*
      END IF
      RETURN
*
*     End of DPOSV
*
      END
      SUBROUTINE DPOSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, EQUED,
     $                   S, B, LDB, X, LDX, RCOND, FERR, BERR, WORK,
     $                   IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED, FACT, UPLO
      INTEGER            INFO, LDA, LDAF, LDB, LDX, N, NRHS
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
     $                   BERR( * ), FERR( * ), S( * ), WORK( * ),
     $                   X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DPOSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
*  compute the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric positive definite matrix and X and B
*  are N-by-NRHS matrices.
*
*  Error bounds on the solution and a condition estimate are also
*  provided.
*
*  Description
*  ===========
*
*  The following steps are performed:
*
*  1. If FACT = 'E', real scaling factors are computed to equilibrate
*     the system:
*        diag(S) * A * diag(S) * inv(diag(S)) * X = diag(S) * B
*     Whether or not the system will be equilibrated depends on the
*     scaling of the matrix A, but if equilibration is used, A is
*     overwritten by diag(S)*A*diag(S) and B by diag(S)*B.
*
*  2. If FACT = 'N' or 'E', the Cholesky decomposition is used to
*     factor the matrix A (after equilibration if FACT = 'E') as
*        A = U**T* U,  if UPLO = 'U', or
*        A = L * L**T,  if UPLO = 'L',
*     where U is an upper triangular matrix and L is a lower triangular
*     matrix.
*
*  3. If the leading i-by-i principal minor is not positive definite,
*     then the routine returns with INFO = i. Otherwise, the factored
*     form of A is used to estimate the condition number of the matrix
*     A.  If the reciprocal of the condition number is less than machine
*     precision, INFO = N+1 is returned as a warning, but the routine
*     still goes on to solve for X and compute error bounds as
*     described below.
*
*  4. The system of equations is solved for X using the factored form
*     of A.
*
*  5. Iterative refinement is applied to improve the computed solution
*     matrix and calculate error bounds and backward error estimates
*     for it.
*
*  6. If equilibration was used, the matrix X is premultiplied by
*     diag(S) so that it solves the original system before
*     equilibration.
*
*  Arguments
*  =========
*
*  FACT    (input) CHARACTER*1
*          Specifies whether or not the factored form of the matrix A is
*          supplied on entry, and if not, whether the matrix A should be
*          equilibrated before it is factored.
*          = 'F':  On entry, AF contains the factored form of A.
*                  If EQUED = 'Y', the matrix A has been equilibrated
*                  with scaling factors given by S.  A and AF will not
*                  be modified.
*          = 'N':  The matrix A will be copied to AF and factored.
*          = 'E':  The matrix A will be equilibrated if necessary, then
*                  copied to AF and factored.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X.  NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the symmetric matrix A, except if FACT = 'F' and
*          EQUED = 'Y', then A must contain the equilibrated matrix
*          diag(S)*A*diag(S).  If UPLO = 'U', the leading
*          N-by-N upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading N-by-N lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.  A is not modified if
*          FACT = 'F' or 'N', or if FACT = 'E' and EQUED = 'N' on exit.
*
*          On exit, if FACT = 'E' and EQUED = 'Y', A is overwritten by
*          diag(S)*A*diag(S).
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  AF      (input or output) DOUBLE PRECISION array, dimension (LDAF,N)
*          If FACT = 'F', then AF is an input argument and on entry
*          contains the triangular factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T, in the same storage
*          format as A.  If EQUED .ne. 'N', then AF is the factored form
*          of the equilibrated matrix diag(S)*A*diag(S).
*
*          If FACT = 'N', then AF is an output argument and on exit
*          returns the triangular factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T of the original
*          matrix A.
*
*          If FACT = 'E', then AF is an output argument and on exit
*          returns the triangular factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T of the equilibrated
*          matrix A (see the description of A for the form of the
*          equilibrated matrix).
*
*  LDAF    (input) INTEGER
*          The leading dimension of the array AF.  LDAF >= max(1,N).
*
*  EQUED   (input or output) CHARACTER*1
*          Specifies the form of equilibration that was done.
*          = 'N':  No equilibration (always true if FACT = 'N').
*          = 'Y':  Equilibration was done, i.e., A has been replaced by
*                  diag(S) * A * diag(S).
*          EQUED is an input argument if FACT = 'F'; otherwise, it is an
*          output argument.
*
*  S       (input or output) DOUBLE PRECISION array, dimension (N)
*          The scale factors for A; not accessed if EQUED = 'N'.  S is
*          an input argument if FACT = 'F'; otherwise, S is an output
*          argument.  If FACT = 'F' and EQUED = 'Y', each element of S
*          must be positive.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit, if EQUED = 'N', B is not modified; if EQUED = 'Y',
*          B is overwritten by diag(S) * B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X to
*          the original system of equations.  Note that if EQUED = 'Y',
*          A and B are modified on exit, and the solution to the
*          equilibrated system is inv(diag(S))*X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  RCOND   (output) DOUBLE PRECISION
*          The estimate of the reciprocal condition number of the matrix
*          A after equilibration (if done).  If RCOND is less than the
*          machine precision (in particular, if RCOND = 0), the matrix
*          is singular to working precision.  This condition is
*          indicated by a return code of INFO > 0.
*
*  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bound for each solution vector
*          X(j) (the j-th column of the solution matrix X).
*          If XTRUE is the true solution corresponding to X(j), FERR(j)
*          is an estimated upper bound for the magnitude of the largest
*          element in (X(j) - XTRUE) divided by the magnitude of the
*          largest element in X(j).  The estimate is as reliable as
*          the estimate for RCOND, and is almost always a slight
*          overestimate of the true error.
*
*  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector X(j) (i.e., the smallest relative change in
*          any element of A or B that makes X(j) an exact solution).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
*
*  IWORK   (workspace) INTEGER array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          > 0: if INFO = i, and i is
*                <= N:  the leading minor of order i of A is
*                       not positive definite, so the factorization
*                       could not be completed, and the solution has not
*                       been computed. RCOND = 0 is returned.
*                = N+1: U is nonsingular, but RCOND is less than machine
*                       precision, meaning that the matrix is singular
*                       to working precision.  Nevertheless, the
*                       solution and error bounds are computed because
*                       there are a number of situations where the
*                       computed solution can be more accurate than the
*                       value of RCOND would suggest.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            EQUIL, NOFACT, RCEQU
      INTEGER            I, INFEQU, J
      DOUBLE PRECISION   AMAX, ANORM, BIGNUM, SCOND, SMAX, SMIN, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           LSAME, DLAMCH, DLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLACPY, DLAQSY, DPOCON, DPOEQU, DPORFS, DPOTRF,
     $                   DPOTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      NOFACT = LSAME( FACT, 'N' )
      EQUIL = LSAME( FACT, 'E' )
      IF( NOFACT .OR. EQUIL ) THEN
         EQUED = 'N'
         RCEQU = .FALSE.
      ELSE
         RCEQU = LSAME( EQUED, 'Y' )
         SMLNUM = DLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
      END IF
*
*     Test the input parameters.
*
      IF( .NOT.NOFACT .AND. .NOT.EQUIL .AND. .NOT.LSAME( FACT, 'F' ) )
     $     THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) )
     $          THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDAF.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LSAME( FACT, 'F' ) .AND. .NOT.
     $         ( RCEQU .OR. LSAME( EQUED, 'N' ) ) ) THEN
         INFO = -9
      ELSE
         IF( RCEQU ) THEN
            SMIN = BIGNUM
            SMAX = ZERO
            DO 10 J = 1, N
               SMIN = MIN( SMIN, S( J ) )
               SMAX = MAX( SMAX, S( J ) )
   10       CONTINUE
            IF( SMIN.LE.ZERO ) THEN
               INFO = -10
            ELSE IF( N.GT.0 ) THEN
               SCOND = MAX( SMIN, SMLNUM ) / MIN( SMAX, BIGNUM )
            ELSE
               SCOND = ONE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            IF( LDB.LT.MAX( 1, N ) ) THEN
               INFO = -12
            ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
               INFO = -14
            END IF
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPOSVX', -INFO )
         RETURN
      END IF
*
      IF( EQUIL ) THEN
*
*        Compute row and column scalings to equilibrate the matrix A.
*
         CALL DPOEQU( N, A, LDA, S, SCOND, AMAX, INFEQU )
         IF( INFEQU.EQ.0 ) THEN
*
*           Equilibrate the matrix.
*
            CALL DLAQSY( UPLO, N, A, LDA, S, SCOND, AMAX, EQUED )
            RCEQU = LSAME( EQUED, 'Y' )
         END IF
      END IF
*
*     Scale the right hand side.
*
      IF( RCEQU ) THEN
         DO 30 J = 1, NRHS
            DO 20 I = 1, N
               B( I, J ) = S( I )*B( I, J )
   20       CONTINUE
   30    CONTINUE
      END IF
*
      IF( NOFACT .OR. EQUIL ) THEN
*
*        Compute the Cholesky factorization A = U'*U or A = L*L'.
*
         CALL DLACPY( UPLO, N, N, A, LDA, AF, LDAF )
         CALL DPOTRF( UPLO, N, AF, LDAF, INFO )
*
*        Return if INFO is non-zero.
*
         IF( INFO.GT.0 )THEN
            RCOND = ZERO
            RETURN
         END IF
      END IF
*
*     Compute the norm of the matrix A.
*
      ANORM = DLANSY( '1', UPLO, N, A, LDA, WORK )
*
*     Compute the reciprocal of the condition number of A.
*
      CALL DPOCON( UPLO, N, AF, LDAF, ANORM, RCOND, WORK, IWORK, INFO )
*
*     Compute the solution matrix X.
*
      CALL DLACPY( 'Full', N, NRHS, B, LDB, X, LDX )
      CALL DPOTRS( UPLO, N, NRHS, AF, LDAF, X, LDX, INFO )
*
*     Use iterative refinement to improve the computed solution and
*     compute error bounds and backward error estimates for it.
*
      CALL DPORFS( UPLO, N, NRHS, A, LDA, AF, LDAF, B, LDB, X, LDX,
     $             FERR, BERR, WORK, IWORK, INFO )
*
*     Transform the solution matrix X to a solution of the original
*     system.
*
      IF( RCEQU ) THEN
         DO 50 J = 1, NRHS
            DO 40 I = 1, N
               X( I, J ) = S( I )*X( I, J )
   40       CONTINUE
   50    CONTINUE
         DO 60 J = 1, NRHS
            FERR( J ) = FERR( J ) / SCOND
   60    CONTINUE
      END IF
*
*     Set INFO = N+1 if the matrix is singular to working precision.
*
      IF( RCOND.LT.DLAMCH( 'Epsilon' ) )
     $   INFO = N + 1
*
      RETURN
*
*     End of DPOSVX
*
      END
      SUBROUTINE DPPSV( UPLO, N, NRHS, AP, B, LDB, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AP( * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DPPSV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric positive definite matrix stored in
*  packed format and X and B are N-by-NRHS matrices.
*
*  The Cholesky decomposition is used to factor A as
*     A = U**T* U,  if UPLO = 'U', or
*     A = L * L**T,  if UPLO = 'L',
*  where U is an upper triangular matrix and L is a lower triangular
*  matrix.  The factored form of A is then used to solve the system of
*  equations A * X = B.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*          See below for further details.
*
*          On exit, if INFO = 0, the factor U or L from the Cholesky
*          factorization A = U**T*U or A = L*L**T, in the same storage
*          format as A.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the leading minor of order i of A is not
*                positive definite, so the factorization could not be
*                completed, and the solution has not been computed.
*
*  Further Details
*  ===============
*
*  The packed storage scheme is illustrated by the following example
*  when N = 4, UPLO = 'U':
*
*  Two-dimensional storage of the symmetric matrix A:
*
*     a11 a12 a13 a14
*         a22 a23 a24
*             a33 a34     (aij = conjg(aji))
*                 a44
*
*  Packed storage of the upper triangle of A:
*
*  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
*
*  =====================================================================
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPPTRF, DPPTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -6
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPPSV ', -INFO )
         RETURN
      END IF
*
*     Compute the Cholesky factorization A = U'*U or A = L*L'.
*
      CALL DPPTRF( UPLO, N, AP, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL DPPTRS( UPLO, N, NRHS, AP, B, LDB, INFO )
*
      END IF
      RETURN
*
*     End of DPPSV
*
      END
      SUBROUTINE DPPSVX( FACT, UPLO, N, NRHS, AP, AFP, EQUED, S, B, LDB,
     $                   X, LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          EQUED, FACT, UPLO
      INTEGER            INFO, LDB, LDX, N, NRHS
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   AFP( * ), AP( * ), B( LDB, * ), BERR( * ),
     $                   FERR( * ), S( * ), WORK( * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DPPSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
*  compute the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric positive definite matrix stored in
*  packed format and X and B are N-by-NRHS matrices.
*
*  Error bounds on the solution and a condition estimate are also
*  provided.
*
*  Description
*  ===========
*
*  The following steps are performed:
*
*  1. If FACT = 'E', real scaling factors are computed to equilibrate
*     the system:
*        diag(S) * A * diag(S) * inv(diag(S)) * X = diag(S) * B
*     Whether or not the system will be equilibrated depends on the
*     scaling of the matrix A, but if equilibration is used, A is
*     overwritten by diag(S)*A*diag(S) and B by diag(S)*B.
*
*  2. If FACT = 'N' or 'E', the Cholesky decomposition is used to
*     factor the matrix A (after equilibration if FACT = 'E') as
*        A = U**T* U,  if UPLO = 'U', or
*        A = L * L**T,  if UPLO = 'L',
*     where U is an upper triangular matrix and L is a lower triangular
*     matrix.
*
*  3. If the leading i-by-i principal minor is not positive definite,
*     then the routine returns with INFO = i. Otherwise, the factored
*     form of A is used to estimate the condition number of the matrix
*     A.  If the reciprocal of the condition number is less than machine
*     precision, INFO = N+1 is returned as a warning, but the routine
*     still goes on to solve for X and compute error bounds as
*     described below.
*
*  4. The system of equations is solved for X using the factored form
*     of A.
*
*  5. Iterative refinement is applied to improve the computed solution
*     matrix and calculate error bounds and backward error estimates
*     for it.
*
*  6. If equilibration was used, the matrix X is premultiplied by
*     diag(S) so that it solves the original system before
*     equilibration.
*
*  Arguments
*  =========
*
*  FACT    (input) CHARACTER*1
*          Specifies whether or not the factored form of the matrix A is
*          supplied on entry, and if not, whether the matrix A should be
*          equilibrated before it is factored.
*          = 'F':  On entry, AFP contains the factored form of A.
*                  If EQUED = 'Y', the matrix A has been equilibrated
*                  with scaling factors given by S.  AP and AFP will not
*                  be modified.
*          = 'N':  The matrix A will be copied to AFP and factored.
*          = 'E':  The matrix A will be equilibrated if necessary, then
*                  copied to AFP and factored.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X.  NRHS >= 0.
*
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array, except if FACT = 'F'
*          and EQUED = 'Y', then A must contain the equilibrated matrix
*          diag(S)*A*diag(S).  The j-th column of A is stored in the
*          array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*          See below for further details.  A is not modified if
*          FACT = 'F' or 'N', or if FACT = 'E' and EQUED = 'N' on exit.
*
*          On exit, if FACT = 'E' and EQUED = 'Y', A is overwritten by
*          diag(S)*A*diag(S).
*
*  AFP     (input or output) DOUBLE PRECISION array, dimension
*                            (N*(N+1)/2)
*          If FACT = 'F', then AFP is an input argument and on entry
*          contains the triangular factor U or L from the Cholesky
*          factorization A = U'*U or A = L*L', in the same storage
*          format as A.  If EQUED .ne. 'N', then AFP is the factored
*          form of the equilibrated matrix A.
*
*          If FACT = 'N', then AFP is an output argument and on exit
*          returns the triangular factor U or L from the Cholesky
*          factorization A = U'*U or A = L*L' of the original matrix A.
*
*          If FACT = 'E', then AFP is an output argument and on exit
*          returns the triangular factor U or L from the Cholesky
*          factorization A = U'*U or A = L*L' of the equilibrated
*          matrix A (see the description of AP for the form of the
*          equilibrated matrix).
*
*  EQUED   (input or output) CHARACTER*1
*          Specifies the form of equilibration that was done.
*          = 'N':  No equilibration (always true if FACT = 'N').
*          = 'Y':  Equilibration was done, i.e., A has been replaced by
*                  diag(S) * A * diag(S).
*          EQUED is an input argument if FACT = 'F'; otherwise, it is an
*          output argument.
*
*  S       (input or output) DOUBLE PRECISION array, dimension (N)
*          The scale factors for A; not accessed if EQUED = 'N'.  S is
*          an input argument if FACT = 'F'; otherwise, S is an output
*          argument.  If FACT = 'F' and EQUED = 'Y', each element of S
*          must be positive.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit, if EQUED = 'N', B is not modified; if EQUED = 'Y',
*          B is overwritten by diag(S) * B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X to
*          the original system of equations.  Note that if EQUED = 'Y',
*          A and B are modified on exit, and the solution to the
*          equilibrated system is inv(diag(S))*X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  RCOND   (output) DOUBLE PRECISION
*          The estimate of the reciprocal condition number of the matrix
*          A after equilibration (if done).  If RCOND is less than the
*          machine precision (in particular, if RCOND = 0), the matrix
*          is singular to working precision.  This condition is
*          indicated by a return code of INFO > 0.
*
*  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bound for each solution vector
*          X(j) (the j-th column of the solution matrix X).
*          If XTRUE is the true solution corresponding to X(j), FERR(j)
*          is an estimated upper bound for the magnitude of the largest
*          element in (X(j) - XTRUE) divided by the magnitude of the
*          largest element in X(j).  The estimate is as reliable as
*          the estimate for RCOND, and is almost always a slight
*          overestimate of the true error.
*
*  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector X(j) (i.e., the smallest relative change in
*          any element of A or B that makes X(j) an exact solution).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
*
*  IWORK   (workspace) INTEGER array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, and i is
*                <= N:  the leading minor of order i of A is
*                       not positive definite, so the factorization
*                       could not be completed, and the solution has not
*                       been computed. RCOND = 0 is returned.
*                = N+1: U is nonsingular, but RCOND is less than machine
*                       precision, meaning that the matrix is singular
*                       to working precision.  Nevertheless, the
*                       solution and error bounds are computed because
*                       there are a number of situations where the
*                       computed solution can be more accurate than the
*                       value of RCOND would suggest.
*
*  Further Details
*  ===============
*
*  The packed storage scheme is illustrated by the following example
*  when N = 4, UPLO = 'U':
*
*  Two-dimensional storage of the symmetric matrix A:
*
*     a11 a12 a13 a14
*         a22 a23 a24
*             a33 a34     (aij = conjg(aji))
*                 a44
*
*  Packed storage of the upper triangle of A:
*
*  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            EQUIL, NOFACT, RCEQU
      INTEGER            I, INFEQU, J
      DOUBLE PRECISION   AMAX, ANORM, BIGNUM, SCOND, SMAX, SMIN, SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSP
      EXTERNAL           LSAME, DLAMCH, DLANSP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DLAQSP, DPPCON, DPPEQU, DPPRFS,
     $                   DPPTRF, DPPTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
      INFO = 0
      NOFACT = LSAME( FACT, 'N' )
      EQUIL = LSAME( FACT, 'E' )
      IF( NOFACT .OR. EQUIL ) THEN
         EQUED = 'N'
         RCEQU = .FALSE.
      ELSE
         RCEQU = LSAME( EQUED, 'Y' )
         SMLNUM = DLAMCH( 'Safe minimum' )
         BIGNUM = ONE / SMLNUM
      END IF
*
*     Test the input parameters.
*
      IF( .NOT.NOFACT .AND. .NOT.EQUIL .AND. .NOT.LSAME( FACT, 'F' ) )
     $     THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) )
     $          THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LSAME( FACT, 'F' ) .AND. .NOT.
     $         ( RCEQU .OR. LSAME( EQUED, 'N' ) ) ) THEN
         INFO = -7
      ELSE
         IF( RCEQU ) THEN
            SMIN = BIGNUM
            SMAX = ZERO
            DO 10 J = 1, N
               SMIN = MIN( SMIN, S( J ) )
               SMAX = MAX( SMAX, S( J ) )
   10       CONTINUE
            IF( SMIN.LE.ZERO ) THEN
               INFO = -8
            ELSE IF( N.GT.0 ) THEN
               SCOND = MAX( SMIN, SMLNUM ) / MIN( SMAX, BIGNUM )
            ELSE
               SCOND = ONE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            IF( LDB.LT.MAX( 1, N ) ) THEN
               INFO = -10
            ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
               INFO = -12
            END IF
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DPPSVX', -INFO )
         RETURN
      END IF
*
      IF( EQUIL ) THEN
*
*        Compute row and column scalings to equilibrate the matrix A.
*
         CALL DPPEQU( UPLO, N, AP, S, SCOND, AMAX, INFEQU )
         IF( INFEQU.EQ.0 ) THEN
*
*           Equilibrate the matrix.
*
            CALL DLAQSP( UPLO, N, AP, S, SCOND, AMAX, EQUED )
            RCEQU = LSAME( EQUED, 'Y' )
         END IF
      END IF
*
*     Scale the right-hand side.
*
      IF( RCEQU ) THEN
         DO 30 J = 1, NRHS
            DO 20 I = 1, N
               B( I, J ) = S( I )*B( I, J )
   20       CONTINUE
   30    CONTINUE
      END IF
*
      IF( NOFACT .OR. EQUIL ) THEN
*
*        Compute the Cholesky factorization A = U'*U or A = L*L'.
*
         CALL DCOPY( N*( N+1 ) / 2, AP, 1, AFP, 1 )
         CALL DPPTRF( UPLO, N, AFP, INFO )
*
*        Return if INFO is non-zero.
*
         IF( INFO.GT.0 )THEN
            RCOND = ZERO
            RETURN
         END IF
      END IF
*
*     Compute the norm of the matrix A.
*
      ANORM = DLANSP( 'I', UPLO, N, AP, WORK )
*
*     Compute the reciprocal of the condition number of A.
*
      CALL DPPCON( UPLO, N, AFP, ANORM, RCOND, WORK, IWORK, INFO )
*
*     Compute the solution matrix X.
*
      CALL DLACPY( 'Full', N, NRHS, B, LDB, X, LDX )
      CALL DPPTRS( UPLO, N, NRHS, AFP, X, LDX, INFO )
*
*     Use iterative refinement to improve the computed solution and
*     compute error bounds and backward error estimates for it.
*
      CALL DPPRFS( UPLO, N, NRHS, AP, AFP, B, LDB, X, LDX, FERR, BERR,
     $             WORK, IWORK, INFO )
*
*     Transform the solution matrix X to a solution of the original
*     system.
*
      IF( RCEQU ) THEN
         DO 50 J = 1, NRHS
            DO 40 I = 1, N
               X( I, J ) = S( I )*X( I, J )
   40       CONTINUE
   50    CONTINUE
         DO 60 J = 1, NRHS
            FERR( J ) = FERR( J ) / SCOND
   60    CONTINUE
      END IF
*
*     Set INFO = N+1 if the matrix is singular to working precision.
*
      IF( RCOND.LT.DLAMCH( 'Epsilon' ) )
     $   INFO = N + 1
*
      RETURN
*
*     End of DPPSVX
*
      END
      SUBROUTINE DSBEV( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,
     $                  INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, KD, LDAB, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSBEV computes all the eigenvalues and, optionally, eigenvectors of
*  a real symmetric band matrix A.
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
*  KD      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first KD+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*
*          On exit, AB is overwritten by values generated during the
*          reduction to tridiagonal form.  If UPLO = 'U', the first
*          superdiagonal and the diagonal of the tridiagonal matrix T
*          are returned in rows KD and KD+1 of AB, and if UPLO = 'L',
*          the diagonal and first subdiagonal of T are returned in the
*          first two rows of AB.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD + 1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
*          eigenvectors of the matrix A, with the i-th column of Z
*          holding the eigenvector associated with W(i).
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,3*N-2))
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
*     ..
*     .. Local Scalars ..
      LOGICAL            LOWER, WANTZ
      INTEGER            IINFO, IMAX, INDE, INDWRK, ISCALE
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSB
      EXTERNAL           LSAME, DLAMCH, DLANSB
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASCL, DSBTRD, DSCAL, DSTEQR, DSTERF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      LOWER = LSAME( UPLO, 'L' )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( KD.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDAB.LT.KD+1 ) THEN
         INFO = -6
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -9
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSBEV ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( LOWER ) THEN
            W( 1 ) = AB( 1, 1 )
         ELSE
            W( 1 ) = AB( KD+1, 1 )
         END IF
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      ANRM = DLANSB( 'M', UPLO, N, KD, AB, LDAB, WORK )
      ISCALE = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         IF( LOWER ) THEN
            CALL DLASCL( 'B', KD, KD, ONE, SIGMA, N, N, AB, LDAB, INFO )
         ELSE
            CALL DLASCL( 'Q', KD, KD, ONE, SIGMA, N, N, AB, LDAB, INFO )
         END IF
      END IF
*
*     Call DSBTRD to reduce symmetric band matrix to tridiagonal form.
*
      INDE = 1
      INDWRK = INDE + N
      CALL DSBTRD( JOBZ, UPLO, N, KD, AB, LDAB, W, WORK( INDE ), Z, LDZ,
     $             WORK( INDWRK ), IINFO )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, call SSTEQR.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         CALL DSTEQR( JOBZ, N, W, WORK( INDE ), Z, LDZ, WORK( INDWRK ),
     $                INFO )
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
      RETURN
*
*     End of DSBEV
*
      END
      SUBROUTINE DSBEVD( JOBZ, UPLO, N, KD, AB, LDAB, W, Z, LDZ, WORK,
     $                   LWORK, IWORK, LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, KD, LDAB, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   AB( LDAB, * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSBEVD computes all the eigenvalues and, optionally, eigenvectors of
*  a real symmetric band matrix A. If eigenvectors are desired, it uses
*  a divide and conquer algorithm.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
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
*  KD      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first KD+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*
*          On exit, AB is overwritten by values generated during the
*          reduction to tridiagonal form.  If UPLO = 'U', the first
*          superdiagonal and the diagonal of the tridiagonal matrix T
*          are returned in rows KD and KD+1 of AB, and if UPLO = 'L',
*          the diagonal and first subdiagonal of T are returned in the
*          first two rows of AB.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD + 1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
*          eigenvectors of the matrix A, with the i-th column of Z
*          holding the eigenvector associated with W(i).
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array,
*                                         dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          IF N <= 1,                LWORK must be at least 1.
*          If JOBZ  = 'N' and N > 2, LWORK must be at least 2*N.
*          If JOBZ  = 'V' and N > 2, LWORK must be at least
*                         ( 1 + 5*N + 2*N**2 ).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal sizes of the WORK and IWORK
*          arrays, returns these values as the first entries of the WORK
*          and IWORK arrays, and no error message related to LWORK or
*          LIWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array LIWORK.
*          If JOBZ  = 'N' or N <= 1, LIWORK must be at least 1.
*          If JOBZ  = 'V' and N > 2, LIWORK must be at least 3 + 5*N.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal sizes of the WORK and
*          IWORK arrays, returns these values as the first entries of
*          the WORK and IWORK arrays, and no error message related to
*          LWORK or LIWORK is issued by XERBLA.
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
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LOWER, LQUERY, WANTZ
      INTEGER            IINFO, INDE, INDWK2, INDWRK, ISCALE, LIWMIN,
     $                   LLWRK2, LWMIN
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSB
      EXTERNAL           LSAME, DLAMCH, DLANSB
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DLACPY, DLASCL, DSBTRD, DSCAL, DSTEDC,
     $                   DSTERF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      LOWER = LSAME( UPLO, 'L' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
*
      INFO = 0
      IF( N.LE.1 ) THEN
         LIWMIN = 1
         LWMIN = 1
      ELSE
         IF( WANTZ ) THEN
            LIWMIN = 3 + 5*N
            LWMIN = 1 + 5*N + 2*N**2
         ELSE
            LIWMIN = 1
            LWMIN = 2*N
         END IF
      END IF
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( KD.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDAB.LT.KD+1 ) THEN
         INFO = -6
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -9
      END IF
*
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
*
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -11
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -13
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSBEVD', -INFO )
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
      IF( N.EQ.1 ) THEN
         W( 1 ) = AB( 1, 1 )
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      ANRM = DLANSB( 'M', UPLO, N, KD, AB, LDAB, WORK )
      ISCALE = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         IF( LOWER ) THEN
            CALL DLASCL( 'B', KD, KD, ONE, SIGMA, N, N, AB, LDAB, INFO )
         ELSE
            CALL DLASCL( 'Q', KD, KD, ONE, SIGMA, N, N, AB, LDAB, INFO )
         END IF
      END IF
*
*     Call DSBTRD to reduce symmetric band matrix to tridiagonal form.
*
      INDE = 1
      INDWRK = INDE + N
      INDWK2 = INDWRK + N*N
      LLWRK2 = LWORK - INDWK2 + 1
      CALL DSBTRD( JOBZ, UPLO, N, KD, AB, LDAB, W, WORK( INDE ), Z, LDZ,
     $             WORK( INDWRK ), IINFO )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, call SSTEDC.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         CALL DSTEDC( 'I', N, W, WORK( INDE ), WORK( INDWRK ), N,
     $                WORK( INDWK2 ), LLWRK2, IWORK, LIWORK, INFO )
         CALL DGEMM( 'N', 'N', N, N, N, ONE, Z, LDZ, WORK( INDWRK ), N,
     $               ZERO, WORK( INDWK2 ), N )
         CALL DLACPY( 'A', N, N, WORK( INDWK2 ), N, Z, LDZ )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( ISCALE.EQ.1 )
     $   CALL DSCAL( N, ONE / SIGMA, W, 1 )
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
      RETURN
*
*     End of DSBEVD
*
      END
      SUBROUTINE DSBEVX( JOBZ, RANGE, UPLO, N, KD, AB, LDAB, Q, LDQ, VL,
     $                   VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,
     $                   IFAIL, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, IU, KD, LDAB, LDQ, LDZ, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      DOUBLE PRECISION   AB( LDAB, * ), Q( LDQ, * ), W( * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSBEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric band matrix A.  Eigenvalues and eigenvectors can
*  be selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  RANGE   (input) CHARACTER*1
*          = 'A': all eigenvalues will be found;
*          = 'V': all eigenvalues in the half-open interval (VL,VU]
*                 will be found;
*          = 'I': the IL-th through IU-th eigenvalues will be found.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  KD      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first KD+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
*
*          On exit, AB is overwritten by values generated during the
*          reduction to tridiagonal form.  If UPLO = 'U', the first
*          superdiagonal and the diagonal of the tridiagonal matrix T
*          are returned in rows KD and KD+1 of AB, and if UPLO = 'L',
*          the diagonal and first subdiagonal of T are returned in the
*          first two rows of AB.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KD + 1.
*
*  Q       (output) DOUBLE PRECISION array, dimension (LDQ, N)
*          If JOBZ = 'V', the N-by-N orthogonal matrix used in the
*                         reduction to tridiagonal form.
*          If JOBZ = 'N', the array Q is not referenced.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.  If JOBZ = 'V', then
*          LDQ >= max(1,N).
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing AB to tridiagonal form.
*
*          Eigenvalues will be computed most accurately when ABSTOL is
*          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
*          If this routine returns with INFO>0, indicating that some
*          eigenvectors did not converge, try setting ABSTOL to
*          2*DLAMCH('S').
*
*          See "Computing Small Singular Values of Bidiagonal Matrices
*          with Guaranteed High Relative Accuracy," by Demmel and
*          Kahan, LAPACK Working Note #3.
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          The first M elements contain the selected eigenvalues in
*          ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          If an eigenvector fails to converge, then that column of Z
*          contains the latest approximation to the eigenvector, and the
*          index of the eigenvector is returned in IFAIL.
*          If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (7*N)
*
*  IWORK   (workspace) INTEGER array, dimension (5*N)
*
*  IFAIL   (output) INTEGER array, dimension (N)
*          If JOBZ = 'V', then if INFO = 0, the first M elements of
*          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*          indices of the eigenvectors that failed to converge.
*          If JOBZ = 'N', then IFAIL is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = i, then i eigenvectors failed to converge.
*                Their indices are stored in array IFAIL.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LOWER, TEST, VALEIG, WANTZ
      CHARACTER          ORDER
      INTEGER            I, IINFO, IMAX, INDD, INDE, INDEE, INDIBL,
     $                   INDISP, INDIWO, INDWRK, ISCALE, ITMP1, J, JJ,
     $                   NSPLIT
      DOUBLE PRECISION   ABSTLL, ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN,
     $                   SIGMA, SMLNUM, TMP1, VLL, VUU
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSB
      EXTERNAL           LSAME, DLAMCH, DLANSB
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMV, DLACPY, DLASCL, DSBTRD, DSCAL,
     $                   DSTEBZ, DSTEIN, DSTEQR, DSTERF, DSWAP, XERBLA
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
      LOWER = LSAME( UPLO, 'L' )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( KD.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.KD+1 ) THEN
         INFO = -7
      ELSE IF( WANTZ .AND. LDQ.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -11
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -12
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -13
            END IF
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) )
     $      INFO = -18
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSBEVX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         M = 1
         IF( LOWER ) THEN
            TMP1 = AB( 1, 1 )
         ELSE
            TMP1 = AB( KD+1, 1 )
         END IF
         IF( VALEIG ) THEN
            IF( .NOT.( VL.LT.TMP1 .AND. VU.GE.TMP1 ) )
     $         M = 0
         END IF
         IF( M.EQ.1 ) THEN
            W( 1 ) = TMP1
            IF( WANTZ )
     $         Z( 1, 1 ) = ONE
         END IF
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
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      ABSTLL = ABSTOL
      IF( VALEIG ) THEN
         VLL = VL
         VUU = VU
      ELSE
         VLL = ZERO
         VUU = ZERO
      END IF
      ANRM = DLANSB( 'M', UPLO, N, KD, AB, LDAB, WORK )
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         IF( LOWER ) THEN
            CALL DLASCL( 'B', KD, KD, ONE, SIGMA, N, N, AB, LDAB, INFO )
         ELSE
            CALL DLASCL( 'Q', KD, KD, ONE, SIGMA, N, N, AB, LDAB, INFO )
         END IF
         IF( ABSTOL.GT.0 )
     $      ABSTLL = ABSTOL*SIGMA
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         END IF
      END IF
*
*     Call DSBTRD to reduce symmetric band matrix to tridiagonal form.
*
      INDD = 1
      INDE = INDD + N
      INDWRK = INDE + N
      CALL DSBTRD( JOBZ, UPLO, N, KD, AB, LDAB, WORK( INDD ),
     $             WORK( INDE ), Q, LDQ, WORK( INDWRK ), IINFO )
*
*     If all eigenvalues are desired and ABSTOL is less than or equal
*     to zero, then call DSTERF or SSTEQR.  If this fails for some
*     eigenvalue, then try DSTEBZ.
*
      TEST = .FALSE.
      IF (INDEIG) THEN
         IF (IL.EQ.1 .AND. IU.EQ.N) THEN
            TEST = .TRUE.
         END IF
      END IF
      IF ((ALLEIG .OR. TEST) .AND. (ABSTOL.LE.ZERO)) THEN
         CALL DCOPY( N, WORK( INDD ), 1, W, 1 )
         INDEE = INDWRK + 2*N
         IF( .NOT.WANTZ ) THEN
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DSTERF( N, W, WORK( INDEE ), INFO )
         ELSE
            CALL DLACPY( 'A', N, N, Q, LDQ, Z, LDZ )
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DSTEQR( JOBZ, N, W, WORK( INDEE ), Z, LDZ,
     $                   WORK( INDWRK ), INFO )
            IF( INFO.EQ.0 ) THEN
               DO 10 I = 1, N
                  IFAIL( I ) = 0
   10          CONTINUE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            M = N
            GO TO 30
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call DSTEBZ and, if eigenvectors are desired, SSTEIN.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF
      INDIBL = 1
      INDISP = INDIBL + N
      INDIWO = INDISP + N
      CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL,
     $             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
     $             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWRK ),
     $             IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
     $                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
     $                WORK( INDWRK ), IWORK( INDIWO ), IFAIL, INFO )
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by DSTEIN.
*
         DO 20 J = 1, M
            CALL DCOPY( N, Z( 1, J ), 1, WORK( 1 ), 1 )
            CALL DGEMV( 'N', N, N, ONE, Q, LDQ, WORK, 1, ZERO,
     $                  Z( 1, J ), 1 )
   20    CONTINUE
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
   30 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         END IF
         CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
      IF( WANTZ ) THEN
         DO 50 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 40 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   40       CONTINUE
*
            IF( I.NE.0 ) THEN
               ITMP1 = IWORK( INDIBL+I-1 )
               W( I ) = W( J )
               IWORK( INDIBL+I-1 ) = IWORK( INDIBL+J-1 )
               W( J ) = TMP1
               IWORK( INDIBL+J-1 ) = ITMP1
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
               IF( INFO.NE.0 ) THEN
                  ITMP1 = IFAIL( I )
                  IFAIL( I ) = IFAIL( J )
                  IFAIL( J ) = ITMP1
               END IF
            END IF
   50    CONTINUE
      END IF
*
      RETURN
*
*     End of DSBEVX
*
      END
      SUBROUTINE DSBGV( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W, Z,
     $                  LDZ, WORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, KA, KB, LDAB, LDBB, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AB( LDAB, * ), BB( LDBB, * ), W( * ),
     $                   WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSBGV computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite banded eigenproblem, of
*  the form A*x=(lambda)*B*x. Here A and B are assumed to be symmetric
*  and banded, and B is also positive definite.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangles of A and B are stored;
*          = 'L':  Lower triangles of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  KA      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'. KA >= 0.
*
*  KB      (input) INTEGER
*          The number of superdiagonals of the matrix B if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'. KB >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first ka+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(ka+1+i-j,j) = A(i,j) for max(1,j-ka)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+ka).
*
*          On exit, the contents of AB are destroyed.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KA+1.
*
*  BB      (input/output) DOUBLE PRECISION array, dimension (LDBB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix B, stored in the first kb+1 rows of the array.  The
*          j-th column of B is stored in the j-th column of the array BB
*          as follows:
*          if UPLO = 'U', BB(kb+1+i-j,j) = B(i,j) for max(1,j-kb)<=i<=j;
*          if UPLO = 'L', BB(1+i-j,j)    = B(i,j) for j<=i<=min(n,j+kb).
*
*          On exit, the factor S from the split Cholesky factorization
*          B = S**T*S, as returned by DPBSTF.
*
*  LDBB    (input) INTEGER
*          The leading dimension of the array BB.  LDBB >= KB+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the matrix Z of
*          eigenvectors, with the i-th column of Z holding the
*          eigenvector associated with W(i). The eigenvectors are
*          normalized so that Z**T*B*Z = I.
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= N.
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, and i is:
*             <= N:  the algorithm failed to converge:
*                    i off-diagonal elements of an intermediate
*                    tridiagonal form did not converge to zero;
*             > N:   if INFO = N + i, for 1 <= i <= N, then DPBSTF
*                    returned INFO = i: B is not positive definite.
*                    The factorization of B could not be completed and
*                    no eigenvalues or eigenvectors were computed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            UPPER, WANTZ
      CHARACTER          VECT
      INTEGER            IINFO, INDE, INDWRK
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPBSTF, DSBGST, DSBTRD, DSTEQR, DSTERF, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      UPPER = LSAME( UPLO, 'U' )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( KA.LT.0 ) THEN
         INFO = -4
      ELSE IF( KB.LT.0 .OR. KB.GT.KA ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.KA+1 ) THEN
         INFO = -7
      ELSE IF( LDBB.LT.KB+1 ) THEN
         INFO = -9
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -12
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSBGV ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Form a split Cholesky factorization of B.
*
      CALL DPBSTF( UPLO, N, KB, BB, LDBB, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem.
*
      INDE = 1
      INDWRK = INDE + N
      CALL DSBGST( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, Z, LDZ,
     $             WORK( INDWRK ), IINFO )
*
*     Reduce to tridiagonal form.
*
      IF( WANTZ ) THEN
         VECT = 'U'
      ELSE
         VECT = 'N'
      END IF
      CALL DSBTRD( VECT, UPLO, N, KA, AB, LDAB, W, WORK( INDE ), Z, LDZ,
     $             WORK( INDWRK ), IINFO )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, call SSTEQR.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         CALL DSTEQR( JOBZ, N, W, WORK( INDE ), Z, LDZ, WORK( INDWRK ),
     $                INFO )
      END IF
      RETURN
*
*     End of DSBGV
*
      END
      SUBROUTINE DSBGVD( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, W,
     $                   Z, LDZ, WORK, LWORK, IWORK, LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, KA, KB, LDAB, LDBB, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   AB( LDAB, * ), BB( LDBB, * ), W( * ),
     $                   WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSBGVD computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite banded eigenproblem, of the
*  form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric and
*  banded, and B is also positive definite.  If eigenvectors are
*  desired, it uses a divide and conquer algorithm.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangles of A and B are stored;
*          = 'L':  Lower triangles of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  KA      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KA >= 0.
*
*  KB      (input) INTEGER
*          The number of superdiagonals of the matrix B if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KB >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first ka+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(ka+1+i-j,j) = A(i,j) for max(1,j-ka)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+ka).
*
*          On exit, the contents of AB are destroyed.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KA+1.
*
*  BB      (input/output) DOUBLE PRECISION array, dimension (LDBB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix B, stored in the first kb+1 rows of the array.  The
*          j-th column of B is stored in the j-th column of the array BB
*          as follows:
*          if UPLO = 'U', BB(ka+1+i-j,j) = B(i,j) for max(1,j-kb)<=i<=j;
*          if UPLO = 'L', BB(1+i-j,j)    = B(i,j) for j<=i<=min(n,j+kb).
*
*          On exit, the factor S from the split Cholesky factorization
*          B = S**T*S, as returned by DPBSTF.
*
*  LDBB    (input) INTEGER
*          The leading dimension of the array BB.  LDBB >= KB+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the matrix Z of
*          eigenvectors, with the i-th column of Z holding the
*          eigenvector associated with W(i).  The eigenvectors are
*          normalized so Z**T*B*Z = I.
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If N <= 1,               LWORK >= 1.
*          If JOBZ = 'N' and N > 1, LWORK >= 3*N.
*          If JOBZ = 'V' and N > 1, LWORK >= 1 + 5*N + 2*N**2.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal sizes of the WORK and IWORK
*          arrays, returns these values as the first entries of the WORK
*          and IWORK arrays, and no error message related to LWORK or
*          LIWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if LIWORK > 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If JOBZ  = 'N' or N <= 1, LIWORK >= 1.
*          If JOBZ  = 'V' and N > 1, LIWORK >= 3 + 5*N.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal sizes of the WORK and
*          IWORK arrays, returns these values as the first entries of
*          the WORK and IWORK arrays, and no error message related to
*          LWORK or LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, and i is:
*             <= N:  the algorithm failed to converge:
*                    i off-diagonal elements of an intermediate
*                    tridiagonal form did not converge to zero;
*             > N:   if INFO = N + i, for 1 <= i <= N, then DPBSTF
*                    returned INFO = i: B is not positive definite.
*                    The factorization of B could not be completed and
*                    no eigenvalues or eigenvectors were computed.
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
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER, WANTZ
      CHARACTER          VECT
      INTEGER            IINFO, INDE, INDWK2, INDWRK, LIWMIN, LLWRK2,
     $                   LWMIN
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DLACPY, DPBSTF, DSBGST, DSBTRD, DSTEDC,
     $                   DSTERF, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
*
      INFO = 0
      IF( N.LE.1 ) THEN
         LIWMIN = 1
         LWMIN = 1
      ELSE IF( WANTZ ) THEN
         LIWMIN = 3 + 5*N
         LWMIN = 1 + 5*N + 2*N**2
      ELSE
         LIWMIN = 1
         LWMIN = 2*N
      END IF
*
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( KA.LT.0 ) THEN
         INFO = -4
      ELSE IF( KB.LT.0 .OR. KB.GT.KA ) THEN
         INFO = -5
      ELSE IF( LDAB.LT.KA+1 ) THEN
         INFO = -7
      ELSE IF( LDBB.LT.KB+1 ) THEN
         INFO = -9
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -12
      END IF
*
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
*
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -14
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -16
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSBGVD', -INFO )
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
*     Form a split Cholesky factorization of B.
*
      CALL DPBSTF( UPLO, N, KB, BB, LDBB, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem.
*
      INDE = 1
      INDWRK = INDE + N
      INDWK2 = INDWRK + N*N
      LLWRK2 = LWORK - INDWK2 + 1
      CALL DSBGST( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, Z, LDZ,
     $             WORK( INDWRK ), IINFO )
*
*     Reduce to tridiagonal form.
*
      IF( WANTZ ) THEN
         VECT = 'U'
      ELSE
         VECT = 'N'
      END IF
      CALL DSBTRD( VECT, UPLO, N, KA, AB, LDAB, W, WORK( INDE ), Z, LDZ,
     $             WORK( INDWRK ), IINFO )
*
*     For eigenvalues only, call DSTERF. For eigenvectors, call SSTEDC.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         CALL DSTEDC( 'I', N, W, WORK( INDE ), WORK( INDWRK ), N,
     $                WORK( INDWK2 ), LLWRK2, IWORK, LIWORK, INFO )
         CALL DGEMM( 'N', 'N', N, N, N, ONE, Z, LDZ, WORK( INDWRK ), N,
     $               ZERO, WORK( INDWK2 ), N )
         CALL DLACPY( 'A', N, N, WORK( INDWK2 ), N, Z, LDZ )
      END IF
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of DSBGVD
*
      END
      SUBROUTINE DSBGVX( JOBZ, RANGE, UPLO, N, KA, KB, AB, LDAB, BB,
     $                   LDBB, Q, LDQ, VL, VU, IL, IU, ABSTOL, M, W, Z,
     $                   LDZ, WORK, IWORK, IFAIL, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, IU, KA, KB, LDAB, LDBB, LDQ, LDZ, M,
     $                   N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      DOUBLE PRECISION   AB( LDAB, * ), BB( LDBB, * ), Q( LDQ, * ),
     $                   W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSBGVX computes selected eigenvalues, and optionally, eigenvectors
*  of a real generalized symmetric-definite banded eigenproblem, of
*  the form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric
*  and banded, and B is also positive definite.  Eigenvalues and
*  eigenvectors can be selected by specifying either all eigenvalues,
*  a range of values or a range of indices for the desired eigenvalues.
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
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangles of A and B are stored;
*          = 'L':  Lower triangles of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  KA      (input) INTEGER
*          The number of superdiagonals of the matrix A if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KA >= 0.
*
*  KB      (input) INTEGER
*          The number of superdiagonals of the matrix B if UPLO = 'U',
*          or the number of subdiagonals if UPLO = 'L'.  KB >= 0.
*
*  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix A, stored in the first ka+1 rows of the array.  The
*          j-th column of A is stored in the j-th column of the array AB
*          as follows:
*          if UPLO = 'U', AB(ka+1+i-j,j) = A(i,j) for max(1,j-ka)<=i<=j;
*          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+ka).
*
*          On exit, the contents of AB are destroyed.
*
*  LDAB    (input) INTEGER
*          The leading dimension of the array AB.  LDAB >= KA+1.
*
*  BB      (input/output) DOUBLE PRECISION array, dimension (LDBB, N)
*          On entry, the upper or lower triangle of the symmetric band
*          matrix B, stored in the first kb+1 rows of the array.  The
*          j-th column of B is stored in the j-th column of the array BB
*          as follows:
*          if UPLO = 'U', BB(ka+1+i-j,j) = B(i,j) for max(1,j-kb)<=i<=j;
*          if UPLO = 'L', BB(1+i-j,j)    = B(i,j) for j<=i<=min(n,j+kb).
*
*          On exit, the factor S from the split Cholesky factorization
*          B = S**T*S, as returned by DPBSTF.
*
*  LDBB    (input) INTEGER
*          The leading dimension of the array BB.  LDBB >= KB+1.
*
*  Q       (output) DOUBLE PRECISION array, dimension (LDQ, N)
*          If JOBZ = 'V', the n-by-n matrix used in the reduction of
*          A*x = (lambda)*B*x to standard form, i.e. C*x = (lambda)*x,
*          and consequently C to tridiagonal form.
*          If JOBZ = 'N', the array Q is not referenced.
*
*  LDQ     (input) INTEGER
*          The leading dimension of the array Q.  If JOBZ = 'N',
*          LDQ >= 1. If JOBZ = 'V', LDQ >= max(1,N).
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing A to tridiagonal form.
*
*          Eigenvalues will be computed most accurately when ABSTOL is
*          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
*          If this routine returns with INFO>0, indicating that some
*          eigenvectors did not converge, try setting ABSTOL to
*          2*DLAMCH('S').
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the matrix Z of
*          eigenvectors, with the i-th column of Z holding the
*          eigenvector associated with W(i).  The eigenvectors are
*          normalized so Z**T*B*Z = I.
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (7*N)
*
*  IWORK   (workspace/output) INTEGER array, dimension (5*N)
*
*  IFAIL   (output) INTEGER array, dimension (M)
*          If JOBZ = 'V', then if INFO = 0, the first M elements of
*          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*          indices of the eigenvalues that failed to converge.
*          If JOBZ = 'N', then IFAIL is not referenced.
*
*  INFO    (output) INTEGER
*          = 0 : successful exit
*          < 0 : if INFO = -i, the i-th argument had an illegal value
*          <= N: if INFO = i, then i eigenvectors failed to converge.
*                  Their indices are stored in IFAIL.
*          > N : DPBSTF returned an error code; i.e.,
*                if INFO = N + i, for 1 <= i <= N, then the leading
*                minor of order i of B is not positive definite.
*                The factorization of B could not be completed and
*                no eigenvalues or eigenvectors were computed.
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
      LOGICAL            ALLEIG, INDEIG, TEST, UPPER, VALEIG, WANTZ
      CHARACTER          ORDER, VECT
      INTEGER            I, IINFO, INDD, INDE, INDEE, INDIBL, INDISP,
     $                   INDIWO, INDWRK, ITMP1, J, JJ, NSPLIT
      DOUBLE PRECISION   TMP1
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMV, DLACPY, DPBSTF, DSBGST, DSBTRD,
     $                   DSTEBZ, DSTEIN, DSTEQR, DSTERF, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      UPPER = LSAME( UPLO, 'U' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( KA.LT.0 ) THEN
         INFO = -5
      ELSE IF( KB.LT.0 .OR. KB.GT.KA ) THEN
         INFO = -6
      ELSE IF( LDAB.LT.KA+1 ) THEN
         INFO = -8
      ELSE IF( LDBB.LT.KB+1 ) THEN
         INFO = -10
      ELSE IF( LDQ.LT.1 .OR. ( WANTZ .AND. LDQ.LT.N ) ) THEN
         INFO = -12
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -14
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -15
            ELSE IF ( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -16
            END IF
         END IF
      END IF
      IF( INFO.EQ.0) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
            INFO = -21
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSBGVX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 )
     $   RETURN
*
*     Form a split Cholesky factorization of B.
*
      CALL DPBSTF( UPLO, N, KB, BB, LDBB, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem.
*
      CALL DSBGST( JOBZ, UPLO, N, KA, KB, AB, LDAB, BB, LDBB, Q, LDQ,
     $             WORK, IINFO )
*
*     Reduce symmetric band matrix to tridiagonal form.
*
      INDD = 1
      INDE = INDD + N
      INDWRK = INDE + N
      IF( WANTZ ) THEN
         VECT = 'U'
      ELSE
         VECT = 'N'
      END IF
      CALL DSBTRD( VECT, UPLO, N, KA, AB, LDAB, WORK( INDD ),
     $             WORK( INDE ), Q, LDQ, WORK( INDWRK ), IINFO )
*
*     If all eigenvalues are desired and ABSTOL is less than or equal
*     to zero, then call DSTERF or SSTEQR.  If this fails for some
*     eigenvalue, then try DSTEBZ.
*
      TEST = .FALSE.
      IF( INDEIG ) THEN
         IF( IL.EQ.1 .AND. IU.EQ.N ) THEN
            TEST = .TRUE.
         END IF
      END IF
      IF( ( ALLEIG .OR. TEST ) .AND. ( ABSTOL.LE.ZERO ) ) THEN
         CALL DCOPY( N, WORK( INDD ), 1, W, 1 )
         INDEE = INDWRK + 2*N
         CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
         IF( .NOT.WANTZ ) THEN
            CALL DSTERF( N, W, WORK( INDEE ), INFO )
         ELSE
            CALL DLACPY( 'A', N, N, Q, LDQ, Z, LDZ )
            CALL DSTEQR( JOBZ, N, W, WORK( INDEE ), Z, LDZ,
     $                   WORK( INDWRK ), INFO )
            IF( INFO.EQ.0 ) THEN
               DO 10 I = 1, N
                  IFAIL( I ) = 0
   10          CONTINUE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            M = N
            GO TO 30
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call DSTEBZ and, if eigenvectors are desired,
*     call DSTEIN.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF
      INDIBL = 1
      INDISP = INDIBL + N
      INDIWO = INDISP + N
      CALL DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL,
     $             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
     $             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWRK ),
     $             IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
     $                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
     $                WORK( INDWRK ), IWORK( INDIWO ), IFAIL, INFO )
*
*        Apply transformation matrix used in reduction to tridiagonal
*        form to eigenvectors returned by DSTEIN.
*
         DO 20 J = 1, M
            CALL DCOPY( N, Z( 1, J ), 1, WORK( 1 ), 1 )
            CALL DGEMV( 'N', N, N, ONE, Q, LDQ, WORK, 1, ZERO,
     $                  Z( 1, J ), 1 )
   20    CONTINUE
      END IF
*
   30 CONTINUE
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
      IF( WANTZ ) THEN
         DO 50 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 40 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   40       CONTINUE
*
            IF( I.NE.0 ) THEN
               ITMP1 = IWORK( INDIBL+I-1 )
               W( I ) = W( J )
               IWORK( INDIBL+I-1 ) = IWORK( INDIBL+J-1 )
               W( J ) = TMP1
               IWORK( INDIBL+J-1 ) = ITMP1
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
               IF( INFO.NE.0 ) THEN
                  ITMP1 = IFAIL( I )
                  IFAIL( I ) = IFAIL( J )
                  IFAIL( J ) = ITMP1
               END IF
            END IF
   50    CONTINUE
      END IF
*
      RETURN
*
*     End of DSBGVX
*
      END
      SUBROUTINE DSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AP( * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSPEV computes all the eigenvalues and, optionally, eigenvectors of a
*  real symmetric matrix A in packed storage.
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
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
*
*          On exit, AP is overwritten by values generated during the
*          reduction to tridiagonal form.  If UPLO = 'U', the diagonal
*          and first superdiagonal of the tridiagonal matrix T overwrite
*          the corresponding elements of A, and if UPLO = 'L', the
*          diagonal and first subdiagonal of T overwrite the
*          corresponding elements of A.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
*          eigenvectors of the matrix A, with the i-th column of Z
*          holding the eigenvector associated with W(i).
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = i, the algorithm failed to converge; i
*                off-diagonal elements of an intermediate tridiagonal
*                form did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            WANTZ
      INTEGER            IINFO, IMAX, INDE, INDTAU, INDWRK, ISCALE
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSP
      EXTERNAL           LSAME, DLAMCH, DLANSP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DOPGTR, DSCAL, DSPTRD, DSTEQR, DSTERF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( UPLO, 'U' ) .OR. LSAME( UPLO, 'L' ) ) )
     $          THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -7
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSPEV ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         W( 1 ) = AP( 1 )
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      ANRM = DLANSP( 'M', UPLO, N, AP, WORK )
      ISCALE = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         CALL DSCAL( ( N*( N+1 ) ) / 2, SIGMA, AP, 1 )
      END IF
*
*     Call DSPTRD to reduce symmetric packed matrix to tridiagonal form.
*
      INDE = 1
      INDTAU = INDE + N
      CALL DSPTRD( UPLO, N, AP, W, WORK( INDE ), WORK( INDTAU ), IINFO )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, first call
*     DOPGTR to generate the orthogonal matrix, then call DSTEQR.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         INDWRK = INDTAU + N
         CALL DOPGTR( UPLO, N, AP, WORK( INDTAU ), Z, LDZ,
     $                WORK( INDWRK ), IINFO )
         CALL DSTEQR( JOBZ, N, W, WORK( INDE ), Z, LDZ, WORK( INDTAU ),
     $                INFO )
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
      RETURN
*
*     End of DSPEV
*
      END
      SUBROUTINE DSPEVD( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK,
     $                   IWORK, LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   AP( * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSPEVD computes all the eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix A in packed storage. If eigenvectors are
*  desired, it uses a divide and conquer algorithm.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
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
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
*
*          On exit, AP is overwritten by values generated during the
*          reduction to tridiagonal form.  If UPLO = 'U', the diagonal
*          and first superdiagonal of the tridiagonal matrix T overwrite
*          the corresponding elements of A, and if UPLO = 'L', the
*          diagonal and first subdiagonal of T overwrite the
*          corresponding elements of A.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
*          eigenvectors of the matrix A, with the i-th column of Z
*          holding the eigenvector associated with W(i).
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array,
*                                         dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the required LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If N <= 1,               LWORK must be at least 1.
*          If JOBZ = 'N' and N > 1, LWORK must be at least 2*N.
*          If JOBZ = 'V' and N > 1, LWORK must be at least
*                                                 1 + 6*N + N**2.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the required sizes of the WORK and IWORK
*          arrays, returns these values as the first entries of the WORK
*          and IWORK arrays, and no error message related to LWORK or
*          LIWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the required LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If JOBZ  = 'N' or N <= 1, LIWORK must be at least 1.
*          If JOBZ  = 'V' and N > 1, LIWORK must be at least 3 + 5*N.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the required sizes of the WORK and
*          IWORK arrays, returns these values as the first entries of
*          the WORK and IWORK arrays, and no error message related to
*          LWORK or LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  if INFO = i, the algorithm failed to converge; i
*                off-diagonal elements of an intermediate tridiagonal
*                form did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, WANTZ
      INTEGER            IINFO, INDE, INDTAU, INDWRK, ISCALE, LIWMIN,
     $                   LLWORK, LWMIN
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSP
      EXTERNAL           LSAME, DLAMCH, DLANSP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DOPMTR, DSCAL, DSPTRD, DSTEDC, DSTERF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( LSAME( UPLO, 'U' ) .OR. LSAME( UPLO, 'L' ) ) )
     $          THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -7
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( N.LE.1 ) THEN
            LIWMIN = 1
            LWMIN = 1
         ELSE
            IF( WANTZ ) THEN
               LIWMIN = 3 + 5*N
               LWMIN = 1 + 6*N + N**2
            ELSE
               LIWMIN = 1
               LWMIN = 2*N
            END IF
         END IF
         IWORK( 1 ) = LIWMIN
         WORK( 1 ) = LWMIN
*
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -9
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -11
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSPEVD', -INFO )
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
      IF( N.EQ.1 ) THEN
         W( 1 ) = AP( 1 )
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      ANRM = DLANSP( 'M', UPLO, N, AP, WORK )
      ISCALE = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         CALL DSCAL( ( N*( N+1 ) ) / 2, SIGMA, AP, 1 )
      END IF
*
*     Call DSPTRD to reduce symmetric packed matrix to tridiagonal form.
*
      INDE = 1
      INDTAU = INDE + N
      CALL DSPTRD( UPLO, N, AP, W, WORK( INDE ), WORK( INDTAU ), IINFO )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, first call
*     DSTEDC to generate the eigenvector matrix, WORK(INDWRK), of the
*     tridiagonal matrix, then call DOPMTR to multiply it by the
*     Householder transformations represented in AP.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         INDWRK = INDTAU + N
         LLWORK = LWORK - INDWRK + 1
         CALL DSTEDC( 'I', N, W, WORK( INDE ), Z, LDZ, WORK( INDWRK ),
     $                LLWORK, IWORK, LIWORK, INFO )
         CALL DOPMTR( 'L', UPLO, 'N', N, N, AP, WORK( INDTAU ), Z, LDZ,
     $                WORK( INDWRK ), IINFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( ISCALE.EQ.1 )
     $   CALL DSCAL( N, ONE / SIGMA, W, 1 )
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
      RETURN
*
*     End of DSPEVD
*
      END
      SUBROUTINE DSPEVX( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU,
     $                   ABSTOL, M, W, Z, LDZ, WORK, IWORK, IFAIL,
     $                   INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, IU, LDZ, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      DOUBLE PRECISION   AP( * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSPEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix A in packed storage.  Eigenvalues/vectors
*  can be selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  RANGE   (input) CHARACTER*1
*          = 'A': all eigenvalues will be found;
*          = 'V': all eigenvalues in the half-open interval (VL,VU]
*                 will be found;
*          = 'I': the IL-th through IU-th eigenvalues will be found.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
*
*          On exit, AP is overwritten by values generated during the
*          reduction to tridiagonal form.  If UPLO = 'U', the diagonal
*          and first superdiagonal of the tridiagonal matrix T overwrite
*          the corresponding elements of A, and if UPLO = 'L', the
*          diagonal and first subdiagonal of T overwrite the
*          corresponding elements of A.
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing AP to tridiagonal form.
*
*          Eigenvalues will be computed most accurately when ABSTOL is
*          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
*          If this routine returns with INFO>0, indicating that some
*          eigenvectors did not converge, try setting ABSTOL to
*          2*DLAMCH('S').
*
*          See "Computing Small Singular Values of Bidiagonal Matrices
*          with Guaranteed High Relative Accuracy," by Demmel and
*          Kahan, LAPACK Working Note #3.
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the selected eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          If an eigenvector fails to converge, then that column of Z
*          contains the latest approximation to the eigenvector, and the
*          index of the eigenvector is returned in IFAIL.
*          If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (8*N)
*
*  IWORK   (workspace) INTEGER array, dimension (5*N)
*
*  IFAIL   (output) INTEGER array, dimension (N)
*          If JOBZ = 'V', then if INFO = 0, the first M elements of
*          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*          indices of the eigenvectors that failed to converge.
*          If JOBZ = 'N', then IFAIL is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, then i eigenvectors failed to converge.
*                Their indices are stored in array IFAIL.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, TEST, VALEIG, WANTZ
      CHARACTER          ORDER
      INTEGER            I, IINFO, IMAX, INDD, INDE, INDEE, INDIBL,
     $                   INDISP, INDIWO, INDTAU, INDWRK, ISCALE, ITMP1,
     $                   J, JJ, NSPLIT
      DOUBLE PRECISION   ABSTLL, ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN,
     $                   SIGMA, SMLNUM, TMP1, VLL, VUU
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSP
      EXTERNAL           LSAME, DLAMCH, DLANSP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DOPGTR, DOPMTR, DSCAL, DSPTRD, DSTEBZ,
     $                   DSTEIN, DSTEQR, DSTERF, DSWAP, XERBLA
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
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LSAME( UPLO, 'L' ) .OR. LSAME( UPLO, 'U' ) ) )
     $          THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -7
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -8
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -9
            END IF
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) )
     $      INFO = -14
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSPEVX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = AP( 1 )
         ELSE
            IF( VL.LT.AP( 1 ) .AND. VU.GE.AP( 1 ) ) THEN
               M = 1
               W( 1 ) = AP( 1 )
            END IF
         END IF
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      ABSTLL = ABSTOL
      IF( VALEIG ) THEN
         VLL = VL
         VUU = VU
      ELSE
         VLL = ZERO
         VUU = ZERO
      END IF
      ANRM = DLANSP( 'M', UPLO, N, AP, WORK )
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         CALL DSCAL( ( N*( N+1 ) ) / 2, SIGMA, AP, 1 )
         IF( ABSTOL.GT.0 )
     $      ABSTLL = ABSTOL*SIGMA
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         END IF
      END IF
*
*     Call DSPTRD to reduce symmetric packed matrix to tridiagonal form.
*
      INDTAU = 1
      INDE = INDTAU + N
      INDD = INDE + N
      INDWRK = INDD + N
      CALL DSPTRD( UPLO, N, AP, WORK( INDD ), WORK( INDE ),
     $             WORK( INDTAU ), IINFO )
*
*     If all eigenvalues are desired and ABSTOL is less than or equal
*     to zero, then call DSTERF or DOPGTR and SSTEQR.  If this fails
*     for some eigenvalue, then try DSTEBZ.
*
      TEST = .FALSE.
      IF (INDEIG) THEN
         IF (IL.EQ.1 .AND. IU.EQ.N) THEN
            TEST = .TRUE.
         END IF
      END IF
      IF ((ALLEIG .OR. TEST) .AND. (ABSTOL.LE.ZERO)) THEN
         CALL DCOPY( N, WORK( INDD ), 1, W, 1 )
         INDEE = INDWRK + 2*N
         IF( .NOT.WANTZ ) THEN
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DSTERF( N, W, WORK( INDEE ), INFO )
         ELSE
            CALL DOPGTR( UPLO, N, AP, WORK( INDTAU ), Z, LDZ,
     $                   WORK( INDWRK ), IINFO )
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DSTEQR( JOBZ, N, W, WORK( INDEE ), Z, LDZ,
     $                   WORK( INDWRK ), INFO )
            IF( INFO.EQ.0 ) THEN
               DO 10 I = 1, N
                  IFAIL( I ) = 0
   10          CONTINUE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            M = N
            GO TO 20
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call DSTEBZ and, if eigenvectors are desired, SSTEIN.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF
      INDIBL = 1
      INDISP = INDIBL + N
      INDIWO = INDISP + N
      CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL,
     $             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
     $             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWRK ),
     $             IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
     $                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
     $                WORK( INDWRK ), IWORK( INDIWO ), IFAIL, INFO )
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by DSTEIN.
*
         CALL DOPMTR( 'L', UPLO, 'N', N, M, AP, WORK( INDTAU ), Z, LDZ,
     $                WORK( INDWRK ), INFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
   20 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         END IF
         CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
      IF( WANTZ ) THEN
         DO 40 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 30 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   30       CONTINUE
*
            IF( I.NE.0 ) THEN
               ITMP1 = IWORK( INDIBL+I-1 )
               W( I ) = W( J )
               IWORK( INDIBL+I-1 ) = IWORK( INDIBL+J-1 )
               W( J ) = TMP1
               IWORK( INDIBL+J-1 ) = ITMP1
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
               IF( INFO.NE.0 ) THEN
                  ITMP1 = IFAIL( I )
                  IFAIL( I ) = IFAIL( J )
                  IFAIL( J ) = ITMP1
               END IF
            END IF
   40    CONTINUE
      END IF
*
      RETURN
*
*     End of DSPEVX
*
      END
      SUBROUTINE DSPGV( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,
     $                  INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, ITYPE, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   AP( * ), BP( * ), W( * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSPGV computes all the eigenvalues and, optionally, the eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
*  Here A and B are assumed to be symmetric, stored in packed format,
*  and B is also positive definite.
*
*  Arguments
*  =========
*
*  ITYPE   (input) INTEGER
*          Specifies the problem type to be solved:
*          = 1:  A*x = (lambda)*B*x
*          = 2:  A*B*x = (lambda)*x
*          = 3:  B*A*x = (lambda)*x
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangles of A and B are stored;
*          = 'L':  Lower triangles of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  AP      (input/output) DOUBLE PRECISION array, dimension
*                            (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
*
*          On exit, the contents of AP are destroyed.
*
*  BP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          B, packed columnwise in a linear array.  The j-th column of B
*          is stored in the array BP as follows:
*          if UPLO = 'U', BP(i + (j-1)*j/2) = B(i,j) for 1<=i<=j;
*          if UPLO = 'L', BP(i + (j-1)*(2*n-j)/2) = B(i,j) for j<=i<=n.
*
*          On exit, the triangular factor U or L from the Cholesky
*          factorization B = U**T*U or B = L*L**T, in the same storage
*          format as B.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the matrix Z of
*          eigenvectors.  The eigenvectors are normalized as follows:
*          if ITYPE = 1 or 2, Z**T*B*Z = I;
*          if ITYPE = 3, Z**T*inv(B)*Z = I.
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  DPPTRF or DSPEV returned an error code:
*             <= N:  if INFO = i, DSPEV failed to converge;
*                    i off-diagonal elements of an intermediate
*                    tridiagonal form did not converge to zero.
*             > N:   if INFO = n + i, for 1 <= i <= n, then the leading
*                    minor of order i of B is not positive definite.
*                    The factorization of B could not be completed and
*                    no eigenvalues or eigenvectors were computed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            UPPER, WANTZ
      CHARACTER          TRANS
      INTEGER            J, NEIG
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPPTRF, DSPEV, DSPGST, DTPMV, DTPSV, XERBLA
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      UPPER = LSAME( UPLO, 'U' )
*
      INFO = 0
      IF( ITYPE.LT.1 .OR. ITYPE.GT.3 ) THEN
         INFO = -1
      ELSE IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -9
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSPGV ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
*     Form a Cholesky factorization of B.
*
      CALL DPPTRF( UPLO, N, BP, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem and solve.
*
      CALL DSPGST( ITYPE, UPLO, N, AP, BP, INFO )
      CALL DSPEV( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, INFO )
*
      IF( WANTZ ) THEN
*
*        Backtransform eigenvectors to the original problem.
*
         NEIG = N
         IF( INFO.GT.0 )
     $      NEIG = INFO - 1
         IF( ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
*
*           For A*x=(lambda)*B*x and A*B*x=(lambda)*x;
*           backtransform eigenvectors: x = inv(L)'*y or inv(U)*y
*
            IF( UPPER ) THEN
               TRANS = 'N'
            ELSE
               TRANS = 'T'
            END IF
*
            DO 10 J = 1, NEIG
               CALL DTPSV( UPLO, TRANS, 'Non-unit', N, BP, Z( 1, J ),
     $                     1 )
   10       CONTINUE
*
         ELSE IF( ITYPE.EQ.3 ) THEN
*
*           For B*A*x=(lambda)*x;
*           backtransform eigenvectors: x = L*y or U'*y
*
            IF( UPPER ) THEN
               TRANS = 'T'
            ELSE
               TRANS = 'N'
            END IF
*
            DO 20 J = 1, NEIG
               CALL DTPMV( UPLO, TRANS, 'Non-unit', N, BP, Z( 1, J ),
     $                     1 )
   20       CONTINUE
         END IF
      END IF
      RETURN
*
*     End of DSPGV
*
      END
      SUBROUTINE DSPGVD( ITYPE, JOBZ, UPLO, N, AP, BP, W, Z, LDZ, WORK,
     $                   LWORK, IWORK, LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, ITYPE, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   AP( * ), BP( * ), W( * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSPGVD computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*  B are assumed to be symmetric, stored in packed format, and B is also
*  positive definite.
*  If eigenvectors are desired, it uses a divide and conquer algorithm.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  ITYPE   (input) INTEGER
*          Specifies the problem type to be solved:
*          = 1:  A*x = (lambda)*B*x
*          = 2:  A*B*x = (lambda)*x
*          = 3:  B*A*x = (lambda)*x
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangles of A and B are stored;
*          = 'L':  Lower triangles of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
*
*          On exit, the contents of AP are destroyed.
*
*  BP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          B, packed columnwise in a linear array.  The j-th column of B
*          is stored in the array BP as follows:
*          if UPLO = 'U', BP(i + (j-1)*j/2) = B(i,j) for 1<=i<=j;
*          if UPLO = 'L', BP(i + (j-1)*(2*n-j)/2) = B(i,j) for j<=i<=n.
*
*          On exit, the triangular factor U or L from the Cholesky
*          factorization B = U**T*U or B = L*L**T, in the same storage
*          format as B.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the matrix Z of
*          eigenvectors.  The eigenvectors are normalized as follows:
*          if ITYPE = 1 or 2, Z**T*B*Z = I;
*          if ITYPE = 3, Z**T*inv(B)*Z = I.
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the required LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If N <= 1,               LWORK >= 1.
*          If JOBZ = 'N' and N > 1, LWORK >= 2*N.
*          If JOBZ = 'V' and N > 1, LWORK >= 1 + 6*N + 2*N**2.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the required sizes of the WORK and IWORK
*          arrays, returns these values as the first entries of the WORK
*          and IWORK arrays, and no error message related to LWORK or
*          LIWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the required LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If JOBZ  = 'N' or N <= 1, LIWORK >= 1.
*          If JOBZ  = 'V' and N > 1, LIWORK >= 3 + 5*N.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the required sizes of the WORK and
*          IWORK arrays, returns these values as the first entries of
*          the WORK and IWORK arrays, and no error message related to
*          LWORK or LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  DPPTRF or DSPEVD returned an error code:
*             <= N:  if INFO = i, DSPEVD failed to converge;
*                    i off-diagonal elements of an intermediate
*                    tridiagonal form did not converge to zero;
*             > N:   if INFO = N + i, for 1 <= i <= N, then the leading
*                    minor of order i of B is not positive definite.
*                    The factorization of B could not be completed and
*                    no eigenvalues or eigenvectors were computed.
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
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER, WANTZ
      CHARACTER          TRANS
      INTEGER            J, LIWMIN, LWMIN, NEIG
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPPTRF, DSPEVD, DSPGST, DTPMV, DTPSV, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
*
      INFO = 0
      IF( ITYPE.LT.1 .OR. ITYPE.GT.3 ) THEN
         INFO = -1
      ELSE IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -9
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( N.LE.1 ) THEN
            LIWMIN = 1
            LWMIN = 1
         ELSE
            IF( WANTZ ) THEN
               LIWMIN = 3 + 5*N
               LWMIN = 1 + 6*N + 2*N**2
            ELSE
               LIWMIN = 1
               LWMIN = 2*N
            END IF
         END IF
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
*
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -11
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -13
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSPGVD', -INFO )
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
*     Form a Cholesky factorization of BP.
*
      CALL DPPTRF( UPLO, N, BP, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem and solve.
*
      CALL DSPGST( ITYPE, UPLO, N, AP, BP, INFO )
      CALL DSPEVD( JOBZ, UPLO, N, AP, W, Z, LDZ, WORK, LWORK, IWORK,
     $             LIWORK, INFO )
      LWMIN = MAX( DBLE( LWMIN ), DBLE( WORK( 1 ) ) )
      LIWMIN = MAX( DBLE( LIWMIN ), DBLE( IWORK( 1 ) ) )
*
      IF( WANTZ ) THEN
*
*        Backtransform eigenvectors to the original problem.
*
         NEIG = N
         IF( INFO.GT.0 )
     $      NEIG = INFO - 1
         IF( ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
*
*           For A*x=(lambda)*B*x and A*B*x=(lambda)*x;
*           backtransform eigenvectors: x = inv(L)'*y or inv(U)*y
*
            IF( UPPER ) THEN
               TRANS = 'N'
            ELSE
               TRANS = 'T'
            END IF
*
            DO 10 J = 1, NEIG
               CALL DTPSV( UPLO, TRANS, 'Non-unit', N, BP, Z( 1, J ),
     $                     1 )
   10       CONTINUE
*
         ELSE IF( ITYPE.EQ.3 ) THEN
*
*           For B*A*x=(lambda)*x;
*           backtransform eigenvectors: x = L*y or U'*y
*
            IF( UPPER ) THEN
               TRANS = 'T'
            ELSE
               TRANS = 'N'
            END IF
*
            DO 20 J = 1, NEIG
               CALL DTPMV( UPLO, TRANS, 'Non-unit', N, BP, Z( 1, J ),
     $                     1 )
   20       CONTINUE
         END IF
      END IF
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of DSPGVD
*
      END
      SUBROUTINE DSPGVX( ITYPE, JOBZ, RANGE, UPLO, N, AP, BP, VL, VU,
     $                   IL, IU, ABSTOL, M, W, Z, LDZ, WORK, IWORK,
     $                   IFAIL, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, ITYPE, IU, LDZ, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      DOUBLE PRECISION   AP( * ), BP( * ), W( * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSPGVX computes selected eigenvalues, and optionally, eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
*  and B are assumed to be symmetric, stored in packed storage, and B
*  is also positive definite.  Eigenvalues and eigenvectors can be
*  selected by specifying either a range of values or a range of indices
*  for the desired eigenvalues.
*
*  Arguments
*  =========
*
*  ITYPE   (input) INTEGER
*          Specifies the problem type to be solved:
*          = 1:  A*x = (lambda)*B*x
*          = 2:  A*B*x = (lambda)*x
*          = 3:  B*A*x = (lambda)*x
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
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A and B are stored;
*          = 'L':  Lower triangle of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrix pencil (A,B).  N >= 0.
*
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
*
*          On exit, the contents of AP are destroyed.
*
*  BP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          B, packed columnwise in a linear array.  The j-th column of B
*          is stored in the array BP as follows:
*          if UPLO = 'U', BP(i + (j-1)*j/2) = B(i,j) for 1<=i<=j;
*          if UPLO = 'L', BP(i + (j-1)*(2*n-j)/2) = B(i,j) for j<=i<=n.
*
*          On exit, the triangular factor U or L from the Cholesky
*          factorization B = U**T*U or B = L*L**T, in the same storage
*          format as B.
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing A to tridiagonal form.
*
*          Eigenvalues will be computed most accurately when ABSTOL is
*          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
*          If this routine returns with INFO>0, indicating that some
*          eigenvectors did not converge, try setting ABSTOL to
*          2*DLAMCH('S').
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          On normal exit, the first M elements contain the selected
*          eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
*          If JOBZ = 'N', then Z is not referenced.
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          The eigenvectors are normalized as follows:
*          if ITYPE = 1 or 2, Z**T*B*Z = I;
*          if ITYPE = 3, Z**T*inv(B)*Z = I.
*
*          If an eigenvector fails to converge, then that column of Z
*          contains the latest approximation to the eigenvector, and the
*          index of the eigenvector is returned in IFAIL.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (8*N)
*
*  IWORK   (workspace) INTEGER array, dimension (5*N)
*
*  IFAIL   (output) INTEGER array, dimension (N)
*          If JOBZ = 'V', then if INFO = 0, the first M elements of
*          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*          indices of the eigenvectors that failed to converge.
*          If JOBZ = 'N', then IFAIL is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  DPPTRF or DSPEVX returned an error code:
*             <= N:  if INFO = i, DSPEVX failed to converge;
*                    i eigenvectors failed to converge.  Their indices
*                    are stored in array IFAIL.
*             > N:   if INFO = N + i, for 1 <= i <= N, then the leading
*                    minor of order i of B is not positive definite.
*                    The factorization of B could not be completed and
*                    no eigenvalues or eigenvectors were computed.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA
*
* =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, UPPER, VALEIG, WANTZ
      CHARACTER          TRANS
      INTEGER            J
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPPTRF, DSPEVX, DSPGST, DTPMV, DTPSV, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      UPPER = LSAME( UPLO, 'U' )
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
*
      INFO = 0
      IF( ITYPE.LT.1 .OR. ITYPE.GT.3 ) THEN
         INFO = -1
      ELSE IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -3
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL ) THEN
               INFO = -9
            END IF
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 ) THEN
               INFO = -10
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -11
            END IF
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
            INFO = -16
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSPGVX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 )
     $   RETURN
*
*     Form a Cholesky factorization of B.
*
      CALL DPPTRF( UPLO, N, BP, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem and solve.
*
      CALL DSPGST( ITYPE, UPLO, N, AP, BP, INFO )
      CALL DSPEVX( JOBZ, RANGE, UPLO, N, AP, VL, VU, IL, IU, ABSTOL, M,
     $             W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
*
      IF( WANTZ ) THEN
*
*        Backtransform eigenvectors to the original problem.
*
         IF( INFO.GT.0 )
     $      M = INFO - 1
         IF( ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
*
*           For A*x=(lambda)*B*x and A*B*x=(lambda)*x;
*           backtransform eigenvectors: x = inv(L)'*y or inv(U)*y
*
            IF( UPPER ) THEN
               TRANS = 'N'
            ELSE
               TRANS = 'T'
            END IF
*
            DO 10 J = 1, M
               CALL DTPSV( UPLO, TRANS, 'Non-unit', N, BP, Z( 1, J ),
     $                     1 )
   10       CONTINUE
*
         ELSE IF( ITYPE.EQ.3 ) THEN
*
*           For B*A*x=(lambda)*x;
*           backtransform eigenvectors: x = L*y or U'*y
*
            IF( UPPER ) THEN
               TRANS = 'T'
            ELSE
               TRANS = 'N'
            END IF
*
            DO 20 J = 1, M
               CALL DTPMV( UPLO, TRANS, 'Non-unit', N, BP, Z( 1, J ),
     $                     1 )
   20       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DSPGVX
*
      END
      SUBROUTINE DSPSV( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDB, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   AP( * ), B( LDB, * )
*     ..
*
*  Purpose
*  =======
*
*  DSPSV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric matrix stored in packed format and X
*  and B are N-by-NRHS matrices.
*
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**T,  if UPLO = 'U', or
*     A = L * D * L**T,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, D is symmetric and block diagonal with 1-by-1
*  and 2-by-2 diagonal blocks.  The factored form of A is then used to
*  solve the system of equations A * X = B.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  AP      (input/output) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          On entry, the upper or lower triangle of the symmetric matrix
*          A, packed columnwise in a linear array.  The j-th column of A
*          is stored in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2n-j)/2) = A(i,j) for j<=i<=n.
*          See below for further details.
*
*          On exit, the block diagonal matrix D and the multipliers used
*          to obtain the factor U or L from the factorization
*          A = U*D*U**T or A = L*D*L**T as computed by DSPTRF, stored as
*          a packed triangular matrix in the same storage format as A.
*
*  IPIV    (output) INTEGER array, dimension (N)
*          Details of the interchanges and the block structure of D, as
*          determined by DSPTRF.  If IPIV(k) > 0, then rows and columns
*          k and IPIV(k) were interchanged, and D(k,k) is a 1-by-1
*          diagonal block.  If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0,
*          then rows and columns k-1 and -IPIV(k) were interchanged and
*          D(k-1:k,k-1:k) is a 2-by-2 diagonal block.  If UPLO = 'L' and
*          IPIV(k) = IPIV(k+1) < 0, then rows and columns k+1 and
*          -IPIV(k) were interchanged and D(k:k+1,k:k+1) is a 2-by-2
*          diagonal block.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, D(i,i) is exactly zero.  The factorization
*                has been completed, but the block diagonal matrix D is
*                exactly singular, so the solution could not be
*                computed.
*
*  Further Details
*  ===============
*
*  The packed storage scheme is illustrated by the following example
*  when N = 4, UPLO = 'U':
*
*  Two-dimensional storage of the symmetric matrix A:
*
*     a11 a12 a13 a14
*         a22 a23 a24
*             a33 a34     (aij = aji)
*                 a44
*
*  Packed storage of the upper triangle of A:
*
*  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
*
*  =====================================================================
*
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSPTRF, DSPTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -7
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSPSV ', -INFO )
         RETURN
      END IF
*
*     Compute the factorization A = U*D*U' or A = L*D*L'.
*
      CALL DSPTRF( UPLO, N, AP, IPIV, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL DSPTRS( UPLO, N, NRHS, AP, IPIV, B, LDB, INFO )
*
      END IF
      RETURN
*
*     End of DSPSV
*
      END
      SUBROUTINE DSPSVX( FACT, UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X,
     $                   LDX, RCOND, FERR, BERR, WORK, IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          FACT, UPLO
      INTEGER            INFO, LDB, LDX, N, NRHS
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * ), IWORK( * )
      DOUBLE PRECISION   AFP( * ), AP( * ), B( LDB, * ), BERR( * ),
     $                   FERR( * ), WORK( * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DSPSVX uses the diagonal pivoting factorization A = U*D*U**T or
*  A = L*D*L**T to compute the solution to a real system of linear
*  equations A * X = B, where A is an N-by-N symmetric matrix stored
*  in packed format and X and B are N-by-NRHS matrices.
*
*  Error bounds on the solution and a condition estimate are also
*  provided.
*
*  Description
*  ===========
*
*  The following steps are performed:
*
*  1. If FACT = 'N', the diagonal pivoting method is used to factor A as
*        A = U * D * U**T,  if UPLO = 'U', or
*        A = L * D * L**T,  if UPLO = 'L',
*     where U (or L) is a product of permutation and unit upper (lower)
*     triangular matrices and D is symmetric and block diagonal with
*     1-by-1 and 2-by-2 diagonal blocks.
*
*  2. If some D(i,i)=0, so that D is exactly singular, then the routine
*     returns with INFO = i. Otherwise, the factored form of A is used
*     to estimate the condition number of the matrix A.  If the
*     reciprocal of the condition number is less than machine precision,
*     INFO = N+1 is returned as a warning, but the routine still goes on
*     to solve for X and compute error bounds as described below.
*
*  3. The system of equations is solved for X using the factored form
*     of A.
*
*  4. Iterative refinement is applied to improve the computed solution
*     matrix and calculate error bounds and backward error estimates
*     for it.
*
*  Arguments
*  =========
*
*  FACT    (input) CHARACTER*1
*          Specifies whether or not the factored form of A has been
*          supplied on entry.
*          = 'F':  On entry, AFP and IPIV contain the factored form of
*                  A.  AP, AFP and IPIV will not be modified.
*          = 'N':  The matrix A will be copied to AFP and factored.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X.  NRHS >= 0.
*
*  AP      (input) DOUBLE PRECISION array, dimension (N*(N+1)/2)
*          The upper or lower triangle of the symmetric matrix A, packed
*          columnwise in a linear array.  The j-th column of A is stored
*          in the array AP as follows:
*          if UPLO = 'U', AP(i + (j-1)*j/2) = A(i,j) for 1<=i<=j;
*          if UPLO = 'L', AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j<=i<=n.
*          See below for further details.
*
*  AFP     (input or output) DOUBLE PRECISION array, dimension
*                            (N*(N+1)/2)
*          If FACT = 'F', then AFP is an input argument and on entry
*          contains the block diagonal matrix D and the multipliers used
*          to obtain the factor U or L from the factorization
*          A = U*D*U**T or A = L*D*L**T as computed by DSPTRF, stored as
*          a packed triangular matrix in the same storage format as A.
*
*          If FACT = 'N', then AFP is an output argument and on exit
*          contains the block diagonal matrix D and the multipliers used
*          to obtain the factor U or L from the factorization
*          A = U*D*U**T or A = L*D*L**T as computed by DSPTRF, stored as
*          a packed triangular matrix in the same storage format as A.
*
*  IPIV    (input or output) INTEGER array, dimension (N)
*          If FACT = 'F', then IPIV is an input argument and on entry
*          contains details of the interchanges and the block structure
*          of D, as determined by DSPTRF.
*          If IPIV(k) > 0, then rows and columns k and IPIV(k) were
*          interchanged and D(k,k) is a 1-by-1 diagonal block.
*          If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and
*          columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
*          is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =
*          IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were
*          interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
*
*          If FACT = 'N', then IPIV is an output argument and on exit
*          contains details of the interchanges and the block structure
*          of D, as determined by DSPTRF.
*
*  B       (input) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          The N-by-NRHS right hand side matrix B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  RCOND   (output) DOUBLE PRECISION
*          The estimate of the reciprocal condition number of the matrix
*          A.  If RCOND is less than the machine precision (in
*          particular, if RCOND = 0), the matrix is singular to working
*          precision.  This condition is indicated by a return code of
*          INFO > 0.
*
*  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bound for each solution vector
*          X(j) (the j-th column of the solution matrix X).
*          If XTRUE is the true solution corresponding to X(j), FERR(j)
*          is an estimated upper bound for the magnitude of the largest
*          element in (X(j) - XTRUE) divided by the magnitude of the
*          largest element in X(j).  The estimate is as reliable as
*          the estimate for RCOND, and is almost always a slight
*          overestimate of the true error.
*
*  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector X(j) (i.e., the smallest relative change in
*          any element of A or B that makes X(j) an exact solution).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (3*N)
*
*  IWORK   (workspace) INTEGER array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, and i is
*                <= N:  D(i,i) is exactly zero.  The factorization
*                       has been completed but the factor D is exactly
*                       singular, so the solution and error bounds could
*                       not be computed. RCOND = 0 is returned.
*                = N+1: D is nonsingular, but RCOND is less than machine
*                       precision, meaning that the matrix is singular
*                       to working precision.  Nevertheless, the
*                       solution and error bounds are computed because
*                       there are a number of situations where the
*                       computed solution can be more accurate than the
*                       value of RCOND would suggest.
*
*  Further Details
*  ===============
*
*  The packed storage scheme is illustrated by the following example
*  when N = 4, UPLO = 'U':
*
*  Two-dimensional storage of the symmetric matrix A:
*
*     a11 a12 a13 a14
*         a22 a23 a24
*             a33 a34     (aij = aji)
*                 a44
*
*  Packed storage of the upper triangle of A:
*
*  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            NOFACT
      DOUBLE PRECISION   ANORM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANSP
      EXTERNAL           LSAME, DLAMCH, DLANSP
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DSPCON, DSPRFS, DSPTRF, DSPTRS,
     $                   XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      NOFACT = LSAME( FACT, 'N' )
      IF( .NOT.NOFACT .AND. .NOT.LSAME( FACT, 'F' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) )
     $          THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -11
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSPSVX', -INFO )
         RETURN
      END IF
*
      IF( NOFACT ) THEN
*
*        Compute the factorization A = U*D*U' or A = L*D*L'.
*
         CALL DCOPY( N*( N+1 ) / 2, AP, 1, AFP, 1 )
         CALL DSPTRF( UPLO, N, AFP, IPIV, INFO )
*
*        Return if INFO is non-zero.
*
         IF( INFO.GT.0 )THEN
            RCOND = ZERO
            RETURN
         END IF
      END IF
*
*     Compute the norm of the matrix A.
*
      ANORM = DLANSP( 'I', UPLO, N, AP, WORK )
*
*     Compute the reciprocal of the condition number of A.
*
      CALL DSPCON( UPLO, N, AFP, IPIV, ANORM, RCOND, WORK, IWORK, INFO )
*
*     Compute the solution vectors X.
*
      CALL DLACPY( 'Full', N, NRHS, B, LDB, X, LDX )
      CALL DSPTRS( UPLO, N, NRHS, AFP, IPIV, X, LDX, INFO )
*
*     Use iterative refinement to improve the computed solutions and
*     compute error bounds and backward error estimates for them.
*
      CALL DSPRFS( UPLO, N, NRHS, AP, AFP, IPIV, B, LDB, X, LDX, FERR,
     $             BERR, WORK, IWORK, INFO )
*
*     Set INFO = N+1 if the matrix is singular to working precision.
*
      IF( RCOND.LT.DLAMCH( 'Epsilon' ) )
     $   INFO = N + 1
*
      RETURN
*
*     End of DSPSVX
*
      END
      SUBROUTINE DSTEDC( COMPZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          COMPZ
      INTEGER            INFO, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSTEDC computes all eigenvalues and, optionally, eigenvectors of a
*  symmetric tridiagonal matrix using the divide and conquer method.
*  The eigenvectors of a full or band real symmetric matrix can also be
*  found if DSYTRD or DSPTRD or DSBTRD has been used to reduce this
*  matrix to tridiagonal form.
*
*  This code makes very mild assumptions about floating point
*  arithmetic. It will work on machines with a guard digit in
*  add/subtract, or on those binary machines without guard digits
*  which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or Cray-2.
*  It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.  See DLAED3 for details.
*
*  Arguments
*  =========
*
*  COMPZ   (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only.
*          = 'I':  Compute eigenvectors of tridiagonal matrix also.
*          = 'V':  Compute eigenvectors of original dense symmetric
*                  matrix also.  On entry, Z contains the orthogonal
*                  matrix used to reduce the original matrix to
*                  tridiagonal form.
*
*  N       (input) INTEGER
*          The dimension of the symmetric tridiagonal matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the diagonal elements of the tridiagonal matrix.
*          On exit, if INFO = 0, the eigenvalues in ascending order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the subdiagonal elements of the tridiagonal matrix.
*          On exit, E has been destroyed.
*
*  Z       (input/output) DOUBLE PRECISION array, dimension (LDZ,N)
*          On entry, if COMPZ = 'V', then Z contains the orthogonal
*          matrix used in the reduction to tridiagonal form.
*          On exit, if INFO = 0, then if COMPZ = 'V', Z contains the
*          orthonormal eigenvectors of the original symmetric matrix,
*          and if COMPZ = 'I', Z contains the orthonormal eigenvectors
*          of the symmetric tridiagonal matrix.
*          If  COMPZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1.
*          If eigenvectors are desired, then LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array,
*                                         dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If COMPZ = 'N' or N <= 1 then LWORK must be at least 1.
*          If COMPZ = 'V' and N > 1 then LWORK must be at least
*                         ( 1 + 3*N + 2*N*lg N + 3*N**2 ),
*                         where lg( N ) = smallest integer k such
*                         that 2**k >= N.
*          If COMPZ = 'I' and N > 1 then LWORK must be at least
*                         ( 1 + 4*N + N**2 ).
*          Note that for COMPZ = 'I' or 'V', then if N is less than or
*          equal to the minimum divide size, usually 25, then LWORK need
*          only be max(1,2*(N-1)).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If COMPZ = 'N' or N <= 1 then LIWORK must be at least 1.
*          If COMPZ = 'V' and N > 1 then LIWORK must be at least
*                         ( 6 + 6*N + 5*N*lg N ).
*          If COMPZ = 'I' and N > 1 then LIWORK must be at least
*                         ( 3 + 5*N ).
*          Note that for COMPZ = 'I' or 'V', then if N is less than or
*          equal to the minimum divide size, usually 25, then LIWORK
*          need only be 1.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal size of the IWORK array,
*          returns this value as the first entry of the IWORK array, and
*          no error message related to LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit.
*          < 0:  if INFO = -i, the i-th argument had an illegal value.
*          > 0:  The algorithm failed to compute an eigenvalue while
*                working on the submatrix lying in rows and columns
*                INFO/(N+1) through mod(INFO,N+1).
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*  Modified by Francoise Tisseur, University of Tennessee.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            FINISH, I, ICOMPZ, II, J, K, LGN, LIWMIN,
     $                   LWMIN, M, SMLSIZ, START, STOREZ, STRTRW
      DOUBLE PRECISION   EPS, ORGNRM, P, TINY
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEMM, DLACPY, DLAED0, DLASCL, DLASET, DLASRT,
     $                   DSTEQR, DSTERF, DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, INT, LOG, MAX, MOD, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
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
      ELSE IF( ( LDZ.LT.1 ) .OR.
     $         ( ICOMPZ.GT.0 .AND. LDZ.LT.MAX( 1, N ) ) ) THEN
         INFO = -6
      END IF
*
      IF( INFO.EQ.0 ) THEN
*
*        Compute the workspace requirements
*
         SMLSIZ = ILAENV( 9, 'DSTEDC', ' ', 0, 0, 0, 0 )
         IF( N.LE.1 .OR. ICOMPZ.EQ.0 ) THEN
            LIWMIN = 1
            LWMIN = 1
         ELSE IF( N.LE.SMLSIZ ) THEN
            LIWMIN = 1
            LWMIN = 2*( N - 1 )
         ELSE
            LGN = INT( LOG( DBLE( N ) )/LOG( TWO ) )
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            IF( 2**LGN.LT.N )
     $         LGN = LGN + 1
            IF( ICOMPZ.EQ.1 ) THEN
               LWMIN = 1 + 3*N + 2*N*LGN + 3*N**2
               LIWMIN = 6 + 6*N + 5*N*LGN
            ELSE IF( ICOMPZ.EQ.2 ) THEN
               LWMIN = 1 + 4*N + N**2
               LIWMIN = 3 + 5*N
            END IF
         END IF
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
*
         IF( LWORK.LT.LWMIN .AND. .NOT. LQUERY ) THEN
            INFO = -8
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT. LQUERY ) THEN
            INFO = -10
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEDC', -INFO )
         RETURN
      ELSE IF (LQUERY) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
      IF( N.EQ.1 ) THEN
         IF( ICOMPZ.NE.0 )
     $      Z( 1, 1 ) = ONE
         RETURN
      END IF
*
*     If the following conditional clause is removed, then the routine
*     will use the Divide and Conquer routine to compute only the
*     eigenvalues, which requires (3N + 3N**2) real workspace and
*     (2 + 5N + 2N lg(N)) integer workspace.
*     Since on many architectures DSTERF is much faster than any other
*     algorithm for finding eigenvalues only, it is used here
*     as the default. If the conditional clause is removed, then
*     information on the size of workspace needs to be changed.
*
*     If COMPZ = 'N', use DSTERF to compute the eigenvalues.
*
      IF( ICOMPZ.EQ.0 ) THEN
         CALL DSTERF( N, D, E, INFO )
         GO TO 50
      END IF
*
*     If N is smaller than the minimum divide size (SMLSIZ+1), then
*     solve the problem with another solver.
*
      IF( N.LE.SMLSIZ ) THEN
*
         CALL DSTEQR( COMPZ, N, D, E, Z, LDZ, WORK, INFO )
*
      ELSE
*
*        If COMPZ = 'V', the Z matrix must be stored elsewhere for later
*        use.
*
         IF( ICOMPZ.EQ.1 ) THEN
            STOREZ = 1 + N*N
         ELSE
            STOREZ = 1
         END IF
*
         IF( ICOMPZ.EQ.2 ) THEN
            CALL DLASET( 'Full', N, N, ZERO, ONE, Z, LDZ )
         END IF
*
*        Scale.
*
         ORGNRM = DLANST( 'M', N, D, E )
         IF( ORGNRM.EQ.ZERO )
     $      GO TO 50
*
         EPS = DLAMCH( 'Epsilon' )
*
         START = 1
*
*        while ( START <= N )
*
   10    CONTINUE
         IF( START.LE.N ) THEN
*
*           Let FINISH be the position of the next subdiagonal entry
*           such that E( FINISH ) <= TINY or FINISH = N if no such
*           subdiagonal exists.  The matrix identified by the elements
*           between START and FINISH constitutes an independent
*           sub-problem.
*
            FINISH = START
   20       CONTINUE
            IF( FINISH.LT.N ) THEN
               TINY = EPS*SQRT( ABS( D( FINISH ) ) )*
     $                    SQRT( ABS( D( FINISH+1 ) ) )
               IF( ABS( E( FINISH ) ).GT.TINY ) THEN
                  FINISH = FINISH + 1
                  GO TO 20
               END IF
            END IF
*
*           (Sub) Problem determined.  Compute its size and solve it.
*
            M = FINISH - START + 1
            IF( M.EQ.1 ) THEN
               START = FINISH + 1
               GO TO 10
            END IF
            IF( M.GT.SMLSIZ ) THEN
*
*              Scale.
*
               ORGNRM = DLANST( 'M', M, D( START ), E( START ) )
               CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, M, 1, D( START ), M,
     $                      INFO )
               CALL DLASCL( 'G', 0, 0, ORGNRM, ONE, M-1, 1, E( START ),
     $                      M-1, INFO )
*
               IF( ICOMPZ.EQ.1 ) THEN
                  STRTRW = 1
               ELSE
                  STRTRW = START
               END IF
               CALL DLAED0( ICOMPZ, N, M, D( START ), E( START ),
     $                      Z( STRTRW, START ), LDZ, WORK( 1 ), N,
     $                      WORK( STOREZ ), IWORK, INFO )
               IF( INFO.NE.0 ) THEN
                  INFO = ( INFO / ( M+1 )+START-1 )*( N+1 ) +
     $                   MOD( INFO, ( M+1 ) ) + START - 1
                  GO TO 50
               END IF
*
*              Scale back.
*
               CALL DLASCL( 'G', 0, 0, ONE, ORGNRM, M, 1, D( START ), M,
     $                      INFO )
*
            ELSE
               IF( ICOMPZ.EQ.1 ) THEN
*
*                 Since QR won't update a Z matrix which is larger than
*                 the length of D, we must solve the sub-problem in a
*                 workspace and then multiply back into Z.
*
                  CALL DSTEQR( 'I', M, D( START ), E( START ), WORK, M,
     $                         WORK( M*M+1 ), INFO )
                  CALL DLACPY( 'A', N, M, Z( 1, START ), LDZ,
     $                         WORK( STOREZ ), N )
                  CALL DGEMM( 'N', 'N', N, M, M, ONE,
     $                        WORK( STOREZ ), N, WORK, M, ZERO,
     $                        Z( 1, START ), LDZ )
               ELSE IF( ICOMPZ.EQ.2 ) THEN
                  CALL DSTEQR( 'I', M, D( START ), E( START ),
     $                         Z( START, START ), LDZ, WORK, INFO )
               ELSE
                  CALL DSTERF( M, D( START ), E( START ), INFO )
               END IF
               IF( INFO.NE.0 ) THEN
                  INFO = START*( N+1 ) + FINISH
                  GO TO 50
               END IF
            END IF
*
            START = FINISH + 1
            GO TO 10
         END IF
*
*        endwhile
*
*        If the problem split any number of times, then the eigenvalues
*        will not be properly ordered.  Here we permute the eigenvalues
*        (and the associated eigenvectors) into ascending order.
*
         IF( M.NE.N ) THEN
            IF( ICOMPZ.EQ.0 ) THEN
*
*              Use Quick Sort
*
               CALL DLASRT( 'I', N, D, INFO )
*
            ELSE
*
*              Use Selection Sort to minimize swaps of eigenvectors
*
               DO 40 II = 2, N
                  I = II - 1
                  K = I
                  P = D( I )
                  DO 30 J = II, N
                     IF( D( J ).LT.P ) THEN
                        K = J
                        P = D( J )
                     END IF
   30             CONTINUE
                  IF( K.NE.I ) THEN
                     D( K ) = D( I )
                     D( I ) = P
                     CALL DSWAP( N, Z( 1, I ), 1, Z( 1, K ), 1 )
                  END IF
   40          CONTINUE
            END IF
         END IF
      END IF
*
   50 CONTINUE
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of DSTEDC
*
      END
      SUBROUTINE DSTEV( JOBZ, N, D, E, Z, LDZ, WORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ
      INTEGER            INFO, LDZ, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSTEV computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric tridiagonal matrix A.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal matrix
*          A.
*          On exit, if INFO = 0, the eigenvalues in ascending order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix A, stored in elements 1 to N-1 of E.
*          On exit, the contents of E are destroyed.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
*          eigenvectors of the matrix A, with the i-th column of Z
*          holding the eigenvector associated with D(i).
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (max(1,2*N-2))
*          If JOBZ = 'N', WORK is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm failed to converge; i
*                off-diagonal elements of E did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            WANTZ
      INTEGER            IMAX, ISCALE
      DOUBLE PRECISION   BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA, SMLNUM,
     $                   TNRM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSCAL, DSTEQR, DSTERF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -6
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEV ', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 )
     $   RETURN
*
      IF( N.EQ.1 ) THEN
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      ISCALE = 0
      TNRM = DLANST( 'M', N, D, E )
      IF( TNRM.GT.ZERO .AND. TNRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / TNRM
      ELSE IF( TNRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / TNRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         CALL DSCAL( N, SIGMA, D, 1 )
         CALL DSCAL( N-1, SIGMA, E( 1 ), 1 )
      END IF
*
*     For eigenvalues only, call DSTERF.  For eigenvalues and
*     eigenvectors, call DSTEQR.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, D, E, INFO )
      ELSE
         CALL DSTEQR( 'I', N, D, E, Z, LDZ, WORK, INFO )
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
         CALL DSCAL( IMAX, ONE / SIGMA, D, 1 )
      END IF
*
      RETURN
*
*     End of DSTEV
*
      END
      SUBROUTINE DSTEVD( JOBZ, N, D, E, Z, LDZ, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ
      INTEGER            INFO, LDZ, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSTEVD computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric tridiagonal matrix. If eigenvectors are desired, it
*  uses a divide and conquer algorithm.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal matrix
*          A.
*          On exit, if INFO = 0, the eigenvalues in ascending order.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix A, stored in elements 1 to N-1 of E.
*          On exit, the contents of E are destroyed.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, N)
*          If JOBZ = 'V', then if INFO = 0, Z contains the orthonormal
*          eigenvectors of the matrix A, with the i-th column of Z
*          holding the eigenvector associated with D(i).
*          If JOBZ = 'N', then Z is not referenced.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array,
*                                         dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If JOBZ  = 'N' or N <= 1 then LWORK must be at least 1.
*          If JOBZ  = 'V' and N > 1 then LWORK must be at least
*                         ( 1 + 4*N + N**2 ).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal sizes of the WORK and IWORK
*          arrays, returns these values as the first entries of the WORK
*          and IWORK arrays, and no error message related to LWORK or
*          LIWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If JOBZ  = 'N' or N <= 1 then LIWORK must be at least 1.
*          If JOBZ  = 'V' and N > 1 then LIWORK must be at least 3+5*N.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal sizes of the WORK and
*          IWORK arrays, returns these values as the first entries of
*          the WORK and IWORK arrays, and no error message related to
*          LWORK or LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, the algorithm failed to converge; i
*                off-diagonal elements of E did not converge to zero.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, WANTZ
      INTEGER            ISCALE, LIWMIN, LWMIN
      DOUBLE PRECISION   BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA, SMLNUM,
     $                   TNRM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSCAL, DSTEDC, DSTERF, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
*
      INFO = 0
      LIWMIN = 1
      LWMIN = 1
      IF( N.GT.1 .AND. WANTZ ) THEN
         LWMIN = 1 + 4*N + N**2
         LIWMIN = 3 + 5*N
      END IF
*
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -6
      END IF
*
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
*
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -8
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -10
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEVD', -INFO )
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
      IF( N.EQ.1 ) THEN
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      ISCALE = 0
      TNRM = DLANST( 'M', N, D, E )
      IF( TNRM.GT.ZERO .AND. TNRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / TNRM
      ELSE IF( TNRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / TNRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         CALL DSCAL( N, SIGMA, D, 1 )
         CALL DSCAL( N-1, SIGMA, E( 1 ), 1 )
      END IF
*
*     For eigenvalues only, call DSTERF.  For eigenvalues and
*     eigenvectors, call DSTEDC.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, D, E, INFO )
      ELSE
         CALL DSTEDC( 'I', N, D, E, Z, LDZ, WORK, LWORK, IWORK, LIWORK,
     $                INFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( ISCALE.EQ.1 )
     $   CALL DSCAL( N, ONE / SIGMA, D, 1 )
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of DSTEVD
*
      END
      SUBROUTINE DSTEVR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL,
     $                   M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE
      INTEGER            IL, INFO, IU, LDZ, LIWORK, LWORK, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSTEVR computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric tridiagonal matrix T.  Eigenvalues and
*  eigenvectors can be selected by specifying either a range of values
*  or a range of indices for the desired eigenvalues.
*
*  Whenever possible, DSTEVR calls DSTEMR to compute the
*  eigenspectrum using Relatively Robust Representations.  DSTEMR
*  computes eigenvalues by the dqds algorithm, while orthogonal
*  eigenvectors are computed from various "good" L D L^T representations
*  (also known as Relatively Robust Representations). Gram-Schmidt
*  orthogonalization is avoided as far as possible. More specifically,
*  the various steps of the algorithm are as follows. For the i-th
*  unreduced block of T,
*     (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
*          is a relatively robust representation,
*     (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
*         relative accuracy by the dqds algorithm,
*     (c) If there is a cluster of close eigenvalues, "choose" sigma_i
*         close to the cluster, and go to step (a),
*     (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
*         compute the corresponding eigenvector by forming a
*         rank-revealing twisted factorization.
*  The desired accuracy of the output can be specified by the input
*  parameter ABSTOL.
*
*  For more details, see "A new O(n^2) algorithm for the symmetric
*  tridiagonal eigenvalue/eigenvector problem", by Inderjit Dhillon,
*  Computer Science Division Technical Report No. UCB//CSD-97-971,
*  UC Berkeley, May 1997.
*
*
*  Note 1 : DSTEVR calls DSTEMR when the full spectrum is requested
*  on machines which conform to the ieee-754 floating point standard.
*  DSTEVR calls DSTEBZ and DSTEIN on non-ieee machines and
*  when partial spectrum requests are made.
*
*  Normal execution of DSTEMR may create NaNs and infinities and
*  hence may abort due to a floating point exception in environments
*  which do not handle NaNs and infinities in the ieee standard default
*  manner.
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
********** For RANGE = 'V' or 'I' and IU - IL < N - 1, DSTEBZ and
********** DSTEIN are called
*
*  N       (input) INTEGER
*          The order of the matrix.  N >= 0.
*
*  D       (input/output) DOUBLE PRECISION array, dimension (N)
*          On entry, the n diagonal elements of the tridiagonal matrix
*          A.
*          On exit, D may be multiplied by a constant factor chosen
*          to avoid over/underflow in computing the eigenvalues.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (max(1,N-1))
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix A in elements 1 to N-1 of E.
*          On exit, E may be multiplied by a constant factor chosen
*          to avoid over/underflow in computing the eigenvalues.
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing A to tridiagonal form.
*
*          See "Computing Small Singular Values of Bidiagonal Matrices
*          with Guaranteed High Relative Accuracy," by Demmel and
*          Kahan, LAPACK Working Note #3.
*
*          If high relative accuracy is important, set ABSTOL to
*          DLAMCH( 'Safe minimum' ).  Doing so will guarantee that
*          eigenvalues are computed to high relative accuracy when
*          possible in future releases.  The current code does not
*          make any guarantees about high relative accuracy, but
*          future releases will. See J. Barlow and J. Demmel,
*          "Computing Accurate Eigensystems of Scaled Diagonally
*          Dominant Matrices", LAPACK Working Note #7, for a discussion
*          of which matrices define their eigenvalues to high relative
*          accuracy.
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
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  ISUPPZ  (output) INTEGER array, dimension ( 2*max(1,M) )
*          The support of the eigenvectors in Z, i.e., the indices
*          indicating the nonzero elements in Z. The i-th eigenvector
*          is nonzero only in elements ISUPPZ( 2*i-1 ) through
*          ISUPPZ( 2*i ).
********** Implemented only for RANGE = 'A' or 'I' and IU - IL = N - 1
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal (and
*          minimal) LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,20*N).
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal sizes of the WORK and IWORK
*          arrays, returns these values as the first entries of the WORK
*          and IWORK arrays, and no error message related to LWORK or
*          LIWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the optimal (and
*          minimal) LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.  LIWORK >= max(1,10*N).
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal sizes of the WORK and
*          IWORK arrays, returns these values as the first entries of
*          the WORK and IWORK arrays, and no error message related to
*          LWORK or LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  Internal error
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*     Ken Stanley, Computer Science Division, University of
*       California at Berkeley, USA
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 2.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, TEST, LQUERY, VALEIG, WANTZ,
     $                   TRYRAC
      CHARACTER          ORDER
      INTEGER            I, IEEEOK, IMAX, INDIBL, INDIFL, INDISP,
     $                   INDIWO, ISCALE, ITMP1, J, JJ, LIWMIN, LWMIN,
     $                   NSPLIT
      DOUBLE PRECISION   BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA, SMLNUM,
     $                   TMP1, TNRM, VLL, VUU
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DSCAL, DSTEBZ, DSTEMR, DSTEIN, DSTERF,
     $                   DSWAP, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*
*     Test the input parameters.
*
      IEEEOK = ILAENV( 10, 'DSTEVR', 'N', 1, 2, 3, 4 )
*
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
*
      LQUERY = ( ( LWORK.EQ.-1 ) .OR. ( LIWORK.EQ.-1 ) )
      LWMIN = MAX( 1, 20*N )
      LIWMIN = MAX( 1, 10*N )
*
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -7
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -8
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -9
            END IF
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
            INFO = -14
         END IF
      END IF
*
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
*
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -17
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -19
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEVR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
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
            IF( VL.LT.D( 1 ) .AND. VU.GE.D( 1 ) ) THEN
               M = 1
               W( 1 ) = D( 1 )
            END IF
         END IF
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      VLL = VL
      VUU = VU
*
      TNRM = DLANST( 'M', N, D, E )
      IF( TNRM.GT.ZERO .AND. TNRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / TNRM
      ELSE IF( TNRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / TNRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         CALL DSCAL( N, SIGMA, D, 1 )
         CALL DSCAL( N-1, SIGMA, E( 1 ), 1 )
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         END IF
      END IF

*     Initialize indices into workspaces.  Note: These indices are used only
*     if DSTERF or DSTEMR fail.

*     IWORK(INDIBL:INDIBL+M-1) corresponds to IBLOCK in DSTEBZ and
*     stores the block indices of each of the M<=N eigenvalues.
      INDIBL = 1
*     IWORK(INDISP:INDISP+NSPLIT-1) corresponds to ISPLIT in DSTEBZ and
*     stores the starting and finishing indices of each block.
      INDISP = INDIBL + N
*     IWORK(INDIFL:INDIFL+N-1) stores the indices of eigenvectors
*     that corresponding to eigenvectors that fail to converge in
*     DSTEIN.  This information is discarded; if any fail, the driver
*     returns INFO > 0.
      INDIFL = INDISP + N
*     INDIWO is the offset of the remaining integer workspace.
      INDIWO = INDISP + N
*
*     If all eigenvalues are desired, then
*     call DSTERF or DSTEMR.  If this fails for some eigenvalue, then
*     try DSTEBZ.
*
*
      TEST = .FALSE.
      IF( INDEIG ) THEN
         IF( IL.EQ.1 .AND. IU.EQ.N ) THEN
            TEST = .TRUE.
         END IF
      END IF
      IF( ( ALLEIG .OR. TEST ) .AND. IEEEOK.EQ.1 ) THEN
         CALL DCOPY( N-1, E( 1 ), 1, WORK( 1 ), 1 )
         IF( .NOT.WANTZ ) THEN
            CALL DCOPY( N, D, 1, W, 1 )
            CALL DSTERF( N, W, WORK, INFO )
         ELSE
            CALL DCOPY( N, D, 1, WORK( N+1 ), 1 )
            IF (ABSTOL .LE. TWO*N*EPS) THEN
               TRYRAC = .TRUE.
            ELSE
               TRYRAC = .FALSE.
            END IF
            CALL DSTEMR( JOBZ, 'A', N, WORK( N+1 ), WORK, VL, VU, IL,
     $                   IU, M, W, Z, LDZ, N, ISUPPZ, TRYRAC,
     $                   WORK( 2*N+1 ), LWORK-2*N, IWORK, LIWORK, INFO )
*
         END IF
         IF( INFO.EQ.0 ) THEN
            M = N
            GO TO 10
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call DSTEBZ and, if eigenvectors are desired, DSTEIN.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF

      CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTOL, D, E, M,
     $             NSPLIT, W, IWORK( INDIBL ), IWORK( INDISP ), WORK,
     $             IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL DSTEIN( N, D, E, M, W, IWORK( INDIBL ), IWORK( INDISP ),
     $                Z, LDZ, WORK, IWORK( INDIWO ), IWORK( INDIFL ),
     $                INFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
   10 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         END IF
         CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
      IF( WANTZ ) THEN
         DO 30 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 20 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   20       CONTINUE
*
            IF( I.NE.0 ) THEN
               ITMP1 = IWORK( I )
               W( I ) = W( J )
               IWORK( I ) = IWORK( J )
               W( J ) = TMP1
               IWORK( J ) = ITMP1
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
            END IF
   30    CONTINUE
      END IF
*
*      Causes problems with tests 19 & 20:
*      IF (wantz .and. INDEIG ) Z( 1,1) = Z(1,1) / 1.002 + .002
*
*
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
      RETURN
*
*     End of DSTEVR
*
      END
      SUBROUTINE DSTEVX( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, ABSTOL,
     $                   M, W, Z, LDZ, WORK, IWORK, IFAIL, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE
      INTEGER            IL, INFO, IU, LDZ, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSTEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric tridiagonal matrix A.  Eigenvalues and
*  eigenvectors can be selected by specifying either a range of values
*  or a range of indices for the desired eigenvalues.
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
*          On entry, the n diagonal elements of the tridiagonal matrix
*          A.
*          On exit, D may be multiplied by a constant factor chosen
*          to avoid over/underflow in computing the eigenvalues.
*
*  E       (input/output) DOUBLE PRECISION array, dimension (max(1,N-1))
*          On entry, the (n-1) subdiagonal elements of the tridiagonal
*          matrix A in elements 1 to N-1 of E.
*          On exit, E may be multiplied by a constant factor chosen
*          to avoid over/underflow in computing the eigenvalues.
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less
*          than or equal to zero, then  EPS*|T|  will be used in
*          its place, where |T| is the 1-norm of the tridiagonal
*          matrix.
*
*          Eigenvalues will be computed most accurately when ABSTOL is
*          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
*          If this routine returns with INFO>0, indicating that some
*          eigenvectors did not converge, try setting ABSTOL to
*          2*DLAMCH('S').
*
*          See "Computing Small Singular Values of Bidiagonal Matrices
*          with Guaranteed High Relative Accuracy," by Demmel and
*          Kahan, LAPACK Working Note #3.
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
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          If an eigenvector fails to converge (INFO > 0), then that
*          column of Z contains the latest approximation to the
*          eigenvector, and the index of the eigenvector is returned
*          in IFAIL.  If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (5*N)
*
*  IWORK   (workspace) INTEGER array, dimension (5*N)
*
*  IFAIL   (output) INTEGER array, dimension (N)
*          If JOBZ = 'V', then if INFO = 0, the first M elements of
*          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*          indices of the eigenvectors that failed to converge.
*          If JOBZ = 'N', then IFAIL is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, then i eigenvectors failed to converge.
*                Their indices are stored in array IFAIL.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, TEST, VALEIG, WANTZ
      CHARACTER          ORDER
      INTEGER            I, IMAX, INDIBL, INDISP, INDIWO, INDWRK,
     $                   ISCALE, ITMP1, J, JJ, NSPLIT
      DOUBLE PRECISION   BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA, SMLNUM,
     $                   TMP1, TNRM, VLL, VUU
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, DLAMCH, DLANST
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DSCAL, DSTEBZ, DSTEIN, DSTEQR, DSTERF,
     $                   DSWAP, XERBLA
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
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -7
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -8
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -9
            END IF
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) )
     $      INFO = -14
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEVX', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
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
            IF( VL.LT.D( 1 ) .AND. VU.GE.D( 1 ) ) THEN
               M = 1
               W( 1 ) = D( 1 )
            END IF
         END IF
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      IF( VALEIG ) THEN
         VLL = VL
         VUU = VU
      ELSE
         VLL = ZERO
         VUU = ZERO
      END IF
      TNRM = DLANST( 'M', N, D, E )
      IF( TNRM.GT.ZERO .AND. TNRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / TNRM
      ELSE IF( TNRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / TNRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         CALL DSCAL( N, SIGMA, D, 1 )
         CALL DSCAL( N-1, SIGMA, E( 1 ), 1 )
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         END IF
      END IF
*
*     If all eigenvalues are desired and ABSTOL is less than zero, then
*     call DSTERF or SSTEQR.  If this fails for some eigenvalue, then
*     try DSTEBZ.
*
      TEST = .FALSE.
      IF( INDEIG ) THEN
         IF( IL.EQ.1 .AND. IU.EQ.N ) THEN
            TEST = .TRUE.
         END IF
      END IF
      IF( ( ALLEIG .OR. TEST ) .AND. ( ABSTOL.LE.ZERO ) ) THEN
         CALL DCOPY( N, D, 1, W, 1 )
         CALL DCOPY( N-1, E( 1 ), 1, WORK( 1 ), 1 )
         INDWRK = N + 1
         IF( .NOT.WANTZ ) THEN
            CALL DSTERF( N, W, WORK, INFO )
         ELSE
            CALL DSTEQR( 'I', N, W, WORK, Z, LDZ, WORK( INDWRK ), INFO )
            IF( INFO.EQ.0 ) THEN
               DO 10 I = 1, N
                  IFAIL( I ) = 0
   10          CONTINUE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            M = N
            GO TO 20
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call DSTEBZ and, if eigenvectors are desired, SSTEIN.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF
      INDWRK = 1
      INDIBL = 1
      INDISP = INDIBL + N
      INDIWO = INDISP + N
      CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTOL, D, E, M,
     $             NSPLIT, W, IWORK( INDIBL ), IWORK( INDISP ),
     $             WORK( INDWRK ), IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL DSTEIN( N, D, E, M, W, IWORK( INDIBL ), IWORK( INDISP ),
     $                Z, LDZ, WORK( INDWRK ), IWORK( INDIWO ), IFAIL,
     $                INFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
   20 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         END IF
         CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
      IF( WANTZ ) THEN
         DO 40 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 30 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   30       CONTINUE
*
            IF( I.NE.0 ) THEN
               ITMP1 = IWORK( INDIBL+I-1 )
               W( I ) = W( J )
               IWORK( INDIBL+I-1 ) = IWORK( INDIBL+J-1 )
               W( J ) = TMP1
               IWORK( INDIBL+J-1 ) = ITMP1
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
               IF( INFO.NE.0 ) THEN
                  ITMP1 = IFAIL( I )
                  IFAIL( I ) = IFAIL( J )
                  IFAIL( J ) = ITMP1
               END IF
            END IF
   40    CONTINUE
      END IF
*
      RETURN
*
*     End of DSTEVX
*
      END
      SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, LDA, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DSYEV computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric matrix A.
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the
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
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= max(1,3*N-1).
*          For optimal efficiency, LWORK >= (NB+2)*N,
*          where NB is the blocksize for DSYTRD returned by ILAENV.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
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
*     ..
*     .. Local Scalars ..
      LOGICAL            LOWER, LQUERY, WANTZ
      INTEGER            IINFO, IMAX, INDE, INDTAU, INDWRK, ISCALE,
     $                   LLWORK, LWKOPT, NB
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLASCL, DORGTR, DSCAL, DSTEQR, DSTERF, DSYTRD,
     $                   XERBLA
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
      END IF
*
      IF( INFO.EQ.0 ) THEN
         NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
         LWKOPT = MAX( 1, ( NB+2 )*N )
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.MAX( 1, 3*N-1 ) .AND. .NOT.LQUERY )
     $      INFO = -8
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYEV ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 ) THEN
         RETURN
      END IF
*
      IF( N.EQ.1 ) THEN
         W( 1 ) = A( 1, 1 )
         WORK( 1 ) = 2
         IF( WANTZ )
     $      A( 1, 1 ) = ONE
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
      ANRM = DLANSY( 'M', UPLO, N, A, LDA, WORK )
      ISCALE = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 )
     $   CALL DLASCL( UPLO, 0, 0, ONE, SIGMA, N, N, A, LDA, INFO )
*
*     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
*
      INDE = 1
      INDTAU = INDE + N
      INDWRK = INDTAU + N
      LLWORK = LWORK - INDWRK + 1
      CALL DSYTRD( UPLO, N, A, LDA, W, WORK( INDE ), WORK( INDTAU ),
     $             WORK( INDWRK ), LLWORK, IINFO )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, first call
*     DORGTR to generate the orthogonal matrix, then call DSTEQR.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         CALL DORGTR( UPLO, N, A, LDA, WORK( INDTAU ), WORK( INDWRK ),
     $                LLWORK, IINFO )
         CALL DSTEQR( JOBZ, N, W, WORK( INDE ), A, LDA, WORK( INDTAU ),
     $                INFO )
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
*     Set WORK(1) to optimal workspace size.
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DSYEV
*
      END
      SUBROUTINE DSYEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK,
     $                   LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, LDA, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DSYEVD computes all eigenvalues and, optionally, eigenvectors of a
*  real symmetric matrix A. If eigenvectors are desired, it uses a
*  divide and conquer algorithm.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Because of large use of BLAS of level 3, DSYEVD needs N**2 more
*  workspace than DSYEVX.
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
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the
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
*  WORK    (workspace/output) DOUBLE PRECISION array,
*                                         dimension (LWORK)
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If N <= 1,               LWORK must be at least 1.
*          If JOBZ = 'N' and N > 1, LWORK must be at least 2*N+1.
*          If JOBZ = 'V' and N > 1, LWORK must be at least
*                                                1 + 6*N + 2*N**2.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal sizes of the WORK and IWORK
*          arrays, returns these values as the first entries of the WORK
*          and IWORK arrays, and no error message related to LWORK or
*          LIWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If N <= 1,                LIWORK must be at least 1.
*          If JOBZ  = 'N' and N > 1, LIWORK must be at least 1.
*          If JOBZ  = 'V' and N > 1, LIWORK must be at least 3 + 5*N.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal sizes of the WORK and
*          IWORK arrays, returns these values as the first entries of
*          the WORK and IWORK arrays, and no error message related to
*          LWORK or LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i and JOBZ = 'N', then the algorithm failed
*                to converge; i off-diagonal elements of an intermediate
*                tridiagonal form did not converge to zero;
*                if INFO = i and JOBZ = 'V', then the algorithm failed
*                to compute an eigenvalue while working on the submatrix
*                lying in rows and columns INFO/(N+1) through
*                mod(INFO,N+1).
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Jeff Rutter, Computer Science Division, University of California
*     at Berkeley, USA
*  Modified by Francoise Tisseur, University of Tennessee.
*
*  Modified description of INFO. Sven, 16 Feb 05.
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
*
      LOGICAL            LOWER, LQUERY, WANTZ
      INTEGER            IINFO, INDE, INDTAU, INDWK2, INDWRK, ISCALE,
     $                   LIOPT, LIWMIN, LLWORK, LLWRK2, LOPT, LWMIN
      DOUBLE PRECISION   ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, SIGMA,
     $                   SMLNUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           LSAME, DLAMCH, DLANSY, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLACPY, DLASCL, DORMTR, DSCAL, DSTEDC, DSTERF,
     $                   DSYTRD, XERBLA
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
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
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
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( N.LE.1 ) THEN
            LIWMIN = 1
            LWMIN = 1
            LOPT = LWMIN
            LIOPT = LIWMIN
         ELSE
            IF( WANTZ ) THEN
               LIWMIN = 3 + 5*N
               LWMIN = 1 + 6*N + 2*N**2
            ELSE
               LIWMIN = 1
               LWMIN = 2*N + 1
            END IF
            LOPT = MAX( LWMIN, 2*N +
     $                  ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 ) )
            LIOPT = LIWMIN
         END IF
         WORK( 1 ) = LOPT
         IWORK( 1 ) = LIOPT
*
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -8
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -10
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYEVD', -INFO )
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
      IF( N.EQ.1 ) THEN
         W( 1 ) = A( 1, 1 )
         IF( WANTZ )
     $      A( 1, 1 ) = ONE
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
      ANRM = DLANSY( 'M', UPLO, N, A, LDA, WORK )
      ISCALE = 0
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 )
     $   CALL DLASCL( UPLO, 0, 0, ONE, SIGMA, N, N, A, LDA, INFO )
*
*     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
*
      INDE = 1
      INDTAU = INDE + N
      INDWRK = INDTAU + N
      LLWORK = LWORK - INDWRK + 1
      INDWK2 = INDWRK + N*N
      LLWRK2 = LWORK - INDWK2 + 1
*
      CALL DSYTRD( UPLO, N, A, LDA, W, WORK( INDE ), WORK( INDTAU ),
     $             WORK( INDWRK ), LLWORK, IINFO )
      LOPT = 2*N + WORK( INDWRK )
*
*     For eigenvalues only, call DSTERF.  For eigenvectors, first call
*     DSTEDC to generate the eigenvector matrix, WORK(INDWRK), of the
*     tridiagonal matrix, then call DORMTR to multiply it by the
*     Householder transformations stored in A.
*
      IF( .NOT.WANTZ ) THEN
         CALL DSTERF( N, W, WORK( INDE ), INFO )
      ELSE
         CALL DSTEDC( 'I', N, W, WORK( INDE ), WORK( INDWRK ), N,
     $                WORK( INDWK2 ), LLWRK2, IWORK, LIWORK, INFO )
         CALL DORMTR( 'L', UPLO, 'N', N, N, A, LDA, WORK( INDTAU ),
     $                WORK( INDWRK ), N, WORK( INDWK2 ), LLWRK2, IINFO )
         CALL DLACPY( 'A', N, N, WORK( INDWRK ), N, A, LDA )
         LOPT = MAX( LOPT, 1+6*N+2*N**2 )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
      IF( ISCALE.EQ.1 )
     $   CALL DSCAL( N, ONE / SIGMA, W, 1 )
*
      WORK( 1 ) = LOPT
      IWORK( 1 ) = LIOPT
*
      RETURN
*
*     End of DSYEVD
*
      END
      SUBROUTINE DSYEVR( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU,
     $                   ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK,
     $                   IWORK, LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, IU, LDA, LDZ, LIWORK, LWORK, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSYEVR computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
*  selected by specifying either a range of values or a range of
*  indices for the desired eigenvalues.
*
*  DSYEVR first reduces the matrix A to tridiagonal form T with a call
*  to DSYTRD.  Then, whenever possible, DSYEVR calls DSTEMR to compute
*  the eigenspectrum using Relatively Robust Representations.  DSTEMR
*  computes eigenvalues by the dqds algorithm, while orthogonal
*  eigenvectors are computed from various "good" L D L^T representations
*  (also known as Relatively Robust Representations). Gram-Schmidt
*  orthogonalization is avoided as far as possible. More specifically,
*  the various steps of the algorithm are as follows.
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
*  The desired accuracy of the output can be specified by the input
*  parameter ABSTOL.
*
*  For more details, see DSTEMR's documentation and:
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
*
*  Note 1 : DSYEVR calls DSTEMR when the full spectrum is requested
*  on machines which conform to the ieee-754 floating point standard.
*  DSYEVR calls DSTEBZ and SSTEIN on non-ieee machines and
*  when partial spectrum requests are made.
*
*  Normal execution of DSTEMR may create NaNs and infinities and
*  hence may abort due to a floating point exception in environments
*  which do not handle NaNs and infinities in the ieee standard default
*  manner.
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
********** For RANGE = 'V' or 'I' and IU - IL < N - 1, DSTEBZ and
********** DSTEIN are called
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of A contains the
*          upper triangular part of the matrix A.  If UPLO = 'L',
*          the leading N-by-N lower triangular part of A contains
*          the lower triangular part of the matrix A.
*          On exit, the lower triangle (if UPLO='L') or the upper
*          triangle (if UPLO='U') of A, including the diagonal, is
*          destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing A to tridiagonal form.
*
*          See "Computing Small Singular Values of Bidiagonal Matrices
*          with Guaranteed High Relative Accuracy," by Demmel and
*          Kahan, LAPACK Working Note #3.
*
*          If high relative accuracy is important, set ABSTOL to
*          DLAMCH( 'Safe minimum' ).  Doing so will guarantee that
*          eigenvalues are computed to high relative accuracy when
*          possible in future releases.  The current code does not
*          make any guarantees about high relative accuracy, but
*          future releases will. See J. Barlow and J. Demmel,
*          "Computing Accurate Eigensystems of Scaled Diagonally
*          Dominant Matrices", LAPACK Working Note #7, for a discussion
*          of which matrices define their eigenvalues to high relative
*          accuracy.
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          The first M elements contain the selected eigenvalues in
*          ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*          Supplying N columns is always safe.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  ISUPPZ  (output) INTEGER array, dimension ( 2*max(1,M) )
*          The support of the eigenvectors in Z, i.e., the indices
*          indicating the nonzero elements in Z. The i-th eigenvector
*          is nonzero only in elements ISUPPZ( 2*i-1 ) through
*          ISUPPZ( 2*i ).
********** Implemented only for RANGE = 'A' or 'I' and IU - IL = N - 1
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.  LWORK >= max(1,26*N).
*          For optimal efficiency, LWORK >= (NB+6)*N,
*          where NB is the max of the blocksize for DSYTRD and DORMTR
*          returned by ILAENV.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the optimal LWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.  LIWORK >= max(1,10*N).
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal size of the IWORK array,
*          returns this value as the first entry of the IWORK array, and
*          no error message related to LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  Internal error
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Inderjit Dhillon, IBM Almaden, USA
*     Osni Marques, LBNL/NERSC, USA
*     Ken Stanley, Computer Science Division, University of
*       California at Berkeley, USA
*     Jason Riedy, Computer Science Division, University of
*       California at Berkeley, USA
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LOWER, LQUERY, VALEIG, WANTZ,
     $                   TRYRAC
      CHARACTER          ORDER
      INTEGER            I, IEEEOK, IINFO, IMAX, INDD, INDDD, INDE,
     $                   INDEE, INDIBL, INDIFL, INDISP, INDIWO, INDTAU,
     $                   INDWK, INDWKN, ISCALE, J, JJ, LIWMIN,
     $                   LLWORK, LLWRKN, LWKOPT, LWMIN, NB, NSPLIT
      DOUBLE PRECISION   ABSTLL, ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN,
     $                   SIGMA, SMLNUM, TMP1, VLL, VUU
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DORMTR, DSCAL, DSTEBZ, DSTEMR, DSTEIN,
     $                   DSTERF, DSWAP, DSYTRD, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      IEEEOK = ILAENV( 10, 'DSYEVR', 'N', 1, 2, 3, 4 )
*
      LOWER = LSAME( UPLO, 'L' )
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
*
      LQUERY = ( ( LWORK.EQ.-1 ) .OR. ( LIWORK.EQ.-1 ) )
*
      LWMIN = MAX( 1, 26*N )
      LIWMIN = MAX( 1, 10*N )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -8
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -9
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -10
            END IF
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
            INFO = -15
         ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -18
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -20
         END IF
      END IF
*
      IF( INFO.EQ.0 ) THEN
         NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
         NB = MAX( NB, ILAENV( 1, 'DORMTR', UPLO, N, -1, -1, -1 ) )
         LWKOPT = MAX( ( NB+1 )*N, LWMIN )
         WORK( 1 ) = LWKOPT
         IWORK( 1 ) = LIWMIN
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYEVR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      END IF
*
      IF( N.EQ.1 ) THEN
         WORK( 1 ) = 7
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = A( 1, 1 )
         ELSE
            IF( VL.LT.A( 1, 1 ) .AND. VU.GE.A( 1, 1 ) ) THEN
               M = 1
               W( 1 ) = A( 1, 1 )
            END IF
         END IF
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      ABSTLL = ABSTOL
      VLL = VL
      VUU = VU
      ANRM = DLANSY( 'M', UPLO, N, A, LDA, WORK )
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         IF( LOWER ) THEN
            DO 10 J = 1, N
               CALL DSCAL( N-J+1, SIGMA, A( J, J ), 1 )
   10       CONTINUE
         ELSE
            DO 20 J = 1, N
               CALL DSCAL( J, SIGMA, A( 1, J ), 1 )
   20       CONTINUE
         END IF
         IF( ABSTOL.GT.0 )
     $      ABSTLL = ABSTOL*SIGMA
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         END IF
      END IF

*     Initialize indices into workspaces.  Note: The IWORK indices are
*     used only if DSTERF or DSTEMR fail.

*     WORK(INDTAU:INDTAU+N-1) stores the scalar factors of the
*     elementary reflectors used in DSYTRD.
      INDTAU = 1
*     WORK(INDD:INDD+N-1) stores the tridiagonal's diagonal entries.
      INDD = INDTAU + N
*     WORK(INDE:INDE+N-1) stores the off-diagonal entries of the
*     tridiagonal matrix from DSYTRD.
      INDE = INDD + N
*     WORK(INDDD:INDDD+N-1) is a copy of the diagonal entries over
*     -written by DSTEMR (the DSTERF path copies the diagonal to W).
      INDDD = INDE + N
*     WORK(INDEE:INDEE+N-1) is a copy of the off-diagonal entries over
*     -written while computing the eigenvalues in DSTERF and DSTEMR.
      INDEE = INDDD + N
*     INDWK is the starting offset of the left-over workspace, and
*     LLWORK is the remaining workspace size.
      INDWK = INDEE + N
      LLWORK = LWORK - INDWK + 1

*     IWORK(INDIBL:INDIBL+M-1) corresponds to IBLOCK in DSTEBZ and
*     stores the block indices of each of the M<=N eigenvalues.
      INDIBL = 1
*     IWORK(INDISP:INDISP+NSPLIT-1) corresponds to ISPLIT in DSTEBZ and
*     stores the starting and finishing indices of each block.
      INDISP = INDIBL + N
*     IWORK(INDIFL:INDIFL+N-1) stores the indices of eigenvectors
*     that corresponding to eigenvectors that fail to converge in
*     DSTEIN.  This information is discarded; if any fail, the driver
*     returns INFO > 0.
      INDIFL = INDISP + N
*     INDIWO is the offset of the remaining integer workspace.
      INDIWO = INDISP + N

*
*     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
*
      CALL DSYTRD( UPLO, N, A, LDA, WORK( INDD ), WORK( INDE ),
     $             WORK( INDTAU ), WORK( INDWK ), LLWORK, IINFO )
*
*     If all eigenvalues are desired
*     then call DSTERF or DSTEMR and DORMTR.
*
      IF( ( ALLEIG .OR. ( INDEIG .AND. IL.EQ.1 .AND. IU.EQ.N ) ) .AND.
     $    IEEEOK.EQ.1 ) THEN
         IF( .NOT.WANTZ ) THEN
            CALL DCOPY( N, WORK( INDD ), 1, W, 1 )
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DSTERF( N, W, WORK( INDEE ), INFO )
         ELSE
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DCOPY( N, WORK( INDD ), 1, WORK( INDDD ), 1 )
*
            IF (ABSTOL .LE. TWO*N*EPS) THEN
               TRYRAC = .TRUE.
            ELSE
               TRYRAC = .FALSE.
            END IF
            CALL DSTEMR( JOBZ, 'A', N, WORK( INDDD ), WORK( INDEE ),
     $                   VL, VU, IL, IU, M, W, Z, LDZ, N, ISUPPZ,
     $                   TRYRAC, WORK( INDWK ), LWORK, IWORK, LIWORK,
     $                   INFO )
*
*
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by DSTEIN.
*
            IF( WANTZ .AND. INFO.EQ.0 ) THEN
               INDWKN = INDE
               LLWRKN = LWORK - INDWKN + 1
               CALL DORMTR( 'L', UPLO, 'N', N, M, A, LDA,
     $                      WORK( INDTAU ), Z, LDZ, WORK( INDWKN ),
     $                      LLWRKN, IINFO )
            END IF
         END IF
*
*
         IF( INFO.EQ.0 ) THEN
*           Everything worked.  Skip DSTEBZ/DSTEIN.  IWORK(:) are
*           undefined.
            M = N
            GO TO 30
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call DSTEBZ and, if eigenvectors are desired, DSTEIN.
*     Also call DSTEBZ and DSTEIN if DSTEMR fails.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF

      CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL,
     $             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
     $             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWK ),
     $             IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
     $                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
     $                WORK( INDWK ), IWORK( INDIWO ), IWORK( INDIFL ),
     $                INFO )
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by DSTEIN.
*
         INDWKN = INDE
         LLWRKN = LWORK - INDWKN + 1
         CALL DORMTR( 'L', UPLO, 'N', N, M, A, LDA, WORK( INDTAU ), Z,
     $                LDZ, WORK( INDWKN ), LLWRKN, IINFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
*  Jump here if DSTEMR/DSTEIN succeeded.
   30 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         END IF
         CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.  Note: We do not sort the IFAIL portion of IWORK.
*     It may not be initialized (if DSTEMR/DSTEIN succeeded), and we do
*     not return this detailed information to the user.
*
      IF( WANTZ ) THEN
         DO 50 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 40 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   40       CONTINUE
*
            IF( I.NE.0 ) THEN
               W( I ) = W( J )
               W( J ) = TMP1
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
            END IF
   50    CONTINUE
      END IF
*
*     Set WORK(1) to optimal workspace size.
*
      WORK( 1 ) = LWKOPT
      IWORK( 1 ) = LIWMIN
*
      RETURN
*
*     End of DSYEVR
*
      END
      SUBROUTINE DSYEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU,
     $                   ABSTOL, M, W, Z, LDZ, WORK, LWORK, IWORK,
     $                   IFAIL, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, IU, LDA, LDZ, LWORK, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * ), Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSYEVX computes selected eigenvalues and, optionally, eigenvectors
*  of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
*  selected by specifying either a range of values or a range of indices
*  for the desired eigenvalues.
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
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The order of the matrix A.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of A contains the
*          upper triangular part of the matrix A.  If UPLO = 'L',
*          the leading N-by-N lower triangular part of A contains
*          the lower triangular part of the matrix A.
*          On exit, the lower triangle (if UPLO='L') or the upper
*          triangle (if UPLO='U') of A, including the diagonal, is
*          destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing A to tridiagonal form.
*
*          Eigenvalues will be computed most accurately when ABSTOL is
*          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
*          If this routine returns with INFO>0, indicating that some
*          eigenvectors did not converge, try setting ABSTOL to
*          2*DLAMCH('S').
*
*          See "Computing Small Singular Values of Bidiagonal Matrices
*          with Guaranteed High Relative Accuracy," by Demmel and
*          Kahan, LAPACK Working Note #3.
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          On normal exit, the first M elements contain the selected
*          eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          If an eigenvector fails to converge, then that column of Z
*          contains the latest approximation to the eigenvector, and the
*          index of the eigenvector is returned in IFAIL.
*          If JOBZ = 'N', then Z is not referenced.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= 1, when N <= 1;
*          otherwise 8*N.
*          For optimal efficiency, LWORK >= (NB+3)*N,
*          where NB is the max of the blocksize for DSYTRD and DORMTR
*          returned by ILAENV.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace) INTEGER array, dimension (5*N)
*
*  IFAIL   (output) INTEGER array, dimension (N)
*          If JOBZ = 'V', then if INFO = 0, the first M elements of
*          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*          indices of the eigenvectors that failed to converge.
*          If JOBZ = 'N', then IFAIL is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  if INFO = i, then i eigenvectors failed to converge.
*                Their indices are stored in array IFAIL.
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LOWER, LQUERY, TEST, VALEIG,
     $                   WANTZ
      CHARACTER          ORDER
      INTEGER            I, IINFO, IMAX, INDD, INDE, INDEE, INDIBL,
     $                   INDISP, INDIWO, INDTAU, INDWKN, INDWRK, ISCALE,
     $                   ITMP1, J, JJ, LLWORK, LLWRKN, LWKMIN,
     $                   LWKOPT, NB, NSPLIT
      DOUBLE PRECISION   ABSTLL, ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN,
     $                   SIGMA, SMLNUM, TMP1, VLL, VUU
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DCOPY, DLACPY, DORGTR, DORMTR, DSCAL, DSTEBZ,
     $                   DSTEIN, DSTEQR, DSTERF, DSWAP, DSYTRD, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      LOWER = LSAME( UPLO, 'L' )
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
      LQUERY = ( LWORK.EQ.-1 )
*
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -8
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -9
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -10
            END IF
         END IF
      END IF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
            INFO = -15
         END IF
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( N.LE.1 ) THEN
            LWKMIN = 1
            WORK( 1 ) = LWKMIN
         ELSE
            LWKMIN = 8*N
            NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
            NB = MAX( NB, ILAENV( 1, 'DORMTR', UPLO, N, -1, -1, -1 ) )
            LWKOPT = MAX( LWKMIN, ( NB + 3 )*N )
            WORK( 1 ) = LWKOPT
         END IF
*
         IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY )
     $      INFO = -17
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYEVX', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 ) THEN
         RETURN
      END IF
*
      IF( N.EQ.1 ) THEN
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = A( 1, 1 )
         ELSE
            IF( VL.LT.A( 1, 1 ) .AND. VU.GE.A( 1, 1 ) ) THEN
               M = 1
               W( 1 ) = A( 1, 1 )
            END IF
         END IF
         IF( WANTZ )
     $      Z( 1, 1 ) = ONE
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
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
*
*     Scale matrix to allowable range, if necessary.
*
      ISCALE = 0
      ABSTLL = ABSTOL
      IF( VALEIG ) THEN
         VLL = VL
         VUU = VU
      END IF
      ANRM = DLANSY( 'M', UPLO, N, A, LDA, WORK )
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      END IF
      IF( ISCALE.EQ.1 ) THEN
         IF( LOWER ) THEN
            DO 10 J = 1, N
               CALL DSCAL( N-J+1, SIGMA, A( J, J ), 1 )
   10       CONTINUE
         ELSE
            DO 20 J = 1, N
               CALL DSCAL( J, SIGMA, A( 1, J ), 1 )
   20       CONTINUE
         END IF
         IF( ABSTOL.GT.0 )
     $      ABSTLL = ABSTOL*SIGMA
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         END IF
      END IF
*
*     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
*
      INDTAU = 1
      INDE = INDTAU + N
      INDD = INDE + N
      INDWRK = INDD + N
      LLWORK = LWORK - INDWRK + 1
      CALL DSYTRD( UPLO, N, A, LDA, WORK( INDD ), WORK( INDE ),
     $             WORK( INDTAU ), WORK( INDWRK ), LLWORK, IINFO )
*
*     If all eigenvalues are desired and ABSTOL is less than or equal to
*     zero, then call DSTERF or DORGTR and SSTEQR.  If this fails for
*     some eigenvalue, then try DSTEBZ.
*
      TEST = .FALSE.
      IF( INDEIG ) THEN
         IF( IL.EQ.1 .AND. IU.EQ.N ) THEN
            TEST = .TRUE.
         END IF
      END IF
      IF( ( ALLEIG .OR. TEST ) .AND. ( ABSTOL.LE.ZERO ) ) THEN
         CALL DCOPY( N, WORK( INDD ), 1, W, 1 )
         INDEE = INDWRK + 2*N
         IF( .NOT.WANTZ ) THEN
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DSTERF( N, W, WORK( INDEE ), INFO )
         ELSE
            CALL DLACPY( 'A', N, N, A, LDA, Z, LDZ )
            CALL DORGTR( UPLO, N, Z, LDZ, WORK( INDTAU ),
     $                   WORK( INDWRK ), LLWORK, IINFO )
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DSTEQR( JOBZ, N, W, WORK( INDEE ), Z, LDZ,
     $                   WORK( INDWRK ), INFO )
            IF( INFO.EQ.0 ) THEN
               DO 30 I = 1, N
                  IFAIL( I ) = 0
   30          CONTINUE
            END IF
         END IF
         IF( INFO.EQ.0 ) THEN
            M = N
            GO TO 40
         END IF
         INFO = 0
      END IF
*
*     Otherwise, call DSTEBZ and, if eigenvectors are desired, SSTEIN.
*
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      END IF
      INDIBL = 1
      INDISP = INDIBL + N
      INDIWO = INDISP + N
      CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL,
     $             WORK( INDD ), WORK( INDE ), M, NSPLIT, W,
     $             IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWRK ),
     $             IWORK( INDIWO ), INFO )
*
      IF( WANTZ ) THEN
         CALL DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W,
     $                IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ,
     $                WORK( INDWRK ), IWORK( INDIWO ), IFAIL, INFO )
*
*        Apply orthogonal matrix used in reduction to tridiagonal
*        form to eigenvectors returned by DSTEIN.
*
         INDWKN = INDE
         LLWRKN = LWORK - INDWKN + 1
         CALL DORMTR( 'L', UPLO, 'N', N, M, A, LDA, WORK( INDTAU ), Z,
     $                LDZ, WORK( INDWKN ), LLWRKN, IINFO )
      END IF
*
*     If matrix was scaled, then rescale eigenvalues appropriately.
*
   40 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         END IF
         CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
      END IF
*
*     If eigenvalues are not in order, then sort them, along with
*     eigenvectors.
*
      IF( WANTZ ) THEN
         DO 60 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 50 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               END IF
   50       CONTINUE
*
            IF( I.NE.0 ) THEN
               ITMP1 = IWORK( INDIBL+I-1 )
               W( I ) = W( J )
               IWORK( INDIBL+I-1 ) = IWORK( INDIBL+J-1 )
               W( J ) = TMP1
               IWORK( INDIBL+J-1 ) = ITMP1
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
               IF( INFO.NE.0 ) THEN
                  ITMP1 = IFAIL( I )
                  IFAIL( I ) = IFAIL( J )
                  IFAIL( J ) = ITMP1
               END IF
            END IF
   60    CONTINUE
      END IF
*
*     Set WORK(1) to optimal workspace size.
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DSYEVX
*
      END
      SUBROUTINE DSYGV( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,
     $                  LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, ITYPE, LDA, LDB, LWORK, N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), W( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DSYGV computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.
*  Here A and B are assumed to be symmetric and B is also
*  positive definite.
*
*  Arguments
*  =========
*
*  ITYPE   (input) INTEGER
*          Specifies the problem type to be solved:
*          = 1:  A*x = (lambda)*B*x
*          = 2:  A*B*x = (lambda)*x
*          = 3:  B*A*x = (lambda)*x
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangles of A and B are stored;
*          = 'L':  Lower triangles of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of A contains the
*          upper triangular part of the matrix A.  If UPLO = 'L',
*          the leading N-by-N lower triangular part of A contains
*          the lower triangular part of the matrix A.
*
*          On exit, if JOBZ = 'V', then if INFO = 0, A contains the
*          matrix Z of eigenvectors.  The eigenvectors are normalized
*          as follows:
*          if ITYPE = 1 or 2, Z**T*B*Z = I;
*          if ITYPE = 3, Z**T*inv(B)*Z = I.
*          If JOBZ = 'N', then on exit the upper triangle (if UPLO='U')
*          or the lower triangle (if UPLO='L') of A, including the
*          diagonal, is destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the symmetric positive definite matrix B.
*          If UPLO = 'U', the leading N-by-N upper triangular part of B
*          contains the upper triangular part of the matrix B.
*          If UPLO = 'L', the leading N-by-N lower triangular part of B
*          contains the lower triangular part of the matrix B.
*
*          On exit, if INFO <= N, the part of B containing the matrix is
*          overwritten by the triangular factor U or L from the Cholesky
*          factorization B = U**T*U or B = L*L**T.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= max(1,3*N-1).
*          For optimal efficiency, LWORK >= (NB+2)*N,
*          where NB is the blocksize for DSYTRD returned by ILAENV.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  DPOTRF or DSYEV returned an error code:
*             <= N:  if INFO = i, DSYEV failed to converge;
*                    i off-diagonal elements of an intermediate
*                    tridiagonal form did not converge to zero;
*             > N:   if INFO = N + i, for 1 <= i <= N, then the leading
*                    minor of order i of B is not positive definite.
*                    The factorization of B could not be completed and
*                    no eigenvalues or eigenvectors were computed.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER, WANTZ
      CHARACTER          TRANS
      INTEGER            LWKMIN, LWKOPT, NB, NEIG
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPOTRF, DSYEV, DSYGST, DTRMM, DTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
*
      INFO = 0
      IF( ITYPE.LT.1 .OR. ITYPE.GT.3 ) THEN
         INFO = -1
      ELSE IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
*
      IF( INFO.EQ.0 ) THEN
         LWKMIN = MAX( 1, 3*N - 1 )
         NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
         LWKOPT = MAX( LWKMIN, ( NB + 2 )*N )
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
            INFO = -11
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYGV ', -INFO )
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
*     Form a Cholesky factorization of B.
*
      CALL DPOTRF( UPLO, N, B, LDB, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem and solve.
*
      CALL DSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
      CALL DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
*
      IF( WANTZ ) THEN
*
*        Backtransform eigenvectors to the original problem.
*
         NEIG = N
         IF( INFO.GT.0 )
     $      NEIG = INFO - 1
         IF( ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
*
*           For A*x=(lambda)*B*x and A*B*x=(lambda)*x;
*           backtransform eigenvectors: x = inv(L)'*y or inv(U)*y
*
            IF( UPPER ) THEN
               TRANS = 'N'
            ELSE
               TRANS = 'T'
            END IF
*
            CALL DTRSM( 'Left', UPLO, TRANS, 'Non-unit', N, NEIG, ONE,
     $                  B, LDB, A, LDA )
*
         ELSE IF( ITYPE.EQ.3 ) THEN
*
*           For B*A*x=(lambda)*x;
*           backtransform eigenvectors: x = L*y or U'*y
*
            IF( UPPER ) THEN
               TRANS = 'T'
            ELSE
               TRANS = 'N'
            END IF
*
            CALL DTRMM( 'Left', UPLO, TRANS, 'Non-unit', N, NEIG, ONE,
     $                  B, LDB, A, LDA )
         END IF
      END IF
*
      WORK( 1 ) = LWKOPT
      RETURN
*
*     End of DSYGV
*
      END
      SUBROUTINE DSYGVD( ITYPE, JOBZ, UPLO, N, A, LDA, B, LDB, W, WORK,
     $                   LWORK, IWORK, LIWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, UPLO
      INTEGER            INFO, ITYPE, LDA, LDB, LIWORK, LWORK, N
*     ..
*     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), W( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DSYGVD computes all the eigenvalues, and optionally, the eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
*  B are assumed to be symmetric and B is also positive definite.
*  If eigenvectors are desired, it uses a divide and conquer algorithm.
*
*  The divide and conquer algorithm makes very mild assumptions about
*  floating point arithmetic. It will work on machines with a guard
*  digit in add/subtract, or on those binary machines without guard
*  digits which subtract like the Cray X-MP, Cray Y-MP, Cray C-90, or
*  Cray-2. It could conceivably fail on hexadecimal or decimal machines
*  without guard digits, but we know of none.
*
*  Arguments
*  =========
*
*  ITYPE   (input) INTEGER
*          Specifies the problem type to be solved:
*          = 1:  A*x = (lambda)*B*x
*          = 2:  A*B*x = (lambda)*x
*          = 3:  B*A*x = (lambda)*x
*
*  JOBZ    (input) CHARACTER*1
*          = 'N':  Compute eigenvalues only;
*          = 'V':  Compute eigenvalues and eigenvectors.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangles of A and B are stored;
*          = 'L':  Lower triangles of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrices A and B.  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of A contains the
*          upper triangular part of the matrix A.  If UPLO = 'L',
*          the leading N-by-N lower triangular part of A contains
*          the lower triangular part of the matrix A.
*
*          On exit, if JOBZ = 'V', then if INFO = 0, A contains the
*          matrix Z of eigenvectors.  The eigenvectors are normalized
*          as follows:
*          if ITYPE = 1 or 2, Z**T*B*Z = I;
*          if ITYPE = 3, Z**T*inv(B)*Z = I.
*          If JOBZ = 'N', then on exit the upper triangle (if UPLO='U')
*          or the lower triangle (if UPLO='L') of A, including the
*          diagonal, is destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB, N)
*          On entry, the symmetric matrix B.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of B contains the
*          upper triangular part of the matrix B.  If UPLO = 'L',
*          the leading N-by-N lower triangular part of B contains
*          the lower triangular part of the matrix B.
*
*          On exit, if INFO <= N, the part of B containing the matrix is
*          overwritten by the triangular factor U or L from the Cholesky
*          factorization B = U**T*U or B = L*L**T.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          If INFO = 0, the eigenvalues in ascending order.
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The dimension of the array WORK.
*          If N <= 1,               LWORK >= 1.
*          If JOBZ = 'N' and N > 1, LWORK >= 2*N+1.
*          If JOBZ = 'V' and N > 1, LWORK >= 1 + 6*N + 2*N**2.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal sizes of the WORK and IWORK
*          arrays, returns these values as the first entries of the WORK
*          and IWORK arrays, and no error message related to LWORK or
*          LIWORK is issued by XERBLA.
*
*  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
*          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
*
*  LIWORK  (input) INTEGER
*          The dimension of the array IWORK.
*          If N <= 1,                LIWORK >= 1.
*          If JOBZ  = 'N' and N > 1, LIWORK >= 1.
*          If JOBZ  = 'V' and N > 1, LIWORK >= 3 + 5*N.
*
*          If LIWORK = -1, then a workspace query is assumed; the
*          routine only calculates the optimal sizes of the WORK and
*          IWORK arrays, returns these values as the first entries of
*          the WORK and IWORK arrays, and no error message related to
*          LWORK or LIWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  DPOTRF or DSYEVD returned an error code:
*             <= N:  if INFO = i and JOBZ = 'N', then the algorithm
*                    failed to converge; i off-diagonal elements of an
*                    intermediate tridiagonal form did not converge to
*                    zero;
*                    if INFO = i and JOBZ = 'V', then the algorithm
*                    failed to compute an eigenvalue while working on
*                    the submatrix lying in rows and columns INFO/(N+1)
*                    through mod(INFO,N+1);
*             > N:   if INFO = N + i, for 1 <= i <= N, then the leading
*                    minor of order i of B is not positive definite.
*                    The factorization of B could not be completed and
*                    no eigenvalues or eigenvectors were computed.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA
*
*  Modified so that no backsubstitution is performed if DSYEVD fails to
*  converge (NEIG in old code could be greater than N causing out of
*  bounds reference to A - reported by Ralf Meyer).  Also corrected the
*  description of INFO and the test on ITYPE. Sven, 16 Feb 05.
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER, WANTZ
      CHARACTER          TRANS
      INTEGER            LIOPT, LIWMIN, LOPT, LWMIN
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPOTRF, DSYEVD, DSYGST, DTRMM, DTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      WANTZ = LSAME( JOBZ, 'V' )
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 .OR. LIWORK.EQ.-1 )
*
      INFO = 0
      IF( N.LE.1 ) THEN
         LIWMIN = 1
         LWMIN = 1
      ELSE IF( WANTZ ) THEN
         LIWMIN = 3 + 5*N
         LWMIN = 1 + 6*N + 2*N**2
      ELSE
         LIWMIN = 1
         LWMIN = 2*N + 1
      END IF
      LOPT = LWMIN
      LIOPT = LIWMIN
      IF( ITYPE.LT.1 .OR. ITYPE.GT.3 ) THEN
         INFO = -1
      ELSE IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      END IF
*
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LOPT
         IWORK( 1 ) = LIOPT
*
         IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -11
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -13
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYGVD', -INFO )
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
*     Form a Cholesky factorization of B.
*
      CALL DPOTRF( UPLO, N, B, LDB, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem and solve.
*
      CALL DSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
      CALL DSYEVD( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, IWORK, LIWORK,
     $             INFO )
      LOPT = MAX( DBLE( LOPT ), DBLE( WORK( 1 ) ) )
      LIOPT = MAX( DBLE( LIOPT ), DBLE( IWORK( 1 ) ) )
*
      IF( WANTZ .AND. INFO.EQ.0 ) THEN
*
*        Backtransform eigenvectors to the original problem.
*
         IF( ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
*
*           For A*x=(lambda)*B*x and A*B*x=(lambda)*x;
*           backtransform eigenvectors: x = inv(L)'*y or inv(U)*y
*
            IF( UPPER ) THEN
               TRANS = 'N'
            ELSE
               TRANS = 'T'
            END IF
*
            CALL DTRSM( 'Left', UPLO, TRANS, 'Non-unit', N, N, ONE,
     $                  B, LDB, A, LDA )
*
         ELSE IF( ITYPE.EQ.3 ) THEN
*
*           For B*A*x=(lambda)*x;
*           backtransform eigenvectors: x = L*y or U'*y
*
            IF( UPPER ) THEN
               TRANS = 'T'
            ELSE
               TRANS = 'N'
            END IF
*
            CALL DTRMM( 'Left', UPLO, TRANS, 'Non-unit', N, N, ONE,
     $                  B, LDB, A, LDA )
         END IF
      END IF
*
      WORK( 1 ) = LOPT
      IWORK( 1 ) = LIOPT
*
      RETURN
*
*     End of DSYGVD
*
      END
      SUBROUTINE DSYGVX( ITYPE, JOBZ, RANGE, UPLO, N, A, LDA, B, LDB,
     $                   VL, VU, IL, IU, ABSTOL, M, W, Z, LDZ, WORK,
     $                   LWORK, IWORK, IFAIL, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, ITYPE, IU, LDA, LDB, LDZ, LWORK, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
*     ..
*     .. Array Arguments ..
      INTEGER            IFAIL( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), W( * ), WORK( * ),
     $                   Z( LDZ, * )
*     ..
*
*  Purpose
*  =======
*
*  DSYGVX computes selected eigenvalues, and optionally, eigenvectors
*  of a real generalized symmetric-definite eigenproblem, of the form
*  A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
*  and B are assumed to be symmetric and B is also positive definite.
*  Eigenvalues and eigenvectors can be selected by specifying either a
*  range of values or a range of indices for the desired eigenvalues.
*
*  Arguments
*  =========
*
*  ITYPE   (input) INTEGER
*          Specifies the problem type to be solved:
*          = 1:  A*x = (lambda)*B*x
*          = 2:  A*B*x = (lambda)*x
*          = 3:  B*A*x = (lambda)*x
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
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A and B are stored;
*          = 'L':  Lower triangle of A and B are stored.
*
*  N       (input) INTEGER
*          The order of the matrix pencil (A,B).  N >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of A contains the
*          upper triangular part of the matrix A.  If UPLO = 'L',
*          the leading N-by-N lower triangular part of A contains
*          the lower triangular part of the matrix A.
*
*          On exit, the lower triangle (if UPLO='L') or the upper
*          triangle (if UPLO='U') of A, including the diagonal, is
*          destroyed.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
*          On entry, the symmetric matrix B.  If UPLO = 'U', the
*          leading N-by-N upper triangular part of B contains the
*          upper triangular part of the matrix B.  If UPLO = 'L',
*          the leading N-by-N lower triangular part of B contains
*          the lower triangular part of the matrix B.
*
*          On exit, if INFO <= N, the part of B containing the matrix is
*          overwritten by the triangular factor U or L from the Cholesky
*          factorization B = U**T*U or B = L*L**T.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
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
*          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
*          Not referenced if RANGE = 'A' or 'V'.
*
*  ABSTOL  (input) DOUBLE PRECISION
*          The absolute error tolerance for the eigenvalues.
*          An approximate eigenvalue is accepted as converged
*          when it is determined to lie in an interval [a,b]
*          of width less than or equal to
*
*                  ABSTOL + EPS *   max( |a|,|b| ) ,
*
*          where EPS is the machine precision.  If ABSTOL is less than
*          or equal to zero, then  EPS*|T|  will be used in its place,
*          where |T| is the 1-norm of the tridiagonal matrix obtained
*          by reducing A to tridiagonal form.
*
*          Eigenvalues will be computed most accurately when ABSTOL is
*          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
*          If this routine returns with INFO>0, indicating that some
*          eigenvectors did not converge, try setting ABSTOL to
*          2*DLAMCH('S').
*
*  M       (output) INTEGER
*          The total number of eigenvalues found.  0 <= M <= N.
*          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
*
*  W       (output) DOUBLE PRECISION array, dimension (N)
*          On normal exit, the first M elements contain the selected
*          eigenvalues in ascending order.
*
*  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
*          If JOBZ = 'N', then Z is not referenced.
*          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
*          contain the orthonormal eigenvectors of the matrix A
*          corresponding to the selected eigenvalues, with the i-th
*          column of Z holding the eigenvector associated with W(i).
*          The eigenvectors are normalized as follows:
*          if ITYPE = 1 or 2, Z**T*B*Z = I;
*          if ITYPE = 3, Z**T*inv(B)*Z = I.
*
*          If an eigenvector fails to converge, then that column of Z
*          contains the latest approximation to the eigenvector, and the
*          index of the eigenvector is returned in IFAIL.
*          Note: the user must ensure that at least max(1,M) columns are
*          supplied in the array Z; if RANGE = 'V', the exact value of M
*          is not known in advance and an upper bound must be used.
*
*  LDZ     (input) INTEGER
*          The leading dimension of the array Z.  LDZ >= 1, and if
*          JOBZ = 'V', LDZ >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK >= max(1,8*N).
*          For optimal efficiency, LWORK >= (NB+3)*N,
*          where NB is the blocksize for DSYTRD returned by ILAENV.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace) INTEGER array, dimension (5*N)
*
*  IFAIL   (output) INTEGER array, dimension (N)
*          If JOBZ = 'V', then if INFO = 0, the first M elements of
*          IFAIL are zero.  If INFO > 0, then IFAIL contains the
*          indices of the eigenvectors that failed to converge.
*          If JOBZ = 'N', then IFAIL is not referenced.
*
*  INFO    (output) INTEGER
*          = 0:  successful exit
*          < 0:  if INFO = -i, the i-th argument had an illegal value
*          > 0:  DPOTRF or DSYEVX returned an error code:
*             <= N:  if INFO = i, DSYEVX failed to converge;
*                    i eigenvectors failed to converge.  Their indices
*                    are stored in array IFAIL.
*             > N:   if INFO = N + i, for 1 <= i <= N, then the leading
*                    minor of order i of B is not positive definite.
*                    The factorization of B could not be completed and
*                    no eigenvalues or eigenvectors were computed.
*
*  Further Details
*  ===============
*
*  Based on contributions by
*     Mark Fahey, Department of Mathematics, Univ. of Kentucky, USA
*
* =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LQUERY, UPPER, VALEIG, WANTZ
      CHARACTER          TRANS
      INTEGER            LWKMIN, LWKOPT, NB
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DPOTRF, DSYEVX, DSYGST, DTRMM, DTRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      UPPER = LSAME( UPLO, 'U' )
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
      LQUERY = ( LWORK.EQ.-1 )
*
      INFO = 0
      IF( ITYPE.LT.1 .OR. ITYPE.GT.3 ) THEN
         INFO = -1
      ELSE IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -3
      ELSE IF( .NOT.( UPPER .OR. LSAME( UPLO, 'L' ) ) ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -7
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL )
     $         INFO = -11
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -12
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -13
            END IF
         END IF
      END IF
      IF (INFO.EQ.0) THEN
         IF (LDZ.LT.1 .OR. (WANTZ .AND. LDZ.LT.N)) THEN
            INFO = -18
         END IF
      END IF
*
      IF( INFO.EQ.0 ) THEN
         LWKMIN = MAX( 1, 8*N )
         NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
         LWKOPT = MAX( LWKMIN, ( NB + 3 )*N )
         WORK( 1 ) = LWKOPT
*
         IF( LWORK.LT.LWKMIN .AND. .NOT.LQUERY ) THEN
            INFO = -20
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYGVX', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Quick return if possible
*
      M = 0
      IF( N.EQ.0 ) THEN
         RETURN
      END IF
*
*     Form a Cholesky factorization of B.
*
      CALL DPOTRF( UPLO, N, B, LDB, INFO )
      IF( INFO.NE.0 ) THEN
         INFO = N + INFO
         RETURN
      END IF
*
*     Transform problem to standard eigenvalue problem and solve.
*
      CALL DSYGST( ITYPE, UPLO, N, A, LDA, B, LDB, INFO )
      CALL DSYEVX( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, ABSTOL,
     $             M, W, Z, LDZ, WORK, LWORK, IWORK, IFAIL, INFO )
*
      IF( WANTZ ) THEN
*
*        Backtransform eigenvectors to the original problem.
*
         IF( INFO.GT.0 )
     $      M = INFO - 1
         IF( ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
*
*           For A*x=(lambda)*B*x and A*B*x=(lambda)*x;
*           backtransform eigenvectors: x = inv(L)'*y or inv(U)*y
*
            IF( UPPER ) THEN
               TRANS = 'N'
            ELSE
               TRANS = 'T'
            END IF
*
            CALL DTRSM( 'Left', UPLO, TRANS, 'Non-unit', N, M, ONE, B,
     $                  LDB, Z, LDZ )
*
         ELSE IF( ITYPE.EQ.3 ) THEN
*
*           For B*A*x=(lambda)*x;
*           backtransform eigenvectors: x = L*y or U'*y
*
            IF( UPPER ) THEN
               TRANS = 'T'
            ELSE
               TRANS = 'N'
            END IF
*
            CALL DTRMM( 'Left', UPLO, TRANS, 'Non-unit', N, M, ONE, B,
     $                  LDB, Z, LDZ )
         END IF
      END IF
*
*     Set WORK(1) to optimal workspace size.
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DSYGVX
*
      END
      SUBROUTINE DSYSV( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, WORK,
     $                  LWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LDB, LWORK, N, NRHS
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DSYSV computes the solution to a real system of linear equations
*     A * X = B,
*  where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
*  matrices.
*
*  The diagonal pivoting method is used to factor A as
*     A = U * D * U**T,  if UPLO = 'U', or
*     A = L * D * L**T,  if UPLO = 'L',
*  where U (or L) is a product of permutation and unit upper (lower)
*  triangular matrices, and D is symmetric and block diagonal with
*  1-by-1 and 2-by-2 diagonal blocks.  The factored form of A is then
*  used to solve the system of equations A * X = B.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrix B.  NRHS >= 0.
*
*  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
*          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
*          N-by-N upper triangular part of A contains the upper
*          triangular part of the matrix A, and the strictly lower
*          triangular part of A is not referenced.  If UPLO = 'L', the
*          leading N-by-N lower triangular part of A contains the lower
*          triangular part of the matrix A, and the strictly upper
*          triangular part of A is not referenced.
*
*          On exit, if INFO = 0, the block diagonal matrix D and the
*          multipliers used to obtain the factor U or L from the
*          factorization A = U*D*U**T or A = L*D*L**T as computed by
*          DSYTRF.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  IPIV    (output) INTEGER array, dimension (N)
*          Details of the interchanges and the block structure of D, as
*          determined by DSYTRF.  If IPIV(k) > 0, then rows and columns
*          k and IPIV(k) were interchanged, and D(k,k) is a 1-by-1
*          diagonal block.  If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0,
*          then rows and columns k-1 and -IPIV(k) were interchanged and
*          D(k-1:k,k-1:k) is a 2-by-2 diagonal block.  If UPLO = 'L' and
*          IPIV(k) = IPIV(k+1) < 0, then rows and columns k+1 and
*          -IPIV(k) were interchanged and D(k:k+1,k:k+1) is a 2-by-2
*          diagonal block.
*
*  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          On entry, the N-by-NRHS right hand side matrix B.
*          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of WORK.  LWORK >= 1, and for best performance
*          LWORK >= max(1,N*NB), where NB is the optimal blocksize for
*          DSYTRF.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          > 0: if INFO = i, D(i,i) is exactly zero.  The factorization
*               has been completed, but the block diagonal matrix D is
*               exactly singular, so the solution could not be computed.
*
*  =====================================================================
*
*     .. Local Scalars ..
      LOGICAL            LQUERY
      INTEGER            LWKOPT, NB
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
*     ..
*     .. External Subroutines ..
      EXTERNAL           DSYTRF, DSYTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LWORK.LT.1 .AND. .NOT.LQUERY ) THEN
         INFO = -10
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( N.EQ.0 ) THEN
            LWKOPT = 1
         ELSE
            NB = ILAENV( 1, 'DSYTRF', UPLO, N, -1, -1, -1 )
            LWKOPT = N*NB
         END IF
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYSV ', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
*     Compute the factorization A = U*D*U' or A = L*D*L'.
*
      CALL DSYTRF( UPLO, N, A, LDA, IPIV, WORK, LWORK, INFO )
      IF( INFO.EQ.0 ) THEN
*
*        Solve the system A*X = B, overwriting B with X.
*
         CALL DSYTRS( UPLO, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
*
      END IF
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DSYSV
*
      END
      SUBROUTINE DSYSVX( FACT, UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B,
     $                   LDB, X, LDX, RCOND, FERR, BERR, WORK, LWORK,
     $                   IWORK, INFO )
*
*  -- LAPACK driver routine (version 3.1) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     November 2006
*
*     .. Scalar Arguments ..
      CHARACTER          FACT, UPLO
      INTEGER            INFO, LDA, LDAF, LDB, LDX, LWORK, N, NRHS
      DOUBLE PRECISION   RCOND
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), AF( LDAF, * ), B( LDB, * ),
     $                   BERR( * ), FERR( * ), WORK( * ), X( LDX, * )
*     ..
*
*  Purpose
*  =======
*
*  DSYSVX uses the diagonal pivoting factorization to compute the
*  solution to a real system of linear equations A * X = B,
*  where A is an N-by-N symmetric matrix and X and B are N-by-NRHS
*  matrices.
*
*  Error bounds on the solution and a condition estimate are also
*  provided.
*
*  Description
*  ===========
*
*  The following steps are performed:
*
*  1. If FACT = 'N', the diagonal pivoting method is used to factor A.
*     The form of the factorization is
*        A = U * D * U**T,  if UPLO = 'U', or
*        A = L * D * L**T,  if UPLO = 'L',
*     where U (or L) is a product of permutation and unit upper (lower)
*     triangular matrices, and D is symmetric and block diagonal with
*     1-by-1 and 2-by-2 diagonal blocks.
*
*  2. If some D(i,i)=0, so that D is exactly singular, then the routine
*     returns with INFO = i. Otherwise, the factored form of A is used
*     to estimate the condition number of the matrix A.  If the
*     reciprocal of the condition number is less than machine precision,
*     INFO = N+1 is returned as a warning, but the routine still goes on
*     to solve for X and compute error bounds as described below.
*
*  3. The system of equations is solved for X using the factored form
*     of A.
*
*  4. Iterative refinement is applied to improve the computed solution
*     matrix and calculate error bounds and backward error estimates
*     for it.
*
*  Arguments
*  =========
*
*  FACT    (input) CHARACTER*1
*          Specifies whether or not the factored form of A has been
*          supplied on entry.
*          = 'F':  On entry, AF and IPIV contain the factored form of
*                  A.  AF and IPIV will not be modified.
*          = 'N':  The matrix A will be copied to AF and factored.
*
*  UPLO    (input) CHARACTER*1
*          = 'U':  Upper triangle of A is stored;
*          = 'L':  Lower triangle of A is stored.
*
*  N       (input) INTEGER
*          The number of linear equations, i.e., the order of the
*          matrix A.  N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of right hand sides, i.e., the number of columns
*          of the matrices B and X.  NRHS >= 0.
*
*  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
*          The symmetric matrix A.  If UPLO = 'U', the leading N-by-N
*          upper triangular part of A contains the upper triangular part
*          of the matrix A, and the strictly lower triangular part of A
*          is not referenced.  If UPLO = 'L', the leading N-by-N lower
*          triangular part of A contains the lower triangular part of
*          the matrix A, and the strictly upper triangular part of A is
*          not referenced.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N).
*
*  AF      (input or output) DOUBLE PRECISION array, dimension (LDAF,N)
*          If FACT = 'F', then AF is an input argument and on entry
*          contains the block diagonal matrix D and the multipliers used
*          to obtain the factor U or L from the factorization
*          A = U*D*U**T or A = L*D*L**T as computed by DSYTRF.
*
*          If FACT = 'N', then AF is an output argument and on exit
*          returns the block diagonal matrix D and the multipliers used
*          to obtain the factor U or L from the factorization
*          A = U*D*U**T or A = L*D*L**T.
*
*  LDAF    (input) INTEGER
*          The leading dimension of the array AF.  LDAF >= max(1,N).
*
*  IPIV    (input or output) INTEGER array, dimension (N)
*          If FACT = 'F', then IPIV is an input argument and on entry
*          contains details of the interchanges and the block structure
*          of D, as determined by DSYTRF.
*          If IPIV(k) > 0, then rows and columns k and IPIV(k) were
*          interchanged and D(k,k) is a 1-by-1 diagonal block.
*          If UPLO = 'U' and IPIV(k) = IPIV(k-1) < 0, then rows and
*          columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
*          is a 2-by-2 diagonal block.  If UPLO = 'L' and IPIV(k) =
*          IPIV(k+1) < 0, then rows and columns k+1 and -IPIV(k) were
*          interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
*
*          If FACT = 'N', then IPIV is an output argument and on exit
*          contains details of the interchanges and the block structure
*          of D, as determined by DSYTRF.
*
*  B       (input) DOUBLE PRECISION array, dimension (LDB,NRHS)
*          The N-by-NRHS right hand side matrix B.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B.  LDB >= max(1,N).
*
*  X       (output) DOUBLE PRECISION array, dimension (LDX,NRHS)
*          If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X.
*
*  LDX     (input) INTEGER
*          The leading dimension of the array X.  LDX >= max(1,N).
*
*  RCOND   (output) DOUBLE PRECISION
*          The estimate of the reciprocal condition number of the matrix
*          A.  If RCOND is less than the machine precision (in
*          particular, if RCOND = 0), the matrix is singular to working
*          precision.  This condition is indicated by a return code of
*          INFO > 0.
*
*  FERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The estimated forward error bound for each solution vector
*          X(j) (the j-th column of the solution matrix X).
*          If XTRUE is the true solution corresponding to X(j), FERR(j)
*          is an estimated upper bound for the magnitude of the largest
*          element in (X(j) - XTRUE) divided by the magnitude of the
*          largest element in X(j).  The estimate is as reliable as
*          the estimate for RCOND, and is almost always a slight
*          overestimate of the true error.
*
*  BERR    (output) DOUBLE PRECISION array, dimension (NRHS)
*          The componentwise relative backward error of each solution
*          vector X(j) (i.e., the smallest relative change in
*          any element of A or B that makes X(j) an exact solution).
*
*  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
*          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
*
*  LWORK   (input) INTEGER
*          The length of WORK.  LWORK >= max(1,3*N), and for best
*          performance, when FACT = 'N', LWORK >= max(1,3*N,N*NB), where
*          NB is the optimal blocksize for DSYTRF.
*
*          If LWORK = -1, then a workspace query is assumed; the routine
*          only calculates the optimal size of the WORK array, returns
*          this value as the first entry of the WORK array, and no error
*          message related to LWORK is issued by XERBLA.
*
*  IWORK   (workspace) INTEGER array, dimension (N)
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*          > 0: if INFO = i, and i is
*                <= N:  D(i,i) is exactly zero.  The factorization
*                       has been completed but the factor D is exactly
*                       singular, so the solution and error bounds could
*                       not be computed. RCOND = 0 is returned.
*                = N+1: D is nonsingular, but RCOND is less than machine
*                       precision, meaning that the matrix is singular
*                       to working precision.  Nevertheless, the
*                       solution and error bounds are computed because
*                       there are a number of situations where the
*                       computed solution can be more accurate than the
*                       value of RCOND would suggest.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
*     ..
*     .. Local Scalars ..
      LOGICAL            LQUERY, NOFACT
      INTEGER            LWKOPT, NB
      DOUBLE PRECISION   ANORM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           DLACPY, DSYCON, DSYRFS, DSYTRF, DSYTRS, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input parameters.
*
      INFO = 0
      NOFACT = LSAME( FACT, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.NOFACT .AND. .NOT.LSAME( FACT, 'F' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.LSAME( UPLO, 'U' ) .AND. .NOT.LSAME( UPLO, 'L' ) )
     $          THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE IF( LDAF.LT.MAX( 1, N ) ) THEN
         INFO = -8
      ELSE IF( LDB.LT.MAX( 1, N ) ) THEN
         INFO = -11
      ELSE IF( LDX.LT.MAX( 1, N ) ) THEN
         INFO = -13
      ELSE IF( LWORK.LT.MAX( 1, 3*N ) .AND. .NOT.LQUERY ) THEN
         INFO = -18
      END IF
*
      IF( INFO.EQ.0 ) THEN
         LWKOPT = MAX( 1, 3*N )
         IF( NOFACT ) THEN
            NB = ILAENV( 1, 'DSYTRF', UPLO, N, -1, -1, -1 )
            LWKOPT = MAX( LWKOPT, N*NB )
         END IF
         WORK( 1 ) = LWKOPT
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYSVX', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      END IF
*
      IF( NOFACT ) THEN
*
*        Compute the factorization A = U*D*U' or A = L*D*L'.
*
         CALL DLACPY( UPLO, N, N, A, LDA, AF, LDAF )
         CALL DSYTRF( UPLO, N, AF, LDAF, IPIV, WORK, LWORK, INFO )
*
*        Return if INFO is non-zero.
*
         IF( INFO.GT.0 )THEN
            RCOND = ZERO
            RETURN
         END IF
      END IF
*
*     Compute the norm of the matrix A.
*
      ANORM = DLANSY( 'I', UPLO, N, A, LDA, WORK )
*
*     Compute the reciprocal of the condition number of A.
*
      CALL DSYCON( UPLO, N, AF, LDAF, IPIV, ANORM, RCOND, WORK, IWORK,
     $             INFO )
*
*     Compute the solution vectors X.
*
      CALL DLACPY( 'Full', N, NRHS, B, LDB, X, LDX )
      CALL DSYTRS( UPLO, N, NRHS, AF, LDAF, IPIV, X, LDX, INFO )
*
*     Use iterative refinement to improve the computed solutions and
*     compute error bounds and backward error estimates for them.
*
      CALL DSYRFS( UPLO, N, NRHS, A, LDA, AF, LDAF, IPIV, B, LDB, X,
     $             LDX, FERR, BERR, WORK, IWORK, INFO )
*
*     Set INFO = N+1 if the matrix is singular to working precision.
*
      IF( RCOND.LT.DLAMCH( 'Epsilon' ) )
     $   INFO = N + 1
*
      WORK( 1 ) = LWKOPT
*
      RETURN
*
*     End of DSYSVX
*
      END
