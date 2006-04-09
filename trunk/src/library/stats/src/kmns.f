c Code in this file based on Applied Statistics algorithms 
c (C) Royal Statistical Society 1979
c
C a minimal modification of AS136 to use double precision
C all variables are now declared.
C B.D. Ripley 1998/06/17
C Declaration re-ordering to satisfy "f77 -ansi",  M.Maechler 2001/04/12
C
      SUBROUTINE KMNS(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *    ITRAN, LIVE, ITER, WSS, IFAULT)
C
C     ALGORITHM AS 136  APPL. STATIST. (1979) VOL.28, NO.1
C
C     Divide M points in N-dimensional space into K clusters so that
C     the within cluster sum of squares is minimized.
C
      INTEGER M,N,K,ITER,IFAULT
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K), LIVE(K)
      DOUBLE PRECISION A(M,N), D(M), C(K,N), AN1(K), AN2(K), WSS(K)
C 
      DOUBLE PRECISION DT(2), ZERO, ONE
      INTEGER I,IL,J,L,INDX,IJ,II
      DOUBLE PRECISION BIG, DA, TEMP, DB, DC,AA
C
C     Define BIG to be a very large positive number
C
      DATA BIG /1.E30/, ZERO /0.0/, ONE /1.0/
C
      IFAULT = 3
      IF (K .LE. 1 .OR. K .GE. M) RETURN
      IFAULT = 0
C
C     For each point I, find its two closest centres, IC1(I) and
C     IC2(I).     Assign it to IC1(I).
C
      DO 50 I = 1, M
        IC1(I) = 1
        IC2(I) = 2
        DO 10 IL = 1, 2
          DT(IL) = ZERO
          DO 10 J = 1, N
            DA = A(I,J) - C(IL,J)
            DT(IL) = DT(IL) + DA*DA
   10   CONTINUE
        IF (DT(1) .GT. DT(2)) THEN
          IC1(I) = 2
          IC2(I) = 1
          TEMP = DT(1)
          DT(1) = DT(2)
          DT(2) = TEMP
        END IF
        DO 50 L = 3, K
          DB = ZERO
          DO 30 J = 1, N
            DC = A(I,J) - C(L,J)
            DB = DB + DC*DC
            IF (DB .GE. DT(2)) GO TO 50
   30     CONTINUE
          IF (DB .LT. DT(1)) GO TO 40
          DT(2) = DB
          IC2(I) = L
          GO TO 50
   40     DT(2) = DT(1)
          IC2(I) = IC1(I)
          DT(1) = DB
          IC1(I) = L
   50 CONTINUE
C
C     Update cluster centres to be the average of points contained
C     within them.
C
      DO 70 L = 1, K
        NC(L) = 0
        DO 60 J = 1, N
   60   C(L,J) = ZERO
   70 CONTINUE
      DO 90 I = 1, M
        L = IC1(I)
        NC(L) = NC(L) + 1
        DO 80 J = 1, N
   80   C(L,J) = C(L,J) + A(I,J)
   90 CONTINUE
C
C     Check to see if there is any empty cluster at this stage
C
      DO 120 L = 1, K
        IF (NC(L) .EQ. 0) THEN
          IFAULT = 1
          RETURN
        END IF
        AA = NC(L)
        DO 110 J = 1, N
  110   C(L,J) = C(L,J) / AA
C
C     Initialize AN1, AN2, ITRAN & NCP
C     AN1(L) = NC(L) / (NC(L) - 1)
C     AN2(L) = NC(L) / (NC(L) + 1)
C     ITRAN(L) = 1 if cluster L is updated in the quick-transfer stage,
C              = 0 otherwise
C     In the optimal-transfer stage, NCP(L) stores the step at which
C     cluster L is last updated.
C     In the quick-transfer stage, NCP(L) stores the step at which
C     cluster L is last updated plus M.
C
        AN2(L) = AA / (AA + ONE)
        AN1(L) = BIG
        IF (AA .GT. ONE) AN1(L) = AA / (AA - ONE)
        ITRAN(L) = 1
        NCP(L) = -1
  120 CONTINUE
      INDX = 0
      DO 140 IJ = 1, ITER
C
C     In this stage, there is only one pass through the data.   Each
C     point is re-allocated, if necessary, to the cluster that will
C     induce the maximum reduction in within-cluster sum of squares.
C
        CALL OPTRA(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *        ITRAN, LIVE, INDX)
C
C     Stop if no transfer took place in the last M optimal transfer
C     steps.
C
        IF (INDX .EQ. M) GO TO 150
C
C     Each point is tested in turn to see if it should be re-allocated
C     to the cluster to which it is most likely to be transferred,
C     IC2(I), from its present cluster, IC1(I).   Loop through the
C     data until no further change is to take place.
C
        CALL QTRAN(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *       ITRAN, INDX)
C
C     If there are only two clusters, there is no need to re-enter the
C     optimal transfer stage.
C
        IF (K .EQ. 2) GO TO 150
C
C     NCP has to be set to 0 before entering OPTRA.
C
        DO 130 L = 1, K
  130   NCP(L) = 0
  140 CONTINUE
C
C     Since the specified number of iterations has been exceeded, set
C     IFAULT = 2.   This may indicate unforeseen looping.
C
      IFAULT = 2
C
C     Compute within-cluster sum of squares for each cluster.
C
  150 DO 160 L = 1, K
        WSS(L) = ZERO
        DO 160 J = 1, N
          C(L,J) = ZERO
  160 CONTINUE
      DO 170 I = 1, M
        II = IC1(I)
        DO 170 J = 1, N
          C(II,J) = C(II,J) + A(I,J)
  170 CONTINUE
      DO 190 J = 1, N
        DO 180 L = 1, K
  180   C(L,J) = C(L,J) / FLOAT(NC(L))
        DO 190 I = 1, M
          II = IC1(I)
          DA = A(I,J) - C(II,J)
          WSS(II) = WSS(II) + DA*DA
  190 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE OPTRA(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *      ITRAN, LIVE, INDX)
C
C     ALGORITHM AS 136.1  APPL. STATIST. (1979) VOL.28, NO.1
C
C     This is the optimal transfer stage.
C
C     Each point is re-allocated, if necessary, to the cluster that
C     will induce a maximum reduction in the within-cluster sum of
C     squares.
C
      INTEGER M,N,K,INDX
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K), LIVE(K)
      DOUBLE PRECISION    A(M,N), D(M), C(K,N), AN1(K), AN2(K)

      INTEGER L,I,L1,L2,LL,J
      DOUBLE PRECISION ZERO, ONE
      DOUBLE PRECISION BIG,DE,DF,DA,DB,R2,RR,DC,DD,AL1,ALW,AL2,ALT
C
C     Define BIG to be a very large positive number.
C
      DATA BIG /1.0E30/, ZERO /0.0/, ONE/1.0/
C
C     If cluster L is updated in the last quick-transfer stage, it
C     belongs to the live set throughout this stage.   Otherwise, at
C     each step, it is not in the live set if it has not been updated
C     in the last M optimal transfer steps.
C
      DO 10 L = 1, K
        IF (ITRAN(L) .EQ. 1) LIVE(L) = M + 1
   10 CONTINUE
      DO 100 I = 1, M
        INDX = INDX + 1
        L1 = IC1(I)
        L2 = IC2(I)
        LL = L2
C
C     If point I is the only member of cluster L1, no transfer.
C
        IF (NC(L1) .EQ. 1) GO TO 90
C
C     If L1 has not yet been updated in this stage, no need to
C     re-compute D(I).
C
        IF (NCP(L1) .EQ. 0) GO TO 30
        DE = ZERO
        DO 20 J = 1, N
          DF = A(I,J) - C(L1,J)
          DE = DE + DF*DF
   20   CONTINUE
        D(I) = DE * AN1(L1)
C
C     Find the cluster with minimum R2.
C
   30   DA = ZERO
        DO 40 J = 1, N
          DB = A(I,J) - C(L2,J)
          DA = DA + DB*DB
   40   CONTINUE
        R2 = DA * AN2(L2)
        DO 60 L = 1, K
C
C     If I >= LIVE(L1), then L1 is not in the live set.   If this is
C     true, we only need to consider clusters that are in the live set
C     for possible transfer of point I.   Otherwise, we need to consider
C     all possible clusters.
C
          IF (I .GE. LIVE(L1) .AND. I .GE. LIVE(L) .OR. L .EQ. L1 .OR.
     *        L .EQ. LL) GO TO 60
          RR = R2 / AN2(L)
          DC = ZERO
          DO 50 J = 1, N
            DD = A(I,J) - C(L,J)
            DC = DC + DD*DD
            IF (DC .GE. RR) GO TO 60
   50     CONTINUE
          R2 = DC * AN2(L)
          L2 = L
   60     CONTINUE
          IF (R2 .LT. D(I)) GO TO 70
C
C     If no transfer is necessary, L2 is the new IC2(I).
C
          IC2(I) = L2
          GO TO 90
C
C     Update cluster centres, LIVE, NCP, AN1 & AN2 for clusters L1 and
C     L2, and update IC1(I) & IC2(I).
C
   70     INDX = 0
          LIVE(L1) = M + I
          LIVE(L2) = M + I
          NCP(L1) = I
          NCP(L2) = I
          AL1 = NC(L1)
          ALW = AL1 - ONE
          AL2 = NC(L2)
          ALT = AL2 + ONE
          DO 80 J = 1, N
            C(L1,J) = (C(L1,J) * AL1 - A(I,J)) / ALW
            C(L2,J) = (C(L2,J) * AL2 + A(I,J)) / ALT
   80     CONTINUE
          NC(L1) = NC(L1) - 1
          NC(L2) = NC(L2) + 1
          AN2(L1) = ALW / AL1
          AN1(L1) = BIG
          IF (ALW .GT. ONE) AN1(L1) = ALW / (ALW - ONE)
          AN1(L2) = ALT / AL2
          AN2(L2) = ALT / (ALT + ONE)
          IC1(I) = L2
          IC2(I) = L1
   90   CONTINUE
        IF (INDX .EQ. M) RETURN
  100 CONTINUE
      DO 110 L = 1, K
C
C     ITRAN(L) = 0 before entering QTRAN.   Also, LIVE(L) has to be
C     decreased by M before re-entering OPTRA.
C
        ITRAN(L) = 0
        LIVE(L) = LIVE(L) - M
  110 CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE QTRAN(A, M, N, C, K, IC1, IC2, NC, AN1, AN2, NCP, D,
     *    ITRAN, INDX)
C
C     ALGORITHM AS 136.2  APPL. STATIST. (1979) VOL.28, NO.1
C
C     This is the quick transfer stage.
C     IC1(I) is the cluster which point I belongs to.
C     IC2(I) is the cluster which point I is most likely to be
C         transferred to.
C     For each point I, IC1(I) & IC2(I) are switched, if necessary, to
C     reduce within-cluster sum of squares.  The cluster centres are
C     updated after each step.
C
      INTEGER M,N,K,INDX
      INTEGER IC1(M), IC2(M), NC(K), NCP(K), ITRAN(K)
      DOUBLE PRECISION A(M,N), D(M), C(K,N), AN1(K), AN2(K)

      DOUBLE PRECISION ZERO, ONE
      INTEGER ICOUN,ISTEP,I,L1,L2,J
      DOUBLE PRECISION BIG,DA,DB,DD,AL1,ALW,AL2,ALT,R2,DE
C
C     Define BIG to be a very large positive number
C
      DATA BIG /1.0E30/, ZERO /0.0/, ONE /1.0/
C
C     In the optimal transfer stage, NCP(L) indicates the step at which
C     cluster L is last updated.   In the quick transfer stage, NCP(L)
C     is equal to the step at which cluster L is last updated plus M.
C
      ICOUN = 0
      ISTEP = 0
   10 DO 70 I = 1, M
        ICOUN = ICOUN + 1
        ISTEP = ISTEP + 1
        L1 = IC1(I)
        L2 = IC2(I)
C
C     If point I is the only member of cluster L1, no transfer.
C
        IF (NC(L1) .EQ. 1) GO TO 60
C
C     If ISTEP > NCP(L1), no need to re-compute distance from point I to
C     cluster L1.   Note that if cluster L1 is last updated exactly M
C     steps ago, we still need to compute the distance from point I to
C     cluster L1.
C
        IF (ISTEP .GT. NCP(L1)) GO TO 30
        DA = ZERO
        DO 20 J = 1, N
          DB = A(I,J) - C(L1,J)
          DA = DA + DB*DB
   20   CONTINUE
        D(I) = DA * AN1(L1)
C
C     If ISTEP >= both NCP(L1) & NCP(L2) there will be no transfer of
C     point I at this step.
C
   30   IF (ISTEP .GE. NCP(L1) .AND. ISTEP .GE. NCP(L2)) GO TO 60
        R2 = D(I) / AN2(L2)
        DD = ZERO
        DO 40 J = 1, N
          DE = A(I,J) - C(L2,J)
          DD = DD + DE*DE
          IF (DD .GE. R2) GO TO 60
   40   CONTINUE
C
C     Update cluster centres, NCP, NC, ITRAN, AN1 & AN2 for clusters
C     L1 & L2.   Also update IC1(I) & IC2(I).   Note that if any
C     updating occurs in this stage, INDX is set back to 0.
C
        ICOUN = 0
        INDX = 0
        ITRAN(L1) = 1
        ITRAN(L2) = 1
        NCP(L1) = ISTEP + M
        NCP(L2) = ISTEP + M
        AL1 = NC(L1)
        ALW = AL1 - ONE
        AL2 = NC(L2)
        ALT = AL2 + ONE
        DO 50 J = 1, N
          C(L1,J) = (C(L1,J) * AL1 - A(I,J)) / ALW
          C(L2,J) = (C(L2,J) * AL2 + A(I,J)) / ALT
   50   CONTINUE
        NC(L1) = NC(L1) - 1
        NC(L2) = NC(L2) + 1
        AN2(L1) = ALW / AL1
        AN1(L1) = BIG
        IF (ALW .GT. ONE) AN1(L1) = ALW / (ALW - ONE)
        AN1(L2) = ALT / AL2
        AN2(L2) = ALT / (ALT + ONE)
        IC1(I) = L2
        IC2(I) = L1
C
C     If no re-allocation took place in the last M steps, return.
C
   60   IF (ICOUN .EQ. M) RETURN
   70 CONTINUE
      GO TO 10
      END
