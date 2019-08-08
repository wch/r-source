C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                            C
C  HIERARCHICAL CLUSTERING using (user-specified) criterion. C
C                                                            C
C  Parameters:                                               C
C                                                            C
C  N                 the number of points being clustered    C
C  DISS(LEN)         dissimilarities in lower half diagonal  C
C                    storage; LEN = N.N-1/2,                 C
C  IOPT              clustering criterion to be used,        C
C  IA, IB, CRIT      history of agglomerations; dimensions   C
C                    N, first N-1 locations only used,       C
C  MEMBR, NN, DISNN  vectors of length N, used to store      C
C                    cluster cardinalities, current nearest  C
C                    neighbour, and the dissimilarity assoc. C
C                    with the latter.                        C
C                    MEMBR must be initialized by R to the   C
C                    default of  rep(1, N)                   C
C  FLAG              boolean indicator of agglomerable obj./ C
C                    clusters.                               C
C                                                            C
C  F. Murtagh, ESA/ESO/STECF, Garching, February 1986.       C
C  Modifications for R: Ross Ihaka, Dec 1996                 C
C                       Fritz Leisch, Jun 2000               C
C  all vars declared:   Martin Maechler, Apr 2001            C
C                                                            C
c- R Bug PR#4195 fixed "along" qclust.c, given in the report C
C- Testing: --> "hclust" in ../../../../tests/reg-tests-1b.R C
C  "ward.D2" (iOpt = 8): Martin Maechler, Mar 2014           C
C                                                            C
C  FLAG not passed as arg to avoid possible                  C
C     C/Fortran inconsistency, May 2019                      C
C------------------------------------------------------------C
      SUBROUTINE HCLUST(N,LEN,IOPT,IA,IB,CRIT,MEMBR,NN,DISNN, DISS)
c Args
      INTEGER N, LEN, IOPT
      INTEGER IA(N),IB(N), NN(N)
      LOGICAL FLAG(N), isWard
      DOUBLE PRECISION CRIT(N), MEMBR(N),DISS(LEN), DISNN(N)
c Var
      INTEGER IM, JJ, JM, I, NCL, J, IND, I2, J2, K, IND1, IND2
      DOUBLE PRECISION INF, DMIN, D12
c External function
      INTEGER IOFFST
c
c     was 1D+20
      DATA INF/1.D+300/
c
c     unnecessary initialization of im jj jm to keep g77 -Wall happy
c
      IM = 0
      JJ = 0
      JM = 0
C
C  Initializations
C
      DO I=1,N
C        We do not initialize MEMBR in order to be able to restart the
C        algorithm from a cut.
C        MEMBR(I)=1.
         FLAG(I)=.TRUE.
      end do
      NCL=N

      IF (iOpt .eq. 8) THEN ! Ward "D2" ---> using *squared* distances
         do I=1,LEN
            DISS(I)=DISS(I)*DISS(I)
         end do
      ENDIF

C
C  Carry out an agglomeration - first create list of NNs
C  Note NN and DISNN are the nearest neighbour and its distance
C  TO THE RIGHT of I.
C
      DO I=1,N-1
         DMIN=INF
         DO J=I+1,N
            IND=IOFFST(N,I,J)
            IF (DMIN .GT. DISS(IND)) THEN
               DMIN=DISS(IND)
               JM=J
            end if
         end do
         NN(I)=JM
         DISNN(I)=DMIN
      end do

C-- Repeat -------------------------------------------------------
  400 CONTINUE

C     Next, determine least diss. using list of NNs
      DMIN=INF
      DO I=1,N-1
         IF (FLAG(I) .AND. DISNN(I) .LT. DMIN) THEN
            DMIN=DISNN(I)
            IM=I
            JM=NN(I)
         end if
      end do
      NCL=NCL-1
C
C  This allows an agglomeration to be carried out.
C
      I2=MIN0(IM,JM)
      J2=MAX0(IM,JM)
      IA(N-NCL)=I2
      IB(N-NCL)=J2
C     WARD'S "D1", or "D2" MINIMUM VARIANCE METHOD -- iOpt = 1 or 8.
      isWard = (iOpt .eq. 1 .or. iOpt .eq. 8)
      IF (iOpt .eq. 8) DMIN = dsqrt(DMIN)
      CRIT(N-NCL)=DMIN
      FLAG(J2)=.FALSE.
C
C  Update dissimilarities from new cluster.
C
      DMIN=INF
      DO K=1,N
         IF (FLAG(K) .AND. K .NE. I2) THEN
            IF (I2.LT.K) THEN
               IND1=IOFFST(N,I2,K)
            ELSE
               IND1=IOFFST(N,K,I2)
            ENDIF
            IF (J2.LT.K) THEN
               IND2=IOFFST(N,J2,K)
            ELSE
               IND2=IOFFST(N,K,J2)
            ENDIF
            D12=DISS(IOFFST(N,I2,J2))
C
C     WARD'S "D1", or "D2" MINIMUM VARIANCE METHOD - IOPT=8.
            IF (isWard) THEN
               DISS(IND1)=(MEMBR(I2)+MEMBR(K))*DISS(IND1)+
     X              (MEMBR(J2)+MEMBR(K))*DISS(IND2) - MEMBR(K)*D12
               DISS(IND1)=DISS(IND1) / (MEMBR(I2)+MEMBR(J2)+MEMBR(K))
C
C     SINGLE LINK METHOD - IOPT=2.
            ELSEIF (IOPT.EQ.2) THEN
               DISS(IND1)=MIN(DISS(IND1),DISS(IND2))
C
C     COMPLETE LINK METHOD - IOPT=3.
            ELSEIF (IOPT.EQ.3) THEN
               DISS(IND1)=MAX(DISS(IND1),DISS(IND2))
C
C     AVERAGE LINK (OR GROUP AVERAGE) METHOD - IOPT=4.
            ELSEIF (IOPT.EQ.4) THEN
               DISS(IND1)= (MEMBR(I2)*DISS(IND1)+MEMBR(J2)*DISS(IND2))
     X                      / (MEMBR(I2)+MEMBR(J2))
C
C     MCQUITTY'S METHOD - IOPT=5.
            ELSEIF (IOPT.EQ.5) THEN
               DISS(IND1)=(DISS(IND1)+DISS(IND2)) / 2
C
C     MEDIAN (GOWER'S) METHOD aka "Weighted Centroid" - IOPT=6.
            ELSEIF (IOPT.EQ.6) THEN
               DISS(IND1)= ((DISS(IND1)+DISS(IND2)) - D12/2) / 2
C
C     Unweighted CENTROID METHOD - IOPT=7.
            ELSEIF (IOPT.EQ.7) THEN
               DISS(IND1)=(MEMBR(I2)*DISS(IND1)+MEMBR(J2)*DISS(IND2)-
     X              MEMBR(I2)*MEMBR(J2)*D12/(MEMBR(I2)+MEMBR(J2)))/
     X              (MEMBR(I2)+MEMBR(J2))
            ENDIF

C
            IF (I2 .lt. K) THEN
               IF (DISS(IND1) .LT. DMIN) THEN
                  DMIN=DISS(IND1)
                  JJ=K
               ENDIF
            else  ! i2 > k
c	     FIX: the rest of the else clause is a fix by JB to ensure
c            correct nearest neighbours are found when a non-monotone
c            clustering method (e.g. the centroid methods) are used
               if(DISS(IND1) .lt. DISNN(K)) then ! find nearest neighbour of i2
                  DISNN(K) = DISS(IND1)
                  NN(K) = I2
               end if
            ENDIF
         ENDIF
      END DO
      MEMBR(I2)=MEMBR(I2)+MEMBR(J2)
      DISNN(I2)=DMIN
      NN(I2)=JJ
C
C  Update list of NNs insofar as this is required.
C
      DO I=1,N-1
         IF (FLAG(I) .AND.
     X        ((NN(I).EQ.I2) .OR. (NN(I).EQ.J2))) THEN
C     (Redetermine NN of I:)
            DMIN=INF
            DO J=I+1,N
               if (FLAG(J)) then
                  IND=IOFFST(N,I,J)
                  if (DISS(IND) .lt. DMIN) then
                     DMIN=DISS(IND)
                     JJ=J
                  end if
               end if
            end do
            NN(I)=JJ
            DISNN(I)=DMIN
         end if
      end do
C
C  Repeat previous steps until N-1 agglomerations carried out.
C
      IF (NCL.GT.1) GOTO 400
C
C
      RETURN
      END
C     of HCLUST()
C
C
      INTEGER FUNCTION IOFFST(N,I,J)
C  Map row I and column J of upper half diagonal symmetric matrix
C  onto vector.
      INTEGER N,I,J
C  use 64-bit integers for temporaries to avoid integer overflow
      INTEGER(KIND=8) N8,I8,J8
      N8=N
      I8=I
      J8=J
      IOFFST=J8+(I8-1)*N8-(I8*(I8+1))/2
      RETURN
      END

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++C
C                                                               C
C  Given a HIERARCHIC CLUSTERING, described as a sequence of    C
C  agglomerations, prepare the seq. of aggloms. and "horiz."    C
C  order of objects for plotting the dendrogram using S routine C
C  'plclust'.                                                   C
C                                                               C
C  Parameters:                                                  C
C                                                               C
C  IA, IB:       vectors of dimension N defining the agglomer-  C
C                 ations.                                       C
C  IIA, IIB:     used to store IA and IB values differently     C
C                (in form needed for S command 'plclust'        C
C  IORDER:       "horiz." order of objects for dendrogram       C
C                                                               C
C  F. Murtagh, ESA/ESO/STECF, Garching, June 1991               C
C                                                               C
C  HISTORY                                                      C
C                                                               C
C  Adapted from routine HCASS, which additionally determines    C
C   cluster assignments at all levels, at extra comput. expense C
C                                                               C
C---------------------------------------------------------------C
      SUBROUTINE HCASS2(N,IA,IB,IORDER,IIA,IIB)
c Args
      INTEGER N,IA(N),IB(N),IORDER(N),IIA(N),IIB(N)
c Var
      INTEGER I, J, K, K1, K2, LOC
C
C     Following bit is to get seq. of merges into format acceptable to plclust
C     I coded clusters as lowest seq. no. of constituents; S's 'hclust' codes
C     singletons as -ve numbers, and non-singletons with their seq. nos.
C
      do I=1,N
         IIA(I)=IA(I)
         IIB(I)=IB(I)
      end do
      do I=1,N-2
C        In the following, smallest (+ve or -ve) seq. no. wanted
         K=MIN(IA(I),IB(I))
         do J=I+1, N-1
            IF(IA(J).EQ.K) IIA(J)=-I
            IF(IB(J).EQ.K) IIB(J)=-I
         end do
      end do
      do I=1,N-1
         IIA(I)=-IIA(I)
         IIB(I)=-IIB(I)
      end do
      do I=1,N-1
         IF (IIA(I).GT.0 .AND. IIB(I).LT.0) THEN
            K = IIA(I)
            IIA(I) = IIB(I)
            IIB(I) = K
         ENDIF
         IF (IIA(I).GT.0 .AND. IIB(I).GT.0) THEN
            K1 = MIN(IIA(I),IIB(I))
            K2 = MAX(IIA(I),IIB(I))
            IIA(I) = K1
            IIB(I) = K2
         ENDIF
      end do
C
C
C     NEW PART FOR 'ORDER'
C
      IORDER(1) = IIA(N-1)
      IORDER(2) = IIB(N-1)
      LOC=2
      DO I=N-2,1,-1
         DO J=1,LOC
            IF(IORDER(J).EQ.I) THEN
C      REPLACE IORDER(J) WITH IIA(I) AND IIB(I)
               IORDER(J)=IIA(I)
               IF (J.EQ.LOC) THEN
                  LOC=LOC+1
                  IORDER(LOC)=IIB(I)
               else
                  LOC=LOC+1
                  do K=LOC,J+2,-1
                     IORDER(K)=IORDER(K-1)
                  end do
                  IORDER(J+1)=IIB(I)
               end if
               GOTO 171
            ENDIF
         end do
C     SHOULD NEVER REACH HERE
 171     CONTINUE
      end do
C
C
      do I=1,N
         IORDER(I) = -IORDER(I)
      end do
C
C
      RETURN
      END
