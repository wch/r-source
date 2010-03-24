C Output from Public domain Ratfor, version 1.0

C PURPOSE
C       Calculation of the cubic B-spline smoothness prior
C       for "usual" interior knot setup.
C       Uses BSPVD and INTRV in the CMLIB
C       sgm[0-3](nb)    Symmetric matrix
C                       whose (i,j)'th element contains the integral of
C                       B''(i,.) B''(j,.) , i=1,2 ... nb and j=i,...nb.
C                       Only the upper four diagonals are computed.

      subroutine sgram(sg0,sg1,sg2,sg3,tb,nb)

c      implicit none
C indices
      integer nb
      DOUBLE precision sg0(nb),sg1(nb),sg2(nb),sg3(nb), tb(nb+4)
c     -------------
      integer ileft,mflag, i,ii,jj, lentb
      DOUBLE precision vnikx(4,3),work(16),yw1(4),yw2(4), wpt
c
      integer interv
      external interv

      lentb=nb+4
C Initialise the sigma vectors
      do 1 i=1,nb
         sg0(i)=0.d0
         sg1(i)=0.d0
         sg2(i)=0.d0
         sg3(i)=0.d0
 1    continue

      ileft = 1
      do 2 i=1,nb
C     Calculate a linear approximation to the
C     second derivative of the non-zero B-splines
C     over the interval [tb(i),tb(i+1)].
C     call intrv(tb(1),(nb+1),tb(i),ilo,ileft,mflag)
         ileft = interv(tb(1), nb+1,tb(i), 0,0, ileft, mflag)
C     Left end second derivatives
C     call bspvd (tb,4,3,tb(i),ileft,4,vnikx,work)
         call bsplvd (tb,lentb,4,tb(i),ileft,work,vnikx,3)
C     Put values into yw1
         do 4 ii=1,4
            yw1(ii) = vnikx(ii,3)
 4       continue

C     Right end second derivatives
C     call bspvd (tb,4,3,tb(i+1),ileft,4,vnikx,work)
         call bsplvd (tb,lentb,4,tb(i+1),ileft,work,vnikx,3)

C     Slope*(length of interval) in Linear Approximation to B''
         do    6 ii=1,4
            yw2(ii) = vnikx(ii,3) - yw1(ii)
 6       continue

         wpt = tb(i+1) - tb(i)
C     Calculate Contributions to the sigma vectors
         if(ileft.ge.4) then
            do 10 ii=1,4
               jj=ii
               sg0(ileft-4+ii) = sg0(ileft-4+ii) +
     &              wpt*(yw1(ii)*yw1(jj)+
     &                   (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                  + yw2(ii)*yw2(jj)*0.3330d0)
               jj=ii+1
               if(jj.le.4)then
                  sg1(ileft+ii-4) = sg1(ileft+ii-4) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
               jj=ii+2
               if(jj.le.4)then
                  sg2(ileft+ii-4) = sg2(ileft+ii-4) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
               jj=ii+3
               if(jj.le.4)then
                  sg3(ileft+ii-4) = sg3(ileft+ii-4) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
 10         continue

         else if(ileft.eq.3)then
            do    20 ii=1,3
               jj=ii
               sg0(ileft-3+ii) = sg0(ileft-3+ii) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               jj=ii+1
               if(jj.le.3)then
                  sg1(ileft+ii-3) = sg1(ileft+ii-3) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
               jj=ii+2
               if(jj.le.3)then
                  sg2(ileft+ii-3) = sg2(ileft+ii-3) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
 20         continue

         else if(ileft.eq.2)then
            do    28 ii=1,2
               jj=ii
               sg0(ileft-2+ii) = sg0(ileft-2+ii) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               jj=ii+1
               if(jj.le.2)then
                  sg1(ileft+ii-2) = sg1(ileft+ii-2) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
 28         continue

         else if(ileft.eq.1)then
            do 34 ii=1,1
               jj=ii
               sg0(ileft-1+ii) = sg0(ileft-1+ii) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
 34         continue

         endif
 2    continue

      return
      end
