C Output from Public domain Ratfor, version 1.0

C PURPOSE
C       Calculation of the cubic B-spline smoothness prior
C       for "usual" interior knot setup.
C       Uses BSPVD and INTRV in the CMLIB
C       sgm[0-3](nb)    Symmetric matrix 'SIGMA'
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
      external interv ! in ../../../appl/interv.c

      lentb=nb+4
C Initialise the sigma vectors
      do i=1,nb
         sg0(i)=0.d0
         sg1(i)=0.d0
         sg2(i)=0.d0
         sg3(i)=0.d0
      end do

      ileft = 1
      do i=1,nb
C        Calculate a linear approximation to the second derivative of the
C        non-zero B-splines over the interval [tb(i),tb(i+1)].
         ileft = interv(tb(1), nb+1,tb(i), 0,0, ileft, mflag)

C        Left end second derivatives
         call bsplvd (tb,lentb,4,tb(i),ileft,work,vnikx,3)

C        Put values into yw1
         do ii=1,4
            yw1(ii) = vnikx(ii,3)
         end do

C        Right end second derivatives
         call bsplvd (tb,lentb,4,tb(i+1),ileft,work,vnikx,3)

C        Slope*(length of interval) in Linear Approximation to B''
         do ii=1,4
            yw2(ii) = vnikx(ii,3) - yw1(ii)
         end do

C        Calculate Contributions to the sigma vectors
         wpt = tb(i+1) - tb(i)
         if(ileft.ge.4) then
            do ii=1,4
               jj=ii
               sg0(ileft-4+ii) = sg0(ileft-4+ii) +
     &              wpt*(yw1(ii)*yw1(jj)+
     &                   (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                  + yw2(ii)*yw2(jj)*0.3330d0)
               jj=ii+1
               if(jj.le.4) then
                  sg1(ileft+ii-4) = sg1(ileft+ii-4) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
               jj=ii+2
               if(jj.le.4) then
                  sg2(ileft+ii-4) = sg2(ileft+ii-4) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
               jj=ii+3
               if(jj.le.4) then
                  sg3(ileft+ii-4) = sg3(ileft+ii-4) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
            end do

         else if(ileft.eq.3) then
            do ii=1,3
               jj=ii
               sg0(ileft-3+ii) = sg0(ileft-3+ii) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               jj=ii+1
               if(jj.le.3) then
                  sg1(ileft+ii-3) = sg1(ileft+ii-3) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
               jj=ii+2
               if(jj.le.3) then
                  sg2(ileft+ii-3) = sg2(ileft+ii-3) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
            end do

         else if(ileft.eq.2) then
            do ii=1,2
               jj=ii
               sg0(ileft-2+ii) = sg0(ileft-2+ii) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               jj=ii+1
               if(jj.le.2) then
                  sg1(ileft+ii-2) = sg1(ileft+ii-2) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
               endif
            end do


         else if(ileft.eq.1) then
            do ii=1,1
               jj=ii
               sg0(ileft-1+ii) = sg0(ileft-1+ii) +
     &                 wpt* (yw1(ii)*yw1(jj) +
     *                       (yw2(ii)*yw1(jj) + yw2(jj)*yw1(ii))*0.5d0
     &                       +yw2(ii)*yw2(jj)*0.3330d0 )
            end do
         endif

      end do

      return
      end
