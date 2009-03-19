C Output from Public domain Ratfor, version 1.0
      subroutine sinerp(abd,ld4,nk,p1ip,p2ip,ldnk,flag)
c
C Purpose :  Computes Inner Products between columns of L^{-1}
C            where L = abd is a Banded Matrix with 3 subdiagonals

C The algorithm works in two passes:
C
C               Pass 1 computes (cj,ck) k=j,j-1,j-2,j-3 ;  j=nk, .. 1
C               Pass 2 computes (cj,ck) k <= j-4  (If flag == 1 ).
C
C               A refinement of Elden's trick is used.
c Args
      integer ld4,nk,ldnk,flag
      DOUBLE precision abd(ld4,nk),p1ip(ld4,nk), p2ip(ldnk,nk)
c Locals
      integer i,j,k
      DOUBLE precision  wjm3(3),wjm2(2),wjm1(1),c0,c1,c2,c3
c
c     unnecessary initialization of c1 c2 c3 to keep g77 -Wall happy
c
      c1 = 0.0d0
      c2 = 0.0d0
      c3 = 0.0d0
C
C Pass 1
      wjm3(1)=0d0
      wjm3(2)=0d0
      wjm3(3)=0d0
      wjm2(1)=0d0
      wjm2(2)=0d0
      wjm1(1)=0d0
      do 100 i=1,nk
         j=nk-i+1
         c0 = 1d0/abd(4,j)
         if(j.le.nk-3)then
            c1 = abd(1,j+3)*c0
            c2 = abd(2,j+2)*c0
            c3 = abd(3,j+1)*c0
         else if(j.eq.nk-2)then
            c1 = 0d0
            c2 = abd(2,j+2)*c0
            c3 = abd(3,j+1)*c0
         else if(j.eq.nk-1)then
            c1 = 0d0
            c2 = 0d0
            c3 = abd(3,j+1)*c0
         else if(j.eq.nk)then
            c1 = 0d0
            c2 = 0d0
            c3 = 0d0
         endif
         p1ip(1,j) = 0d0- (c1*wjm3(1)+c2*wjm3(2)+c3*wjm3(3))
         p1ip(2,j) = 0d0- (c1*wjm3(2)+c2*wjm2(1)+c3*wjm2(2))
         p1ip(3,j) = 0d0- (c1*wjm3(3)+c2*wjm2(2)+c3*wjm1(1))
         p1ip(4,j) = c0**2 + c1**2*wjm3(1) + 2d0*c1*c2*wjm3(2)+
     &        2d0*c1*c3*wjm3(3) + c2**2*wjm2(1) + 2d0*c2*c3*wjm2(2) +
     &        c3**2*wjm1(1)
         wjm3(1)=wjm2(1)
         wjm3(2)=wjm2(2)
         wjm3(3)=p1ip(2,j)
         wjm2(1)=wjm1(1)
         wjm2(2)=p1ip(3,j)
         wjm1(1)=p1ip(4,j)
 100  continue

      if(flag.ne.0)then

C     ____ Pass 2 _____

C     Compute p2ip
         do 120 i=1,nk
            j=nk-i+1
C           for(k=1;k<=4 & j+k-1<=nk;k=k+1) { p2ip(.) = .. }:
            do 160 k=1,4
               if(j+k-1 .gt. nk)goto 120
               p2ip(j,j+k-1) = p1ip(5-k,j)
 160        continue
 120     continue

         do 170 i=1,nk
            j=nk-i+1
c           for(k=j-4;k>=1;k=k-1){
            if(j-4 .ge. 1) then
               do 210 k= j-4,1, -1
                  c0 = 1d0/abd(4,k)
                  c1 = abd(1,k+3)*c0
                  c2 = abd(2,k+2)*c0
                  c3 = abd(3,k+1)*c0
                  p2ip(k,j)= 0d0 - ( c1*p2ip(k+3,j) + c2*p2ip(k+2,j) +
     &                 c3*p2ip(k+1,j) )
 210           continue
            endif
 170     continue
      endif
      return
      end

