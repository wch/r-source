      subroutine sinerp(abd,ld4,nk,p1ip,p2ip,ldnk,flag)
      implicit double precision(a-h,o-z)
      integerflag,ld4,nk,ldnk,i,j,k
      double precision abd(ld4,nk),p1ip(ld4,nk),p2ip(ldnk,nk),wjm3(3),
     &wjm2(2),wjm1(1),c0,c1,c2,c3
      wjm3(1)=0e0
      wjm3(2)=0e0
      wjm3(1)=0e0
      wjm2(1)=0e0
      wjm2(2)=0e0
      wjm1(1)=0e0
      do 23000 i=1,nk 
      j=nk-i+1
      c0 = 1e0/abd(4,j)
      if(.not.(j.le.nk-3))goto 23002
      c1 = abd(1,j+3)*c0
      c2 = abd(2,j+2)*c0
      c3 = abd(3,j+1)*c0 
      goto 23003
23002 continue
      if(.not.(j.eq.nk-2))goto 23004
      c1 = 0e0
      c2 = abd(2,j+2)*c0
      c3 = abd(3,j+1)*c0 
      goto 23005
23004 continue
      if(.not.(j.eq.nk-1))goto 23006
      c1 = 0e0
      c2 = 0e0
      c3 = abd(3,j+1)*c0 
      goto 23007
23006 continue
      if(.not.(j.eq.nk))goto 23008
      c1 = 0e0
      c2 = 0e0
      c3 = 0e0
23008 continue
23007 continue
23005 continue
23003 continue
      p1ip(1,j) = 0e0- (c1*wjm3(1)+c2*wjm3(2)+c3*wjm3(3))
      p1ip(2,j) = 0e0- (c1*wjm3(2)+c2*wjm2(1)+c3*wjm2(2))
      p1ip(3,j) = 0e0- (c1*wjm3(3)+c2*wjm2(2)+c3*wjm1(1))
      p1ip(4,j) = c0**2 +c1**2*wjm3(1)+2.*c1*c2*wjm3(2)+2.*c1*c3*wjm3(3)
     & +c2**2*wjm2(1)+2.*c2*c3*wjm2(2) +c3**2*wjm1(1)
      wjm3(1)=wjm2(1) 
      wjm3(2)=wjm2(2) 
      wjm3(3)=p1ip(2,j)
      wjm2(1)=wjm1(1) 
      wjm2(2)=p1ip(3,j)
      wjm1(1)=p1ip(4,j)
23000 continue
      if(.not.(flag.eq.0))goto 23010
      return
23010 continue
      do 23012 i=1,nk 
      j=nk-i+1
      k=1
23014 if(.not.(k.le.4.and.j+k-1.le.nk))goto 23016
      p2ip(j,j+k-1) = p1ip(5-k,j) 
      k=k+1
      goto 23014
23016 continue
23012 continue
      do 23017 i=1,nk 
      j=nk-i+1
      k=j-4
23019 if(.not.(k.ge.1))goto 23021
      c0 = 1./abd(4,k) 
      c1 = abd(1,k+3)*c0
      c2 = abd(2,k+2)*c0 
      c3 = abd(3,k+1)*c0
      p2ip(k,j) = 0e0- ( c1*p2ip(k+3,j) +c2*p2ip(k+2,j) +c3*p2ip(k+1,j) 
     &) 
      k=k-1
      goto 23019
23021 continue
23017 continue
      return
23011 continue
      end
