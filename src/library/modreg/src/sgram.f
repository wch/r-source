      subroutine sgram(sg0,sg1,sg2,sg3,tb,nb)
      implicit double precision(a-h,o-z)
      integer nb,ileft,ilo,mflag,i,ii,jj
      double precision sg0(nb),sg1(nb),sg2(nb),sg3(nb),tb(nb+4),vnikx(4,
&     3),work(16),yw1(4),yw2(4),wpt
      lentb=nb+4
      do 23000 i=1,nb
      sg0(i)=0.
      sg1(i)=0.
      sg2(i)=0.
      sg3(i)=0.
23000 continue
      ilo = 1
      do 23002 i=1,nb 
      call interv(tb(1),(nb+1),tb(i),ileft,mflag)
      call bsplvd (tb,lentb,4,tb(i),ileft,work,vnikx,3)
      do 23004 ii=1,4 
      yw1(ii) = vnikx(ii,3) 
23004 continue
      call bsplvd (tb,lentb,4,tb(i+1),ileft,work,vnikx,3)
      do 23006 ii=1,4 
      yw2(ii) = vnikx(ii,3) - yw1(ii) 
23006 continue
      wpt = tb(i+1) - tb(i)
      if(.not.(ileft.ge.4))goto 23008
      do 23010 ii=1,4 
      jj=ii
      sg0(ileft-4+ii) = sg0(ileft-4+ii) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
      jj=ii+1
      if(.not.(jj.le.4))goto 23012
      sg1(ileft+ii-4) = sg1(ileft+ii-4) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
23012 continue
      jj=ii+2
      if(.not.(jj.le.4))goto 23014
      sg2(ileft+ii-4) = sg2(ileft+ii-4) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
23014 continue
      jj=ii+3
      if(.not.(jj.le.4))goto 23016
      sg3(ileft+ii-4) = sg3(ileft+ii-4) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
23016 continue
23010 continue
      goto 23009
23008 continue
      if(.not.(ileft.eq.3))goto 23018
      do 23020 ii=1,3 
      jj=ii
      sg0(ileft-3+ii) = sg0(ileft-3+ii) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
      jj=ii+1
      if(.not.(jj.le.3))goto 23022
      sg1(ileft+ii-3) = sg1(ileft+ii-3) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
23022 continue
      jj=ii+2
      if(.not.(jj.le.3))goto 23024
      sg2(ileft+ii-3) = sg2(ileft+ii-3) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
23024 continue
23020 continue
      goto 23019
23018 continue
      if(.not.(ileft.eq.2))goto 23026
      do 23028 ii=1,2 
      jj=ii
      sg0(ileft-2+ii) = sg0(ileft-2+ii) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
      jj=ii+1
      if(.not.(jj.le.2))goto 23030
      sg1(ileft+ii-2) = sg1(ileft+ii-2) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
23030 continue
23028 continue
      goto 23027
23026 continue
      if(.not.(ileft.eq.1))goto 23032
      do 23034 ii=1,1 
      jj=ii
      sg0(ileft-1+ii) = sg0(ileft-1+ii) +wpt* (yw1(ii)*yw1(jj) + (yw2(
&     ii)*yw1(jj) + yw2(jj)*yw1(ii))*.50 +yw2(ii)*yw2(jj)*.3330 )
23034 continue
23032 continue
23027 continue
23019 continue
23009 continue
23002 continue
      return
      end
