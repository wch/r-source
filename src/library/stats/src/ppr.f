C
C     Modified from the SMART package by J.H. Friedman, 10/10/84
C     Main change is to add spline smoothing modified from BRUTO,
C     calling code written for smooth.spline in S.
C
C     B.D. Ripley (ripley@stats.ox.ac.uk)  1994-7.
C
C
      subroutine smart(m,mu,p,q,n, w,x,y,ww,smod,nsmod,
     &     sp,nsp,dp,ndp,edf)

      integer m,mu,p,q,n, nsmod, nsp,ndp
      double precision x(p,n),y(q,n),w(n),ww(q),smod(nsmod),
     &     sp(nsp),edf(m),dp(ndp)
      smod(1)=m
      smod(2)=p
      smod(3)=q
      smod(4)=n
      call smart1(m,mu,p,q,n, w,x,y,ww, smod(6),smod(q+6),
     &     smod(q+7),smod(q+7+p*m),smod(q+7+m*(p+q)),
     &     smod(q+7+m*(p+q+n)),smod(q+7+m*(p+q+2*n)),
     &     sp,sp(q*n+1),sp(n*(q+15)+1),sp(n*(q+15)+q+1),
     &     dp,smod(5),edf)
      return
      end

      subroutine smart1(m,mu,p,q,n, w,x,y,ww, yb,ys,
     &     a,b,f,
     &     t,asr,
     &     r,sc,bt,g,
     &     dp,flm,edf)

      integer m,mu,p,q,n
      double precision w(n),x(p,n),y(*),ww(q), yb(q), ys
      double precision a(p,m),b(q,m),f(n,m),t(n,m), asr(15),asr1
      double precision r(q,n),sc(n,15),bt(q),g(p,3)
      double precision dp(*), flm,edf(m)
C                        ^^^ really (ndb) of  smart(.)
      integer i,j,l, lm
      double precision sw,s
c Common Vars
      double precision         span,alpha,big
      integer           ifl,lf
      common /pprpar/ ifl,lf,span,alpha,big

      double precision conv,            cutmin,fdel,cjeps
      integer              maxit,mitone,                  mitcj
      common /pprz01/ conv,maxit,mitone,cutmin,fdel,cjeps,mitcj

      sw=0d0
      do 161 j=1,n
 161     sw=sw+w(j)
      do 201 j=1,n
         do 201 i=1,q
            r(i,j)=y(q*(j-1)+i)
 201  continue
      do 241 i=1,q
         s=0d0
         do 251 j=1,n
            s=s+w(j)*r(i,j)
 251     continue
         yb(i)=s/sw
 241  continue
c     yb is vector of means
      do 261 j=1,n
        do 261 i=1,q
 261       r(i,j)=r(i,j)-yb(i)
      ys=0.d0
      do 281 i=1,q
         s=0.d0
         do 291 j=1,n
 291        s=s+w(j)*r(i,j)**2
         ys=ys+ww(i)*s/sw
 281  continue
      if(ys .gt. 0d0) goto 311
c     ys is the overall standard deviation -- quit if zero
      return

 311  continue
      ys=sqrt(ys)
      s=1.d0/ys
      do 331 j=1,n
         do 331 i=1,q
 331        r(i,j)=r(i,j)*s

c     r is now standardized residuals
c     subfit adds up to m  terms one at time; lm is the number fitted.
      call subfit(m,p,q,n,w,sw,x,r,ww,lm,a,b,f,t,asr(1),sc,bt,g,dp,edf)
      if(lf.le.0) go to 9999
      call fulfit(lm,lf,p,q,n,w,sw,x,r,ww,a,b,f,t,asr,sc,bt,g,dp,edf)
C REPEAT
 371  continue
      do 381 l=1,lm
        sc(l,1)=l+0.1d0
        s=0d0
        do 391 i=1,q
 391       s=s+ww(i)*abs(b(i,l))
        sc(l,2)=-s
 381  continue
      call sort(sc(1,2),sc,1,lm)
      do 461 j=1,n
         do 461 i=1,q
 461        r(i,j)=y(q*(j-1)+i)
      do 521 i=1,q
        do 531 j=1,n
           r(i,j)=r(i,j)-yb(i)
           s=0.d0
           do 541 l=1,lm
 541          s=s+b(i,l)*f(j,l)
          r(i,j)=r(i,j)/ys-s
531     continue
521   continue
      if(lm.le.mu) goto 9999
c back to integer:
      l=sc(lm,1)
      asr1=0d0
      do 561 j=1,n
        do 561 i=1,q
          r(i,j)=r(i,j)+b(i,l)*f(j,l)
561     asr1=asr1+w(j)*ww(i)*r(i,j)**2
      asr1=asr1/sw
      asr(1)=asr1
      if(l .ge. lm) goto 591
      do 601 i=1,p
 601     a(i,l)=a(i,lm)
      do 611 i=1,q
 611     b(i,l)=b(i,lm)
      do 621 j=1,n
         f(j,l)=f(j,lm)
 621     t(j,l)=t(j,lm)
591   continue
      lm=lm-1
      call fulfit(lm,lf,p,q,n,w,sw,x,r,ww,a,b,f,t,asr,sc,bt,g,dp,edf)
      goto 371
C END REPEAT
 9999 continue
      flm=lm
      return
      end

      subroutine subfit(m,p,q,n,w,sw,x,r,ww,lm,a,b,f,t,asr,sc,
     &     bt,g,dp,edf)
c Args
      integer              m,p,q,n,            lm
      double precision w(n),sw, x(p,n),r(q,n),ww(q),a(p,m),b(q,m),
     &     f(n,m), t(n,m), asr(15), sc(n,15), bt(q), g(p,3), edf(m)
      double precision dp(*)
c Var
      integer i,j,l, iflsv
      double precision asrold
c Common Vars
      double precision         span,alpha,big
      integer           ifl,lf
      common /pprpar/ ifl,lf,span,alpha,big

      double precision conv,            cutmin,fdel,cjeps
      integer              maxit,mitone,                  mitcj
      common /pprz01/ conv,maxit,mitone,cutmin,fdel,cjeps,mitcj

      asr(1)=big
      lm=0
      do 100 l=1,m
         call rchkusr()
         lm=lm+1
         asrold=asr(1)
         call newb(lm,q,ww,b)
c does 'edf' mean 'edf(1)' or 'edf(l)'?
         call onetrm(0,p,q,n,w,sw,x,r,ww,a(1,lm),b(1,lm),
     &        f(1,lm),t(1,lm),asr(1),sc,g,dp,edf(1))
         do 10 j=1,n
            do 10 i=1,q
 10            r(i,j)=r(i,j)-b(i,lm)*f(j,lm)
         if(lm.eq.1) goto 100
         if(lf.gt.0) then
            if(lm.eq.m) return
            iflsv=ifl
            ifl=0
            call fulfit(lm,1,p,q,n,w,sw,x,r,ww,a,b,f,t,asr,sc,bt,
     &           g,dp, edf)
            ifl=iflsv
         endif
         if(asr(1).le.0d0.or.(asrold-asr(1))/asrold.lt.conv) return
100   continue
      return
      end

      subroutine fulfit(lm,lbf,p,q,n,w,sw,x,r,ww,a,b,f,t,
     &     asr,sc,bt,g,dp,edf)
c Args
      integer              lm,lbf,p,q,n
      double precision w(n),sw,x(p,n),r(q,n),ww(q),a(p,lm),b(q,lm),
     &     f(n,lm),t(n,lm),asr(1+lm), sc(n,15),bt(q),g(p,3), edf(lm)
      double precision dp(*)
c Var
      double precision asri, fsv, asrold
      integer i,j,iter,lp,isv
c Common Vars
      double precision         span,alpha,big
      integer           ifl,lf
      common /pprpar/ ifl,lf,span,alpha,big

      double precision conv,            cutmin,fdel,cjeps
      integer              maxit,mitone,                  mitcj
      common /pprz01/ conv,maxit,mitone,cutmin,fdel,cjeps,mitcj

      if(lbf.le.0) return
      asri=asr(1)
      fsv=cutmin
      isv=mitone
      if(lbf .lt. 3) then
         cutmin=1d0
         mitone=lbf-1
      endif
      iter=0
C Outer loop:
1000  continue
      asrold=asri
      iter=iter+1
      do 100 lp=1,lm
        do 1 i=1,q
 1         bt(i)=b(i,lp)
        do 2 i=1,p
 2         g(i,3)=a(i,lp)
        do 3 j=1,n
          do 3 i=1,q
 3           r(i,j)=r(i,j)+bt(i)*f(j,lp)

        call onetrm(1,p,q,n,w,sw,x,r,ww,g(1,3),bt,sc(1,14),sc(1,15),
     &            asri,sc,g,dp,edf(lp))
        if(asri .lt. asrold) then
           do 4 i=1,q
 4            b(i,lp)=bt(i)
           do 5 i=1,p
 5            a(i,lp)=g(i,3)
           do 6 j=1,n
              f(j,lp)=sc(j,14)
              t(j,lp)=sc(j,15)
 6         continue
        else
           asri=asrold
        endif
        do 8 j=1,n
          do 8 i=1,q
 8           r(i,j)=r(i,j)-b(i,lp)*f(j,lp)
100   continue
      if((iter .le. maxit) .and. ((asri .gt. 0d0)
     &     .and. ((asrold-asri)/asrold .ge. conv))) goto 1000
      cutmin=fsv
      mitone=isv
      if(ifl .gt. 0) then
         asr(1+lm) = asri
         asr(1) = asri
      endif
      return
      end

      subroutine onetrm(jfl,p,q,n,w,sw,x,y,ww,a,b,f,t,asr,
     &     sc,g,dp,edf)
c Args
      integer              jfl,p,q,n
      double precision w(n),sw, x(p,n),y(q,n),ww(q),a(p),b(q),f(n),t(n),
     &     asr, sc(n,13),g(p,2), edf
      double precision dp(*)
c Var
      double precision asrold,s
      integer i,j,iter
c Common Vars
      double precision         span,alpha,big
      integer           ifl,lf
      common /pprpar/ ifl,lf,span,alpha,big

      double precision conv,            cutmin,fdel,cjeps
      integer              maxit,mitone,                  mitcj
      common /pprz01/ conv,maxit,mitone,cutmin,fdel,cjeps,mitcj

      iter=0
      asr=big
C REPEAT
1000  continue
      iter=iter+1
      asrold=asr
      do 11 j=1,n
         s=0d0
         do 21 i=1,q
 21         s=s+ww(i)*b(i)*y(i,j)
         sc(j,13)=s
11    continue
      call oneone(max0(jfl,iter-1),p,n,w,sw,sc(1,13),x,a,f,t,
     &                 asr,sc,g,dp,edf)
      do 31 i=1,q
        s=0d0
        do 41 j=1,n
 41        s=s+w(j)*y(i,j)*f(j)
        b(i)=s/sw
31    continue
      asr=0d0
      do 51 i=1,q
         s=0d0
         do 61 j=1,n
 61         s=s+w(j)*(y(i,j)-b(i)*f(j))**2
         asr=asr+ww(i)*s/sw
51    continue
      if((q .ne. 1) .and. (iter .le. maxit) .and. (asr .gt. 0d0)
     &      .and. (asrold-asr)/asrold .ge. conv) goto 1000
      return
      end

      subroutine oneone(ist,p,n, w,sw,y,x,a,f,t,asr,sc,g,dp,edf)
c Args
      integer              ist,p,n
      double precision w(n),sw,y(n),x(p,n),a(p),f(n),t(n),asr,
     &     sc(n,12), g(p,2), edf, dp(*)
c Var
      integer i,j,k,iter
      double precision sml, s,v,cut,asrold
c Common Vars
      double precision         span,alpha,big
      integer           ifl,lf
      common /pprpar/ ifl,lf,span,alpha,big

      double precision conv,            cutmin,fdel,cjeps
      integer              maxit,mitone,                  mitcj
      common /pprz01/ conv,maxit,mitone,cutmin,fdel,cjeps,mitcj

      sml=1d0/big
      if(ist .le. 0) then
        if(p .le. 1) a(1)=1d0
        do 10 j=1,n
          sc(j,2)=1d0
 10     continue
        call pprdir(p,n,w,sw,y,x,sc(1,2),a,dp)
      endif
      s=0d0
      do 20 i=1,p
        g(i,1)=0d0
        s=s+a(i)**2
 20   continue
      s=1d0/sqrt(s)
      do 30 i=1,p
        a(i)=a(i)*s
 30   continue
      iter=0
      asr=big
      cut=1d0
C REPEAT -----------------------------
 100  continue
      iter=iter+1
      asrold=asr
C REPEAT [inner loop] -----
 60   continue
      s=0d0
      do 70 i=1,p
        g(i,2)=a(i)+g(i,1)
        s=s+g(i,2)**2
 70   continue
      s=1.d0/sqrt(s)
      do 80 i=1,p
        g(i,2)=g(i,2)*s
 80   continue
      do 90 j=1,n
        sc(j,1)=j+0.1d0
        s=0.d0
        do 91 i=1,p
          s=s+g(i,2)*x(i,j)
 91     continue
        sc(j,11)=s
 90   continue
      call sort(sc(1,11),sc,1,n)
      do 110 j=1,n
        k=sc(j,1)
        sc(j,2)=y(k)
        sc(j,3)=max(w(k),sml)
 110  continue
      call supsmu(n,sc(1,11),sc(1,2),sc(1,3),1,span,alpha,
     &     sc(1,12),sc(1,4), edf)
      s=0d0
      do 120 j=1,n
        s=s+sc(j,3)*(sc(j,2)-sc(j,12))**2
 120  continue
      s=s/sw
      if(s .lt. asr) goto 140
      cut=cut*0.5d0
      if(cut.lt.cutmin) goto 199
      do 150 i=1,p
        g(i,1)=g(i,1)*cut
 150  continue
      go to 60
C     --------
 140  continue
      asr=s
      cut=1d0
      do 160 i=1,p
        a(i)=g(i,2)
 160  continue
      do 170 j=1,n
        k=sc(j,1)
        t(k)=sc(j,11)
        f(k)=sc(j,12)
 170  continue
      if(asr.le.0d0.or.(asrold-asr)/asrold.lt.conv) goto 199
      if(iter.gt.mitone.or.p.le.1) goto 199
      call pprder(n,sc(1,11),sc(1,12),sc(1,3),fdel,sc(1,4),sc(1,5))
      do 180 j=1,n
        k=sc(j,1)
        sc(j,5)=y(j)-f(j)
        sc(k,6)=sc(j,4)
 180  continue
      call pprdir(p,n,w,sw,sc(1,5),x,sc(1,6),g,dp)

      goto 100
c--------------
 199   continue
c--------------
      s=0d0
      v=s
      do 210 j=1,n
        s=s+w(j)*f(j)
 210  continue
      s=s/sw
      do 220 j=1,n
        f(j)=f(j)-s
        v=v+w(j)*f(j)**2
 220  continue
      if(v .gt. 0d0) then
        v=1d0/sqrt(v/sw)
        do 230 j=1,n
 230      f(j)=f(j)*v
      endif
      return
      end


      subroutine pprdir(p,n,w,sw,r,x,d,e,g)

      integer           p,n
      double precision      w(n),sw,r(n),x(p,n),d(n),e(p), g(*)

      double precision s
      integer i,j,k,l,m1,m2

      double precision conv,            cutmin,fdel,cjeps
      integer              maxit,mitone,                  mitcj
      common /pprz01/ conv,maxit,mitone,cutmin,fdel,cjeps,mitcj

      do 10 i=1,p
         s=0d0
         do 15 j=1,n
            s=s+w(j)*d(j)*x(i,j)
 15      continue
         e(i)=s/sw
 10   continue
      k=0
      m1=p*(p+1)/2
      m2=m1+p
      do 20 j=1,p
         s=0d0
         do 22 l=1,n
            s=s+w(l)*r(l)*(d(l)*x(j,l)-e(j))
 22      continue
         g(m1+j)=s/sw
         do 25 i=1,j
            s=0d0
            do 27 l=1,n
               s=s+w(l)*(d(l)*x(i,l)-e(i))*(d(l)*x(j,l)-e(j))
 27         continue
            k=k+1
            g(k)=s/sw
 25      continue
 20   continue
      call ppconj(p,g,g(m1+1),g(m2+1),cjeps,mitcj,g(m2+p+1))
      do 30 i=1,p
         e(i)=g(m2+i)
 30   continue
      return
      end

      subroutine ppconj(p,g,c,x,eps,maxit,sc)
      integer p,maxit
      double precision g(*),c(p),x(p),eps,sc(p,4)

      integer i,j,im1,iter,nit
      double precision beta,h,s,alpha,t

      do 1 i=1,p
         x(i)=0d0
         sc(i,2)=0d0
 1    continue
      nit=0
C REPEAT
11321 continue
      nit=nit+1
      h=0d0
      beta=0d0
      do 11331 i=1,p
        sc(i,4)=x(i)
        s=g(i*(i-1)/2+i)*x(i)
        im1=i-1
        j=1
        goto 11343
11341   j=j+1
11343   if(j.gt.im1) goto 11342
        s=s+g(i*(i-1)/2+j)*x(j)
        goto 11341
11342   continue
        j=i+1
        goto 11353
11351   j=j+1
11353   if(j.gt.p) goto 11352
        s=s+g(j*(j-1)/2+i)*x(j)
        goto 11351
11352   continue
        sc(i,1)=s-c(i)
        h=h+sc(i,1)**2
11331 continue
      if(h.le.0d0) goto 11322
      do 11361 iter=1,p
        do 11371 i=1,p
          sc(i,2)=beta*sc(i,2)-sc(i,1)
11371   continue
        t=0d0
        do 11381 i=1,p
          s=g(i*(i-1)/2+i)*sc(i,2)
          im1=i-1
          j=1
          goto 11393
11391     j=j+1
11393     if(j.gt.im1) goto 11392
          s=s+g(i*(i-1)/2+j)*sc(j,2)
          goto 11391
11392     continue
          j=i+1
          goto 11403
11401     j=j+1
11403     if(j.gt.p) goto 11402
          s=s+g(j*(j-1)/2+i)*sc(j,2)
          goto 11401
11402     continue
          sc(i,3)=s
          t=t+s*sc(i,2)
11381   continue
        alpha=h/t
        s=0d0
        do 11411 i=1,p
          x(i)=x(i)+alpha*sc(i,2)
          sc(i,1)=sc(i,1)+alpha*sc(i,3)
          s=s+sc(i,1)**2
11411   continue
        if(s.le.0d0) goto 11362
        beta=s/h
        h=s
11361 continue
11362 continue
      s=0d0
      do 11421 i=1,p
        s=dmax1(s,dabs(x(i)-sc(i,4)))
11421 continue
      if((s .ge. eps) .and. (nit .lt. maxit)) goto 11321
11322 continue
      return
      end

      subroutine pprder (n,x,s,w,fdel,d,sc)
      integer n
      double precision x(n),s(n),w(n), fdel, d(n),sc(n,3)

      integer i,j,bl,el,bc,ec,br,er
      double precision scale, del
c
c     unnecessary initialization of bl el ec to keep g77 -Wall happy
c
      bl = 0
      el = 0
      ec = 0
c
      if(x(n) .gt. x(1)) goto 11441
      do 11451 j=1,n
      d(j)=0d0
11451 continue
      return
11441 continue
      i=n/4
      j=3*i
      scale=x(j)-x(i)
11461 if(scale.gt.0d0) goto 11462
      if(j.lt.n) j=j+1
      if(i.gt.1) i=i-1
      scale=x(j)-x(i)
      goto 11461
11462 continue
      del=fdel*scale*2d0
      do 11471 j=1,n
      sc(j,1)=x(j)
      sc(j,2)=s(j)
      sc(j,3)=w(j)
11471 continue
      call pool (n,sc,sc(1,2),sc(1,3),del)
      bc=0
      br=bc
      er=br
11481 continue
      br=er+1
      er=br
11491 if(er .ge. n) goto 11492
      if(sc(br,1) .ne. sc(er+1,1)) goto 11511
      er=er+1
      goto 11521
11511 continue
      goto 11492
11521 continue
      goto 11491
11492 continue
      if(br .ne. 1) goto 11541
      bl=br
      el=er
      goto 11481
11541 continue
      if(bc .ne. 0) goto 11561
      bc=br
      ec=er
      do 11571 j=bl,el
      d(j)=(sc(bc,2)-sc(bl,2))/(sc(bc,1)-sc(bl,1))
11571 continue
      goto 11481
11561 continue
c sanity check needed for PR#13517
      if(br.gt.n) call rexit('br is too large')
      do 11581 j=bc,ec
      d(j)=(sc(br,2)-sc(bl,2))/(sc(br,1)-sc(bl,1))
11581 continue
      if(er .ne. n) goto 11601
      do 11611 j=br,er
      d(j)=(sc(br,2)-sc(bc,2))/(sc(br,1)-sc(bc,1))
11611 continue
      goto 11482
11601 continue
      bl=bc
      el=ec
      bc=br
      ec=er
      goto 11481
11482 continue
      return
      end

      subroutine pool (n,x,y,w,del)
      integer n
      double precision x(n),y(n),w(n),del

      integer i,bb,eb,br,er,bl,el
      double precision px, py, pw

      bb=0
      eb=bb
11621 if(eb.ge.n) goto 11622
      bb=eb+1
      eb=bb
11631 if(eb .ge. n) goto 11632
      if(x(bb) .ne. x(eb+1)) goto 11651
      eb=eb+1
      goto 11661
11651 continue
      goto 11632
11661 continue
      goto 11631
11632 continue
      if(eb .ge. n) goto 11681
      if(x(eb+1)-x(eb) .ge. del) goto 11701
      br=eb+1
      er=br
11711 if(er .ge. n) goto 11712
      if(x(er+1) .ne. x(br)) goto 11731
      er=er+1
      goto 11741
11731 continue
      goto 11712
11741 continue
      goto 11711
11712 continue
C avoid bounds error: this was .and. but order is not guaranteed
      if(er.lt.n) then
        if(x(er+1)-x(er).lt.x(eb+1)-x(eb)) goto 11621
      endif
      eb=er
      pw=w(bb)+w(eb)
      px=(x(bb)*w(bb)+x(eb)*w(eb))/pw
      py=(y(bb)*w(bb)+y(eb)*w(eb))/pw
      do 11751 i=bb,eb
      x(i)=px
      y(i)=py
      w(i)=pw
11751 continue
11701 continue
11681 continue
11761 continue
      if(bb.le.1) goto 11762
      if(x(bb)-x(bb-1).ge.del) goto 11762
      bl=bb-1
      el=bl
11771 if(bl .le. 1) goto 11772
      if(x(bl-1) .ne. x(el)) goto 11791
      bl=bl-1
      goto 11801
11791 continue
      goto 11772
11801 continue
      goto 11771
11772 continue
      bb=bl
      pw=w(bb)+w(eb)
      px=(x(bb)*w(bb)+x(eb)*w(eb))/pw
      py=(y(bb)*w(bb)+y(eb)*w(eb))/pw
      do 11811 i=bb,eb
      x(i)=px
      y(i)=py
      w(i)=pw
11811 continue
      goto 11761
11762 continue
      goto 11621
11622 continue
      return
      end

      subroutine newb(lm,q,ww,b)
      integer lm, q
      double precision ww(q),b(q,lm)

      integer i,lm1,l,l1
      double precision s,t,sml
c Common
      double precision         span,alpha,big
      integer           ifl,lf
      common /pprpar/ ifl,lf,span,alpha,big


      sml=1d0/big
      if(q .ne. 1) goto 11831
      b(1,lm)=1d0
      return
11831 continue
      if(lm .ne. 1) goto 11851
      do 11861 i=1,q
      b(i,lm)=i
11861 continue
      return
11851 continue
      lm1=lm-1
      do 11871 i=1,q
      b(i,lm)=0d0
11871 continue
      t=0d0
      do 11881 i=1,q
      s=0d0
      do 11891 l=1,lm1
      s=s+abs(b(i,l))
11891 continue
      b(i,lm)=s
      t=t+s
11881 continue
      do 11901 i=1,q
      b(i,lm)=ww(i)*(t-b(i,lm))
11901 continue
      l1=1
      if(lm.gt.q) l1=lm-q+1
      do 11911 l=l1,lm1
      s=0d0
      t=s
      do 11921 i=1,q
      s=s+ww(i)*b(i,lm)*b(i,l)
      t=t+ww(i)*b(i,l)**2
11921 continue
      s=s/sqrt(t)
      do 11931 i=1,q
      b(i,lm)=b(i,lm)-s*b(i,l)
11931 continue
11911 continue
      do 11941 i=2,q
      if(abs(b(i-1,lm)-b(i,lm)).gt.sml) return
11941 continue
      do 11951 i=1,q
      b(i,lm)=i
11951 continue
      return
      end

      block data bkppr

c Common Vars
      double precision         span,alpha,big
      integer           ifl,lf
      common /pprpar/ ifl,lf,span,alpha,big

      double precision conv,            cutmin,fdel,cjeps
      integer              maxit,mitone,                  mitcj
      common /pprz01/ conv,maxit,mitone,cutmin,fdel,cjeps,mitcj

      double precision     df, gcvpen
      integer                          ismethod
      common /spsmooth/ df, gcvpen, ismethod

      data ifl,maxit, conv, mitone, cutmin, fdel,
     &     span,alpha, big, cjeps, mitcj, lf
     &     /6,  20,   .005,   20,     0.1,  0.02,
     &     0.0,  0.0,1.0e20,0.001,   1,    2/
      data df, gcvpen, ismethod /4d0, 1d0, 0/
      end

      subroutine setppr(span1, alpha1, optlevel, ism, df1, gcvpen1)
c Put `parameters' into Common blocks
      integer optlevel,ism
      double precision span1,alpha1, df1, gcvpen1

      double precision         span,alpha,big
      integer           ifl,lf
      common /pprpar/ ifl,lf,span,alpha,big

      double precision     df, gcvpen
      integer                          ismethod
      common /spsmooth/ df, gcvpen, ismethod

      span = span1
      lf = optlevel
      alpha = alpha1
      ismethod = ism
      df = df1
      gcvpen = gcvpen1
      return
      end

      subroutine fsort(mu,n,f,t,sp)
c
      integer mu, n
      double precision f(n,mu),t(n,mu),sp(n,2)
c
      integer l,j,k

      do 100 l=1,mu
         do 10 j=1,n
            sp(j,1)=j+0.1d0
            sp(j,2)=f(j,l)
 10      continue
         call sort(t(1,l),sp,1,n)
         do 20 j=1,n
            k=sp(j,1)
            f(j,l)=sp(k,2)
 20      continue
 100  continue
      return
      end

      subroutine pppred(np,x,smod,y,sc)

      integer np
      double precision x(np,*),y(np,*),smod(*), sc(*)

      integer p,q, place,low,high, i,j,l,m,n,
     +     inp,ja,jb,jf,jt,jfl,jfh,jtl,jth, mu
      double precision ys, s, t

      m=smod(1)+0.1d0
      p=smod(2)+0.1d0
      q=smod(3)+0.1d0
      n=smod(4)+0.1d0
      mu=smod(5)+0.1d0
      ys=smod(q+6)
      ja=q+6
      jb=ja+p*m
      jf=jb+m*q
      jt=jf+n*m
      call fsort(mu,n,smod(jf+1),smod(jt+1),sc)
      do 100 inp = 1, np
        ja=q+6
        jb=ja+p*m
        jf=jb+m*q
        jt=jf+n*m
        do 81 i=1,q
          y(inp,i)=0d0
 81     continue
        do 91 l=1,mu
          s=0d0
          do 12201 j=1,p
            s=s+smod(ja+j)*x(inp,j)
12201     continue
          if(s .gt. smod(jt+1)) goto 12221
          place=1
          go to 12230
12221     continue
          if(s .lt. smod(jt+n)) goto 12251
          place=n
          go to 12230

12251     continue
          low=0
          high=n+1
C        WHILE
12261     if(low+1.ge.high) goto 12262
          place=(low+high)/2
          t=smod(jt+place)
          if(s.eq.t) goto 12230
          if(s .lt. t) then
             high=place
          else
             low=place
          endif
          goto 12261
C        END
12262     continue
          jfl=jf+low
          jfh=jf+high
          jtl=jt+low
          jth=jt+high
          t=smod(jfl)+(smod(jfh)-smod(jfl))*(s-smod(jtl))  /
     &         (smod(jth)-smod(jtl))
          go to 12300
12230     continue
          t=smod(jf+place)
12300     continue
          do 12311 i=1,q
             y(inp,i)=y(inp,i)+smod(jb+i)*t
12311     continue
          ja=ja+p
          jb=jb+q
          jf=jf+n
          jt=jt+n
 91     continue
        do 12321 i=1,q
           y(inp,i)=ys*y(inp,i)+smod(i+5)
12321   continue
 100  continue
      return
      end

      subroutine setsmu
      double precision     df, gcvpen
      integer                          ismethod
      common /spsmooth/ df, gcvpen, ismethod

      ismethod = 0
      return
      end

      subroutine supsmu (n,x,y,w,iper,span,alpha,smo,sc,edf)
c
c------------------------------------------------------------------
c
c super smoother (Friedman, 1984).
c
c version 10/10/84
c
c coded  and copywrite (c) 1984 by:
c
c                        Jerome H. Friedman
c                     department of statistics
c                               and
c                stanford linear accelerator center
c                        stanford university
c
c all rights reserved.
c
c
c input:
c    n : number of observations (x,y - pairs).
c    x(n) : ordered abscissa values.
c    y(n) : corresponding ordinate (response) values.
c    w(n) : weight for each (x,y) observation.
c    iper : periodic variable flag.
c       iper=1 => x is ordered interval variable.
c       iper=2 => x is a periodic variable with values
c                 in the range (0.0,1.0) and period 1.0.
c    span : smoother span (fraction of observations in window).
c           span=0.0 => automatic (variable) span selection.
c    alpha : controls high frequency (small span) penality
c            used with automatic span selection (bass tone control).
c            (alpha.le.0.0 or alpha.gt.10.0 => no effect.)
c output:
c   smo(n) : smoothed ordinate (response) values.
c scratch:
c   sc(n,7) : internal working storage.
c
c note:
c    for small samples (n < 40) or if there are substantial serial
c    correlations between observations close in x - value, then
c    a prespecified fixed span smoother (span > 0) should be
c    used. reasonable span values are 0.2 to 0.4.
c
c------------------------------------------------------------------

c Args
      integer n, iper
      double precision x(n),y(n),w(n), smo(n),sc(n,7)
      double precision span, alpha, edf
c Var
      double precision sy,sw, a,h(n),f, scale,vsmlsq,resmin
      integer i,j, jper

      double precision  spans(3),          big,sml,eps
      common /spans/ spans  /consts/ big,sml,eps

      double precision     df, gcvpen
      integer                          ismethod
      common /spsmooth/ df, gcvpen, ismethod

      if (x(n).gt.x(1)) go to 30
      sy=0d0
      sw=sy
      do 10 j=1,n
         sy=sy+w(j)*y(j)
         sw=sw+w(j)
 10   continue
      a=0d0
      if (sw.gt.0d0) a=sy/sw
      do 20 j=1,n
         smo(j)=a
 20   continue
      return
 30   continue

C     change by
      if (ismethod .ne. 0) then
        call spline(n, x, y, w, smo, edf)
      else
         i=n/4
         j=3*i
         scale=x(j)-x(i)
 40      if (scale.gt.0d0) go to 50
         if (j.lt.n) j=j+1
         if (i.gt.1) i=i-1
         scale=x(j)-x(i)
         go to 40
 50      vsmlsq=(eps*scale)**2
         jper=iper
         if (iper.eq.2.and.(x(1).lt.0d0.or.x(n).gt.1d0)) jper=1
         if (jper.lt.1.or.jper.gt.2) jper=1
         if (span.le.0d0) go to 60
         call smooth (n,x,y,w,span,jper,vsmlsq,smo,sc)
         return
 60      do 70 i=1,3
            call smooth (n,x,y,w,spans(i),jper,vsmlsq,
     &           sc(1,2*i-1),sc(1,7))
            call smooth (n,x,sc(1,7),w,spans(2),-jper,vsmlsq,
     &           sc(1,2*i),h)
 70      continue
         do 90 j=1,n
            resmin=big
            do 80 i=1,3
               if (sc(j,2*i).ge.resmin) go to 80
               resmin=sc(j,2*i)
               sc(j,7)=spans(i)
 80         continue
            if (alpha.gt.0d0.and.alpha.le.10d0 .and.
     &           resmin.lt.sc(j,6).and.resmin.gt.0d0)
     &           sc(j,7)= sc(j,7)+(spans(3)-sc(j,7)) *
     &                          max(sml,resmin/sc(j,6))**(10d0-alpha)
 90      continue

         call smooth (n,x,sc(1,7),w,spans(2),-jper,vsmlsq,sc(1,2),h)
         do 110 j=1,n
            if (sc(j,2).le.spans(1)) sc(j,2)=spans(1)
            if (sc(j,2).ge.spans(3)) sc(j,2)=spans(3)
            f=sc(j,2)-spans(2)
            if (f.ge.0d0) go to 100
            f=-f/(spans(2)-spans(1))
            sc(j,4)=(1d0-f)*sc(j,3)+f*sc(j,1)
            go to 110
 100        f=f/(spans(3)-spans(2))
            sc(j,4)=(1d0-f)*sc(j,3)+f*sc(j,5)
 110     continue
         call smooth (n,x,sc(1,4),w,spans(1),-jper,vsmlsq,smo,h)
         edf = 0
      endif
      return
      end

      subroutine smooth (n,x,y,w,span,iper,vsmlsq,smo,acvr)
c Args
      integer n, iper
      double precision x(n),y(n),w(n), span,vsmlsq, smo(n),acvr(n)
c Var
      integer i,j, in,out, jper,ibw,it, j0
      double precision xm,ym,var,cvar, fbw,fbo,xti,xto,tmp, a,h,sy,wt

      xm=0d0
      ym=xm
      var=ym
      cvar=var
      fbw=cvar
      jper=iabs(iper)
      ibw=0.5d0*span*n+0.5d0
      if (ibw.lt.2) ibw=2
      it=2*ibw+1
      if (it .gt. n) it = n
      do 20 i=1,it
         j=i
         if (jper.eq.2) j=i-ibw-1
         if (j.ge.1) xti=x(j)
         if (j.ge.1) go to 10
         j=n+j
         xti=x(j)-1d0
 10      wt=w(j)
         fbo=fbw
         fbw=fbw+wt
         if (fbw.gt.0d0) xm=(fbo*xm+wt*xti)/fbw
         if (fbw.gt.0d0) ym=(fbo*ym+wt*y(j))/fbw
         tmp=0d0
         if (fbo.gt.0d0) tmp=fbw*wt*(xti-xm)/fbo
         var=var+tmp*(xti-xm)
         cvar=cvar+tmp*(y(j)-ym)
 20   continue
      do 80 j=1,n
         out=j-ibw-1
         in=j+ibw
         if ((jper.ne.2).and.(out.lt.1.or.in.gt.n)) go to 60
         if (out.ge.1) go to 30
         out=n+out
         xto=x(out)-1d0
         xti=x(in)
         go to 50
 30      if (in.le.n) go to 40
         in=in-n
         xti=x(in)+1d0
         xto=x(out)
         go to 50
 40      xto=x(out)
         xti=x(in)
 50      wt=w(out)
         fbo=fbw
         fbw=fbw-wt
         tmp=0d0
         if (fbw.gt.0d0) tmp=fbo*wt*(xto-xm)/fbw
         var=var-tmp*(xto-xm)
         cvar=cvar-tmp*(y(out)-ym)
         if (fbw.gt.0d0) xm=(fbo*xm-wt*xto)/fbw
         if (fbw.gt.0d0) ym=(fbo*ym-wt*y(out))/fbw
         wt=w(in)
         fbo=fbw
         fbw=fbw+wt
         if (fbw.gt.0d0) xm=(fbo*xm+wt*xti)/fbw
         if (fbw.gt.0d0) ym=(fbo*ym+wt*y(in))/fbw
         tmp=0d0
         if (fbo.gt.0d0) tmp=fbw*wt*(xti-xm)/fbo
         var=var+tmp*(xti-xm)
         cvar=cvar+tmp*(y(in)-ym)
 60      a=0d0
         if (var.gt.vsmlsq) a=cvar/var
         smo(j)=a*(x(j)-xm)+ym
         if (iper.le.0) go to 80
         h=0d0
         if (fbw.gt.0d0) h=1d0/fbw
         if (var.gt.vsmlsq) h=h+(x(j)-xm)**2/var
         acvr(j)=0d0
         a=1d0-w(j)*h
         if (a.le.0d0) go to 70
         acvr(j)=abs(y(j)-smo(j))/a
         go to 80
 70      if (j.le.1) go to 80
         acvr(j)=acvr(j-1)
 80   continue
      j=1
c--
 90   j0=j
      sy=smo(j)*w(j)
      fbw=w(j)
      if (j.ge.n) go to 110
 100  if (x(j+1).gt.x(j)) go to 110
      j=j+1
      sy=sy+w(j)*smo(j)
      fbw=fbw+w(j)
      if (j.lt.n) go to 100
 110  if (j.le.j0) go to 130
      a=0d0
      if (fbw.gt.0d0) a=sy/fbw
      do 120 i=j0,j
         smo(i)=a
 120  continue
 130  j=j+1
      if (j.le.n) go to 90
      return
      end

      block data bksupsmu
      double precision spans(3), big,sml,eps
      common /spans/ spans /consts/ big,sml,eps

      data spans, big,sml,eps /0.05,0.2,0.5, 1.0e20,1.0e-7,1.0e-3/
      end
c---------------------------------------------------------------
c
c this sets the compile time (default) values for various
c internal parameters :
c
c spans : span values for the three running linear smoothers.
c spans(1) : tweeter span.
c spans(2) : midrange span.
c spans(3) : woofer span.
c       (these span values should be changed only with care.)
c big : a large representable floating point number.
c sml : a small number. should be set so that (sml)**(10.0) does
c       not cause floating point underflow.
c eps : used to numerically stabilize slope calculations for
c       running linear fits.
c
c these parameter values can be changed by declaring the
c relevant labeled common in the main program and resetting
c them with executable statements.
c
c-----------------------------------------------------------------

      subroutine spline (n, x, y, w, smo, edf)
c
c------------------------------------------------------------------
c
c input:
c    n : number of observations.
c    x(n) : ordered abscissa values.
c    y(n) : corresponding ordinate (response) values.
c    w(n) : weight for each (x,y) observation.
c output:
c   smo(n) : smoothed ordinate (response) values.
c   edf : equivalent degrees of freedom
c
c------------------------------------------------------------------
c Args
      integer n
      double precision x(n), y(n), w(n), smo(n), edf
c Var
      double precision knot(29), coef(25), work((17+25)*25)
      double precision dx(2500),dy(2500), dw(2500),dsmo(2500), lev(2500)
      double precision param(4), df1, lambda, crit, p, s
      integer iparms(3), i, nk, ip, isetup,ier

      double precision     df, gcvpen
      integer                          ismethod
      common /spsmooth/ df, gcvpen, ismethod

      if (n .gt. 2500) call bdrsplerr()
      do 10 i = 1,n
        dx(i) = (x(i)-x(1))/(x(n)-x(1))
        dy(i) = y(i)
 10     dw(i) = w(i)
      nk = min(n,15)
      knot(1) = dx(1)
      knot(2) = dx(1)
      knot(3) = dx(1)
      knot(4) = dx(1)
      knot(nk+1) = dx(n)
      knot(nk+2) = dx(n)
      knot(nk+3) = dx(n)
      knot(nk+4) = dx(n)
      do 40 i = 5, nk
        p = (n-1)*real(i-4)/real(nk-3)
        ip = int(p)
        p = p-ip
        knot(i) = (1-p)*dx(ip+1) + p*dx(ip+2)
 40   continue
c      call dblepr('knots', 5, knot, nk+4)
C     iparms(1:2) := (icrit, ispar)  for ./sbart.f
      if (iabs(ismethod) .eq. 1) then
         iparms(1) = 3
         df1 = df
      else
         iparms(1) = 1
         df1 = 0d0
      endif
c     ispar := 0 <==> estimate `spar' :
      iparms(2) = 0
c     maxit = 500 :
      iparms(3) = 500
      param(1) = 0d0
      param(2) = 1.5d0
c  tol for `spar' estimation:
      param(3) = 1d-2
c   this was `eps' (=? sqrt(machine eps)) in ./sbart.f :
      param(4) = .000244

      ier = 1
      call rbart(gcvpen,df1,dx,dy,dw,0.0d0,n,knot,nk,coef,dsmo,lev,
     &     crit,iparms,lambda,param, work,4,1,ier)
      if(ier .gt. 0) call intpr('TROUBLE:',8, ier, 1)
      do 50 i = 1,n
 50      smo(i) = dsmo(i)
c      call dblepr('smoothed',8, dsmo, n)
      s = 0
      do 60 i = 1, n
 60      s = s + lev(i)
      edf = s
      if(ismethod.lt.0) then
         call dblepr('lambda', 6, lambda, 1)
         call dblepr('df', 2, s, 1)
      endif
      return
      end


***********************************************************************

C=== This was 'sort()' in  gamfit's  mysort.f  [or sortdi() in sortdi.f ] :
C
C===  FIXME:  Translate to C and add to ../../../main/sort.c <<<<<
C
C     why on earth is       a() double precision ????
      subroutine sort (v,a, ii,jj)
c
c     Puts into a the permutation vector which sorts v into
c     increasing order.  Only elements from ii to jj are considered.
c     Arrays iu(k) and il(k) permit sorting up to 2**(k+1)-1 elements
c
c     This is a modification of CACM algorithm #347 by R. C. Singleton,
c     which is a modified Hoare quicksort.
c
      integer ii,jj
      double precision v(*), a(jj)
c
      integer iu(20),il(20)
      integer t,tt, m,i,j,ij,k,l
      double precision vt, vtt

      m=1
      i=ii
      j=jj
 10   if (i.ge.j) go to 80
 20   k=i
      ij=(j+i)/2
      t=a(ij)
      vt=v(ij)
      if (v(i).le.vt) go to 30
      a(ij)=a(i)
      a(i)=t
      t=a(ij)
      v(ij)=v(i)
      v(i)=vt
      vt=v(ij)
 30   l=j
      if (v(j).ge.vt) go to 50
      a(ij)=a(j)
      a(j)=t
      t=a(ij)
      v(ij)=v(j)
      v(j)=vt
      vt=v(ij)
      if (v(i).le.vt) go to 50
      a(ij)=a(i)
      a(i)=t
      t=a(ij)
      v(ij)=v(i)
      v(i)=vt
      vt=v(ij)
      go to 50
 40   a(l)=a(k)
      a(k)=tt
      v(l)=v(k)
      v(k)=vtt
 50   l=l-1
      if (v(l).gt.vt) go to 50
      tt=a(l)
      vtt=v(l)
 60   k=k+1
      if (v(k).lt.vt) go to 60
      if (k.le.l) go to 40
      if (l-i.le.j-k) go to 70
      il(m)=i
      iu(m)=l
      i=k
      m=m+1
      go to 90
 70   il(m)=k
      iu(m)=j
      j=l
      m=m+1
      go to 90
 80   m=m-1
      if (m.eq.0) return
      i=il(m)
      j=iu(m)
 90   if (j-i.gt.10) go to 20
      if (i.eq.ii) go to 10
      i=i-1
 100  i=i+1
      if (i.eq.j) go to 80
      t=a(i+1)
      vt=v(i+1)
      if (v(i).le.vt) go to 100
      k=i
 110  a(k+1)=a(k)
      v(k+1)=v(k)
      k=k-1
      if (vt.lt.v(k)) go to 110
      a(k+1)=t
      v(k+1)=vt
      go to 100
      end
