      subroutine nscor1 (s, n, n2, work, ifault)
c
c        Algorithm AS 177   Appl. Statist. (1982) Vol. 31, No. 2
c
c        Exact calculation of Normal Scores
c
      implicit none
      integer n, n2, ifault
      double precision s(n2), work(4,721)

      double precision zero, c, scor, ai1, ani, an, h
      integer nstep, i, j, i1, ni
      data zero/0.0d0/, h/0.025d0/, nstep/721/
c
      ifault=3
      if (n2 .ne. n/2) return
      ifault=1
      if (n .le. 1) return
      ifault=0
      if (n .gt. 2000) ifault=2
c
      an=n
      c=log(an)
c
c        Accumulate ordinates for calculation of integral for rankits
c
      do 20 i=1, n2
        i1=i-1
        ni=n-i
        ai1=i1
        ani=ni
        scor=zero
        do 10 j=1,nstep
   10      scor=scor+exp(work(2,j) + ai1 * work(3,j) + ani * work(4,j)
     *          + c) * work(1,j)
        s(i)=scor * h
        c=c+log(ani/dble(i))
   20 continue
      return
      end
c
c
      subroutine init(work)
c
c        Algorithm AS 177.1   Appl. Statist. (1982) Vol. 31, No. 2
c
      implicit none
      double precision work(4,721)
c     calls EXTERNAL alnorm { = Appl.Stat. AS 66 }   = {1 - } pnorm(.)
      double precision alnorm
c
      double precision xstart, h, pi2, half, xx
      integer nstep, i
      data xstart/-9.0d0/, h/0.025d0/, pi2/-0.918938533d0/,
     *     half/0.5d0/,  nstep/721/
      xx=xstart
c
c        Set up arrays for calculation of integral
c
      do 10 i=1,nstep
        work(1,i)=xx
        work(2,i)=pi2 - xx * xx * half
        work(3,i)=log(alnorm(xx, .true.))
        work(4,i)=log(alnorm(xx, .false.))
        xx=xstart + dble(i) * h
   10 continue
      return
      end
c
c
      subroutine nscor2(s,n,n2,ier)
c
c     algorithm as 177.3, applied statistics, v.31, 161-165, 1982.
c
c     calculates approximate expected values of normal order statistics.
c     claimed accuracy is 0.0001, though usually accurate to 5-6 dec.
c
c***  N.B. This routine is NOT in double precision ***
c
c     arguments:
c     s(n2)   = output, the first n2 expected values.
c     n       = input, the sample size.
c     n2      = input, the number of order statistics required; must
c                      be <= n/2.
c     ier     = output, error indicator
c                   = 0 if no error detected
c                   = 1 if n <= 1.
c                   = 2 if n > 2000, in which case the order statistics
c                          are still calculated, but may be inaccurate.
c                   = 3 if n2 > n/2 (n.b. this differs from the
c                          published algorithm which returns an error
c                          if n2 is not equal to n/2.)
c
c     calls ppnd = applied statistics algorithm 111. =^= qnorm() in R
c     An alternative is ppnd7 in algorithm AS 241.
c
      real s(n2), eps(4), dl1(4), dl2(4), gam(4), lam(4),
     *   bb, d, b1, an, ai, e1, e2, l1, correc
      data eps/0.419885e0, 0.450536e0, 0.456936e0, 0.468488e0/,
     1 dl1/0.112063e0, 0.121770e0, 0.239299e0, 0.215159e0/,
     2 dl2/0.080122e0, 0.111348e0, -0.211867e0, -0.115049e0/,
     3 gam/0.474798e0, 0.469051e0, 0.208597e0, 0.259784e0/,
     4 lam/0.282765e0, 0.304856e0, 0.407708e0, 0.414093e0/,
     5 bb/-0.283833/, d/-0.106136/, b1/0.5641896/
c
c     input parameter checks.
c
      ier = 3
      if(n2.gt.n/2) return
      ier = 1
      if(n.le.1) return
      ier = 0
      if(n.gt.2000) ier = 2
      s(1) = b1
      if(n.eq.2) return
c
c     calculate normal tail areas for first 3 order statistics.
c
      an = n
      k = 3
      if(n2.lt.k) k = n2
      do 5 i = 1,k
        ai = i
        e1 = (ai - eps(i))/(an + gam(i))
        e2 = e1**lam(i)
        s(i) = e1 + e2*(dl1(i) + e2*dl2(i))/an - correc(i,n)
    5 continue
      if(n2.eq.k) go to 20
c
c     calculate normal areas for other cases.
c
      do 10 i = 4,n2
        ai = i
        l1 = lam(4) + bb/(ai + d)
        e1 = (ai - eps(4))/(an + gam(4))
        e2 = e1**l1
        s(i) = e1 + e2*(dl1(4) + e2*dl2(4))/an - correc(i,n)
   10 continue
c
c     convert tail areas to normal deviates.
c
 20   do 30 i = 1,n2
   30   s(i) = -real(ppnd(dble(s(i)),ier))
      return
      end
c
c
      real function correc(i,n)
c
c     calculates correction for tail area of the i-th largest of n
c     order statistics.
c
      dimension c1(7),c2(7),c3(7)
      real mic
      data c1/9.5, 28.7, 1.9, 0., -7.0, -6.2, -1.6/,
     1 c2/-6195., -9569., -6728., -17614., -8278., -3570., 1075./,
     2 c3/9.338e4, 1.7516e5, 4.1040e5, 2.1576e6, 2.376e6, 2.065e6,
     3 2.065e6/, mic/1.e-6/, c14/1.9e-5/
c
      correc = c14
      if(i*n.eq.4) return
      correc = 0.0
      if(i.lt.1.or.i.gt.7) return
      if(i.ne.4.and.n.gt.20) return
      if(i.eq.4.and.n.gt.40) return
      an = n
      an = 1.0/(an*an)
      correc = (c1(i) + an*(c2(i) + an*c3(i)))*mic
      return
      end
C--- alnfac() is NOT called from anything above! --- ?? ---
c
c
      double precision function alnfac(j)
c
c        algorithm as 177.2  appl. statist. (1982) vol.31, no.2
c
c        natural logarithm of factorial for non-negative agrument
c
      implicit logical (a-z)
      integer j
      double precision r(7), one, half, a0, three, four, fourtn, fortty,
     *  fivfty, w, z
      data r(1), r(2), r(3), r(4), r(5), r(6), r(7) /0.0d0, 0.0d0,
     *  0.69314718056d0, 1.79175946923d0, 3.17805383035d0,
     *  4.78749174278d0, 6.57925121101d0/
      data one, half, a0, three, four, fourtn, fortty, fivfty /
     *  1.0d0, 0.5d0, 0.918938533205d0, 3.0d0, 4.0d0, 14.0d0, 420.0d0,
     *  5040.0d0/
      if (j .ge. 0) goto 10
      alnfac = one
      return
   10 if (j .ge. 7) goto 20
      alnfac = r(j + 1)
      return
   20 w = j + 1
      z = one / (w * w)
      alnfac = (w - half) * log(w) - w + a0 + (((four - three * z)
     *  * z - fourtn) * z + fortty) / (fivfty * w)
      return
      end
