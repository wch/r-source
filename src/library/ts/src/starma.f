c Code in this file based on Applied Statistics algorithms 
c (C) Royal Statistical Society 1980, 1982
c
c  applied statistics algorithm as154
c
c  start of as 154
      subroutine starma(ip, iq, ir, np, phi, theta, a, p, v, thetab,
     $  xnext, xrow, rbar, nrbar, ifault)
c      implicit none
      integer ip,iq,ir,np,nrbar,ifault,i,ind,j,k,irank,ifail,ind1,
     $  npr,npr1,ind2,indi,indj,indn
      double precision phi(ir), theta(ir), a(ir), p(np), v(np), 
     $  thetab(np),
     $  xnext(np), xrow(np), rbar(nrbar), vj, phii, phij, ssqerr,
     $  recres, ynext, zero, one
c
      data zero, one /0.0d0, 1.0d0/
c
c        algorithm as 154  appl. statist. (1980) vol.29, p.311
c
c        invoking this subroutine sets the values of v and phi, and
c        obtains the initial values of a and p.
c        this routine is not suitable for use with an ar(1) process.
c        in this case the following instructions should be used for
c        initialisation.
c
c     check if ar(1)
c
      if (iq .gt. 0 .or. ip .gt. 1) go to 5
      v(1) = 1.0
      a(1) = 0.0
      p(1) = 1.0/(1.0-phi(1)*phi(1))
      return
c
c        check for failure indication.
c
 5    ifault = 0
      if (ip .lt. 0) ifault = 1
      if (iq .lt. 0) ifault = ifault + 2
      if (ip .eq. 0 .and. iq .eq. 0) ifault = 4
      k = iq + 1
      if (k .lt. ip) k = ip
      if (ir .ne. k) ifault = 5
      if (np .ne. ir * (ir + 1) / 2) ifault = 6
      if (nrbar .ne. np * (np - 1) / 2) ifault = 7
      if (ir .eq. 1) ifault = 8
      if (ifault .ne. 0) return
c
c        now set a(0), v and phi.
c
      do 10 i = 2, ir
      a(i) = zero
      if (i .gt. ip) phi(i) = zero
      v(i) = zero
      if (i .le. iq + 1) v(i) = theta(i - 1)
   10 continue
      a(1) = zero
      if (ip .eq. 0) phi(1) = zero
      v(1) = one
      ind = ir
      do 20 j = 2, ir
      vj = v(j)
      do 20 i = j, ir
      ind = ind + 1
      v(ind) = v(i) * vj
   20 continue
c
c        now find p(0).
c
      if (ip .eq. 0) goto 300
c
c        the set of equations s * vec(p(0)) = vec(v)
c        is solved for vec(p(0)).
c        s is generated row by row in the array xnext.
c        the order of elements in p is changed, so as to
c        bring more leading zeros into the rows of s,
c        hence achieving a reduction of computing time.
c
      irank = 0
      ssqerr = zero
      do 40 i = 1, nrbar
   40 rbar(i) = zero
      do 50 i = 1, np
      p(i) = zero
      thetab(i) = zero
      xnext(i) = zero
   50 continue
      ind = 0
      ind1 = 0
      npr = np - ir
      npr1 = npr + 1
      indj = npr1
      ind2 = npr
      do 110 j = 1, ir
      phij = phi(j)
      xnext(indj) = zero
      indj = indj + 1
      indi = npr1 + j
      do 110 i = j, ir
      ind = ind + 1
      ynext = v(ind)
      phii = phi(i)
      if (j .eq. ir) goto 100
      xnext(indj) = -phii
      if (i .eq. ir) goto 100
      xnext(indi) = xnext(indi) - phij
      ind1 = ind1 + 1
      xnext(ind1) = -one
  100 xnext(npr1) = -phii * phij
      ind2 = ind2 + 1
      if (ind2 .gt. np) ind2 = 1
      xnext(ind2) = xnext(ind2) + one
      call inclu2(np, nrbar, one, xnext, xrow, ynext,
     $  p, rbar, thetab, ssqerr, recres, irank, ifail)
c
c        no need to check ifail as weight = 1.0
c
      xnext(ind2) = zero
      if (i .eq. ir) goto 110
      xnext(indi) = zero
      indi = indi + 1
      xnext(ind1) = zero
  110 continue
      call regres(np, nrbar, rbar, thetab, p)
c
c        now re-order p.
c
      ind = npr
      do 200 i = 1, ir
      ind = ind + 1
      xnext(i) = p(ind)
  200 continue
      ind = np
      ind1 = npr
      do 210 i = 1, npr
      p(ind) = p(ind1)
      ind = ind - 1
      ind1 = ind1 - 1
  210 continue
      do 220 i = 1, ir
  220 p(i) = xnext(i)
      return
c
c        p(0) is obtained by backsubstitution for
c        a moving average process.
c
  300 indn = np + 1
      ind = np + 1
      do 310 i = 1, ir
      do 310 j = 1, i
      ind = ind - 1
      p(ind) = v(ind)
      if (j .eq. 1) goto 310
      indn = indn - 1
      p(ind) = p(ind) + p(indn)
  310 continue
      return
      end
c
      subroutine karma(ip, iq, ir, np, phi, theta, a, p,
     $  v, n, w, resid, sumlog, ssq, iupd, delta, e, nit)
c      implicit none
c
c        algorithm as 154.1  appl. statist. (1980) vol.29, p.311
c
c        invoking this subroutine updates a, p, sumlog and ssq by
c        inclusion of data values w(1) to w(n). the corresponding
c        values of resid are also obtained.
c        when ft is less than (1 + delta), quick recursions are used.
c
      integer ip,iq,ir,np,n,iupd,nit,ir1,i,inde,j,ind,indn,l,ii,indw
      double precision phi(ir), theta(ir), a(ir), p(np), v(np), 
     $  w(n), resid(n),
     $  e(ir), sumlog, ssq, delta, wnext, a1, dt, et, ft, ut, g,
     $  zero
c
      data zero /0.0d0/
c
c
      ir1 = ir - 1
      do 10 i = 1, ir
   10 e(i) = zero
      inde = 1
c
c        for non-zero values of nit, perform quick recursions.
c
      if (nit .ne. 0) goto 600
      do 500 i = 1, n
      wnext = w(i)
c
c        prediction.
c
      if (iupd .eq. 1 .and. i .eq. 1) goto 300
c
c        here dt = ft - 1.0
c
      dt = zero
      if (ir .ne. 1) dt = p(ir + 1)
      if (dt .lt. delta) goto 610
      a1 = a(1)
      if (ir .eq. 1) goto 110
      do 100 j = 1, ir1
  100 a(j) = a(j + 1)
  110 a(ir) = zero
      if (ip .eq. 0) goto 200
      do 120 j = 1, ip
  120 a(j) = a(j) + phi(j) * a1
  200 ind = 0
      indn = ir
      do 210 l = 1, ir
      do 210 j = l, ir
      ind = ind + 1
      p(ind) = v(ind)
      if (j .eq. ir) goto 210
      indn = indn + 1
      p(ind) = p(ind) + p(indn)
  210 continue
c
c        updating.
c
  300 ft = p(1)
      ut = wnext - a(1)
      if (ir .eq. 1) goto 410
      ind = ir
      do 400 j = 2, ir
      g = p(j) / ft
      a(j) = a(j) + g * ut
      do 400 l = j, ir
      ind = ind + 1
      p(ind) = p(ind) - g * p(l)
  400 continue
  410 a(1) = wnext
      do 420 l = 1, ir
  420 p(l) = zero
      resid(i) = ut / sqrt(ft)
      e(inde) = resid(i)
      inde = inde + 1
      if (inde .gt. iq) inde = 1
      ssq = ssq + ut * ut / ft
      sumlog = sumlog + log(ft)
  500 continue
      nit = n
      return
c
c        quick recursions
c
  600 i = 1
  610 nit = i - 1
      do 650 ii = i, n
      et = w(ii)
      indw = ii
      if (ip .eq. 0) goto 630
      do 620 j = 1, ip
      indw = indw - 1
      if (indw .lt. 1) goto 630
      et = et - phi(j) * w(indw)
  620 continue
  630 if (iq .eq. 0) goto 645
      do 640 j = 1, iq
      inde = inde - 1
      if (inde .eq. 0) inde = iq
      et = et - theta(j) * e(inde)
  640 continue
  645 e(inde) = et
      resid(ii) = et
      ssq = ssq + et * et
      inde = inde + 1
      if (inde .gt. iq) inde = 1
  650 continue
      return
      end
c
      subroutine kalfor(m, ip, ir, np, phi, a, p, v, work, x, var)
c
c        algorithm as 154.2  appl. statist. (1980) vol.29, p.311
c
c        invoking this subroutine obtains predictions
c        of a and p, m steps ahead.
c
c      implicit none
      integer m, ip, ir, np, ir1, l, i, j, ind, ind1
      double precision phi(ir), a(ir), p(np), v(np), work(ir), dt,
     $  a1, phii, phij, phijdt, zero, x(m), var(m)
c
      data zero /0.0d0/
c
      ir1 = ir - 1
      do 300 l = 1, m
c
c        predict a.
c
      a1 = a(1)
      if (ir .eq. 1) goto 110
      do 100 i = 1, ir1
  100 a(i) = a(i + 1)
  110 a(ir) = zero
      if (ip .eq. 0) goto 200
      do 120 j = 1, ip
  120 a(j) = a(j) + phi(j) * a1
c
c        predict p.
c
  200 do 210 i = 1, ir
  210 work(i) = p(i)
      ind = 0
      ind1 = ir
      dt = p(1)
      do 220 j = 1, ir
      phij = phi(j)
      phijdt = phij * dt
      do 220 i = j, ir
      ind = ind + 1
      phii = phi(i)
      p(ind) = v(ind) + phii * phijdt
      if (j .lt. ir) p(ind) = p(ind) + work(j + 1) * phii
      if (i .eq. ir) goto 220
      ind1 = ind1 + 1
      p(ind) = p(ind) + work(i + 1) * phij + p(ind1)
  220 continue
c modifications here
      x(l) = a(1)
      var(l) = p(1)
  300 continue
      return
      end
c
      subroutine inclu2(np, nrbar, weight, xnext, xrow, ynext, d, rbar,
     $  thetab, ssqerr, recres, irank, ifault)
c      implicit none
c
c        algorithm as 154.3  appl. statist. (1980) vol.29, p.311
c
c        fortran version of revised version of algorithm as 75.1
c        appl. statist. (1974) vol.23, p.448
c        see remark as r17 appl. statist. (1976) vol.25, p.323
c
      integer np,nrbar,irank,ifault,i,ithisr,i1,k
      double precision xnext(np), xrow(np), d(np), rbar(nrbar),
     $  thetab(np),
     $  weight, ynext, ssqerr, recres, wt, y, di, dpi, xi, xk,
     $  cbar, sbar, rbthis, zero
c
      data zero /0.0d0/
c
c        invoking this subroutine updates d, rbar, thetab, ssqerr
c        and irank by the inclusion of xnext and ynext with a
c        specified weight. the values of xnext, ynext and weight will
c        be conserved. the corresponding value of recres is calculated.
c
      y = ynext
      wt = weight
      do 10 i = 1, np
   10 xrow(i) = xnext(i)
      recres = zero
      ifault = 1
      if (wt .le. zero) return
      ifault = 0
c
      ithisr = 0
      do 50 i = 1, np
      if (xrow(i) .ne. zero) goto 20
      ithisr = ithisr + np - i
      goto 50
   20 xi = xrow(i)
      di = d(i)
      dpi = di + wt * xi * xi
      d(i) = dpi
      cbar = di / dpi
      sbar = wt * xi / dpi
      wt = cbar * wt
      if (i .eq. np) goto 40
      i1 = i + 1
      do 30 k = i1, np
      ithisr = ithisr + 1
      xk = xrow(k)
      rbthis = rbar(ithisr)
      xrow(k) = xk - xi * rbthis
      rbar(ithisr) = cbar * rbthis + sbar * xk
   30 continue
   40 xk = y
      y = xk - xi * thetab(i)
      thetab(i) = cbar * thetab(i) + sbar * xk
      if (di .eq. zero) goto 100
   50 continue
      ssqerr = ssqerr + wt * y * y
      recres = y * sqrt(wt)
      return
  100 irank = irank + 1
      return
      end
c
      subroutine regres(np, nrbar, rbar, thetab, beta)
c
c        algorithm as 154.4  appl. statist. (1980) vol.29, p.311
c
c        revised version of algorithm as 75.4
c        appl. statist. (1974) vol.23, p.448
c        invoking this subroutine obtains beta by backsubstitution
c        in the triangular system rbar and thetab.
c
c      implicit none
      integer np,nrbar,ithisr,im,i1,jm,i,j
      double precision rbar(nrbar), thetab(np), beta(np), bi
      ithisr = nrbar
      im = np
      do 50 i = 1, np
      bi = thetab(im)
      if (im .eq. np) goto 30
      i1 = i - 1
      jm = np
      do 10 j = 1, i1
      bi = bi - rbar(ithisr) * beta(jm)
      ithisr = ithisr - 1
      jm = jm - 1
   10 continue
   30 beta(im) = bi
      im = im - 1
   50 continue
      return
      end
c  end of as 154

c  applied statistics algorithm as182
c
c  start of as 182
      subroutine forkal(ip, iq, ir, np, ird, irz, id, il, n, nrbar,
     *   phi, theta, delta, w, y, amse, a, p, v, resid, e, xnext, xrow,
     *   rbar, thetab, store, ifault)
c
c     algorithm as 182  appl. statist. (1982) vol.31, no.2
c
c     finite sample prediction from arima processes.
c
c     auxiliary routines required: karma & starma from as 154 and
c     routines called by them: inclu2 from asr 17 (a slight variant on
c     as 75, and regres from as 75.
c
      integer ip, iq, ir, np, ird, irz, id, il, n, nrbar, ifault,
     * k, i, j,ll, nt, nj, idk, iid, iupd, nit, ind, ir1, id1,
     * id2r, id2r1, idd1, idd2, i45, idrr1, iddr, jkl, jkl1, id2r2,
     * ibc, l, iri1, jj, lk, lk1, jklj, iri, kk1,k1, kk, kkk, ind1,
     * ind2, jrj, j1, jrk, irj, ir2
      double precision aa, sumlog, ssq, del, sigma, dt, phij, phijdt,
     * phii, a1, ams
      double precision phi(ir), theta(ir), delta(id), w(n), y(il), 
     *   amse(il),
     *   a(ird), p(irz), v(np), resid(n), e(ir), xnext(np), xrow(np),
     *   rbar(nrbar), thetab(np), store(ird)
      double precision zero, one, two
      data zero/0.0d0/, one/1.0d0/, two/2.0d0/
c
c     invoking this routine will calculate the finite sample predictions
c     and their conditional mean square errors for any arima process.
c
c     check for input faults.
c
      ifault = 0
      if (ip .lt. 0) ifault = 1
      if (iq .lt. 0) ifault = ifault + 2
      if (ip * ip + iq * iq .eq. 0) ifault = 4
      k = iq + 1
      if (k .lt. ip) k = ip
      if (ir .ne. k) ifault = 5
      if (np .ne. ir * (ir + 1) / 2) ifault = 6
      if (nrbar .lt. np * (np - 1) / 2) ifault = 7
      if (id .lt. 0) ifault = 8
      if (ird .ne. ir + id) ifault = 9
      if (irz .ne. ird * (ird + 1) / 2) ifault = 10
      if (il .lt. 1) ifault = 11
      if (ifault .ne. 0) return
c
c     calculate initial conditions for kalman filter
c
      a(1) = zero
      v(1) = one
c This is all pointless: starma will revise v
c It over-runs v, too.
c      if (np .eq. 1) go to 130
c      do 100 i = 2, np
c  100 v(i) = zero
c      if (iq .eq. 0) go to 130
c      iq1 = iq + 1
c      do 110 i = 2, iq1
c  110 v(i) = theta(i-1)
c      do 120 j = 1, iq
c       ll = j * (2*ir + 1 - j) / 2
c       do 120 i = j, iq
c         lli = ll + i
c         v(lli) = theta(i) * theta(j)
c  120 continue
c
c     find initial likelihood conditions.
c     ifault not tested on exit from starma as all possible errors
c     have been checked above.
c
  130 if (ir .eq. 1) p(1) = one / (one - phi(1) * phi(1))
      if (ir .ne. 1) call starma(ip, iq, ir, np, phi, theta, a, p, v,
     *  thetab, xnext, xrow, rbar, nrbar, ifault)
c
c     calculate data transformations
c
      nt = n - id
      if (id .eq. 0) go to 170
      do 140 j = 1, id
        nj = n - j
        store(j) = w(nj)
  140 continue
      do 160 i = 1, nt
        aa = zero
        do 150 k = 1, id
          idk = id + i - k
          aa = aa - delta(k) * w(idk)
  150   continue
        iid = i + id
        w(i) = w(iid) + aa
  160 continue
c
c     evaluate likelihood to obtain final kf conditions
c
  170 sumlog = zero
      ssq = zero
      iupd = 1
      del = - one
      nit = 0
      call karma(ip, iq, ir, np, phi, theta, a, p, v, nt, w, resid,
     *    sumlog, ssq, iupd, del, e, nit)
c
c     calculate m.l.e. of sigma squared
c
      sigma = zero
      do 200 j = 1, nt
  200 sigma = sigma + resid(j)**2
      sigma = sigma / nt
c
c     reset the initial a and p when differencing occurs
c
      if (id .eq. 0) go to 250
      do 210 i = 1, np
  210 xrow(i) = p(i)
      do 220 i = 1, irz
  220 p(i) = zero
      ind = 0
      do 230 j = 1, ir
        k = (j-1) * (id + ir + 1) - (j-1) * j / 2
        do 230 i = j, ir
          ind = ind + 1
          k = k + 1
          p(k) = xrow(ind)
  230 continue
      do 240 j = 1, id
        irj = ir + j
        a(irj) = store(j)
  240 continue
c
c     set up constants
c
  250 ir2 = ir + 1
      ir1 = ir - 1
      id1 = id - 1
      id2r = 2 * ird
      id2r1 = id2r - 1
      idd1 = 2 * id + 1
      idd2 = idd1 + 1
      i45 = id2r + 1
      idrr1 = ird + 1
      iddr = 2 * id + ir
      jkl = ir * (iddr + 1) / 2
      jkl1 = jkl + 1
      id2r2 = id2r + 2
      ibc = ir * (i45 - ir) / 2
      do 560 l = 1, il
c
c     predict a
c
        a1 = a(1)
        if (ir .eq. 1) go to 310
        do 300 i = 1, ir1
  300   a(i) = a(i+1)
  310   a(ir) = zero
        if (ip .eq. 0) go to 330
        do 320 j = 1, ip
  320   a(j) = a(j) + phi(j) * a1
c original has label 360 and overruns a
  330   if (id .eq. 0) go to 361
        do 340 j = 1, id
          irj = ir + j
          a1 = a1 + delta(j) * a(irj)
  340   continue
        if (id .lt. 2) go to 360
        do 350 i = 1, id1
          iri1 = ird - i
          a(iri1 + 1) = a(iri1)
  350   continue
  360   a(ir2) = a1
  361   continue
c     
c     predict p
c
        if (id .eq. 0) go to 480
        do 370 i = 1, id
          store(i) = zero
          do 370 j = 1, id
            ll = max(i,j)
            k = min(i,j)
            jj = jkl + (ll - k) + 1 + (k-1) * (idd2 - k) / 2
            store(i) = store(i) + delta(j) * p(jj)
  370   continue
        if (id .eq. 1) go to 400
        do 380 j = 1, id1
          jj = id - j
          lk = (jj-1) * (idd2 - jj) / 2 + jkl
          lk1 = jj * (idd1 - jj) / 2 + jkl
          do 380 i = 1, j
            lk = lk + 1
            lk1 = lk1 + 1
            p(lk1) = p(lk)
  380   continue
        do 390 j = 1, id1
          jklj = jkl1 + j
          irj = ir + j
          p(jklj) = store(j) + p(irj)
  390   continue
  400   p(jkl1) = p(1)
        do 410 i = 1, id
          iri = ir + i
          p(jkl1) = p(jkl1) + delta(i) * (store(i) + two * p(iri))
  410   continue
        do 420 i = 1, id
          iri = ir + i
          store(i) = p(iri)
  420   continue
        do 430 j = 1, ir
          kk1 = j * (id2r1 - j) / 2 + ir
          k1 = (j-1) * (id2r - j) / 2 + ir
          do 430 i = 1, id
            kk = kk1 + i
            k = k1 + i
            p(k) = phi(j) * store(i)
            if (j .ne. ir) p(k) = p(k) + p(kk)
  430   continue
c
        do 440 j = 1, ir
          store(j) = zero
          kkk = j * (i45 - j) / 2 - id
          do 440 i = 1, id
            kkk = kkk + 1
            store(j) = store(j) + delta(i) * p(kkk)
  440   continue
        if (id .eq. 1) go to 460
        do 450 j = 1, ir
          k = j * idrr1 - j * (j+1) / 2 + 1
          do 450 i = 1, id1
            k = k - 1
            p(k) = p(k-1)
  450   continue
  460   do 470 j = 1, ir
          k = (j-1) * (id2r - j) / 2 + ir + 1
          p(k) = store(j) + phi(j) * p(1)
          if (j .lt. ir) p(k) = p(k) + p(j+1)
  470   continue
  480   do 490 i = 1, ir
  490   store(i) = p(i)
c
        ind = 0
        dt = p(1)
        do 500 j = 1, ir
          phij = phi(j)
          phijdt = phij * dt
          ind2 = (j-1) * (id2r2 - j) / 2
          ind1 = j * (i45 - j) / 2
          do 500 i = j, ir
            ind = ind + 1
            ind2 = ind2 + 1
            phii = phi(i)
            p(ind2) = v(ind) + phii * phijdt
            if (j .lt. ir) p(ind2) = p(ind2) + store(j+1) * phii
            if (i .eq. ir) go to 500
            ind1 = ind1 + 1
            p(ind2) = p(ind2) + store(i+1) * phij + p(ind1)
  500   continue
c
c     predict y
c
        y(l) = a(1)
        if (id .eq. 0) go to 520
        do 510 j = 1, id
          irj = ir + j
          y(l) = y(l) + a(irj) * delta(j)
  510   continue
c
c     calculate m.s.e. of y
c
  520   ams = p(1)
        if (id .eq. 0) go to 550
        do 530 j = 1, id
          jrj = ibc + (j-1) * (idd2 - j) / 2
          irj = ir + j
          ams = ams + two * delta(j) * p(irj) + p(jrj+1) * delta(j)**2
  530   continue
        if (id .eq. 1) go to 550
        do 540 j = 1, id1
          j1 = j + 1
          jrk = ibc + 1 + (j-1) * (idd2 - j) / 2
          do 540 i = j1, id
            jrk = jrk + 1
            ams = ams + two * delta(i) * delta(j) * p(jrk)
  540   continue
  550   amse(l) = ams * sigma
  560 continue
c
      return
      end
c  end of as 182
