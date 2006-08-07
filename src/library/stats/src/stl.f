c     
c     from netlib/a/stl: no authorship nor copyright claim in the source;
c     presumably by the authors of 
c     
c     R.B. Cleveland, W.S.Cleveland, J.E. McRae, and I. Terpenning,
c     STL: A Seasonal-Trend Decomposition Procedure Based on Loess, 
c     Statistics Research Report, AT&T Bell Laboratories.
c     
c     Converted to double precision by B.D. Ripley 1999.
c     Indented, goto labels renamed, many goto's replaced by `if then {else}'
c     (using Emacs), many more comments;  by M.Maechler 2001-02.
c     
      subroutine stl(y,n,np,ns,nt,nl, isdeg,itdeg,ildeg, 
     &     nsjump,ntjump,nljump, ni,no, rw,season,trend,work)

      implicit none
c Arg
      integer n, np, ns,nt,nl, isdeg,itdeg,ildeg, nsjump,ntjump,nljump,
     &     ni, no
c	n                   : length(y)
c	ns, nt, nl          : spans        for `s', `t' and `l' smoother
c	isdeg, itdeg, ildeg : local degree for `s', `t' and `l' smoother
c	nsjump,ntjump,nljump: ........     for `s', `t' and `l' smoother
c       ni, no              : number of inner and outer (robust) iterations

      double precision y(n), rw(n), season(n), trend(n), 
     &     work(n+2*np,5)
c Var
      integer i,k, newns, newnt, newnl, newnp
      logical userw

      userw = .false.
      do 1 i = 1,n
         trend(i) = 0.
 1    continue
c the three spans must be at least three and odd:
      newns = max0(3,ns)
      newnt = max0(3,nt)
      newnl = max0(3,nl)
      if(mod(newns,2) .eq. 0) newns = newns + 1
      if(mod(newnt,2) .eq. 0) newnt = newnt + 1
      if(mod(newnl,2) .eq. 0) newnl = newnl + 1
c periodicity at least 2:
      newnp = max0(2,np)

      k = 0
c --- outer loop -- robustnes iterations
 100  continue
      call stlstp(y,n, newnp,newns,newnt,newnl, isdeg,itdeg,ildeg,
     &     nsjump,ntjump,nljump, ni,userw,rw,season, trend, work)
      k = k+1
      if(k .gt. no) goto 10

      do 3 i = 1,n
         work(i,1) = trend(i)+season(i)
 3    continue
      call stlrwt(y,n,work(1,1),rw)
      userw = .true.
      goto 100
c --- end Loop
 10   continue

c     robustness weights when there were no robustness iterations:
      if(no .le. 0) then
         do 15 i = 1,n
            rw(i) = 1.
 15      continue
      endif
      return
      end
      
      subroutine stless(y,n,len,ideg,njump,userw,rw,ys,res)

      implicit none
c Arg
      integer n, len, ideg, njump
      double precision y(n), rw(n), ys(n), res(n)
c Var
      integer newnj, nleft, nright, nsh, k, i, j
      double precision delta
      logical ok, userw

      if(n .lt. 2) then
         ys(1) = y(1)
         return
      endif 

      newnj = min0(njump, n-1)
      if(len .ge. n) then
         nleft = 1
         nright = n
         do 20 i = 1,n,newnj 
            call stlest(y,n,len,ideg,dble(i),ys(i),nleft,nright,res,
     &           userw,rw,ok)
            if(.not. ok) ys(i) = y(i)
 20      continue

      else

         if(newnj .eq. 1) then
            nsh = (len+1)/2
            nleft = 1
            nright = len
            do 30 i = 1,n 
               if(i .gt. nsh  .and.  nright .ne. n) then
                  nleft = nleft+1
                  nright = nright+1
               endif
               call stlest(y,n,len,ideg,dble(i),ys(i),nleft,nright,res,
     &              userw,rw,ok)
               if(.not. ok) ys(i) = y(i)
 30         continue
         else
            nsh = (len+1)/2
            do 40 i = 1,n,newnj 
               if(i .lt. nsh) then
                  nleft = 1
                  nright = len
               else if(i .ge. n-nsh+1) then
                  nleft = n-len+1
                  nright = n
               else
                  nleft = i-nsh+1
                  nright = len+i-nsh
               endif

               call stlest(y,n,len,ideg,dble(i),ys(i),nleft,nright,res,
     &              userw,rw,ok)
               if(.not. ok) ys(i) = y(i)
 40         continue

         endif

      endif

      if(newnj .ne. 1) then
         do 45 i = 1,n-newnj,newnj 
            delta = (ys(i+newnj)-ys(i))/dble(newnj)
            do 47 j = i+1,i+newnj-1
               ys(j) = ys(i)+delta*dble(j-i)
 47         continue
 45      continue
         k = ((n-1)/newnj)*newnj+1

         if(k .ne. n) then
            call stlest(y,n,len,ideg,dble(n),ys(n),nleft,nright,res,
     &           userw,rw,ok)
            if(.not. ok) ys(n) = y(n)

            if(k .ne. n-1) then
               delta = (ys(n)-ys(k))/dble(n-k)
               do 55 j = k+1,n-1
                  ys(j) = ys(k)+delta*dble(j-k)
 55            continue
            endif
         endif
      endif
      return
      end

      subroutine stlest(y,n,len,ideg,xs,ys,nleft,nright,w,
     &     userw,rw,ok)

      implicit none
c Arg
      integer n, len, ideg, nleft, nright
      double precision y(n), w(n), rw(n), xs, ys
      logical userw,ok
c Var
      double precision range, h, h1, h9, a, b, c, r
      integer j

      range = dble(n)-dble(1)
      h = max(xs - dble(nleft), dble(nright) - xs)
      if(len .gt. n) h = h + dble((len-n)/2)
      h9 = .999*h
      h1 = .001*h
      a = 0.
      do 60 j = nleft,nright 
         r = abs(dble(j)-xs)
         if(r .le. h9) then
            if(r .le. h1) then
               w(j) = 1.
            else
               w(j) = (1. - (r/h)**3)**3
            endif
            if(userw) w(j) = rw(j)*w(j)
            a = a+w(j)
         else
            w(j) = 0.
         endif
 60   continue

      if(a .le. 0.) then
         ok = .false.
      else
         ok = .true.
         do 69 j = nleft,nright
            w(j) = w(j)/a
 69      continue
         if((h .gt. 0.) .and. (ideg .gt. 0)) then
            a = 0.
            do 73 j = nleft,nright
               a = a+w(j)*dble(j)
 73         continue
            b = xs-a
            c = 0.
            do 75 j = nleft,nright
               c = c+w(j)*(dble(j)-a)**2
 75         continue
            if(sqrt(c) .gt. .001*range) then
               b = b/c
               do 79 j = nleft,nright
                  w(j) = w(j)*(b*(dble(j)-a)+1.0)
 79            continue
            endif
         endif
         ys = 0.
         do 81 j = nleft,nright
            ys = ys+w(j)*y(j)
 81      continue
      endif

      return
      end

      subroutine stlfts(x,n,np,trend,work)
      integer n, np
      double precision x(n), trend(n), work(n)

      call stlma(x,    n,      np, trend)
      call stlma(trend,n-np+1, np, work)
      call stlma(work, n-2*np+2,3, trend)
      return
      end


      subroutine stlma(x, n, len, ave)

c Moving Average (aka "running mean")
c ave(i) := mean(x{j}, j = max(1,i-k),..., min(n, i+k))
c           for i = 1,2,..,n

      implicit none
c Arg
      integer n, len
      double precision x(n), ave(n)
c Var
      double precision flen, v
      integer i, j, k, m, newn
      newn = n-len+1
      flen = dble(len)
      v = 0.
      do 3 i = 1,len
         v = v+x(i)
 3    continue
      ave(1) = v/flen
      if(newn .gt. 1) then
         k = len
         m = 0
         do 7 j = 2, newn 
            k = k+1
            m = m+1
            v = v-x(m)+x(k)
            ave(j) = v/flen
 7       continue
      endif
      return
      end


      subroutine stlstp(y,n,np,ns,nt,nl,isdeg,itdeg,ildeg,nsjump,
     &     ntjump,nljump,ni,userw,rw,season,trend,work)

      implicit none
c Arg
      integer n,np,ns,nt,nl,isdeg,itdeg,ildeg,nsjump,ntjump,nljump,ni
      logical userw
      double precision y(n),rw(n),season(n),trend(n),work(n+2*np,5)
c Var
      integer i,j

      do 80 j = 1,ni 
         do 1 i = 1,n
            work(i,1) = y(i)-trend(i)
 1       continue
         call stlss(work(1,1),n,np,ns,isdeg,nsjump,userw,rw,work(1,2),
     &        work(1,3),work(1,4),work(1,5),season)
         call stlfts(work(1,2),n+2*np,np,work(1,3),work(1,1))
         call stless(work(1,3),n,nl,ildeg,nljump,.false.,work(1,4),
     &        work(1,1),work(1,5))
         do 3 i = 1,n
            season(i) = work(np+i,2)-work(i,1)
 3       continue
         do 5 i = 1,n
            work(i,1) = y(i)-season(i)
 5       continue
         call stless(work(1,1),n,nt,itdeg,ntjump,userw,rw,trend,
     &        work(1,3))
 80   continue
      return
      end

      subroutine stlrwt(y,n,fit,rw)
c Robustness Weights
c	rw_i := B( |y_i - fit_i| / (6 M) ),   i = 1,2,...,n
c		where B(u) = (1 - u^2)^2  * 1[|u| < 1]   {Tukey's biweight}
c		and   M := median{ |y_i - fit_i| }
      implicit none
c Arg
      integer n
      double precision y(n), fit(n), rw(n)
c Var
      integer mid(2), i
      double precision cmad, c9, c1, r

      do 7 i = 1,n
         rw(i) = abs(y(i)-fit(i))
 7    continue
      mid(1) = n/2+1
      mid(2) = n-mid(1)+1
      call psort(rw,n,mid,2)
      cmad = 3.0*(rw(mid(1))+rw(mid(2))) 
c     = 6 * MAD      
      c9 = .999*cmad
      c1 = .001*cmad
      do 10 i = 1,n 
         r = abs(y(i)-fit(i))
         if(r .le. c1) then
            rw(i) = 1.
         else if(r .le. c9) then
            rw(i) = (1. - (r/cmad)**2)**2
         else
            rw(i) = 0.
         endif
 10   continue
      return
      end

      subroutine stlss(y,n,np,ns,isdeg,nsjump,userw,rw,season,
     &     work1,work2,work3,work4)
c
c	called by stlstp() at the beginning of each (inner) iteration
c
      implicit none
c Arg
      integer n, np, ns, isdeg, nsjump
      double precision y(n), rw(n), season(n+2*np), 
     &     work1(n), work2(n), work3(n), work4(n)
      logical userw
c Var
      integer nright, nleft, i, j, k, m
      logical ok
      double precision xs

      if(np .lt. 1) return

      do 200 j = 1, np
         k = (n-j)/np+1
         do 10 i = 1,k
            work1(i) = y((i-1)*np+j)
 10      continue
         if(userw) then
            do 12 i = 1,k
               work3(i) = rw((i-1)*np+j)
 12         continue
         endif
         call stless(work1,k,ns,isdeg,nsjump,userw,work3,work2(2),work4)
         xs = 0
         nright = min0(ns,k)
         call stlest(work1,k,ns,isdeg,xs,work2(1),1,nright,work4,
     &        userw,work3,ok)
         if(.not. ok) work2(1) = work2(2)
         xs = k+1
         nleft = max0(1,k-ns+1)
         call stlest(work1,k,ns,isdeg,xs,work2(k+2),nleft,k,work4,
     &        userw,work3,ok)
         if(.not. ok) work2(k+2) = work2(k+1)
         do 18 m = 1,k+2
            season((m-1)*np+j) = work2(m)
 18      continue

 200  continue

      return
      end


c  STL E_Z_ : "Easy" user interface  -- not called from R

      subroutine stlez(y, n, np, ns, isdeg, itdeg, robust, no, rw, 
     &     season, trend, work)

      implicit none
c Arg
      integer n, np, ns, isdeg, itdeg, no
      logical robust
      double precision y(n), rw(n), season(n), trend(n), work(n+2*np,7)
c Var
      integer i, j, ildeg, nt, nl, ni, nsjump, ntjump, nljump, 
     &     newns, newnp
      double precision maxs, mins, maxt, mint, maxds, maxdt, difs, dift

      ildeg = itdeg
      newns = max0(3,ns)
      if(mod(newns,2) .eq. 0) newns = newns+1
      newnp = max0(2,np)
      nt = (1.5*newnp)/(1 - 1.5/newns) + 0.5
      nt = max0(3,nt)
      if(mod(nt,2) .eq. 0) nt = nt+1
      nl = newnp
      if(mod(nl,2) .eq. 0) nl = nl+1

      if(robust) then
         ni = 1
      else
         ni = 2
      endif

      nsjump = max0(1,int(float(newns)/10 + 0.9))
      ntjump = max0(1,int(float(nt)/10 + 0.9))
      nljump = max0(1,int(float(nl)/10 + 0.9))
      do 2 i = 1,n
         trend(i) = 0.
 2    continue
      call stlstp(y,n,newnp,newns,nt,nl,isdeg,itdeg,ildeg,nsjump,
     &     ntjump,nljump,ni,.false.,rw,season,trend,work)

      no = 0
      if(robust) then
         j=1
C        Loop  --- 15 robustness iterations
 100     if(j .le. 15) then
            do 35 i = 1,n
               work(i,6) = season(i)
               work(i,7) = trend(i)
               work(i,1) = trend(i)+season(i)
 35        continue
            call stlrwt(y,n,work(1,1),rw)
            call stlstp(y, n, newnp, newns, nt,nl, isdeg,itdeg,ildeg, 
     &           nsjump,ntjump,nljump, ni, .true., 
     &           rw, season, trend, work)
            no = no+1
            maxs = work(1,6)
            mins = work(1,6)
            maxt = work(1,7)
            mint = work(1,7)
            maxds = abs(work(1,6) - season(1))
            maxdt = abs(work(1,7) - trend(1))
            do   137 i = 2,n
               if(maxs .lt. work(i,6)) maxs = work(i,6)
               if(maxt .lt. work(i,7)) maxt = work(i,7)
               if(mins .gt. work(i,6)) mins = work(i,6)
               if(mint .gt. work(i,7)) mint = work(i,7)
               difs = abs(work(i,6) - season(i))
               dift = abs(work(i,7) - trend(i))
               if(maxds .lt. difs) maxds = difs
               if(maxdt .lt. dift) maxdt = dift
 137        continue
            if((maxds/(maxs-mins) .lt. .01) .and.
     &         (maxdt/(maxt-mint) .lt. .01))   goto 300
            continue
            j=j+1
            goto 100
         endif
C        end Loop
 300     continue

      else 
c     	.not. robust

         do 150 i = 1,n
            rw(i) = 1.0
 150     continue
      endif

      return
      end

      subroutine psort(a,n,ind,ni)
c
c Partial Sorting ; used for Median (MAD) computation only
c
      implicit none
c Arg
      integer n,ni
      double precision a(n)
      integer ind(ni)
c Var
      integer indu(16),indl(16),iu(16),il(16),p,jl,ju,i,j,m,k,ij,l
      double precision t,tt

      if(n .lt. 0 .or. ni .lt. 0) return

      if(n .lt. 2 .or. ni .eq. 0) return

      jl = 1
      ju = ni
      indl(1) = 1
      indu(1) = ni
      i = 1
      j = n
      m = 1

c Outer Loop
 161  continue
      if(i .lt. j) go to 10
      
c  _Loop_
 166  continue
      m = m-1
      if(m .eq. 0) return
      i = il(m)
      j = iu(m)
      jl = indl(m)
      ju = indu(m)
      if(.not.(jl .le. ju))  goto 166

c     while (j - i > 10) 
 173  if(.not.(j-i .gt. 10)) goto 174

 10   k = i
      ij = (i+j)/2
      t = a(ij)
      if(a(i) .gt. t) then
         a(ij) = a(i)
         a(i) = t
         t = a(ij)
      endif
      l = j
      if(a(j) .lt. t) then
         a(ij) = a(j)
         a(j) = t
         t = a(ij)
         if(a(i) .gt. t) then
            a(ij) = a(i)
            a(i) = t
            t = a(ij)
         endif
      endif

 181  continue
      l = l-1
      if(a(l) .le. t)then
         tt = a(l)
 186     continue
         k = k+1
         if(.not.(a(k) .ge. t)) goto 186

         if(k .gt. l) goto 183

         a(l) = a(k)
         a(k) = tt
      endif
      goto   181

 183  continue
      indl(m) = jl
      indu(m) = ju
      p = m
      m = m+1
      if(l-i .le. j-k) then
         il(p) = k
         iu(p) = j
         j = l

 193     continue
         if(jl .gt. ju) goto 166
         if(ind(ju) .gt. j) then
            ju = ju-1
            goto 193
         endif
         indl(p) = ju+1
      else
         il(p) = i
         iu(p) = l
         i = k

 200     continue
         if(jl .gt. ju) goto 166
         if(ind(jl) .lt. i) then
            jl = jl+1
            goto 200
         endif
         indu(p) = jl-1
      endif

      goto   173
c     end while
 174  continue

      if(i .ne. 1) then
         i = i-1
 209     continue
         i = i+1
         if(i .eq. j) goto 166
         t = a(i+1)
         if(a(i) .gt. t) then
            k = i
c           repeat
 216        continue
            a(k+1) = a(k)
            k = k-1
            if(.not.(t .ge. a(k))) goto 216
c           until  t >= a(k)
            a(k+1) = t
         endif
         goto 209

      endif

      goto 161
c End Outer Loop

      end
