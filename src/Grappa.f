      subroutine mcwh(nv,nn,nvals,nc,cnbr,
     &      it,inbre,iel,le,nne,nbre,
     &      nbr,wt,next)

      integer first,cnbr
c     integer size,totsize
      dimension nvals(nv),nn(nv),nbr(nv,nv),wt(nv),
     &      next(nv),iel(nv),cnbr(nc),le(nv),nne(nv),
     &      nbre(nc)

      ip = 0
      do i = 1,nv
            do j = 1,nn(i)
                  ip = ip+1
                  nbr(j,i) = cnbr(ip)
            end do
            do j = nn(i)+1,nv
                  nbr(j,i) = 0
            end do
      end do

c.. init list of active vertices

      first = 1
      do i = 1,nv-1
            next(i) = i+1
      end do
      next(nv) = 0

c.. init weights

      do i = 1,nv
            wt(i) = 0.0
            do j = 1,nn(i)
                  wt(i) = wt(i)+log(real(nvals(nbr(j,i))))
            end do
      end do

c.. init iel

      do i = 1,nv
            iel(i) = 0
      end do

c.. init vertex and edge count

      nedge = 0
      do i = 1,nv
            nedge = nedge+nn(i)
      end do
      nedge = nedge/2
      nactive = nv
      it = 0
      inbre = 0

c     totsize = 0

c.. main loop

      do while(nedge.lt.(nactive*(nactive-1))/2)

      it = it+1

c     write(*,*) nactive,nedge
c     write(*,*) wt

c.. find min weight

      iprev = 0
      i = first
      imin = 0
      iminprev = 0
      wmin = 0.0
      do while(i.ne.0)
            if(imin.eq.0.or.wt(i).lt.wmin) then
                  imin = i
                  iminprev = iprev
                  wmin = wt(i)
            end if
            iprev = i
            i = next(i)
      end do

      if(iminprev.eq.0) then
            first = next(imin)
      else
            next(iminprev) = next(imin)
      end if

      i = imin

c     write(*,*) 'eliminate',i,' with nbrs:',(nbr(j,i),j=1,nn(i))

      nactive = nactive-1
      iel(i) = it
      le(it) = i
      nne(it) = nn(i)
c     size = nvals(i)
      do j = 1,nn(i)
            inbre = inbre+1
            nbre(inbre) = nbr(j,i)
c           size = size*nvals(nbr(j,i))
      end do
c     totsize = totsize+size

c.. remove i from nbr lists, and reduce weights

      do j = 1,nn(i)
            k = nbr(j,i)
            do l = 1,nn(k)
                  if(nbr(l,k).eq.i) go to 1
            end do
            return
c           stop 99
1            if(l.ne.nn(k)) nbr(l,k) = nbr(nn(k),k)
            nn(k) = nn(k)-1
            wt(k) = wt(k)-log(real(nvals(i)))
      end do
      nedge = nedge-nn(i)

c.. do we need to fill in?

      do j1 = 1,nn(i)
            k1 = nbr(j1,i)
            do j2 = 1,nn(i)
            if(j2.ne.j1) then
                  k2 = nbr(j2,i)
                  do l = 1,nn(k1)
                        if(nbr(l,k1).eq.k2) go to 2
                  end do
c                 write(*,*) 'fill in',k1,k2
                  nn(k1) = nn(k1)+1
                  nn(k2) = nn(k2)+1
                  nbr(nn(k1),k1) = k2
                  nbr(nn(k2),k2) = k1
                  wt(k1) = wt(k1)+log(real(nvals(k2)))
                  wt(k2) = wt(k2)+log(real(nvals(k1)))
                  nedge = nedge+1
2                  continue
            end if
            end do
      end do

c.. end main loop

      end do

c     write(*,*) 'left with',first,(nbr(j,first),j=1,nn(first))
      iel(first) = it+1
      do j = 1,nn(first)
            iel(nbr(j,first)) = it+1
      end do

c     write(*,*) 'total of clique sizes:',totsize

c.. add last clique on end of separator list

      nne(it+1) = nn(first)+1
      inbre = inbre+1
      nbre(inbre) = first
      do j = 1,nn(first)
            inbre = inbre+1
            nbre(inbre) = nbr(j,first)
      end do

      return

      end

c-----------------------------------------------------------

      subroutine mcs(nv,adj,repair,lab,label,stk,save,nlabnb,
     &      list,mxlist,ltop,ad,mxad,adtop)

      integer adj(nv,nv)
      integer label(nv),stk(nv),save(nv),nlabnb(nv)
      integer list(mxlist),ad(2,mxad),adtop,repair

1      do i = 1,nv
          adj(i,i) = 0
          label(i) = 0
          nlabnb(i) = 0
      end do

      il = 0
      iad = 0

c.. start at arbitary vertex, label it 1, and initialise

      next = 1

      label(next) = 1
      do j = 1,nv
          if(adj(j,next).eq.1) nlabnb(j) = 1
      end do

      isave = 1
      save(isave) = next

      do lab = 2,nv

c.. find unlabelled vertex with largest number of labelled neighbours

      mnln = 0
      next = 0
      nsing = 0
      do j = 1,nv
          if(label(j).eq.0) then
            if(nsing.eq.0) nsing = j
            if(nlabnb(j).gt.mnln) then
                mnln = nlabnb(j)
                next = j
            end if
          end if
      end do

c.. case where no unlabelled verices have any labelled neighbours

      if(next.eq.0) then
            next = nsing
      end if

c.. label the new vertex

      label(next) = lab

c.. scan over its neighbours, pushing labelled neighbours on a stack
c   and updating all neighbour's counts of labelled neighbours

      istk = 1
      do j = 1,nv
          if(adj(j,next).eq.1) then
            if(label(j).ne.0) then
                stk(istk) = j
                istk = istk+1
            end if
            nlabnb(j) = nlabnb(j)+1
          end if
      end do

c.. make sure those labelled neighbours are neighbours of each other
c   failure here detects non-decomposability - in which case exit

      if(istk.gt.2) then
          do is = 1,istk-2
          do js = is+1,istk-1
            if(adj(stk(is),stk(js)).eq.0) then
                  if(repair.eq.0) return
                  i1 = stk(is)
                  i2 = stk(js)
                  adj(i1,i2) = 1
                  adj(i2,i1) = 1
                  go to 1
            end if
          end do
          end do
      end if

c.. have we detected a clique?

      if(istk.eq.isave+1) then
          do is = 1,isave
            if(stk(is).ne.save(is)) go to 50
          end do
      else
          go to 50
      end if
      go to 51

c.. write clique and separator

50      continue
c     write(*,'("cliq",20i3)') (save(i),i=1,isave)
c     write(*,'("sep ",20i3)') (stk(i),i=1,istk-1)
      iad = iad+1
      if(iad.gt.mxad) return
      if(il+isave+istk-1.gt.mxlist) then
c           lab = -1
            return
      end if
      ad(1,iad) = il
      do i = 1,isave
            il = il+1
            list(il) = save(i)
      end do
      ad(2,iad) = il
      do i = 1,istk-1
            il = il+1
            list(il) = stk(i)
      end do
      

c.. save stack, with next inserted in order

51      nexta = next
      ip = istk
      if(istk.gt.1) then
          do is = istk-1,1,-1
            if(stk(is).gt.nexta) then
                save(ip) = stk(is)
                ip = ip-1
            else
                save(ip) = nexta
                nexta = 0
                save(ip-1) = stk(is)
                ip = ip-2
            end if
          end do
      end if
      if(nexta.ne.0) save(1) = nexta
      isave = istk

c.. end of labelling loop

      end do

c.. write final clique, and exit successfully

c     write(*,'("cliq",20i3)') (save(i),i=1,isave)
      iad = iad+1
      if(iad.gt.mxad) return
      if(il+isave.gt.mxlist) then
c           lab = -1
            return
      end if
      ad(1,iad) = il
      do i = 1,isave
            il = il+1
            list(il) = save(i)
      end do
      ltop = il
      adtop = iad

      lab = 0

      return

      end

c-----------------------------------------------------------

      subroutine trav(start,out,ncq,nlk,csl,sep,nxt,
     &      togo,stack,m,res)

      integer csl(ncq),sep(nlk),nxt(nlk),togo(ncq),stack(ncq),
     &      m(ncq,3),res(3,nlk),start,out,this,hi

      do iv = 1,ncq
            togo(iv) = 1
            stack(iv) = 0
      end do
      togo(start) = 0
      stack(1) = start
      is = 1
      ires = 0
      im = 0

      if(out.eq.0) then
            do j = 1,3
            do iv = 1,ncq-1
                  m(iv,j) = 0
            end do
            end do
      end if

      do while(is.ne.0) 
            this = stack(is)
            is = is-1
            if(this.gt.1) then
                  lo = csl(this-1)+1
            else
                  lo = 1
            end if
            hi = csl(this)
            do k = lo,hi
                  nx = nxt(k)
                  if(togo(nx).eq.1) then
                        if(out.eq.1) then
                              ires = ires+1
                              res(1,ires) = this
                              res(2,ires) = sep(k)
                              res(3,ires) = nx
                        else
                              im = im+1
                              m(im,1) = this
                              m(im,2) = sep(k)
                              m(im,3) = nx
                        end if
                        is = is+1
                        stack(is) = nx
                        togo(nx) = 0
                  end if
            end do
      end do

      if(out.eq.0) then
            do jm = im,1,-1
                  ires = ires+1
                  res(1,ires) = m(jm,3)
                  res(2,ires) = m(jm,2)
                  res(3,ires) = m(jm,1)
            end do
      end if

      return

      end

c-----------------------------------------------------------

      subroutine dopass(nvars,nvals,ncq,nsch,sched,
     &      ftcqv,ftcqvlo,ftcqvhi,ftcqp,ftcqplo,ftcqphi,
     &      ftspv,ftspvlo,ftspvhi,ftspp,ftspplo,ftspphi,
     &      wk,nwk,idone)

      use, intrinsic :: iso_fortran_env
      integer nvals(nvars),sched(nsch,3),
     &      ftcqv(*),ftcqvlo(nvars),ftcqvhi(nvars),
     &      ftcqplo(nvars),ftcqphi(nvars),
     &      ftspv(*),ftspvlo(nvars),ftspvhi(nvars),
     &      ftspplo(nvars),ftspphi(nvars),
     &      wk(nwk),c1,s,c2,idone(nsch)
      real(real64) :: ftcqp(*),ftspp(*)

      do isch=1,nsch
         c1 = sched(isch,1)
         s = sched(isch,2)
         c2 = sched(isch,3)
         k1 = ftcqvhi(c1)-ftcqvlo(c1)+1
         ks = ftspvhi(s)-ftspvlo(s)+1
         k2 = ftcqvhi(c2)-ftcqvlo(c2)+1
         kt = k1+ks+k2

         call pass(k1,ks,k2,nvars,nvals,
     &      ftcqv(ftcqvlo(c1)),ftspv(ftspvlo(s)),ftcqv(ftcqvlo(c2)),
     &      wk(4*kt+2*ks+1),wk(4*kt+2*ks+k1+1),wk(4*kt+3*ks+k1+1),
     &      wk        ,wk(     k1+1),wk(     k1+ks+1),
     &      wk(  kt+1),wk(  kt+k1+1),wk(  kt+k1+ks+1),
     &      wk(2*kt+1),wk(2*kt+k1+1),wk(2*kt+k1+ks+1),
     &      wk(3*kt+1),wk(3*kt+k1+1),wk(3*kt+k1+ks+1),
     &      wk(4*kt+1),wk(4*kt+ks+1),
     &      ftcqp(ftcqplo(c1)),ftspp(ftspplo(s)),ftcqp(ftcqplo(c2)),
     &      idone(isch))

      end do

      return

      end

c-----------------------------------------------------------

      subroutine pass(kcq1,ksep,kcq2,nvars,nvals,
     &      icq1,isep,icq2,acq1,asep,acq2,
     &      rcq1,rsep,rcq2,cpcq1,cpsep,cpcq2,
     &      qcq1,qsep,qcq2,nxtcq1,nxtsep,nxtcq2,
     &      cpx1,cpx2,a,b,c,idone)

      use, intrinsic :: iso_fortran_env
      integer icq1(kcq1),isep(ksep),icq2(kcq2)
      integer acq1(kcq1),rcq1(kcq1),cpcq1(kcq1),qcq1(kcq1),nxtcq1(kcq1)
      integer asep(ksep),rsep(ksep),cpsep(ksep),qsep(ksep),nxtsep(ksep)
      integer acq2(kcq2),rcq2(kcq2),cpcq2(kcq2),qcq2(kcq2),nxtcq2(kcq2)
      integer offsetsep,firstsep,offsetcq1,firstcq1,offsetcq2,firstcq2
      integer extracq1,extracq2,cpx1(ksep),cpx2(ksep),nvals(nvars)
      logical advance,done
      real(real64) :: a(*),b(*),c(*),temp,ratio

c.. input values of i***: 'names' of variables

c     call intpr('in pass',7,icq1,0)
c     call intpr('kcq1',4,kcq1,1)
c     call intpr('ksep',4,ksep,1)
c     call intpr('kcq2',4,kcq2,1)

      call setq(qcq1,kcq1,icq1,isep,ksep)
      call setq(qcq2,kcq2,icq2,isep,ksep)
c     call intpr('after setq',10,icq1,0)

      do j = 1,ksep
            qsep(j) = 1
            rsep(j) = nvals(isep(j))
            asep(j) = 0
      end do
      do j = 1,kcq1
            rcq1(j) = nvals(icq1(j))
            acq1(j) = 0
      end do
      do j = 1,kcq2
            rcq2(j) = nvals(icq2(j))
            acq2(j) = 0
      end do
      
      call setup(kcq1,rcq1,qcq1,cpcq1,nxtcq1,firstcq1)      
      call setup(ksep,rsep,qsep,cpsep,nxtsep,firstsep)
      call setup(kcq2,rcq2,qcq2,cpcq2,nxtcq2,firstcq2)
c     call intpr('after 2 setup',13,icq1,0)

      i = 0
      do j = 1,kcq1
            if(qcq1(j).eq.0) then
                  i = i+1
                  cpx1(i) = cpcq1(j)
            end if
      end do
      i = 0
      do j = 1,kcq2
            if(qcq2(j).eq.0) then
                  i = i+1
                  cpx2(i) = cpcq2(j)
            end if
      end do

c     if(kcq1.eq.4.and.ksep.eq.3.and.kcq2.eq.3) then
c     call intpr('acq1',4,acq1,kcq1)
c     call intpr('qcq1',4,qcq1,kcq1)
c     call intpr('rcq1',4,rcq1,kcq1)
c     call intpr('cpcq1',4,cpcq1,kcq1)
c     call intpr('nxtcq1',4,nxtcq1,kcq1)
c     call intpr('icq1',4,icq1,kcq1)
c
c     call intpr('asep',4,asep,ksep)
c     call intpr('qsep',4,qsep,ksep)
c     call intpr('rsep',4,rsep,ksep)
c     call intpr('cpsep',4,cpsep,ksep)
c     call intpr('nxtsep',4,nxtsep,ksep)
c     call intpr('isep',4,isep,ksep)
c
c     call intpr('acq2',4,acq2,kcq2)
c     call intpr('qcq2',4,qcq2,kcq2)
c     call intpr('rcq2',4,rcq2,kcq2)
c     call intpr('cpcq2',4,cpcq2,kcq2)
c     call intpr('nxtcq2',4,nxtcq2,kcq2)
c     call intpr('icq2',4,icq2,kcq2)
c
c     return
c     end if

      call init(ksep,qsep,asep,cpsep,offsetsep,firstsep)
c     call intpr('after init',10,icq1,0)

      idone = 0
      done = .true.

      do while(advance(ksep,rsep,asep,cpsep,nxtsep,offsetsep,firstsep))
            extracq1 = 0
            extracq2 = 0
            do j = 1,ksep
                  extracq1 = extracq1+asep(j)*cpx1(j)
                  extracq2 = extracq2+asep(j)*cpx2(j)
            end do
            ib = offsetsep+1
            call init(kcq1,qcq1,acq1,cpcq1,offsetcq1,firstcq1)
            if(firstcq1.eq.0) then
               temp = a(offsetcq1+extracq1+1)
            else
               temp = 0.0
               do while(advance(kcq1,rcq1,acq1,cpcq1,nxtcq1,
     &                  offsetcq1,firstcq1))
                  ia = offsetcq1+extracq1+1
                  temp = temp+a(ia)
               end do
            end if
            done = done.and.(abs(temp-b(ib)).lt.
     &                  0.0001*max(abs(temp),abs(b(ib))))
            if(b(ib).gt.0.0d0) then
                  ratio = temp/b(ib)
            else
                  ratio = temp
c                 call dblepr('b(ib)',5,b(ib),1)
c                 call dblepr('temp',4,temp,1)
            end if
            b(ib) = temp

            call init(kcq2,qcq2,acq2,cpcq2,offsetcq2,firstcq2)
            if(firstcq2.eq.0) then
               c(offsetcq2+extracq2+1) = c(offsetcq2+extracq2+1)*ratio
            else
               do while(advance(kcq2,rcq2,acq2,cpcq2,nxtcq2,
     &                  offsetcq2,firstcq2))
                  ic = offsetcq2+extracq2+1
                  c(ic) = c(ic)*ratio
               end do
            end if
      end do

      if(done) idone = 1
      return
      end

c---------------------------------------

      logical function advance(k,r,a,cp,nxt,offset,first)
      integer r(k),a(k),cp(k),nxt(k),offset,first

      a(first) = a(first)+1
      offset = offset+cp(first)
      j = first
      do while(a(j).ge.r(j))
            a(j) = 0
            offset = offset-r(j)*cp(j)
            j = nxt(j)
            if(j.eq.0) then
                  advance = .false.
                  return
            end if
            a(j) = a(j)+1
            offset = offset+cp(j)
      end do
      advance = .true.
      return
      end

c---------------------------------------

      subroutine setup(k,r,q,cp,nxt,first)
      integer r(k),q(k),cp(k),nxt(k),first,prod,prev

c.. input:  k,r(),q()
c.. output: cp(),nxt(),first

      prod = 1
      do j = 1,k
            cp(j) = prod
            prod = prod*r(j)
      end do

      first = 0
      prev = 0
      do j = 1,k
            nxt(j) = 0
            if(q(j).eq.1) then
                  if(first.eq.0) then
                        first = j
                  else
                        nxt(prev) = j
                  end if
                  prev = j
            end if
      end do
      if(prev.ne.0) nxt(prev) = 0

      return
      end

c---------------------------------------

      subroutine init(k,q,a,cp,offset,first)
      integer q(k),a(k),cp(k),offset,first

c.. input:  k,q(),a(),cp(),first
c.. output: some a(),offset

      offset = 0
      do j = 1,k
            if(q(j).eq.1) then
                  a(j) = 0
            else
                  offset = offset+a(j)*cp(j)
            end if
      end do

      if(first.eq.0) return

      a(first) = -1
      offset = offset-cp(first)

      return
      end

c---------------------------------------

      subroutine setq(qcq,kcq,acq,asep,ksep)
      integer qcq(kcq),acq(kcq),asep(ksep)

c.. input:  kcq,acq(),ksep,asep()
c.. output: qcq()

      jcq = 1
      do jsep = 1,ksep
            do while(acq(jcq).lt.asep(jsep))
                  qcq(jcq) = 1
                  jcq = jcq+1
            end do
c           if(acq(jcq).ne.asep(jsep)) stop 99
            qcq(jcq) = 0
            jcq = jcq+1
      end do
      if(jcq.le.kcq) then
            do j = jcq,kcq
                  qcq(j) = 1
            end do
      end if

      return
      end
