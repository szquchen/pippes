module pot_ttm_mod
use nasa_mod
implicit none

Contains
   !....................................................
   !
   !  calculates one bead energy
   !
   !....................................................
   subroutine pot_ttm(nw, box, r, rcut, der, vir, watdm, upot)
   use constants,             only: TWOPI, CHARGECON, DEBYE
   implicit none
   integer, intent( in    ) :: nw
   double precision, dimension(3),     intent( in    ) :: box
   double precision, dimension(3,3*nw),   intent( in    ) :: r
   double precision, intent( in    ) :: rcut
   double precision, dimension(3, 3*nw),  intent(   out ) :: der
   double precision, dimension(3,3),   intent(   out ) :: vir
   double precision, dimension(3,nw),  intent(   out ) :: watdm
   double precision, intent(   out ) :: upot
   !... local variables
   double precision :: umon, uvdw, uelec, uind
   integer :: io, jo, ih1, ih2, im, ia, ja, iw, jw
   integer :: fo, lo, fh, lh, fm, lm, fq, lq, fd, ld, fqod, lqod
   integer :: ii, jj, kk, iter
   integer :: na, nam
   double precision, dimension(3) :: ri, rij, dij, roh1, roh2
   double precision, dimension(3, 3) :: v1, rmon, vir1, arr33
   double precision :: drij, drsq, r2i, r4i, r6i, r8i, r10i, r12i, r14i, rc3, rc7, rc9
   double precision :: uij, rcut2, e1, pfI, tmp, gamma
   double precision, dimension(3,4*nw) :: d1, derm, Efq
   double precision, dimension(4*nw) :: chg, phi1
   double precision :: qi, qj, ts0, ts1, ts2, ts2c, ts3, vol
   double precision :: scl, uvdw_lrc, vir_lrc
   double precision :: en, delta_dip
   double precision, dimension(4*nw) :: phi
   double precision, dimension(4*nw)  :: polfac, polar
   double precision, dimension(:,:), allocatable     :: Efd, dip, dip_pr
   double precision, dimension(3)                    :: q3, di, dj, qdqd, tmp3, dmspars
   double precision, dimension(3, 3)                 :: drmon, dgrad, dd3
   double precision, dimension(3, 3, 3)              :: dq3, TSabc
   double precision, dimension(:,:,:,:), allocatable :: grdq
   double precision :: vdwA, vdwB, vdwC, vdwD, vdwE
   double precision :: selfq, p_fac
   double precision :: aDD, aCCaCD, polfacO, polfacH, polfacM, polarO, polarH, polarM
   double precision :: induce_sor=0.7d0
   double precision, dimension(:,:), allocatable :: rf

   gamma = 0.46d0 !the same as gammaM in TTM3
   na = size(r, dim=2)
   nam = 4*nw
   allocate(rf(3, nam))
   rf(1:3, 1:na) = r(1:3, 1:na)
   !.... find M-sites
   do iw=1, nw
      io=iw; ih1=io+nw; ih2=ih1+nw; im=ih2+nw
      rf(1:3, im) = 0.5d0*gamma*(rf(:,ih1)+rf(:,ih2))+rf(:,io)*(1.d0-gamma)
   enddo
      
   !... TTM model parameters

   vdwA = 0.d0
   vdwB = 0.d0
   vdwC = -0.72298855D+03
   vdwD = 0.10211829D+06
   vdwE = 0.37170376D+01
  
   dmspars(1) = 0.5d0
   dmspars(2) = 0.9578d0
   dmspars(3) = 0.012d0

   aDD = 0.175d0
   aCCaCD = 0.175d0
   polarO = 0.0d0
   polarH = 0.0d0
   polarM = 1.444d0
   polfacO = 0.837d0
   polfacH = 0.496d0
   polfacM = 0.837d0

   vol = box(1)*box(2)*box(3)

   rcut2 = rcut**2

   fo=1;   lo=nw
   fh=lo+1; lh=lo + 2*nw
   fm = lh+1; lm=lh + nw

   fq = fH; lq = lm
   fd = fO; ld = lH
   fqod = min(fq, fd); lqod = max(lq, ld)
   !... 
   allocate(grdq(3,3,3,nw))
   polfac(1:nw) = 1.d0 / sqrt(polfacO)
   polfac(nw+1:na) = 1.d0 / sqrt(polfacH)
   polfac(na+1:nam) = 1.d0 / sqrt(polfacM)

   allocate(dip(3, 1:nam))
   dip=0.d0
   polar(1:nw) = polarO
   polar(nw+1:na) = polarH
   polar(na+1:nam) = polarM

   en = 0.d0
   derm = 0.d0
   vir = 0.d0
   !..........................................................!
   !.... MONOMER DMS
   !..........................................................!
   tmp = 0.5d0*gamma/(1.d0-gamma)
   !$OMP PARALLEL DO DEFAULT (NONE)&
   !$OMP PRIVATE (iw,io,ih1,ih2, im, rmon, q3, dq3) &
   !$OMP SHARED (nw, dmspars, gamma, rf, box, tmp) &
   !$OMP SHARED(grdq, chg)
   do iw=1, nw
      io=iw; ih1=io+nw; ih2=ih1+nw; im=ih2+nw
      rmon(1:3, 1:3) = rf(1:3, [io, ih1, ih2])
      call dms_nasa(rmon, dmspars, q3, dq3)
      q3 = q3*CHARGECON
      dq3 = dq3*CHARGECON
      chg(io ) = 0.d0
      chg(ih1) =  q3(2) + tmp*(q3(2)+q3(3))
      chg(ih2) =  q3(3) + tmp*(q3(2)+q3(3))
      chg(im ) = q3(1) / (1.d0-gamma)
      grdq(:,1,:,iw)= dq3(:,1,:) + tmp*(dq3(:,1,:)+dq3(:,2,:))
      grdq(:,2,:,iw)= dq3(:,2,:) + tmp*(dq3(:,1,:)+dq3(:,2,:))
      grdq(:,3,:,iw)= dq3(:,3,:)-2.d0*tmp*(dq3(:,1,:)+dq3(:,2,:))
   enddo
   !$OMP END PARALLEL  DO
   !..........................................................!
   !.... MONOMER ENERGY
   !..........................................................!
   tmp = 0.5d0*gamma/(1.d0-gamma)
   d1 = 0.d0
   v1 = 0.d0
   e1 = 0.d0
   umon = 0.d0
   !$OMP PARALLEL DO DEFAULT (NONE)&
   !$OMP PRIVATE (iw,io,ih1,ih2,roh1,roh2,e1, rmon, drmon) &
   !$OMP SHARED (nw, box, rf, d1) &
   !$OMP REDUCTION (+: umon, v1)
   do iw=1, nw
      io=iw; ih1=io+nw; ih2=ih1+nw
      rmon(1:3, 1:3) = rf(1:3, [io, ih1, ih2])
      roh1(1:3) = rmon(1:3,2)-rmon(1:3,1)
      roh2(1:3) = rmon(1:3,3)-rmon(1:3,1)

      call pot_nasa(rmon, drmon, e1)
      umon = umon + e1
      d1(:, [io, ih1, ih2]) = drmon
      v1(1:3,1) = v1(1:3,1) + roh1(1:3)*drmon(1,2) + roh2(1:3)*drmon(1,3)
      v1(1:3,2) = v1(1:3,2) + roh1(1:3)*drmon(2,2) + roh2(1:3)*drmon(2,3)
      v1(1:3,3) = v1(1:3,3) + roh1(1:3)*drmon(3,2) + roh2(1:3)*drmon(3,3)
   enddo
   !$OMP END PARALLEL  DO
   en   = en + umon
   derm = derm + d1
   vir  = vir + v1

   !..........................................................!
   !.... VDW ENERGY
   !..........................................................!
   uvdw = 0.d0
   d1  = 0.d0
   v1  = 0.d0
   !$OMP PARALLEL DO DEFAULT (NONE)&
   !$OMP PRIVATE (io, ri, jo, rij, drsq, drij, r2i, r4i, r6i, r8i, r10i, r12i, r14i, tmp, uij, dij) &
   !$OMP SHARED (nw, box, rf, rcut2, vdwA, vdwB, vdwC, vdwD, vdwE) &
   !$OMP REDUCTION (+: uvdw, d1, v1)
   do io=1, nw-1
      ri = rf(1:3, io)
      do jo=io+1, nw

         rij = ri - rf(1:3, jo)
         rij = rij - box(1:3)*anint(rij / box(1:3)); drsq = dot_product(rij, rij)

         if  (drsq > rcut2) cycle

         drij = sqrt(drsq)
         r2i = 1.d0/drsq
         r6i = r2i**3
         r4i = r2i*r2i
         r8i = r6i*r2i
         r10i = r8i*r2i
         r12i = r6i*r6i
         r14i = r12i*r2i
         tmp = vdwD*dexp(-vdwE*drij)
         uij = vdwC*r6i + tmp
         dij =  -(r8i*6.d0*vdwC + tmp*vdwE/drij)*rij
         uvdw = uvdw + uij
         d1(1:3,io) = d1(1:3,io) + dij
         d1(1:3,jo) = d1(1:3,jo) - dij
         v1(1:3,1) = v1(1:3,1) + dij(1)*rij
         v1(1:3,2) = v1(1:3,2) + dij(2)*rij
         v1(1:3,3) = v1(1:3,3) + dij(3)*rij
      enddo
   enddo
   !$OMP END PARALLEL  DO
   !.... vdw-LRC
   rc3 = rcut2*rcut
   rc7 = rc3*rcut2*rcut2
   rc9 = rc7*rcut2
   uvdw_lrc = TWOPI * vdwC * nw * nw / (3.d0*rc3*vol) 
   vir_lrc = -2.d0*Uvdw_lrc
   uvdw    = uvdw    + uvdw_lrc
   v1(1,1) = v1(1,1) + vir_lrc
   v1(2,2) = v1(2,2) + vir_lrc
   v1(3,3) = v1(3,3) + vir_lrc

   en = en + uvdw 
   derm = derm + d1
   vir = vir + v1

   !..........................................................!
   !.... ELECTROSTATIC ENERGY
   !..........................................................!
   uelec = 0.d0
   d1    = 0.d0
   v1    = 0.d0
   phi = 0.d0
   Efq = 0.d0
   uelec = 0.d0
   phi = 0.d0
   do ia=fq, lq
      selfq =  21.d0/12.d0
      uelec = uelec - 0.5 * selfq*chg(ia)**2/rcut
      phi(ia) = phi(ia) - selfq*chg(ia)/rcut
   enddo
   !... (real space)
   !$OMP PARALLEL DO DEFAULT (NONE)&
   !$OMP PRIVATE (ia, iw, qi, ri, pfI, ja, qj, jw, scl, ts0,ts1,ts2,ts3, dij,rij, drsq, p_fac) &
   !$OMP SHARED (fqod,lqod,nw,chg,rf,box, rcut2, rcut, polfac, aCCaCD) &
   !$OMP REDUCTION (+: uelec, phi, efq, v1, d1)
   do ia=fqod, lqod-1
      iw = mod(ia-1, nw) + 1
      qi = chg(ia)
      ri = rf(1:3, ia)
      pfI = polfac(ia)
      do ja=ia+1, lqod
         jw = mod(ja-1, nw) + 1

         rij = ri -rf(1:3, ja)
         rij = rij - box(1:3)*anint(rij / box(1:3)); drsq = dot_product(rij, rij)

         if (drsq>rcut2) cycle

         qj = chg(ja)
         p_fac = aCCaCD*pfI*polfac(ja)
         scl = 1.d0; if (iw==jw) scl = 0.d0

         !SOS
!         scl = 1.d0
!         drsq=3.d0
         call sp3thole_tens(drsq, scl, rcut, p_fac, ts0, ts1, ts2, ts3)

         uelec = uelec + qi*qj * ts0
         phi(ia) = phi(ia) + qj*ts0
         phi(ja) = phi(ja) + qi*ts0
         Efq(1:3, ia) = Efq(1:3, ia) + ts1*qj*rij(1:3)
         Efq(1:3, ja) = Efq(1:3, ja) - ts1*qi*rij(1:3)
         dij = -qi*qj*ts1*rij
         d1(1:3,ia) = d1(1:3,ia) + dij
         d1(1:3,ja) = d1(1:3,ja) - dij
         v1(1:3,1) = v1(1:3,1) +  dij(1)*rij
         v1(1:3,2) = v1(1:3,2) +  dij(2)*rij
         v1(1:3,3) = v1(1:3,3) +  dij(3)*rij
      enddo
   enddo

   derm(1:3, fqod:lqod) = derm(1:3, fqod:lqod) + d1(1:3, fqod:lqod)
   vir = vir+v1

   !..........................................................!
   !.... Induced energy
   !..........................................................!

   allocate(dip_pr(3, fd:lm))
   allocate(Efd(3, fd:lm))
   Efd = 0.d0
   !... predict dipole
   do ia=fm, lm
      dip(1:3, ia) = polar(ia)*Efq(1:3, ia)
   enddo
   uind = 0.d0
   do ia=fm, lm
      uind = uind + dip(1,ia)*Efq(1,ia) + dip(2,ia)*Efq(2,ia) + dip(3,ia)*Efq(3,ia)
   enddo
   uind = -0.5d0*uind


   !allocate(dip_pr(3, fd:ld))
   !allocate(Efd(3, fd:ld))
   !Efd = 0.d0
   !... predict dipole
   !write(*,*) "initial dipole"
   !do ia=fd, ld
   !   dip(1:3, ia) = polar(ia)*Efq(1:3, ia)
   !enddo
   !uind = 0.d0
   !do ia=fd, ld
   !   uind = uind + dip(1,ia)*Efq(1,ia) + dip(2,ia)*Efq(2,ia) + dip(3,ia)*Efq(3,ia)
   !enddo
   !uind = -0.5d0*uind
   !write(*,*) "end dipole"
   !write(*,*) uind
   !... start iterative procedure
   do iter=1, 300  !induce maxiter
      dip_pr = dip
      !efd(1:3, fd:ld) = efq(1:3, fd:ld)
      efd(1:3, fm:lm) = efq(1:3, fm:lm)
      !$OMP PARALLEL DO DEFAULT (NONE)&
      !$OMP PRIVATE (ia, iw, di, ri,  pfI, ja, jw, drsq, scl, dj,ts0, ts1, ts2, ts2c, ts3, rij, p_fac, dd3)&
      !$OMP SHARED (fm, lm, nw, dip, rf, polfac, box, aDD, rcut2, rcut)& 
      !$OMP REDUCTION (+: Efd)
      !do ia=fd, ld-1
      do ia=fm, lm
         di = dip(1:3, ia)
         iw = mod(ia-1, nw) + 1
         ri = rf(1:3, ia)
         pfI = polfac(ia)
         do ja=ia+1, lm
            dj = dip(1:3, ja)

            jw = mod(ja-1, nw) + 1
            rij = ri - rf(1:3, ja)
            rij = rij - box(1:3)*anint(rij / box(1:3)); drsq = dot_product(rij, rij)

            if (drsq> rcut2) cycle

            scl = 1.d0; if (iw==jw) scl = 1.d0   ! scl=1 always for ttm21f

            p_fac = aDD*pfI*polfac(ja)
            call sp3thole_tens(drsq, scl, rcut, p_fac, ts0, ts1, ts2, ts3)
            ts2c = 3.d0*ts2
            dd3(1:3, 1) = [ts2c*rij(1)*rij(1)-ts1, ts2c*rij(1)*rij(2),     ts2c*rij(1)*rij(3)]
            dd3(1:3, 2) = [ts2c*rij(2)*rij(1)    , ts2c*rij(2)*rij(2)-ts1, ts2c*rij(2)*rij(3)]
            dd3(1:3, 3) = [ts2c*rij(3)*rij(1)    , ts2c*rij(3)*rij(2),     ts2c*rij(3)*rij(3)-ts1]
            Efd(1:3, ia) = Efd(1:3, ia) + dd3(:,1)*dj(1) + dd3(:,2)*dj(2) + dd3(:,3)*dj(3) 
            Efd(1:3, ja) = Efd(1:3, ja) + dd3(:,1)*di(1) + dd3(:,2)*di(2) + dd3(:,3)*di(3) 
         enddo
      enddo
      !$OMP END PARALLEL  DO

      uind = 0.d0
      delta_dip = 0.d0
      !do ia=fd, ld
      do ia=fm, lm
         dip(:,ia) = (1.d0-induce_sor)*dip(:, ia) + induce_sor*polar(ia)*Efd(:,ia)
         Uind = Uind+dip(1,ia)*Efq(1,ia)+dip(2,ia)*Efq(2,ia)+dip(3,ia)*Efq(3,ia)
         tmp3 = dip(1:3, ia) - dip_pr(1:3, ia)
         delta_dip = delta_dip+dot_product(tmp3, tmp3)
      enddo
      uind = -0.5*uind
      delta_dip = sqrt(delta_dip/(ld-fd+1))
      !
      if (delta_dip<1.d-6) exit
   enddo


   !////////// CALCULATION OF FORCES/VIRIAL  /////////////////////////!
   vir1 = 0.d0
   d1 = 0.d0
   !$OMP PARALLEL DO DEFAULT(NONE) &
   !$OMP PRIVATE(ia,iw,  pfI, di, ja, jw, dj, ts0, ts1, ts2, ts3, ri, rij,ii,jj,kk,Tsabc,arr33,dij, scl, drsq, p_fac) &
   !$OMP SHARED(fm, lm, dip, nw, rf, polfac, box, rcut2, aDD, rcut ) &
   !$OMP REDUCTION(+:vir1, d1)
   do ia=fm, lm-1
      di = dip(1:3, ia)
      iw = mod(ia-1, nw) + 1
      ri = rf(1:3, ia)
      pfI = polfac(ia)
      do ja=ia+1, lm
         jw = mod(ja-1, nw) + 1
         rij = ri - rf(1:3, ja)
         rij = rij - box(1:3)*anint(rij / box(1:3)); drsq = dot_product(rij, rij)

         if (drsq> rcut2) cycle
         dj = dip(1:3, ja)
         scl = 1.d0; if (iw==jw) scl = 1.d0   ! scl=1 always for ttm21f

         p_fac = aDD*pfI*polfac(ja)
         call sp3thole_tens(drsq, scl, rcut, p_fac, ts0, ts1, ts2, ts3)
         do ii=1,3; do jj=1,3; do kk=1,3
            TSabc(ii,jj,kk)=15.d0*rij(ii)*rij(jj)*rij(kk)*ts3
         enddo; enddo; enddo
         do jj=1,3
            TSabc(:,jj,jj) = TSabc(:,jj,jj) - 3.d0*ts2*rij(:)
            TSabc(jj,:,jj) = TSabc(jj,:,jj) - 3.d0*ts2*rij(:)
            TSabc(jj,jj,:) = TSabc(jj,jj,:) - 3.d0*ts2*rij(:)
         enddo
         arr33=TSabc(:,:,1)*dj(1)+ TSabc(:,:,2)*dj(2)+ TSabc(:,:,3)*dj(3)
         dij = di(1)*arr33(1,:)+di(2)*arr33(2,:)+di(3)*arr33(3,:)
         d1(1:3, ia) = d1(1:3, ia) + dij
         d1(1:3, ja) = d1(1:3, ja) - dij
         vir1(1:3,1)=vir1(1:3,1) + dij(1:3)*rij(1)
         vir1(1:3,2)=vir1(1:3,2) + dij(1:3)*rij(2)
         vir1(1:3,3)=vir1(1:3,3) + dij(1:3)*rij(3)
      enddo
   enddo
   !$OMP END PARALLEL DO 
   vir = vir + vir1
   derm(1:3, fm:lm) = derm(1:3, fm:lm) + d1(1:3, fm:lm)
   !... forces/second part
   d1 = 0.d0
   v1 = 0.d0
   phi1 = 0.d0
   !$OMP PARALLEL DO DEFAULT (NONE)&
   !$OMP PRIVATE (ia, iw, qi, di, ri, pfi, ja,jw,scl,qj,dj, drsq,qdqd,rij, ts0,ts1,ts2,ts3, dd3, dij, p_fac) &
   !$OMP SHARED (fqod,lqod,nw,dip,rf, polfac, chg, box, rcut2, aCCaCD,rcut) &
   !$OMP REDUCTION (+: v1, d1, phi1)
   do ia=fqod, lqod-1
      iw = mod(ia-1, nw) + 1
      qi = chg(ia)
      di = dip(1:3, ia)
      ri = rf(1:3, ia)
      pfI = polfac(ia)
      do ja=ia+1, lqod

         jw = mod(ja-1, nw) + 1

         scl = 1.d0; if (iw==jw) scl = 0.d0

         qj = chg(ja)
         dj = dip(1:3, ja)
         qdqd=qj*di - qi*dj
         rij = ri - rf(1:3,ja)
         rij = rij - box(1:3)*anint(rij / box(1:3)); drsq = dot_product(rij, rij)

         if (drsq>rcut2) cycle

         p_fac = aCCaCD*pfI*polfac(ja)
         call sp3thole_tens(drsq, scl, rcut, p_fac, ts0, ts1, ts2, ts3)
         dd3(1:3,1) = 3.d0*ts2*rij(1)*rij(1:3); dd3(1,1)=dd3(1,1)-ts1;
         dd3(1:3,2) = 3.d0*ts2*rij(2)*rij(1:3); dd3(2,2)=dd3(2,2)-ts1;
         dd3(1:3,3) = 3.d0*ts2*rij(3)*rij(1:3); dd3(3,3)=dd3(3,3)-ts1;
         dij = dd3(1,:)*qdqd(1) + dd3(2,:)*qdqd(2) + dd3(3,:)*qdqd(3)

         phi1(ia) = phi1(ia) + ts1*(rij(1)*dj(1) + rij(2)*dj(2) + rij(3)*dj(3))
         phi1(ja) = phi1(ja) - ts1*(rij(1)*di(1) + rij(2)*di(2) + rij(3)*di(3))
         d1(1:3,ia) = d1(1:3,ia) + dij
         d1(1:3,ja) = d1(1:3,ja) - dij
         v1(1:3,1) = v1(1:3,1) +  dij(1:3)*rij(1)
         v1(1:3,2) = v1(1:3,2) +  dij(1:3)*rij(2)
         v1(1:3,3) = v1(1:3,3) +  dij(1:3)*rij(3)
      enddo 
   enddo ! do ia=fh, lm
   !$OMP END PARALLEL  DO
   derm(1:3, fqod:lqod) = derm(1:3, fqod:lqod) + d1(1:3, fqod:lqod)
   phi(fqod:lqod) = phi(fqod:lqod) + phi1(fqod:lqod)

   vir = vir + v1
   deallocate(dip_pr, efd)
   !
   !   derivatives and virial coming from fluctuating charges
   !
   v1 = 0.d0
   !$OMP PARALLEL DO DEFAULT (NONE)&
   !$OMP PRIVATE (iw, io, ih1, ih2, im, dgrad, roh1, roh2)&
   !$OMP SHARED (nw, grdq, phi, derm, rf, box) &
   !$OMP REDUCTION (+: v1)
   do iw=1, nw
      iO=iw; iH1=iO+nw; iH2=iO+2*nw; iM=iO+3*nw
      dgrad(:,2)=grdq(1,1,:,iw)*phi(iH1)+grdq(1,2,:,iw)*phi(iH2)+grdq(1,3,:,iw)*phi(iM)
      dgrad(:,3)=grdq(2,1,:,iw)*phi(iH1)+grdq(2,2,:,iw)*phi(iH2)+grdq(2,3,:,iw)*phi(iM)
      dgrad(:,1)=grdq(3,1,:,iw)*phi(iH1)+grdq(3,2,:,iw)*phi(iH2)+grdq(3,3,:,iw)*phi(iM)
      derm(:,iH1)=derm(:,iH1)+dgrad(:,2)
      derm(:,iH2)=derm(:,iH2)+dgrad(:,3)
      derm(:,iO )=derm(:,iO )+dgrad(:,1)
      roh1=rf(:,ih1)-rf(:,io)
      roh2=rf(:,ih2)-rf(:,io)
      v1(:,1) = v1(:,1) + (roh1(:)*dgrad(1,2) + roh2(:)*dgrad(1,3) )
      v1(:,2) = v1(:,2) + (roh1(:)*dgrad(2,2) + roh2(:)*dgrad(2,3) )
      v1(:,3) = v1(:,3) + (roh1(:)*dgrad(3,2) + roh2(:)*dgrad(3,3) )
   enddo
   !$OMP END PARALLEL  DO
   vir = vir + v1
   !
   !   redistribude forces from Msite to the real atoms
   !
   do iw=1, nw
      io=iw; ih1=io+nw; ih2=ih1+nw; im=ih2+nw
      der(:, ih1) = derm(:, ih1) + 0.5d0*gamma*derm(:,im)
      der(:, ih2) = derm(:, ih2) + 0.5d0*gamma*derm(:,im)
      der(:, io ) = derm(:, io ) + (1.d0-gamma)*derm(:,im)
   enddo
   !
   !   calculate dipole moments
   !
   watdm = 0.d0
   do iw=1, nw
      io=iw; ih1=io+nw; ih2=ih1+nw; im=ih2+nw
      watdm(1:3, iw) = chg(io)*rf(1:3,io) + chg(ih1)*rf(1:3,ih1)  + chg(ih2)*rf(1:3,ih2) + chg(im)*rf(1:3,im) 
   enddo
   do ia=fd, ld
      iw=mod(ia-1, nw) + 1
      watdm(1:3, iw) = watdm(1:3, iw) + dip(1:3, ia)
   enddo
   ! finalize
   deallocate(grdq, rf)
   upot= umon + uvdw + uelec + uind
   end subroutine pot_ttm

   !
   subroutine sp3thole_tens(drsq, scl, rc, p_fac, ts0, ts1, ts2, ts3)
   use math_mod,                    ONLY: gammq2_3
   implicit none
   double precision,             intent( in    ) :: drsq, p_fac, rc, scl
   double precision,             intent(   out ), optional :: ts0, ts1, ts2, ts3
   !...
   double precision :: r2i, dri, dd, ra3, ra, exp1, ch1, ch2, ch3, sr1, sr2, sr3
   double precision :: rs, rs3, rs5, rs6, rs7, g0, g1, g2, g3
   double precision, parameter :: g23=1.3541179394264d0

   R2i = 1.d0/dRsq; dRi = sqrt(R2i); dd=sqrt(dRsq)
   ra3 = drsq*dd*p_fac
   ra  = p_fac**(1.d0/3.d0)
   exp1 = dexp(-rA3)
!
   rs=dd/rc; rs3=rs**3;  rs5=rs**5; rs6=rs**6; rs7=rs**7
   g0=1.d0-(21.d0/12.d0*rs-21.d0/4.d0*rs5+7.d0*rs6-2.5d0*rs7) + (scl-1.d0)
   ts0 = g0 * dri
   g1= (-21.d0/12.d0*rs + 105.d0/4.d0*rs5 - 42.d0*rs6 + 17.5d0*rs7)*dri
   ts1 = (ts0 - g1) * r2i
   g2= (105.d0*rs5 - 210.d0*rs6 + 105.d0*rs7)*r2i
   ts2 = (ts1 + g2*dri/3.d0) * r2i
   g3  = (315.d0*rs5 - 840.d0*rs6 + 525.d0*rs7)*dri*r2i
   ts3 = ts2*r2i  + (g2-dd*g3)/15.d0*r2i*dri*r2i

   ts0 = ts0  - exp1*dri*scl + rA*g23*gammq2_3( ra3 )*scl
   ch1 = -dri*r2i
   sr1 = (1.d0 - exp1)*dri*r2i
   ts1 = ts1+(ch1+sr1)*scl
   ch2 =  ch1*r2i
   sr2 = (sr1 - exp1*p_fac)*r2i
   ts2 = ts2+(ch2+sr2)*scl
   ch3 =  ch2*r2i
   sr3 = (sr2 - 0.6d0*exp1*p_fac*p_fac*dd)*r2i
   ts3 = ts3+(ch3+sr3)*scl
   end subroutine sp3thole_tens
   
end module pot_ttm_mod
