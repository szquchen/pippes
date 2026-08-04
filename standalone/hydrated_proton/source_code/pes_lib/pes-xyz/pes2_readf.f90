SUBROUTINE pes2_readf (iun, lab, xn0, f0, gf0, g2f0, dip)
! Read data lab, f0 (, dip), xn0 (, gf0, g2f0)
integer, intent (in) :: iun
character (len=*), intent (out) :: lab
real (kind=wp), intent (out) :: xn0(0:,0:), f0
real (kind=wp), intent (out), optional :: gf0(0:,0:), g2f0(0:,0:), &
  dip(0:)
!-----------------------------------------------------------------------
integer :: nd, nk, k, ik, idum
character (len=8) :: chdum
character (len=1023) :: chrec
real (kind=wp) :: tv0(0:size(xn0,1)-1), tv1(0:size(xn0,1)-1)
nd = size(xn0,1)
nk = size(xn0,2)
if (present(gf0)) then
 if (size(gf0,1).ne.nd.or.size(gf0,2).ne.nk) then
  stop 'pes2_readf: dimension error gf0'
 endif
endif
if (present(g2f0)) then
 if (size(g2f0,1).ne.nd*nk.or.size(g2f0,2).ne.nd*nk) then
  stop 'pes2_readf: dimension error g2f0'
 endif
endif
if (present(dip)) then
 if (size(dip).ne.nd) then
  stop 'pes2_readf: dimension error dip'
 endif
endif
read (iun,*) chrec
read (chrec,*) idum
if (idum.ne.nk) then
 stop 'pes2_readf: bad input nk'
endif
lab = ''
read (chrec,*,end=1) idum, lab
1 continue
! Note the units in our xyz data files: geometry in angstrom,
! energy in hartree, dipole moment in (elcharge)*bohr, energy
! gradient in hartree/bohr, hessian in hartree/bohr^2.
! And note as well: it is the gradient and not the force.
if (present(dip)) then
 read (iun,*) f0, dip
 f0 = f0*pes_hartree
 dip = dip*pes_elcharge*pes_bohr
else
 read (iun,*) f0
 f0 = f0*pes_hartree
endif
do k = 0, nk-1
 if (allocated(pes2_ikord)) then
  ik = pes2_ikord(k)
 else
  ik = k
 endif
 if (present(gf0)) then
  read (iun,*) chdum, tv0(:), tv1(:)
  xn0(:,ik) = tv0(:)*pes_angstrom
  gf0(:,ik) = tv1(:)*pes_hartree/pes_bohr
 else
  read (iun,*) chdum, tv0(:)
  xn0(:,ik) = tv0(:)*pes_angstrom
 endif
enddo
if (present(g2f0)) then
 do k = 0, size(g2f0,2)-1
  if (allocated(pes2_ikord2)) then
   read (iun,*) chdum, g2f0(pes2_ikord2,pes2_ikord2(k))
   g2f0(:,pes2_ikord2(k)) = g2f0(:,pes2_ikord2(k))*pes_hartree/(pes_bohr**2)
  else
   read (iun,*) chdum, g2f0(:,k)
   g2f0(:,k) = g2f0(:,k)*pes_hartree/(pes_bohr**2)
  endif
 enddo
endif
return
END SUBROUTINE pes2_readf
