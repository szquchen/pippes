SUBROUTINE pes2_init (fname, nk, fun, vfun, iord)
! Initialize module data
integer, intent (in) :: nk
character (len=*), intent (in) :: fname
optional :: vfun
interface
 function fun (xn0) result (f0)
 use pes0, wp=>pes0_wp
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp) :: f0
 end function fun
 function vfun (xn0) result (vf0)
 use pes0, wp=>pes0_wp
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp) :: vf0(0:size(xn0,2)-1)
 end function vfun
end interface
integer, intent (in), optional :: iord(0:nk-1)
!----------------------------------------------------------------------
integer :: iun, n, ip, i, k, iord2(0:pes2_nd*nk-1)
real (kind=wp) :: t0, xn0(0:pes2_nd-1,0:nk-1), xn1(0:pes2_nd-1,0:nk-1), &
  gf0(0:pes2_nd-1,0:nk-1), g2f0(0:pes2_nd*nk-1,0:pes2_nd*nk-1)
character (len=80) :: cht0
call pes_getiun (iun)
open (iun, status='old', file=fname)
n = pes_nmbrec(iun)/(nk+2)
allocate (pes2_ikord(0:nk-1))
allocate (pes2_ikord2(0:pes2_nd*nk-1))
if (present(iord)) then
 pes2_ikord = iord
else
 pes2_ikord = (/(k,k=0,nk-1)/)
endif
do i = 0, pes2_nd*nk-1
 k = i/pes2_nd
 pes2_ikord2(i) = pes2_nd*pes2_ikord(k)+modulo(i,pes2_nd)
enddo
allocate (pes2_xn(0:pes2_nd-1,0:nk-1,0:n-1))
allocate (pes2_lab(0:n-1))
allocate (pes2_f(0:n-1), pes2_df(0:n-1))
if (pes2_havegf) then
 allocate (pes2_gf(0:pes2_nd-1,0:nk-1,0:n-1))
 allocate (pes2_dgf(0:pes2_nd-1,0:nk-1,0:n-1))
endif
if (pes2_haveg2f) then
 allocate (pes2_g2f(0:pes2_nd*nk-1,0:pes2_nd*nk-1,0:n-1))
 allocate (pes2_dg2f(0:pes2_nd*nk-1,0:pes2_nd*nk-1,0:n-1))
endif
if (pes2_havedip) then
 allocate (pes2_dip(0:pes2_nd-1,0:n-1))
 allocate (pes2_ddip(0:pes2_nd-1,0:n-1))
endif
write (*,*) 'Reading ', trim(fname)
cht0 = 'configurations: function values'
if (pes2_havegf) then
 cht0 = trim(cht0)//' and gradients'
endif
if (pes2_haveg2f) then
 cht0 = trim(cht0)//' and hessians'
endif
if (pes2_havedip) then
 cht0 = trim(cht0)//' and dipoles'
endif
write (*,*) 'Expecting', n, trim(cht0)
!! For now, require that pes2_haveg2f implies pes2_havegf
if (pes2_haveg2f.and..not.pes2_havegf) then
 stop 'pes2_init: pes2_haveg2f but not pes2_havegf'
endif
do ip = 0, n-1
 if (pes2_haveg2f) then
  if (pes2_havedip) then
   call pes2_readf (iun, pes2_lab(ip), xn0(:,:), pes2_f(ip), &
     gf0=gf0(:,:), g2f0=g2f0(:,:), dip=pes2_dip(:,ip))
  else
   call pes2_readf (iun, pes2_lab(ip), xn0(:,:), pes2_f(ip), &
     gf0=gf0(:,:), g2f0=g2f0(:,:))
  endif
  pes2_xn(:,:,ip) = xn0(:,:)
  pes2_gf(:,:,ip) = gf0(:,:)
  pes2_g2f(:,:,ip) = g2f0(:,:)
 else if (pes2_havegf) then
  if (pes2_havedip) then
   call pes2_readf (iun, pes2_lab(ip), xn0(:,:), pes2_f(ip), &
     gf0=gf0(:,:), dip=pes2_dip(:,ip))
  else
   call pes2_readf (iun, pes2_lab(ip), xn0(:,:), pes2_f(ip), &
     gf0=gf0(:,:))
  endif
  pes2_xn(:,:,ip) = xn0(:,:)
  pes2_gf(:,:,ip) = gf0(:,:)
 else if (pes2_havedip) then
  call pes2_readf (iun, pes2_lab(ip), xn0(:,:), pes2_f(ip), &
    dip=pes2_dip(:,ip))
  pes2_xn(:,:,ip) = xn0(:,:)
 else
  call pes2_readf (iun, pes2_lab(ip), xn0(:,:), pes2_f(ip))
  pes2_xn(:,:,ip) = xn0(:,:)
 endif
enddo
close (iun)
pes2_fmin = minval(pes2_f)
pes2_fmax = maxval(pes2_f)
write (*,'(2x,a,es14.6,4x,a,es14.6)') &
  ' fmin =', pes2_fmin, ' fmax =', pes2_fmax
do ip = 0, n-1
 xn1 = pes2_xn(:,:,ip)
 pes2_df(ip) = pes2_f(ip)-fun(xn1)
 if (pes2_havegf) then
  pes2_dgf(:,:,ip) = pes2_gf(:,:,ip)- &
    pes2_grad(fun,pes2_dfdis,xn1)
 endif
 if (pes2_haveg2f) then
  pes2_dg2f(:,:,ip) = pes2_g2f(:,:,ip)- &
    pes2_grad2(fun,pes2_df2dis,xn1)
 endif
 if (pes2_havedip) then
  pes2_ddip(:,ip) = pes2_dip(:,ip)-matmul(xn1,vfun(xn1))
 endif
enddo
write (*,'(2x,a,es14.6,4x,a,es14.6)') &
  'dfmin =', minval(pes2_df), 'dfmax =', maxval(pes2_df)
if (pes2_havedip) then
 t0 = -huge(t0)
 do ip = 0, n-1
  t0 = max(t0,sqrt(sum(pes2_ddip(:,ip)**2)))
 enddo
 write (*,'(2x,a,es14.6)') &
   'max norm ddip =', t0
endif
return
END SUBROUTINE pes2_init
