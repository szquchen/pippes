SUBROUTINE px_errvf (vbase, vcoef)
! Print an error report
interface
 subroutine vbase (xn0, w)
 integer, parameter :: wp=selected_real_kind(12,300)
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp), intent (out) :: w(0:,0:)
 end subroutine vbase
end interface
real (kind=wp), intent (in) :: vcoef(0:)
!-----------------------------------------------------------------------
integer :: n, n1, n2, n5, ip
real (kind=wp) :: wt, td0, tdw, td1, td2, td5, errd, errdw, &
  errd1, errd2, errd5
real (kind=wp) :: vec(0:size(pes2_xn,2)-1,0:size(vcoef)-1), &
  xn1(0:size(pes2_xn,1)-1,0:size(pes2_xn,2)-1), &
  ddip0(0:size(pes2_xn,1)-1)
td0 = 0 ; tdw = 0
td1 = 0 ; td2 = 0 ; td5 = 0
n1 = 0 ; n2 = 0 ; n5 = 0
n = size(pes2_f)
do ip = 0, n-1
 wt = product(pes2_dwt/(pes2_dwt+pes2_f(ip)-pes2_fmin))
 xn1 = pes2_xn(:,:,ip)
 call vbase (xn1, vec)
 ddip0 = pes2_ddip(:,ip)-matmul(xn1,matmul(vec,vcoef))
 td0 = td0+sum(ddip0**2)
 tdw = tdw+wt**2*sum(ddip0**2)
 if (pes2_f(ip)-pes2_fmin.lt.pes2_dwt(0)) then
  td1 = td1+sum(ddip0**2)
  n1 = n1+1
 else if (pes2_f(ip)-pes2_fmin.lt.2*pes2_dwt(0)) then
  td2 = td2+sum(ddip0**2)
  n2 = n2+1
 else if (pes2_f(ip)-pes2_fmin.lt.5*pes2_dwt(0)) then
  td5 = td5+sum(ddip0**2)
  n5 = n5+1
 endif
enddo
errd = sqrt(td0/n)
errdw = sqrt(tdw/n)
errd1 = sqrt(td1/max(1,n1))
errd2 = sqrt(td2/max(1,n2))
errd5 = sqrt(td5/max(1,n5))
write (*,'(2x,i0,1x,a,1p,(4g9.2))') &
  n, 'configurations, weighting parameter dwt(0:) =', pes2_dwt
write (*,'(2x,es9.2,2x,a,4x,a,es9.2)') &
  errd, 'rms error over all configs;', &
  'weighted rms =', errdw
write (*,'(2x,a)') &
  'number and rms error for (f0(ip)-fmin)/dwt(0) in [0,1), [1,2), [2,5):'
write (*,'(2x,3(2x,i7,1x,es10.2))') &
  n1, errd1, n2, errd2, n5, errd5
return
END SUBROUTINE px_errvf
