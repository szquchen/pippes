SUBROUTINE pes2_errvf (nk, lev)
! Print an error report
integer, intent (in) :: nk, lev
!-----------------------------------------------------------------------
integer :: n, n1, n2, n5, ip, k
real (kind=wp) :: wt, t0, t1d, t2d, t5d, &
  err, err1, err2, err5, errd1, errd2, errd5
real (kind=wp) :: gf0(0:pes2_nd-1,0:nk-1)
t1d = 0 ; t2d = 0 ; t5d = 0
n1 = 0 ; n2 = 0 ; n5 = 0
n = size(pes2_f)
if (pes2_havedip) then
 do ip = 0, n-1
  wt = product(pes2_dwt/(pes2_dwt+pes2_f(ip)-pes2_fmin))
  if (pes2_f(ip)-pes2_fmin.lt.pes2_dwt(0)) then
   t1d = t1d+sum(pes2_ddip(:,ip)**2)
   n1 = n1+1
  else if (pes2_f(ip)-pes2_fmin.lt.2*pes2_dwt(0)) then
   t2d = t2d+sum(pes2_ddip(:,ip)**2)
   n2 = n2+1
  else if (pes2_f(ip)-pes2_fmin.lt.5*pes2_dwt(0)) then
   t5d = t5d+sum(pes2_ddip(:,ip)**2)
   n5 = n5+1
  endif
 enddo
 errd1 = sqrt(t1d/max(1,n1))
 errd2 = sqrt(t2d/max(1,n2))
 errd5 = sqrt(t5d/max(1,n5))
 write (*,'(2x,i0,1x,a,1p,(4g9.2))') &
   n, 'configurations, weighting parameter dwt(0:) =', pes2_dwt
 write (*,'(2x,a)') &
   'number and rms error for (f0(ip)-fmin)/dwt(0) in [0,1), [1,2), [2,5):'
 write (*,'(2x,a)') &
   'dipole errors for (f0(ip)-fmin)/dwt(0) in [0,1), [1,2), [2,5):'
 write (*,'(2x,3(2x,i7,1x,es10.2))') &
   n1, errd1, n2, errd2, n5, errd5
 if (1.le.lev) then
! look for outliers on pes2_dip
  write (*,'(a)') ' Looking for outliers on dip.'
  do ip = 0, n-1
   t0 = sqrt(sum(pes2_ddip(:,ip)**2))
   if (pes2_f(ip)-pes2_fmin.lt.2*pes2_dwt(0).and. &
     5*max(errd1,errd2).lt.t0) then
    write (*,'(2x,i8,4f12.4,f10.2,1x,a)') &
      ip, pes2_f(ip), pes2_dip(:,ip), &
      t0/max(errd1,errd2), '(*errd12)'
   else if (pes2_f(ip)-pes2_fmin.lt.5*pes2_dwt(0).and. &
     5*max(errd1,errd2,errd5).lt.t0) then
    write (*,'(2x,i8,4f12.4,f10.2,1x,a)') &
      ip, pes2_f(ip), pes2_dip(:,ip), &
      t0/max(errd1,errd2,errd5), &
      '(*errd125)'
   endif
  enddo
 endif
endif
return
END SUBROUTINE pes2_errvf
