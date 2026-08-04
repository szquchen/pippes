SUBROUTINE pes2_errf (nk, lev)
! Print an error report
integer, intent (in) :: nk, lev
!-----------------------------------------------------------------------
integer :: n, n1, n2, n5, ip, k
real (kind=wp) :: wt, t0, tw, t1, t2, t5, t1d, t2d, t5d, &
  err, errw, err1, err2, err5, errd1, errd2, errd5
real (kind=wp) :: gf0(0:pes2_nd-1,0:nk-1)
t0 = 0 ; tw = 0
t1 = 0 ; t2 = 0 ; t5 = 0
t1d = 0 ; t2d = 0 ; t5d = 0
n1 = 0 ; n2 = 0 ; n5 = 0
n = size(pes2_f)
do ip = 0, n-1
 wt = product(pes2_dwt/(pes2_dwt+pes2_f(ip)-pes2_fmin))
 t0 = t0+pes2_df(ip)**2
 tw = tw+(wt*pes2_df(ip))**2
 if (pes2_f(ip)-pes2_fmin.lt.pes2_dwt(0)) then
  t1 = t1+pes2_df(ip)**2
  if (pes2_havedip) then
   t1d = t1d+sum(pes2_ddip(:,ip)**2)
  endif
  n1 = n1+1
 else if (pes2_f(ip)-pes2_fmin.lt.2*pes2_dwt(0)) then
  t2 = t2+pes2_df(ip)**2
  if (pes2_havedip) then
   t2d = t2d+sum(pes2_ddip(:,ip)**2)
  endif
  n2 = n2+1
 else if (pes2_f(ip)-pes2_fmin.lt.5*pes2_dwt(0)) then
  t5 = t5+pes2_df(ip)**2
  if (pes2_havedip) then
   t5d = t5d+sum(pes2_ddip(:,ip)**2)
  endif
  n5 = n5+1
 endif
enddo
err = sqrt(t0/n)
errw = sqrt(tw/n)
err1 = sqrt(t1/max(1,n1))
err2 = sqrt(t2/max(1,n2))
err5 = sqrt(t5/max(1,n5))
if (pes2_havedip) then
 errd1 = sqrt(t1d/max(1,n1))
 errd2 = sqrt(t2d/max(1,n2))
 errd5 = sqrt(t5d/max(1,n5))
endif
write (*,'(2x,i0,1x,a,1p,(4g9.2))') &
  n, 'configurations, weighting parameter dwt(0:) =', pes2_dwt
write (*,'(2x,es9.2,2x,a,4x,a,es9.2)') &
  err, 'rms error over all configs;', &
  'weighted rms =', errw
write (*,'(2x,a)') &
  'number and rms error for (f0(ip)-fmin)/dwt(0) in [0,1), [1,2), [2,5):'
write (*,'(2x,3(2x,i7,1x,es10.2))') &
  n1, err1, n2, err2, n5, err5
if (pes2_havedip) then
 write (*,'(2x,a)') &
   'dipole errors for (f0(ip)-fmin)/dwt(0) in [0,1), [1,2), [2,5):'
 write (*,'(2x,3(2x,i7,1x,es10.2))') &
   n1, errd1, n2, errd2, n5, errd5
endif
if (1.le.lev) then
! look for outliers on pes2_f
 write (*,'(a)') ' Looking for outliers on f.'
 do ip = 0, n-1
  if (pes2_f(ip)-pes2_fmin.lt.2*pes2_dwt(0).and. &
    5*max(err1,err2).lt.abs(pes2_df(ip))) then
   write (*,'(2x,i8,3f12.4,f10.2,1x,a)') &
     ip, pes2_f(ip), pes2_f(ip)-pes2_fmin, pes2_df(ip), &
     pes2_df(ip)/max(err1,err2), '(*err12)'
  else if (pes2_f(ip)-pes2_fmin.lt.5*pes2_dwt(0).and. &
    5*max(err1,err2,err5).lt.abs(pes2_df(ip))) then
   write (*,'(2x,i8,3f12.4,f10.2,1x,a)') &
     ip, pes2_f(ip), pes2_f(ip)-pes2_fmin, pes2_df(ip), &
     pes2_df(ip)/max(err1,err2,err5), &
     '(*err125)'
  endif
  if (1.le.ip) then
!!    if (pes2_f(ip-1).eq.pes2_f(ip)) then
!!     write (*,'(a,2x,i8)') ' Alert, equal f:', ip
!!    endif
  endif
 enddo
endif
if (2.le.lev.and.pes2_havegf) then
! look for extreme outliers on pes2_gf
 write (*,'(a)') ' Looking for extreme outliers on grad.f.'
 do ip = 0, n-1
  gf0 = pes2_gf(:,:,ip)-pes2_dgf(:,:,ip)
  t0 = sqrt(sum(gf0(:,:)**2))
  t1 = sqrt(sum(pes2_dgf(:,:,ip)**2))
  if (t0.lt.t1) then
   write (*,'(a,i6)') ' ip =', ip
!! reorder the nuclei?
   do k = 0, nk-1
    write (*,'(a6,i4,4f16.8)') 'gf', k, pes2_gf(:,k,ip)
    write (*,'(a6,i4,4f16.8)') 'gf0', k, gf0(:,k)
    write (*,'(a6,i4,4f16.8)') 'dgf', k, pes2_dgf(:,k,ip)
   enddo
  endif
 enddo
endif
if (1.le.lev.and.pes2_havedip) then
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
return
END SUBROUTINE pes2_errf
