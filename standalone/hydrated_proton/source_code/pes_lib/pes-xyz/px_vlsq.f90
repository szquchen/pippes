SUBROUTINE px_vlsq (vbase, vcoef)
! Set up and solve least squares system for vcoef
interface
 subroutine vbase (xn0, w)
 integer, parameter :: wp=selected_real_kind(12,300)
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp), intent (out) :: w(0:,0:)
 end subroutine vbase
end interface
real (kind=wp), intent (out) :: vcoef(0:)
!----------------------------------------------------------------------
integer :: stat0, nd, nk, nb, n, nn0, nn, ip, i, j, k
real (kind=wp) :: wt
real (kind=wp), allocatable :: xn1(:,:), vec0(:,:), rhs(:), mat(:,:)
nd = size(pes2_xn,1)
nk = size(pes2_xn,2)
nb = size(vcoef)
n = size(pes2_f)
nn0 = 1+nd
nn = nn0*n
write (*,'(1x,a,i0,a,i0,a,i0,a)') &
  'Forming the LSQ system: ', nn, ' equations, ', &
  nb, ' unknowns; ', nn*nb/1024, ' KW'
allocate (xn1(0:nd-1,0:nk-1), vec0(0:nk-1,0:nb-1), rhs(0:nn-1), &
  mat(0:nn-1,0:nb-1), stat=stat0)
if (stat0.ne.0) then
 stop 'px_vlsq: failure to allocate workspace'
endif
do ip = 0, n-1
 wt = product(pes2_dwt/(pes2_dwt+pes2_f(ip)-pes2_fmin))
 xn1 = pes2_xn(:,:,ip)
 call vbase (xn1, vec0)
 mat(nn0*ip,0:nb-1) = wt*sum(vec0,1)
 mat(nn0*ip+1:nn0*ip+nd,0:nb-1) = wt*matmul(xn1,vec0)
 rhs(nn0*ip) = 0
 rhs(nn0*ip+1:nn0*ip+nd) = wt*pes2_ddip(:,ip)
enddo
if (1.le.nb) then
 call px_lsqs (mat, rhs, px_rcond, vcoef)
endif
! Note: mat has been destroyed
!! The following deallocation should not be necessary
deallocate (xn1, vec0, rhs, mat)
return
END SUBROUTINE px_vlsq
