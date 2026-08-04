SUBROUTINE px_prune (iun, base)
!! No longer used.  Must reorder the data for printout.
! Produce a pruned dataset
integer, intent (in) :: iun
interface
 subroutine base (xn0, w)
 integer, parameter :: wp=selected_real_kind(12,300)
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp), intent (out) :: w(0:)
 end subroutine base
end interface
!-----------------------------------------------------------------------
integer :: n, ip, k
real (kind=wp) :: df0
real (kind=wp), allocatable :: vec(:)
allocate (vec(0:size(coef)-1))
open (iun, status='replace', form='formatted', &
  file='./newset.out')
n = size(pes2_f)
do ip = 0, n-1
 call base (xn(:,:,ip), vec)
 df0 = pes2_df(ip)-dot_product(coef,vec)
 if (pes2_f(ip)-pes2_fmin.lt.2.0_wp.and. &
   df0.lt.0.01_wp+(pes2_f(ip)-pes2_fmin)/10) then
  write (iun,'(2x,i6)') ip
  do k = 0, size(xn,2)-1
   write (iun,'(2x,4f18.9)') xn(:,k,ip)
  enddo
  write (iun,'(2x,f18.9)') pes2_f(ip)
  if (pes2_havegf) then
   do k = 0, size(xn,2)-1
    write (iun,'(2x,4f18.9)') pes2_gf(:,k,ip)
   enddo
  endif
  if (pes2_haveg2f) then
   do k = 0, size(pes2_g2f,2)-1
    write (iun,'(2x,10f18.9)') pes2_g2f(:,k,ip)
   enddo
  endif
 else
  write (*,'(2x,i8,3f12.4)') ip, pes2_f(ip), pes2_f(ip)-pes2_fmin, df0
 endif
enddo
close (iun)
return
END SUBROUTINE px_prune
