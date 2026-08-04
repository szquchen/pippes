SUBROUTINE pes2_readf_old (iun, xn0, f0, gf0, g2f0)
! Read data xn0, f0 (, gf0)
integer, intent (in) :: iun
real (kind=wp), intent (out) :: xn0(0:,0:), f0
real (kind=wp), intent (out), optional :: gf0(0:,0:), g2f0(0:,0:)
!-----------------------------------------------------------------------
integer :: k
read (iun,*)
do k = 0, size(xn0,2)-1
 read (iun,*) xn0(:,k)
enddo
read (iun,*) f0
if (present(gf0)) then
 do k = 0, size(gf0,2)-1
  read (iun,*) gf0(:,k)
 enddo
endif
if (present(g2f0)) then
 do k = 0, size(g2f0,2)-1
  read (iun,*) g2f0(:,k)
 enddo
endif
return
END SUBROUTINE pes2_readf_old
