SUBROUTINE mgx_gens (nkj, ind, iord)
integer, intent (in) :: nkj(0:), ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
integer :: nk, i
integer, allocatable :: iord0(:)
real (kind=wp), allocatable :: x(:), d(:,:)
nk = sum(nkj)
allocate (iord0(0:nk-1))
allocate (x(0:nk*(nk-1)/2-1), d(0:nk-1,0:nk-1))
if (size(iord).ne.nk*(nk-1)/2) then
 stop 'mgx_gens: bad size iord'
endif
call mgx_gens2d (nkj, ind, iord0)
do i = 0, nk*(nk-1)/2-1
 x(i) = i
enddo
call mgx_mk2d (nkj, x, d)
call mgx_mk1d (nkj, d(iord0,iord0), x)
do i = 0, nk*(nk-1)/2-1
 iord(i) = x(i)
enddo
return
END SUBROUTINE mgx_gens
