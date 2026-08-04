SUBROUTINE mgv433_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: l1
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv433_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv433_nb(mxd)) then
 stop 'mgv433_base: bad size w'
endif
w = 0
call mgv433_base0 (mxd, r, w(0:nkj(0)-1,0:mgv433_nb0(mxd)-1))
l1 = mgv433_nb0(mxd)
call mgv433_base1 (mxd, r, &
  w(nkj(0):sum(nkj(0:1))-1,l1:l1+mgv433_nb1(mxd)-1))
l1 = l1+mgv433_nb1(mxd)
call mgv433_base2 (mxd, r, &
  w(sum(nkj(0:1)):sum(nkj(0:2))-1,l1:l1+mgv433_nb2(mxd)-1))
return
END SUBROUTINE mgv433_base
