SUBROUTINE mgv62_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: l1
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv62_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv62_nb(mxd)) then
 stop 'mgv62_base: bad size w'
endif
w = 0
call mgv62_base0 (mxd, r, w(0:nkj(0)-1,0:mgv62_nb0(mxd)-1))
l1 = mgv62_nb0(mxd)
call mgv62_base1 (mxd, r, w(nkj(0):sum(nkj(0:1))-1,l1:l1+mgv62_nb1(mxd)-1))
return
END SUBROUTINE mgv62_base
