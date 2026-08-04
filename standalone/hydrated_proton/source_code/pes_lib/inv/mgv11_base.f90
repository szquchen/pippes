SUBROUTINE mgv11_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
real (kind=wp) :: wx(0:mg11_nb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv11_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv11_nb(mxd)) then
 stop 'mgv11_base: bad size w'
endif
w = 0
call mg11_base (mxd, r, wx)
w(0,0:size(wx)-1) = wx
w(nkj(0),size(wx):2*size(wx)-1) = wx
return
END SUBROUTINE mgv11_base
