SUBROUTINE mgv11111_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
real (kind=wp) :: wx(0:mg11111_nb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv11111_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv11111_nb(mxd)) then
 stop 'mgv11111_base: bad size w'
endif
w = 0
call mg11111_base (mxd, r, wx)
w(0,0:size(wx)-1) = wx
w(nkj(0),size(wx):2*size(wx)-1) = wx
w(sum(nkj(0:1)),2*size(wx):3*size(wx)-1) = wx
w(sum(nkj(0:2)),3*size(wx):4*size(wx)-1) = wx
w(sum(nkj(0:3)),4*size(wx):5*size(wx)-1) = wx
return
END SUBROUTINE mgv11111_base
