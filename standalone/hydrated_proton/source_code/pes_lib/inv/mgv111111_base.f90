SUBROUTINE mgv111111_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
real (kind=wp) :: wx(0:mg111111_nb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv111111_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv111111_nb(mxd)) then
 stop 'mgv111111_base: bad size w'
endif
w = 0
call mg111111_base (mxd, r, wx)
w(0,0:size(wx)-1) = wx
w(nkj(0),size(wx):2*size(wx)-1) = wx
w(sum(nkj(0:1)),2*size(wx):3*size(wx)-1) = wx
w(sum(nkj(0:2)),3*size(wx):4*size(wx)-1) = wx
w(sum(nkj(0:3)),4*size(wx):5*size(wx)-1) = wx
w(sum(nkj(0:4)),5*size(wx):6*size(wx)-1) = wx
return
END SUBROUTINE mgv111111_base
