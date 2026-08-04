SUBROUTINE mgv1111111111_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
real (kind=wp) :: wx(0:mg1111111111_nb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv1111111111_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv1111111111_nb(mxd)) then
 stop 'mgv1111111111_base: bad size w'
endif
w = 0
call mg1111111111_base (mxd, r, wx)
w(0,0:size(wx)-1) = wx
w(nkj(0),size(wx):2*size(wx)-1) = wx
w(sum(nkj(0:1)),2*size(wx):3*size(wx)-1) = wx
w(sum(nkj(0:2)),3*size(wx):4*size(wx)-1) = wx
w(sum(nkj(0:3)),4*size(wx):5*size(wx)-1) = wx
w(sum(nkj(0:4)),5*size(wx):6*size(wx)-1) = wx
w(sum(nkj(0:5)),6*size(wx):7*size(wx)-1) = wx
w(sum(nkj(0:6)),7*size(wx):8*size(wx)-1) = wx
w(sum(nkj(0:7)),8*size(wx):9*size(wx)-1) = wx
w(sum(nkj(0:8)),9*size(wx):10*size(wx)-1) = wx
return
END SUBROUTINE mgv1111111111_base
