SUBROUTINE mgv1_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
real (kind=wp) :: wx(0:mg1_nb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv1_base: bad size r'
else if (size(w,1).ne.nk.or.size(w,2).ne.mgv1_nb(mxd)) then
 stop 'mgv1_base: bad size w'
endif
w = 0
call mg1_base (mxd, r, wx)
w(0,0:size(wx)-1) = wx
return
END SUBROUTINE mgv1_base
