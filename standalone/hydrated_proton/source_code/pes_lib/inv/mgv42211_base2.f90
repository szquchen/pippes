SUBROUTINE mgv42211_base2 (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer, parameter :: k0=nkj(0)+nkj(1)
integer :: k
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mgv42211_base2: bad size r'
else if (size(w,1).ne.nkj(2).or.size(w,2).ne.mgv42211_nb2(mxd)) then
 stop 'mgv42211_base2: bad size w'
endif
do k = 0, nkj(2)-1
 call mg421111_base (mxd, &
   r(mgv42211_iord(:,k0+k),mgv42211_iord(:,k0+k)), w(k,:))
enddo
return
END SUBROUTINE mgv42211_base2
