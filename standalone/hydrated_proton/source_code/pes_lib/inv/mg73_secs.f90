SUBROUTINE mg73_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg73_nsc(mxd)) then
 stop 'mg73_secs: bad dimensions'
endif
call mg73_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:2) = pv(0:1)
endif
if (3.le.mxd) then
 v(3:18) = pv(2:17)
endif
if (4.le.mxd) then
 v(19) = pv(0)*pv(0)
 v(20) = pv(0)*pv(1)
 v(21) = pv(1)*pv(1)
 v(22:92) = pv(18:88)
endif
if (5.le.mxd) then
 v(93) = pv(0)*pv(2)
 v(94) = pv(1)*pv(2)
 v(95) = pv(0)*pv(3)
 v(96) = pv(1)*pv(3)
 v(97) = pv(0)*pv(4)
 v(98) = pv(1)*pv(4)
 v(99) = pv(0)*pv(5)
 v(100) = pv(1)*pv(5)
 v(101) = pv(0)*pv(6)
 v(102) = pv(1)*pv(6)
 v(103) = pv(0)*pv(7)
 v(104) = pv(1)*pv(7)
 v(105) = pv(0)*pv(8)
 v(106) = pv(1)*pv(8)
 v(107) = pv(0)*pv(9)
 v(108) = pv(1)*pv(9)
 v(109) = pv(0)*pv(10)
 v(110) = pv(1)*pv(10)
 v(111) = pv(0)*pv(11)
 v(112) = pv(1)*pv(11)
 v(113) = pv(0)*pv(12)
 v(114) = pv(1)*pv(12)
 v(115) = pv(0)*pv(13)
 v(116) = pv(1)*pv(13)
 v(117) = pv(0)*pv(14)
 v(118) = pv(0)*pv(15)
 v(119) = pv(1)*pv(15)
 v(120) = pv(0)*pv(16)
 v(121) = pv(1)*pv(16)
 v(122) = pv(0)*pv(17)
 v(123:406) = pv(89:372)
endif
if (6.le.mxd) then
!! Should still do degree 6; no problem for Magma
 stop 'mg73: degree 6 not implemented'
endif
return
END SUBROUTINE mg73_secs
