SUBROUTINE mg71_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg71_nsc(mxd)) then
 stop 'mg71_secs: bad dimensions'
endif
call mg71_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (3.le.mxd) then
 v(2:6) = pv(1:5)
endif
if (4.le.mxd) then
 v(7) = pv(0)*pv(0)
 v(8:32) = pv(6:30)
endif
if (5.le.mxd) then
 v(33) = pv(0)*pv(1)
 v(34) = pv(0)*pv(2)
 v(35) = pv(0)*pv(3)
 v(36) = pv(0)*pv(4)
 v(37) = pv(0)*pv(5)
 v(38:120) = pv(31:113)
endif
if (6.le.mxd) then
 v(121) = pv(0)*pv(0)*pv(0)
 v(122) = pv(1)*pv(1)
 v(123) = pv(1)*pv(2)
 v(124) = pv(2)*pv(2)
 v(125) = pv(1)*pv(3)
 v(126) = pv(2)*pv(3)
 v(127) = pv(3)*pv(3)
 v(128) = pv(1)*pv(4)
 v(129) = pv(2)*pv(4)
 v(130) = pv(3)*pv(4)
 v(131) = pv(4)*pv(4)
 v(132) = pv(1)*pv(5)
 v(133) = pv(2)*pv(5)
 v(134) = pv(3)*pv(5)
 v(135) = pv(4)*pv(5)
 v(136) = pv(5)*pv(5)
 v(137) = pv(0)*pv(6)
 v(138) = pv(0)*pv(7)
 v(139) = pv(0)*pv(8)
 v(140) = pv(0)*pv(9)
 v(141) = pv(0)*pv(10)
 v(142) = pv(0)*pv(11)
 v(143) = pv(0)*pv(12)
 v(144) = pv(0)*pv(13)
 v(145) = pv(0)*pv(14)
 v(146) = pv(0)*pv(15)
 v(147) = pv(0)*pv(16)
 v(148) = pv(0)*pv(17)
 v(149) = pv(0)*pv(18)
 v(150) = pv(0)*pv(19)
 v(151) = pv(0)*pv(20)
 v(152) = pv(0)*pv(21)
 v(153) = pv(0)*pv(22)
 v(154) = pv(0)*pv(23)
 v(155) = pv(0)*pv(24)
 v(156) = pv(0)*pv(25)
 v(157) = pv(0)*pv(26)
 v(158) = pv(0)*pv(27)
 v(159) = pv(0)*pv(28)
 v(160) = pv(0)*pv(29)
 v(161) = pv(0)*pv(30)
 v(162:421) = pv(114:373)
endif
if (7.le.mxd) then
 stop 'mg71: degree 7 not implemented'
endif
return
END SUBROUTINE mg71_secs
