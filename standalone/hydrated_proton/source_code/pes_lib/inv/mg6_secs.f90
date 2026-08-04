SUBROUTINE mg6_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We don't have the secondaries at degree 9; Magma ran out of
!! memory (4GB max) computing v(315:642).
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg6_nsc(mxd)) then
 stop 'mg6_secs: bad dimensions'
endif
call mg6_isecs (mxd, r, pv)
v(0) = 1
if (3.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (4.le.mxd) then
 v(2:8) = pv(1:7)
endif
if (5.le.mxd) then
 v(9:26) = pv(8:25)
endif
if (6.le.mxd) then
 v(27) = pv(0)*pv(0)
 v(28:66) = pv(26:64)
endif
if (7.le.mxd) then
 v(67) = pv(0)*pv(1)
 v(68) = pv(0)*pv(2)
 v(69) = pv(0)*pv(3)
 v(70) = pv(0)*pv(4)
 v(71) = pv(0)*pv(5)
 v(72) = pv(0)*pv(6)
 v(73) = pv(0)*pv(7)
 v(74:147) = pv(65:138)
endif
if (8.le.mxd) then
 v(148) = pv(1)*pv(1)
 v(149) = pv(1)*pv(2)
 v(150) = pv(2)*pv(2)
 v(151) = pv(1)*pv(3)
 v(152) = pv(2)*pv(3)
 v(153) = pv(3)*pv(3)
 v(154) = pv(1)*pv(4)
 v(155) = pv(2)*pv(4)
 v(156) = pv(3)*pv(4)
 v(157) = pv(4)*pv(4)
 v(158) = pv(1)*pv(5)
 v(159) = pv(2)*pv(5)
 v(160) = pv(3)*pv(5)
 v(161) = pv(4)*pv(5)
 v(162) = pv(5)*pv(5)
 v(163) = pv(1)*pv(6)
 v(164) = pv(2)*pv(6)
 v(165) = pv(3)*pv(6)
 v(166) = pv(4)*pv(6)
 v(167) = pv(5)*pv(6)
 v(168) = pv(6)*pv(6)
 v(169) = pv(1)*pv(7)
 v(170) = pv(2)*pv(7)
 v(171) = pv(3)*pv(7)
 v(172) = pv(4)*pv(7)
 v(173) = pv(5)*pv(7)
 v(174) = pv(6)*pv(7)
 v(175) = pv(7)*pv(7)
 v(176) = pv(0)*pv(8)
 v(177) = pv(0)*pv(9)
 v(178) = pv(0)*pv(10)
 v(179) = pv(0)*pv(11)
 v(180) = pv(0)*pv(12)
 v(181) = pv(0)*pv(13)
 v(182) = pv(0)*pv(14)
 v(183) = pv(0)*pv(15)
 v(184) = pv(0)*pv(16)
 v(185) = pv(0)*pv(17)
 v(186) = pv(0)*pv(18)
 v(187) = pv(0)*pv(19)
 v(188) = pv(0)*pv(20)
 v(189) = pv(0)*pv(21)
 v(190) = pv(0)*pv(22)
 v(191) = pv(0)*pv(23)
 v(192) = pv(0)*pv(24)
 v(193) = pv(0)*pv(25)
 v(194:314) = pv(139:259)
endif
if (9.le.mxd) then
 stop 'mg6: degree 9 not implemented'
endif
return
END SUBROUTINE mg6_secs
