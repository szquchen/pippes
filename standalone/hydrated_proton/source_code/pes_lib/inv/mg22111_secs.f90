SUBROUTINE mg22111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg22111_nsc(mxd)) then
 stop 'mg22111_secs: bad dimensions'
endif
call mg22111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:12) = pv(0:11)
endif
if (3.le.mxd) then
 v(13:28) = pv(12:27)
endif
if (4.le.mxd) then
 v(29) = pv(0)*pv(1)
 v(30) = pv(1)*pv(2)
 v(31) = pv(1)*pv(3)
 v(32) = pv(0)*pv(4)
 v(33) = pv(2)*pv(4)
 v(34) = pv(3)*pv(4)
 v(35) = pv(0)*pv(5)
 v(36) = pv(2)*pv(5)
 v(37) = pv(3)*pv(5)
 v(38) = pv(1)*pv(6)
 v(39) = pv(4)*pv(6)
 v(40) = pv(5)*pv(6)
 v(41) = pv(1)*pv(7)
 v(42) = pv(4)*pv(7)
 v(43) = pv(5)*pv(7)
 v(44) = pv(0)*pv(8)
 v(45) = pv(1)*pv(8)
 v(46) = pv(4)*pv(8)
 v(47) = pv(5)*pv(8)
 v(48) = pv(0)*pv(9)
 v(49) = pv(2)*pv(9)
 v(50) = pv(3)*pv(9)
 v(51) = pv(6)*pv(9)
 v(52) = pv(7)*pv(9)
 v(53) = pv(8)*pv(9)
 v(54) = pv(0)*pv(10)
 v(55) = pv(2)*pv(10)
 v(56) = pv(3)*pv(10)
 v(57) = pv(6)*pv(10)
 v(58) = pv(7)*pv(10)
 v(59) = pv(8)*pv(10)
 v(60) = pv(0)*pv(11)
 v(61) = pv(1)*pv(11)
 v(62) = pv(2)*pv(11)
 v(63) = pv(3)*pv(11)
 v(64) = pv(6)*pv(11)
 v(65) = pv(7)*pv(11)
 v(66) = pv(8)*pv(11)
endif
if (5.le.mxd) then
 v(67) = pv(0)*pv(16)
 v(68) = pv(0)*pv(17)
 v(69) = pv(1)*pv(18)
 v(70) = pv(1)*pv(19)
 v(71) = pv(0)*pv(20)
 v(72) = pv(1)*pv(20)
 v(73) = pv(0)*pv(21)
 v(74) = pv(2)*pv(21)
 v(75) = pv(3)*pv(21)
 v(76) = pv(0)*pv(22)
 v(77) = pv(2)*pv(22)
 v(78) = pv(3)*pv(22)
 v(79) = pv(0)*pv(23)
 v(80) = pv(1)*pv(23)
 v(81) = pv(2)*pv(23)
 v(82) = pv(3)*pv(23)
 v(83) = pv(1)*pv(24)
 v(84) = pv(4)*pv(24)
 v(85) = pv(5)*pv(24)
 v(86) = pv(1)*pv(25)
 v(87) = pv(4)*pv(25)
 v(88) = pv(5)*pv(25)
 v(89) = pv(0)*pv(26)
 v(90) = pv(1)*pv(26)
 v(91) = pv(4)*pv(26)
 v(92) = pv(5)*pv(26)
 v(93) = pv(0)*pv(27)
 v(94) = pv(1)*pv(27)
 v(95) = pv(2)*pv(27)
 v(96) = pv(3)*pv(27)
 v(97) = pv(4)*pv(27)
 v(98) = pv(5)*pv(27)
endif
if (6.le.mxd) then
 v(99) = pv(0)*pv(1)*pv(8)
 v(100) = pv(0)*pv(4)*pv(8)
 v(101) = pv(0)*pv(5)*pv(8)
 v(102) = pv(0)*pv(8)*pv(9)
 v(103) = pv(0)*pv(8)*pv(10)
 v(104) = pv(0)*pv(1)*pv(11)
 v(105) = pv(1)*pv(2)*pv(11)
 v(106) = pv(1)*pv(3)*pv(11)
 v(107) = pv(1)*pv(6)*pv(11)
 v(108) = pv(1)*pv(7)*pv(11)
 v(109) = pv(0)*pv(8)*pv(11)
 v(110) = pv(1)*pv(8)*pv(11)
endif
if (7.le.mxd) then
 stop 'mg22111: degree 7 not implemented'
endif
return
END SUBROUTINE mg22111_secs
