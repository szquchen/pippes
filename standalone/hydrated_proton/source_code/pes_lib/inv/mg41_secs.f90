SUBROUTINE mg41_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg41_nsc(mxd)) then
 stop 'mg41_secs: bad dimensions'
endif
call mg41_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (3.le.mxd) then
 v(2:6) = pv(1:5)
endif
if (4.le.mxd) then
 v(7) = pv(0)*pv(0)
 v(8:15) = pv(6:13)
endif
if (5.le.mxd) then
 v(16) = pv(0)*pv(1)
 v(17) = pv(0)*pv(2)
 v(18) = pv(0)*pv(3)
 v(19) = pv(0)*pv(4)
 v(20) = pv(0)*pv(5)
 v(21:27) = pv(14:20)
endif
if (6.le.mxd) then
 v(28) = pv(1)*pv(1)
 v(29) = pv(1)*pv(2)
 v(30) = pv(1)*pv(3)
 v(31) = pv(1)*pv(4)
 v(32) = pv(3)*pv(4)
 v(33) = pv(1)*pv(5)
 v(34) = pv(2)*pv(5)
 v(35) = pv(3)*pv(5)
 v(36) = pv(4)*pv(5)
 v(37) = pv(5)*pv(5)
 v(38) = pv(0)*pv(6)
 v(39) = pv(0)*pv(7)
 v(40) = pv(0)*pv(8)
 v(41) = pv(0)*pv(9)
 v(42) = pv(0)*pv(10)
 v(43) = pv(0)*pv(11)
 v(44) = pv(0)*pv(12)
 v(45) = pv(0)*pv(13)
endif
if (7.le.mxd) then
 v(46) = pv(1)*pv(8)
 v(47) = pv(1)*pv(9)
 v(48) = pv(1)*pv(10)
 v(49) = pv(3)*pv(10)
 v(50) = pv(1)*pv(11)
 v(51) = pv(2)*pv(11)
 v(52) = pv(3)*pv(11)
 v(53) = pv(1)*pv(12)
 v(54) = pv(3)*pv(12)
 v(55) = pv(1)*pv(13)
 v(56) = pv(2)*pv(13)
 v(57) = pv(3)*pv(13)
 v(58) = pv(4)*pv(13)
 v(59) = pv(5)*pv(13)
 v(60) = pv(0)*pv(14)
 v(61) = pv(0)*pv(15)
 v(62) = pv(0)*pv(16)
 v(63) = pv(0)*pv(17)
 v(64) = pv(0)*pv(18)
 v(65) = pv(0)*pv(19)
 v(66) = pv(0)*pv(20)
endif
if (8.le.mxd) then
 v(67) = pv(11)*pv(13)
 v(68) = pv(12)*pv(13)
 v(69) = pv(13)*pv(13)
 v(70) = pv(1)*pv(15)
 v(71) = pv(4)*pv(15)
 v(72) = pv(5)*pv(15)
 v(73) = pv(1)*pv(16)
 v(74) = pv(2)*pv(16)
 v(75) = pv(3)*pv(16)
 v(76) = pv(4)*pv(16)
 v(77) = pv(5)*pv(16)
 v(78) = pv(1)*pv(17)
 v(79) = pv(3)*pv(17)
 v(80) = pv(5)*pv(17)
 v(81) = pv(1)*pv(18)
 v(82) = pv(2)*pv(18)
 v(83) = pv(3)*pv(18)
 v(84) = pv(4)*pv(18)
 v(85) = pv(5)*pv(18)
 v(86) = pv(1)*pv(19)
 v(87) = pv(3)*pv(19)
 v(88) = pv(1)*pv(20)
 v(89) = pv(2)*pv(20)
 v(90) = pv(3)*pv(20)
endif
if (9.le.mxd) then
 v(91) = pv(0)*pv(0)*pv(20)
 v(92) = pv(6)*pv(14)
 v(93) = pv(6)*pv(16)
 v(94) = pv(8)*pv(16)
 v(95) = pv(9)*pv(16)
 v(96) = pv(12)*pv(16)
 v(97) = pv(6)*pv(17)
 v(98) = pv(8)*pv(17)
 v(99) = pv(9)*pv(17)
 v(100) = pv(6)*pv(18)
 v(101) = pv(7)*pv(18)
 v(102) = pv(8)*pv(18)
 v(103) = pv(9)*pv(18)
 v(104) = pv(11)*pv(18)
 v(105) = pv(12)*pv(18)
 v(106) = pv(13)*pv(18)
 v(107) = pv(6)*pv(19)
 v(108) = pv(7)*pv(19)
 v(109) = pv(8)*pv(19)
 v(110) = pv(9)*pv(19)
 v(111) = pv(13)*pv(19)
 v(112) = pv(6)*pv(20)
 v(113) = pv(7)*pv(20)
 v(114) = pv(8)*pv(20)
 v(115) = pv(9)*pv(20)
 v(116) = pv(13)*pv(20)
endif
return
END SUBROUTINE mg41_secs
