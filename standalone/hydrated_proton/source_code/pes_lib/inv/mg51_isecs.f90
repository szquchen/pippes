SUBROUTINE mg51_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=5, m2=m*(m-1), m3=m2*(m-2), mm=m3*(m-3)
integer :: i0, i1, i2, i3, j0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5, d6, d7, d8
if (size(r).ne.nk*nk) then
 stop 'mg51_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5, d6, d7, d8)
pv = 0
j0 = m
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg51_i1 (mxd, m2)
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg51_i2 (mxd, m3)
     do i3 = 0, m-1
      if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
       call mg51_ii (mxd, mm)
      endif
     enddo
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg51_i1 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
endif
if (3.le.mxd) then
 pv(3) = pv(3)+d2(i0,i1)*d(i0,j0)/mu
 pv(6) = pv(6)+d(i0,i1)*d2(i0,j0)/mu
 pv(7) = pv(7)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
endif
if (4.le.mxd) then
 pv(13) = pv(13)+d3(i0,i1)*d(i0,j0)/mu
 pv(20) = pv(20)+d2(i0,i1)*d2(i0,j0)/mu
 pv(23) = pv(23)+d(i0,i1)*d3(i0,j0)/mu
 pv(24) = pv(24)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(26) = pv(26)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
endif
if (5.le.mxd) then
 pv(35) = pv(35)+d4(i0,i1)*d(i0,j0)/mu
 pv(48) = pv(48)+d3(i0,i1)*d2(i0,j0)/mu
 pv(55) = pv(55)+d2(i0,i1)*d3(i0,j0)/mu
 pv(58) = pv(58)+d(i0,i1)*d4(i0,j0)/mu
 pv(59) = pv(59)+d3(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(63) = pv(63)+d2(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(65) = pv(65)+d(i0,i1)*d3(i0,j0)*d(i1,j0)/mu
endif
if (6.le.mxd) then
 pv(78) = pv(78)+d5(i0,i1)*d(i0,j0)/mu
 pv(96) = pv(96)+d4(i0,i1)*d2(i0,j0)/mu
 pv(108) = pv(108)+d3(i0,i1)*d3(i0,j0)/mu
 pv(114) = pv(114)+d2(i0,i1)*d4(i0,j0)/mu
 pv(116) = pv(116)+d4(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(119) = pv(119)+d3(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(122) = pv(122)+d2(i0,i1)*d3(i0,j0)*d(i1,j0)/mu
 pv(124) = pv(124)+d(i0,i1)*d4(i0,j0)*d(i1,j0)/mu
endif
if (7.le.mxd) then
 pv(125) = pv(125)+d7(i0,i1)/mu
 pv(138) = pv(138)+d6(i0,i1)*d(i0,j0)/mu
 pv(157) = pv(157)+d5(i0,i1)*d2(i0,j0)/mu
 pv(170) = pv(170)+d4(i0,i1)*d3(i0,j0)/mu
 pv(178) = pv(178)+d3(i0,i1)*d4(i0,j0)/mu
 pv(182) = pv(182)+d5(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(183) = pv(183)+d3(i0,i1)*d3(i0,j0)*d(i1,j0)/mu
 pv(184) = pv(184)+d2(i0,i1)*d4(i0,j0)*d(i1,j0)/mu
 pv(186) = pv(186)+d(i0,i1)*d4(i0,j0)*d2(i1,j0)/mu
endif
if (8.le.mxd) then
 pv(187) = pv(187)+d8(i0,i1)/mu
 pv(191) = pv(191)+d7(i0,i1)*d(i0,j0)/mu
 pv(197) = pv(197)+d6(i0,i1)*d2(i0,j0)/mu
endif
END SUBROUTINE mg51_i1
SUBROUTINE mg51_i2 (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(1) = pv(1)+d2(i0,i1)*d(i0,i2)/mu
 pv(2) = pv(2)+d(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(4) = pv(4)+d(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(5) = pv(5)+d(i0,i1)*d(i1,i2)*d(i0,j0)/mu
endif
if (4.le.mxd) then
 pv(8) = pv(8)+d3(i0,i1)*d(i0,i2)/mu
 pv(9) = pv(9)+d2(i0,i1)*d2(i0,i2)/mu
 pv(10) = pv(10)+d2(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(14) = pv(14)+d2(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(15) = pv(15)+d2(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(16) = pv(16)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(17) = pv(17)+d(i0,i1)*d2(i1,i2)*d(i0,j0)/mu
 pv(21) = pv(21)+d(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(22) = pv(22)+d(i0,i1)*d(i1,i2)*d2(i0,j0)/mu
 pv(25) = pv(25)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
endif
if (5.le.mxd) then
 pv(27) = pv(27)+d4(i0,i1)*d(i0,i2)/mu
 pv(28) = pv(28)+d3(i0,i1)*d2(i0,i2)/mu
 pv(29) = pv(29)+d3(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(30) = pv(30)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(36) = pv(36)+d3(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(37) = pv(37)+d2(i0,i1)*d2(i0,i2)*d(i0,j0)/mu
 pv(38) = pv(38)+d3(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(39) = pv(39)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(40) = pv(40)+d2(i0,i1)*d2(i1,i2)*d(i0,j0)/mu
 pv(41) = pv(41)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,j0)/mu
 pv(42) = pv(42)+d(i0,i1)*d3(i1,i2)*d(i0,j0)/mu
 pv(49) = pv(49)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(50) = pv(50)+d2(i0,i1)*d(i1,i2)*d2(i0,j0)/mu
 pv(51) = pv(51)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,j0)/mu
 pv(52) = pv(52)+d(i0,i1)*d2(i1,i2)*d2(i0,j0)/mu
 pv(56) = pv(56)+d(i0,i1)*d(i0,i2)*d3(i0,j0)/mu
 pv(57) = pv(57)+d(i0,i1)*d(i1,i2)*d3(i0,j0)/mu
 pv(60) = pv(60)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(61) = pv(61)+d(i0,i1)*d2(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(62) = pv(62)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(64) = pv(64)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
endif
if (6.le.mxd) then
 pv(66) = pv(66)+d5(i0,i1)*d(i0,i2)/mu
 pv(67) = pv(67)+d4(i0,i1)*d2(i0,i2)/mu
 pv(68) = pv(68)+d3(i0,i1)*d3(i0,i2)/mu
 pv(69) = pv(69)+d4(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(70) = pv(70)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(71) = pv(71)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)/mu
 pv(79) = pv(79)+d4(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(80) = pv(80)+d3(i0,i1)*d2(i0,i2)*d(i0,j0)/mu
 pv(81) = pv(81)+d4(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(82) = pv(82)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(83) = pv(83)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(84) = pv(84)+d3(i0,i1)*d2(i1,i2)*d(i0,j0)/mu
 pv(85) = pv(85)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,j0)/mu
 pv(86) = pv(86)+d2(i0,i1)*d3(i1,i2)*d(i0,j0)/mu
 pv(87) = pv(87)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,j0)/mu
 pv(88) = pv(88)+d(i0,i1)*d4(i1,i2)*d(i0,j0)/mu
 pv(97) = pv(97)+d3(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(98) = pv(98)+d2(i0,i1)*d2(i0,i2)*d2(i0,j0)/mu
 pv(99) = pv(99)+d3(i0,i1)*d(i1,i2)*d2(i0,j0)/mu
 pv(100) = pv(100)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,j0)/mu
 pv(101) = pv(101)+d2(i0,i1)*d2(i1,i2)*d2(i0,j0)/mu
 pv(102) = pv(102)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,j0)/mu
 pv(103) = pv(103)+d(i0,i1)*d3(i1,i2)*d2(i0,j0)/mu
 pv(109) = pv(109)+d2(i0,i1)*d(i0,i2)*d3(i0,j0)/mu
 pv(110) = pv(110)+d2(i0,i1)*d(i1,i2)*d3(i0,j0)/mu
 pv(111) = pv(111)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,j0)/mu
 pv(112) = pv(112)+d(i0,i1)*d2(i1,i2)*d3(i0,j0)/mu
 pv(115) = pv(115)+d(i0,i1)*d(i1,i2)*d4(i0,j0)/mu
 pv(117) = pv(117)+d3(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(118) = pv(118)+d(i0,i1)*d3(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(120) = pv(120)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(121) = pv(121)+d(i0,i1)*d2(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(123) = pv(123)+d(i0,i1)*d(i0,i2)*d3(i0,j0)*d(i1,j0)/mu
endif
if (7.le.mxd) then
 pv(126) = pv(126)+d6(i0,i1)*d(i0,i2)/mu
 pv(127) = pv(127)+d5(i0,i1)*d2(i0,i2)/mu
 pv(128) = pv(128)+d4(i0,i1)*d3(i0,i2)/mu
 pv(129) = pv(129)+d5(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(130) = pv(130)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(131) = pv(131)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)/mu
 pv(132) = pv(132)+d3(i0,i1)*d2(i0,i2)*d2(i1,i2)/mu
 pv(139) = pv(139)+d5(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(140) = pv(140)+d4(i0,i1)*d2(i0,i2)*d(i0,j0)/mu
 pv(141) = pv(141)+d3(i0,i1)*d3(i0,i2)*d(i0,j0)/mu
 pv(142) = pv(142)+d5(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(143) = pv(143)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(144) = pv(144)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(145) = pv(145)+d4(i0,i1)*d2(i1,i2)*d(i0,j0)/mu
 pv(146) = pv(146)+d3(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,j0)/mu
 pv(147) = pv(147)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,j0)/mu
 pv(148) = pv(148)+d3(i0,i1)*d3(i1,i2)*d(i0,j0)/mu
 pv(149) = pv(149)+d2(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,j0)/mu
 pv(150) = pv(150)+d2(i0,i1)*d4(i1,i2)*d(i0,j0)/mu
 pv(151) = pv(151)+d(i0,i1)*d(i0,i2)*d4(i1,i2)*d(i0,j0)/mu
 pv(152) = pv(152)+d(i0,i1)*d5(i1,i2)*d(i0,j0)/mu
 pv(158) = pv(158)+d4(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(159) = pv(159)+d3(i0,i1)*d2(i0,i2)*d2(i0,j0)/mu
 pv(160) = pv(160)+d4(i0,i1)*d(i1,i2)*d2(i0,j0)/mu
 pv(161) = pv(161)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,j0)/mu
 pv(162) = pv(162)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,j0)/mu
 pv(163) = pv(163)+d3(i0,i1)*d2(i1,i2)*d2(i0,j0)/mu
 pv(164) = pv(164)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,j0)/mu
 pv(165) = pv(165)+d2(i0,i1)*d3(i1,i2)*d2(i0,j0)/mu
 pv(166) = pv(166)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,j0)/mu
 pv(167) = pv(167)+d(i0,i1)*d4(i1,i2)*d2(i0,j0)/mu
 pv(171) = pv(171)+d3(i0,i1)*d(i0,i2)*d3(i0,j0)/mu
 pv(172) = pv(172)+d2(i0,i1)*d2(i0,i2)*d3(i0,j0)/mu
 pv(173) = pv(173)+d3(i0,i1)*d(i1,i2)*d3(i0,j0)/mu
 pv(174) = pv(174)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,j0)/mu
 pv(175) = pv(175)+d2(i0,i1)*d2(i1,i2)*d3(i0,j0)/mu
 pv(176) = pv(176)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d3(i0,j0)/mu
 pv(177) = pv(177)+d(i0,i1)*d3(i1,i2)*d3(i0,j0)/mu
 pv(179) = pv(179)+d2(i0,i1)*d(i1,i2)*d4(i0,j0)/mu
 pv(180) = pv(180)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d4(i0,j0)/mu
 pv(181) = pv(181)+d(i0,i1)*d2(i1,i2)*d4(i0,j0)/mu
 pv(185) = pv(185)+d(i0,i1)*d(i1,i2)*d4(i0,j0)*d(i1,j0)/mu
endif
if (8.le.mxd) then
 pv(188) = pv(188)+d7(i0,i1)*d(i0,i2)/mu
 pv(189) = pv(189)+d6(i0,i1)*d2(i0,i2)/mu
 pv(190) = pv(190)+d5(i0,i1)*d3(i0,i2)/mu
 pv(192) = pv(192)+d6(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(193) = pv(193)+d5(i0,i1)*d2(i0,i2)*d(i0,j0)/mu
 pv(194) = pv(194)+d5(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(195) = pv(195)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(196) = pv(196)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(198) = pv(198)+d4(i0,i1)*d2(i0,i2)*d2(i0,j0)/mu
endif
END SUBROUTINE mg51_i2
SUBROUTINE mg51_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(11) = pv(11)+d2(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(12) = pv(12)+d2(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(18) = pv(18)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(19) = pv(19)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
endif
if (5.le.mxd) then
 pv(31) = pv(31)+d3(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(32) = pv(32)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(33) = pv(33)+d3(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(34) = pv(34)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(43) = pv(43)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(44) = pv(44)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(45) = pv(45)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(46) = pv(46)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(47) = pv(47)+d(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,j0)/mu
 pv(53) = pv(53)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(54) = pv(54)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d2(i0,j0)/mu
endif
if (6.le.mxd) then
 pv(72) = pv(72)+d4(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(73) = pv(73)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(74) = pv(74)+d4(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(75) = pv(75)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(76) = pv(76)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(77) = pv(77)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(89) = pv(89)+d3(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(90) = pv(90)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(91) = pv(91)+d3(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(92) = pv(92)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(93) = pv(93)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(94) = pv(94)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(95) = pv(95)+d2(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,j0)/mu
 pv(104) = pv(104)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(105) = pv(105)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(106) = pv(106)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(107) = pv(107)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(113) = pv(113)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d3(i0,j0)/mu
endif
if (7.le.mxd) then
 pv(133) = pv(133)+d5(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(134) = pv(134)+d4(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(135) = pv(135)+d3(i0,i1)*d3(i0,i2)*d(i0,i3)/mu
 pv(136) = pv(136)+d5(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(137) = pv(137)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(153) = pv(153)+d4(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(154) = pv(154)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(155) = pv(155)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(156) = pv(156)+d3(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,j0)/mu
 pv(168) = pv(168)+d3(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(169) = pv(169)+d2(i0,i1)*d(i1,i2)*d2(i0,i3)*d2(i0,j0)/mu
endif
END SUBROUTINE mg51_ii
END SUBROUTINE mg51_isecs
