SUBROUTINE mg6_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!! Note: We don't have the secondaries at degree 9; Magma ran out of
!! memory (4GB max) computing v(315:642).
!-----------------------------------------------------------------------
integer, parameter :: m=nk, m2=m*(m-1), m3=m2*(m-2), m4=m3*(m-3), &
  mm=m4*(m-4)
integer :: i0, i1, i2, i3, i4
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5, d6, d7, d8
if (size(r).ne.nk*nk) then
 stop 'mg6_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5, d6, d7, d8)
pv = 0
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg6_i1 (mxd, m2)
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg6_i2 (mxd, m3)
     do i3 = 0, m-1
      if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
       call mg6_i3 (mxd, m4)
       do i4 = 0, m-1
        if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
         call mg6_ii (mxd, mm)
        endif
       enddo
      endif
     enddo
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg6_i1 (mxd, mu)
integer, intent (in) :: mxd, mu
if (7.le.mxd) then
 pv(65) = pv(65)+d7(i0,i1)/mu
endif
if (8.le.mxd) then
 pv(139) = pv(139)+d8(i0,i1)/mu
endif
END SUBROUTINE mg6_i1
SUBROUTINE mg6_i2 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(1) = pv(1)+d2(i0,i1)*d2(i0,i2)/mu
 pv(2) = pv(2)+d2(i0,i1)*d(i0,i2)*d(i1,i2)/mu
endif
if (5.le.mxd) then
 pv(8) = pv(8)+d3(i0,i1)*d2(i0,i2)/mu
 pv(9) = pv(9)+d3(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(10) = pv(10)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
endif
if (6.le.mxd) then
 pv(26) = pv(26)+d5(i0,i1)*d(i0,i2)/mu
 pv(27) = pv(27)+d4(i0,i1)*d2(i0,i2)/mu
 pv(28) = pv(28)+d3(i0,i1)*d3(i0,i2)/mu
 pv(29) = pv(29)+d4(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(30) = pv(30)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(31) = pv(31)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)/mu
endif
if (7.le.mxd) then
 pv(66) = pv(66)+d6(i0,i1)*d(i0,i2)/mu
 pv(67) = pv(67)+d5(i0,i1)*d2(i0,i2)/mu
 pv(68) = pv(68)+d4(i0,i1)*d3(i0,i2)/mu
 pv(69) = pv(69)+d5(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(70) = pv(70)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(71) = pv(71)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)/mu
 pv(72) = pv(72)+d3(i0,i1)*d2(i0,i2)*d2(i1,i2)/mu
endif
if (8.le.mxd) then
 pv(140) = pv(140)+d7(i0,i1)*d(i0,i2)/mu
 pv(141) = pv(141)+d6(i0,i1)*d2(i0,i2)/mu
 pv(142) = pv(142)+d5(i0,i1)*d3(i0,i2)/mu
 pv(143) = pv(143)+d4(i0,i1)*d4(i0,i2)/mu
 pv(144) = pv(144)+d6(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(145) = pv(145)+d5(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(146) = pv(146)+d4(i0,i1)*d3(i0,i2)*d(i1,i2)/mu
 pv(147) = pv(147)+d4(i0,i1)*d2(i0,i2)*d2(i1,i2)/mu
 pv(148) = pv(148)+d3(i0,i1)*d3(i0,i2)*d2(i1,i2)/mu
endif
END SUBROUTINE mg6_i2
SUBROUTINE mg6_i3 (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i1,i2)*d(i0,i3)/mu
endif
if (4.le.mxd) then
 pv(3) = pv(3)+d2(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(4) = pv(4)+d2(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(5) = pv(5)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(6) = pv(6)+d(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(7) = pv(7)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
endif
if (5.le.mxd) then
 pv(11) = pv(11)+d3(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(12) = pv(12)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(13) = pv(13)+d3(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(14) = pv(14)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(15) = pv(15)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(16) = pv(16)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/mu
 pv(17) = pv(17)+d(i0,i1)*d3(i1,i2)*d(i0,i3)/mu
 pv(18) = pv(18)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/mu
 pv(19) = pv(19)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)/mu
 pv(20) = pv(20)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(21) = pv(21)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
endif
if (6.le.mxd) then
 pv(32) = pv(32)+d4(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(33) = pv(33)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(34) = pv(34)+d4(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(35) = pv(35)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(36) = pv(36)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(37) = pv(37)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(38) = pv(38)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/mu
 pv(39) = pv(39)+d2(i0,i1)*d3(i1,i2)*d(i0,i3)/mu
 pv(40) = pv(40)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)/mu
 pv(41) = pv(41)+d(i0,i1)*d4(i1,i2)*d(i0,i3)/mu
 pv(42) = pv(42)+d2(i0,i1)*d2(i0,i2)*d2(i0,i3)/mu
 pv(43) = pv(43)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/mu
 pv(44) = pv(44)+d2(i0,i1)*d2(i1,i2)*d2(i0,i3)/mu
 pv(45) = pv(45)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)/mu
 pv(46) = pv(46)+d(i0,i1)*d3(i1,i2)*d2(i0,i3)/mu
 pv(47) = pv(47)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)/mu
 pv(48) = pv(48)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(49) = pv(49)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(50) = pv(50)+d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(51) = pv(51)+d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(52) = pv(52)+d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(53) = pv(53)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/mu
endif
if (7.le.mxd) then
 pv(73) = pv(73)+d5(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(74) = pv(74)+d4(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(75) = pv(75)+d3(i0,i1)*d3(i0,i2)*d(i0,i3)/mu
 pv(76) = pv(76)+d5(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(77) = pv(77)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(78) = pv(78)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(79) = pv(79)+d4(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(80) = pv(80)+d3(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/mu
 pv(81) = pv(81)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)/mu
 pv(82) = pv(82)+d3(i0,i1)*d3(i1,i2)*d(i0,i3)/mu
 pv(83) = pv(83)+d2(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)/mu
 pv(84) = pv(84)+d2(i0,i1)*d4(i1,i2)*d(i0,i3)/mu
 pv(85) = pv(85)+d(i0,i1)*d(i0,i2)*d4(i1,i2)*d(i0,i3)/mu
 pv(86) = pv(86)+d(i0,i1)*d5(i1,i2)*d(i0,i3)/mu
 pv(87) = pv(87)+d3(i0,i1)*d2(i0,i2)*d2(i0,i3)/mu
 pv(88) = pv(88)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/mu
 pv(89) = pv(89)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)/mu
 pv(90) = pv(90)+d3(i0,i1)*d2(i1,i2)*d2(i0,i3)/mu
 pv(91) = pv(91)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)/mu
 pv(92) = pv(92)+d2(i0,i1)*d3(i1,i2)*d2(i0,i3)/mu
 pv(93) = pv(93)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)/mu
 pv(94) = pv(94)+d(i0,i1)*d4(i1,i2)*d2(i0,i3)/mu
 pv(95) = pv(95)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)/mu
 pv(96) = pv(96)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d3(i0,i3)/mu
 pv(97) = pv(97)+d(i0,i1)*d3(i1,i2)*d3(i0,i3)/mu
 pv(98) = pv(98)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d4(i0,i3)/mu
 pv(99) = pv(99)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(100) = pv(100)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(101) = pv(101)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(102) = pv(102)+d4(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(103) = pv(103)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(104) = pv(104)+d3(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(105) = pv(105)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(106) = pv(106)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(107) = pv(107)+d2(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(108) = pv(108)+d(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(109) = pv(109)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/mu
endif
if (8.le.mxd) then
 pv(149) = pv(149)+d6(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(150) = pv(150)+d5(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(151) = pv(151)+d4(i0,i1)*d3(i0,i2)*d(i0,i3)/mu
 pv(152) = pv(152)+d6(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(153) = pv(153)+d5(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(154) = pv(154)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(155) = pv(155)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(156) = pv(156)+d5(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(157) = pv(157)+d4(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/mu
 pv(158) = pv(158)+d3(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)/mu
 pv(159) = pv(159)+d4(i0,i1)*d3(i1,i2)*d(i0,i3)/mu
 pv(160) = pv(160)+d3(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)/mu
 pv(161) = pv(161)+d2(i0,i1)*d2(i0,i2)*d3(i1,i2)*d(i0,i3)/mu
 pv(162) = pv(162)+d3(i0,i1)*d4(i1,i2)*d(i0,i3)/mu
 pv(163) = pv(163)+d2(i0,i1)*d(i0,i2)*d4(i1,i2)*d(i0,i3)/mu
 pv(164) = pv(164)+d2(i0,i1)*d5(i1,i2)*d(i0,i3)/mu
 pv(165) = pv(165)+d(i0,i1)*d(i0,i2)*d5(i1,i2)*d(i0,i3)/mu
 pv(166) = pv(166)+d(i0,i1)*d6(i1,i2)*d(i0,i3)/mu
 pv(167) = pv(167)+d4(i0,i1)*d2(i0,i2)*d2(i0,i3)/mu
 pv(168) = pv(168)+d3(i0,i1)*d3(i0,i2)*d2(i0,i3)/mu
 pv(169) = pv(169)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/mu
 pv(170) = pv(170)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)/mu
 pv(171) = pv(171)+d4(i0,i1)*d2(i1,i2)*d2(i0,i3)/mu
 pv(172) = pv(172)+d3(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)/mu
 pv(173) = pv(173)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d2(i0,i3)/mu
 pv(174) = pv(174)+d3(i0,i1)*d3(i1,i2)*d2(i0,i3)/mu
 pv(175) = pv(175)+d2(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)/mu
 pv(176) = pv(176)+d2(i0,i1)*d4(i1,i2)*d2(i0,i3)/mu
 pv(177) = pv(177)+d(i0,i1)*d(i0,i2)*d4(i1,i2)*d2(i0,i3)/mu
 pv(178) = pv(178)+d(i0,i1)*d5(i1,i2)*d2(i0,i3)/mu
 pv(179) = pv(179)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)/mu
 pv(180) = pv(180)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d3(i0,i3)/mu
 pv(181) = pv(181)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d3(i0,i3)/mu
 pv(182) = pv(182)+d2(i0,i1)*d3(i1,i2)*d3(i0,i3)/mu
 pv(183) = pv(183)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d3(i0,i3)/mu
 pv(184) = pv(184)+d(i0,i1)*d4(i1,i2)*d3(i0,i3)/mu
 pv(185) = pv(185)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d4(i0,i3)/mu
 pv(186) = pv(186)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d4(i0,i3)/mu
 pv(187) = pv(187)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d5(i0,i3)/mu
 pv(188) = pv(188)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(189) = pv(189)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(190) = pv(190)+d2(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(191) = pv(191)+d(i0,i1)*d4(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(192) = pv(192)+d5(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(193) = pv(193)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(194) = pv(194)+d(i0,i1)*d3(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(195) = pv(195)+d4(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(196) = pv(196)+d3(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(197) = pv(197)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(198) = pv(198)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(199) = pv(199)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(200) = pv(200)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(201) = pv(201)+d3(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(202) = pv(202)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(203) = pv(203)+d2(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(204) = pv(204)+d(i0,i2)*d4(i1,i2)*d2(i0,i3)*d(i1,i3)/mu
 pv(205) = pv(205)+d(i0,i2)*d3(i1,i2)*d3(i0,i3)*d(i1,i3)/mu
 pv(206) = pv(206)+d2(i0,i2)*d2(i1,i2)*d2(i0,i3)*d2(i1,i3)/mu
 pv(207) = pv(207)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/mu
 pv(208) = pv(208)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/mu
 pv(209) = pv(209)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i2,i3)/mu
endif
END SUBROUTINE mg6_i3
SUBROUTINE mg6_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
 pv(22) = pv(22)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(23) = pv(23)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(24) = pv(24)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(25) = pv(25)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
endif
if (6.le.mxd) then
 pv(54) = pv(54)+d3(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(55) = pv(55)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(56) = pv(56)+d3(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(57) = pv(57)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(58) = pv(58)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(59) = pv(59)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(60) = pv(60)+d(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(61) = pv(61)+d2(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(62) = pv(62)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(63) = pv(63)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(64) = pv(64)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
endif
if (7.le.mxd) then
 pv(110) = pv(110)+d4(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(111) = pv(111)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(112) = pv(112)+d4(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(113) = pv(113)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(114) = pv(114)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(115) = pv(115)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(116) = pv(116)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(117) = pv(117)+d2(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(118) = pv(118)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(119) = pv(119)+d(i0,i1)*d4(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(120) = pv(120)+d2(i0,i1)*d2(i0,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(121) = pv(121)+d3(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(122) = pv(122)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(123) = pv(123)+d2(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(124) = pv(124)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(125) = pv(125)+d(i0,i1)*d3(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(126) = pv(126)+d2(i0,i1)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/mu
 pv(127) = pv(127)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/mu
 pv(128) = pv(128)+d(i0,i1)*d2(i1,i2)*d3(i0,i3)*d(i0,i4)/mu
 pv(129) = pv(129)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(130) = pv(130)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(131) = pv(131)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(132) = pv(132)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(133) = pv(133)+d3(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(134) = pv(134)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(135) = pv(135)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(136) = pv(136)+d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(137) = pv(137)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(138) = pv(138)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
endif
if (8.le.mxd) then
 pv(210) = pv(210)+d5(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(211) = pv(211)+d4(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(212) = pv(212)+d3(i0,i1)*d3(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(213) = pv(213)+d5(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(214) = pv(214)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(215) = pv(215)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(216) = pv(216)+d4(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(217) = pv(217)+d3(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(218) = pv(218)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(219) = pv(219)+d3(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(220) = pv(220)+d2(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(221) = pv(221)+d2(i0,i1)*d4(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(222) = pv(222)+d(i0,i1)*d(i0,i2)*d4(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(223) = pv(223)+d(i0,i1)*d5(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(224) = pv(224)+d3(i0,i1)*d2(i0,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(225) = pv(225)+d4(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(226) = pv(226)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(227) = pv(227)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(228) = pv(228)+d3(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(229) = pv(229)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(230) = pv(230)+d2(i0,i1)*d3(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(231) = pv(231)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(232) = pv(232)+d(i0,i1)*d4(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(233) = pv(233)+d3(i0,i1)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/mu
 pv(234) = pv(234)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/mu
 pv(235) = pv(235)+d2(i0,i1)*d2(i1,i2)*d3(i0,i3)*d(i0,i4)/mu
 pv(236) = pv(236)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d3(i0,i3)*d(i0,i4)/mu
 pv(237) = pv(237)+d(i0,i1)*d3(i1,i2)*d3(i0,i3)*d(i0,i4)/mu
 pv(238) = pv(238)+d2(i0,i1)*d(i1,i2)*d4(i0,i3)*d(i0,i4)/mu
 pv(239) = pv(239)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d4(i0,i3)*d(i0,i4)/mu
 pv(240) = pv(240)+d(i0,i1)*d2(i1,i2)*d4(i0,i3)*d(i0,i4)/mu
 pv(241) = pv(241)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(242) = pv(242)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(243) = pv(243)+d2(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(244) = pv(244)+d(i0,i1)*d4(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(245) = pv(245)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(246) = pv(246)+d(i0,i1)*d3(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(247) = pv(247)+d4(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(248) = pv(248)+d3(i0,i2)*d3(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(249) = pv(249)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(250) = pv(250)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(251) = pv(251)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(252) = pv(252)+d4(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(253) = pv(253)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(254) = pv(254)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(255) = pv(255)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(256) = pv(256)+d3(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(257) = pv(257)+d2(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(258) = pv(258)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(259) = pv(259)+d2(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
endif
END SUBROUTINE mg6_ii
END SUBROUTINE mg6_isecs
