SUBROUTINE cg6_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We don't have the secondaries at degree 9; Magma ran out of
!! memory (4GB max) computing v(315:642).
!-----------------------------------------------------------------------
integer, parameter :: npv=260, nk2=nk*(nk-1), nk3=nk2*(nk-2), &
  nk4=nk3*(nk-3), nk5=nk4*(nk-4)
integer :: i0, i1, i2, i3, i4
real (kind=wp) :: pv(0:npv-1), d(0:nk-1,0:nk-1), d2(0:nk-1,0:nk-1), &
  d3(0:nk-1,0:nk-1), d4(0:nk-1,0:nk-1), d5(0:nk-1,0:nk-1), &
  d6(0:nk-1,0:nk-1), d7(0:nk-1,0:nk-1), d8(0:nk-1,0:nk-1)
if (size(x).ne.nr.or.size(v).ne.cg6_nsc(mxd)) then
 stop 'cg6_secs: bad dimensions'
endif
call cg_setd ()
pv = 0
do i0 = 0, nk-1
 do i1 = 0, nk-1
 if (i1.ne.i0) then
  if (7.le.mxd) then
   call cg6_deg7_i1 ()
  endif
  if (8.le.mxd) then
   call cg6_deg8_i1 ()
  endif
  do i2 = 0, nk-1
  if (i2.ne.i0.and.i2.ne.i1) then
   if (4.le.mxd) then
    call cg6_deg4_i2 ()
   endif
   if (5.le.mxd) then
    call cg6_deg5_i2 ()
   endif
   if (6.le.mxd) then
    call cg6_deg6_i2 ()
   endif
   if (7.le.mxd) then
    call cg6_deg7_i2 ()
   endif
   if (8.le.mxd) then
    call cg6_deg8_i2 ()
   endif
   do i3 = 0, nk-1
   if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
    if (3.le.mxd) then
     call cg6_deg3_i3 ()
    endif
    if (4.le.mxd) then
     call cg6_deg4_i3 ()
    endif
    if (5.le.mxd) then
     call cg6_deg5_i3 ()
    endif
    if (6.le.mxd) then
     call cg6_deg6_i3 ()
    endif
    if (7.le.mxd) then
     call cg6_deg7_i3 ()
    endif
    if (8.le.mxd) then
     call cg6_deg8_i3 ()
    endif
    do i4 = 0, nk-1
    if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
     if (5.le.mxd) then
      call cg6_deg5_i4 ()
     endif
     if (6.le.mxd) then
      call cg6_deg6_i4 ()
     endif
     if (7.le.mxd) then
      call cg6_deg7_i4 ()
     endif
     if (8.le.mxd) then
      call cg6_deg8_i4 ()
     endif
    endif
    enddo
   endif
   enddo
  endif
  enddo
 endif
 enddo
enddo
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
 stop 'cg6: degree 9 not implemented'
endif
return
CONTAINS
SUBROUTINE cg_setd ()
 integer :: i0, i1, k
 k = 0
 do i1 = 1, nk-1
  do i0 = 0, i1-1
   d(i0,i1) = x(k)
   d(i1,i0) = d(i0,i1)
   d2(i0,i1) = x(k)**2
   d2(i1,i0) = d2(i0,i1)
   d3(i0,i1) = x(k)**3
   d3(i1,i0) = d3(i0,i1)
   d4(i0,i1) = x(k)**4
   d4(i1,i0) = d4(i0,i1)
   d5(i0,i1) = x(k)**5
   d5(i1,i0) = d5(i0,i1)
   d6(i0,i1) = x(k)**6
   d6(i1,i0) = d6(i0,i1)
   d7(i0,i1) = x(k)**7
   d7(i1,i0) = d7(i0,i1)
   d8(i0,i1) = x(k)**8
   d8(i1,i0) = d8(i0,i1)
   k = k+1
  enddo
  d(i1,i1) = 0
  d2(i1,i1) = 0
  d3(i1,i1) = 0
  d4(i1,i1) = 0
  d5(i1,i1) = 0
  d6(i1,i1) = 0
  d7(i1,i1) = 0
  d8(i1,i1) = 0
 enddo
END SUBROUTINE cg_setd
SUBROUTINE cg6_deg3_i3 ()
 pv(0) = pv(0)+d(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
END SUBROUTINE cg6_deg3_i3
SUBROUTINE cg6_deg4_i2 ()
 pv(1) = pv(1)+d2(i0,i1)*d2(i0,i2)/nk3
 pv(2) = pv(2)+d2(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
END SUBROUTINE cg6_deg4_i2
SUBROUTINE cg6_deg4_i3 ()
 pv(3) = pv(3)+d2(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(4) = pv(4)+d2(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(5) = pv(5)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(6) = pv(6)+d(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
 pv(7) = pv(7)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
END SUBROUTINE cg6_deg4_i3
SUBROUTINE cg6_deg5_i2 ()
 pv(8) = pv(8)+d3(i0,i1)*d2(i0,i2)/nk3
 pv(9) = pv(9)+d3(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(10) = pv(10)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
END SUBROUTINE cg6_deg5_i2
SUBROUTINE cg6_deg5_i3 ()
 pv(11) = pv(11)+d3(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(12) = pv(12)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(13) = pv(13)+d3(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(14) = pv(14)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(15) = pv(15)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
 pv(16) = pv(16)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(17) = pv(17)+d(i0,i1)*d3(i1,i2)*d(i0,i3)/nk4
 pv(18) = pv(18)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(19) = pv(19)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(20) = pv(20)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(21) = pv(21)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
END SUBROUTINE cg6_deg5_i3
SUBROUTINE cg6_deg5_i4 ()
 pv(22) = pv(22)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(23) = pv(23)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(24) = pv(24)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(25) = pv(25)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
END SUBROUTINE cg6_deg5_i4
SUBROUTINE cg6_deg6_i2 ()
 pv(26) = pv(26)+d5(i0,i1)*d(i0,i2)/nk3
 pv(27) = pv(27)+d4(i0,i1)*d2(i0,i2)/nk3
 pv(28) = pv(28)+d3(i0,i1)*d3(i0,i2)/nk3
 pv(29) = pv(29)+d4(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(30) = pv(30)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
 pv(31) = pv(31)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)/nk3
END SUBROUTINE cg6_deg6_i2
SUBROUTINE cg6_deg6_i3 ()
 pv(32) = pv(32)+d4(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(33) = pv(33)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(34) = pv(34)+d4(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(35) = pv(35)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(36) = pv(36)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(37) = pv(37)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
 pv(38) = pv(38)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(39) = pv(39)+d2(i0,i1)*d3(i1,i2)*d(i0,i3)/nk4
 pv(40) = pv(40)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)/nk4
 pv(41) = pv(41)+d(i0,i1)*d4(i1,i2)*d(i0,i3)/nk4
 pv(42) = pv(42)+d2(i0,i1)*d2(i0,i2)*d2(i0,i3)/nk4
 pv(43) = pv(43)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(44) = pv(44)+d2(i0,i1)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(45) = pv(45)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(46) = pv(46)+d(i0,i1)*d3(i1,i2)*d2(i0,i3)/nk4
 pv(47) = pv(47)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)/nk4
 pv(48) = pv(48)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(49) = pv(49)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(50) = pv(50)+d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(51) = pv(51)+d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(52) = pv(52)+d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(53) = pv(53)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/nk4
END SUBROUTINE cg6_deg6_i3
SUBROUTINE cg6_deg6_i4 ()
 pv(54) = pv(54)+d3(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(55) = pv(55)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(56) = pv(56)+d3(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(57) = pv(57)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(58) = pv(58)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(59) = pv(59)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(60) = pv(60)+d(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(61) = pv(61)+d2(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(62) = pv(62)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(63) = pv(63)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(64) = pv(64)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
END SUBROUTINE cg6_deg6_i4
SUBROUTINE cg6_deg7_i1 ()
 pv(65) = pv(65)+d7(i0,i1)/nk2
END SUBROUTINE cg6_deg7_i1
SUBROUTINE cg6_deg7_i2 ()
 pv(66) = pv(66)+d6(i0,i1)*d(i0,i2)/nk3
 pv(67) = pv(67)+d5(i0,i1)*d2(i0,i2)/nk3
 pv(68) = pv(68)+d4(i0,i1)*d3(i0,i2)/nk3
 pv(69) = pv(69)+d5(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(70) = pv(70)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
 pv(71) = pv(71)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)/nk3
 pv(72) = pv(72)+d3(i0,i1)*d2(i0,i2)*d2(i1,i2)/nk3
END SUBROUTINE cg6_deg7_i2
SUBROUTINE cg6_deg7_i3 ()
 pv(73) = pv(73)+d5(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(74) = pv(74)+d4(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(75) = pv(75)+d3(i0,i1)*d3(i0,i2)*d(i0,i3)/nk4
 pv(76) = pv(76)+d5(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(77) = pv(77)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(78) = pv(78)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(79) = pv(79)+d4(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
 pv(80) = pv(80)+d3(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(81) = pv(81)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(82) = pv(82)+d3(i0,i1)*d3(i1,i2)*d(i0,i3)/nk4
 pv(83) = pv(83)+d2(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)/nk4
 pv(84) = pv(84)+d2(i0,i1)*d4(i1,i2)*d(i0,i3)/nk4
 pv(85) = pv(85)+d(i0,i1)*d(i0,i2)*d4(i1,i2)*d(i0,i3)/nk4
 pv(86) = pv(86)+d(i0,i1)*d5(i1,i2)*d(i0,i3)/nk4
 pv(87) = pv(87)+d3(i0,i1)*d2(i0,i2)*d2(i0,i3)/nk4
 pv(88) = pv(88)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(89) = pv(89)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(90) = pv(90)+d3(i0,i1)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(91) = pv(91)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(92) = pv(92)+d2(i0,i1)*d3(i1,i2)*d2(i0,i3)/nk4
 pv(93) = pv(93)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)/nk4
 pv(94) = pv(94)+d(i0,i1)*d4(i1,i2)*d2(i0,i3)/nk4
 pv(95) = pv(95)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)/nk4
 pv(96) = pv(96)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d3(i0,i3)/nk4
 pv(97) = pv(97)+d(i0,i1)*d3(i1,i2)*d3(i0,i3)/nk4
 pv(98) = pv(98)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d4(i0,i3)/nk4
 pv(99) = pv(99)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(100) = pv(100)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(101) = pv(101)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(102) = pv(102)+d4(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(103) = pv(103)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(104) = pv(104)+d3(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(105) = pv(105)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(106) = pv(106)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(107) = pv(107)+d2(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(108) = pv(108)+d(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(109) = pv(109)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/nk4
END SUBROUTINE cg6_deg7_i3
SUBROUTINE cg6_deg7_i4 ()
 pv(110) = pv(110)+d4(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(111) = pv(111)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(112) = pv(112)+d4(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(113) = pv(113)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(114) = pv(114)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(115) = pv(115)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(116) = pv(116)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(117) = pv(117)+d2(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(118) = pv(118)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(119) = pv(119)+d(i0,i1)*d4(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(120) = pv(120)+d2(i0,i1)*d2(i0,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(121) = pv(121)+d3(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(122) = pv(122)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(123) = pv(123)+d2(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(124) = pv(124)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(125) = pv(125)+d(i0,i1)*d3(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(126) = pv(126)+d2(i0,i1)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(127) = pv(127)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(128) = pv(128)+d(i0,i1)*d2(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(129) = pv(129)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(130) = pv(130)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(131) = pv(131)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(132) = pv(132)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(133) = pv(133)+d3(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(134) = pv(134)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(135) = pv(135)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(136) = pv(136)+d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(137) = pv(137)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(138) = pv(138)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
END SUBROUTINE cg6_deg7_i4
SUBROUTINE cg6_deg8_i1 ()
 pv(139) = pv(139)+d8(i0,i1)/nk2
END SUBROUTINE cg6_deg8_i1
SUBROUTINE cg6_deg8_i2 ()
 pv(140) = pv(140)+d7(i0,i1)*d(i0,i2)/nk3
 pv(141) = pv(141)+d6(i0,i1)*d2(i0,i2)/nk3
 pv(142) = pv(142)+d5(i0,i1)*d3(i0,i2)/nk3
 pv(143) = pv(143)+d4(i0,i1)*d4(i0,i2)/nk3
 pv(144) = pv(144)+d6(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(145) = pv(145)+d5(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
 pv(146) = pv(146)+d4(i0,i1)*d3(i0,i2)*d(i1,i2)/nk3
 pv(147) = pv(147)+d4(i0,i1)*d2(i0,i2)*d2(i1,i2)/nk3
 pv(148) = pv(148)+d3(i0,i1)*d3(i0,i2)*d2(i1,i2)/nk3
END SUBROUTINE cg6_deg8_i2
SUBROUTINE cg6_deg8_i3 ()
 pv(149) = pv(149)+d6(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(150) = pv(150)+d5(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(151) = pv(151)+d4(i0,i1)*d3(i0,i2)*d(i0,i3)/nk4
 pv(152) = pv(152)+d6(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(153) = pv(153)+d5(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(154) = pv(154)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(155) = pv(155)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(156) = pv(156)+d5(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
 pv(157) = pv(157)+d4(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(158) = pv(158)+d3(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(159) = pv(159)+d4(i0,i1)*d3(i1,i2)*d(i0,i3)/nk4
 pv(160) = pv(160)+d3(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)/nk4
 pv(161) = pv(161)+d2(i0,i1)*d2(i0,i2)*d3(i1,i2)*d(i0,i3)/nk4
 pv(162) = pv(162)+d3(i0,i1)*d4(i1,i2)*d(i0,i3)/nk4
 pv(163) = pv(163)+d2(i0,i1)*d(i0,i2)*d4(i1,i2)*d(i0,i3)/nk4
 pv(164) = pv(164)+d2(i0,i1)*d5(i1,i2)*d(i0,i3)/nk4
 pv(165) = pv(165)+d(i0,i1)*d(i0,i2)*d5(i1,i2)*d(i0,i3)/nk4
 pv(166) = pv(166)+d(i0,i1)*d6(i1,i2)*d(i0,i3)/nk4
 pv(167) = pv(167)+d4(i0,i1)*d2(i0,i2)*d2(i0,i3)/nk4
 pv(168) = pv(168)+d3(i0,i1)*d3(i0,i2)*d2(i0,i3)/nk4
 pv(169) = pv(169)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(170) = pv(170)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(171) = pv(171)+d4(i0,i1)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(172) = pv(172)+d3(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(173) = pv(173)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(174) = pv(174)+d3(i0,i1)*d3(i1,i2)*d2(i0,i3)/nk4
 pv(175) = pv(175)+d2(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)/nk4
 pv(176) = pv(176)+d2(i0,i1)*d4(i1,i2)*d2(i0,i3)/nk4
 pv(177) = pv(177)+d(i0,i1)*d(i0,i2)*d4(i1,i2)*d2(i0,i3)/nk4
 pv(178) = pv(178)+d(i0,i1)*d5(i1,i2)*d2(i0,i3)/nk4
 pv(179) = pv(179)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)/nk4
 pv(180) = pv(180)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d3(i0,i3)/nk4
 pv(181) = pv(181)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d3(i0,i3)/nk4
 pv(182) = pv(182)+d2(i0,i1)*d3(i1,i2)*d3(i0,i3)/nk4
 pv(183) = pv(183)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d3(i0,i3)/nk4
 pv(184) = pv(184)+d(i0,i1)*d4(i1,i2)*d3(i0,i3)/nk4
 pv(185) = pv(185)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d4(i0,i3)/nk4
 pv(186) = pv(186)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d4(i0,i3)/nk4
 pv(187) = pv(187)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d5(i0,i3)/nk4
 pv(188) = pv(188)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(189) = pv(189)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(190) = pv(190)+d2(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(191) = pv(191)+d(i0,i1)*d4(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(192) = pv(192)+d5(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(193) = pv(193)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(194) = pv(194)+d(i0,i1)*d3(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(195) = pv(195)+d4(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(196) = pv(196)+d3(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(197) = pv(197)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(198) = pv(198)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(199) = pv(199)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(200) = pv(200)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(201) = pv(201)+d3(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(202) = pv(202)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(203) = pv(203)+d2(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(204) = pv(204)+d(i0,i2)*d4(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(205) = pv(205)+d(i0,i2)*d3(i1,i2)*d3(i0,i3)*d(i1,i3)/nk4
 pv(206) = pv(206)+d2(i0,i2)*d2(i1,i2)*d2(i0,i3)*d2(i1,i3)/nk4
 pv(207) = pv(207)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/nk4
 pv(208) = pv(208)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/nk4
 pv(209) = pv(209)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i2,i3)/nk4
END SUBROUTINE cg6_deg8_i3
SUBROUTINE cg6_deg8_i4 ()
 pv(210) = pv(210)+d5(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(211) = pv(211)+d4(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(212) = pv(212)+d3(i0,i1)*d3(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(213) = pv(213)+d5(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(214) = pv(214)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(215) = pv(215)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(216) = pv(216)+d4(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(217) = pv(217)+d3(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(218) = pv(218)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(219) = pv(219)+d3(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(220) = pv(220)+d2(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(221) = pv(221)+d2(i0,i1)*d4(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(222) = pv(222)+d(i0,i1)*d(i0,i2)*d4(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(223) = pv(223)+d(i0,i1)*d5(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(224) = pv(224)+d3(i0,i1)*d2(i0,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(225) = pv(225)+d4(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(226) = pv(226)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(227) = pv(227)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(228) = pv(228)+d3(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(229) = pv(229)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(230) = pv(230)+d2(i0,i1)*d3(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(231) = pv(231)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(232) = pv(232)+d(i0,i1)*d4(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(233) = pv(233)+d3(i0,i1)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(234) = pv(234)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(235) = pv(235)+d2(i0,i1)*d2(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(236) = pv(236)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(237) = pv(237)+d(i0,i1)*d3(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(238) = pv(238)+d2(i0,i1)*d(i1,i2)*d4(i0,i3)*d(i0,i4)/nk5
 pv(239) = pv(239)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d4(i0,i3)*d(i0,i4)/nk5
 pv(240) = pv(240)+d(i0,i1)*d2(i1,i2)*d4(i0,i3)*d(i0,i4)/nk5
 pv(241) = pv(241)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(242) = pv(242)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(243) = pv(243)+d2(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(244) = pv(244)+d(i0,i1)*d4(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(245) = pv(245)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(246) = pv(246)+d(i0,i1)*d3(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(247) = pv(247)+d4(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(248) = pv(248)+d3(i0,i2)*d3(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(249) = pv(249)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(250) = pv(250)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(251) = pv(251)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(252) = pv(252)+d4(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(253) = pv(253)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(254) = pv(254)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(255) = pv(255)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(256) = pv(256)+d3(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(257) = pv(257)+d2(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(258) = pv(258)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(259) = pv(259)+d2(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
END SUBROUTINE cg6_deg8_i4
END SUBROUTINE cg6_secs
