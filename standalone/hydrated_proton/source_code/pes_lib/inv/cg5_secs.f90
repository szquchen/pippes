SUBROUTINE cg5_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
integer, parameter :: npv=46, nk2=nk*(nk-1), nk3=nk2*(nk-2), &
  nk4=nk3*(nk-3)
integer :: i0, i1, i2, i3
real (kind=wp) :: pv(0:npv-1), d(0:nk-1,0:nk-1), d2(0:nk-1,0:nk-1), &
  d3(0:nk-1,0:nk-1), d4(0:nk-1,0:nk-1), d5(0:nk-1,0:nk-1), &
  d6(0:nk-1,0:nk-1), d7(0:nk-1,0:nk-1), d8(0:nk-1,0:nk-1), &
  d9(0:nk-1,0:nk-1)
if (size(x).ne.nr.or.size(v).ne.cg5_nsc(mxd)) then
 stop 'cg5_secs: bad dimensions'
endif
call cg_setd ()
pv = 0
do i0 = 0, nk-1
 do i1 = 0, nk-1
 if (i1.ne.i0) then
  if (7.le.mxd) then
   call cg5_deg7_i1 ()
  endif
  if (8.le.mxd) then
   call cg5_deg8_i1 ()
  endif
  if (9.le.mxd) then
   call cg5_deg9_i1 ()
  endif
  do i2 = 0, nk-1
  if (i2.ne.i0.and.i2.ne.i1) then
   if (3.le.mxd) then
    call cg5_deg3_i2 ()
   endif
   if (4.le.mxd) then
    call cg5_deg4_i2 ()
   endif
   if (5.le.mxd) then
    call cg5_deg5_i2 ()
   endif
   if (6.le.mxd) then
    call cg5_deg6_i2 ()
   endif
   if (7.le.mxd) then
    call cg5_deg7_i2 ()
   endif
   if (8.le.mxd) then
    call cg5_deg8_i2 ()
   endif
   if (9.le.mxd) then
    call cg5_deg9_i2 ()
   endif
   do i3 = 0, nk-1
   if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
    if (4.le.mxd) then
     call cg5_deg4_i3 ()
    endif
    if (5.le.mxd) then
     call cg5_deg5_i3 ()
    endif
    if (6.le.mxd) then
     call cg5_deg6_i3 ()
    endif
    if (7.le.mxd) then
     call cg5_deg7_i3 ()
    endif
   endif
   enddo
  endif
  enddo
 endif
 enddo
enddo
v(0) = 1
if (3.le.mxd) then
 v(1:2) = pv(0:1)
endif
if (4.le.mxd) then
 v(3:7) = pv(2:6)
endif
if (5.le.mxd) then
 v(8:15) = pv(7:14)
endif
if (6.le.mxd) then
 v(16) = pv(0)*pv(0)
 v(17) = pv(0)*pv(1)
 v(18) = pv(1)*pv(1)
 v(19:30) = pv(15:26)
endif
if (7.le.mxd) then
 v(31) = pv(0)*pv(2)
 v(32) = pv(1)*pv(2)
 v(33) = pv(0)*pv(3)
 v(34) = pv(1)*pv(3)
 v(35) = pv(0)*pv(4)
 v(36) = pv(1)*pv(4)
 v(37) = pv(0)*pv(5)
 v(38) = pv(1)*pv(5)
 v(39) = pv(0)*pv(6)
 v(40) = pv(1)*pv(6)
 v(41:53) = pv(27:39)
endif
if (8.le.mxd) then
 v(54) = pv(3)*pv(3)
 v(55) = pv(2)*pv(4)
 v(56) = pv(3)*pv(4)
 v(57) = pv(4)*pv(4)
 v(58) = pv(2)*pv(5)
 v(59) = pv(3)*pv(5)
 v(60) = pv(4)*pv(5)
 v(61) = pv(5)*pv(5)
 v(62) = pv(2)*pv(6)
 v(63) = pv(3)*pv(6)
 v(64) = pv(4)*pv(6)
 v(65) = pv(5)*pv(6)
 v(66) = pv(6)*pv(6)
 v(67) = pv(0)*pv(7)
 v(68) = pv(1)*pv(7)
 v(69) = pv(0)*pv(8)
 v(70) = pv(1)*pv(8)
 v(71) = pv(0)*pv(9)
 v(72) = pv(1)*pv(9)
 v(73) = pv(0)*pv(10)
 v(74) = pv(1)*pv(10)
 v(75) = pv(0)*pv(11)
 v(76) = pv(1)*pv(11)
 v(77) = pv(0)*pv(12)
 v(78) = pv(1)*pv(12)
 v(79) = pv(0)*pv(13)
 v(80) = pv(1)*pv(13)
 v(81) = pv(0)*pv(14)
 v(82) = pv(1)*pv(14)
 v(83:86) = pv(40:43)
endif
if (9.le.mxd) then
 v(87) = pv(2)*pv(11)
 v(88) = pv(3)*pv(11)
 v(89) = pv(4)*pv(11)
 v(90) = pv(5)*pv(11)
 v(91) = pv(6)*pv(11)
 v(92) = pv(2)*pv(12)
 v(93) = pv(3)*pv(12)
 v(94) = pv(4)*pv(12)
 v(95) = pv(5)*pv(12)
 v(96) = pv(6)*pv(12)
 v(97) = pv(2)*pv(13)
 v(98) = pv(3)*pv(13)
 v(99) = pv(4)*pv(13)
 v(100) = pv(5)*pv(13)
 v(101) = pv(6)*pv(13)
 v(102) = pv(2)*pv(14)
 v(103) = pv(3)*pv(14)
 v(104) = pv(4)*pv(14)
 v(105) = pv(5)*pv(14)
 v(106) = pv(6)*pv(14)
 v(107) = pv(0)*pv(15)
 v(108) = pv(1)*pv(15)
 v(109) = pv(0)*pv(16)
 v(110) = pv(1)*pv(16)
 v(111) = pv(0)*pv(17)
 v(112) = pv(1)*pv(17)
 v(113) = pv(0)*pv(18)
 v(114) = pv(1)*pv(18)
 v(115) = pv(0)*pv(19)
 v(116) = pv(1)*pv(19)
 v(117) = pv(0)*pv(20)
 v(118) = pv(1)*pv(20)
 v(119) = pv(0)*pv(21)
 v(120) = pv(1)*pv(21)
 v(121) = pv(0)*pv(22)
 v(122) = pv(1)*pv(22)
 v(123) = pv(0)*pv(23)
 v(124) = pv(1)*pv(23)
 v(125) = pv(0)*pv(24)
 v(126) = pv(1)*pv(24)
 v(127) = pv(0)*pv(25)
 v(128) = pv(1)*pv(25)
 v(129) = pv(0)*pv(26)
 v(130) = pv(1)*pv(26)
 v(131:132) = pv(44:45)
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
   d9(i0,i1) = x(k)**9
   d9(i1,i0) = d9(i0,i1)
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
  d9(i1,i1) = 0
 enddo
END SUBROUTINE cg_setd
SUBROUTINE cg5_deg3_i2
 pv(0) = pv(0)+d2(i0,i1)*d(i0,i2)/nk3
 pv(1) = pv(1)+d(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
END SUBROUTINE cg5_deg3_i2
SUBROUTINE cg5_deg4_i2
 pv(2) = pv(2)+d3(i0,i1)*d(i0,i2)/nk3
 pv(3) = pv(3)+d2(i0,i1)*d2(i0,i2)/nk3
 pv(4) = pv(4)+d2(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
END SUBROUTINE cg5_deg4_i2
SUBROUTINE cg5_deg4_i3
 pv(5) = pv(5)+d2(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(6) = pv(6)+d2(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
END SUBROUTINE cg5_deg4_i3
SUBROUTINE cg5_deg5_i2
 pv(7) = pv(7)+d4(i0,i1)*d(i0,i2)/nk3
 pv(8) = pv(8)+d3(i0,i1)*d2(i0,i2)/nk3
 pv(9) = pv(9)+d3(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(10) = pv(10)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
END SUBROUTINE cg5_deg5_i2
SUBROUTINE cg5_deg5_i3
 pv(11) = pv(11)+d3(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(12) = pv(12)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(13) = pv(13)+d3(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(14) = pv(14)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
END SUBROUTINE cg5_deg5_i3
SUBROUTINE cg5_deg6_i2
 pv(15) = pv(15)+d5(i0,i1)*d(i0,i2)/nk3
 pv(16) = pv(16)+d4(i0,i1)*d2(i0,i2)/nk3
 pv(17) = pv(17)+d3(i0,i1)*d3(i0,i2)/nk3
 pv(18) = pv(18)+d4(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(19) = pv(19)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
 pv(20) = pv(20)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)/nk3
END SUBROUTINE cg5_deg6_i2
SUBROUTINE cg5_deg6_i3
 pv(21) = pv(21)+d4(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(22) = pv(22)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(23) = pv(23)+d4(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(24) = pv(24)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(25) = pv(25)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(26) = pv(26)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
END SUBROUTINE cg5_deg6_i3
SUBROUTINE cg5_deg7_i1
 pv(27) = pv(27)+d7(i0,i1)/nk2
END SUBROUTINE cg5_deg7_i1
SUBROUTINE cg5_deg7_i2
 pv(28) = pv(28)+d6(i0,i1)*d(i0,i2)/nk3
 pv(29) = pv(29)+d5(i0,i1)*d2(i0,i2)/nk3
 pv(30) = pv(30)+d4(i0,i1)*d3(i0,i2)/nk3
 pv(31) = pv(31)+d5(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(32) = pv(32)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
 pv(33) = pv(33)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)/nk3
 pv(34) = pv(34)+d3(i0,i1)*d2(i0,i2)*d2(i1,i2)/nk3
END SUBROUTINE cg5_deg7_i2
SUBROUTINE cg5_deg7_i3
 pv(35) = pv(35)+d5(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(36) = pv(36)+d4(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(37) = pv(37)+d3(i0,i1)*d3(i0,i2)*d(i0,i3)/nk4
 pv(38) = pv(38)+d5(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(39) = pv(39)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
END SUBROUTINE cg5_deg7_i3
SUBROUTINE cg5_deg8_i1
 pv(40) = pv(40)+d8(i0,i1)/nk2
END SUBROUTINE cg5_deg8_i1
SUBROUTINE cg5_deg8_i2
 pv(41) = pv(41)+d7(i0,i1)*d(i0,i2)/nk3
 pv(42) = pv(42)+d6(i0,i1)*d2(i0,i2)/nk3
 pv(43) = pv(43)+d5(i0,i1)*d3(i0,i2)/nk3
END SUBROUTINE cg5_deg8_i2
SUBROUTINE cg5_deg9_i1
 pv(44) = pv(44)+d9(i0,i1)/nk2
END SUBROUTINE cg5_deg9_i1
SUBROUTINE cg5_deg9_i2
 pv(45) = pv(45)+d8(i0,i1)*d(i0,i2)/nk3
END SUBROUTINE cg5_deg9_i2
END SUBROUTINE cg5_secs
