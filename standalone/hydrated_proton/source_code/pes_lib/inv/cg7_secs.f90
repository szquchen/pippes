SUBROUTINE cg7_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We don't have the secondaries at degrees 8; Magma ran out of
!! memory (4GB max) computing v(273:722).
!-----------------------------------------------------------------------
integer, parameter :: npv=272, nk2=nk*(nk-1), nk3=nk2*(nk-2), &
  nk4=nk3*(nk-3), nk5=nk4*(nk-4), nk6=nk5*(nk-5)
integer :: i0, i1, i2, i3, i4, i5
real (kind=wp) :: pv(0:npv-1), d(0:nk-1,0:nk-1), d2(0:nk-1,0:nk-1), &
  d3(0:nk-1,0:nk-1), d4(0:nk-1,0:nk-1), d5(0:nk-1,0:nk-1), &
  d6(0:nk-1,0:nk-1), d7(0:nk-1,0:nk-1)
if (size(x).ne.nr.or.size(v).ne.cg7_nsc(mxd)) then
 stop 'cg7_secs: bad dimensions'
endif
call cg_setd ()
pv = 0
do i0 = 0, nk-1
 do i1 = 0, nk-1
 if (i1.ne.i0) then
  do i2 = 0, nk-1
  if (i2.ne.i0.and.i2.ne.i1) then
   if (4.le.mxd) then
    call cg7_deg4_i2 ()
   endif
   if (5.le.mxd) then
    call cg7_deg5_i2 ()
   endif
   if (6.le.mxd) then
    call cg7_deg6_i2 ()
   endif
   if (7.le.mxd) then
    call cg7_deg7_i2 ()
   endif
   do i3 = 0, nk-1
   if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
    if (4.le.mxd) then
     call cg7_deg4_i3 ()
    endif
    if (5.le.mxd) then
     call cg7_deg5_i3 ()
    endif
    if (6.le.mxd) then
     call cg7_deg6_i3 ()
    endif
    if (7.le.mxd) then
     call cg7_deg7_i3 ()
    endif
    do i4 = 0, nk-1
    if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
     if (4.le.mxd) then
      call cg7_deg4_i4 ()
     endif
     if (5.le.mxd) then
      call cg7_deg5_i4 ()
     endif
     if (6.le.mxd) then
      call cg7_deg6_i4 ()
     endif
     if (7.le.mxd) then
      call cg7_deg7_i4 ()
     endif
     do i5 = 0, nk-1
     if (i5.ne.i0.and.i5.ne.i1.and.i5.ne.i2.and.i5.ne.i3.and.i5.ne.i4) then
      if (5.le.mxd) then
       call cg7_deg5_i5 ()
      endif
      if (6.le.mxd) then
       call cg7_deg6_i5 ()
      endif
      if (7.le.mxd) then
       call cg7_deg7_i5 ()
      endif
     endif
     enddo
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
if (4.le.mxd) then
 v(1:8) = pv(0:7)
endif
if (5.le.mxd) then
 v(9:33) = pv(8:32)
endif
if (6.le.mxd) then
 v(34:103) = pv(33:102)
endif
if (7.le.mxd) then
 v(104:272) = pv(103:271)
endif
if (8.le.mxd) then
 stop 'cg7: degree 8 not implemented'
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
   k = k+1
  enddo
  d(i1,i1) = 0
  d2(i1,i1) = 0
  d3(i1,i1) = 0
  d4(i1,i1) = 0
  d5(i1,i1) = 0
  d6(i1,i1) = 0
  d7(i1,i1) = 0
 enddo
END SUBROUTINE cg_setd
SUBROUTINE cg7_deg4_i2 ()
 pv(0) = pv(0)+d2(i0,i1)*d2(i0,i2)/nk3
 pv(1) = pv(1)+d2(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
END SUBROUTINE cg7_deg4_i2
SUBROUTINE cg7_deg4_i3 ()
 pv(2) = pv(2)+d2(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(3) = pv(3)+d2(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(4) = pv(4)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(5) = pv(5)+d(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
 pv(6) = pv(6)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
END SUBROUTINE cg7_deg4_i3
SUBROUTINE cg7_deg4_i4 ()
 pv(7) = pv(7)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
END SUBROUTINE cg7_deg4_i4
SUBROUTINE cg7_deg5_i2 ()
 pv(8) = pv(8)+d3(i0,i1)*d2(i0,i2)/nk3
 pv(9) = pv(9)+d3(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(10) = pv(10)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
END SUBROUTINE cg7_deg5_i2
SUBROUTINE cg7_deg5_i3 ()
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
END SUBROUTINE cg7_deg5_i3
SUBROUTINE cg7_deg5_i4 ()
 pv(22) = pv(22)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(23) = pv(23)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(24) = pv(24)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(25) = pv(25)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(26) = pv(26)+d(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(27) = pv(27)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(28) = pv(28)+d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(29) = pv(29)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(30) = pv(30)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(31) = pv(31)+d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
END SUBROUTINE cg7_deg5_i4
SUBROUTINE cg7_deg5_i5 ()
 pv(32) = pv(32)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
END SUBROUTINE cg7_deg5_i5
SUBROUTINE cg7_deg6_i2 ()
 pv(33) = pv(33)+d5(i0,i1)*d(i0,i2)/nk3
 pv(34) = pv(34)+d4(i0,i1)*d2(i0,i2)/nk3
 pv(35) = pv(35)+d3(i0,i1)*d3(i0,i2)/nk3
 pv(36) = pv(36)+d4(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(37) = pv(37)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
 pv(38) = pv(38)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)/nk3
END SUBROUTINE cg7_deg6_i2
SUBROUTINE cg7_deg6_i3 ()
 pv(39) = pv(39)+d4(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(40) = pv(40)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(41) = pv(41)+d4(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(42) = pv(42)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(43) = pv(43)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(44) = pv(44)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
 pv(45) = pv(45)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(46) = pv(46)+d2(i0,i1)*d3(i1,i2)*d(i0,i3)/nk4
 pv(47) = pv(47)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)/nk4
 pv(48) = pv(48)+d(i0,i1)*d4(i1,i2)*d(i0,i3)/nk4
 pv(49) = pv(49)+d2(i0,i1)*d2(i0,i2)*d2(i0,i3)/nk4
 pv(50) = pv(50)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(51) = pv(51)+d2(i0,i1)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(52) = pv(52)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(53) = pv(53)+d(i0,i1)*d3(i1,i2)*d2(i0,i3)/nk4
 pv(54) = pv(54)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)/nk4
 pv(55) = pv(55)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(56) = pv(56)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(57) = pv(57)+d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(58) = pv(58)+d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(59) = pv(59)+d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(60) = pv(60)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/nk4
END SUBROUTINE cg7_deg6_i3
SUBROUTINE cg7_deg6_i4 ()
 pv(61) = pv(61)+d3(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(62) = pv(62)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(63) = pv(63)+d3(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(64) = pv(64)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(65) = pv(65)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(66) = pv(66)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(67) = pv(67)+d(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(68) = pv(68)+d2(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(69) = pv(69)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(70) = pv(70)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(71) = pv(71)+d(i0,i1)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(72) = pv(72)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(73) = pv(73)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(74) = pv(74)+d3(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(75) = pv(75)+d2(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(76) = pv(76)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(77) = pv(77)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(78) = pv(78)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(79) = pv(79)+d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(80) = pv(80)+d2(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(81) = pv(81)+d(i0,i1)*d2(i1,i2)*d2(i1,i3)*d(i0,i4)/nk5
 pv(82) = pv(82)+d(i0,i2)*d2(i1,i2)*d2(i1,i3)*d(i0,i4)/nk5
 pv(83) = pv(83)+d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(84) = pv(84)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(85) = pv(85)+d(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(86) = pv(86)+d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(87) = pv(87)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d2(i0,i4)/nk5
 pv(88) = pv(88)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d2(i0,i4)/nk5
 pv(89) = pv(89)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(90) = pv(90)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(91) = pv(91)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(92) = pv(92)+d2(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
END SUBROUTINE cg7_deg6_i4
SUBROUTINE cg7_deg6_i5
 pv(93) = pv(93)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(94) = pv(94)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(95) = pv(95)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(96) = pv(96)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(97) = pv(97)+d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(98) = pv(98)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(99) = pv(99)+d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(100) = pv(100)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(101) = pv(101)+d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(102) = pv(102)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d2(i0,i4)*d(i0,i5)/nk6
END SUBROUTINE cg7_deg6_i5
SUBROUTINE cg7_deg7_i2 ()
 pv(103) = pv(103)+d5(i0,i1)*d2(i0,i2)/nk3
 pv(104) = pv(104)+d4(i0,i1)*d3(i0,i2)/nk3
 pv(105) = pv(105)+d5(i0,i1)*d(i0,i2)*d(i1,i2)/nk3
 pv(106) = pv(106)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)/nk3
 pv(107) = pv(107)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)/nk3
 pv(108) = pv(108)+d3(i0,i1)*d2(i0,i2)*d2(i1,i2)/nk3
END SUBROUTINE cg7_deg7_i2
SUBROUTINE cg7_deg7_i3 ()
 pv(109) = pv(109)+d5(i0,i1)*d(i0,i2)*d(i0,i3)/nk4
 pv(110) = pv(110)+d4(i0,i1)*d2(i0,i2)*d(i0,i3)/nk4
 pv(111) = pv(111)+d3(i0,i1)*d3(i0,i2)*d(i0,i3)/nk4
 pv(112) = pv(112)+d5(i0,i1)*d(i1,i2)*d(i0,i3)/nk4
 pv(113) = pv(113)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(114) = pv(114)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/nk4
 pv(115) = pv(115)+d4(i0,i1)*d2(i1,i2)*d(i0,i3)/nk4
 pv(116) = pv(116)+d3(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(117) = pv(117)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)/nk4
 pv(118) = pv(118)+d3(i0,i1)*d3(i1,i2)*d(i0,i3)/nk4
 pv(119) = pv(119)+d2(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)/nk4
 pv(120) = pv(120)+d2(i0,i1)*d4(i1,i2)*d(i0,i3)/nk4
 pv(121) = pv(121)+d(i0,i1)*d(i0,i2)*d4(i1,i2)*d(i0,i3)/nk4
 pv(122) = pv(122)+d(i0,i1)*d5(i1,i2)*d(i0,i3)/nk4
 pv(123) = pv(123)+d3(i0,i1)*d2(i0,i2)*d2(i0,i3)/nk4
 pv(124) = pv(124)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(125) = pv(125)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)/nk4
 pv(126) = pv(126)+d3(i0,i1)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(127) = pv(127)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)/nk4
 pv(128) = pv(128)+d2(i0,i1)*d3(i1,i2)*d2(i0,i3)/nk4
 pv(129) = pv(129)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d2(i0,i3)/nk4
 pv(130) = pv(130)+d(i0,i1)*d4(i1,i2)*d2(i0,i3)/nk4
 pv(131) = pv(131)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)/nk4
 pv(132) = pv(132)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d3(i0,i3)/nk4
 pv(133) = pv(133)+d(i0,i1)*d3(i1,i2)*d3(i0,i3)/nk4
 pv(134) = pv(134)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d4(i0,i3)/nk4
 pv(135) = pv(135)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(136) = pv(136)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(137) = pv(137)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(138) = pv(138)+d4(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(139) = pv(139)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(140) = pv(140)+d3(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)/nk4
 pv(141) = pv(141)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(142) = pv(142)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(143) = pv(143)+d2(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(144) = pv(144)+d(i0,i2)*d3(i1,i2)*d2(i0,i3)*d(i1,i3)/nk4
 pv(145) = pv(145)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)/nk4
END SUBROUTINE cg7_deg7_i3
SUBROUTINE cg7_deg7_i4 ()
 pv(146) = pv(146)+d4(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(147) = pv(147)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(148) = pv(148)+d4(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(149) = pv(149)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(150) = pv(150)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(151) = pv(151)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(152) = pv(152)+d2(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(153) = pv(153)+d2(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(154) = pv(154)+d(i0,i1)*d(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(155) = pv(155)+d(i0,i1)*d4(i1,i2)*d(i0,i3)*d(i0,i4)/nk5
 pv(156) = pv(156)+d2(i0,i1)*d2(i0,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(157) = pv(157)+d3(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(158) = pv(158)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(159) = pv(159)+d2(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(160) = pv(160)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(161) = pv(161)+d(i0,i1)*d3(i1,i2)*d2(i0,i3)*d(i0,i4)/nk5
 pv(162) = pv(162)+d2(i0,i1)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(163) = pv(163)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(164) = pv(164)+d(i0,i1)*d2(i1,i2)*d3(i0,i3)*d(i0,i4)/nk5
 pv(165) = pv(165)+d(i0,i1)*d(i1,i2)*d4(i0,i3)*d(i0,i4)/nk5
 pv(166) = pv(166)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(167) = pv(167)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(168) = pv(168)+d(i0,i1)*d3(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(169) = pv(169)+d4(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(170) = pv(170)+d(i0,i1)*d2(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(171) = pv(171)+d3(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)/nk5
 pv(172) = pv(172)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(173) = pv(173)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(174) = pv(174)+d3(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(175) = pv(175)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(176) = pv(176)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(177) = pv(177)+d2(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(178) = pv(178)+d(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(179) = pv(179)+d(i0,i2)*d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(180) = pv(180)+d2(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(181) = pv(181)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(182) = pv(182)+d(i0,i2)*d2(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(183) = pv(183)+d3(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(184) = pv(184)+d2(i1,i2)*d3(i0,i3)*d(i1,i3)*d(i0,i4)/nk5
 pv(185) = pv(185)+d2(i0,i1)*d2(i1,i2)*d2(i1,i3)*d(i0,i4)/nk5
 pv(186) = pv(186)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d2(i1,i3)*d(i0,i4)/nk5
 pv(187) = pv(187)+d2(i0,i2)*d2(i1,i2)*d2(i1,i3)*d(i0,i4)/nk5
 pv(188) = pv(188)+d(i0,i1)*d3(i1,i2)*d2(i1,i3)*d(i0,i4)/nk5
 pv(189) = pv(189)+d(i0,i2)*d3(i1,i2)*d2(i1,i3)*d(i0,i4)/nk5
 pv(190) = pv(190)+d(i0,i2)*d2(i1,i2)*d(i0,i3)*d2(i1,i3)*d(i0,i4)/nk5
 pv(191) = pv(191)+d3(i1,i2)*d(i0,i3)*d2(i1,i3)*d(i0,i4)/nk5
 pv(192) = pv(192)+d3(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(193) = pv(193)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(194) = pv(194)+d2(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(195) = pv(195)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(196) = pv(196)+d(i0,i1)*d3(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(197) = pv(197)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)*&
   d(i0,i4)/nk5
 pv(198) = pv(198)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(199) = pv(199)+d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(200) = pv(200)+d2(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(201) = pv(201)+d(i0,i1)*d2(i1,i2)*d2(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(202) = pv(202)+d(i0,i2)*d2(i1,i2)*d2(i1,i3)*d(i2,i3)*d(i0,i4)/nk5
 pv(203) = pv(203)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d2(i0,i4)/nk5
 pv(204) = pv(204)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)*d2(i0,i4)/nk5
 pv(205) = pv(205)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d2(i0,i4)/nk5
 pv(206) = pv(206)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d2(i0,i4)/nk5
 pv(207) = pv(207)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d2(i0,i4)/nk5
 pv(208) = pv(208)+d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d2(i0,i4)/nk5
 pv(209) = pv(209)+d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d2(i0,i4)/nk5
 pv(210) = pv(210)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d2(i0,i4)/nk5
 pv(211) = pv(211)+d(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i2,i3)*d2(i0,i4)/nk5
 pv(212) = pv(212)+d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)*d2(i0,i4)/nk5
 pv(213) = pv(213)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d3(i0,i4)/nk5
 pv(214) = pv(214)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d3(i0,i4)/nk5
 pv(215) = pv(215)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*&
   d(i1,i4)/nk5
 pv(216) = pv(216)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(217) = pv(217)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(218) = pv(218)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(219) = pv(219)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*&
   d(i1,i4)/nk5
 pv(220) = pv(220)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(221) = pv(221)+d3(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(222) = pv(222)+d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(223) = pv(223)+d2(i1,i2)*d2(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(224) = pv(224)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)*d(i0,i4)*&
   d(i1,i4)/nk5
 pv(225) = pv(225)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(226) = pv(226)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d2(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
 pv(227) = pv(227)+d2(i1,i2)*d(i0,i3)*d2(i2,i3)*d(i0,i4)*d(i1,i4)/nk5
END SUBROUTINE cg7_deg7_i4
SUBROUTINE cg7_deg7_i5 ()
 pv(228) = pv(228)+d3(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(229) = pv(229)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(230) = pv(230)+d3(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(231) = pv(231)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(232) = pv(232)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(233) = pv(233)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(234) = pv(234)+d(i0,i1)*d3(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(235) = pv(235)+d2(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(236) = pv(236)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(237) = pv(237)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(238) = pv(238)+d3(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(239) = pv(239)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(240) = pv(240)+d(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(241) = pv(241)+d3(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(242) = pv(242)+d2(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(243) = pv(243)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(244) = pv(244)+d2(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(245) = pv(245)+d(i0,i2)*d3(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(246) = pv(246)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*&
   d(i0,i5)/nk6
 pv(247) = pv(247)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(248) = pv(248)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(249) = pv(249)+d(i0,i2)*d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(250) = pv(250)+d3(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(251) = pv(251)+d2(i1,i2)*d2(i0,i3)*d(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(252) = pv(252)+d(i0,i1)*d2(i1,i2)*d2(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(253) = pv(253)+d(i0,i2)*d2(i1,i2)*d2(i1,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(254) = pv(254)+d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(255) = pv(255)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)*&
   d(i0,i5)/nk6
 pv(256) = pv(256)+d(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(257) = pv(257)+d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i2,i3)*d(i0,i4)*d(i0,i5)/nk6
 pv(258) = pv(258)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d2(i0,i4)*d(i0,i5)/nk6
 pv(259) = pv(259)+d2(i0,i2)*d(i1,i2)*d(i1,i3)*d2(i0,i4)*d(i0,i5)/nk6
 pv(260) = pv(260)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d2(i0,i4)*d(i0,i5)/nk6
 pv(261) = pv(261)+d2(i1,i2)*d(i0,i3)*d(i1,i3)*d2(i0,i4)*d(i0,i5)/nk6
 pv(262) = pv(262)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d3(i0,i4)*d(i0,i5)/nk6
 pv(263) = pv(263)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i1,i4)*&
   d(i0,i5)/nk6
 pv(264) = pv(264)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i1,i4)*d(i0,i5)/nk6
 pv(265) = pv(265)+d2(i0,i1)*d2(i0,i2)*d(i2,i3)*d(i1,i4)*d(i0,i5)/nk6
 pv(266) = pv(266)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i2,i3)*d(i1,i4)*d(i0,i5)/nk6
 pv(267) = pv(267)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i2,i3)*d(i1,i4)*d(i0,i5)/nk6
 pv(268) = pv(268)+d(i0,i1)*d2(i0,i2)*d(i0,i3)*d(i2,i3)*d(i1,i4)*d(i0,i5)/nk6
 pv(269) = pv(269)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i1,i4)*d(i0,i5)/nk6
 pv(270) = pv(270)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i1,i4)*&
   d(i0,i5)/nk6
 pv(271) = pv(271)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i1,i4)*d(i0,i5)/nk6
END SUBROUTINE cg7_deg7_i5
END SUBROUTINE cg7_secs
