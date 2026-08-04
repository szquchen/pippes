SUBROUTINE mg4111111_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=4, m2=m*(m-1), mm=m2*(m-2)
integer :: i0, i1, i2, i3, j0, k0, l0, m0, n0, o0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5, d6
if (size(r).ne.nk*nk) then
 stop 'mg4111111_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5, d6)
pv = 0
j0 = m ; k0 = j0+1 ; l0 = k0+1 ; m0 = l0+1 ; n0 = m0+1 ; o0 = n0+1
do i0 = 0, m-1
 call mg4111111_i0 (mxd, m)
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg4111111_i1 (mxd, m2)
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     do i3 = 0, m-1
      if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
       call mg4111111_ii (mxd, mm)
      endif
     enddo
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg4111111_i0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/mu
 pv(4) = pv(4)+d(i0,j0)*d(i0,l0)/mu
 pv(5) = pv(5)+d(i0,k0)*d(i0,l0)/mu
 pv(7) = pv(7)+d(i0,j0)*d(i0,m0)/mu
 pv(8) = pv(8)+d(i0,k0)*d(i0,m0)/mu
 pv(9) = pv(9)+d(i0,l0)*d(i0,m0)/mu
 pv(11) = pv(11)+d(i0,j0)*d(i0,n0)/mu
 pv(12) = pv(12)+d(i0,k0)*d(i0,n0)/mu
 pv(13) = pv(13)+d(i0,l0)*d(i0,n0)/mu
 pv(14) = pv(14)+d(i0,m0)*d(i0,n0)/mu
 pv(16) = pv(16)+d(i0,j0)*d(i0,o0)/mu
 pv(17) = pv(17)+d(i0,k0)*d(i0,o0)/mu
 pv(18) = pv(18)+d(i0,l0)*d(i0,o0)/mu
 pv(19) = pv(19)+d(i0,m0)*d(i0,o0)/mu
 pv(20) = pv(20)+d(i0,n0)*d(i0,o0)/mu
endif
if (3.le.mxd) then
 pv(29) = pv(29)+d2(i0,j0)*d(i0,k0)/mu
 pv(32) = pv(32)+d(i0,j0)*d2(i0,k0)/mu
 pv(37) = pv(37)+d2(i0,j0)*d(i0,l0)/mu
 pv(40) = pv(40)+d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(41) = pv(41)+d2(i0,k0)*d(i0,l0)/mu
 pv(44) = pv(44)+d(i0,j0)*d2(i0,l0)/mu
 pv(45) = pv(45)+d(i0,k0)*d2(i0,l0)/mu
 pv(50) = pv(50)+d2(i0,j0)*d(i0,m0)/mu
 pv(53) = pv(53)+d(i0,j0)*d(i0,k0)*d(i0,m0)/mu
 pv(54) = pv(54)+d2(i0,k0)*d(i0,m0)/mu
 pv(57) = pv(57)+d(i0,j0)*d(i0,l0)*d(i0,m0)/mu
 pv(58) = pv(58)+d(i0,k0)*d(i0,l0)*d(i0,m0)/mu
 pv(59) = pv(59)+d2(i0,l0)*d(i0,m0)/mu
 pv(62) = pv(62)+d(i0,j0)*d2(i0,m0)/mu
 pv(63) = pv(63)+d(i0,k0)*d2(i0,m0)/mu
 pv(64) = pv(64)+d(i0,l0)*d2(i0,m0)/mu
 pv(69) = pv(69)+d2(i0,j0)*d(i0,n0)/mu
 pv(72) = pv(72)+d(i0,j0)*d(i0,k0)*d(i0,n0)/mu
 pv(73) = pv(73)+d2(i0,k0)*d(i0,n0)/mu
 pv(76) = pv(76)+d(i0,j0)*d(i0,l0)*d(i0,n0)/mu
 pv(77) = pv(77)+d(i0,k0)*d(i0,l0)*d(i0,n0)/mu
 pv(78) = pv(78)+d2(i0,l0)*d(i0,n0)/mu
 pv(81) = pv(81)+d(i0,j0)*d(i0,m0)*d(i0,n0)/mu
 pv(82) = pv(82)+d(i0,k0)*d(i0,m0)*d(i0,n0)/mu
 pv(83) = pv(83)+d(i0,l0)*d(i0,m0)*d(i0,n0)/mu
 pv(84) = pv(84)+d2(i0,m0)*d(i0,n0)/mu
 pv(87) = pv(87)+d(i0,j0)*d2(i0,n0)/mu
 pv(88) = pv(88)+d(i0,k0)*d2(i0,n0)/mu
 pv(89) = pv(89)+d(i0,l0)*d2(i0,n0)/mu
 pv(90) = pv(90)+d(i0,m0)*d2(i0,n0)/mu
 pv(95) = pv(95)+d2(i0,j0)*d(i0,o0)/mu
 pv(98) = pv(98)+d(i0,j0)*d(i0,k0)*d(i0,o0)/mu
 pv(99) = pv(99)+d2(i0,k0)*d(i0,o0)/mu
 pv(102) = pv(102)+d(i0,j0)*d(i0,l0)*d(i0,o0)/mu
 pv(103) = pv(103)+d(i0,k0)*d(i0,l0)*d(i0,o0)/mu
 pv(104) = pv(104)+d2(i0,l0)*d(i0,o0)/mu
 pv(107) = pv(107)+d(i0,j0)*d(i0,m0)*d(i0,o0)/mu
 pv(108) = pv(108)+d(i0,k0)*d(i0,m0)*d(i0,o0)/mu
 pv(109) = pv(109)+d(i0,l0)*d(i0,m0)*d(i0,o0)/mu
 pv(110) = pv(110)+d2(i0,m0)*d(i0,o0)/mu
 pv(113) = pv(113)+d(i0,j0)*d(i0,n0)*d(i0,o0)/mu
 pv(114) = pv(114)+d(i0,k0)*d(i0,n0)*d(i0,o0)/mu
 pv(115) = pv(115)+d(i0,l0)*d(i0,n0)*d(i0,o0)/mu
 pv(116) = pv(116)+d(i0,m0)*d(i0,n0)*d(i0,o0)/mu
 pv(117) = pv(117)+d2(i0,n0)*d(i0,o0)/mu
 pv(120) = pv(120)+d(i0,j0)*d2(i0,o0)/mu
 pv(121) = pv(121)+d(i0,k0)*d2(i0,o0)/mu
 pv(122) = pv(122)+d(i0,l0)*d2(i0,o0)/mu
 pv(123) = pv(123)+d(i0,m0)*d2(i0,o0)/mu
 pv(124) = pv(124)+d(i0,n0)*d2(i0,o0)/mu
endif
END SUBROUTINE mg4111111_i0
SUBROUTINE mg4111111_i1 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
 pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/mu
 pv(3) = pv(3)+d(i0,i1)*d(i0,l0)/mu
 pv(6) = pv(6)+d(i0,i1)*d(i0,m0)/mu
 pv(10) = pv(10)+d(i0,i1)*d(i0,n0)/mu
 pv(15) = pv(15)+d(i0,i1)*d(i0,o0)/mu
endif
if (3.le.mxd) then
 pv(22) = pv(22)+d2(i0,i1)*d(i0,j0)/mu
 pv(24) = pv(24)+d(i0,i1)*d2(i0,j0)/mu
 pv(25) = pv(25)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(26) = pv(26)+d2(i0,i1)*d(i0,k0)/mu
 pv(28) = pv(28)+d(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(30) = pv(30)+d(i0,i1)*d(i1,j0)*d(i0,k0)/mu
 pv(31) = pv(31)+d(i0,i1)*d2(i0,k0)/mu
 pv(33) = pv(33)+d(i0,i1)*d(i0,k0)*d(i1,k0)/mu
 pv(34) = pv(34)+d2(i0,i1)*d(i0,l0)/mu
 pv(36) = pv(36)+d(i0,i1)*d(i0,j0)*d(i0,l0)/mu
 pv(38) = pv(38)+d(i0,i1)*d(i1,j0)*d(i0,l0)/mu
 pv(39) = pv(39)+d(i0,i1)*d(i0,k0)*d(i0,l0)/mu
 pv(42) = pv(42)+d(i0,i1)*d(i1,k0)*d(i0,l0)/mu
 pv(43) = pv(43)+d(i0,i1)*d2(i0,l0)/mu
 pv(46) = pv(46)+d(i0,i1)*d(i0,l0)*d(i1,l0)/mu
 pv(47) = pv(47)+d2(i0,i1)*d(i0,m0)/mu
 pv(49) = pv(49)+d(i0,i1)*d(i0,j0)*d(i0,m0)/mu
 pv(51) = pv(51)+d(i0,i1)*d(i1,j0)*d(i0,m0)/mu
 pv(52) = pv(52)+d(i0,i1)*d(i0,k0)*d(i0,m0)/mu
 pv(55) = pv(55)+d(i0,i1)*d(i1,k0)*d(i0,m0)/mu
 pv(56) = pv(56)+d(i0,i1)*d(i0,l0)*d(i0,m0)/mu
 pv(60) = pv(60)+d(i0,i1)*d(i1,l0)*d(i0,m0)/mu
 pv(61) = pv(61)+d(i0,i1)*d2(i0,m0)/mu
 pv(65) = pv(65)+d(i0,i1)*d(i0,m0)*d(i1,m0)/mu
 pv(66) = pv(66)+d2(i0,i1)*d(i0,n0)/mu
 pv(68) = pv(68)+d(i0,i1)*d(i0,j0)*d(i0,n0)/mu
 pv(70) = pv(70)+d(i0,i1)*d(i1,j0)*d(i0,n0)/mu
 pv(71) = pv(71)+d(i0,i1)*d(i0,k0)*d(i0,n0)/mu
 pv(74) = pv(74)+d(i0,i1)*d(i1,k0)*d(i0,n0)/mu
 pv(75) = pv(75)+d(i0,i1)*d(i0,l0)*d(i0,n0)/mu
 pv(79) = pv(79)+d(i0,i1)*d(i1,l0)*d(i0,n0)/mu
 pv(80) = pv(80)+d(i0,i1)*d(i0,m0)*d(i0,n0)/mu
 pv(85) = pv(85)+d(i0,i1)*d(i1,m0)*d(i0,n0)/mu
 pv(86) = pv(86)+d(i0,i1)*d2(i0,n0)/mu
 pv(91) = pv(91)+d(i0,i1)*d(i0,n0)*d(i1,n0)/mu
 pv(92) = pv(92)+d2(i0,i1)*d(i0,o0)/mu
 pv(94) = pv(94)+d(i0,i1)*d(i0,j0)*d(i0,o0)/mu
 pv(96) = pv(96)+d(i0,i1)*d(i1,j0)*d(i0,o0)/mu
 pv(97) = pv(97)+d(i0,i1)*d(i0,k0)*d(i0,o0)/mu
 pv(100) = pv(100)+d(i0,i1)*d(i1,k0)*d(i0,o0)/mu
 pv(101) = pv(101)+d(i0,i1)*d(i0,l0)*d(i0,o0)/mu
 pv(105) = pv(105)+d(i0,i1)*d(i1,l0)*d(i0,o0)/mu
 pv(106) = pv(106)+d(i0,i1)*d(i0,m0)*d(i0,o0)/mu
 pv(111) = pv(111)+d(i0,i1)*d(i1,m0)*d(i0,o0)/mu
 pv(112) = pv(112)+d(i0,i1)*d(i0,n0)*d(i0,o0)/mu
 pv(118) = pv(118)+d(i0,i1)*d(i1,n0)*d(i0,o0)/mu
 pv(119) = pv(119)+d(i0,i1)*d2(i0,o0)/mu
 pv(125) = pv(125)+d(i0,i1)*d(i0,o0)*d(i1,o0)/mu
endif
END SUBROUTINE mg4111111_i1
SUBROUTINE mg4111111_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(21) = pv(21)+d2(i0,i1)*d(i0,i2)/mu
 pv(23) = pv(23)+d(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(27) = pv(27)+d(i0,i1)*d(i0,i2)*d(i0,k0)/mu
 pv(35) = pv(35)+d(i0,i1)*d(i0,i2)*d(i0,l0)/mu
 pv(48) = pv(48)+d(i0,i1)*d(i0,i2)*d(i0,m0)/mu
 pv(67) = pv(67)+d(i0,i1)*d(i0,i2)*d(i0,n0)/mu
 pv(93) = pv(93)+d(i0,i1)*d(i0,i2)*d(i0,o0)/mu
endif
END SUBROUTINE mg4111111_ii
END SUBROUTINE mg4111111_isecs
