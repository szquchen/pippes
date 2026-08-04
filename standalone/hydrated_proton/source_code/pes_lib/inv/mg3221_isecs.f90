SUBROUTINE mg3221_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=3, n=2, l=2, mm=m*(m-1), nn=n, ll=l
integer :: i0, i1, i2, j0, j1, k0, k1, l0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg3221_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
l0 = m+n+l
do j0 = m, m+n-1
 do j1 = m, m+n-1
  if (j1.ne.j0) then
   do k0 = m+n, m+n+l-1
    do k1 = m+n, m+n+l-1
     if (k1.ne.k0) then
      do i0 = 0, m-1
       call mg3221_i0 (mxd, m*nn*ll)
       do i1 = 0, m-1
        if (i1.ne.i0) then
         do i2 = 0, m-1
          if (i2.ne.i0.and.i2.ne.i1) then
           call mg3221_ii (mxd, mm*nn*ll)
          endif
         enddo
        endif
       enddo
      enddo
     endif
    enddo
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg3221_i0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/mu
 pv(3) = pv(3)+d(i0,j0)*d(j0,k0)/mu
 pv(4) = pv(4)+d(i0,k0)*d(j0,k0)/mu
 pv(6) = pv(6)+d(i0,j0)*d(i0,l0)/mu
 pv(7) = pv(7)+d(i0,k0)*d(i0,l0)/mu
 pv(8) = pv(8)+d(i0,j0)*d(j0,l0)/mu
 pv(9) = pv(9)+d(j0,k0)*d(j0,l0)/mu
 pv(10) = pv(10)+d(i0,k0)*d(k0,l0)/mu
 pv(11) = pv(11)+d(j0,k0)*d(k0,l0)/mu
endif
if (3.le.mxd) then
 pv(14) = pv(14)+d3(i0,j0)/mu
 pv(20) = pv(20)+d2(i0,j0)*d(i0,k0)/mu
 pv(22) = pv(22)+d(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(24) = pv(24)+d(i0,j0)*d2(i0,k0)/mu
 pv(25) = pv(25)+d3(i0,k0)/mu
 pv(30) = pv(30)+d(i0,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(32) = pv(32)+d2(i0,j0)*d(j0,k0)/mu
 pv(34) = pv(34)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(36) = pv(36)+d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(37) = pv(37)+d2(i0,k0)*d(j0,k0)/mu
 pv(38) = pv(38)+d(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(39) = pv(39)+d(i0,j0)*d2(j0,k0)/mu
 pv(40) = pv(40)+d(i0,k0)*d2(j0,k0)/mu
 pv(41) = pv(41)+d3(j0,k0)/mu
 pv(44) = pv(44)+d2(i0,j0)*d(i0,l0)/mu
 pv(46) = pv(46)+d(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(48) = pv(48)+d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(49) = pv(49)+d2(i0,k0)*d(i0,l0)/mu
 pv(51) = pv(51)+d(i0,k0)*d(i0,k1)*d(i0,l0)/mu
 pv(52) = pv(52)+d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(53) = pv(53)+d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(55) = pv(55)+d(i0,j0)*d2(i0,l0)/mu
 pv(56) = pv(56)+d(i0,k0)*d2(i0,l0)/mu
 pv(58) = pv(58)+d2(i0,j0)*d(j0,l0)/mu
 pv(59) = pv(59)+d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(60) = pv(60)+d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(61) = pv(61)+d2(j0,k0)*d(j0,l0)/mu
 pv(62) = pv(62)+d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(64) = pv(64)+d(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(65) = pv(65)+d2(i0,k0)*d(k0,l0)/mu
 pv(66) = pv(66)+d(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(67) = pv(67)+d2(j0,k0)*d(k0,l0)/mu
 pv(68) = pv(68)+d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(69) = pv(69)+d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(72) = pv(72)+d4(i0,j0)/mu
 pv(76) = pv(76)+d3(i0,j0)*d(i0,j1)/mu
 pv(78) = pv(78)+d3(i0,j0)*d(i0,k0)/mu
 pv(83) = pv(83)+d2(i0,j0)*d2(i0,k0)/mu
 pv(85) = pv(85)+d(i0,j0)*d(i0,j1)*d2(i0,k0)/mu
 pv(87) = pv(87)+d(i0,j0)*d3(i0,k0)/mu
 pv(88) = pv(88)+d4(i0,k0)/mu
 pv(97) = pv(97)+d2(i0,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(99) = pv(99)+d3(i0,k0)*d(i0,k1)/mu
 pv(102) = pv(102)+d3(i0,j0)*d(j0,k0)/mu
 pv(103) = pv(103)+d2(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(106) = pv(106)+d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(112) = pv(112)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(114) = pv(114)+d2(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(116) = pv(116)+d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(118) = pv(118)+d(i0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(119) = pv(119)+d3(i0,k0)*d(j0,k0)/mu
 pv(122) = pv(122)+d2(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(123) = pv(123)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(124) = pv(124)+d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(126) = pv(126)+d(i0,j0)*d2(i0,k1)*d(j0,k0)/mu
 pv(128) = pv(128)+d2(i0,j0)*d2(j0,k0)/mu
 pv(130) = pv(130)+d(i0,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(131) = pv(131)+d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(132) = pv(132)+d2(i0,k0)*d2(j0,k0)/mu
 pv(133) = pv(133)+d(i0,j0)*d(i0,k1)*d2(j0,k0)/mu
 pv(135) = pv(135)+d3(i0,j0)*d(i0,l0)/mu
 pv(138) = pv(138)+d2(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(141) = pv(141)+d(i0,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(142) = pv(142)+d3(i0,k0)*d(i0,l0)/mu
 pv(147) = pv(147)+d2(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(149) = pv(149)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(151) = pv(151)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(152) = pv(152)+d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(154) = pv(154)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(155) = pv(155)+d(i0,j0)*d2(j0,k0)*d(i0,l0)/mu
 pv(156) = pv(156)+d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(157) = pv(157)+d2(i0,j0)*d2(i0,l0)/mu
 pv(159) = pv(159)+d2(i0,k0)*d2(i0,l0)/mu
 pv(161) = pv(161)+d(i0,j0)*d(j0,k0)*d2(i0,l0)/mu
 pv(162) = pv(162)+d(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(165) = pv(165)+d3(i0,j0)*d(j0,l0)/mu
 pv(166) = pv(166)+d2(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(168) = pv(168)+d2(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(169) = pv(169)+d(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(171) = pv(171)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,l0)/mu
 pv(173) = pv(173)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(174) = pv(174)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(175) = pv(175)+d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(177) = pv(177)+d2(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(178) = pv(178)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(179) = pv(179)+d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(180) = pv(180)+d(i0,j0)*d2(i0,l0)*d(j0,l0)/mu
 pv(183) = pv(183)+d2(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(185) = pv(185)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(187) = pv(187)+d(i0,j0)*d2(i0,k0)*d(k0,l0)/mu
 pv(188) = pv(188)+d3(i0,k0)*d(k0,l0)/mu
 pv(189) = pv(189)+d2(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(191) = pv(191)+d2(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(192) = pv(192)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(193) = pv(193)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(195) = pv(195)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(196) = pv(196)+d2(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(197) = pv(197)+d(i0,j0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(198) = pv(198)+d(i0,k0)*d2(i0,l0)*d(k0,l0)/mu
 pv(199) = pv(199)+d(i0,j0)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(201) = pv(201)+d5(i0,j0)/mu
 pv(202) = pv(202)+d4(i0,j0)*d(i0,k0)/mu
 pv(204) = pv(204)+d3(i0,j0)*d2(i0,k0)/mu
 pv(207) = pv(207)+d2(i0,j0)*d3(i0,k0)/mu
 pv(210) = pv(210)+d(i0,j0)*d4(i0,k0)/mu
 pv(211) = pv(211)+d5(i0,k0)/mu
 pv(220) = pv(220)+d3(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(229) = pv(229)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(233) = pv(233)+d(i0,j0)*d2(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(235) = pv(235)+d2(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(240) = pv(240)+d(i0,j0)*d(i0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(242) = pv(242)+d(i0,j0)*d3(i0,k0)*d(j0,k0)/mu
 pv(248) = pv(248)+d2(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(249) = pv(249)+d(i0,j0)*d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(254) = pv(254)+d(i0,j0)*d(i0,k0)*d2(i0,k1)*d(j0,k0)/mu
 pv(257) = pv(257)+d3(i0,j0)*d2(j0,k0)/mu
 pv(258) = pv(258)+d2(i0,j0)*d(i0,j1)*d2(j0,k0)/mu
 pv(261) = pv(261)+d2(i0,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(263) = pv(263)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(264) = pv(264)+d2(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(266) = pv(266)+d(i0,j0)*d2(i0,k0)*d2(j0,k0)/mu
 pv(267) = pv(267)+d(i0,j1)*d2(i0,k0)*d2(j0,k0)/mu
 pv(268) = pv(268)+d3(i0,k0)*d2(j0,k0)/mu
 pv(270) = pv(270)+d2(i0,j0)*d(i0,k1)*d2(j0,k0)/mu
 pv(271) = pv(271)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(272) = pv(272)+d2(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(273) = pv(273)+d(i0,j0)*d2(i0,k1)*d2(j0,k0)/mu
 pv(274) = pv(274)+d4(i0,j0)*d(i0,l0)/mu
 pv(275) = pv(275)+d2(i0,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(277) = pv(277)+d4(i0,k0)*d(i0,l0)/mu
 pv(281) = pv(281)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(286) = pv(286)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(288) = pv(288)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(294) = pv(294)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(297) = pv(297)+d2(i0,j0)*d2(j0,k0)*d(i0,l0)/mu
 pv(299) = pv(299)+d(i0,j0)*d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(300) = pv(300)+d(i0,j1)*d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(301) = pv(301)+d2(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(302) = pv(302)+d(i0,j0)*d(i0,k1)*d2(j0,k0)*d(i0,l0)/mu
 pv(303) = pv(303)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(306) = pv(306)+d(i0,j0)*d2(j0,k0)*d2(i0,l0)/mu
 pv(307) = pv(307)+d(i0,k0)*d2(j0,k0)*d2(i0,l0)/mu
 pv(309) = pv(309)+d2(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(310) = pv(310)+d(i0,j0)*d3(i0,k0)*d(j0,l0)/mu
 pv(316) = pv(316)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(318) = pv(318)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(319) = pv(319)+d2(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(321) = pv(321)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(322) = pv(322)+d(i0,j1)*d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(323) = pv(323)+d3(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(324) = pv(324)+d2(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(325) = pv(325)+d(i0,j0)*d2(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(328) = pv(328)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(329) = pv(329)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(330) = pv(330)+d2(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(331) = pv(331)+d(i0,k0)*d(j0,k0)*d2(i0,l0)*d(j0,l0)/mu
 pv(333) = pv(333)+d3(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(336) = pv(336)+d2(i0,j0)*d2(i0,k0)*d(k0,l0)/mu
 pv(340) = pv(340)+d3(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(341) = pv(341)+d2(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(343) = pv(343)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(344) = pv(344)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(346) = pv(346)+d2(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(347) = pv(347)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(348) = pv(348)+d(i0,j0)*d2(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(349) = pv(349)+d2(i0,j0)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(352) = pv(352)+d2(i0,j0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(353) = pv(353)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(354) = pv(354)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(355) = pv(355)+d(i0,j0)*d(j0,k0)*d2(i0,l0)*d(k0,l0)/mu
 pv(357) = pv(357)+d2(i0,j0)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(358) = pv(358)+d(i0,j0)*d2(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(359) = pv(359)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)*d(k0,l0)/mu
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg3221_i0
SUBROUTINE mg3221_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
 pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/mu
 pv(5) = pv(5)+d(i0,i1)*d(i0,l0)/mu
endif
if (3.le.mxd) then
 pv(12) = pv(12)+d2(i0,i1)*d(i0,j0)/mu
 pv(13) = pv(13)+d(i0,i1)*d2(i0,j0)/mu
 pv(15) = pv(15)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(16) = pv(16)+d2(i0,j0)*d(i1,j0)/mu
 pv(17) = pv(17)+d(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(18) = pv(18)+d2(i0,i1)*d(i0,k0)/mu
 pv(19) = pv(19)+d(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(21) = pv(21)+d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(23) = pv(23)+d(i0,i1)*d2(i0,k0)/mu
 pv(26) = pv(26)+d(i0,i1)*d(i0,k0)*d(i1,k0)/mu
 pv(27) = pv(27)+d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(28) = pv(28)+d2(i0,k0)*d(i1,k0)/mu
 pv(29) = pv(29)+d(i0,i1)*d(i0,k0)*d(i0,k1)/mu
 pv(31) = pv(31)+d(i0,i1)*d(i0,j0)*d(j0,k0)/mu
 pv(33) = pv(33)+d(i0,i1)*d(i0,k0)*d(j0,k0)/mu
 pv(35) = pv(35)+d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(42) = pv(42)+d2(i0,i1)*d(i0,l0)/mu
 pv(43) = pv(43)+d(i0,i1)*d(i0,j0)*d(i0,l0)/mu
 pv(45) = pv(45)+d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(47) = pv(47)+d(i0,i1)*d(i0,k0)*d(i0,l0)/mu
 pv(50) = pv(50)+d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(54) = pv(54)+d(i0,i1)*d2(i0,l0)/mu
 pv(57) = pv(57)+d(i0,i1)*d(i0,j0)*d(j0,l0)/mu
 pv(63) = pv(63)+d(i0,i1)*d(i0,k0)*d(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(70) = pv(70)+d2(i0,i1)*d2(i0,j0)/mu
 pv(71) = pv(71)+d(i0,i1)*d3(i0,j0)/mu
 pv(73) = pv(73)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(74) = pv(74)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(75) = pv(75)+d3(i0,j0)*d(i1,j0)/mu
 pv(77) = pv(77)+d(i0,i1)*d2(i0,j0)*d(i0,k0)/mu
 pv(79) = pv(79)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(80) = pv(80)+d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(81) = pv(81)+d2(i0,i1)*d2(i0,k0)/mu
 pv(82) = pv(82)+d(i0,i1)*d(i0,j0)*d2(i0,k0)/mu
 pv(84) = pv(84)+d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(86) = pv(86)+d(i0,i1)*d3(i0,k0)/mu
 pv(89) = pv(89)+d2(i0,i1)*d(i0,k0)*d(i1,k0)/mu
 pv(90) = pv(90)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(91) = pv(91)+d2(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(92) = pv(92)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(93) = pv(93)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(94) = pv(94)+d(i0,i1)*d2(i0,k0)*d(i1,k0)/mu
 pv(95) = pv(95)+d(i0,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(96) = pv(96)+d3(i0,k0)*d(i1,k0)/mu
 pv(98) = pv(98)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(100) = pv(100)+d2(i0,i1)*d(i0,j0)*d(j0,k0)/mu
 pv(101) = pv(101)+d(i0,i1)*d2(i0,j0)*d(j0,k0)/mu
 pv(104) = pv(104)+d2(i0,i1)*d(i0,k0)*d(j0,k0)/mu
 pv(105) = pv(105)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(107) = pv(107)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(108) = pv(108)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(109) = pv(109)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(110) = pv(110)+d2(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(111) = pv(111)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(113) = pv(113)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(115) = pv(115)+d(i0,i1)*d2(i0,k0)*d(j0,k0)/mu
 pv(117) = pv(117)+d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(120) = pv(120)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(121) = pv(121)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(125) = pv(125)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(127) = pv(127)+d(i0,i1)*d(i0,j0)*d2(j0,k0)/mu
 pv(129) = pv(129)+d(i0,i1)*d(i0,k0)*d2(j0,k0)/mu
 pv(134) = pv(134)+d(i0,i1)*d2(i0,j0)*d(i0,l0)/mu
 pv(136) = pv(136)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(137) = pv(137)+d2(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(139) = pv(139)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(140) = pv(140)+d(i0,i1)*d2(i0,k0)*d(i0,l0)/mu
 pv(143) = pv(143)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(144) = pv(144)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(145) = pv(145)+d2(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(146) = pv(146)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(148) = pv(148)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(150) = pv(150)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(153) = pv(153)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(158) = pv(158)+d(i0,j0)*d(i1,j0)*d2(i0,l0)/mu
 pv(160) = pv(160)+d(i0,k0)*d(i1,k0)*d2(i0,l0)/mu
 pv(163) = pv(163)+d2(i0,i1)*d(i0,j0)*d(j0,l0)/mu
 pv(164) = pv(164)+d(i0,i1)*d2(i0,j0)*d(j0,l0)/mu
 pv(167) = pv(167)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(170) = pv(170)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(172) = pv(172)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(176) = pv(176)+d(i0,i1)*d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(181) = pv(181)+d2(i0,i1)*d(i0,k0)*d(k0,l0)/mu
 pv(182) = pv(182)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(184) = pv(184)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(186) = pv(186)+d(i0,i1)*d2(i0,k0)*d(k0,l0)/mu
 pv(190) = pv(190)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(194) = pv(194)+d(i0,i1)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(200) = pv(200)+d(i0,i1)*d4(i0,j0)/mu
 pv(203) = pv(203)+d(i0,i1)*d2(i0,j0)*d2(i0,k0)/mu
 pv(205) = pv(205)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(206) = pv(206)+d2(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(208) = pv(208)+d(i0,j0)*d(i1,j0)*d3(i0,k0)/mu
 pv(209) = pv(209)+d(i0,i1)*d4(i0,k0)/mu
 pv(212) = pv(212)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(213) = pv(213)+d3(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(214) = pv(214)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(215) = pv(215)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(216) = pv(216)+d2(i0,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(217) = pv(217)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(218) = pv(218)+d2(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(219) = pv(219)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(221) = pv(221)+d2(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(222) = pv(222)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(223) = pv(223)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(224) = pv(224)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(225) = pv(225)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(226) = pv(226)+d(i0,i1)*d2(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(227) = pv(227)+d(i0,j0)*d2(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(228) = pv(228)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(230) = pv(230)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(231) = pv(231)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(232) = pv(232)+d2(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(234) = pv(234)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(236) = pv(236)+d(i0,i1)*d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(237) = pv(237)+d(i0,i2)*d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(238) = pv(238)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(239) = pv(239)+d2(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(241) = pv(241)+d(i1,j0)*d(i0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(243) = pv(243)+d(i1,j0)*d3(i0,k0)*d(j0,k0)/mu
 pv(244) = pv(244)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(245) = pv(245)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(246) = pv(246)+d(i0,j0)*d2(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(247) = pv(247)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(250) = pv(250)+d(i1,j0)*d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(251) = pv(251)+d(i0,i1)*d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(252) = pv(252)+d2(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(253) = pv(253)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(255) = pv(255)+d2(i0,i1)*d(i0,j0)*d2(j0,k0)/mu
 pv(256) = pv(256)+d(i0,i1)*d2(i0,j0)*d2(j0,k0)/mu
 pv(259) = pv(259)+d2(i0,i1)*d(i0,k0)*d2(j0,k0)/mu
 pv(260) = pv(260)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(262) = pv(262)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(265) = pv(265)+d(i0,i1)*d2(i0,k0)*d2(j0,k0)/mu
 pv(269) = pv(269)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d2(j0,k0)/mu
 pv(276) = pv(276)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(278) = pv(278)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(279) = pv(279)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(280) = pv(280)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(282) = pv(282)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(283) = pv(283)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(284) = pv(284)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(285) = pv(285)+d2(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(287) = pv(287)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(289) = pv(289)+d(i1,j0)*d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(290) = pv(290)+d(i0,i1)*d(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(291) = pv(291)+d2(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(292) = pv(292)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(293) = pv(293)+d(i1,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(295) = pv(295)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(296) = pv(296)+d(i0,i1)*d(i0,j0)*d2(j0,k0)*d(i0,l0)/mu
 pv(298) = pv(298)+d(i0,i1)*d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(304) = pv(304)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(305) = pv(305)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(308) = pv(308)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(311) = pv(311)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(312) = pv(312)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(313) = pv(313)+d(i0,j0)*d2(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(314) = pv(314)+d2(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(315) = pv(315)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(317) = pv(317)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(320) = pv(320)+d(i0,i1)*d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(326) = pv(326)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(327) = pv(327)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(332) = pv(332)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(334) = pv(334)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(335) = pv(335)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(337) = pv(337)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(k0,l0)/mu
 pv(338) = pv(338)+d2(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(339) = pv(339)+d(i0,i1)*d2(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(342) = pv(342)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(345) = pv(345)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(350) = pv(350)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(351) = pv(351)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(356) = pv(356)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg3221_ii
END SUBROUTINE mg3221_isecs
