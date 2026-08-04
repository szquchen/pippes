SUBROUTINE mg311_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg311_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg311_prib: bad size u'
!! else if (size(w).ne.mg311_npb(mxd)) then
!!  stop 'mg311_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 4 3 3 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 4 13 35 83 179 361 685 1240 2155
! constant term
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
 w(4) = u(3)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(5:8) = u(0)*w(1:4)
 w(9:11) = u(1)*w(2:4)
 w(12:13) = u(2)*w(3:4)
 w(14) = u(3)*w(4)
 w(15) = u(4)*w(0)
 w(16) = u(5)*w(0)
 w(17) = u(6)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(18:30) = u(0)*w(5:17)
 w(31:39) = u(1)*w(9:17)
 w(40:45) = u(2)*w(12:17)
 w(46:49) = u(3)*w(14:17)
 w(50) = u(7)*w(0)
 w(51) = u(8)*w(0)
 w(52) = u(9)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(53:87) = u(0)*w(18:52)
 w(88:109) = u(1)*w(31:52)
 w(110:122) = u(2)*w(40:52)
 w(123:129) = u(3)*w(46:52)
 w(130:132) = u(4)*w(15:17)
 w(133:134) = u(5)*w(16:17)
 w(135) = u(6)*w(17)
endif
! terms of degree 5
if (5.le.mxd) then
 w(136:218) = u(0)*w(53:135)
 w(219:266) = u(1)*w(88:135)
 w(267:292) = u(2)*w(110:135)
 w(293:305) = u(3)*w(123:135)
 w(306:308) = u(4)*w(50:52)
 w(309:311) = u(5)*w(50:52)
 w(312:314) = u(6)*w(50:52)
endif
! terms of degree 6
if (6.le.mxd) then
 w(315:493) = u(0)*w(136:314)
 w(494:589) = u(1)*w(219:314)
 w(590:637) = u(2)*w(267:314)
 w(638:659) = u(3)*w(293:314)
 w(660:665) = u(4)*w(130:135)
 w(666:668) = u(5)*w(133:135)
 w(669) = u(6)*w(135)
 w(670:672) = u(7)*w(50:52)
 w(673:674) = u(8)*w(51:52)
 w(675) = u(9)*w(52)
endif
! terms of degree 7
if (7.le.mxd) then
 w(676:1036) = u(0)*w(315:675)
 w(1037:1218) = u(1)*w(494:675)
 w(1219:1304) = u(2)*w(590:675)
 w(1305:1342) = u(3)*w(638:675)
 w(1343:1351) = u(4)*w(306:314)
 w(1352:1357) = u(5)*w(309:314)
 w(1358:1360) = u(6)*w(312:314)
endif
! terms of degree 8
if (8.le.mxd) then
 w(1361:2045) = u(0)*w(676:1360)
 w(2046:2369) = u(1)*w(1037:1360)
 w(2370:2511) = u(2)*w(1219:1360)
 w(2512:2567) = u(3)*w(1305:1360)
 w(2568:2583) = u(4)*w(660:675)
 w(2584:2593) = u(5)*w(666:675)
 w(2594:2600) = u(6)*w(669:675)
endif
! terms of degree 9
if (9.le.mxd) then
 w(2601:3840) = u(0)*w(1361:2600)
 w(3841:4395) = u(1)*w(2046:2600)
 w(4396:4626) = u(2)*w(2370:2600)
 w(4627:4715) = u(3)*w(2512:2600)
 w(4716:4733) = u(4)*w(1343:1360)
 w(4734:4742) = u(5)*w(1352:1360)
 w(4743:4745) = u(6)*w(1358:1360)
 w(4746:4751) = u(7)*w(670:675)
 w(4752:4754) = u(8)*w(673:675)
 w(4755) = u(9)*w(675)
endif
END SUBROUTINE mg311_prib
