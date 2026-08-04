SUBROUTINE mg5_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=nk, m2=m*(m-1), m3=m2*(m-2), mm=m3*(m-3)
integer :: i0, i1, i2, i3
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5, d6, &
  d7, d8, d9
if (size(r).ne.nk*nk) then
 stop 'mg5_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5, d6, d7, d8, d9)
pv = 0
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg5_i1 (mxd, m2)
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg5_i2 (mxd, m3)
     do i3 = 0, m-1
      if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
       call mg5_ii (mxd, mm)
      endif
     enddo
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg5_i1 (mxd, mu)
integer, intent (in) :: mxd, mu
if (7.le.mxd) then
 pv(27) = pv(27)+d7(i0,i1)/mu
endif
if (8.le.mxd) then
 pv(40) = pv(40)+d8(i0,i1)/mu
endif
if (9.le.mxd) then
 pv(44) = pv(44)+d9(i0,i1)/mu
endif
END SUBROUTINE mg5_i1
SUBROUTINE mg5_i2 (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(0) = pv(0)+d2(i0,i1)*d(i0,i2)/mu
 pv(1) = pv(1)+d(i0,i1)*d(i0,i2)*d(i1,i2)/mu
endif
if (4.le.mxd) then
 pv(2) = pv(2)+d3(i0,i1)*d(i0,i2)/mu
 pv(3) = pv(3)+d2(i0,i1)*d2(i0,i2)/mu
 pv(4) = pv(4)+d2(i0,i1)*d(i0,i2)*d(i1,i2)/mu
endif
if (5.le.mxd) then
 pv(7) = pv(7)+d4(i0,i1)*d(i0,i2)/mu
 pv(8) = pv(8)+d3(i0,i1)*d2(i0,i2)/mu
 pv(9) = pv(9)+d3(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(10) = pv(10)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
endif
if (6.le.mxd) then
 pv(15) = pv(15)+d5(i0,i1)*d(i0,i2)/mu
 pv(16) = pv(16)+d4(i0,i1)*d2(i0,i2)/mu
 pv(17) = pv(17)+d3(i0,i1)*d3(i0,i2)/mu
 pv(18) = pv(18)+d4(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(19) = pv(19)+d3(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(20) = pv(20)+d2(i0,i1)*d2(i0,i2)*d2(i1,i2)/mu
endif
if (7.le.mxd) then
 pv(28) = pv(28)+d6(i0,i1)*d(i0,i2)/mu
 pv(29) = pv(29)+d5(i0,i1)*d2(i0,i2)/mu
 pv(30) = pv(30)+d4(i0,i1)*d3(i0,i2)/mu
 pv(31) = pv(31)+d5(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(32) = pv(32)+d4(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
 pv(33) = pv(33)+d3(i0,i1)*d3(i0,i2)*d(i1,i2)/mu
 pv(34) = pv(34)+d3(i0,i1)*d2(i0,i2)*d2(i1,i2)/mu
endif
if (8.le.mxd) then
 pv(41) = pv(41)+d7(i0,i1)*d(i0,i2)/mu
 pv(42) = pv(42)+d6(i0,i1)*d2(i0,i2)/mu
 pv(43) = pv(43)+d5(i0,i1)*d3(i0,i2)/mu
endif
if (9.le.mxd) then
 pv(45) = pv(45)+d8(i0,i1)*d(i0,i2)/mu
endif
END SUBROUTINE mg5_i2
SUBROUTINE mg5_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(5) = pv(5)+d2(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(6) = pv(6)+d2(i0,i1)*d(i1,i2)*d(i0,i3)/mu
endif
if (5.le.mxd) then
 pv(11) = pv(11)+d3(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(12) = pv(12)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(13) = pv(13)+d3(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(14) = pv(14)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
endif
if (6.le.mxd) then
 pv(21) = pv(21)+d4(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(22) = pv(22)+d3(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(23) = pv(23)+d4(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(24) = pv(24)+d3(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(25) = pv(25)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(26) = pv(26)+d3(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
endif
if (7.le.mxd) then
 pv(35) = pv(35)+d5(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(36) = pv(36)+d4(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(37) = pv(37)+d3(i0,i1)*d3(i0,i2)*d(i0,i3)/mu
 pv(38) = pv(38)+d5(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(39) = pv(39)+d4(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
endif
END SUBROUTINE mg5_ii
END SUBROUTINE mg5_isecs
