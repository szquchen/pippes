SUBROUTINE mg73_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=7, n=3, m2=m*(m-1), m3=m2*(m-2), m4=m3*(m-3), &
  m5=m4*(m-4), mm=m5*(m-5), nn=n*(n-1)
integer :: i0, i1, i2, i3, i4, i5, i6, j0, j1, j2
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg73_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg73_i2 (mxd, m3)
     do i3 = 0, m-1
      if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
       call mg73_i3 (mxd, m4)
       do i4 = 0, m-1
        if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
         call mg73_i4 (mxd, m5)
         do i5 = 0, m-1
          if (i5.ne.i0.and.i5.ne.i1.and.i5.ne.i2.and.i5.ne.i3.and. &
            i5.ne.i4) then
           do i6 = 0, m-1
            if (i6.ne.i0.and.i6.ne.i1.and.i6.ne.i2.and.i6.ne.i3.and. &
              i6.ne.i4.and.i6.ne.i5) then
             call mg73_ii (mxd, mm)
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
  endif
 enddo
enddo
do j0 = m, m+n-1
 do i0 = 0, m-1
  do i1 = 0, m-1
   if (i1.ne.i0) then
    call mg73_i1j0 (mxd, m2*n)
    do i2 = 0, m-1
     if (i2.ne.i0.and.i2.ne.i1) then
      call mg73_i2j0 (mxd, m3*n)
      do i3 = 0, m-1
       if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
        call mg73_i3j0 (mxd, m4*n)
        do i4 = 0, m-1
         if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
          call mg73_i4j0 (mxd, m5*n)
          do i5 = 0, m-1
           if (i5.ne.i0.and.i5.ne.i1.and.i5.ne.i2.and.i5.ne.i3.and. &
             i5.ne.i4) then
            do i6 = 0, m-1
             if (i6.ne.i0.and.i6.ne.i1.and.i6.ne.i2.and.i6.ne.i3.and. &
               i6.ne.i4.and.i6.ne.i5) then
              call mg73_iij0 (mxd, mm*n)
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
   endif
  enddo
 enddo
 do j1 = m, m+n-1
  if (j1.ne.j0) then
   do j2 = m, m+n-1
    if (j2.ne.j0.and.j2.ne.j1) then
     do i0 = 0, m-1
      call mg73_i0jj (mxd, m*nn)
      do i1 = 0, m-1
       if (i1.ne.i0) then
        call mg73_i1jj (mxd, m2*nn)
        do i2 = 0, m-1
         if (i2.ne.i0.and.i2.ne.i1) then
          call mg73_i2jj (mxd, m3*nn)
          do i3 = 0, m-1
           if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
            call mg73_i3jj (mxd, m4*nn)
            do i4 = 0, m-1
             if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
              call mg73_i4jj (mxd, m5*nn)
              do i5 = 0, m-1
               if (i5.ne.i0.and.i5.ne.i1.and.i5.ne.i2.and.i5.ne.i3.and. &
                 i5.ne.i4) then
                do i6 = 0, m-1
                 if (i6.ne.i0.and.i6.ne.i1.and.i6.ne.i2.and.i6.ne.i3.and. &
                   i6.ne.i4.and.i6.ne.i5) then
                  call mg73_iijj (mxd, mm*nn)
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
       endif
      enddo
     enddo
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg73_i2 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(18) = pv(18)+d2(i0,i1)*d2(i0,i2)/mu
 pv(19) = pv(19)+d2(i0,i1)*d(i0,i2)*d(i1,i2)/mu
endif
if (5.le.mxd) then
 pv(89) = pv(89)+d3(i0,i1)*d2(i0,i2)/mu
 pv(90) = pv(90)+d3(i0,i1)*d(i0,i2)*d(i1,i2)/mu
 pv(91) = pv(91)+d2(i0,i1)*d2(i0,i2)*d(i1,i2)/mu
endif
END SUBROUTINE mg73_i2
SUBROUTINE mg73_i3 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(20) = pv(20)+d2(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(21) = pv(21)+d2(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(22) = pv(22)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(23) = pv(23)+d(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(24) = pv(24)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
endif
if (5.le.mxd) then
 pv(92) = pv(92)+d3(i0,i1)*d(i0,i2)*d(i0,i3)/mu
 pv(93) = pv(93)+d2(i0,i1)*d2(i0,i2)*d(i0,i3)/mu
 pv(94) = pv(94)+d3(i0,i1)*d(i1,i2)*d(i0,i3)/mu
 pv(95) = pv(95)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)/mu
 pv(96) = pv(96)+d2(i0,i1)*d2(i1,i2)*d(i0,i3)/mu
 pv(97) = pv(97)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,i3)/mu
 pv(98) = pv(98)+d(i0,i1)*d3(i1,i2)*d(i0,i3)/mu
 pv(99) = pv(99)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,i3)/mu
 pv(100) = pv(100)+d(i0,i1)*d2(i1,i2)*d2(i0,i3)/mu
 pv(101) = pv(101)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
 pv(102) = pv(102)+d2(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)/mu
endif
END SUBROUTINE mg73_i3
SUBROUTINE mg73_i4 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(25) = pv(25)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
endif
if (5.le.mxd) then
 pv(103) = pv(103)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(104) = pv(104)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(105) = pv(105)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(106) = pv(106)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,i4)/mu
 pv(107) = pv(107)+d(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,i4)/mu
 pv(108) = pv(108)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(109) = pv(109)+d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)/mu
 pv(110) = pv(110)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,i4)/mu
 pv(111) = pv(111)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,i4)/mu
 pv(112) = pv(112)+d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,i4)*d(i1,i4)/mu
endif
END SUBROUTINE mg73_i4
SUBROUTINE mg73_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
 pv(113) = pv(113)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,i5)/mu
endif
END SUBROUTINE mg73_ii
SUBROUTINE mg73_i1j0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
endif
if (3.le.mxd) then
 pv(2) = pv(2)+d2(i0,i1)*d(i0,j0)/mu
 pv(5) = pv(5)+d(i0,i1)*d2(i0,j0)/mu
 pv(6) = pv(6)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(8) = pv(8)+d2(i0,j0)*d(i1,j0)/mu
endif
if (4.le.mxd) then
 pv(26) = pv(26)+d3(i0,i1)*d(i0,j0)/mu
 pv(35) = pv(35)+d2(i0,i1)*d2(i0,j0)/mu
 pv(38) = pv(38)+d(i0,i1)*d3(i0,j0)/mu
 pv(39) = pv(39)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(46) = pv(46)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(49) = pv(49)+d2(i0,j0)*d2(i1,j0)/mu
endif
if (5.le.mxd) then
 pv(114) = pv(114)+d4(i0,i1)*d(i0,j0)/mu
 pv(142) = pv(142)+d3(i0,i1)*d2(i0,j0)/mu
 pv(151) = pv(151)+d2(i0,i1)*d3(i0,j0)/mu
 pv(154) = pv(154)+d(i0,i1)*d4(i0,j0)/mu
 pv(155) = pv(155)+d3(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(177) = pv(177)+d2(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(188) = pv(188)+d(i0,i1)*d3(i0,j0)*d(i1,j0)/mu
 pv(191) = pv(191)+d4(i0,j0)*d(i1,j0)/mu
 pv(192) = pv(192)+d(i0,i1)*d2(i0,j0)*d2(i1,j0)/mu
 pv(194) = pv(194)+d3(i0,j0)*d2(i1,j0)/mu
endif
END SUBROUTINE mg73_i1j0
SUBROUTINE mg73_i2j0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(3) = pv(3)+d(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(4) = pv(4)+d(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(7) = pv(7)+d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
endif
if (4.le.mxd) then
 pv(27) = pv(27)+d2(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(28) = pv(28)+d2(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(29) = pv(29)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(30) = pv(30)+d(i0,i1)*d2(i1,i2)*d(i0,j0)/mu
 pv(36) = pv(36)+d(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(37) = pv(37)+d(i0,i1)*d(i1,i2)*d2(i0,j0)/mu
 pv(40) = pv(40)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(41) = pv(41)+d2(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(42) = pv(42)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(47) = pv(47)+d(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(48) = pv(48)+d(i1,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(50) = pv(50)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(52) = pv(52)+d2(i0,j0)*d(i1,j0)*d(i2,j0)/mu
endif
if (5.le.mxd) then
 pv(115) = pv(115)+d3(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(116) = pv(116)+d2(i0,i1)*d2(i0,i2)*d(i0,j0)/mu
 pv(117) = pv(117)+d3(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(118) = pv(118)+d2(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)/mu
 pv(119) = pv(119)+d2(i0,i1)*d2(i1,i2)*d(i0,j0)/mu
 pv(120) = pv(120)+d(i0,i1)*d(i0,i2)*d2(i1,i2)*d(i0,j0)/mu
 pv(121) = pv(121)+d(i0,i1)*d3(i1,i2)*d(i0,j0)/mu
 pv(143) = pv(143)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(144) = pv(144)+d2(i0,i1)*d(i1,i2)*d2(i0,j0)/mu
 pv(145) = pv(145)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d2(i0,j0)/mu
 pv(146) = pv(146)+d(i0,i1)*d2(i1,i2)*d2(i0,j0)/mu
 pv(152) = pv(152)+d(i0,i1)*d(i0,i2)*d3(i0,j0)/mu
 pv(153) = pv(153)+d(i0,i1)*d(i1,i2)*d3(i0,j0)/mu
 pv(156) = pv(156)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(157) = pv(157)+d(i0,i1)*d2(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(158) = pv(158)+d3(i0,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(159) = pv(159)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(160) = pv(160)+d2(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)/mu
 pv(178) = pv(178)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(179) = pv(179)+d2(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(180) = pv(180)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(181) = pv(181)+d(i0,i2)*d(i1,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(182) = pv(182)+d2(i1,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(189) = pv(189)+d(i0,i2)*d3(i0,j0)*d(i1,j0)/mu
 pv(190) = pv(190)+d(i1,i2)*d3(i0,j0)*d(i1,j0)/mu
 pv(193) = pv(193)+d(i0,i2)*d2(i0,j0)*d2(i1,j0)/mu
 pv(195) = pv(195)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(196) = pv(196)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(204) = pv(204)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(205) = pv(205)+d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(208) = pv(208)+d3(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(209) = pv(209)+d2(i0,j0)*d2(i1,j0)*d(i2,j0)/mu
endif
END SUBROUTINE mg73_i2j0
SUBROUTINE mg73_i3j0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(31) = pv(31)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(32) = pv(32)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(33) = pv(33)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(34) = pv(34)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(43) = pv(43)+d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(44) = pv(44)+d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(45) = pv(45)+d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(51) = pv(51)+d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
endif
if (5.le.mxd) then
 pv(122) = pv(122)+d2(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(123) = pv(123)+d2(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(124) = pv(124)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(125) = pv(125)+d(i0,i1)*d2(i1,i2)*d(i0,i3)*d(i0,j0)/mu
 pv(126) = pv(126)+d(i0,i1)*d(i1,i2)*d2(i0,i3)*d(i0,j0)/mu
 pv(127) = pv(127)+d2(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(128) = pv(128)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(129) = pv(129)+d2(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(130) = pv(130)+d(i0,i1)*d2(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(131) = pv(131)+d(i0,i2)*d2(i1,i2)*d(i1,i3)*d(i0,j0)/mu
 pv(132) = pv(132)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,j0)/mu
 pv(133) = pv(133)+d2(i1,i2)*d(i0,i3)*d(i1,i3)*d(i0,j0)/mu
 pv(134) = pv(134)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i2,i3)*d(i0,j0)/mu
 pv(147) = pv(147)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(148) = pv(148)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d2(i0,j0)/mu
 pv(149) = pv(149)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d2(i0,j0)/mu
 pv(150) = pv(150)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d2(i0,j0)/mu
 pv(161) = pv(161)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(162) = pv(162)+d2(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(163) = pv(163)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(164) = pv(164)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(165) = pv(165)+d2(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(166) = pv(166)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(167) = pv(167)+d2(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(168) = pv(168)+d(i0,i2)*d(i1,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(169) = pv(169)+d(i0,i2)*d(i0,i3)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(170) = pv(170)+d(i1,i2)*d(i0,i3)*d(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(171) = pv(171)+d(i0,i2)*d2(i2,i3)*d(i0,j0)*d(i1,j0)/mu
 pv(183) = pv(183)+d(i0,i2)*d(i0,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(184) = pv(184)+d(i1,i2)*d(i0,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(185) = pv(185)+d(i1,i2)*d(i1,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(186) = pv(186)+d(i0,i2)*d(i2,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(187) = pv(187)+d(i1,i2)*d(i2,i3)*d2(i0,j0)*d(i1,j0)/mu
 pv(197) = pv(197)+d(i0,i1)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(198) = pv(198)+d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(199) = pv(199)+d2(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(200) = pv(200)+d(i0,i3)*d(i1,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(206) = pv(206)+d(i0,i3)*d2(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(207) = pv(207)+d(i1,i3)*d2(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(210) = pv(210)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i3,j0)/mu
endif
END SUBROUTINE mg73_i3j0
SUBROUTINE mg73_i4j0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
 pv(135) = pv(135)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)/mu
 pv(136) = pv(136)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)/mu
 pv(137) = pv(137)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,j0)/mu
 pv(138) = pv(138)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,i4)*d(i0,j0)/mu
 pv(139) = pv(139)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i1,i4)*d(i0,j0)/mu
 pv(140) = pv(140)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i1,i4)*d(i0,j0)/mu
 pv(141) = pv(141)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i1,i4)*d(i0,j0)/mu
 pv(172) = pv(172)+d(i0,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)/mu
 pv(173) = pv(173)+d(i1,i2)*d(i0,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)/mu
 pv(174) = pv(174)+d(i0,i2)*d(i2,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)/mu
 pv(175) = pv(175)+d(i1,i2)*d(i2,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)/mu
 pv(176) = pv(176)+d(i0,i2)*d(i2,i3)*d(i2,i4)*d(i0,j0)*d(i1,j0)/mu
 pv(201) = pv(201)+d(i0,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(202) = pv(202)+d(i1,i3)*d(i0,i4)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
 pv(203) = pv(203)+d(i0,i3)*d(i3,i4)*d(i0,j0)*d(i1,j0)*d(i2,j0)/mu
endif
END SUBROUTINE mg73_i4j0
SUBROUTINE mg73_iij0 (mxd, mu)
integer, intent (in) :: mxd, mu
END SUBROUTINE mg73_iij0
SUBROUTINE mg73_i0jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(1) = pv(1)+d(i0,j0)*d(j0,j1)/mu
endif
if (3.le.mxd) then
 pv(10) = pv(10)+d2(i0,j0)*d(i0,j1)/mu
 pv(14) = pv(14)+d2(i0,j0)*d(j0,j1)/mu
 pv(16) = pv(16)+d(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(17) = pv(17)+d(i0,j0)*d2(j0,j1)/mu
endif
if (4.le.mxd) then
 pv(68) = pv(68)+d2(i0,j0)*d2(i0,j1)/mu
 pv(77) = pv(77)+d3(i0,j0)*d(j0,j1)/mu
 pv(82) = pv(82)+d2(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(85) = pv(85)+d2(i0,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(87) = pv(87)+d2(i0,j0)*d2(j0,j1)/mu
 pv(88) = pv(88)+d(i0,j0)*d(i0,j1)*d2(j0,j1)/mu
endif
if (5.le.mxd) then
 pv(224) = pv(224)+d4(i0,j0)*d(i0,j1)/mu
 pv(277) = pv(277)+d3(i0,j0)*d2(i0,j1)/mu
 pv(297) = pv(297)+d3(i0,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(323) = pv(323)+d4(i0,j0)*d(j0,j1)/mu
 pv(341) = pv(341)+d3(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(352) = pv(352)+d2(i0,j0)*d2(i0,j1)*d(j0,j1)/mu
 pv(356) = pv(356)+d3(i0,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(361) = pv(361)+d2(i0,j0)*d(i0,j1)*d(i0,j2)*d(j0,j1)/mu
 pv(367) = pv(367)+d3(i0,j0)*d2(j0,j1)/mu
 pv(370) = pv(370)+d2(i0,j0)*d(i0,j1)*d2(j0,j1)/mu
 pv(372) = pv(372)+d2(i0,j0)*d(i0,j2)*d2(j0,j1)/mu
endif
END SUBROUTINE mg73_i0jj
SUBROUTINE mg73_i1jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(9) = pv(9)+d(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(11) = pv(11)+d(i0,i1)*d(i1,j0)*d(i0,j1)/mu
 pv(12) = pv(12)+d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(13) = pv(13)+d(i0,i1)*d(i0,j0)*d(j0,j1)/mu
 pv(15) = pv(15)+d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
endif
if (4.le.mxd) then
 pv(53) = pv(53)+d2(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(56) = pv(56)+d(i0,i1)*d2(i0,j0)*d(i0,j1)/mu
 pv(57) = pv(57)+d2(i0,i1)*d(i1,j0)*d(i0,j1)/mu
 pv(60) = pv(60)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(63) = pv(63)+d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(64) = pv(64)+d(i0,i1)*d2(i1,j0)*d(i0,j1)/mu
 pv(65) = pv(65)+d(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(69) = pv(69)+d(i0,j0)*d(i1,j0)*d2(i0,j1)/mu
 pv(70) = pv(70)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)/mu
 pv(71) = pv(71)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(72) = pv(72)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(73) = pv(73)+d2(i0,i1)*d(i0,j0)*d(j0,j1)/mu
 pv(76) = pv(76)+d(i0,i1)*d2(i0,j0)*d(j0,j1)/mu
 pv(78) = pv(78)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(80) = pv(80)+d2(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(81) = pv(81)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(83) = pv(83)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(84) = pv(84)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(86) = pv(86)+d(i0,i1)*d(i0,j0)*d2(j0,j1)/mu
endif
if (5.le.mxd) then
 pv(211) = pv(211)+d3(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(220) = pv(220)+d2(i0,i1)*d2(i0,j0)*d(i0,j1)/mu
 pv(223) = pv(223)+d(i0,i1)*d3(i0,j0)*d(i0,j1)/mu
 pv(225) = pv(225)+d3(i0,i1)*d(i1,j0)*d(i0,j1)/mu
 pv(235) = pv(235)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(246) = pv(246)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(249) = pv(249)+d3(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(250) = pv(250)+d2(i0,i1)*d2(i1,j0)*d(i0,j1)/mu
 pv(254) = pv(254)+d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(257) = pv(257)+d2(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(258) = pv(258)+d(i0,i1)*d3(i1,j0)*d(i0,j1)/mu
 pv(259) = pv(259)+d(i0,j0)*d3(i1,j0)*d(i0,j1)/mu
 pv(276) = pv(276)+d(i0,i1)*d2(i0,j0)*d2(i0,j1)/mu
 pv(278) = pv(278)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,j1)/mu
 pv(281) = pv(281)+d2(i0,j0)*d(i1,j0)*d2(i0,j1)/mu
 pv(282) = pv(282)+d(i0,i1)*d2(i1,j0)*d2(i0,j1)/mu
 pv(283) = pv(283)+d(i0,j0)*d2(i1,j0)*d2(i0,j1)/mu
 pv(286) = pv(286)+d(i0,j0)*d(i1,j0)*d3(i0,j1)/mu
 pv(287) = pv(287)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)/mu
 pv(289) = pv(289)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)/mu
 pv(293) = pv(293)+d2(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(296) = pv(296)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(298) = pv(298)+d2(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(302) = pv(302)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(303) = pv(303)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(304) = pv(304)+d(i0,i1)*d2(i1,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(306) = pv(306)+d(i0,i1)*d(i1,j0)*d2(i0,j1)*d(i0,j2)/mu
 pv(307) = pv(307)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i1,j1)*d(i0,j2)/mu
 pv(308) = pv(308)+d2(i0,j0)*d(i1,j0)*d(i1,j1)*d(i0,j2)/mu
 pv(310) = pv(310)+d3(i0,i1)*d(i0,j0)*d(j0,j1)/mu
 pv(319) = pv(319)+d2(i0,i1)*d2(i0,j0)*d(j0,j1)/mu
 pv(322) = pv(322)+d(i0,i1)*d3(i0,j0)*d(j0,j1)/mu
 pv(324) = pv(324)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(331) = pv(331)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(334) = pv(334)+d3(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(335) = pv(335)+d2(i0,j0)*d2(i1,j0)*d(j0,j1)/mu
 pv(337) = pv(337)+d2(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(340) = pv(340)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(342) = pv(342)+d2(i0,i1)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(345) = pv(345)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(348) = pv(348)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(349) = pv(349)+d(i0,i1)*d2(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(350) = pv(350)+d(i0,j0)*d2(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(353) = pv(353)+d(i0,j0)*d(i1,j0)*d2(i0,j1)*d(j0,j1)/mu
 pv(354) = pv(354)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)*d(j0,j1)/mu
 pv(355) = pv(355)+d(i0,i1)*d2(i0,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(358) = pv(358)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(359) = pv(359)+d2(i0,j0)*d(i1,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(360) = pv(360)+d(i0,i1)*d2(i1,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(362) = pv(362)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,j2)*d(j0,j1)/mu
 pv(363) = pv(363)+d2(i0,i1)*d(i0,j0)*d2(j0,j1)/mu
 pv(366) = pv(366)+d(i0,i1)*d2(i0,j0)*d2(j0,j1)/mu
 pv(368) = pv(368)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(j0,j1)/mu
 pv(369) = pv(369)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d2(j0,j1)/mu
 pv(371) = pv(371)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d2(j0,j1)/mu
endif
END SUBROUTINE mg73_i1jj
SUBROUTINE mg73_i2jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(54) = pv(54)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(55) = pv(55)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(58) = pv(58)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(59) = pv(59)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(61) = pv(61)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(62) = pv(62)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(66) = pv(66)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(67) = pv(67)+d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(74) = pv(74)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,j1)/mu
 pv(75) = pv(75)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,j1)/mu
 pv(79) = pv(79)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
endif
if (5.le.mxd) then
 pv(212) = pv(212)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(213) = pv(213)+d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(214) = pv(214)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(215) = pv(215)+d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(i0,j1)/mu
 pv(221) = pv(221)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i0,j1)/mu
 pv(222) = pv(222)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(i0,j1)/mu
 pv(226) = pv(226)+d2(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(227) = pv(227)+d(i0,i1)*d2(i0,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(228) = pv(228)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(229) = pv(229)+d2(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)/mu
 pv(236) = pv(236)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(237) = pv(237)+d2(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(238) = pv(238)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(239) = pv(239)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(240) = pv(240)+d2(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(247) = pv(247)+d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(248) = pv(248)+d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(251) = pv(251)+d(i0,i1)*d(i0,i2)*d2(i1,j0)*d(i0,j1)/mu
 pv(252) = pv(252)+d(i0,i1)*d(i1,i2)*d2(i1,j0)*d(i0,j1)/mu
 pv(253) = pv(253)+d(i0,i2)*d(i1,i2)*d2(i1,j0)*d(i0,j1)/mu
 pv(255) = pv(255)+d(i0,i2)*d(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(256) = pv(256)+d(i1,i2)*d(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(260) = pv(260)+d2(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(261) = pv(261)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(262) = pv(262)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(267) = pv(267)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(268) = pv(268)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(271) = pv(271)+d2(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(272) = pv(272)+d(i0,i1)*d2(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(273) = pv(273)+d(i0,i2)*d2(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(274) = pv(274)+d(i0,j0)*d2(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(279) = pv(279)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d2(i0,j1)/mu
 pv(280) = pv(280)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d2(i0,j1)/mu
 pv(284) = pv(284)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d2(i0,j1)/mu
 pv(285) = pv(285)+d(i0,j0)*d(i1,j0)*d(i2,j0)*d2(i0,j1)/mu
 pv(288) = pv(288)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i1,j1)/mu
 pv(290) = pv(290)+d(i0,i1)*d(i0,j0)*d(i2,j0)*d(i0,j1)*d(i1,j1)/mu
 pv(291) = pv(291)+d(i1,i2)*d(i0,j0)*d(i2,j0)*d(i0,j1)*d(i1,j1)/mu
 pv(292) = pv(292)+d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)*d(i1,j1)/mu
 pv(294) = pv(294)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(295) = pv(295)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(299) = pv(299)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(300) = pv(300)+d(i0,i1)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(301) = pv(301)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(305) = pv(305)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)*d(i0,j2)/mu
 pv(309) = pv(309)+d(i0,i1)*d(i0,i2)*d(i2,j0)*d(i1,j1)*d(i0,j2)/mu
 pv(311) = pv(311)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(j0,j1)/mu
 pv(312) = pv(312)+d2(i0,i1)*d(i1,i2)*d(i0,j0)*d(j0,j1)/mu
 pv(313) = pv(313)+d(i0,i1)*d(i0,i2)*d(i1,i2)*d(i0,j0)*d(j0,j1)/mu
 pv(314) = pv(314)+d(i0,i1)*d2(i1,i2)*d(i0,j0)*d(j0,j1)/mu
 pv(320) = pv(320)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(j0,j1)/mu
 pv(321) = pv(321)+d(i0,i1)*d(i1,i2)*d2(i0,j0)*d(j0,j1)/mu
 pv(325) = pv(325)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(326) = pv(326)+d2(i0,i2)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(327) = pv(327)+d(i0,i2)*d(i1,i2)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(332) = pv(332)+d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(333) = pv(333)+d(i1,i2)*d2(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(336) = pv(336)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(j0,j1)/mu
 pv(338) = pv(338)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(339) = pv(339)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(343) = pv(343)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(344) = pv(344)+d(i0,i2)*d(i1,i2)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(346) = pv(346)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(347) = pv(347)+d(i1,i2)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(351) = pv(351)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(357) = pv(357)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(364) = pv(364)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d2(j0,j1)/mu
 pv(365) = pv(365)+d(i0,i1)*d(i1,i2)*d(i0,j0)*d2(j0,j1)/mu
endif
END SUBROUTINE mg73_i2jj
SUBROUTINE mg73_i3jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
 pv(216) = pv(216)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i0,j1)/mu
 pv(217) = pv(217)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i0,j1)/mu
 pv(218) = pv(218)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i0,j1)/mu
 pv(219) = pv(219)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i0,j1)/mu
 pv(230) = pv(230)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(231) = pv(231)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(232) = pv(232)+d(i0,i2)*d(i1,i2)*d(i0,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(233) = pv(233)+d(i0,i1)*d(i0,i2)*d(i2,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(234) = pv(234)+d(i0,i2)*d(i1,i2)*d(i2,i3)*d(i1,j0)*d(i0,j1)/mu
 pv(241) = pv(241)+d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(242) = pv(242)+d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(243) = pv(243)+d(i1,i2)*d(i1,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(244) = pv(244)+d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(245) = pv(245)+d(i1,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(263) = pv(263)+d(i0,i1)*d(i0,i3)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(264) = pv(264)+d(i0,i1)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(265) = pv(265)+d(i0,i2)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(266) = pv(266)+d(i0,i3)*d(i1,i3)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(269) = pv(269)+d(i0,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(270) = pv(270)+d(i1,i3)*d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,j1)/mu
 pv(275) = pv(275)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i3,j0)*d(i0,j1)/mu
 pv(315) = pv(315)+d(i0,i1)*d(i0,i2)*d(i0,i3)*d(i0,j0)*d(j0,j1)/mu
 pv(316) = pv(316)+d(i0,i1)*d(i1,i2)*d(i0,i3)*d(i0,j0)*d(j0,j1)/mu
 pv(317) = pv(317)+d(i0,i1)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(j0,j1)/mu
 pv(318) = pv(318)+d(i0,i2)*d(i1,i2)*d(i1,i3)*d(i0,j0)*d(j0,j1)/mu
 pv(328) = pv(328)+d(i0,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(329) = pv(329)+d(i1,i2)*d(i0,i3)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(330) = pv(330)+d(i0,i2)*d(i2,i3)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
endif
END SUBROUTINE mg73_i3jj
SUBROUTINE mg73_i4jj (mxd, mu)
integer, intent (in) :: mxd, mu
END SUBROUTINE mg73_i4jj
SUBROUTINE mg73_iijj (mxd, mu)
integer, intent (in) :: mxd, mu
END SUBROUTINE mg73_iijj
END SUBROUTINE mg73_isecs
