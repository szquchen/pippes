SUBROUTINE mg3331_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=3, n=3, l=3, mm=m*(m-1), nn=n*(n-1), ll=l*(l-1)
integer :: i0, i1, i2, j0, j1, j2, k0, k1, k2, l0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg3331_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
l0 = m+n+l
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg3331_ii (mxd, mm)
    endif
   enddo
  endif
 enddo
enddo
do j0 = m, m+n-1
 do i0 = 0, m-1
  call mg3331_i0j0 (mxd, m*n)
  do i1 = 0, m-1
   if (i1.ne.i0) then
    do i2 = 0, m-1
     if (i2.ne.i0.and.i2.ne.i1) then
      call mg3331_iij0 (mxd, mm*n)
     endif
    enddo
   endif
  enddo
 enddo
 do j1 = m, m+n-1
  if (j1.ne.j0) then
   do j2 = m, m+n-1
    if (j2.ne.j0.and.j2.ne.j1) then
     call mg3331_jj (mxd, nn)
     do i0 = 0, m-1
      call mg3331_i0jj (mxd, m*nn)
      do i1 = 0, m-1
       if (i1.ne.i0) then
        do i2 = 0, m-1
         if (i2.ne.i0.and.i2.ne.i1) then
          call mg3331_iijj (mxd, mm*nn)
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
do k0 = m+n, m+n+l-1
 do i0 = 0, m-1
  call mg3331_i0k0 (mxd, m*l)
  do i1 = 0, m-1
   if (i1.ne.i0) then
    do i2 = 0, m-1
     if (i2.ne.i0.and.i2.ne.i1) then
      call mg3331_iik0 (mxd, mm*l)
     endif
    enddo
   endif
  enddo
 enddo
 do j0 = m, m+n-1
  call mg3331_j0k0 (mxd, n*l)
  do i0 = 0, m-1
   call mg3331_i0j0k0 (mxd, m*n*l)
   do i1 = 0, m-1
    if (i1.ne.i0) then
     do i2 = 0, m-1
      if (i2.ne.i0.and.i2.ne.i1) then
       call mg3331_iij0k0 (mxd, mm*n*l)
      endif
     enddo
    endif
   enddo
  enddo
  do j1 = m, m+n-1
   if (j1.ne.j0) then
    do j2 = m, m+n-1
     if (j2.ne.j0.and.j2.ne.j1) then
      call mg3331_jjk0 (mxd, nn*l)
      do i0 = 0, m-1
       call mg3331_i0jjk0 (mxd, m*nn*l)
       do i1 = 0, m-1
        if (i1.ne.i0) then
         do i2 = 0, m-1
          if (i2.ne.i0.and.i2.ne.i1) then
           call mg3331_iijjk0 (mxd, mm*nn*l)
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
 do k1 = m+n, m+n+l-1
  if (k1.ne.k0) then
   do k2 = m+n, m+n+l-1
    if (k2.ne.k0.and.k2.ne.k1) then
     call mg3331_kk (mxd, ll)
     do i0 = 0, m-1
      call mg3331_i0kk (mxd, m*ll)
      do i1 = 0, m-1
       if (i1.ne.i0) then
        do i2 = 0, m-1
         if (i2.ne.i0.and.i2.ne.i1) then
          call mg3331_iikk (mxd, mm*ll)
         endif
        enddo
       endif
      enddo
     enddo
     do j0 = m, m+n-1
      call mg3331_j0kk (mxd, n*ll)
      do i0 = 0, m-1
       call mg3331_i0j0kk (mxd, m*n*ll)
       do i1 = 0, m-1
        if (i1.ne.i0) then
         do i2 = 0, m-1
          if (i2.ne.i0.and.i2.ne.i1) then
           call mg3331_iij0kk (mxd, mm*n*ll)
          endif
         enddo
        endif
       enddo
      enddo
      do j1 = m, m+n-1
       if (j1.ne.j0) then
        do j2 = m, m+n-1
         if (j2.ne.j0.and.j2.ne.j1) then
          call mg3331_jjkk (mxd, nn*ll)
          do i0 = 0, m-1
           call mg3331_i0jjkk (mxd, m*nn*ll)
           do i1 = 0, m-1
            if (i1.ne.i0) then
             do i2 = 0, m-1
              if (i2.ne.i0.and.i2.ne.i1) then
               call mg3331_iijjkk (mxd, mm*nn*ll)
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
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg3331_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(9) = pv(9)+d(i0,i1)*d(i0,l0)/mu
endif
if (3.le.mxd) then
 pv(85) = pv(85)+d2(i0,i1)*d(i0,l0)/mu
 pv(99) = pv(99)+d(i0,i1)*d2(i0,l0)/mu
endif
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
END SUBROUTINE mg3331_ii
SUBROUTINE mg3331_i0j0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(10) = pv(10)+d(i0,j0)*d(i0,l0)/mu
 pv(12) = pv(12)+d(i0,j0)*d(j0,l0)/mu
endif
if (3.le.mxd) then
 pv(87) = pv(87)+d2(i0,j0)*d(i0,l0)/mu
 pv(100) = pv(100)+d(i0,j0)*d2(i0,l0)/mu
 pv(103) = pv(103)+d2(i0,j0)*d(j0,l0)/mu
 pv(116) = pv(116)+d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(117) = pv(117)+d(i0,j0)*d2(j0,l0)/mu
endif
if (4.le.mxd) then
 pv(389) = pv(389)+d3(i0,j0)*d(i0,l0)/mu
 pv(441) = pv(441)+d2(i0,j0)*d2(i0,l0)/mu
 pv(451) = pv(451)+d3(i0,j0)*d(j0,l0)/mu
 pv(503) = pv(503)+d2(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(510) = pv(510)+d(i0,j0)*d2(i0,l0)*d(j0,l0)/mu
 pv(512) = pv(512)+d2(i0,j0)*d2(j0,l0)/mu
 pv(519) = pv(519)+d(i0,j0)*d(i0,l0)*d2(j0,l0)/mu
endif
if (5.le.mxd) then
 pv(603) = pv(603)+d5(i0,j0)/mu
 pv(1412) = pv(1412)+d4(i0,j0)*d(i0,l0)/mu
 pv(1606) = pv(1606)+d3(i0,j0)*d2(i0,l0)/mu
 pv(1637) = pv(1637)+d4(i0,j0)*d(j0,l0)/mu
 pv(1831) = pv(1831)+d3(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(1866) = pv(1866)+d2(i0,j0)*d2(i0,l0)*d(j0,l0)/mu
 pv(1873) = pv(1873)+d3(i0,j0)*d2(j0,l0)/mu
 pv(1901) = pv(1901)+d2(i0,j0)*d(i0,l0)*d2(j0,l0)/mu
 pv(1905) = pv(1905)+d(i0,j0)*d2(i0,l0)*d2(j0,l0)/mu
endif
END SUBROUTINE mg3331_i0j0
SUBROUTINE mg3331_iij0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
endif
if (3.le.mxd) then
 pv(18) = pv(18)+d2(i0,i1)*d(i0,j0)/mu
 pv(19) = pv(19)+d(i0,i1)*d2(i0,j0)/mu
 pv(20) = pv(20)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(21) = pv(21)+d2(i0,j0)*d(i1,j0)/mu
 pv(86) = pv(86)+d(i0,i1)*d(i0,j0)*d(i0,l0)/mu
 pv(88) = pv(88)+d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(102) = pv(102)+d(i0,i1)*d(i0,j0)*d(j0,l0)/mu
 pv(104) = pv(104)+d(i0,j0)*d(i1,j0)*d(j0,l0)/mu
endif
if (4.le.mxd) then
 pv(139) = pv(139)+d2(i0,i1)*d2(i0,j0)/mu
 pv(140) = pv(140)+d(i0,i1)*d3(i0,j0)/mu
 pv(141) = pv(141)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(142) = pv(142)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(143) = pv(143)+d(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(144) = pv(144)+d3(i0,j0)*d(i1,j0)/mu
 pv(145) = pv(145)+d2(i0,j0)*d2(i1,j0)/mu
 pv(388) = pv(388)+d(i0,i1)*d2(i0,j0)*d(i0,l0)/mu
 pv(390) = pv(390)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(391) = pv(391)+d2(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(392) = pv(392)+d(i0,j0)*d2(i1,j0)*d(i0,l0)/mu
 pv(442) = pv(442)+d(i0,j0)*d(i1,j0)*d2(i0,l0)/mu
 pv(449) = pv(449)+d2(i0,i1)*d(i0,j0)*d(j0,l0)/mu
 pv(450) = pv(450)+d(i0,i1)*d2(i0,j0)*d(j0,l0)/mu
 pv(452) = pv(452)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(453) = pv(453)+d2(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(502) = pv(502)+d(i0,i1)*d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(504) = pv(504)+d(i0,j0)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(511) = pv(511)+d(i0,i1)*d(i0,j0)*d2(j0,l0)/mu
endif
if (5.le.mxd) then
 pv(601) = pv(601)+d2(i0,i1)*d3(i0,j0)/mu
 pv(602) = pv(602)+d(i0,i1)*d4(i0,j0)/mu
 pv(604) = pv(604)+d2(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(605) = pv(605)+d(i0,i1)*d(i0,i2)*d2(i0,j0)*d(i1,j0)/mu
 pv(606) = pv(606)+d(i0,i1)*d3(i0,j0)*d(i1,j0)/mu
 pv(607) = pv(607)+d(i0,i2)*d3(i0,j0)*d(i1,j0)/mu
 pv(608) = pv(608)+d4(i0,j0)*d(i1,j0)/mu
 pv(609) = pv(609)+d(i0,i1)*d2(i0,j0)*d2(i1,j0)/mu
 pv(610) = pv(610)+d3(i0,j0)*d2(i1,j0)/mu
 pv(1411) = pv(1411)+d(i0,i1)*d3(i0,j0)*d(i0,l0)/mu
 pv(1413) = pv(1413)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(1414) = pv(1414)+d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(1415) = pv(1415)+d3(i0,j0)*d(i1,j0)*d(i0,l0)/mu
 pv(1416) = pv(1416)+d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,l0)/mu
 pv(1417) = pv(1417)+d2(i0,j0)*d2(i1,j0)*d(i0,l0)/mu
 pv(1418) = pv(1418)+d(i0,j0)*d3(i1,j0)*d(i0,l0)/mu
 pv(1607) = pv(1607)+d2(i0,j0)*d(i1,j0)*d2(i0,l0)/mu
 pv(1608) = pv(1608)+d(i0,j0)*d2(i1,j0)*d2(i0,l0)/mu
 pv(1635) = pv(1635)+d2(i0,i1)*d2(i0,j0)*d(j0,l0)/mu
 pv(1636) = pv(1636)+d(i0,i1)*d3(i0,j0)*d(j0,l0)/mu
 pv(1638) = pv(1638)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(1639) = pv(1639)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(1640) = pv(1640)+d3(i0,j0)*d(i1,j0)*d(j0,l0)/mu
 pv(1830) = pv(1830)+d(i0,i1)*d2(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(1832) = pv(1832)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(1833) = pv(1833)+d2(i0,j0)*d(i1,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(1867) = pv(1867)+d(i0,j0)*d(i1,j0)*d2(i0,l0)*d(j0,l0)/mu
 pv(1871) = pv(1871)+d2(i0,i1)*d(i0,j0)*d2(j0,l0)/mu
 pv(1872) = pv(1872)+d(i0,i1)*d2(i0,j0)*d2(j0,l0)/mu
 pv(1900) = pv(1900)+d(i0,i1)*d(i0,j0)*d(i0,l0)*d2(j0,l0)/mu
endif
END SUBROUTINE mg3331_iij0
SUBROUTINE mg3331_jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(13) = pv(13)+d(j0,j1)*d(j0,l0)/mu
endif
if (3.le.mxd) then
 pv(107) = pv(107)+d2(j0,j1)*d(j0,l0)/mu
 pv(118) = pv(118)+d(j0,j1)*d2(j0,l0)/mu
endif
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
END SUBROUTINE mg3331_jj
SUBROUTINE mg3331_i0jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(1) = pv(1)+d(i0,j0)*d(j0,j1)/mu
endif
if (3.le.mxd) then
 pv(23) = pv(23)+d2(i0,j0)*d(i0,j1)/mu
 pv(26) = pv(26)+d2(i0,j0)*d(j0,j1)/mu
 pv(28) = pv(28)+d(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(29) = pv(29)+d(i0,j0)*d2(j0,j1)/mu
 pv(89) = pv(89)+d(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(90) = pv(90)+d(i0,j0)*d(j0,j1)*d(i0,l0)/mu
 pv(105) = pv(105)+d(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(106) = pv(106)+d(i0,j0)*d(j0,j1)*d(j0,l0)/mu
endif
if (4.le.mxd) then
 pv(147) = pv(147)+d3(i0,j0)*d(i0,j1)/mu
 pv(151) = pv(151)+d2(i0,j0)*d2(i0,j1)/mu
 pv(155) = pv(155)+d3(i0,j0)*d(j0,j1)/mu
 pv(159) = pv(159)+d2(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(161) = pv(161)+d2(i0,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(163) = pv(163)+d2(i0,j0)*d2(j0,j1)/mu
 pv(164) = pv(164)+d(i0,j0)*d(i0,j1)*d2(j0,j1)/mu
 pv(393) = pv(393)+d2(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(396) = pv(396)+d2(i0,j0)*d(j0,j1)*d(i0,l0)/mu
 pv(398) = pv(398)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,l0)/mu
 pv(399) = pv(399)+d(i0,j0)*d2(j0,j1)*d(i0,l0)/mu
 pv(443) = pv(443)+d(i0,j0)*d(j0,j1)*d2(i0,l0)/mu
 pv(455) = pv(455)+d2(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(457) = pv(457)+d(i0,j0)*d2(i0,j1)*d(j0,l0)/mu
 pv(459) = pv(459)+d2(i0,j0)*d(j0,j1)*d(j0,l0)/mu
 pv(460) = pv(460)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,l0)/mu
 pv(505) = pv(505)+d(i0,j0)*d(i0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(506) = pv(506)+d(i0,j0)*d(j0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(513) = pv(513)+d(i0,j0)*d(i0,j1)*d2(j0,l0)/mu
endif
if (5.le.mxd) then
 pv(612) = pv(612)+d4(i0,j0)*d(i0,j1)/mu
 pv(618) = pv(618)+d3(i0,j0)*d2(i0,j1)/mu
 pv(622) = pv(622)+d4(i0,j0)*d(j0,j1)/mu
 pv(627) = pv(627)+d3(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(630) = pv(630)+d2(i0,j0)*d2(i0,j1)*d(j0,j1)/mu
 pv(632) = pv(632)+d3(i0,j0)*d(i0,j2)*d(j0,j1)/mu
 pv(635) = pv(635)+d3(i0,j0)*d2(j0,j1)/mu
 pv(637) = pv(637)+d2(i0,j0)*d(i0,j1)*d2(j0,j1)/mu
 pv(638) = pv(638)+d2(i0,j0)*d(i0,j2)*d2(j0,j1)/mu
 pv(1419) = pv(1419)+d3(i0,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(1423) = pv(1423)+d3(i0,j0)*d(j0,j1)*d(i0,l0)/mu
 pv(1426) = pv(1426)+d2(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,l0)/mu
 pv(1429) = pv(1429)+d2(i0,j0)*d2(j0,j1)*d(i0,l0)/mu
 pv(1430) = pv(1430)+d(i0,j0)*d(i0,j1)*d2(j0,j1)*d(i0,l0)/mu
 pv(1609) = pv(1609)+d2(i0,j0)*d(j0,j1)*d2(i0,l0)/mu
 pv(1611) = pv(1611)+d(i0,j0)*d2(j0,j1)*d2(i0,l0)/mu
 pv(1642) = pv(1642)+d3(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(1645) = pv(1645)+d2(i0,j0)*d2(i0,j1)*d(j0,l0)/mu
 pv(1647) = pv(1647)+d(i0,j0)*d3(i0,j1)*d(j0,l0)/mu
 pv(1650) = pv(1650)+d3(i0,j0)*d(j0,j1)*d(j0,l0)/mu
 pv(1652) = pv(1652)+d2(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,l0)/mu
 pv(1653) = pv(1653)+d(i0,j0)*d2(i0,j1)*d(j0,j1)*d(j0,l0)/mu
 pv(1654) = pv(1654)+d2(i0,j0)*d(i0,j2)*d(j0,j1)*d(j0,l0)/mu
 pv(1834) = pv(1834)+d2(i0,j0)*d(i0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(1837) = pv(1837)+d2(i0,j0)*d(j0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(1838) = pv(1838)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(1868) = pv(1868)+d(i0,j0)*d(j0,j1)*d2(i0,l0)*d(j0,l0)/mu
 pv(1875) = pv(1875)+d2(i0,j0)*d(i0,j1)*d2(j0,l0)/mu
 pv(1876) = pv(1876)+d(i0,j0)*d2(i0,j1)*d2(j0,l0)/mu
 pv(1902) = pv(1902)+d(i0,j0)*d(i0,j1)*d(i0,l0)*d2(j0,l0)/mu
endif
END SUBROUTINE mg3331_i0jj
SUBROUTINE mg3331_iijj (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(22) = pv(22)+d(i0,i1)*d(i0,j0)*d(i0,j1)/mu
 pv(24) = pv(24)+d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(25) = pv(25)+d(i0,i1)*d(i0,j0)*d(j0,j1)/mu
 pv(27) = pv(27)+d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
endif
if (4.le.mxd) then
 pv(146) = pv(146)+d(i0,i1)*d2(i0,j0)*d(i0,j1)/mu
 pv(148) = pv(148)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(149) = pv(149)+d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(150) = pv(150)+d(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(152) = pv(152)+d(i0,j0)*d(i1,j0)*d2(i0,j1)/mu
 pv(153) = pv(153)+d2(i0,i1)*d(i0,j0)*d(j0,j1)/mu
 pv(154) = pv(154)+d(i0,i1)*d2(i0,j0)*d(j0,j1)/mu
 pv(156) = pv(156)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(157) = pv(157)+d2(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(158) = pv(158)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(160) = pv(160)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(162) = pv(162)+d(i0,i1)*d(i0,j0)*d2(j0,j1)/mu
 pv(394) = pv(394)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(395) = pv(395)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,l0)/mu
 pv(397) = pv(397)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,l0)/mu
 pv(454) = pv(454)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(456) = pv(456)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(458) = pv(458)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(j0,l0)/mu
endif
if (5.le.mxd) then
 pv(611) = pv(611)+d(i0,i1)*d3(i0,j0)*d(i0,j1)/mu
 pv(613) = pv(613)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(614) = pv(614)+d3(i0,j0)*d(i1,j0)*d(i0,j1)/mu
 pv(615) = pv(615)+d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(616) = pv(616)+d2(i0,j0)*d2(i1,j0)*d(i0,j1)/mu
 pv(617) = pv(617)+d(i0,j0)*d3(i1,j0)*d(i0,j1)/mu
 pv(619) = pv(619)+d2(i0,j0)*d(i1,j0)*d2(i0,j1)/mu
 pv(620) = pv(620)+d2(i0,i1)*d2(i0,j0)*d(j0,j1)/mu
 pv(621) = pv(621)+d(i0,i1)*d3(i0,j0)*d(j0,j1)/mu
 pv(623) = pv(623)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(624) = pv(624)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(625) = pv(625)+d3(i0,j0)*d(i1,j0)*d(j0,j1)/mu
 pv(626) = pv(626)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(628) = pv(628)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(629) = pv(629)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)/mu
 pv(631) = pv(631)+d(i0,j0)*d(i1,j0)*d2(i0,j1)*d(j0,j1)/mu
 pv(633) = pv(633)+d2(i0,i1)*d(i0,j0)*d2(j0,j1)/mu
 pv(634) = pv(634)+d(i0,i1)*d2(i0,j0)*d2(j0,j1)/mu
 pv(636) = pv(636)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d2(j0,j1)/mu
 pv(1420) = pv(1420)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(1421) = pv(1421)+d(i0,j0)*d2(i1,j0)*d(i0,j1)*d(i0,l0)/mu
 pv(1422) = pv(1422)+d(i0,i1)*d2(i0,j0)*d(j0,j1)*d(i0,l0)/mu
 pv(1424) = pv(1424)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,l0)/mu
 pv(1425) = pv(1425)+d2(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,l0)/mu
 pv(1427) = pv(1427)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)*d(i0,l0)/mu
 pv(1428) = pv(1428)+d(i0,i1)*d(i0,j0)*d2(j0,j1)*d(i0,l0)/mu
 pv(1610) = pv(1610)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d2(i0,l0)/mu
 pv(1641) = pv(1641)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(1643) = pv(1643)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(1644) = pv(1644)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,l0)/mu
 pv(1646) = pv(1646)+d(i0,j0)*d(i1,j0)*d2(i0,j1)*d(j0,l0)/mu
 pv(1648) = pv(1648)+d2(i0,i1)*d(i0,j0)*d(j0,j1)*d(j0,l0)/mu
 pv(1649) = pv(1649)+d(i0,i1)*d2(i0,j0)*d(j0,j1)*d(j0,l0)/mu
 pv(1651) = pv(1651)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,l0)/mu
 pv(1835) = pv(1835)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(1836) = pv(1836)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,l0)*d(j0,l0)/mu
 pv(1874) = pv(1874)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d2(j0,l0)/mu
endif
END SUBROUTINE mg3331_iijj
SUBROUTINE mg3331_i0k0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(11) = pv(11)+d(i0,k0)*d(i0,l0)/mu
 pv(15) = pv(15)+d(i0,k0)*d(k0,l0)/mu
endif
if (3.le.mxd) then
 pv(93) = pv(93)+d2(i0,k0)*d(i0,l0)/mu
 pv(101) = pv(101)+d(i0,k0)*d2(i0,l0)/mu
 pv(122) = pv(122)+d2(i0,k0)*d(k0,l0)/mu
 pv(134) = pv(134)+d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(136) = pv(136)+d(i0,k0)*d2(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(405) = pv(405)+d3(i0,k0)*d(i0,l0)/mu
 pv(444) = pv(444)+d2(i0,k0)*d2(i0,l0)/mu
 pv(528) = pv(528)+d3(i0,k0)*d(k0,l0)/mu
 pv(575) = pv(575)+d2(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(581) = pv(581)+d(i0,k0)*d2(i0,l0)*d(k0,l0)/mu
 pv(593) = pv(593)+d2(i0,k0)*d2(k0,l0)/mu
 pv(599) = pv(599)+d(i0,k0)*d(i0,l0)*d2(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(679) = pv(679)+d5(i0,k0)/mu
 pv(1442) = pv(1442)+d4(i0,k0)*d(i0,l0)/mu
 pv(1612) = pv(1612)+d3(i0,k0)*d2(i0,l0)/mu
 pv(1926) = pv(1926)+d4(i0,k0)*d(k0,l0)/mu
 pv(2106) = pv(2106)+d3(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2137) = pv(2137)+d2(i0,k0)*d2(i0,l0)*d(k0,l0)/mu
 pv(2194) = pv(2194)+d3(i0,k0)*d2(k0,l0)/mu
 pv(2217) = pv(2217)+d2(i0,k0)*d(i0,l0)*d2(k0,l0)/mu
 pv(2220) = pv(2220)+d(i0,k0)*d2(i0,l0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_i0k0
SUBROUTINE mg3331_iik0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(2) = pv(2)+d(i0,i1)*d(i0,k0)/mu
endif
if (3.le.mxd) then
 pv(30) = pv(30)+d2(i0,i1)*d(i0,k0)/mu
 pv(36) = pv(36)+d(i0,i1)*d2(i0,k0)/mu
 pv(38) = pv(38)+d(i0,i1)*d(i0,k0)*d(i1,k0)/mu
 pv(40) = pv(40)+d2(i0,k0)*d(i1,k0)/mu
 pv(91) = pv(91)+d(i0,i1)*d(i0,k0)*d(i0,l0)/mu
 pv(94) = pv(94)+d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(120) = pv(120)+d(i0,i1)*d(i0,k0)*d(k0,l0)/mu
 pv(123) = pv(123)+d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(177) = pv(177)+d2(i0,i1)*d2(i0,k0)/mu
 pv(183) = pv(183)+d(i0,i1)*d3(i0,k0)/mu
 pv(185) = pv(185)+d2(i0,i1)*d(i0,k0)*d(i1,k0)/mu
 pv(191) = pv(191)+d(i0,i1)*d2(i0,k0)*d(i1,k0)/mu
 pv(192) = pv(192)+d(i0,i2)*d2(i0,k0)*d(i1,k0)/mu
 pv(195) = pv(195)+d3(i0,k0)*d(i1,k0)/mu
 pv(196) = pv(196)+d2(i0,k0)*d2(i1,k0)/mu
 pv(403) = pv(403)+d(i0,i1)*d2(i0,k0)*d(i0,l0)/mu
 pv(406) = pv(406)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(408) = pv(408)+d2(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(409) = pv(409)+d(i0,k0)*d2(i1,k0)*d(i0,l0)/mu
 pv(445) = pv(445)+d(i0,k0)*d(i1,k0)*d2(i0,l0)/mu
 pv(520) = pv(520)+d2(i0,i1)*d(i0,k0)*d(k0,l0)/mu
 pv(526) = pv(526)+d(i0,i1)*d2(i0,k0)*d(k0,l0)/mu
 pv(529) = pv(529)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(531) = pv(531)+d2(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(573) = pv(573)+d(i0,i1)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(576) = pv(576)+d(i0,k0)*d(i1,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(591) = pv(591)+d(i0,i1)*d(i0,k0)*d2(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(671) = pv(671)+d2(i0,i1)*d3(i0,k0)/mu
 pv(677) = pv(677)+d(i0,i1)*d4(i0,k0)/mu
 pv(692) = pv(692)+d2(i0,i1)*d2(i0,k0)*d(i1,k0)/mu
 pv(693) = pv(693)+d(i0,i1)*d(i0,i2)*d2(i0,k0)*d(i1,k0)/mu
 pv(705) = pv(705)+d(i0,i1)*d3(i0,k0)*d(i1,k0)/mu
 pv(706) = pv(706)+d(i0,i2)*d3(i0,k0)*d(i1,k0)/mu
 pv(709) = pv(709)+d4(i0,k0)*d(i1,k0)/mu
 pv(710) = pv(710)+d(i0,i1)*d2(i0,k0)*d2(i1,k0)/mu
 pv(712) = pv(712)+d3(i0,k0)*d2(i1,k0)/mu
 pv(1440) = pv(1440)+d(i0,i1)*d3(i0,k0)*d(i0,l0)/mu
 pv(1447) = pv(1447)+d(i0,i1)*d2(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(1448) = pv(1448)+d(i0,i2)*d2(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(1451) = pv(1451)+d3(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(1452) = pv(1452)+d(i0,i1)*d(i0,k0)*d2(i1,k0)*d(i0,l0)/mu
 pv(1454) = pv(1454)+d2(i0,k0)*d2(i1,k0)*d(i0,l0)/mu
 pv(1455) = pv(1455)+d(i0,k0)*d3(i1,k0)*d(i0,l0)/mu
 pv(1613) = pv(1613)+d2(i0,k0)*d(i1,k0)*d2(i0,l0)/mu
 pv(1614) = pv(1614)+d(i0,k0)*d2(i1,k0)*d2(i0,l0)/mu
 pv(1918) = pv(1918)+d2(i0,i1)*d2(i0,k0)*d(k0,l0)/mu
 pv(1924) = pv(1924)+d(i0,i1)*d3(i0,k0)*d(k0,l0)/mu
 pv(1927) = pv(1927)+d2(i0,i1)*d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(1933) = pv(1933)+d(i0,i1)*d2(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(1935) = pv(1935)+d3(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(2104) = pv(2104)+d(i0,i1)*d2(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2107) = pv(2107)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2109) = pv(2109)+d2(i0,k0)*d(i1,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2138) = pv(2138)+d(i0,k0)*d(i1,k0)*d2(i0,l0)*d(k0,l0)/mu
 pv(2186) = pv(2186)+d2(i0,i1)*d(i0,k0)*d2(k0,l0)/mu
 pv(2192) = pv(2192)+d(i0,i1)*d2(i0,k0)*d2(k0,l0)/mu
 pv(2215) = pv(2215)+d(i0,i1)*d(i0,k0)*d(i0,l0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_iik0
SUBROUTINE mg3331_j0k0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(14) = pv(14)+d(j0,k0)*d(j0,l0)/mu
 pv(16) = pv(16)+d(j0,k0)*d(k0,l0)/mu
endif
if (3.le.mxd) then
 pv(112) = pv(112)+d2(j0,k0)*d(j0,l0)/mu
 pv(119) = pv(119)+d(j0,k0)*d2(j0,l0)/mu
 pv(128) = pv(128)+d2(j0,k0)*d(k0,l0)/mu
 pv(135) = pv(135)+d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(137) = pv(137)+d(j0,k0)*d2(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(484) = pv(484)+d3(j0,k0)*d(j0,l0)/mu
 pv(516) = pv(516)+d2(j0,k0)*d2(j0,l0)/mu
 pv(554) = pv(554)+d3(j0,k0)*d(k0,l0)/mu
 pv(586) = pv(586)+d2(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(590) = pv(590)+d(j0,k0)*d2(j0,l0)*d(k0,l0)/mu
 pv(597) = pv(597)+d2(j0,k0)*d2(k0,l0)/mu
 pv(600) = pv(600)+d(j0,k0)*d(j0,l0)*d2(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(1021) = pv(1021)+d5(j0,k0)/mu
 pv(1748) = pv(1748)+d4(j0,k0)*d(j0,l0)/mu
 pv(1890) = pv(1890)+d3(j0,k0)*d2(j0,l0)/mu
 pv(2021) = pv(2021)+d4(j0,k0)*d(k0,l0)/mu
 pv(2164) = pv(2164)+d3(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2183) = pv(2183)+d2(j0,k0)*d2(j0,l0)*d(k0,l0)/mu
 pv(2209) = pv(2209)+d3(j0,k0)*d2(k0,l0)/mu
 pv(2224) = pv(2224)+d2(j0,k0)*d(j0,l0)*d2(k0,l0)/mu
 pv(2226) = pv(2226)+d(j0,k0)*d2(j0,l0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_j0k0
SUBROUTINE mg3331_i0j0k0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(3) = pv(3)+d(i0,j0)*d(i0,k0)/mu
 pv(4) = pv(4)+d(i0,j0)*d(j0,k0)/mu
 pv(6) = pv(6)+d(i0,k0)*d(j0,k0)/mu
endif
if (3.le.mxd) then
 pv(32) = pv(32)+d2(i0,j0)*d(i0,k0)/mu
 pv(37) = pv(37)+d(i0,j0)*d2(i0,k0)/mu
 pv(46) = pv(46)+d2(i0,j0)*d(j0,k0)/mu
 pv(52) = pv(52)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(56) = pv(56)+d2(i0,k0)*d(j0,k0)/mu
 pv(60) = pv(60)+d(i0,j0)*d2(j0,k0)/mu
 pv(62) = pv(62)+d(i0,k0)*d2(j0,k0)/mu
 pv(92) = pv(92)+d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(96) = pv(96)+d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(97) = pv(97)+d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(108) = pv(108)+d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(109) = pv(109)+d(i0,j0)*d(j0,k0)*d(j0,l0)/mu
 pv(111) = pv(111)+d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(121) = pv(121)+d(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(125) = pv(125)+d(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(127) = pv(127)+d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(166) = pv(166)+d3(i0,j0)*d(i0,k0)/mu
 pv(179) = pv(179)+d2(i0,j0)*d2(i0,k0)/mu
 pv(184) = pv(184)+d(i0,j0)*d3(i0,k0)/mu
 pv(211) = pv(211)+d3(i0,j0)*d(j0,k0)/mu
 pv(223) = pv(223)+d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(243) = pv(243)+d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(247) = pv(247)+d3(i0,k0)*d(j0,k0)/mu
 pv(272) = pv(272)+d2(i0,j0)*d2(j0,k0)/mu
 pv(278) = pv(278)+d(i0,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(282) = pv(282)+d2(i0,k0)*d2(j0,k0)/mu
 pv(286) = pv(286)+d(i0,j0)*d3(j0,k0)/mu
 pv(288) = pv(288)+d(i0,k0)*d3(j0,k0)/mu
 pv(400) = pv(400)+d2(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(404) = pv(404)+d(i0,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(413) = pv(413)+d2(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(418) = pv(418)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(422) = pv(422)+d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(427) = pv(427)+d(i0,j0)*d2(j0,k0)*d(i0,l0)/mu
 pv(428) = pv(428)+d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(446) = pv(446)+d(i0,j0)*d(j0,k0)*d2(i0,l0)/mu
 pv(447) = pv(447)+d(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(462) = pv(462)+d2(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(466) = pv(466)+d(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(470) = pv(470)+d2(i0,j0)*d(j0,k0)*d(j0,l0)/mu
 pv(473) = pv(473)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(477) = pv(477)+d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(481) = pv(481)+d(i0,j0)*d2(j0,k0)*d(j0,l0)/mu
 pv(483) = pv(483)+d(i0,k0)*d2(j0,k0)*d(j0,l0)/mu
 pv(507) = pv(507)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(508) = pv(508)+d(i0,j0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(509) = pv(509)+d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(514) = pv(514)+d(i0,j0)*d(i0,k0)*d2(j0,l0)/mu
 pv(515) = pv(515)+d(i0,k0)*d(j0,k0)*d2(j0,l0)/mu
 pv(522) = pv(522)+d2(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(527) = pv(527)+d(i0,j0)*d2(i0,k0)*d(k0,l0)/mu
 pv(538) = pv(538)+d2(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(544) = pv(544)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(548) = pv(548)+d2(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(551) = pv(551)+d(i0,j0)*d2(j0,k0)*d(k0,l0)/mu
 pv(553) = pv(553)+d(i0,k0)*d2(j0,k0)*d(k0,l0)/mu
 pv(574) = pv(574)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(578) = pv(578)+d(i0,j0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(579) = pv(579)+d(i0,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(582) = pv(582)+d(i0,j0)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(583) = pv(583)+d(i0,j0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(585) = pv(585)+d(i0,k0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(592) = pv(592)+d(i0,j0)*d(i0,k0)*d2(k0,l0)/mu
 pv(595) = pv(595)+d(i0,j0)*d(j0,k0)*d2(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(640) = pv(640)+d4(i0,j0)*d(i0,k0)/mu
 pv(660) = pv(660)+d3(i0,j0)*d2(i0,k0)/mu
 pv(673) = pv(673)+d2(i0,j0)*d3(i0,k0)/mu
 pv(678) = pv(678)+d(i0,j0)*d4(i0,k0)/mu
 pv(739) = pv(739)+d4(i0,j0)*d(j0,k0)/mu
 pv(759) = pv(759)+d3(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(816) = pv(816)+d2(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(836) = pv(836)+d(i0,j0)*d3(i0,k0)*d(j0,k0)/mu
 pv(840) = pv(840)+d4(i0,k0)*d(j0,k0)/mu
 pv(943) = pv(943)+d3(i0,j0)*d2(j0,k0)/mu
 pv(955) = pv(955)+d2(i0,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(975) = pv(975)+d(i0,j0)*d2(i0,k0)*d2(j0,k0)/mu
 pv(979) = pv(979)+d3(i0,k0)*d2(j0,k0)/mu
 pv(1004) = pv(1004)+d2(i0,j0)*d3(j0,k0)/mu
 pv(1010) = pv(1010)+d(i0,j0)*d(i0,k0)*d3(j0,k0)/mu
 pv(1014) = pv(1014)+d2(i0,k0)*d3(j0,k0)/mu
 pv(1018) = pv(1018)+d(i0,j0)*d4(j0,k0)/mu
 pv(1020) = pv(1020)+d(i0,k0)*d4(j0,k0)/mu
 pv(1431) = pv(1431)+d3(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(1437) = pv(1437)+d2(i0,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(1441) = pv(1441)+d(i0,j0)*d3(i0,k0)*d(i0,l0)/mu
 pv(1460) = pv(1460)+d3(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(1469) = pv(1469)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1487) = pv(1487)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1491) = pv(1491)+d3(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1518) = pv(1518)+d2(i0,j0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1523) = pv(1523)+d(i0,j0)*d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1527) = pv(1527)+d2(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1532) = pv(1532)+d(i0,j0)*d3(j0,k0)*d(i0,l0)/mu
 pv(1533) = pv(1533)+d(i0,k0)*d3(j0,k0)*d(i0,l0)/mu
 pv(1615) = pv(1615)+d2(i0,j0)*d(j0,k0)*d2(i0,l0)/mu
 pv(1618) = pv(1618)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(1621) = pv(1621)+d2(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(1624) = pv(1624)+d(i0,j0)*d2(j0,k0)*d2(i0,l0)/mu
 pv(1625) = pv(1625)+d(i0,k0)*d2(j0,k0)*d2(i0,l0)/mu
 pv(1656) = pv(1656)+d3(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(1665) = pv(1665)+d2(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(1669) = pv(1669)+d(i0,j0)*d3(i0,k0)*d(j0,l0)/mu
 pv(1684) = pv(1684)+d3(i0,j0)*d(j0,k0)*d(j0,l0)/mu
 pv(1690) = pv(1690)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1707) = pv(1707)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1711) = pv(1711)+d3(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1734) = pv(1734)+d2(i0,j0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1737) = pv(1737)+d(i0,j0)*d(i0,k0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1741) = pv(1741)+d2(i0,k0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1745) = pv(1745)+d(i0,j0)*d3(j0,k0)*d(j0,l0)/mu
 pv(1747) = pv(1747)+d(i0,k0)*d3(j0,k0)*d(j0,l0)/mu
 pv(1839) = pv(1839)+d2(i0,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1842) = pv(1842)+d(i0,j0)*d2(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1845) = pv(1845)+d2(i0,j0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1848) = pv(1848)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1852) = pv(1852)+d2(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1857) = pv(1857)+d(i0,j0)*d2(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1858) = pv(1858)+d(i0,k0)*d2(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1869) = pv(1869)+d(i0,j0)*d(j0,k0)*d2(i0,l0)*d(j0,l0)/mu
 pv(1870) = pv(1870)+d(i0,k0)*d(j0,k0)*d2(i0,l0)*d(j0,l0)/mu
 pv(1878) = pv(1878)+d2(i0,j0)*d(i0,k0)*d2(j0,l0)/mu
 pv(1880) = pv(1880)+d(i0,j0)*d2(i0,k0)*d2(j0,l0)/mu
 pv(1884) = pv(1884)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d2(j0,l0)/mu
 pv(1886) = pv(1886)+d2(i0,k0)*d(j0,k0)*d2(j0,l0)/mu
 pv(1889) = pv(1889)+d(i0,k0)*d2(j0,k0)*d2(j0,l0)/mu
 pv(1903) = pv(1903)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d2(j0,l0)/mu
 pv(1904) = pv(1904)+d(i0,k0)*d(j0,k0)*d(i0,l0)*d2(j0,l0)/mu
 pv(1907) = pv(1907)+d3(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(1920) = pv(1920)+d2(i0,j0)*d2(i0,k0)*d(k0,l0)/mu
 pv(1925) = pv(1925)+d(i0,j0)*d3(i0,k0)*d(k0,l0)/mu
 pv(1950) = pv(1950)+d3(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(1962) = pv(1962)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1982) = pv(1982)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1986) = pv(1986)+d3(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(2005) = pv(2005)+d2(i0,j0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2011) = pv(2011)+d(i0,j0)*d(i0,k0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2015) = pv(2015)+d2(i0,k0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2018) = pv(2018)+d(i0,j0)*d3(j0,k0)*d(k0,l0)/mu
 pv(2020) = pv(2020)+d(i0,k0)*d3(j0,k0)*d(k0,l0)/mu
 pv(2101) = pv(2101)+d2(i0,j0)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2105) = pv(2105)+d(i0,j0)*d2(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2113) = pv(2113)+d2(i0,j0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2118) = pv(2118)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2122) = pv(2122)+d2(i0,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2126) = pv(2126)+d(i0,j0)*d2(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2127) = pv(2127)+d(i0,k0)*d2(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2139) = pv(2139)+d(i0,j0)*d(j0,k0)*d2(i0,l0)*d(k0,l0)/mu
 pv(2140) = pv(2140)+d(i0,k0)*d(j0,k0)*d2(i0,l0)*d(k0,l0)/mu
 pv(2143) = pv(2143)+d2(i0,j0)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2147) = pv(2147)+d(i0,j0)*d2(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2151) = pv(2151)+d2(i0,j0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2154) = pv(2154)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2158) = pv(2158)+d2(i0,k0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2161) = pv(2161)+d(i0,j0)*d2(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2163) = pv(2163)+d(i0,k0)*d2(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2178) = pv(2178)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)*d(k0,l0)/mu
 pv(2179) = pv(2179)+d(i0,j0)*d(j0,k0)*d(i0,l0)*d(j0,l0)*d(k0,l0)/mu
 pv(2180) = pv(2180)+d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)*d(k0,l0)/mu
 pv(2181) = pv(2181)+d(i0,j0)*d(i0,k0)*d2(j0,l0)*d(k0,l0)/mu
 pv(2182) = pv(2182)+d(i0,k0)*d(j0,k0)*d2(j0,l0)*d(k0,l0)/mu
 pv(2188) = pv(2188)+d2(i0,j0)*d(i0,k0)*d2(k0,l0)/mu
 pv(2193) = pv(2193)+d(i0,j0)*d2(i0,k0)*d2(k0,l0)/mu
 pv(2200) = pv(2200)+d2(i0,j0)*d(j0,k0)*d2(k0,l0)/mu
 pv(2205) = pv(2205)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d2(k0,l0)/mu
 pv(2207) = pv(2207)+d(i0,j0)*d2(j0,k0)*d2(k0,l0)/mu
 pv(2216) = pv(2216)+d(i0,j0)*d(i0,k0)*d(i0,l0)*d2(k0,l0)/mu
 pv(2219) = pv(2219)+d(i0,j0)*d(j0,k0)*d(i0,l0)*d2(k0,l0)/mu
 pv(2221) = pv(2221)+d(i0,j0)*d(i0,k0)*d(j0,l0)*d2(k0,l0)/mu
 pv(2222) = pv(2222)+d(i0,j0)*d(j0,k0)*d(j0,l0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_i0j0k0
SUBROUTINE mg3331_iij0k0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(31) = pv(31)+d(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(33) = pv(33)+d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(39) = pv(39)+d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(45) = pv(45)+d(i0,i1)*d(i0,j0)*d(j0,k0)/mu
 pv(47) = pv(47)+d(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(51) = pv(51)+d(i0,i1)*d(i0,k0)*d(j0,k0)/mu
 pv(53) = pv(53)+d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(57) = pv(57)+d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
endif
if (4.le.mxd) then
 pv(165) = pv(165)+d(i0,i1)*d2(i0,j0)*d(i0,k0)/mu
 pv(167) = pv(167)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(168) = pv(168)+d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(169) = pv(169)+d(i0,j0)*d2(i1,j0)*d(i0,k0)/mu
 pv(178) = pv(178)+d(i0,i1)*d(i0,j0)*d2(i0,k0)/mu
 pv(180) = pv(180)+d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(186) = pv(186)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(187) = pv(187)+d2(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(188) = pv(188)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(193) = pv(193)+d(i0,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(194) = pv(194)+d(i1,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(209) = pv(209)+d2(i0,i1)*d(i0,j0)*d(j0,k0)/mu
 pv(210) = pv(210)+d(i0,i1)*d2(i0,j0)*d(j0,k0)/mu
 pv(212) = pv(212)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(213) = pv(213)+d2(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(221) = pv(221)+d2(i0,i1)*d(i0,k0)*d(j0,k0)/mu
 pv(222) = pv(222)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(224) = pv(224)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(225) = pv(225)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(226) = pv(226)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(227) = pv(227)+d2(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(228) = pv(228)+d(i1,j0)*d(i2,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(242) = pv(242)+d(i0,i1)*d2(i0,k0)*d(j0,k0)/mu
 pv(244) = pv(244)+d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(248) = pv(248)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(249) = pv(249)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(250) = pv(250)+d(i2,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(253) = pv(253)+d2(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(271) = pv(271)+d(i0,i1)*d(i0,j0)*d2(j0,k0)/mu
 pv(273) = pv(273)+d(i0,j0)*d(i1,j0)*d2(j0,k0)/mu
 pv(277) = pv(277)+d(i0,i1)*d(i0,k0)*d2(j0,k0)/mu
 pv(279) = pv(279)+d(i1,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(283) = pv(283)+d(i0,k0)*d(i1,k0)*d2(j0,k0)/mu
 pv(401) = pv(401)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(407) = pv(407)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(412) = pv(412)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(414) = pv(414)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(417) = pv(417)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(419) = pv(419)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(423) = pv(423)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(424) = pv(424)+d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(461) = pv(461)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(463) = pv(463)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(467) = pv(467)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(469) = pv(469)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j0,l0)/mu
 pv(472) = pv(472)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(474) = pv(474)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(478) = pv(478)+d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(521) = pv(521)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(523) = pv(523)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(530) = pv(530)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(537) = pv(537)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(539) = pv(539)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(543) = pv(543)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(545) = pv(545)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(639) = pv(639)+d(i0,i1)*d3(i0,j0)*d(i0,k0)/mu
 pv(641) = pv(641)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(642) = pv(642)+d(i0,i2)*d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(643) = pv(643)+d3(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(644) = pv(644)+d(i0,i1)*d(i0,j0)*d2(i1,j0)*d(i0,k0)/mu
 pv(645) = pv(645)+d2(i0,j0)*d2(i1,j0)*d(i0,k0)/mu
 pv(646) = pv(646)+d(i0,j0)*d3(i1,j0)*d(i0,k0)/mu
 pv(659) = pv(659)+d(i0,i1)*d2(i0,j0)*d2(i0,k0)/mu
 pv(661) = pv(661)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(662) = pv(662)+d2(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(663) = pv(663)+d(i0,j0)*d2(i1,j0)*d2(i0,k0)/mu
 pv(672) = pv(672)+d(i0,i1)*d(i0,j0)*d3(i0,k0)/mu
 pv(674) = pv(674)+d(i0,j0)*d(i1,j0)*d3(i0,k0)/mu
 pv(680) = pv(680)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(681) = pv(681)+d3(i0,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(682) = pv(682)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(683) = pv(683)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(684) = pv(684)+d2(i0,j0)*d(i2,j0)*d(i0,k0)*d(i1,k0)/mu
 pv(694) = pv(694)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(695) = pv(695)+d(i0,i2)*d(i0,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(696) = pv(696)+d2(i0,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(697) = pv(697)+d(i0,i1)*d(i1,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(698) = pv(698)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(699) = pv(699)+d2(i1,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(700) = pv(700)+d(i0,j0)*d(i2,j0)*d2(i0,k0)*d(i1,k0)/mu
 pv(707) = pv(707)+d(i0,j0)*d3(i0,k0)*d(i1,k0)/mu
 pv(708) = pv(708)+d(i1,j0)*d3(i0,k0)*d(i1,k0)/mu
 pv(711) = pv(711)+d(i0,j0)*d2(i0,k0)*d2(i1,k0)/mu
 pv(737) = pv(737)+d2(i0,i1)*d2(i0,j0)*d(j0,k0)/mu
 pv(738) = pv(738)+d(i0,i1)*d3(i0,j0)*d(j0,k0)/mu
 pv(740) = pv(740)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(741) = pv(741)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(742) = pv(742)+d3(i0,j0)*d(i1,j0)*d(j0,k0)/mu
 pv(757) = pv(757)+d2(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(758) = pv(758)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(760) = pv(760)+d2(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(761) = pv(761)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(762) = pv(762)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(763) = pv(763)+d(i0,i2)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(764) = pv(764)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(765) = pv(765)+d(i0,i1)*d2(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(766) = pv(766)+d(i0,i2)*d2(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(767) = pv(767)+d(i0,j0)*d2(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(768) = pv(768)+d3(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(769) = pv(769)+d(i0,i1)*d(i1,j0)*d(i2,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(770) = pv(770)+d(i0,j0)*d(i1,j0)*d(i2,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(814) = pv(814)+d2(i0,i1)*d2(i0,k0)*d(j0,k0)/mu
 pv(815) = pv(815)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(817) = pv(817)+d(i0,i1)*d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(818) = pv(818)+d(i0,i2)*d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(819) = pv(819)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(820) = pv(820)+d2(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(821) = pv(821)+d(i1,j0)*d(i2,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(835) = pv(835)+d(i0,i1)*d3(i0,k0)*d(j0,k0)/mu
 pv(837) = pv(837)+d(i1,j0)*d3(i0,k0)*d(j0,k0)/mu
 pv(841) = pv(841)+d2(i0,i1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(842) = pv(842)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(843) = pv(843)+d(i0,i2)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(844) = pv(844)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(845) = pv(845)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(846) = pv(846)+d(i0,i1)*d(i2,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(847) = pv(847)+d(i0,j0)*d(i2,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(848) = pv(848)+d2(i2,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(862) = pv(862)+d(i0,i1)*d2(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(863) = pv(863)+d(i0,j0)*d2(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(864) = pv(864)+d(i1,j0)*d2(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(865) = pv(865)+d(i2,j0)*d2(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(868) = pv(868)+d3(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(941) = pv(941)+d2(i0,i1)*d(i0,j0)*d2(j0,k0)/mu
 pv(942) = pv(942)+d(i0,i1)*d2(i0,j0)*d2(j0,k0)/mu
 pv(944) = pv(944)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(j0,k0)/mu
 pv(945) = pv(945)+d2(i0,j0)*d(i1,j0)*d2(j0,k0)/mu
 pv(953) = pv(953)+d2(i0,i1)*d(i0,k0)*d2(j0,k0)/mu
 pv(954) = pv(954)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(956) = pv(956)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(957) = pv(957)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(958) = pv(958)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(959) = pv(959)+d2(i1,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(960) = pv(960)+d(i1,j0)*d(i2,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(974) = pv(974)+d(i0,i1)*d2(i0,k0)*d2(j0,k0)/mu
 pv(976) = pv(976)+d(i1,j0)*d2(i0,k0)*d2(j0,k0)/mu
 pv(980) = pv(980)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d2(j0,k0)/mu
 pv(981) = pv(981)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d2(j0,k0)/mu
 pv(982) = pv(982)+d(i2,j0)*d(i0,k0)*d(i1,k0)*d2(j0,k0)/mu
 pv(985) = pv(985)+d2(i0,k0)*d(i1,k0)*d2(j0,k0)/mu
 pv(1003) = pv(1003)+d(i0,i1)*d(i0,j0)*d3(j0,k0)/mu
 pv(1005) = pv(1005)+d(i0,j0)*d(i1,j0)*d3(j0,k0)/mu
 pv(1009) = pv(1009)+d(i0,i1)*d(i0,k0)*d3(j0,k0)/mu
 pv(1011) = pv(1011)+d(i1,j0)*d(i0,k0)*d3(j0,k0)/mu
 pv(1015) = pv(1015)+d(i0,k0)*d(i1,k0)*d3(j0,k0)/mu
 pv(1432) = pv(1432)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(1433) = pv(1433)+d(i0,j0)*d2(i1,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(1438) = pv(1438)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(i0,l0)/mu
 pv(1443) = pv(1443)+d2(i0,j0)*d(i1,j0)*d(i1,k0)*d(i0,l0)/mu
 pv(1444) = pv(1444)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(1445) = pv(1445)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(1449) = pv(1449)+d(i0,j0)*d2(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(1450) = pv(1450)+d(i1,j0)*d2(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(1453) = pv(1453)+d(i0,j0)*d(i0,k0)*d2(i1,k0)*d(i0,l0)/mu
 pv(1459) = pv(1459)+d(i0,i1)*d2(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(1461) = pv(1461)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(1462) = pv(1462)+d2(i0,j0)*d(i1,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(1468) = pv(1468)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1470) = pv(1470)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1471) = pv(1471)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1472) = pv(1472)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1473) = pv(1473)+d2(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1474) = pv(1474)+d(i1,j0)*d(i2,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1486) = pv(1486)+d(i0,i1)*d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1488) = pv(1488)+d(i1,j0)*d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1492) = pv(1492)+d(i0,i1)*d(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1493) = pv(1493)+d2(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1494) = pv(1494)+d(i0,j0)*d(i1,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1497) = pv(1497)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1498) = pv(1498)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1499) = pv(1499)+d(i1,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1500) = pv(1500)+d(i2,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1503) = pv(1503)+d2(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1504) = pv(1504)+d(i0,j0)*d2(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1517) = pv(1517)+d(i0,i1)*d(i0,j0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1519) = pv(1519)+d(i0,j0)*d(i1,j0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1522) = pv(1522)+d(i0,i1)*d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1524) = pv(1524)+d(i1,j0)*d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1528) = pv(1528)+d(i0,j0)*d(i1,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1529) = pv(1529)+d(i0,k0)*d(i1,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1616) = pv(1616)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d2(i0,l0)/mu
 pv(1619) = pv(1619)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(1622) = pv(1622)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(1623) = pv(1623)+d(i0,k0)*d(i1,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(1655) = pv(1655)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(1657) = pv(1657)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(1658) = pv(1658)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(1664) = pv(1664)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(1666) = pv(1666)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(j0,l0)/mu
 pv(1670) = pv(1670)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(1671) = pv(1671)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(1672) = pv(1672)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(1675) = pv(1675)+d(i0,j0)*d2(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(1676) = pv(1676)+d(i1,j0)*d2(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(1682) = pv(1682)+d2(i0,i1)*d(i0,j0)*d(j0,k0)*d(j0,l0)/mu
 pv(1683) = pv(1683)+d(i0,i1)*d2(i0,j0)*d(j0,k0)*d(j0,l0)/mu
 pv(1688) = pv(1688)+d2(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1689) = pv(1689)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1691) = pv(1691)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1692) = pv(1692)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1693) = pv(1693)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1694) = pv(1694)+d2(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1706) = pv(1706)+d(i0,i1)*d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1708) = pv(1708)+d(i1,j0)*d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1712) = pv(1712)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1713) = pv(1713)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1714) = pv(1714)+d(i2,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1717) = pv(1717)+d2(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1733) = pv(1733)+d(i0,i1)*d(i0,j0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1736) = pv(1736)+d(i0,i1)*d(i0,k0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1738) = pv(1738)+d(i1,j0)*d(i0,k0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1742) = pv(1742)+d(i0,k0)*d(i1,k0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1840) = pv(1840)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1843) = pv(1843)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1844) = pv(1844)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1847) = pv(1847)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1849) = pv(1849)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1853) = pv(1853)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1854) = pv(1854)+d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1877) = pv(1877)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d2(j0,l0)/mu
 pv(1881) = pv(1881)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d2(j0,l0)/mu
 pv(1883) = pv(1883)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d2(j0,l0)/mu
 pv(1887) = pv(1887)+d(i0,k0)*d(i1,k0)*d(j0,k0)*d2(j0,l0)/mu
 pv(1906) = pv(1906)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(1908) = pv(1908)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(1909) = pv(1909)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(1910) = pv(1910)+d(i0,j0)*d2(i1,j0)*d(i0,k0)*d(k0,l0)/mu
 pv(1919) = pv(1919)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(k0,l0)/mu
 pv(1921) = pv(1921)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(k0,l0)/mu
 pv(1928) = pv(1928)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(1929) = pv(1929)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(1930) = pv(1930)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(1934) = pv(1934)+d(i0,j0)*d2(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(1948) = pv(1948)+d2(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(1949) = pv(1949)+d(i0,i1)*d2(i0,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(1951) = pv(1951)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(1952) = pv(1952)+d2(i0,j0)*d(i1,j0)*d(j0,k0)*d(k0,l0)/mu
 pv(1960) = pv(1960)+d2(i0,i1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1961) = pv(1961)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1963) = pv(1963)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1964) = pv(1964)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1965) = pv(1965)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1966) = pv(1966)+d2(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1967) = pv(1967)+d(i1,j0)*d(i2,j0)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1981) = pv(1981)+d(i0,i1)*d2(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1983) = pv(1983)+d(i1,j0)*d2(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1987) = pv(1987)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(2004) = pv(2004)+d(i0,i1)*d(i0,j0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2006) = pv(2006)+d(i0,j0)*d(i1,j0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2010) = pv(2010)+d(i0,i1)*d(i0,k0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2012) = pv(2012)+d(i1,j0)*d(i0,k0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2102) = pv(2102)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2108) = pv(2108)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2112) = pv(2112)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2114) = pv(2114)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2117) = pv(2117)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2119) = pv(2119)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2123) = pv(2123)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2142) = pv(2142)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2144) = pv(2144)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2148) = pv(2148)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2150) = pv(2150)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2153) = pv(2153)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2155) = pv(2155)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2187) = pv(2187)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d2(k0,l0)/mu
 pv(2189) = pv(2189)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d2(k0,l0)/mu
 pv(2199) = pv(2199)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d2(k0,l0)/mu
 pv(2201) = pv(2201)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_iij0k0
SUBROUTINE mg3331_jjk0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(5) = pv(5)+d(j0,j1)*d(j0,k0)/mu
endif
if (3.le.mxd) then
 pv(50) = pv(50)+d2(j0,j1)*d(j0,k0)/mu
 pv(61) = pv(61)+d(j0,j1)*d2(j0,k0)/mu
 pv(64) = pv(64)+d(j0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(66) = pv(66)+d2(j0,k0)*d(j1,k0)/mu
 pv(110) = pv(110)+d(j0,j1)*d(j0,k0)*d(j0,l0)/mu
 pv(113) = pv(113)+d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(126) = pv(126)+d(j0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(129) = pv(129)+d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(276) = pv(276)+d2(j0,j1)*d2(j0,k0)/mu
 pv(287) = pv(287)+d(j0,j1)*d3(j0,k0)/mu
 pv(294) = pv(294)+d2(j0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(305) = pv(305)+d(j0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(306) = pv(306)+d(j0,j2)*d2(j0,k0)*d(j1,k0)/mu
 pv(308) = pv(308)+d3(j0,k0)*d(j1,k0)/mu
 pv(309) = pv(309)+d2(j0,k0)*d2(j1,k0)/mu
 pv(482) = pv(482)+d(j0,j1)*d2(j0,k0)*d(j0,l0)/mu
 pv(487) = pv(487)+d(j0,j1)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(489) = pv(489)+d2(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(490) = pv(490)+d(j0,k0)*d2(j1,k0)*d(j0,l0)/mu
 pv(517) = pv(517)+d(j0,k0)*d(j1,k0)*d2(j0,l0)/mu
 pv(542) = pv(542)+d2(j0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(552) = pv(552)+d(j0,j1)*d2(j0,k0)*d(k0,l0)/mu
 pv(556) = pv(556)+d(j0,j1)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(557) = pv(557)+d2(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(584) = pv(584)+d(j0,j1)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(587) = pv(587)+d(j0,k0)*d(j1,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(596) = pv(596)+d(j0,j1)*d(j0,k0)*d2(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(1008) = pv(1008)+d2(j0,j1)*d3(j0,k0)/mu
 pv(1019) = pv(1019)+d(j0,j1)*d4(j0,k0)/mu
 pv(1088) = pv(1088)+d2(j0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1090) = pv(1090)+d(j0,j1)*d(j0,j2)*d2(j0,k0)*d(j1,k0)/mu
 pv(1103) = pv(1103)+d(j0,j1)*d3(j0,k0)*d(j1,k0)/mu
 pv(1104) = pv(1104)+d(j0,j2)*d3(j0,k0)*d(j1,k0)/mu
 pv(1106) = pv(1106)+d4(j0,k0)*d(j1,k0)/mu
 pv(1108) = pv(1108)+d(j0,j1)*d2(j0,k0)*d2(j1,k0)/mu
 pv(1109) = pv(1109)+d3(j0,k0)*d2(j1,k0)/mu
 pv(1746) = pv(1746)+d(j0,j1)*d3(j0,k0)*d(j0,l0)/mu
 pv(1772) = pv(1772)+d(j0,j1)*d2(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1773) = pv(1773)+d(j0,j2)*d2(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1775) = pv(1775)+d3(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1778) = pv(1778)+d(j0,j1)*d(j0,k0)*d2(j1,k0)*d(j0,l0)/mu
 pv(1779) = pv(1779)+d2(j0,k0)*d2(j1,k0)*d(j0,l0)/mu
 pv(1780) = pv(1780)+d(j0,k0)*d3(j1,k0)*d(j0,l0)/mu
 pv(1893) = pv(1893)+d2(j0,k0)*d(j1,k0)*d2(j0,l0)/mu
 pv(1894) = pv(1894)+d(j0,k0)*d2(j1,k0)*d2(j0,l0)/mu
 pv(2009) = pv(2009)+d2(j0,j1)*d2(j0,k0)*d(k0,l0)/mu
 pv(2019) = pv(2019)+d(j0,j1)*d3(j0,k0)*d(k0,l0)/mu
 pv(2027) = pv(2027)+d2(j0,j1)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2031) = pv(2031)+d(j0,j1)*d2(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2032) = pv(2032)+d3(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2162) = pv(2162)+d(j0,j1)*d2(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2167) = pv(2167)+d(j0,j1)*d(j0,k0)*d(j1,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2168) = pv(2168)+d2(j0,k0)*d(j1,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2184) = pv(2184)+d(j0,k0)*d(j1,k0)*d2(j0,l0)*d(k0,l0)/mu
 pv(2204) = pv(2204)+d2(j0,j1)*d(j0,k0)*d2(k0,l0)/mu
 pv(2208) = pv(2208)+d(j0,j1)*d2(j0,k0)*d2(k0,l0)/mu
 pv(2223) = pv(2223)+d(j0,j1)*d(j0,k0)*d(j0,l0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_jjk0
SUBROUTINE mg3331_i0jjk0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(34) = pv(34)+d(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(35) = pv(35)+d(i0,j0)*d(j0,j1)*d(i0,k0)/mu
 pv(48) = pv(48)+d(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(49) = pv(49)+d(i0,j0)*d(j0,j1)*d(j0,k0)/mu
 pv(54) = pv(54)+d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(55) = pv(55)+d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(63) = pv(63)+d(i0,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(65) = pv(65)+d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
endif
if (4.le.mxd) then
 pv(170) = pv(170)+d2(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(173) = pv(173)+d2(i0,j0)*d(j0,j1)*d(i0,k0)/mu
 pv(175) = pv(175)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)/mu
 pv(176) = pv(176)+d(i0,j0)*d2(j0,j1)*d(i0,k0)/mu
 pv(181) = pv(181)+d(i0,j0)*d(i0,j1)*d2(i0,k0)/mu
 pv(182) = pv(182)+d(i0,j0)*d(j0,j1)*d2(i0,k0)/mu
 pv(215) = pv(215)+d2(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(217) = pv(217)+d(i0,j0)*d2(i0,j1)*d(j0,k0)/mu
 pv(219) = pv(219)+d2(i0,j0)*d(j0,j1)*d(j0,k0)/mu
 pv(220) = pv(220)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,k0)/mu
 pv(230) = pv(230)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(232) = pv(232)+d2(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(235) = pv(235)+d(i0,j1)*d(i0,j2)*d(i0,k0)*d(j0,k0)/mu
 pv(237) = pv(237)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(239) = pv(239)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(240) = pv(240)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(241) = pv(241)+d2(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(245) = pv(245)+d(i0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(246) = pv(246)+d(j0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(274) = pv(274)+d(i0,j0)*d(i0,j1)*d2(j0,k0)/mu
 pv(275) = pv(275)+d(i0,j0)*d(j0,j1)*d2(j0,k0)/mu
 pv(280) = pv(280)+d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(281) = pv(281)+d(j0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(290) = pv(290)+d2(i0,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(292) = pv(292)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(293) = pv(293)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(296) = pv(296)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(298) = pv(298)+d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(299) = pv(299)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(300) = pv(300)+d2(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(303) = pv(303)+d(i0,j0)*d2(j0,k0)*d(j1,k0)/mu
 pv(304) = pv(304)+d(i0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(307) = pv(307)+d(i0,k0)*d2(j0,k0)*d(j1,k0)/mu
 pv(402) = pv(402)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(415) = pv(415)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(416) = pv(416)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(420) = pv(420)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(421) = pv(421)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(429) = pv(429)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(430) = pv(430)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(464) = pv(464)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(465) = pv(465)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(471) = pv(471)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,l0)/mu
 pv(475) = pv(475)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(476) = pv(476)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(485) = pv(485)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(486) = pv(486)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(488) = pv(488)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(524) = pv(524)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(525) = pv(525)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(540) = pv(540)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(541) = pv(541)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(546) = pv(546)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(547) = pv(547)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(555) = pv(555)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(647) = pv(647)+d3(i0,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(651) = pv(651)+d3(i0,j0)*d(j0,j1)*d(i0,k0)/mu
 pv(654) = pv(654)+d2(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)/mu
 pv(657) = pv(657)+d2(i0,j0)*d2(j0,j1)*d(i0,k0)/mu
 pv(658) = pv(658)+d(i0,j0)*d(i0,j1)*d2(j0,j1)*d(i0,k0)/mu
 pv(664) = pv(664)+d2(i0,j0)*d(i0,j1)*d2(i0,k0)/mu
 pv(667) = pv(667)+d2(i0,j0)*d(j0,j1)*d2(i0,k0)/mu
 pv(669) = pv(669)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d2(i0,k0)/mu
 pv(670) = pv(670)+d(i0,j0)*d2(j0,j1)*d2(i0,k0)/mu
 pv(675) = pv(675)+d(i0,j0)*d(i0,j1)*d3(i0,k0)/mu
 pv(676) = pv(676)+d(i0,j0)*d(j0,j1)*d3(i0,k0)/mu
 pv(744) = pv(744)+d3(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(747) = pv(747)+d2(i0,j0)*d2(i0,j1)*d(j0,k0)/mu
 pv(749) = pv(749)+d(i0,j0)*d3(i0,j1)*d(j0,k0)/mu
 pv(752) = pv(752)+d3(i0,j0)*d(j0,j1)*d(j0,k0)/mu
 pv(754) = pv(754)+d2(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,k0)/mu
 pv(755) = pv(755)+d(i0,j0)*d2(i0,j1)*d(j0,j1)*d(j0,k0)/mu
 pv(756) = pv(756)+d2(i0,j0)*d(i0,j2)*d(j0,j1)*d(j0,k0)/mu
 pv(772) = pv(772)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(779) = pv(779)+d(i0,j0)*d2(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(781) = pv(781)+d3(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(790) = pv(790)+d(i0,j0)*d(i0,j1)*d(i0,j2)*d(i0,k0)*d(j0,k0)/mu
 pv(794) = pv(794)+d2(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(800) = pv(800)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(802) = pv(802)+d2(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(806) = pv(806)+d(i0,j0)*d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(808) = pv(808)+d(i0,j1)*d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(809) = pv(809)+d2(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(811) = pv(811)+d(i0,j0)*d2(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(812) = pv(812)+d(i0,j1)*d2(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(813) = pv(813)+d(i0,j2)*d2(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(823) = pv(823)+d(i0,j0)*d(i0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(825) = pv(825)+d2(i0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(828) = pv(828)+d(i0,j1)*d(i0,j2)*d2(i0,k0)*d(j0,k0)/mu
 pv(830) = pv(830)+d(i0,j0)*d(j0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(832) = pv(832)+d(i0,j1)*d(j0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(833) = pv(833)+d(i0,j2)*d(j0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(834) = pv(834)+d2(j0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(838) = pv(838)+d(i0,j1)*d3(i0,k0)*d(j0,k0)/mu
 pv(839) = pv(839)+d(j0,j1)*d3(i0,k0)*d(j0,k0)/mu
 pv(947) = pv(947)+d2(i0,j0)*d(i0,j1)*d2(j0,k0)/mu
 pv(949) = pv(949)+d(i0,j0)*d2(i0,j1)*d2(j0,k0)/mu
 pv(951) = pv(951)+d2(i0,j0)*d(j0,j1)*d2(j0,k0)/mu
 pv(952) = pv(952)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d2(j0,k0)/mu
 pv(962) = pv(962)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(964) = pv(964)+d2(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(967) = pv(967)+d(i0,j1)*d(i0,j2)*d(i0,k0)*d2(j0,k0)/mu
 pv(969) = pv(969)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(971) = pv(971)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(972) = pv(972)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(973) = pv(973)+d2(j0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(977) = pv(977)+d(i0,j1)*d2(i0,k0)*d2(j0,k0)/mu
 pv(978) = pv(978)+d(j0,j1)*d2(i0,k0)*d2(j0,k0)/mu
 pv(1006) = pv(1006)+d(i0,j0)*d(i0,j1)*d3(j0,k0)/mu
 pv(1007) = pv(1007)+d(i0,j0)*d(j0,j1)*d3(j0,k0)/mu
 pv(1012) = pv(1012)+d(i0,j1)*d(i0,k0)*d3(j0,k0)/mu
 pv(1013) = pv(1013)+d(j0,j1)*d(i0,k0)*d3(j0,k0)/mu
 pv(1024) = pv(1024)+d3(i0,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(1028) = pv(1028)+d2(i0,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(1030) = pv(1030)+d2(i0,j0)*d(i0,j2)*d(j0,k0)*d(j1,k0)/mu
 pv(1032) = pv(1032)+d2(i0,j0)*d(j0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(1033) = pv(1033)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(1036) = pv(1036)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1042) = pv(1042)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1046) = pv(1046)+d(i0,j0)*d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1048) = pv(1048)+d2(i0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1050) = pv(1050)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1052) = pv(1052)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1053) = pv(1053)+d2(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1054) = pv(1054)+d(i0,j0)*d(j0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1056) = pv(1056)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1058) = pv(1058)+d(i0,j2)*d2(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1059) = pv(1059)+d(j0,j1)*d2(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1060) = pv(1060)+d3(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1079) = pv(1079)+d2(i0,j0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1082) = pv(1082)+d(i0,j0)*d(i0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1084) = pv(1084)+d2(i0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1085) = pv(1085)+d(i0,j0)*d(i0,j2)*d2(j0,k0)*d(j1,k0)/mu
 pv(1086) = pv(1086)+d(i0,j0)*d(j0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1087) = pv(1087)+d(i0,j1)*d(j0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1089) = pv(1089)+d(i0,j0)*d(j0,j2)*d2(j0,k0)*d(j1,k0)/mu
 pv(1092) = pv(1092)+d(i0,j0)*d(i0,k0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1094) = pv(1094)+d(i0,j1)*d(i0,k0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1095) = pv(1095)+d(i0,j2)*d(i0,k0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1096) = pv(1096)+d(j0,j1)*d(i0,k0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1097) = pv(1097)+d2(i0,k0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1101) = pv(1101)+d(i0,j0)*d3(j0,k0)*d(j1,k0)/mu
 pv(1102) = pv(1102)+d(i0,j1)*d3(j0,k0)*d(j1,k0)/mu
 pv(1105) = pv(1105)+d(i0,k0)*d3(j0,k0)*d(j1,k0)/mu
 pv(1107) = pv(1107)+d(i0,j0)*d2(j0,k0)*d2(j1,k0)/mu
 pv(1434) = pv(1434)+d2(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(1436) = pv(1436)+d(i0,j0)*d2(j0,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(1439) = pv(1439)+d(i0,j0)*d(j0,j1)*d2(i0,k0)*d(i0,l0)/mu
 pv(1463) = pv(1463)+d2(i0,j0)*d(i0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(1466) = pv(1466)+d2(i0,j0)*d(j0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(1467) = pv(1467)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(1475) = pv(1475)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1477) = pv(1477)+d2(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1481) = pv(1481)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1483) = pv(1483)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1484) = pv(1484)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1485) = pv(1485)+d2(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1489) = pv(1489)+d(i0,j1)*d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1490) = pv(1490)+d(j0,j1)*d2(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1520) = pv(1520)+d(i0,j0)*d(i0,j1)*d2(j0,k0)*d(i0,l0)/mu
 pv(1521) = pv(1521)+d(i0,j0)*d(j0,j1)*d2(j0,k0)*d(i0,l0)/mu
 pv(1525) = pv(1525)+d(i0,j1)*d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1526) = pv(1526)+d(j0,j1)*d(i0,k0)*d2(j0,k0)*d(i0,l0)/mu
 pv(1535) = pv(1535)+d2(i0,j0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1537) = pv(1537)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1538) = pv(1538)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1540) = pv(1540)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1542) = pv(1542)+d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1543) = pv(1543)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1544) = pv(1544)+d2(i0,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1548) = pv(1548)+d(i0,j0)*d2(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1549) = pv(1549)+d(i0,j1)*d2(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1550) = pv(1550)+d(i0,k0)*d2(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1617) = pv(1617)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d2(i0,l0)/mu
 pv(1620) = pv(1620)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d2(i0,l0)/mu
 pv(1626) = pv(1626)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d2(i0,l0)/mu
 pv(1627) = pv(1627)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d2(i0,l0)/mu
 pv(1659) = pv(1659)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(1662) = pv(1662)+d2(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(1663) = pv(1663)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(1667) = pv(1667)+d(i0,j0)*d(i0,j1)*d2(i0,k0)*d(j0,l0)/mu
 pv(1668) = pv(1668)+d(i0,j0)*d(j0,j1)*d2(i0,k0)*d(j0,l0)/mu
 pv(1686) = pv(1686)+d2(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,l0)/mu
 pv(1687) = pv(1687)+d(i0,j0)*d2(i0,j1)*d(j0,k0)*d(j0,l0)/mu
 pv(1696) = pv(1696)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1698) = pv(1698)+d2(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1701) = pv(1701)+d(i0,j1)*d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1703) = pv(1703)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1704) = pv(1704)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1705) = pv(1705)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1709) = pv(1709)+d(i0,j1)*d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1710) = pv(1710)+d(j0,j1)*d2(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1735) = pv(1735)+d(i0,j0)*d(i0,j1)*d2(j0,k0)*d(j0,l0)/mu
 pv(1739) = pv(1739)+d(i0,j1)*d(i0,k0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1740) = pv(1740)+d(j0,j1)*d(i0,k0)*d2(j0,k0)*d(j0,l0)/mu
 pv(1749) = pv(1749)+d2(i0,j0)*d(i0,j1)*d(j1,k0)*d(j0,l0)/mu
 pv(1751) = pv(1751)+d2(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1753) = pv(1753)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1754) = pv(1754)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1755) = pv(1755)+d(i0,j0)*d2(i0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1759) = pv(1759)+d2(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1760) = pv(1760)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1762) = pv(1762)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1764) = pv(1764)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1765) = pv(1765)+d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1766) = pv(1766)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1767) = pv(1767)+d2(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1770) = pv(1770)+d(i0,j0)*d2(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1771) = pv(1771)+d(i0,j1)*d2(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1774) = pv(1774)+d(i0,k0)*d2(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1776) = pv(1776)+d(i0,j0)*d(i0,k0)*d2(j1,k0)*d(j0,l0)/mu
 pv(1777) = pv(1777)+d(i0,j0)*d(j0,k0)*d2(j1,k0)*d(j0,l0)/mu
 pv(1841) = pv(1841)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1846) = pv(1846)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1850) = pv(1850)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1851) = pv(1851)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1859) = pv(1859)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1860) = pv(1860)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1861) = pv(1861)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1879) = pv(1879)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d2(j0,l0)/mu
 pv(1885) = pv(1885)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d2(j0,l0)/mu
 pv(1891) = pv(1891)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d2(j0,l0)/mu
 pv(1892) = pv(1892)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d2(j0,l0)/mu
 pv(1911) = pv(1911)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(1914) = pv(1914)+d2(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(1916) = pv(1916)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(1917) = pv(1917)+d(i0,j0)*d2(j0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(1922) = pv(1922)+d(i0,j0)*d(i0,j1)*d2(i0,k0)*d(k0,l0)/mu
 pv(1923) = pv(1923)+d(i0,j0)*d(j0,j1)*d2(i0,k0)*d(k0,l0)/mu
 pv(1954) = pv(1954)+d2(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(1956) = pv(1956)+d(i0,j0)*d2(i0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(1958) = pv(1958)+d2(i0,j0)*d(j0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(1959) = pv(1959)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(1969) = pv(1969)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1971) = pv(1971)+d2(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1974) = pv(1974)+d(i0,j1)*d(i0,j2)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1976) = pv(1976)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1978) = pv(1978)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1979) = pv(1979)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1980) = pv(1980)+d2(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1984) = pv(1984)+d(i0,j1)*d2(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1985) = pv(1985)+d(j0,j1)*d2(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(2007) = pv(2007)+d(i0,j0)*d(i0,j1)*d2(j0,k0)*d(k0,l0)/mu
 pv(2008) = pv(2008)+d(i0,j0)*d(j0,j1)*d2(j0,k0)*d(k0,l0)/mu
 pv(2013) = pv(2013)+d(i0,j1)*d(i0,k0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2014) = pv(2014)+d(j0,j1)*d(i0,k0)*d2(j0,k0)*d(k0,l0)/mu
 pv(2023) = pv(2023)+d2(i0,j0)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2025) = pv(2025)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2026) = pv(2026)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2028) = pv(2028)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2030) = pv(2030)+d(i0,j0)*d2(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2103) = pv(2103)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2115) = pv(2115)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2116) = pv(2116)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2120) = pv(2120)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2121) = pv(2121)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2128) = pv(2128)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2145) = pv(2145)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2146) = pv(2146)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2152) = pv(2152)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2156) = pv(2156)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2157) = pv(2157)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2165) = pv(2165)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2166) = pv(2166)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2190) = pv(2190)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d2(k0,l0)/mu
 pv(2191) = pv(2191)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d2(k0,l0)/mu
 pv(2202) = pv(2202)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d2(k0,l0)/mu
 pv(2203) = pv(2203)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_i0jjk0
SUBROUTINE mg3331_iijjk0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(171) = pv(171)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(172) = pv(172)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,k0)/mu
 pv(174) = pv(174)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)/mu
 pv(189) = pv(189)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(190) = pv(190)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(214) = pv(214)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(216) = pv(216)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(218) = pv(218)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(j0,k0)/mu
 pv(229) = pv(229)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(231) = pv(231)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(233) = pv(233)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(234) = pv(234)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(236) = pv(236)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(238) = pv(238)+d(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(251) = pv(251)+d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(252) = pv(252)+d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(289) = pv(289)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(291) = pv(291)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(295) = pv(295)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(297) = pv(297)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
endif
if (5.le.mxd) then
 pv(648) = pv(648)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(649) = pv(649)+d(i0,j0)*d2(i1,j0)*d(i0,j1)*d(i0,k0)/mu
 pv(650) = pv(650)+d(i0,i1)*d2(i0,j0)*d(j0,j1)*d(i0,k0)/mu
 pv(652) = pv(652)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)/mu
 pv(653) = pv(653)+d2(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)/mu
 pv(655) = pv(655)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)/mu
 pv(656) = pv(656)+d(i0,i1)*d(i0,j0)*d2(j0,j1)*d(i0,k0)/mu
 pv(665) = pv(665)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d2(i0,k0)/mu
 pv(666) = pv(666)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d2(i0,k0)/mu
 pv(668) = pv(668)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d2(i0,k0)/mu
 pv(685) = pv(685)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(686) = pv(686)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(687) = pv(687)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(688) = pv(688)+d2(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(689) = pv(689)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(690) = pv(690)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(691) = pv(691)+d(i0,j0)*d2(j0,j1)*d(i0,k0)*d(i1,k0)/mu
 pv(701) = pv(701)+d(i0,j0)*d(i0,j1)*d2(i0,k0)*d(i1,k0)/mu
 pv(702) = pv(702)+d(i1,j0)*d(i0,j1)*d2(i0,k0)*d(i1,k0)/mu
 pv(703) = pv(703)+d(i0,j0)*d(j0,j1)*d2(i0,k0)*d(i1,k0)/mu
 pv(704) = pv(704)+d(i1,j0)*d(j0,j1)*d2(i0,k0)*d(i1,k0)/mu
 pv(743) = pv(743)+d(i0,i1)*d2(i0,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(745) = pv(745)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(746) = pv(746)+d2(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,k0)/mu
 pv(748) = pv(748)+d(i0,j0)*d(i1,j0)*d2(i0,j1)*d(j0,k0)/mu
 pv(750) = pv(750)+d2(i0,i1)*d(i0,j0)*d(j0,j1)*d(j0,k0)/mu
 pv(751) = pv(751)+d(i0,i1)*d2(i0,j0)*d(j0,j1)*d(j0,k0)/mu
 pv(753) = pv(753)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,k0)/mu
 pv(771) = pv(771)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(773) = pv(773)+d(i0,i1)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(774) = pv(774)+d(i0,i2)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(775) = pv(775)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(776) = pv(776)+d2(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(777) = pv(777)+d(i1,j0)*d(i2,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(778) = pv(778)+d(i0,i1)*d2(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(780) = pv(780)+d(i1,j0)*d2(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(782) = pv(782)+d(i0,i1)*d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(783) = pv(783)+d(i0,j0)*d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(784) = pv(784)+d2(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(785) = pv(785)+d(i0,i1)*d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(786) = pv(786)+d(i0,j0)*d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(787) = pv(787)+d(i1,j0)*d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(788) = pv(788)+d(i2,j0)*d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(789) = pv(789)+d2(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(791) = pv(791)+d(i1,j0)*d(i0,j1)*d(i0,j2)*d(i0,k0)*d(j0,k0)/mu
 pv(792) = pv(792)+d2(i0,i1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(793) = pv(793)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(795) = pv(795)+d(i0,i1)*d(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(796) = pv(796)+d(i0,i2)*d(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(797) = pv(797)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(798) = pv(798)+d2(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(799) = pv(799)+d(i0,i1)*d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(801) = pv(801)+d(i1,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(803) = pv(803)+d(i1,j0)*d(i1,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(804) = pv(804)+d(i0,j1)*d(i1,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(805) = pv(805)+d(i0,i1)*d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(807) = pv(807)+d(i1,j0)*d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(810) = pv(810)+d(i0,i1)*d2(j0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(822) = pv(822)+d(i0,i1)*d(i0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(824) = pv(824)+d(i1,j0)*d(i0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(826) = pv(826)+d(i1,j0)*d(i1,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(827) = pv(827)+d(i0,j1)*d(i1,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(829) = pv(829)+d(i0,i1)*d(j0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(831) = pv(831)+d(i1,j0)*d(j0,j1)*d2(i0,k0)*d(j0,k0)/mu
 pv(849) = pv(849)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(850) = pv(850)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(851) = pv(851)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(852) = pv(852)+d(i2,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(853) = pv(853)+d2(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(854) = pv(854)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(855) = pv(855)+d(i0,j1)*d(i0,j2)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(856) = pv(856)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(857) = pv(857)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(858) = pv(858)+d(i2,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(859) = pv(859)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(860) = pv(860)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(861) = pv(861)+d2(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(866) = pv(866)+d(i0,j1)*d2(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(867) = pv(867)+d(j0,j1)*d2(i0,k0)*d(i1,k0)*d(j0,k0)/mu
 pv(946) = pv(946)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d2(j0,k0)/mu
 pv(948) = pv(948)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d2(j0,k0)/mu
 pv(950) = pv(950)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d2(j0,k0)/mu
 pv(961) = pv(961)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(963) = pv(963)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(965) = pv(965)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(966) = pv(966)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(968) = pv(968)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(970) = pv(970)+d(i1,j0)*d(j0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(983) = pv(983)+d(i0,j1)*d(i0,k0)*d(i1,k0)*d2(j0,k0)/mu
 pv(984) = pv(984)+d(j0,j1)*d(i0,k0)*d(i1,k0)*d2(j0,k0)/mu
 pv(1022) = pv(1022)+d2(i0,i1)*d(i0,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(1023) = pv(1023)+d(i0,i1)*d2(i0,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(1025) = pv(1025)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(1026) = pv(1026)+d2(i0,j0)*d(i1,j0)*d(j0,k0)*d(j1,k0)/mu
 pv(1027) = pv(1027)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(1029) = pv(1029)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(1031) = pv(1031)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(j0,k0)*d(j1,k0)/mu
 pv(1034) = pv(1034)+d2(i0,i1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1035) = pv(1035)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1037) = pv(1037)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1038) = pv(1038)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1039) = pv(1039)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1040) = pv(1040)+d2(i1,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1041) = pv(1041)+d(i1,j0)*d(i2,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1043) = pv(1043)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1044) = pv(1044)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1045) = pv(1045)+d(i0,i1)*d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1047) = pv(1047)+d(i1,j0)*d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1049) = pv(1049)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1051) = pv(1051)+d(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1055) = pv(1055)+d(i0,i1)*d2(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1057) = pv(1057)+d(i1,j0)*d2(i0,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1061) = pv(1061)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j1,k0)/mu
 pv(1078) = pv(1078)+d(i0,i1)*d(i0,j0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1080) = pv(1080)+d(i0,j0)*d(i1,j0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1081) = pv(1081)+d(i0,i1)*d(i0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1083) = pv(1083)+d(i1,j0)*d(i0,j1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1091) = pv(1091)+d(i0,i1)*d(i0,k0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1093) = pv(1093)+d(i1,j0)*d(i0,k0)*d2(j0,k0)*d(j1,k0)/mu
 pv(1435) = pv(1435)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)*d(i0,l0)/mu
 pv(1446) = pv(1446)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(i0,l0)/mu
 pv(1464) = pv(1464)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(1465) = pv(1465)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(j0,k0)*d(i0,l0)/mu
 pv(1476) = pv(1476)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1478) = pv(1478)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1479) = pv(1479)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1480) = pv(1480)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1482) = pv(1482)+d(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1495) = pv(1495)+d(i0,j0)*d(i0,j1)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1496) = pv(1496)+d(i0,j0)*d(j0,j1)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1501) = pv(1501)+d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1502) = pv(1502)+d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(i0,l0)/mu
 pv(1534) = pv(1534)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1536) = pv(1536)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1539) = pv(1539)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1541) = pv(1541)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1545) = pv(1545)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1660) = pv(1660)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(1661) = pv(1661)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,l0)/mu
 pv(1673) = pv(1673)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(1674) = pv(1674)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,l0)/mu
 pv(1685) = pv(1685)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,l0)/mu
 pv(1695) = pv(1695)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1697) = pv(1697)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1699) = pv(1699)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1700) = pv(1700)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1702) = pv(1702)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1715) = pv(1715)+d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1716) = pv(1716)+d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,l0)/mu
 pv(1750) = pv(1750)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1752) = pv(1752)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1756) = pv(1756)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1758) = pv(1758)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1761) = pv(1761)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1763) = pv(1763)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1912) = pv(1912)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(1913) = pv(1913)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(1915) = pv(1915)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)*d(k0,l0)/mu
 pv(1931) = pv(1931)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(1932) = pv(1932)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(k0,l0)/mu
 pv(1953) = pv(1953)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(1955) = pv(1955)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(1957) = pv(1957)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(j0,k0)*d(k0,l0)/mu
 pv(1968) = pv(1968)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1970) = pv(1970)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1972) = pv(1972)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1973) = pv(1973)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1975) = pv(1975)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(1977) = pv(1977)+d(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,l0)/mu
 pv(2022) = pv(2022)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2024) = pv(2024)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
endif
END SUBROUTINE mg3331_iijjk0
SUBROUTINE mg3331_kk (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(17) = pv(17)+d(k0,k1)*d(k0,l0)/mu
endif
if (3.le.mxd) then
 pv(133) = pv(133)+d2(k0,k1)*d(k0,l0)/mu
 pv(138) = pv(138)+d(k0,k1)*d2(k0,l0)/mu
endif
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
END SUBROUTINE mg3331_kk
SUBROUTINE mg3331_i0kk (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(7) = pv(7)+d(i0,k0)*d(k0,k1)/mu
endif
if (3.le.mxd) then
 pv(43) = pv(43)+d2(i0,k0)*d(i0,k1)/mu
 pv(74) = pv(74)+d2(i0,k0)*d(k0,k1)/mu
 pv(76) = pv(76)+d(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(83) = pv(83)+d(i0,k0)*d2(k0,k1)/mu
 pv(95) = pv(95)+d(i0,k0)*d(i0,k1)*d(i0,l0)/mu
 pv(98) = pv(98)+d(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(124) = pv(124)+d(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(131) = pv(131)+d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(202) = pv(202)+d3(i0,k0)*d(i0,k1)/mu
 pv(207) = pv(207)+d2(i0,k0)*d2(i0,k1)/mu
 pv(343) = pv(343)+d3(i0,k0)*d(k0,k1)/mu
 pv(349) = pv(349)+d2(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(351) = pv(351)+d2(i0,k0)*d(i0,k2)*d(k0,k1)/mu
 pv(382) = pv(382)+d2(i0,k0)*d2(k0,k1)/mu
 pv(383) = pv(383)+d(i0,k0)*d(i0,k1)*d2(k0,k1)/mu
 pv(410) = pv(410)+d2(i0,k0)*d(i0,k1)*d(i0,l0)/mu
 pv(435) = pv(435)+d2(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(437) = pv(437)+d(i0,k0)*d(i0,k1)*d(k0,k1)*d(i0,l0)/mu
 pv(440) = pv(440)+d(i0,k0)*d2(k0,k1)*d(i0,l0)/mu
 pv(448) = pv(448)+d(i0,k0)*d(k0,k1)*d2(i0,l0)/mu
 pv(534) = pv(534)+d2(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(536) = pv(536)+d(i0,k0)*d2(i0,k1)*d(k0,l0)/mu
 pv(567) = pv(567)+d2(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(568) = pv(568)+d(i0,k0)*d(i0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(577) = pv(577)+d(i0,k0)*d(i0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(580) = pv(580)+d(i0,k0)*d(k0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(594) = pv(594)+d(i0,k0)*d(i0,k1)*d2(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(724) = pv(724)+d4(i0,k0)*d(i0,k1)/mu
 pv(735) = pv(735)+d3(i0,k0)*d2(i0,k1)/mu
 pv(1239) = pv(1239)+d4(i0,k0)*d(k0,k1)/mu
 pv(1254) = pv(1254)+d3(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1258) = pv(1258)+d2(i0,k0)*d2(i0,k1)*d(k0,k1)/mu
 pv(1260) = pv(1260)+d3(i0,k0)*d(i0,k2)*d(k0,k1)/mu
 pv(1390) = pv(1390)+d3(i0,k0)*d2(k0,k1)/mu
 pv(1393) = pv(1393)+d2(i0,k0)*d(i0,k1)*d2(k0,k1)/mu
 pv(1394) = pv(1394)+d2(i0,k0)*d(i0,k2)*d2(k0,k1)/mu
 pv(1456) = pv(1456)+d3(i0,k0)*d(i0,k1)*d(i0,l0)/mu
 pv(1575) = pv(1575)+d3(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1579) = pv(1579)+d2(i0,k0)*d(i0,k1)*d(k0,k1)*d(i0,l0)/mu
 pv(1603) = pv(1603)+d2(i0,k0)*d2(k0,k1)*d(i0,l0)/mu
 pv(1604) = pv(1604)+d(i0,k0)*d(i0,k1)*d2(k0,k1)*d(i0,l0)/mu
 pv(1630) = pv(1630)+d2(i0,k0)*d(k0,k1)*d2(i0,l0)/mu
 pv(1634) = pv(1634)+d(i0,k0)*d2(k0,k1)*d2(i0,l0)/mu
 pv(1941) = pv(1941)+d3(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1945) = pv(1945)+d2(i0,k0)*d2(i0,k1)*d(k0,l0)/mu
 pv(1947) = pv(1947)+d(i0,k0)*d3(i0,k1)*d(k0,l0)/mu
 pv(2077) = pv(2077)+d3(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2080) = pv(2080)+d2(i0,k0)*d(i0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2081) = pv(2081)+d(i0,k0)*d2(i0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2082) = pv(2082)+d2(i0,k0)*d(i0,k2)*d(k0,k1)*d(k0,l0)/mu
 pv(2110) = pv(2110)+d2(i0,k0)*d(i0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2134) = pv(2134)+d2(i0,k0)*d(k0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2135) = pv(2135)+d(i0,k0)*d(i0,k1)*d(k0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2141) = pv(2141)+d(i0,k0)*d(k0,k1)*d2(i0,l0)*d(k0,l0)/mu
 pv(2197) = pv(2197)+d2(i0,k0)*d(i0,k1)*d2(k0,l0)/mu
 pv(2198) = pv(2198)+d(i0,k0)*d2(i0,k1)*d2(k0,l0)/mu
 pv(2218) = pv(2218)+d(i0,k0)*d(i0,k1)*d(i0,l0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_i0kk
SUBROUTINE mg3331_iikk (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(41) = pv(41)+d(i0,i1)*d(i0,k0)*d(i0,k1)/mu
 pv(44) = pv(44)+d(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(72) = pv(72)+d(i0,i1)*d(i0,k0)*d(k0,k1)/mu
 pv(75) = pv(75)+d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
endif
if (4.le.mxd) then
 pv(200) = pv(200)+d(i0,i1)*d2(i0,k0)*d(i0,k1)/mu
 pv(203) = pv(203)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(205) = pv(205)+d2(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(206) = pv(206)+d(i0,k0)*d2(i1,k0)*d(i0,k1)/mu
 pv(208) = pv(208)+d(i0,k0)*d(i1,k0)*d2(i0,k1)/mu
 pv(335) = pv(335)+d2(i0,i1)*d(i0,k0)*d(k0,k1)/mu
 pv(341) = pv(341)+d(i0,i1)*d2(i0,k0)*d(k0,k1)/mu
 pv(344) = pv(344)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(346) = pv(346)+d2(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(347) = pv(347)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(350) = pv(350)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(380) = pv(380)+d(i0,i1)*d(i0,k0)*d2(k0,k1)/mu
 pv(411) = pv(411)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(i0,l0)/mu
 pv(433) = pv(433)+d(i0,i1)*d(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(436) = pv(436)+d(i0,k0)*d(i1,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(532) = pv(532)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(535) = pv(535)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(565) = pv(565)+d(i0,i1)*d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(722) = pv(722)+d(i0,i1)*d3(i0,k0)*d(i0,k1)/mu
 pv(728) = pv(728)+d(i0,i1)*d2(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(730) = pv(730)+d3(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(731) = pv(731)+d(i0,i1)*d(i0,k0)*d2(i1,k0)*d(i0,k1)/mu
 pv(733) = pv(733)+d2(i0,k0)*d2(i1,k0)*d(i0,k1)/mu
 pv(734) = pv(734)+d(i0,k0)*d3(i1,k0)*d(i0,k1)/mu
 pv(736) = pv(736)+d2(i0,k0)*d(i1,k0)*d2(i0,k1)/mu
 pv(1231) = pv(1231)+d2(i0,i1)*d2(i0,k0)*d(k0,k1)/mu
 pv(1237) = pv(1237)+d(i0,i1)*d3(i0,k0)*d(k0,k1)/mu
 pv(1240) = pv(1240)+d2(i0,i1)*d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1246) = pv(1246)+d(i0,i1)*d2(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1248) = pv(1248)+d3(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1252) = pv(1252)+d(i0,i1)*d2(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1255) = pv(1255)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1257) = pv(1257)+d2(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1259) = pv(1259)+d(i0,k0)*d(i1,k0)*d2(i0,k1)*d(k0,k1)/mu
 pv(1382) = pv(1382)+d2(i0,i1)*d(i0,k0)*d2(k0,k1)/mu
 pv(1388) = pv(1388)+d(i0,i1)*d2(i0,k0)*d2(k0,k1)/mu
 pv(1391) = pv(1391)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d2(k0,k1)/mu
 pv(1457) = pv(1457)+d2(i0,k0)*d(i1,k0)*d(i0,k1)*d(i0,l0)/mu
 pv(1458) = pv(1458)+d(i0,k0)*d2(i1,k0)*d(i0,k1)*d(i0,l0)/mu
 pv(1573) = pv(1573)+d(i0,i1)*d2(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1576) = pv(1576)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1578) = pv(1578)+d2(i0,k0)*d(i1,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1580) = pv(1580)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,k1)*d(i0,l0)/mu
 pv(1601) = pv(1601)+d(i0,i1)*d(i0,k0)*d2(k0,k1)*d(i0,l0)/mu
 pv(1631) = pv(1631)+d(i0,k0)*d(i1,k0)*d(k0,k1)*d2(i0,l0)/mu
 pv(1939) = pv(1939)+d(i0,i1)*d2(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1942) = pv(1942)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1944) = pv(1944)+d2(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1946) = pv(1946)+d(i0,k0)*d(i1,k0)*d2(i0,k1)*d(k0,l0)/mu
 pv(2069) = pv(2069)+d2(i0,i1)*d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2075) = pv(2075)+d(i0,i1)*d2(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2078) = pv(2078)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2111) = pv(2111)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2132) = pv(2132)+d(i0,i1)*d(i0,k0)*d(k0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2195) = pv(2195)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_iikk
SUBROUTINE mg3331_j0kk (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(8) = pv(8)+d(j0,k0)*d(k0,k1)/mu
endif
if (3.le.mxd) then
 pv(70) = pv(70)+d2(j0,k0)*d(j0,k1)/mu
 pv(80) = pv(80)+d2(j0,k0)*d(k0,k1)/mu
 pv(82) = pv(82)+d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(84) = pv(84)+d(j0,k0)*d2(k0,k1)/mu
 pv(114) = pv(114)+d(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(115) = pv(115)+d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(130) = pv(130)+d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(132) = pv(132)+d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
endif
if (4.le.mxd) then
 pv(326) = pv(326)+d3(j0,k0)*d(j0,k1)/mu
 pv(333) = pv(333)+d2(j0,k0)*d2(j0,k1)/mu
 pv(370) = pv(370)+d3(j0,k0)*d(k0,k1)/mu
 pv(377) = pv(377)+d2(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(379) = pv(379)+d2(j0,k0)*d(j0,k2)*d(k0,k1)/mu
 pv(386) = pv(386)+d2(j0,k0)*d2(k0,k1)/mu
 pv(387) = pv(387)+d(j0,k0)*d(j0,k1)*d2(k0,k1)/mu
 pv(492) = pv(492)+d2(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(498) = pv(498)+d2(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(500) = pv(500)+d(j0,k0)*d(j0,k1)*d(k0,k1)*d(j0,l0)/mu
 pv(501) = pv(501)+d(j0,k0)*d2(k0,k1)*d(j0,l0)/mu
 pv(518) = pv(518)+d(j0,k0)*d(k0,k1)*d2(j0,l0)/mu
 pv(562) = pv(562)+d2(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(564) = pv(564)+d(j0,k0)*d2(j0,k1)*d(k0,l0)/mu
 pv(571) = pv(571)+d2(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(572) = pv(572)+d(j0,k0)*d(j0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(588) = pv(588)+d(j0,k0)*d(j0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(589) = pv(589)+d(j0,k0)*d(k0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(598) = pv(598)+d(j0,k0)*d(j0,k1)*d2(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(1183) = pv(1183)+d4(j0,k0)*d(j0,k1)/mu
 pv(1216) = pv(1216)+d3(j0,k0)*d2(j0,k1)/mu
 pv(1344) = pv(1344)+d4(j0,k0)*d(k0,k1)/mu
 pv(1372) = pv(1372)+d3(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1377) = pv(1377)+d2(j0,k0)*d2(j0,k1)*d(k0,k1)/mu
 pv(1381) = pv(1381)+d3(j0,k0)*d(j0,k2)*d(k0,k1)/mu
 pv(1406) = pv(1406)+d3(j0,k0)*d2(k0,k1)/mu
 pv(1409) = pv(1409)+d2(j0,k0)*d(j0,k1)*d2(k0,k1)/mu
 pv(1410) = pv(1410)+d2(j0,k0)*d(j0,k2)*d2(k0,k1)/mu
 pv(1788) = pv(1788)+d3(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1817) = pv(1817)+d3(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1823) = pv(1823)+d2(j0,k0)*d(j0,k1)*d(k0,k1)*d(j0,l0)/mu
 pv(1828) = pv(1828)+d2(j0,k0)*d2(k0,k1)*d(j0,l0)/mu
 pv(1829) = pv(1829)+d(j0,k0)*d(j0,k1)*d2(k0,k1)*d(j0,l0)/mu
 pv(1897) = pv(1897)+d2(j0,k0)*d(k0,k1)*d2(j0,l0)/mu
 pv(1899) = pv(1899)+d(j0,k0)*d2(k0,k1)*d2(j0,l0)/mu
 pv(2058) = pv(2058)+d3(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2066) = pv(2066)+d2(j0,k0)*d2(j0,k1)*d(k0,l0)/mu
 pv(2068) = pv(2068)+d(j0,k0)*d3(j0,k1)*d(k0,l0)/mu
 pv(2094) = pv(2094)+d3(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2098) = pv(2098)+d2(j0,k0)*d(j0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2099) = pv(2099)+d(j0,k0)*d2(j0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2100) = pv(2100)+d2(j0,k0)*d(j0,k2)*d(k0,k1)*d(k0,l0)/mu
 pv(2171) = pv(2171)+d2(j0,k0)*d(j0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2176) = pv(2176)+d2(j0,k0)*d(k0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2177) = pv(2177)+d(j0,k0)*d(j0,k1)*d(k0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2185) = pv(2185)+d(j0,k0)*d(k0,k1)*d2(j0,l0)*d(k0,l0)/mu
 pv(2213) = pv(2213)+d2(j0,k0)*d(j0,k1)*d2(k0,l0)/mu
 pv(2214) = pv(2214)+d(j0,k0)*d2(j0,k1)*d2(k0,l0)/mu
 pv(2225) = pv(2225)+d(j0,k0)*d(j0,k1)*d(j0,l0)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_j0kk
SUBROUTINE mg3331_i0j0kk (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(42) = pv(42)+d(i0,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(58) = pv(58)+d(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(59) = pv(59)+d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(67) = pv(67)+d(i0,j0)*d(j0,k0)*d(j0,k1)/mu
 pv(69) = pv(69)+d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(73) = pv(73)+d(i0,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(77) = pv(77)+d(i0,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(79) = pv(79)+d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
endif
if (4.le.mxd) then
 pv(197) = pv(197)+d2(i0,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(201) = pv(201)+d(i0,j0)*d2(i0,k0)*d(i0,k1)/mu
 pv(255) = pv(255)+d2(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(260) = pv(260)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(264) = pv(264)+d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(267) = pv(267)+d(i0,j0)*d2(i0,k1)*d(j0,k0)/mu
 pv(268) = pv(268)+d(i0,k0)*d2(i0,k1)*d(j0,k0)/mu
 pv(270) = pv(270)+d(i0,j0)*d(i0,k1)*d(i0,k2)*d(j0,k0)/mu
 pv(284) = pv(284)+d(i0,j0)*d(i0,k1)*d2(j0,k0)/mu
 pv(285) = pv(285)+d(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(311) = pv(311)+d2(i0,j0)*d(j0,k0)*d(j0,k1)/mu
 pv(314) = pv(314)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(318) = pv(318)+d2(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(320) = pv(320)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(321) = pv(321)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(j0,k1)/mu
 pv(322) = pv(322)+d(i0,j0)*d2(j0,k0)*d(j0,k1)/mu
 pv(324) = pv(324)+d(i0,k0)*d2(j0,k0)*d(j0,k1)/mu
 pv(325) = pv(325)+d(i0,k1)*d2(j0,k0)*d(j0,k1)/mu
 pv(337) = pv(337)+d2(i0,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(342) = pv(342)+d(i0,j0)*d2(i0,k0)*d(k0,k1)/mu
 pv(348) = pv(348)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(353) = pv(353)+d2(i0,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(359) = pv(359)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(363) = pv(363)+d2(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(364) = pv(364)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(365) = pv(365)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(366) = pv(366)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(367) = pv(367)+d(i0,j0)*d2(j0,k0)*d(k0,k1)/mu
 pv(369) = pv(369)+d(i0,k0)*d2(j0,k0)*d(k0,k1)/mu
 pv(374) = pv(374)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(376) = pv(376)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(381) = pv(381)+d(i0,j0)*d(i0,k0)*d2(k0,k1)/mu
 pv(384) = pv(384)+d(i0,j0)*d(j0,k0)*d2(k0,k1)/mu
 pv(425) = pv(425)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(426) = pv(426)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(431) = pv(431)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(432) = pv(432)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(434) = pv(434)+d(i0,j0)*d(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(438) = pv(438)+d(i0,j0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(439) = pv(439)+d(i0,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(468) = pv(468)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,l0)/mu
 pv(479) = pv(479)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(480) = pv(480)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(491) = pv(491)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(494) = pv(494)+d(i0,j0)*d(i0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(495) = pv(495)+d(i0,j0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(497) = pv(497)+d(i0,k0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(533) = pv(533)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(549) = pv(549)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(550) = pv(550)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(558) = pv(558)+d(i0,j0)*d(i0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(559) = pv(559)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(561) = pv(561)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(566) = pv(566)+d(i0,j0)*d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(569) = pv(569)+d(i0,j0)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(713) = pv(713)+d3(i0,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(719) = pv(719)+d2(i0,j0)*d2(i0,k0)*d(i0,k1)/mu
 pv(723) = pv(723)+d(i0,j0)*d3(i0,k0)*d(i0,k1)/mu
 pv(870) = pv(870)+d3(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(879) = pv(879)+d2(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(897) = pv(897)+d(i0,j0)*d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(901) = pv(901)+d3(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(916) = pv(916)+d2(i0,j0)*d2(i0,k1)*d(j0,k0)/mu
 pv(920) = pv(920)+d(i0,j0)*d(i0,k0)*d2(i0,k1)*d(j0,k0)/mu
 pv(923) = pv(923)+d2(i0,k0)*d2(i0,k1)*d(j0,k0)/mu
 pv(926) = pv(926)+d(i0,j0)*d3(i0,k1)*d(j0,k0)/mu
 pv(927) = pv(927)+d(i0,k0)*d3(i0,k1)*d(j0,k0)/mu
 pv(936) = pv(936)+d2(i0,j0)*d(i0,k1)*d(i0,k2)*d(j0,k0)/mu
 pv(939) = pv(939)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(i0,k2)*d(j0,k0)/mu
 pv(987) = pv(987)+d2(i0,j0)*d(i0,k1)*d2(j0,k0)/mu
 pv(992) = pv(992)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(996) = pv(996)+d2(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(999) = pv(999)+d(i0,j0)*d2(i0,k1)*d2(j0,k0)/mu
 pv(1000) = pv(1000)+d(i0,k0)*d2(i0,k1)*d2(j0,k0)/mu
 pv(1002) = pv(1002)+d(i0,j0)*d(i0,k1)*d(i0,k2)*d2(j0,k0)/mu
 pv(1016) = pv(1016)+d(i0,j0)*d(i0,k1)*d3(j0,k0)/mu
 pv(1017) = pv(1017)+d(i0,k0)*d(i0,k1)*d3(j0,k0)/mu
 pv(1112) = pv(1112)+d3(i0,j0)*d(j0,k0)*d(j0,k1)/mu
 pv(1118) = pv(1118)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1135) = pv(1135)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1139) = pv(1139)+d3(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1147) = pv(1147)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(1151) = pv(1151)+d2(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(1155) = pv(1155)+d2(i0,j0)*d(i0,k2)*d(j0,k0)*d(j0,k1)/mu
 pv(1157) = pv(1157)+d(i0,j0)*d(i0,k0)*d(i0,k2)*d(j0,k0)*d(j0,k1)/mu
 pv(1158) = pv(1158)+d2(i0,k0)*d(i0,k2)*d(j0,k0)*d(j0,k1)/mu
 pv(1160) = pv(1160)+d(i0,j0)*d2(i0,k2)*d(j0,k0)*d(j0,k1)/mu
 pv(1162) = pv(1162)+d2(i0,j0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1165) = pv(1165)+d(i0,j0)*d(i0,k0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1169) = pv(1169)+d2(i0,k0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1172) = pv(1172)+d(i0,j0)*d(i0,k1)*d2(j0,k0)*d(j0,k1)/mu
 pv(1174) = pv(1174)+d(i0,k0)*d(i0,k1)*d2(j0,k0)*d(j0,k1)/mu
 pv(1176) = pv(1176)+d2(i0,k1)*d2(j0,k0)*d(j0,k1)/mu
 pv(1177) = pv(1177)+d(i0,j0)*d(i0,k2)*d2(j0,k0)*d(j0,k1)/mu
 pv(1178) = pv(1178)+d(i0,k0)*d(i0,k2)*d2(j0,k0)*d(j0,k1)/mu
 pv(1179) = pv(1179)+d(i0,j0)*d3(j0,k0)*d(j0,k1)/mu
 pv(1181) = pv(1181)+d(i0,k0)*d3(j0,k0)*d(j0,k1)/mu
 pv(1182) = pv(1182)+d(i0,k1)*d3(j0,k0)*d(j0,k1)/mu
 pv(1215) = pv(1215)+d(i0,k0)*d2(j0,k0)*d2(j0,k1)/mu
 pv(1220) = pv(1220)+d3(i0,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(1233) = pv(1233)+d2(i0,j0)*d2(i0,k0)*d(k0,k1)/mu
 pv(1238) = pv(1238)+d(i0,j0)*d3(i0,k0)*d(k0,k1)/mu
 pv(1249) = pv(1249)+d2(i0,j0)*d(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1253) = pv(1253)+d(i0,j0)*d2(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1263) = pv(1263)+d3(i0,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(1275) = pv(1275)+d2(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1295) = pv(1295)+d(i0,j0)*d2(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1299) = pv(1299)+d3(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1302) = pv(1302)+d2(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1307) = pv(1307)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1311) = pv(1311)+d2(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1313) = pv(1313)+d(i0,j0)*d2(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1314) = pv(1314)+d(i0,k0)*d2(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1317) = pv(1317)+d2(i0,j0)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1321) = pv(1321)+d(i0,j0)*d(i0,k0)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1322) = pv(1322)+d2(i0,k0)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1324) = pv(1324)+d(i0,j0)*d(i0,k1)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1325) = pv(1325)+d(i0,j0)*d2(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1327) = pv(1327)+d2(i0,j0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1333) = pv(1333)+d(i0,j0)*d(i0,k0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1337) = pv(1337)+d2(i0,k0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1338) = pv(1338)+d(i0,j0)*d(i0,k1)*d2(j0,k0)*d(k0,k1)/mu
 pv(1339) = pv(1339)+d(i0,k0)*d(i0,k1)*d2(j0,k0)*d(k0,k1)/mu
 pv(1340) = pv(1340)+d(i0,j0)*d(i0,k2)*d2(j0,k0)*d(k0,k1)/mu
 pv(1341) = pv(1341)+d(i0,j0)*d3(j0,k0)*d(k0,k1)/mu
 pv(1343) = pv(1343)+d(i0,k0)*d3(j0,k0)*d(k0,k1)/mu
 pv(1358) = pv(1358)+d2(i0,j0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1361) = pv(1361)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1365) = pv(1365)+d2(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1366) = pv(1366)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1367) = pv(1367)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1368) = pv(1368)+d(i0,j0)*d2(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1370) = pv(1370)+d(i0,k0)*d2(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1371) = pv(1371)+d(i0,k1)*d2(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1379) = pv(1379)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,k2)*d(k0,k1)/mu
 pv(1380) = pv(1380)+d(i0,k0)*d2(j0,k0)*d(j0,k2)*d(k0,k1)/mu
 pv(1384) = pv(1384)+d2(i0,j0)*d(i0,k0)*d2(k0,k1)/mu
 pv(1389) = pv(1389)+d(i0,j0)*d2(i0,k0)*d2(k0,k1)/mu
 pv(1392) = pv(1392)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d2(k0,k1)/mu
 pv(1396) = pv(1396)+d2(i0,j0)*d(j0,k0)*d2(k0,k1)/mu
 pv(1401) = pv(1401)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d2(k0,k1)/mu
 pv(1402) = pv(1402)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d2(k0,k1)/mu
 pv(1403) = pv(1403)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d2(k0,k1)/mu
 pv(1404) = pv(1404)+d(i0,j0)*d2(j0,k0)*d2(k0,k1)/mu
 pv(1407) = pv(1407)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d2(k0,k1)/mu
 pv(1505) = pv(1505)+d2(i0,j0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1508) = pv(1508)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1511) = pv(1511)+d2(i0,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1514) = pv(1514)+d(i0,j0)*d2(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1530) = pv(1530)+d(i0,j0)*d(i0,k1)*d2(j0,k0)*d(i0,l0)/mu
 pv(1531) = pv(1531)+d(i0,k0)*d(i0,k1)*d2(j0,k0)*d(i0,l0)/mu
 pv(1552) = pv(1552)+d2(i0,j0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1555) = pv(1555)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1559) = pv(1559)+d2(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1562) = pv(1562)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1563) = pv(1563)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1564) = pv(1564)+d(i0,j0)*d2(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1565) = pv(1565)+d(i0,k0)*d2(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1566) = pv(1566)+d(i0,k1)*d2(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1570) = pv(1570)+d2(i0,j0)*d(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1574) = pv(1574)+d(i0,j0)*d2(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1582) = pv(1582)+d2(i0,j0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1587) = pv(1587)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1591) = pv(1591)+d2(i0,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1593) = pv(1593)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1594) = pv(1594)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1595) = pv(1595)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1596) = pv(1596)+d(i0,j0)*d2(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1597) = pv(1597)+d(i0,k0)*d2(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1599) = pv(1599)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d(k0,k1)*d(i0,l0)/mu
 pv(1600) = pv(1600)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)*d(i0,l0)/mu
 pv(1602) = pv(1602)+d(i0,j0)*d(i0,k0)*d2(k0,k1)*d(i0,l0)/mu
 pv(1605) = pv(1605)+d(i0,j0)*d(j0,k0)*d2(k0,k1)*d(i0,l0)/mu
 pv(1628) = pv(1628)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d2(i0,l0)/mu
 pv(1629) = pv(1629)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d2(i0,l0)/mu
 pv(1632) = pv(1632)+d(i0,j0)*d(j0,k0)*d(k0,k1)*d2(i0,l0)/mu
 pv(1633) = pv(1633)+d(i0,k0)*d(j0,k0)*d(k0,k1)*d2(i0,l0)/mu
 pv(1677) = pv(1677)+d2(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,l0)/mu
 pv(1680) = pv(1680)+d(i0,j0)*d2(i0,k0)*d(i0,k1)*d(j0,l0)/mu
 pv(1719) = pv(1719)+d2(i0,j0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1722) = pv(1722)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1726) = pv(1726)+d2(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1729) = pv(1729)+d(i0,j0)*d2(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1730) = pv(1730)+d(i0,k0)*d2(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1732) = pv(1732)+d(i0,j0)*d(i0,k1)*d(i0,k2)*d(j0,k0)*d(j0,l0)/mu
 pv(1743) = pv(1743)+d(i0,j0)*d(i0,k1)*d2(j0,k0)*d(j0,l0)/mu
 pv(1744) = pv(1744)+d(i0,k0)*d(i0,k1)*d2(j0,k0)*d(j0,l0)/mu
 pv(1782) = pv(1782)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1784) = pv(1784)+d2(i0,k0)*d(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1786) = pv(1786)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1787) = pv(1787)+d(i0,k0)*d2(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1795) = pv(1795)+d2(i0,j0)*d(i0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1799) = pv(1799)+d(i0,j0)*d2(i0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1801) = pv(1801)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(k0,k1)*d(j0,l0)/mu
 pv(1803) = pv(1803)+d2(i0,j0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1806) = pv(1806)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1810) = pv(1810)+d2(i0,k0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1811) = pv(1811)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1812) = pv(1812)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1813) = pv(1813)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1814) = pv(1814)+d(i0,j0)*d2(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1816) = pv(1816)+d(i0,k0)*d2(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1822) = pv(1822)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)*d(j0,l0)/mu
 pv(1825) = pv(1825)+d(i0,j0)*d(i0,k0)*d2(k0,k1)*d(j0,l0)/mu
 pv(1826) = pv(1826)+d(i0,j0)*d(j0,k0)*d2(k0,k1)*d(j0,l0)/mu
 pv(1855) = pv(1855)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1856) = pv(1856)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)*d(j0,l0)/mu
 pv(1862) = pv(1862)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)*d(j0,l0)/mu
 pv(1863) = pv(1863)+d(i0,j0)*d(i0,k0)*d(k0,k1)*d(i0,l0)*d(j0,l0)/mu
 pv(1864) = pv(1864)+d(i0,j0)*d(j0,k0)*d(k0,k1)*d(i0,l0)*d(j0,l0)/mu
 pv(1865) = pv(1865)+d(i0,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)*d(j0,l0)/mu
 pv(1882) = pv(1882)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d2(j0,l0)/mu
 pv(1888) = pv(1888)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d2(j0,l0)/mu
 pv(1895) = pv(1895)+d(i0,j0)*d(i0,k0)*d(k0,k1)*d2(j0,l0)/mu
 pv(1896) = pv(1896)+d(i0,k0)*d(j0,k0)*d(k0,k1)*d2(j0,l0)/mu
 pv(1936) = pv(1936)+d2(i0,j0)*d(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1940) = pv(1940)+d(i0,j0)*d2(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1989) = pv(1989)+d2(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1994) = pv(1994)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1998) = pv(1998)+d2(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(2000) = pv(2000)+d(i0,j0)*d2(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(2001) = pv(2001)+d(i0,k0)*d2(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(2003) = pv(2003)+d(i0,j0)*d(i0,k1)*d(i0,k2)*d(j0,k0)*d(k0,l0)/mu
 pv(2016) = pv(2016)+d(i0,j0)*d(i0,k1)*d2(j0,k0)*d(k0,l0)/mu
 pv(2017) = pv(2017)+d(i0,k0)*d(i0,k1)*d2(j0,k0)*d(k0,l0)/mu
 pv(2034) = pv(2034)+d2(i0,j0)*d(i0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2038) = pv(2038)+d(i0,j0)*d2(i0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2040) = pv(2040)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k1)*d(k0,l0)/mu
 pv(2041) = pv(2041)+d2(i0,k0)*d(i0,k1)*d(j0,k1)*d(k0,l0)/mu
 pv(2043) = pv(2043)+d2(i0,j0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2046) = pv(2046)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2050) = pv(2050)+d2(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2051) = pv(2051)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2052) = pv(2052)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2053) = pv(2053)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2054) = pv(2054)+d(i0,j0)*d2(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2056) = pv(2056)+d(i0,k0)*d2(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2057) = pv(2057)+d(i0,k1)*d2(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2064) = pv(2064)+d(i0,j0)*d(i0,k0)*d2(j0,k1)*d(k0,l0)/mu
 pv(2065) = pv(2065)+d(i0,k0)*d(j0,k0)*d2(j0,k1)*d(k0,l0)/mu
 pv(2071) = pv(2071)+d2(i0,j0)*d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2076) = pv(2076)+d(i0,j0)*d2(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2079) = pv(2079)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2084) = pv(2084)+d2(i0,j0)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2089) = pv(2089)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2090) = pv(2090)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2091) = pv(2091)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2092) = pv(2092)+d(i0,j0)*d2(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2095) = pv(2095)+d(i0,j0)*d(i0,k0)*d(j0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2096) = pv(2096)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2124) = pv(2124)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2125) = pv(2125)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)*d(k0,l0)/mu
 pv(2129) = pv(2129)+d(i0,j0)*d(i0,k0)*d(j0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2130) = pv(2130)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2131) = pv(2131)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2133) = pv(2133)+d(i0,j0)*d(i0,k0)*d(k0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2136) = pv(2136)+d(i0,j0)*d(j0,k0)*d(k0,k1)*d(i0,l0)*d(k0,l0)/mu
 pv(2149) = pv(2149)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2159) = pv(2159)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2160) = pv(2160)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)*d(k0,l0)/mu
 pv(2169) = pv(2169)+d(i0,j0)*d(i0,k0)*d(j0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2170) = pv(2170)+d(i0,k0)*d(j0,k0)*d(j0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2173) = pv(2173)+d(i0,j0)*d(i0,k0)*d(k0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2174) = pv(2174)+d(i0,j0)*d(j0,k0)*d(k0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2196) = pv(2196)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d2(k0,l0)/mu
 pv(2206) = pv(2206)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d2(k0,l0)/mu
 pv(2210) = pv(2210)+d(i0,j0)*d(i0,k0)*d(j0,k1)*d2(k0,l0)/mu
 pv(2211) = pv(2211)+d(i0,j0)*d(j0,k0)*d(j0,k1)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_i0j0kk
SUBROUTINE mg3331_iij0kk (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(198) = pv(198)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(204) = pv(204)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(254) = pv(254)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(256) = pv(256)+d(i0,j0)*d(i1,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(259) = pv(259)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(261) = pv(261)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(265) = pv(265)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(266) = pv(266)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(269) = pv(269)+d(i0,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(310) = pv(310)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j0,k1)/mu
 pv(313) = pv(313)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(315) = pv(315)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(319) = pv(319)+d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(336) = pv(336)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(338) = pv(338)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(345) = pv(345)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(352) = pv(352)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(354) = pv(354)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(358) = pv(358)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(360) = pv(360)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
endif
if (5.le.mxd) then
 pv(714) = pv(714)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(715) = pv(715)+d(i0,j0)*d2(i1,j0)*d(i0,k0)*d(i0,k1)/mu
 pv(720) = pv(720)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(i0,k1)/mu
 pv(725) = pv(725)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(726) = pv(726)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(729) = pv(729)+d(i0,j0)*d2(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(732) = pv(732)+d(i0,j0)*d(i0,k0)*d2(i1,k0)*d(i0,k1)/mu
 pv(869) = pv(869)+d(i0,i1)*d2(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(871) = pv(871)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(872) = pv(872)+d2(i0,j0)*d(i1,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(878) = pv(878)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(880) = pv(880)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(881) = pv(881)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(882) = pv(882)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(883) = pv(883)+d2(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(884) = pv(884)+d(i1,j0)*d(i2,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(896) = pv(896)+d(i0,i1)*d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(898) = pv(898)+d(i1,j0)*d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(902) = pv(902)+d(i0,i1)*d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(903) = pv(903)+d2(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(904) = pv(904)+d(i0,j0)*d(i1,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(907) = pv(907)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(908) = pv(908)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(909) = pv(909)+d(i1,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(910) = pv(910)+d(i2,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(913) = pv(913)+d2(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(914) = pv(914)+d(i0,j0)*d2(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(915) = pv(915)+d(i0,i1)*d(i0,j0)*d2(i0,k1)*d(j0,k0)/mu
 pv(917) = pv(917)+d(i0,j0)*d(i1,j0)*d2(i0,k1)*d(j0,k0)/mu
 pv(921) = pv(921)+d(i1,j0)*d(i0,k0)*d2(i0,k1)*d(j0,k0)/mu
 pv(924) = pv(924)+d(i0,j0)*d(i1,k0)*d2(i0,k1)*d(j0,k0)/mu
 pv(925) = pv(925)+d(i0,k0)*d(i1,k0)*d2(i0,k1)*d(j0,k0)/mu
 pv(928) = pv(928)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(929) = pv(929)+d2(i0,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(930) = pv(930)+d(i0,j0)*d(i1,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(933) = pv(933)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(934) = pv(934)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(935) = pv(935)+d(i0,j0)*d2(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(937) = pv(937)+d(i0,j0)*d(i1,j0)*d(i0,k1)*d(i0,k2)*d(j0,k0)/mu
 pv(940) = pv(940)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(i0,k2)*d(j0,k0)/mu
 pv(986) = pv(986)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d2(j0,k0)/mu
 pv(988) = pv(988)+d(i0,j0)*d(i1,j0)*d(i0,k1)*d2(j0,k0)/mu
 pv(991) = pv(991)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(993) = pv(993)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(997) = pv(997)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(998) = pv(998)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(1001) = pv(1001)+d(i0,j0)*d(i0,k1)*d(i1,k1)*d2(j0,k0)/mu
 pv(1110) = pv(1110)+d2(i0,i1)*d(i0,j0)*d(j0,k0)*d(j0,k1)/mu
 pv(1111) = pv(1111)+d(i0,i1)*d2(i0,j0)*d(j0,k0)*d(j0,k1)/mu
 pv(1116) = pv(1116)+d2(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1117) = pv(1117)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1119) = pv(1119)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1120) = pv(1120)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1121) = pv(1121)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1122) = pv(1122)+d2(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1134) = pv(1134)+d(i0,i1)*d2(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1136) = pv(1136)+d(i1,j0)*d2(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1140) = pv(1140)+d(i0,i1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1141) = pv(1141)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1142) = pv(1142)+d(i2,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1145) = pv(1145)+d2(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1146) = pv(1146)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(1148) = pv(1148)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(1152) = pv(1152)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(1153) = pv(1153)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(1154) = pv(1154)+d(i0,i1)*d(i0,j0)*d(i0,k2)*d(j0,k0)*d(j0,k1)/mu
 pv(1159) = pv(1159)+d(i0,j0)*d(i1,k0)*d(i0,k2)*d(j0,k0)*d(j0,k1)/mu
 pv(1161) = pv(1161)+d(i0,i1)*d(i0,j0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1164) = pv(1164)+d(i0,i1)*d(i0,k0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1166) = pv(1166)+d(i1,j0)*d(i0,k0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1170) = pv(1170)+d(i0,k0)*d(i1,k0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1171) = pv(1171)+d(i0,i1)*d(i0,k1)*d2(j0,k0)*d(j0,k1)/mu
 pv(1175) = pv(1175)+d(i1,k0)*d(i0,k1)*d2(j0,k0)*d(j0,k1)/mu
 pv(1219) = pv(1219)+d(i0,i1)*d2(i0,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(1221) = pv(1221)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(1222) = pv(1222)+d2(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(1223) = pv(1223)+d(i0,j0)*d2(i1,j0)*d(i0,k0)*d(k0,k1)/mu
 pv(1232) = pv(1232)+d(i0,i1)*d(i0,j0)*d2(i0,k0)*d(k0,k1)/mu
 pv(1234) = pv(1234)+d(i0,j0)*d(i1,j0)*d2(i0,k0)*d(k0,k1)/mu
 pv(1241) = pv(1241)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1242) = pv(1242)+d2(i0,j0)*d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1243) = pv(1243)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1247) = pv(1247)+d(i0,j0)*d2(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1250) = pv(1250)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1256) = pv(1256)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1261) = pv(1261)+d2(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(1262) = pv(1262)+d(i0,i1)*d2(i0,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(1264) = pv(1264)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(1265) = pv(1265)+d2(i0,j0)*d(i1,j0)*d(j0,k0)*d(k0,k1)/mu
 pv(1273) = pv(1273)+d2(i0,i1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1274) = pv(1274)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1276) = pv(1276)+d(i0,i1)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1277) = pv(1277)+d(i0,i2)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1278) = pv(1278)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1279) = pv(1279)+d2(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1280) = pv(1280)+d(i1,j0)*d(i2,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1294) = pv(1294)+d(i0,i1)*d2(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1296) = pv(1296)+d(i1,j0)*d2(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1300) = pv(1300)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1301) = pv(1301)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1303) = pv(1303)+d(i0,j0)*d(i1,j0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1306) = pv(1306)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1308) = pv(1308)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1312) = pv(1312)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1315) = pv(1315)+d(i0,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1316) = pv(1316)+d(i0,i1)*d(i0,j0)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1318) = pv(1318)+d(i0,j0)*d(i1,j0)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1323) = pv(1323)+d(i0,j0)*d(i1,k0)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1326) = pv(1326)+d(i0,i1)*d(i0,j0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1328) = pv(1328)+d(i0,j0)*d(i1,j0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1332) = pv(1332)+d(i0,i1)*d(i0,k0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1334) = pv(1334)+d(i1,j0)*d(i0,k0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1357) = pv(1357)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1360) = pv(1360)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1362) = pv(1362)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1383) = pv(1383)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d2(k0,k1)/mu
 pv(1385) = pv(1385)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d2(k0,k1)/mu
 pv(1395) = pv(1395)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d2(k0,k1)/mu
 pv(1397) = pv(1397)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d2(k0,k1)/mu
 pv(1506) = pv(1506)+d(i0,j0)*d(i1,j0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1509) = pv(1509)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1512) = pv(1512)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1513) = pv(1513)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1515) = pv(1515)+d(i1,j0)*d(i0,k0)*d(i1,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1516) = pv(1516)+d(i0,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1551) = pv(1551)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1554) = pv(1554)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1556) = pv(1556)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1560) = pv(1560)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1561) = pv(1561)+d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1571) = pv(1571)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1577) = pv(1577)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1581) = pv(1581)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1583) = pv(1583)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1586) = pv(1586)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1588) = pv(1588)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1592) = pv(1592)+d(i0,j0)*d(i1,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1678) = pv(1678)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,l0)/mu
 pv(1681) = pv(1681)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,l0)/mu
 pv(1718) = pv(1718)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1721) = pv(1721)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1723) = pv(1723)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1727) = pv(1727)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1728) = pv(1728)+d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1731) = pv(1731)+d(i0,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1781) = pv(1781)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1785) = pv(1785)+d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1794) = pv(1794)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1796) = pv(1796)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1800) = pv(1800)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1802) = pv(1802)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1805) = pv(1805)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1807) = pv(1807)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1937) = pv(1937)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1943) = pv(1943)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1988) = pv(1988)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1990) = pv(1990)+d(i0,j0)*d(i1,j0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1993) = pv(1993)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1995) = pv(1995)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1999) = pv(1999)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(2002) = pv(2002)+d(i0,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(2033) = pv(2033)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2035) = pv(2035)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2039) = pv(2039)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2042) = pv(2042)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2045) = pv(2045)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2047) = pv(2047)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2070) = pv(2070)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2072) = pv(2072)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2083) = pv(2083)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2085) = pv(2085)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
endif
END SUBROUTINE mg3331_iij0kk
SUBROUTINE mg3331_jjkk (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(68) = pv(68)+d(j0,j1)*d(j0,k0)*d(j0,k1)/mu
 pv(71) = pv(71)+d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(78) = pv(78)+d(j0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(81) = pv(81)+d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
endif
if (4.le.mxd) then
 pv(323) = pv(323)+d(j0,j1)*d2(j0,k0)*d(j0,k1)/mu
 pv(329) = pv(329)+d(j0,j1)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(331) = pv(331)+d2(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(332) = pv(332)+d(j0,k0)*d2(j1,k0)*d(j0,k1)/mu
 pv(334) = pv(334)+d(j0,k0)*d(j1,k0)*d2(j0,k1)/mu
 pv(357) = pv(357)+d2(j0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(368) = pv(368)+d(j0,j1)*d2(j0,k0)*d(k0,k1)/mu
 pv(372) = pv(372)+d(j0,j1)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(373) = pv(373)+d2(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(375) = pv(375)+d(j0,j1)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(378) = pv(378)+d(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(385) = pv(385)+d(j0,j1)*d(j0,k0)*d2(k0,k1)/mu
 pv(493) = pv(493)+d(j0,k0)*d(j1,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(496) = pv(496)+d(j0,j1)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(499) = pv(499)+d(j0,k0)*d(j1,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(560) = pv(560)+d(j0,j1)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(563) = pv(563)+d(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(570) = pv(570)+d(j0,j1)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
endif
if (5.le.mxd) then
 pv(1180) = pv(1180)+d(j0,j1)*d3(j0,k0)*d(j0,k1)/mu
 pv(1207) = pv(1207)+d(j0,j1)*d2(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1209) = pv(1209)+d3(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1212) = pv(1212)+d(j0,j1)*d(j0,k0)*d2(j1,k0)*d(j0,k1)/mu
 pv(1213) = pv(1213)+d2(j0,k0)*d2(j1,k0)*d(j0,k1)/mu
 pv(1214) = pv(1214)+d(j0,k0)*d3(j1,k0)*d(j0,k1)/mu
 pv(1218) = pv(1218)+d2(j0,k0)*d(j1,k0)*d2(j0,k1)/mu
 pv(1331) = pv(1331)+d2(j0,j1)*d2(j0,k0)*d(k0,k1)/mu
 pv(1342) = pv(1342)+d(j0,j1)*d3(j0,k0)*d(k0,k1)/mu
 pv(1350) = pv(1350)+d2(j0,j1)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1355) = pv(1355)+d(j0,j1)*d2(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1356) = pv(1356)+d3(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1369) = pv(1369)+d(j0,j1)*d2(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1375) = pv(1375)+d(j0,j1)*d(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1376) = pv(1376)+d2(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1378) = pv(1378)+d(j0,k0)*d(j1,k0)*d2(j0,k1)*d(k0,k1)/mu
 pv(1400) = pv(1400)+d2(j0,j1)*d(j0,k0)*d2(k0,k1)/mu
 pv(1405) = pv(1405)+d(j0,j1)*d2(j0,k0)*d2(k0,k1)/mu
 pv(1408) = pv(1408)+d(j0,j1)*d(j0,k0)*d(j0,k1)*d2(k0,k1)/mu
 pv(1792) = pv(1792)+d2(j0,k0)*d(j1,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1793) = pv(1793)+d(j0,k0)*d2(j1,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1815) = pv(1815)+d(j0,j1)*d2(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1820) = pv(1820)+d(j0,j1)*d(j0,k0)*d(j1,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1821) = pv(1821)+d2(j0,k0)*d(j1,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1824) = pv(1824)+d(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,k1)*d(j0,l0)/mu
 pv(1827) = pv(1827)+d(j0,j1)*d(j0,k0)*d2(k0,k1)*d(j0,l0)/mu
 pv(1898) = pv(1898)+d(j0,k0)*d(j1,k0)*d(k0,k1)*d2(j0,l0)/mu
 pv(2055) = pv(2055)+d(j0,j1)*d2(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2062) = pv(2062)+d(j0,j1)*d(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2063) = pv(2063)+d2(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2067) = pv(2067)+d(j0,k0)*d(j1,k0)*d2(j0,k1)*d(k0,l0)/mu
 pv(2088) = pv(2088)+d2(j0,j1)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2093) = pv(2093)+d(j0,j1)*d2(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2097) = pv(2097)+d(j0,j1)*d(j0,k0)*d(j0,k1)*d(k0,k1)*d(k0,l0)/mu
 pv(2172) = pv(2172)+d(j0,k0)*d(j1,k0)*d(j0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2175) = pv(2175)+d(j0,j1)*d(j0,k0)*d(k0,k1)*d(j0,l0)*d(k0,l0)/mu
 pv(2212) = pv(2212)+d(j0,j1)*d(j0,k0)*d(j0,k1)*d2(k0,l0)/mu
endif
END SUBROUTINE mg3331_jjkk
SUBROUTINE mg3331_i0jjkk (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(199) = pv(199)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,k1)/mu
 pv(257) = pv(257)+d(i0,j0)*d(i0,j1)*d(i0,k1)*d(j0,k0)/mu
 pv(258) = pv(258)+d(i0,j0)*d(j0,j1)*d(i0,k1)*d(j0,k0)/mu
 pv(262) = pv(262)+d(i0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(263) = pv(263)+d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(301) = pv(301)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(302) = pv(302)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(312) = pv(312)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,k1)/mu
 pv(316) = pv(316)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(317) = pv(317)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(327) = pv(327)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(328) = pv(328)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(330) = pv(330)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(339) = pv(339)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(340) = pv(340)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(355) = pv(355)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(356) = pv(356)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(361) = pv(361)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(362) = pv(362)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(371) = pv(371)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
endif
if (5.le.mxd) then
 pv(716) = pv(716)+d2(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,k1)/mu
 pv(718) = pv(718)+d(i0,j0)*d2(j0,j1)*d(i0,k0)*d(i0,k1)/mu
 pv(721) = pv(721)+d(i0,j0)*d(j0,j1)*d2(i0,k0)*d(i0,k1)/mu
 pv(873) = pv(873)+d2(i0,j0)*d(i0,j1)*d(i0,k1)*d(j0,k0)/mu
 pv(876) = pv(876)+d2(i0,j0)*d(j0,j1)*d(i0,k1)*d(j0,k0)/mu
 pv(877) = pv(877)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,k1)*d(j0,k0)/mu
 pv(885) = pv(885)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(887) = pv(887)+d2(i0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(891) = pv(891)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(893) = pv(893)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(894) = pv(894)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(895) = pv(895)+d2(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(899) = pv(899)+d(i0,j1)*d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(900) = pv(900)+d(j0,j1)*d2(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(918) = pv(918)+d(i0,j0)*d(i0,j1)*d2(i0,k1)*d(j0,k0)/mu
 pv(919) = pv(919)+d(i0,j0)*d(j0,j1)*d2(i0,k1)*d(j0,k0)/mu
 pv(922) = pv(922)+d(j0,j1)*d(i0,k0)*d2(i0,k1)*d(j0,k0)/mu
 pv(938) = pv(938)+d(i0,j0)*d(j0,j1)*d(i0,k1)*d(i0,k2)*d(j0,k0)/mu
 pv(989) = pv(989)+d(i0,j0)*d(i0,j1)*d(i0,k1)*d2(j0,k0)/mu
 pv(990) = pv(990)+d(i0,j0)*d(j0,j1)*d(i0,k1)*d2(j0,k0)/mu
 pv(994) = pv(994)+d(i0,j1)*d(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(995) = pv(995)+d(j0,j1)*d(i0,k0)*d(i0,k1)*d2(j0,k0)/mu
 pv(1063) = pv(1063)+d2(i0,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1065) = pv(1065)+d(i0,j0)*d(i0,j1)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1066) = pv(1066)+d(i0,j0)*d(j0,j1)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1068) = pv(1068)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1070) = pv(1070)+d(i0,j2)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1071) = pv(1071)+d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1072) = pv(1072)+d2(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1074) = pv(1074)+d(i0,j0)*d2(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1075) = pv(1075)+d(i0,k0)*d2(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1077) = pv(1077)+d(i0,j0)*d(i0,k1)*d(i0,k2)*d(j0,k0)*d(j1,k0)/mu
 pv(1098) = pv(1098)+d(i0,j0)*d(i0,k1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1099) = pv(1099)+d(i0,j1)*d(i0,k1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1100) = pv(1100)+d(i0,k0)*d(i0,k1)*d2(j0,k0)*d(j1,k0)/mu
 pv(1114) = pv(1114)+d2(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,k1)/mu
 pv(1115) = pv(1115)+d(i0,j0)*d2(i0,j1)*d(j0,k0)*d(j0,k1)/mu
 pv(1124) = pv(1124)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1126) = pv(1126)+d2(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1129) = pv(1129)+d(i0,j1)*d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1131) = pv(1131)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1132) = pv(1132)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1133) = pv(1133)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1137) = pv(1137)+d(i0,j1)*d2(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1138) = pv(1138)+d(j0,j1)*d2(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1149) = pv(1149)+d(i0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(1150) = pv(1150)+d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,k1)/mu
 pv(1156) = pv(1156)+d(i0,j0)*d(i0,j1)*d(i0,k2)*d(j0,k0)*d(j0,k1)/mu
 pv(1163) = pv(1163)+d(i0,j0)*d(i0,j1)*d2(j0,k0)*d(j0,k1)/mu
 pv(1167) = pv(1167)+d(i0,j1)*d(i0,k0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1168) = pv(1168)+d(j0,j1)*d(i0,k0)*d2(j0,k0)*d(j0,k1)/mu
 pv(1173) = pv(1173)+d(i0,j1)*d(i0,k1)*d2(j0,k0)*d(j0,k1)/mu
 pv(1185) = pv(1185)+d2(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1187) = pv(1187)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1188) = pv(1188)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1189) = pv(1189)+d(i0,j0)*d2(i0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1191) = pv(1191)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j1,k0)*d(j0,k1)/mu
 pv(1193) = pv(1193)+d2(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1194) = pv(1194)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1196) = pv(1196)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1198) = pv(1198)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1199) = pv(1199)+d(i0,j2)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1200) = pv(1200)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1201) = pv(1201)+d2(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1202) = pv(1202)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1203) = pv(1203)+d(i0,j1)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1204) = pv(1204)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1205) = pv(1205)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1206) = pv(1206)+d(i0,j0)*d2(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1208) = pv(1208)+d(i0,k0)*d2(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1210) = pv(1210)+d(i0,j0)*d(i0,k0)*d2(j1,k0)*d(j0,k1)/mu
 pv(1211) = pv(1211)+d(i0,j0)*d(j0,k0)*d2(j1,k0)*d(j0,k1)/mu
 pv(1217) = pv(1217)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d2(j0,k1)/mu
 pv(1224) = pv(1224)+d2(i0,j0)*d(i0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(1227) = pv(1227)+d2(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(1229) = pv(1229)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(1230) = pv(1230)+d(i0,j0)*d2(j0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(1235) = pv(1235)+d(i0,j0)*d(i0,j1)*d2(i0,k0)*d(k0,k1)/mu
 pv(1236) = pv(1236)+d(i0,j0)*d(j0,j1)*d2(i0,k0)*d(k0,k1)/mu
 pv(1251) = pv(1251)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,k1)*d(k0,k1)/mu
 pv(1267) = pv(1267)+d2(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(1269) = pv(1269)+d(i0,j0)*d2(i0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(1271) = pv(1271)+d2(i0,j0)*d(j0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(1272) = pv(1272)+d(i0,j0)*d(i0,j1)*d(j0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(1282) = pv(1282)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1284) = pv(1284)+d2(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1287) = pv(1287)+d(i0,j1)*d(i0,j2)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1289) = pv(1289)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1291) = pv(1291)+d(i0,j1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1292) = pv(1292)+d(i0,j2)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1293) = pv(1293)+d2(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1297) = pv(1297)+d(i0,j1)*d2(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1298) = pv(1298)+d(j0,j1)*d2(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1304) = pv(1304)+d(i0,j0)*d(i0,j1)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1305) = pv(1305)+d(i0,j0)*d(j0,j1)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1309) = pv(1309)+d(i0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1310) = pv(1310)+d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,k1)/mu
 pv(1319) = pv(1319)+d(i0,j0)*d(i0,j1)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1320) = pv(1320)+d(i0,j0)*d(j0,j1)*d(i0,k2)*d(j0,k0)*d(k0,k1)/mu
 pv(1329) = pv(1329)+d(i0,j0)*d(i0,j1)*d2(j0,k0)*d(k0,k1)/mu
 pv(1330) = pv(1330)+d(i0,j0)*d(j0,j1)*d2(j0,k0)*d(k0,k1)/mu
 pv(1335) = pv(1335)+d(i0,j1)*d(i0,k0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1336) = pv(1336)+d(j0,j1)*d(i0,k0)*d2(j0,k0)*d(k0,k1)/mu
 pv(1346) = pv(1346)+d2(i0,j0)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1348) = pv(1348)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1349) = pv(1349)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1351) = pv(1351)+d(i0,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1352) = pv(1352)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1353) = pv(1353)+d(i0,j0)*d(i0,k2)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1354) = pv(1354)+d(i0,j0)*d2(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1359) = pv(1359)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1363) = pv(1363)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1364) = pv(1364)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1373) = pv(1373)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1374) = pv(1374)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,k1)/mu
 pv(1386) = pv(1386)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d2(k0,k1)/mu
 pv(1387) = pv(1387)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d2(k0,k1)/mu
 pv(1398) = pv(1398)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d2(k0,k1)/mu
 pv(1399) = pv(1399)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d2(k0,k1)/mu
 pv(1507) = pv(1507)+d(i0,j0)*d(j0,j1)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1510) = pv(1510)+d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(i0,l0)/mu
 pv(1546) = pv(1546)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1547) = pv(1547)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(i0,l0)/mu
 pv(1553) = pv(1553)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1557) = pv(1557)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1558) = pv(1558)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1567) = pv(1567)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1568) = pv(1568)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1569) = pv(1569)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)*d(i0,l0)/mu
 pv(1572) = pv(1572)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1584) = pv(1584)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1585) = pv(1585)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1589) = pv(1589)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1590) = pv(1590)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1598) = pv(1598)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(k0,k1)*d(i0,l0)/mu
 pv(1679) = pv(1679)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,l0)/mu
 pv(1720) = pv(1720)+d(i0,j0)*d(i0,j1)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1724) = pv(1724)+d(i0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1725) = pv(1725)+d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j0,l0)/mu
 pv(1757) = pv(1757)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j1,k0)*d(j0,l0)/mu
 pv(1768) = pv(1768)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1769) = pv(1769)+d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(j0,l0)/mu
 pv(1783) = pv(1783)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1789) = pv(1789)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1790) = pv(1790)+d(i0,j1)*d(i0,k1)*d(j1,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1791) = pv(1791)+d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)*d(j0,l0)/mu
 pv(1797) = pv(1797)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1798) = pv(1798)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1804) = pv(1804)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1808) = pv(1808)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1809) = pv(1809)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1818) = pv(1818)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1819) = pv(1819)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(k0,k1)*d(j0,l0)/mu
 pv(1938) = pv(1938)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i0,k1)*d(k0,l0)/mu
 pv(1991) = pv(1991)+d(i0,j0)*d(i0,j1)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1992) = pv(1992)+d(i0,j0)*d(j0,j1)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1996) = pv(1996)+d(i0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(1997) = pv(1997)+d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(k0,l0)/mu
 pv(2029) = pv(2029)+d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)*d(k0,l0)/mu
 pv(2036) = pv(2036)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2037) = pv(2037)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2044) = pv(2044)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2048) = pv(2048)+d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2049) = pv(2049)+d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2059) = pv(2059)+d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2060) = pv(2060)+d(i0,j1)*d(i0,k1)*d(j1,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2061) = pv(2061)+d(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,k1)*d(k0,l0)/mu
 pv(2073) = pv(2073)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2074) = pv(2074)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2086) = pv(2086)+d(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
 pv(2087) = pv(2087)+d(i0,j0)*d(j0,j1)*d(j0,k0)*d(k0,k1)*d(k0,l0)/mu
endif
END SUBROUTINE mg3331_i0jjkk
SUBROUTINE mg3331_iijjkk (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
 pv(717) = pv(717)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)*d(i0,k1)/mu
 pv(727) = pv(727)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(i0,k1)/mu
 pv(874) = pv(874)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k1)*d(j0,k0)/mu
 pv(875) = pv(875)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,k1)*d(j0,k0)/mu
 pv(886) = pv(886)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(888) = pv(888)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(889) = pv(889)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(890) = pv(890)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(892) = pv(892)+d(i1,j0)*d(j0,j1)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(905) = pv(905)+d(i0,j0)*d(i0,j1)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(906) = pv(906)+d(i0,j0)*d(j0,j1)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(911) = pv(911)+d(i0,j1)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(912) = pv(912)+d(j0,j1)*d(i0,k0)*d(i1,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(931) = pv(931)+d(i0,j0)*d(i0,j1)*d(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(932) = pv(932)+d(i0,j0)*d(j0,j1)*d(i0,k1)*d(i1,k1)*d(j0,k0)/mu
 pv(1062) = pv(1062)+d(i0,i1)*d(i0,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1064) = pv(1064)+d(i0,j0)*d(i1,j0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1067) = pv(1067)+d(i0,i1)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1069) = pv(1069)+d(i1,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1073) = pv(1073)+d(i0,j0)*d(i1,k0)*d(i0,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1076) = pv(1076)+d(i0,j0)*d(i0,k1)*d(i1,k1)*d(j0,k0)*d(j1,k0)/mu
 pv(1113) = pv(1113)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,k0)*d(j0,k1)/mu
 pv(1123) = pv(1123)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1125) = pv(1125)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1127) = pv(1127)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1128) = pv(1128)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1130) = pv(1130)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1143) = pv(1143)+d(i0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1144) = pv(1144)+d(j0,j1)*d(i0,k0)*d(i1,k0)*d(j0,k0)*d(j0,k1)/mu
 pv(1184) = pv(1184)+d(i0,i1)*d(i0,j0)*d(i0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1186) = pv(1186)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1190) = pv(1190)+d(i0,j0)*d(i0,k0)*d(i1,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1192) = pv(1192)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1195) = pv(1195)+d(i0,i1)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1197) = pv(1197)+d(i1,j0)*d(i0,k0)*d(j0,k0)*d(j1,k0)*d(j0,k1)/mu
 pv(1225) = pv(1225)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(1226) = pv(1226)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(1228) = pv(1228)+d(i0,j0)*d(i1,j0)*d(j0,j1)*d(i0,k0)*d(k0,k1)/mu
 pv(1244) = pv(1244)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1245) = pv(1245)+d(i0,j0)*d(j0,j1)*d(i0,k0)*d(i1,k0)*d(k0,k1)/mu
 pv(1266) = pv(1266)+d(i0,i1)*d(i0,j0)*d(i0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(1268) = pv(1268)+d(i0,j0)*d(i1,j0)*d(i0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(1270) = pv(1270)+d(i0,i1)*d(i0,j0)*d(j0,j1)*d(j0,k0)*d(k0,k1)/mu
 pv(1281) = pv(1281)+d(i0,i1)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1283) = pv(1283)+d(i1,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1285) = pv(1285)+d(i1,j0)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1286) = pv(1286)+d(i0,j1)*d(i1,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1288) = pv(1288)+d(i0,i1)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1290) = pv(1290)+d(i1,j0)*d(j0,j1)*d(i0,k0)*d(j0,k0)*d(k0,k1)/mu
 pv(1345) = pv(1345)+d(i0,i1)*d(i0,j0)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
 pv(1347) = pv(1347)+d(i0,j0)*d(i1,j0)*d(j0,k0)*d(j1,k0)*d(k0,k1)/mu
endif
END SUBROUTINE mg3331_iijjkk
END SUBROUTINE mg3331_isecs
