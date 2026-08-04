SUBROUTINE mg82_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=7, n=2, m2=m*(m-1), m3=m2*(m-2), m4=m3*(m-3), &
  m5=m4*(m-4), m6=m5*(m-5), mm=m6*(m-6), nn=n
integer :: i0, i1, i2, i3, i4, i5, i6, i7, j0, j1
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg82_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg82_i2 (mxd, m3)
     do i3 = 0, m-1
      if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
       call mg82_i3 (mxd, m4)
       do i4 = 0, m-1
        if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
         call mg82_i4 (mxd, m5)
         do i5 = 0, m-1
          if (i5.ne.i0.and.i5.ne.i1.and.i5.ne.i2.and.i5.ne.i3.and. &
            i5.ne.i4) then
           call mg82_i5 (mxd, m6)
           do i6 = 0, m-1
            if (i6.ne.i0.and.i6.ne.i1.and.i6.ne.i2.and.i6.ne.i3.and. &
              i6.ne.i4.and.i6.ne.i5) then
             do i7 = 0, m-1
              if (i7.ne.i0.and.i7.ne.i1.and.i7.ne.i2.and.i7.ne.i3.and. &
                i7.ne.i4.and.i7.ne.i5.and.i7.ne.i6) then
               call mg82_ii (mxd, mm)
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
  endif
 enddo
enddo
do j0 = m, m+n-1
 do j1 = m, m+n-1
  if (j1.ne.j0) then
   do i0 = 0, m-1
    call mg82_i0jj (mxd, m*nn)
    do i1 = 0, m-1
     if (i1.ne.i0) then
      call mg82_i1jj (mxd, m2*nn)
      do i2 = 0, m-1
       if (i2.ne.i0.and.i2.ne.i1) then
        call mg82_i2jj (mxd, m3*nn)
        do i3 = 0, m-1
         if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
          call mg82_i3jj (mxd, m4*nn)
          do i4 = 0, m-1
           if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
            call mg82_i4jj (mxd, m5*nn)
            do i5 = 0, m-1
             if (i5.ne.i0.and.i5.ne.i1.and.i5.ne.i2.and.i5.ne.i3.and. &
               i5.ne.i4) then
              call mg82_i5jj (mxd, m6*nn)
              do i6 = 0, m-1
               if (i6.ne.i0.and.i6.ne.i1.and.i6.ne.i2.and.i6.ne.i3.and. &
                 i6.ne.i4.and.i6.ne.i5) then
                do i7 = 0, m-1
                 if (i7.ne.i0.and.i7.ne.i1.and.i7.ne.i2.and.i7.ne.i3.and. &
                   i7.ne.i4.and.i7.ne.i5.and.i7.ne.i6) then
                  call mg82_iijj (mxd, mm*nn)
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
     endif
    enddo
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg82_i2 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_i2
SUBROUTINE mg82_i3 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_i3
SUBROUTINE mg82_i4 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_i4
SUBROUTINE mg82_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_ii
SUBROUTINE mg82_i0jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_i0jj
SUBROUTINE mg82_i1jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
endif
if (3.le.mxd) then
endif
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_i1jj
SUBROUTINE mg82_i2jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
endif
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_i2jj
SUBROUTINE mg82_i3jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_i3jj
SUBROUTINE mg82_i4jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (5.le.mxd) then
endif
if (6.le.mxd) then
endif
END SUBROUTINE mg82_i4jj
SUBROUTINE mg82_iijj (mxd, mu)
integer, intent (in) :: mxd, mu
if (6.le.mxd) then
endif
END SUBROUTINE mg82_iijj
END SUBROUTINE mg82_isecs
