SUBROUTINE mg63_msecs ()
!-----------------------------------------------------------------------
integer, parameter :: m=6, n=3, m2=m*(m-1), m3=m2*(m-2), m4=m3*(m-3), &
  mm=m4*(m-4), nn=n*(n-1)
integer :: i0, i1, i2, i3, i4, i5, j0, j1, j2, k,k1


k=0
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     do i3 = 0, m-1
      if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
       do i4 = 0, m-1
        if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
         mg63_mii(:,k)=(/i0,i1,i2,i3,i4/)
         k=k+1
        endif
       enddo
      endif
     enddo
    endif
   enddo
  endif
 enddo
enddo

k=0; k1=0
do j0 = m, m+n-1

 do i0 = 0, m-1
  do i1 = 0, m-1
   if (i1.ne.i0) then
    do i2 = 0, m-1
     if (i2.ne.i0.and.i2.ne.i1) then
      do i3 = 0, m-1
       if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then
        do i4 = 0, m-1
         if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
           mg63_miij0(:,k)=(/j0,i0,i1,i2,i3,i4/)
           k=k+1
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
      do i1 = 0, m-1
       if (i1.ne.i0) then
        do i2 = 0, m-1
         if (i2.ne.i0.and.i2.ne.i1) then
          do i3 = 0, m-1
           if (i3.ne.i0.and.i3.ne.i1.and.i3.ne.i2) then              
            do i4 = 0, m-1
             if (i4.ne.i0.and.i4.ne.i1.and.i4.ne.i2.and.i4.ne.i3) then
               mg63_miijj(:,k1)=(/j0,j1,j2,i0,i1,i2,i3,i4/)
               k1=k1+1
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
END SUBROUTINE mg63_msecs
