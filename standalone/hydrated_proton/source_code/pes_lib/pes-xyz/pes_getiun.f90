SUBROUTINE pes_getiun (iun)
! Obtain a free unit number
integer, intent (out) :: iun
!-----------------------------------------------------------------------
integer :: k
logical :: b
k = 20
inquire (unit=k, opened=b)
do while (b.and.k.lt.100)
 k = k+1
 inquire (unit=k, opened=b)
enddo
if (.not.b) then
 iun = k
else
 stop 'pes_getiun: no free unit'
endif
return
END SUBROUTINE pes_getiun
