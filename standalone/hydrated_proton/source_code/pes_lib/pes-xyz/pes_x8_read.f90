SUBROUTINE pes_x8_read (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_x8_pc
read (iun,*) nb
if (nb.ne.pes_x8_nb(pes_x8_pc%dg)) then
 stop 'pes_x8_read: dimension error'
endif
allocate (pes_x8_cf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_x8_cf
endif
close (iun)
return
END SUBROUTINE pes_x8_read
