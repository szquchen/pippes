SUBROUTINE pes_x4y2z1_read (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_x4y2z1_pc
read (iun,*) nb
if (nb.ne.pes_x4y2z1_nb(pes_x4y2z1_pc%dg)) then
 stop 'pes_x4y2z1_read: dimension error'
endif
allocate (pes_x4y2z1_cf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_x4y2z1_cf
endif
close (iun)
return
END SUBROUTINE pes_x4y2z1_read
