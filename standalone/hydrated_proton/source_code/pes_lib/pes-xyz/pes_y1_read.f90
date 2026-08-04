SUBROUTINE pes_y1_read (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
open (iun, status='old', file=fn)
read (iun,*) pes_y1_cf
close (iun)
return
END SUBROUTINE pes_y1_read
