SUBROUTINE pes_u1_vread (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
open (iun, status='old', file=fn)
read (iun,*) pes_u1_vcf
close (iun)
return
END SUBROUTINE pes_u1_vread
