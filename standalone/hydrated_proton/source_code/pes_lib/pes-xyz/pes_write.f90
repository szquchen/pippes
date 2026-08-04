SUBROUTINE pes_write (iun, fname, pc, cf)
integer, intent (in) :: iun
character (len=*), intent (in) :: fname
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: l0, l1
l0 = len_trim(pes_dir)
l1 = len_trim(pcf_suffix)
write (*,*) ' writing ', fname//pcf_suffix(1:l1)
open (iun, status='replace', &
  file=pes_dir(1:l0)//fname//pcf_suffix(1:l1))
call cx_write (iun, pc)
write (iun,'(i8,2x,a)') size(cf), 'coefficients'
write (iun,'(4es22.14)') cf
close (iun)
return
END SUBROUTINE pes_write
