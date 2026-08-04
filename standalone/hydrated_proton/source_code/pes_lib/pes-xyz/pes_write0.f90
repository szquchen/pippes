SUBROUTINE pes_write0 (iun, fname, cf0)
integer, intent (in) :: iun
character (len=*), intent (in) :: fname
real (kind=wp), intent (in) :: cf0
!-----------------------------------------------------------------------
integer :: l0, l1
l0 = len_trim(pes_dir)
l1 = len_trim(pcf_suffix)
write (*,*) ' writing ', fname//pcf_suffix(1:l1)
open (iun, status='replace', &
  file=pes_dir(1:l0)//fname//pcf_suffix(1:l1))
write (iun,'(2x,1pg18.12,2x,a)') cf0, 'one-body'
close (iun)
return
END SUBROUTINE pes_write0
