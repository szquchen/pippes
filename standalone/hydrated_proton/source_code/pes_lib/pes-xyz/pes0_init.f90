SUBROUTINE pes0_init (dir, suffix)
! Initialize module data in pes0
character (len=*), intent (in), optional :: dir, suffix
!-----------------------------------------------------------------------
logical :: b0
if (present(dir)) then
 b0 = dir(len_trim(dir):len_trim(dir)).eq.'/'
 if (b0) then
  write (*,*) 'Principal data directory: ', dir(1:len_trim(dir))
  pes_dir = dir
 else
  write (*,*) 'Principal data directory: ', dir(1:len_trim(dir))//'/'
  pes_dir = trim(dir)//'/'
 endif
endif
if (present(suffix)) then
 write (*,*) 'pcf output suffix: ', suffix(1:len_trim(suffix))
 pcf_suffix = suffix
endif
return
END SUBROUTINE pes0_init
