SUBROUTINE pes1_init_xyz (sys)
! Read pcf data files for XYZ systems.
character (len=*), intent (in) :: sys
!------------------------------------------------------------------------
integer :: iun, l0, l1
call pes_getiun (iun)
l0 = len_trim(pes_dir)
l1 = len_trim(sys)

if (cx_substr(sys(1:l1),'x6y3')) then
! write (*,*) ' reading pcf-x6y3.dat'
 call pes_x6y3_read (iun, pes_dir(1:l0)//'pcf-x6y3.dat')
else
 pes_x6y3_pc = cx_null ; allocate (pes_x6y3_cf(0:-1))
endif
return
END SUBROUTINE pes1_init_xyz
