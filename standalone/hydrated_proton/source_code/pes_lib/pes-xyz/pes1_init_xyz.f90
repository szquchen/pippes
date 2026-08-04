SUBROUTINE pes1_init_xyz (sys)
! Read pcf data files for XYZ systems.
character (len=*), intent (in) :: sys
!------------------------------------------------------------------------
integer :: iun, l0, l1
call pes_getiun (iun)
l0 = len_trim(pes_dir)
l1 = len_trim(sys)
! Read pcf data for the one-body systems
if (cx_substr(sys(1:l1),'x1')) then
 write (*,*) ' reading pcf-x1.dat'
 call pes_x1_read (iun, pes_dir(1:l0)//'pcf-x1.dat')
else
 pes_x1_cf = 0
endif
if (cx_substr(sys(1:l1),'y1')) then
 write (*,*) ' reading pcf-y1.dat'
 call pes_y1_read (iun, pes_dir(1:l0)//'pcf-y1.dat')
else
 pes_y1_cf = 0
endif
if (cx_substr(sys(1:l1),'z1')) then
 write (*,*) ' reading pcf-z1.dat'
 call pes_z1_read (iun, pes_dir(1:l0)//'pcf-z1.dat')
else
 pes_z1_cf = 0
endif
if (cx_substr(sys(1:l1),'u1')) then
 write (*,*) ' reading pcf-u1.dat'
 call pes_u1_read (iun, pes_dir(1:l0)//'pcf-u1.dat')
else
 pes_u1_cf = 0
endif
! Read pcf data for the multi-body systems.  We've arranged the
! code according to grevlex order on number of X, Y, Z, U.
! Read pcf data for two-body systems.
if (cx_substr(sys(1:l1),'x2')) then
 write (*,*) ' reading pcf-x2.dat'
 call pes_x2_read (iun, pes_dir(1:l0)//'pcf-x2.dat')
else
 pes_x2_pc = cx_null ; allocate (pes_x2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1')) then
 write (*,*) ' reading pcf-x1y1.dat'
 call pes_x1y1_read (iun, pes_dir(1:l0)//'pcf-x1y1.dat')
else
 pes_x1y1_pc = cx_null ; allocate (pes_x1y1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2')) then
 write (*,*) ' reading pcf-y2.dat'
 call pes_y2_read (iun, pes_dir(1:l0)//'pcf-y2.dat')
else
 pes_y2_pc = cx_null ; allocate (pes_y2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1z1')) then
 write (*,*) ' reading pcf-x1z1.dat'
 call pes_x1z1_read (iun, pes_dir(1:l0)//'pcf-x1z1.dat')
else
 pes_x1z1_pc = cx_null ; allocate (pes_x1z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1z1')) then
 write (*,*) ' reading pcf-y1z1.dat'
 call pes_y1z1_read (iun, pes_dir(1:l0)//'pcf-y1z1.dat')
else
 pes_y1z1_pc = cx_null ; allocate (pes_y1z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'z2')) then
 write (*,*) ' reading pcf-z2.dat'
 call pes_z2_read (iun, pes_dir(1:l0)//'pcf-z2.dat')
else
 pes_z2_pc = cx_null ; allocate (pes_z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1u1')) then
 write (*,*) ' reading pcf-x1u1.dat'
 call pes_x1u1_read (iun, pes_dir(1:l0)//'pcf-x1u1.dat')
else
 pes_x1u1_pc = cx_null ; allocate (pes_x1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1u1')) then
 write (*,*) ' reading pcf-y1u1.dat'
 call pes_y1u1_read (iun, pes_dir(1:l0)//'pcf-y1u1.dat')
else
 pes_y1u1_pc = cx_null ; allocate (pes_y1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'z1u1')) then
 write (*,*) ' reading pcf-z1u1.dat'
 call pes_z1u1_read (iun, pes_dir(1:l0)//'pcf-z1u1.dat')
else
 pes_z1u1_pc = cx_null ; allocate (pes_z1u1_cf(0:-1))
endif
! Read pcf data for three-body systems.
if (cx_substr(sys(1:l1),'x3')) then
 write (*,*) ' reading pcf-x3.dat'
 call pes_x3_read (iun, pes_dir(1:l0)//'pcf-x3.dat')
else
 pes_x3_pc = cx_null ; allocate (pes_x3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1')) then
 write (*,*) ' reading pcf-x2y1.dat'
 call pes_x2y1_read (iun, pes_dir(1:l0)//'pcf-x2y1.dat')
else
 pes_x2y1_pc = cx_null ; allocate (pes_x2y1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2')) then
 write (*,*) ' reading pcf-x1y2.dat'
 call pes_x1y2_read (iun, pes_dir(1:l0)//'pcf-x1y2.dat')
else
 pes_x1y2_pc = cx_null ; allocate (pes_x1y2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y3')) then
 write (*,*) ' reading pcf-y3.dat'
 call pes_y3_read (iun, pes_dir(1:l0)//'pcf-y3.dat')
else
 pes_y3_pc = cx_null ; allocate (pes_y3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2z1')) then
 write (*,*) ' reading pcf-x2z1.dat'
 call pes_x2z1_read (iun, pes_dir(1:l0)//'pcf-x2z1.dat')
else
 pes_x2z1_pc = cx_null ; allocate (pes_x2z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1z1')) then
 write (*,*) ' reading pcf-x1y1z1.dat'
 call pes_x1y1z1_read (iun, pes_dir(1:l0)//'pcf-x1y1z1.dat')
else
 pes_x1y1z1_pc = cx_null ; allocate (pes_x1y1z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2z1')) then
 write (*,*) ' reading pcf-y2z1.dat'
 call pes_y2z1_read (iun, pes_dir(1:l0)//'pcf-y2z1.dat')
else
 pes_y2z1_pc = cx_null ; allocate (pes_y2z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1z2')) then
 write (*,*) ' reading pcf-x1z2.dat'
 call pes_x1z2_read (iun, pes_dir(1:l0)//'pcf-x1z2.dat')
else
 pes_x1z2_pc = cx_null ; allocate (pes_x1z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1z2')) then
 write (*,*) ' reading pcf-y1z2.dat'
 call pes_y1z2_read (iun, pes_dir(1:l0)//'pcf-y1z2.dat')
else
 pes_y1z2_pc = cx_null ; allocate (pes_y1z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'z3')) then
 write (*,*) ' reading pcf-z3.dat'
 call pes_z3_read (iun, pes_dir(1:l0)//'pcf-z3.dat')
else
 pes_z3_pc = cx_null ; allocate (pes_z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2u1')) then
 write (*,*) ' reading pcf-x2u1.dat'
 call pes_x2u1_read (iun, pes_dir(1:l0)//'pcf-x2u1.dat')
else
 pes_x2u1_pc = cx_null ; allocate (pes_x2u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1u1')) then
 write (*,*) ' reading pcf-x1y1u1.dat'
 call pes_x1y1u1_read (iun, pes_dir(1:l0)//'pcf-x1y1u1.dat')
else
 pes_x1y1u1_pc = cx_null ; allocate (pes_x1y1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2u1')) then
 write (*,*) ' reading pcf-y2u1.dat'
 call pes_y2u1_read (iun, pes_dir(1:l0)//'pcf-y2u1.dat')
else
 pes_y2u1_pc = cx_null ; allocate (pes_y2u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1z1u1')) then
 write (*,*) ' reading pcf-x1z1u1.dat'
 call pes_x1z1u1_read (iun, pes_dir(1:l0)//'pcf-x1z1u1.dat')
else
 pes_x1z1u1_pc = cx_null ; allocate (pes_x1z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1z1u1')) then
 write (*,*) ' reading pcf-y1z1u1.dat'
 call pes_y1z1u1_read (iun, pes_dir(1:l0)//'pcf-y1z1u1.dat')
else
 pes_y1z1u1_pc = cx_null ; allocate (pes_y1z1u1_cf(0:-1))
endif
! Read pcf data for four-body systems.
if (cx_substr(sys(1:l1),'x4')) then
 write (*,*) ' reading pcf-x4.dat'
 call pes_x4_read (iun, pes_dir(1:l0)//'pcf-x4.dat')
else
 pes_x4_pc = cx_null ; allocate (pes_x4_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1')) then
 write (*,*) ' reading pcf-x3y1.dat'
 call pes_x3y1_read (iun, pes_dir(1:l0)//'pcf-x3y1.dat')
else
 pes_x3y1_pc = cx_null ; allocate (pes_x3y1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2')) then
 write (*,*) ' reading pcf-x2y2.dat'
 call pes_x2y2_read (iun, pes_dir(1:l0)//'pcf-x2y2.dat')
else
 pes_x2y2_pc = cx_null ; allocate (pes_x2y2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y3')) then
 write (*,*) ' reading pcf-x1y3.dat'
 call pes_x1y3_read (iun, pes_dir(1:l0)//'pcf-x1y3.dat')
else
 pes_x1y3_pc = cx_null ; allocate (pes_x1y3_cf(0:-1))
endif
!! if (cx_substr(sys(1:l1),'y4')) then
!!  write (*,*) ' reading pcf-y4.dat'
!!  call pes_y4_read (iun, pes_dir(1:l0)//'pcf-y4.dat')
!! else
!!  pes_y4_pc = cx_null ; allocate (pes_y4_cf(0:-1))
!! endif
if (cx_substr(sys(1:l1),'x3z1')) then
 write (*,*) ' reading pcf-x3z1.dat'
 call pes_x3z1_read (iun, pes_dir(1:l0)//'pcf-x3z1.dat')
else
 pes_x3z1_pc = cx_null ; allocate (pes_x3z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1z1')) then
 write (*,*) ' reading pcf-x2y1z1.dat'
 call pes_x2y1z1_read (iun, pes_dir(1:l0)//'pcf-x2y1z1.dat')
else
 pes_x2y1z1_pc = cx_null ; allocate (pes_x2y1z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2z1')) then
 write (*,*) ' reading pcf-x1y2z1.dat'
 call pes_x1y2z1_read (iun, pes_dir(1:l0)//'pcf-x1y2z1.dat')
else
 pes_x1y2z1_pc = cx_null ; allocate (pes_x1y2z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y3z1')) then
 write (*,*) ' reading pcf-y3z1.dat'
 call pes_y3z1_read (iun, pes_dir(1:l0)//'pcf-y3z1.dat')
else
 pes_y3z1_pc = cx_null ; allocate (pes_y3z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2z2')) then
 write (*,*) ' reading pcf-x2z2.dat'
 call pes_x2z2_read (iun, pes_dir(1:l0)//'pcf-x2z2.dat')
else
 pes_x2z2_pc = cx_null ; allocate (pes_x2z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1z2')) then
 write (*,*) ' reading pcf-x1y1z2.dat'
 call pes_x1y1z2_read (iun, pes_dir(1:l0)//'pcf-x1y1z2.dat')
else
 pes_x1y1z2_pc = cx_null ; allocate (pes_x1y1z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2z2')) then
 write (*,*) ' reading pcf-y2z2.dat'
 call pes_y2z2_read (iun, pes_dir(1:l0)//'pcf-y2z2.dat')
else
 pes_y2z2_pc = cx_null ; allocate (pes_y2z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1z3')) then
 write (*,*) ' reading pcf-x1z3.dat'
 call pes_x1z3_read (iun, pes_dir(1:l0)//'pcf-x1z3.dat')
else
 pes_x1z3_pc = cx_null ; allocate (pes_x1z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1z3')) then
 write (*,*) ' reading pcf-y1z3.dat'
 call pes_y1z3_read (iun, pes_dir(1:l0)//'pcf-y1z3.dat')
else
 pes_y1z3_pc = cx_null ; allocate (pes_y1z3_cf(0:-1))
endif
!! if (cx_substr(sys(1:l1),'z4')) then
!!  write (*,*) ' reading pcf-z4.dat'
!!  call pes_z4_read (iun, pes_dir(1:l0)//'pcf-z4.dat')
!! else
!!  pes_z4_pc = cx_null ; allocate (pes_z4_cf(0:-1))
!! endif
if (cx_substr(sys(1:l1),'x3u1')) then
 write (*,*) ' reading pcf-x3u1.dat'
 call pes_x3u1_read (iun, pes_dir(1:l0)//'pcf-x3u1.dat')
else
 pes_x3u1_pc = cx_null ; allocate (pes_x3u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1u1')) then
 write (*,*) ' reading pcf-x2y1u1.dat'
 call pes_x2y1u1_read (iun, pes_dir(1:l0)//'pcf-x2y1u1.dat')
else
 pes_x2y1u1_pc = cx_null ; allocate (pes_x2y1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2u1')) then
 write (*,*) ' reading pcf-x1y2u1.dat'
 call pes_x1y2u1_read (iun, pes_dir(1:l0)//'pcf-x1y2u1.dat')
else
 pes_x1y2u1_pc = cx_null ; allocate (pes_x1y2u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2z1u1')) then
 write (*,*) ' reading pcf-x2z1u1.dat'
 call pes_x2z1u1_read (iun, pes_dir(1:l0)//'pcf-x2z1u1.dat')
else
 pes_x2z1u1_pc = cx_null ; allocate (pes_x2z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1z1u1')) then
 write (*,*) ' reading pcf-x1y1z1u1.dat'
 call pes_x1y1z1u1_read (iun, pes_dir(1:l0)//'pcf-x1y1z1u1.dat')
else
 pes_x1y1z1u1_pc = cx_null ; allocate (pes_x1y1z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2z1u1')) then
 write (*,*) ' reading pcf-y2z1u1.dat'
 call pes_y2z1u1_read (iun, pes_dir(1:l0)//'pcf-y2z1u1.dat')
else
 pes_y2z1u1_pc = cx_null ; allocate (pes_y2z1u1_cf(0:-1))
endif
! Read pcf data for five-body systems.
if (cx_substr(sys(1:l1),'x5')) then
 write (*,*) ' reading pcf-x5.dat'
 call pes_x5_read (iun, pes_dir(1:l0)//'pcf-x5.dat')
else
 pes_x5_pc = cx_null ; allocate (pes_x5_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1')) then
 write (*,*) ' reading pcf-x4y1.dat'
 call pes_x4y1_read (iun, pes_dir(1:l0)//'pcf-x4y1.dat')
else
 pes_x4y1_pc = cx_null ; allocate (pes_x4y1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2')) then
 write (*,*) ' reading pcf-x3y2.dat'
 call pes_x3y2_read (iun, pes_dir(1:l0)//'pcf-x3y2.dat')
else
 pes_x3y2_pc = cx_null ; allocate (pes_x3y2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y3')) then
 write (*,*) ' reading pcf-x2y3.dat'
 call pes_x2y3_read (iun, pes_dir(1:l0)//'pcf-x2y3.dat')
else
 pes_x2y3_pc = cx_null ; allocate (pes_x2y3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4z1')) then
 write (*,*) ' reading pcf-x4z1.dat'
 call pes_x4z1_read (iun, pes_dir(1:l0)//'pcf-x4z1.dat')
else
 pes_x4z1_pc = cx_null ; allocate (pes_x4z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1z1')) then
 write (*,*) ' reading pcf-x3y1z1.dat'
 call pes_x3y1z1_read (iun, pes_dir(1:l0)//'pcf-x3y1z1.dat')
else
 pes_x3y1z1_pc = cx_null ; allocate (pes_x3y1z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2z1')) then
 write (*,*) ' reading pcf-x2y2z1.dat'
 call pes_x2y2z1_read (iun, pes_dir(1:l0)//'pcf-x2y2z1.dat')
else
 pes_x2y2z1_pc = cx_null ; allocate (pes_x2y2z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y3z1')) then
 write (*,*) ' reading pcf-x1y3z1.dat'
 call pes_x1y3z1_read (iun, pes_dir(1:l0)//'pcf-x1y3z1.dat')
else
 pes_x1y3z1_pc = cx_null ; allocate (pes_x1y3z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3z2')) then
 write (*,*) ' reading pcf-x3z2.dat'
 call pes_x3z2_read (iun, pes_dir(1:l0)//'pcf-x3z2.dat')
else
 pes_x3z2_pc = cx_null ; allocate (pes_x3z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1z2')) then
 write (*,*) ' reading pcf-x2y1z2.dat'
 call pes_x2y1z2_read (iun, pes_dir(1:l0)//'pcf-x2y1z2.dat')
else
 pes_x2y1z2_pc = cx_null ; allocate (pes_x2y1z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2z2')) then
 write (*,*) ' reading pcf-x1y2z2.dat'
 call pes_x1y2z2_read (iun, pes_dir(1:l0)//'pcf-x1y2z2.dat')
else
 pes_x1y2z2_pc = cx_null ; allocate (pes_x1y2z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y3z2')) then
 write (*,*) ' reading pcf-y3z2.dat'
 call pes_y3z2_read (iun, pes_dir(1:l0)//'pcf-y3z2.dat')
else
 pes_y3z2_pc = cx_null ; allocate (pes_y3z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2z3')) then
 write (*,*) ' reading pcf-x2z3.dat'
 call pes_x2z3_read (iun, pes_dir(1:l0)//'pcf-x2z3.dat')
else
 pes_x2z3_pc = cx_null ; allocate (pes_x2z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1z3')) then
 write (*,*) ' reading pcf-x1y1z3.dat'
 call pes_x1y1z3_read (iun, pes_dir(1:l0)//'pcf-x1y1z3.dat')
else
 pes_x1y1z3_pc = cx_null ; allocate (pes_x1y1z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2z3')) then
 write (*,*) ' reading pcf-y2z3.dat'
 call pes_y2z3_read (iun, pes_dir(1:l0)//'pcf-y2z3.dat')
else
 pes_y2z3_pc = cx_null ; allocate (pes_y2z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4u1')) then
 write (*,*) ' reading pcf-x4u1.dat'
 call pes_x4u1_read (iun, pes_dir(1:l0)//'pcf-x4u1.dat')
else
 pes_x4u1_pc = cx_null ; allocate (pes_x4u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1u1')) then
 write (*,*) ' reading pcf-x3y1u1.dat'
 call pes_x3y1u1_read (iun, pes_dir(1:l0)//'pcf-x3y1u1.dat')
else
 pes_x3y1u1_pc = cx_null ; allocate (pes_x3y1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2u1')) then
 write (*,*) ' reading pcf-x2y2u1.dat'
 call pes_x2y2u1_read (iun, pes_dir(1:l0)//'pcf-x2y2u1.dat')
else
 pes_x2y2u1_pc = cx_null ; allocate (pes_x2y2u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3z1u1')) then
 write (*,*) ' reading pcf-x3z1u1.dat'
 call pes_x3z1u1_read (iun, pes_dir(1:l0)//'pcf-x3z1u1.dat')
else
 pes_x3z1u1_pc = cx_null ; allocate (pes_x3z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1z1u1')) then
 write (*,*) ' reading pcf-x2y1z1u1.dat'
 call pes_x2y1z1u1_read (iun, pes_dir(1:l0)//'pcf-x2y1z1u1.dat')
else
 pes_x2y1z1u1_pc = cx_null ; allocate (pes_x2y1z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2z1u1')) then
 write (*,*) ' reading pcf-x1y2z1u1.dat'
 call pes_x1y2z1u1_read (iun, pes_dir(1:l0)//'pcf-x1y2z1u1.dat')
else
 pes_x1y2z1u1_pc = cx_null ; allocate (pes_x1y2z1u1_cf(0:-1))
endif
! Read pcf data for six-body systems.
if (cx_substr(sys(1:l1),'x6')) then
 write (*,*) ' reading pcf-x6.dat'
 call pes_x6_read (iun, pes_dir(1:l0)//'pcf-x6.dat')
else
 pes_x6_pc = cx_null ; allocate (pes_x6_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y1')) then
 write (*,*) ' reading pcf-x5y1.dat'
 call pes_x5y1_read (iun, pes_dir(1:l0)//'pcf-x5y1.dat')
else
 pes_x5y1_pc = cx_null ; allocate (pes_x5y1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2')) then
 write (*,*) ' reading pcf-x4y2.dat'
 call pes_x4y2_read (iun, pes_dir(1:l0)//'pcf-x4y2.dat')
else
 pes_x4y2_pc = cx_null ; allocate (pes_x4y2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y3')) then
 write (*,*) ' reading pcf-x3y3.dat'
 call pes_x3y3_read (iun, pes_dir(1:l0)//'pcf-x3y3.dat')
else
 pes_x3y3_pc = cx_null ; allocate (pes_x3y3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5z1')) then
 write (*,*) ' reading pcf-x5z1.dat'
 call pes_x5z1_read (iun, pes_dir(1:l0)//'pcf-x5z1.dat')
else
 pes_x5z1_pc = cx_null ; allocate (pes_x5z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1z1')) then
 write (*,*) ' reading pcf-x4y1z1.dat'
 call pes_x4y1z1_read (iun, pes_dir(1:l0)//'pcf-x4y1z1.dat')
else
 pes_x4y1z1_pc = cx_null ; allocate (pes_x4y1z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2z1')) then
 write (*,*) ' reading pcf-x3y2z1.dat'
 call pes_x3y2z1_read (iun, pes_dir(1:l0)//'pcf-x3y2z1.dat')
else
 pes_x3y2z1_pc = cx_null ; allocate (pes_x3y2z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y3z1')) then
 write (*,*) ' reading pcf-x2y3z1.dat'
 call pes_x2y3z1_read (iun, pes_dir(1:l0)//'pcf-x2y3z1.dat')
else
 pes_x2y3z1_pc = cx_null ; allocate (pes_x2y3z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4z2')) then
 write (*,*) ' reading pcf-x4z2.dat'
 call pes_x4z2_read (iun, pes_dir(1:l0)//'pcf-x4z2.dat')
else
 pes_x4z2_pc = cx_null ; allocate (pes_x4z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1z2')) then
 write (*,*) ' reading pcf-x3y1z2.dat'
 call pes_x3y1z2_read (iun, pes_dir(1:l0)//'pcf-x3y1z2.dat')
else
 pes_x3y1z2_pc = cx_null ; allocate (pes_x3y1z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2z2')) then
 write (*,*) ' reading pcf-x2y2z2.dat'
 call pes_x2y2z2_read (iun, pes_dir(1:l0)//'pcf-x2y2z2.dat')
else
 pes_x2y2z2_pc = cx_null ; allocate (pes_x2y2z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y3z2')) then
 write (*,*) ' reading pcf-x1y3z2.dat'
 call pes_x1y3z2_read (iun, pes_dir(1:l0)//'pcf-x1y3z2.dat')
else
 pes_x1y3z2_pc = cx_null ; allocate (pes_x1y3z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3z3')) then
 write (*,*) ' reading pcf-x3z3.dat'
 call pes_x3z3_read (iun, pes_dir(1:l0)//'pcf-x3z3.dat')
else
 pes_x3z3_pc = cx_null ; allocate (pes_x3z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1z3')) then
 write (*,*) ' reading pcf-x2y1z3.dat'
 call pes_x2y1z3_read (iun, pes_dir(1:l0)//'pcf-x2y1z3.dat')
else
 pes_x2y1z3_pc = cx_null ; allocate (pes_x2y1z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2z3')) then
 write (*,*) ' reading pcf-x1y2z3.dat'
 call pes_x1y2z3_read (iun, pes_dir(1:l0)//'pcf-x1y2z3.dat')
else
 pes_x1y2z3_pc = cx_null ; allocate (pes_x1y2z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'y3z3')) then
 write (*,*) ' reading pcf-y3z3.dat'
 call pes_y3z3_read (iun, pes_dir(1:l0)//'pcf-y3z3.dat')
else
 pes_y3z3_pc = cx_null ; allocate (pes_y3z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5u1')) then
 write (*,*) ' reading pcf-x5u1.dat'
 call pes_x5u1_read (iun, pes_dir(1:l0)//'pcf-x5u1.dat')
else
 pes_x5u1_pc = cx_null ; allocate (pes_x5u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1u1')) then
 write (*,*) ' reading pcf-x4y1u1.dat'
 call pes_x4y1u1_read (iun, pes_dir(1:l0)//'pcf-x4y1u1.dat')
else
 pes_x4y1u1_pc = cx_null ; allocate (pes_x4y1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2u1')) then
 write (*,*) ' reading pcf-x3y2u1.dat'
 call pes_x3y2u1_read (iun, pes_dir(1:l0)//'pcf-x3y2u1.dat')
else
 pes_x3y2u1_pc = cx_null ; allocate (pes_x3y2u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4z1u1')) then
 write (*,*) ' reading pcf-x4z1u1.dat'
 call pes_x4z1u1_read (iun, pes_dir(1:l0)//'pcf-x4z1u1.dat')
else
 pes_x4z1u1_pc = cx_null ; allocate (pes_x4z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1z1u1')) then
 write (*,*) ' reading pcf-x3y1z1u1.dat'
 call pes_x3y1z1u1_read (iun, pes_dir(1:l0)//'pcf-x3y1z1u1.dat')
else
 pes_x3y1z1u1_pc = cx_null ; allocate (pes_x3y1z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2z1u1')) then
 write (*,*) ' reading pcf-x2y2z1u1.dat'
 call pes_x2y2z1u1_read (iun, pes_dir(1:l0)//'pcf-x2y2z1u1.dat')
else
 pes_x2y2z1u1_pc = cx_null ; allocate (pes_x2y2z1u1_cf(0:-1))
endif
! Read pcf data for seven-body systems.
if (cx_substr(sys(1:l1),'x7')) then
 write (*,*) ' reading pcf-x7.dat'
 call pes_x7_read (iun, pes_dir(1:l0)//'pcf-x7.dat')
else
 pes_x7_pc = cx_null ; allocate (pes_x7_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6y1')) then
 write (*,*) ' reading pcf-x6y1.dat'
 call pes_x6y1_read (iun, pes_dir(1:l0)//'pcf-x6y1.dat')
else
 pes_x6y1_pc = cx_null ; allocate (pes_x6y1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y2')) then
 write (*,*) ' reading pcf-x5y2.dat'
 call pes_x5y2_read (iun, pes_dir(1:l0)//'pcf-x5y2.dat')
else
 pes_x5y2_pc = cx_null ; allocate (pes_x5y2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y3')) then
 write (*,*) ' reading pcf-x4y3.dat'
 call pes_x4y3_read (iun, pes_dir(1:l0)//'pcf-x4y3.dat')
else
 pes_x4y3_pc = cx_null ; allocate (pes_x4y3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y1z1')) then
 write (*,*) ' reading pcf-x5y1z1.dat'
 call pes_x5y1z1_read (iun, pes_dir(1:l0)//'pcf-x5y1z1.dat')
else
 pes_x5y1z1_pc = cx_null ; allocate (pes_x5y1z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2z1')) then
 write (*,*) ' reading pcf-x4y2z1.dat'
 call pes_x4y2z1_read (iun, pes_dir(1:l0)//'pcf-x4y2z1.dat')
else
 pes_x4y2z1_pc = cx_null ; allocate (pes_x4y2z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y3z1')) then
 write (*,*) ' reading pcf-x3y3z1.dat'
 call pes_x3y3z1_read (iun, pes_dir(1:l0)//'pcf-x3y3z1.dat')
else
 pes_x3y3z1_pc = cx_null ; allocate (pes_x3y3z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1z2')) then
 write (*,*) ' reading pcf-x4y1z2.dat'
 call pes_x4y1z2_read (iun, pes_dir(1:l0)//'pcf-x4y1z2.dat')
else
 pes_x4y1z2_pc = cx_null ; allocate (pes_x4y1z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2z2')) then
 write (*,*) ' reading pcf-x3y2z2.dat'
 call pes_x3y2z2_read (iun, pes_dir(1:l0)//'pcf-x3y2z2.dat')
else
 pes_x3y2z2_pc = cx_null ; allocate (pes_x3y2z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y3z2')) then
 write (*,*) ' reading pcf-x2y3z2.dat'
 call pes_x2y3z2_read (iun, pes_dir(1:l0)//'pcf-x2y3z2.dat')
else
 pes_x2y3z2_pc = cx_null ; allocate (pes_x2y3z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1z3')) then
 write (*,*) ' reading pcf-x3y1z3.dat'
 call pes_x3y1z3_read (iun, pes_dir(1:l0)//'pcf-x3y1z3.dat')
else
 pes_x3y1z3_pc = cx_null ; allocate (pes_x3y1z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2z3')) then
 write (*,*) ' reading pcf-x2y2z3.dat'
 call pes_x2y2z3_read (iun, pes_dir(1:l0)//'pcf-x2y2z3.dat')
else
 pes_x2y2z3_pc = cx_null ; allocate (pes_x2y2z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y3z3')) then
 write (*,*) ' reading pcf-x1y3z3.dat'
 call pes_x1y3z3_read (iun, pes_dir(1:l0)//'pcf-x1y3z3.dat')
else
 pes_x1y3z3_pc = cx_null ; allocate (pes_x1y3z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6u1')) then
 write (*,*) ' reading pcf-x6u1.dat'
 call pes_x6u1_read (iun, pes_dir(1:l0)//'pcf-x6u1.dat')
else
 pes_x6u1_pc = cx_null ; allocate (pes_x6u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y1u1')) then
 write (*,*) ' reading pcf-x5y1u1.dat'
 call pes_x5y1u1_read (iun, pes_dir(1:l0)//'pcf-x5y1u1.dat')
else
 pes_x5y1u1_pc = cx_null ; allocate (pes_x5y1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2u1')) then
 write (*,*) ' reading pcf-x4y2u1.dat'
 call pes_x4y2u1_read (iun, pes_dir(1:l0)//'pcf-x4y2u1.dat')
else
 pes_x4y2u1_pc = cx_null ; allocate (pes_x4y2u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5z1u1')) then
 write (*,*) ' reading pcf-x5z1u1.dat'
 call pes_x5z1u1_read (iun, pes_dir(1:l0)//'pcf-x5z1u1.dat')
else
 pes_x5z1u1_pc = cx_null ; allocate (pes_x5z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1z1u1')) then
 write (*,*) ' reading pcf-x4y1z1u1.dat'
 call pes_x4y1z1u1_read (iun, pes_dir(1:l0)//'pcf-x4y1z1u1.dat')
else
 pes_x4y1z1u1_pc = cx_null ; allocate (pes_x4y1z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2z1u1')) then
 write (*,*) ' reading pcf-x3y2z1u1.dat'
 call pes_x3y2z1u1_read (iun, pes_dir(1:l0)//'pcf-x3y2z1u1.dat')
else
 pes_x3y2z1u1_pc = cx_null ; allocate (pes_x3y2z1u1_cf(0:-1))
endif
! Read pcf data for eight-body systems.
if (cx_substr(sys(1:l1),'x7y1')) then
 write (*,*) ' reading pcf-x7y1.dat'
 call pes_x7y1_read (iun, pes_dir(1:l0)//'pcf-x7y1.dat')
else
 pes_x7y1_pc = cx_null ; allocate (pes_x7y1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6y2')) then
 write (*,*) ' reading pcf-x6y2.dat'
 call pes_x6y2_read (iun, pes_dir(1:l0)//'pcf-x6y2.dat')
else
 pes_x6y2_pc = cx_null ; allocate (pes_x6y2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y3')) then
 write (*,*) ' reading pcf-x5y3.dat'
 call pes_x5y3_read (iun, pes_dir(1:l0)//'pcf-x5y3.dat')
else
 pes_x5y3_pc = cx_null ; allocate (pes_x5y3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y4')) then
 write (*,*) ' reading pcf-x4y4.dat'
 call pes_x4y4_read (iun, pes_dir(1:l0)//'pcf-x4y4.dat')
else
 pes_x4y4_pc = cx_null ; allocate (pes_x4y4_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y2z1')) then
 write (*,*) ' reading pcf-x5y2z1.dat'
 call pes_x5y2z1_read (iun, pes_dir(1:l0)//'pcf-x5y2z1.dat')
else
 pes_x5y2z1_pc = cx_null ; allocate (pes_x5y2z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y3z1')) then
 write (*,*) ' reading pcf-x4y3z1.dat'
 call pes_x4y3z1_read (iun, pes_dir(1:l0)//'pcf-x4y3z1.dat')
else
 pes_x4y3z1_pc = cx_null ; allocate (pes_x4y3z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2z2')) then
 write (*,*) ' reading pcf-x4y2z2.dat'
 call pes_x4y2z2_read (iun, pes_dir(1:l0)//'pcf-x4y2z2.dat')
else
 pes_x4y2z2_pc = cx_null ; allocate (pes_x4y2z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y3z2')) then
 write (*,*) ' reading pcf-x3y3z2.dat'
 call pes_x3y3z2_read (iun, pes_dir(1:l0)//'pcf-x3y3z2.dat')
else
 pes_x3y3z2_pc = cx_null ; allocate (pes_x3y3z2_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2z3')) then
 write (*,*) ' reading pcf-x3y2z3.dat'
 call pes_x3y2z3_read (iun, pes_dir(1:l0)//'pcf-x3y2z3.dat')
else
 pes_x3y2z3_pc = cx_null ; allocate (pes_x3y2z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y3z3')) then
 write (*,*) ' reading pcf-x2y3z3.dat'
 call pes_x2y3z3_read (iun, pes_dir(1:l0)//'pcf-x2y3z3.dat')
else
 pes_x2y3z3_pc = cx_null ; allocate (pes_x2y3z3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6y1u1')) then
 write (*,*) ' reading pcf-x6y1u1.dat'
 call pes_x6y1u1_read (iun, pes_dir(1:l0)//'pcf-x6y1u1.dat')
else
 pes_x6y1u1_pc = cx_null ; allocate (pes_x6y1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6z1u1')) then
 write (*,*) ' reading pcf-x6z1u1.dat'
 call pes_x6z1u1_read (iun, pes_dir(1:l0)//'pcf-x6z1u1.dat')
else
 pes_x6z1u1_pc = cx_null ; allocate (pes_x6z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y1z1u1')) then
 write (*,*) ' reading pcf-x5y1z1u1.dat'
 call pes_x5y1z1u1_read (iun, pes_dir(1:l0)//'pcf-x5y1z1u1.dat')
else
 pes_x5y1z1u1_pc = cx_null ; allocate (pes_x5y1z1u1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2z1u1')) then
 write (*,*) ' reading pcf-x4y2z1u1.dat'
 call pes_x4y2z1u1_read (iun, pes_dir(1:l0)//'pcf-x4y2z1u1.dat')
else
 pes_x4y2z1u1_pc = cx_null ; allocate (pes_x4y2z1u1_cf(0:-1))
endif
! Read pcf data for nine-body systems.
if (cx_substr(sys(1:l1),'x6y3')) then
 write (*,*) ' reading pcf-x6y3.dat'
 call pes_x6y3_read (iun, pes_dir(1:l0)//'pcf-x6y3.dat')
else
 pes_x6y3_pc = cx_null ; allocate (pes_x6y3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y4')) then
 write (*,*) ' reading pcf-x5y4.dat'
 call pes_x5y4_read (iun, pes_dir(1:l0)//'pcf-x5y4.dat')
else
 pes_x5y4_pc = cx_null ; allocate (pes_x5y4_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y3z1')) then
 write (*,*) ' reading pcf-x5y3z1.dat'
 call pes_x5y3z1_read (iun, pes_dir(1:l0)//'pcf-x5y3z1.dat')
else
 pes_x5y3z1_pc = cx_null ; allocate (pes_x5y3z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y4z1')) then
 write (*,*) ' reading pcf-x4y4z1.dat'
 call pes_x4y4z1_read (iun, pes_dir(1:l0)//'pcf-x4y4z1.dat')
else
 pes_x4y4z1_pc = cx_null ; allocate (pes_x4y4z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y3z3')) then
 write (*,*) ' reading pcf-x3y3z3.dat'
 call pes_x3y3z3_read (iun, pes_dir(1:l0)//'pcf-x3y3z3.dat')
else
 pes_x3y3z3_pc = cx_null ; allocate (pes_x3y3z3_cf(0:-1))
endif
! Read pcf data for ten-body systems.
if (cx_substr(sys(1:l1),'x7y3')) then
 write (*,*) ' reading pcf-x7y3.dat'
 call pes_x7y3_read (iun, pes_dir(1:l0)//'pcf-x7y3.dat')
else
 pes_x7y3_pc = cx_null ; allocate (pes_x7y3_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6y3z1')) then
 write (*,*) ' reading pcf-x6y3z1.dat'
 call pes_x6y3z1_read (iun, pes_dir(1:l0)//'pcf-x6y3z1.dat')
else
 pes_x6y3z1_pc = cx_null ; allocate (pes_x6y3z1_cf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y4z2')) then
 write (*,*) ' reading pcf-x4y4z2.dat'
 call pes_x4y4z2_read (iun, pes_dir(1:l0)//'pcf-x4y4z2.dat')
else
 pes_x4y4z2_pc = cx_null ; allocate (pes_x4y4z2_cf(0:-1))
endif
return
END SUBROUTINE pes1_init_xyz
