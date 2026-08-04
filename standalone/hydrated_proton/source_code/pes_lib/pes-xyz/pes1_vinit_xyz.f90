SUBROUTINE pes1_vinit_xyz (sys)
! Read vpcf data files for XYZ systems.
character (len=*), intent (in) :: sys
!------------------------------------------------------------------------
integer :: iun, l0, l1
call pes_getiun (iun)
l0 = len_trim(pes_dir)
l1 = len_trim(sys)
! Read vpcf data for the one-body systems
if (cx_substr(sys(1:l1),'x1')) then
 write (*,*) ' reading vpcf-x1.dat'
 call pes_x1_vread (iun, pes_dir(1:l0)//'vpcf-x1.dat')
else
 pes_x1_vcf = 0
endif
if (cx_substr(sys(1:l1),'y1')) then
 write (*,*) ' reading vpcf-y1.dat'
 call pes_y1_vread (iun, pes_dir(1:l0)//'vpcf-y1.dat')
else
 pes_y1_vcf = 0
endif
if (cx_substr(sys(1:l1),'z1')) then
 write (*,*) ' reading vpcf-z1.dat'
 call pes_z1_vread (iun, pes_dir(1:l0)//'vpcf-z1.dat')
else
 pes_z1_vcf = 0
endif
if (cx_substr(sys(1:l1),'u1')) then
 write (*,*) ' reading vpcf-u1.dat'
 call pes_u1_vread (iun, pes_dir(1:l0)//'vpcf-u1.dat')
else
 pes_u1_vcf = 0
endif
! Read vpcf data for the multi-body systems.  We've arranged the
! code according to grevlex order on number of X, Y, Z.
! Read vpcf data for two-body systems.
if (cx_substr(sys(1:l1),'x2')) then
 write (*,*) ' reading vpcf-x2.dat'
 call pes_x2_vread (iun, pes_dir(1:l0)//'vpcf-x2.dat')
else
 pes_x2_vpc = cx_null ; allocate (pes_x2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1')) then
 write (*,*) ' reading vpcf-x1y1.dat'
 call pes_x1y1_vread (iun, pes_dir(1:l0)//'vpcf-x1y1.dat')
else
 pes_x1y1_vpc = cx_null ; allocate (pes_x1y1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2')) then
 write (*,*) ' reading vpcf-y2.dat'
 call pes_y2_vread (iun, pes_dir(1:l0)//'vpcf-y2.dat')
else
 pes_y2_vpc = cx_null ; allocate (pes_y2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1z1')) then
 write (*,*) ' reading vpcf-x1z1.dat'
 call pes_x1z1_vread (iun, pes_dir(1:l0)//'vpcf-x1z1.dat')
else
 pes_x1z1_vpc = cx_null ; allocate (pes_x1z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1z1')) then
 write (*,*) ' reading vpcf-y1z1.dat'
 call pes_y1z1_vread (iun, pes_dir(1:l0)//'vpcf-y1z1.dat')
else
 pes_y1z1_vpc = cx_null ; allocate (pes_y1z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'z2')) then
 write (*,*) ' reading vpcf-z2.dat'
 call pes_z2_vread (iun, pes_dir(1:l0)//'vpcf-z2.dat')
else
 pes_z2_vpc = cx_null ; allocate (pes_z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1u1')) then
 write (*,*) ' reading vpcf-x1u1.dat'
 call pes_x1u1_vread (iun, pes_dir(1:l0)//'vpcf-x1u1.dat')
else
 pes_x1u1_vpc = cx_null ; allocate (pes_x1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1u1')) then
 write (*,*) ' reading vpcf-y1u1.dat'
 call pes_y1u1_vread (iun, pes_dir(1:l0)//'vpcf-y1u1.dat')
else
 pes_y1u1_vpc = cx_null ; allocate (pes_y1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'z1u1')) then
 write (*,*) ' reading vpcf-z1u1.dat'
 call pes_z1u1_vread (iun, pes_dir(1:l0)//'vpcf-z1u1.dat')
else
 pes_z1u1_vpc = cx_null ; allocate (pes_z1u1_vcf(0:-1))
endif
! Read vpcf data for three-body systems.
if (cx_substr(sys(1:l1),'x3')) then
 write (*,*) ' reading vpcf-x3.dat'
 call pes_x3_vread (iun, pes_dir(1:l0)//'vpcf-x3.dat')
else
 pes_x3_vpc = cx_null ; allocate (pes_x3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1')) then
 write (*,*) ' reading vpcf-x2y1.dat'
 call pes_x2y1_vread (iun, pes_dir(1:l0)//'vpcf-x2y1.dat')
else
 pes_x2y1_vpc = cx_null ; allocate (pes_x2y1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2')) then
 write (*,*) ' reading vpcf-x1y2.dat'
 call pes_x1y2_vread (iun, pes_dir(1:l0)//'vpcf-x1y2.dat')
else
 pes_x1y2_vpc = cx_null ; allocate (pes_x1y2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y3')) then
 write (*,*) ' reading vpcf-y3.dat'
 call pes_y3_vread (iun, pes_dir(1:l0)//'vpcf-y3.dat')
else
 pes_y3_vpc = cx_null ; allocate (pes_y3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2z1')) then
 write (*,*) ' reading vpcf-x2z1.dat'
 call pes_x2z1_vread (iun, pes_dir(1:l0)//'vpcf-x2z1.dat')
else
 pes_x2z1_vpc = cx_null ; allocate (pes_x2z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1z1')) then
 write (*,*) ' reading vpcf-x1y1z1.dat'
 call pes_x1y1z1_vread (iun, pes_dir(1:l0)//'vpcf-x1y1z1.dat')
else
 pes_x1y1z1_vpc = cx_null ; allocate (pes_x1y1z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2z1')) then
 write (*,*) ' reading vpcf-y2z1.dat'
 call pes_y2z1_vread (iun, pes_dir(1:l0)//'vpcf-y2z1.dat')
else
 pes_y2z1_vpc = cx_null ; allocate (pes_y2z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1z2')) then
 write (*,*) ' reading vpcf-x1z2.dat'
 call pes_x1z2_vread (iun, pes_dir(1:l0)//'vpcf-x1z2.dat')
else
 pes_x1z2_vpc = cx_null ; allocate (pes_x1z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1z2')) then
 write (*,*) ' reading vpcf-y1z2.dat'
 call pes_y1z2_vread (iun, pes_dir(1:l0)//'vpcf-y1z2.dat')
else
 pes_y1z2_vpc = cx_null ; allocate (pes_y1z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'z3')) then
 write (*,*) ' reading vpcf-z3.dat'
 call pes_z3_vread (iun, pes_dir(1:l0)//'vpcf-z3.dat')
else
 pes_z3_vpc = cx_null ; allocate (pes_z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2u1')) then
 write (*,*) ' reading vpcf-x2u1.dat'
 call pes_x2u1_vread (iun, pes_dir(1:l0)//'vpcf-x2u1.dat')
else
 pes_x2u1_vpc = cx_null ; allocate (pes_x2u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1u1')) then
 write (*,*) ' reading vpcf-x1y1u1.dat'
 call pes_x1y1u1_vread (iun, pes_dir(1:l0)//'vpcf-x1y1u1.dat')
else
 pes_x1y1u1_vpc = cx_null ; allocate (pes_x1y1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2u1')) then
 write (*,*) ' reading vpcf-y2u1.dat'
 call pes_y2u1_vread (iun, pes_dir(1:l0)//'vpcf-y2u1.dat')
else
 pes_y2u1_vpc = cx_null ; allocate (pes_y2u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1z1u1')) then
 write (*,*) ' reading vpcf-x1z1u1.dat'
 call pes_x1z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x1z1u1.dat')
else
 pes_x1z1u1_vpc = cx_null ; allocate (pes_x1z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1z1u1')) then
 write (*,*) ' reading vpcf-y1z1u1.dat'
 call pes_y1z1u1_vread (iun, pes_dir(1:l0)//'vpcf-y1z1u1.dat')
else
 pes_y1z1u1_vpc = cx_null ; allocate (pes_y1z1u1_vcf(0:-1))
endif
! Read vpcf data for four-body systems.
if (cx_substr(sys(1:l1),'x4')) then
 write (*,*) ' reading vpcf-x4.dat'
 call pes_x4_vread (iun, pes_dir(1:l0)//'vpcf-x4.dat')
else
 pes_x4_vpc = cx_null ; allocate (pes_x4_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1')) then
 write (*,*) ' reading vpcf-x3y1.dat'
 call pes_x3y1_vread (iun, pes_dir(1:l0)//'vpcf-x3y1.dat')
else
 pes_x3y1_vpc = cx_null ; allocate (pes_x3y1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2')) then
 write (*,*) ' reading vpcf-x2y2.dat'
 call pes_x2y2_vread (iun, pes_dir(1:l0)//'vpcf-x2y2.dat')
else
 pes_x2y2_vpc = cx_null ; allocate (pes_x2y2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y3')) then
 write (*,*) ' reading vpcf-x1y3.dat'
 call pes_x1y3_vread (iun, pes_dir(1:l0)//'vpcf-x1y3.dat')
else
 pes_x1y3_vpc = cx_null ; allocate (pes_x1y3_vcf(0:-1))
endif
!! if (cx_substr(sys(1:l1),'y4')) then
!!  write (*,*) ' reading vpcf-y4.dat'
!!  call pes_y4_vread (iun, pes_dir(1:l0)//'vpcf-y4.dat')
!! else
!!  pes_y4_vpc = cx_null ; allocate (pes_y4_vcf(0:-1))
!! endif
if (cx_substr(sys(1:l1),'x3z1')) then
 write (*,*) ' reading vpcf-x3z1.dat'
 call pes_x3z1_vread (iun, pes_dir(1:l0)//'vpcf-x3z1.dat')
else
 pes_x3z1_vpc = cx_null ; allocate (pes_x3z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1z1')) then
 write (*,*) ' reading vpcf-x2y1z1.dat'
 call pes_x2y1z1_vread (iun, pes_dir(1:l0)//'vpcf-x2y1z1.dat')
else
 pes_x2y1z1_vpc = cx_null ; allocate (pes_x2y1z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2z1')) then
 write (*,*) ' reading vpcf-x1y2z1.dat'
 call pes_x1y2z1_vread (iun, pes_dir(1:l0)//'vpcf-x1y2z1.dat')
else
 pes_x1y2z1_vpc = cx_null ; allocate (pes_x1y2z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y3z1')) then
 write (*,*) ' reading vpcf-y3z1.dat'
 call pes_y3z1_vread (iun, pes_dir(1:l0)//'vpcf-y3z1.dat')
else
 pes_y3z1_vpc = cx_null ; allocate (pes_y3z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2z2')) then
 write (*,*) ' reading vpcf-x2z2.dat'
 call pes_x2z2_vread (iun, pes_dir(1:l0)//'vpcf-x2z2.dat')
else
 pes_x2z2_vpc = cx_null ; allocate (pes_x2z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1z2')) then
 write (*,*) ' reading vpcf-x1y1z2.dat'
 call pes_x1y1z2_vread (iun, pes_dir(1:l0)//'vpcf-x1y1z2.dat')
else
 pes_x1y1z2_vpc = cx_null ; allocate (pes_x1y1z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2z2')) then
 write (*,*) ' reading vpcf-y2z2.dat'
 call pes_y2z2_vread (iun, pes_dir(1:l0)//'vpcf-y2z2.dat')
else
 pes_y2z2_vpc = cx_null ; allocate (pes_y2z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1z3')) then
 write (*,*) ' reading vpcf-x1z3.dat'
 call pes_x1z3_vread (iun, pes_dir(1:l0)//'vpcf-x1z3.dat')
else
 pes_x1z3_vpc = cx_null ; allocate (pes_x1z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y1z3')) then
 write (*,*) ' reading vpcf-y1z3.dat'
 call pes_y1z3_vread (iun, pes_dir(1:l0)//'vpcf-y1z3.dat')
else
 pes_y1z3_vpc = cx_null ; allocate (pes_y1z3_vcf(0:-1))
endif
!! if (cx_substr(sys(1:l1),'z4')) then
!!  write (*,*) ' reading vpcf-z4.dat'
!!  call pes_z4_vread (iun, pes_dir(1:l0)//'vpcf-z4.dat')
!! else
!!  pes_z4_vpc = cx_null ; allocate (pes_z4_vcf(0:-1))
!! endif
if (cx_substr(sys(1:l1),'x3u1')) then
 write (*,*) ' reading vpcf-x3u1.dat'
 call pes_x3u1_vread (iun, pes_dir(1:l0)//'vpcf-x3u1.dat')
else
 pes_x3u1_vpc = cx_null ; allocate (pes_x3u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1u1')) then
 write (*,*) ' reading vpcf-x2y1u1.dat'
 call pes_x2y1u1_vread (iun, pes_dir(1:l0)//'vpcf-x2y1u1.dat')
else
 pes_x2y1u1_vpc = cx_null ; allocate (pes_x2y1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2u1')) then
 write (*,*) ' reading vpcf-x1y2u1.dat'
 call pes_x1y2u1_vread (iun, pes_dir(1:l0)//'vpcf-x1y2u1.dat')
else
 pes_x1y2u1_vpc = cx_null ; allocate (pes_x1y2u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2z1u1')) then
 write (*,*) ' reading vpcf-x2z1u1.dat'
 call pes_x2z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x2z1u1.dat')
else
 pes_x2z1u1_vpc = cx_null ; allocate (pes_x2z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1z1u1')) then
 write (*,*) ' reading vpcf-x1y1z1u1.dat'
 call pes_x1y1z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x1y1z1u1.dat')
else
 pes_x1y1z1u1_vpc = cx_null ; allocate (pes_x1y1z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2z1u1')) then
 write (*,*) ' reading vpcf-y2z1u1.dat'
 call pes_y2z1u1_vread (iun, pes_dir(1:l0)//'vpcf-y2z1u1.dat')
else
 pes_y2z1u1_vpc = cx_null ; allocate (pes_y2z1u1_vcf(0:-1))
endif
! Read vpcf data for five-body systems.
if (cx_substr(sys(1:l1),'x5')) then
 write (*,*) ' reading vpcf-x5.dat'
 call pes_x5_vread (iun, pes_dir(1:l0)//'vpcf-x5.dat')
else
 pes_x5_vpc = cx_null ; allocate (pes_x5_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1')) then
 write (*,*) ' reading vpcf-x4y1.dat'
 call pes_x4y1_vread (iun, pes_dir(1:l0)//'vpcf-x4y1.dat')
else
 pes_x4y1_vpc = cx_null ; allocate (pes_x4y1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2')) then
 write (*,*) ' reading vpcf-x3y2.dat'
 call pes_x3y2_vread (iun, pes_dir(1:l0)//'vpcf-x3y2.dat')
else
 pes_x3y2_vpc = cx_null ; allocate (pes_x3y2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y3')) then
 write (*,*) ' reading vpcf-x2y3.dat'
 call pes_x2y3_vread (iun, pes_dir(1:l0)//'vpcf-x2y3.dat')
else
 pes_x2y3_vpc = cx_null ; allocate (pes_x2y3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4z1')) then
 write (*,*) ' reading vpcf-x4z1.dat'
 call pes_x4z1_vread (iun, pes_dir(1:l0)//'vpcf-x4z1.dat')
else
 pes_x4z1_vpc = cx_null ; allocate (pes_x4z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1z1')) then
 write (*,*) ' reading vpcf-x3y1z1.dat'
 call pes_x3y1z1_vread (iun, pes_dir(1:l0)//'vpcf-x3y1z1.dat')
else
 pes_x3y1z1_vpc = cx_null ; allocate (pes_x3y1z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2z1')) then
 write (*,*) ' reading vpcf-x2y2z1.dat'
 call pes_x2y2z1_vread (iun, pes_dir(1:l0)//'vpcf-x2y2z1.dat')
else
 pes_x2y2z1_vpc = cx_null ; allocate (pes_x2y2z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y3z1')) then
 write (*,*) ' reading vpcf-x1y3z1.dat'
 call pes_x1y3z1_vread (iun, pes_dir(1:l0)//'vpcf-x1y3z1.dat')
else
 pes_x1y3z1_vpc = cx_null ; allocate (pes_x1y3z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3z2')) then
 write (*,*) ' reading vpcf-x3z2.dat'
 call pes_x3z2_vread (iun, pes_dir(1:l0)//'vpcf-x3z2.dat')
else
 pes_x3z2_vpc = cx_null ; allocate (pes_x3z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1z2')) then
 write (*,*) ' reading vpcf-x2y1z2.dat'
 call pes_x2y1z2_vread (iun, pes_dir(1:l0)//'vpcf-x2y1z2.dat')
else
 pes_x2y1z2_vpc = cx_null ; allocate (pes_x2y1z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2z2')) then
 write (*,*) ' reading vpcf-x1y2z2.dat'
 call pes_x1y2z2_vread (iun, pes_dir(1:l0)//'vpcf-x1y2z2.dat')
else
 pes_x1y2z2_vpc = cx_null ; allocate (pes_x1y2z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y3z2')) then
 write (*,*) ' reading vpcf-y3z2.dat'
 call pes_y3z2_vread (iun, pes_dir(1:l0)//'vpcf-y3z2.dat')
else
 pes_y3z2_vpc = cx_null ; allocate (pes_y3z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2z3')) then
 write (*,*) ' reading vpcf-x2z3.dat'
 call pes_x2z3_vread (iun, pes_dir(1:l0)//'vpcf-x2z3.dat')
else
 pes_x2z3_vpc = cx_null ; allocate (pes_x2z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y1z3')) then
 write (*,*) ' reading vpcf-x1y1z3.dat'
 call pes_x1y1z3_vread (iun, pes_dir(1:l0)//'vpcf-x1y1z3.dat')
else
 pes_x1y1z3_vpc = cx_null ; allocate (pes_x1y1z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y2z3')) then
 write (*,*) ' reading vpcf-y2z3.dat'
 call pes_y2z3_vread (iun, pes_dir(1:l0)//'vpcf-y2z3.dat')
else
 pes_y2z3_vpc = cx_null ; allocate (pes_y2z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4u1')) then
 write (*,*) ' reading vpcf-x4u1.dat'
 call pes_x4u1_vread (iun, pes_dir(1:l0)//'vpcf-x4u1.dat')
else
 pes_x4u1_vpc = cx_null ; allocate (pes_x4u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1u1')) then
 write (*,*) ' reading vpcf-x3y1u1.dat'
 call pes_x3y1u1_vread (iun, pes_dir(1:l0)//'vpcf-x3y1u1.dat')
else
 pes_x3y1u1_vpc = cx_null ; allocate (pes_x3y1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2u1')) then
 write (*,*) ' reading vpcf-x2y2u1.dat'
 call pes_x2y2u1_vread (iun, pes_dir(1:l0)//'vpcf-x2y2u1.dat')
else
 pes_x2y2u1_vpc = cx_null ; allocate (pes_x2y2u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3z1u1')) then
 write (*,*) ' reading vpcf-x3z1u1.dat'
 call pes_x3z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x3z1u1.dat')
else
 pes_x3z1u1_vpc = cx_null ; allocate (pes_x3z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1z1u1')) then
 write (*,*) ' reading vpcf-x2y1z1u1.dat'
 call pes_x2y1z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x2y1z1u1.dat')
else
 pes_x2y1z1u1_vpc = cx_null ; allocate (pes_x2y1z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2z1u1')) then
 write (*,*) ' reading vpcf-x1y2z1u1.dat'
 call pes_x1y2z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x1y2z1u1.dat')
else
 pes_x1y2z1u1_vpc = cx_null ; allocate (pes_x1y2z1u1_vcf(0:-1))
endif
! Read vpcf data for six-body systems.
if (cx_substr(sys(1:l1),'x6')) then
 write (*,*) ' reading vpcf-x6.dat'
 call pes_x6_vread (iun, pes_dir(1:l0)//'vpcf-x6.dat')
else
 pes_x6_vpc = cx_null ; allocate (pes_x6_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y1')) then
 write (*,*) ' reading vpcf-x5y1.dat'
 call pes_x5y1_vread (iun, pes_dir(1:l0)//'vpcf-x5y1.dat')
else
 pes_x5y1_vpc = cx_null ; allocate (pes_x5y1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2')) then
 write (*,*) ' reading vpcf-x4y2.dat'
 call pes_x4y2_vread (iun, pes_dir(1:l0)//'vpcf-x4y2.dat')
else
 pes_x4y2_vpc = cx_null ; allocate (pes_x4y2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y3')) then
 write (*,*) ' reading vpcf-x3y3.dat'
 call pes_x3y3_vread (iun, pes_dir(1:l0)//'vpcf-x3y3.dat')
else
 pes_x3y3_vpc = cx_null ; allocate (pes_x3y3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5z1')) then
 write (*,*) ' reading vpcf-x5z1.dat'
 call pes_x5z1_vread (iun, pes_dir(1:l0)//'vpcf-x5z1.dat')
else
 pes_x5z1_vpc = cx_null ; allocate (pes_x5z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1z1')) then
 write (*,*) ' reading vpcf-x4y1z1.dat'
 call pes_x4y1z1_vread (iun, pes_dir(1:l0)//'vpcf-x4y1z1.dat')
else
 pes_x4y1z1_vpc = cx_null ; allocate (pes_x4y1z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2z1')) then
 write (*,*) ' reading vpcf-x3y2z1.dat'
 call pes_x3y2z1_vread (iun, pes_dir(1:l0)//'vpcf-x3y2z1.dat')
else
 pes_x3y2z1_vpc = cx_null ; allocate (pes_x3y2z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y3z1')) then
 write (*,*) ' reading vpcf-x2y3z1.dat'
 call pes_x2y3z1_vread (iun, pes_dir(1:l0)//'vpcf-x2y3z1.dat')
else
 pes_x2y3z1_vpc = cx_null ; allocate (pes_x2y3z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1z2')) then
 write (*,*) ' reading vpcf-x3y1z2.dat'
 call pes_x3y1z2_vread (iun, pes_dir(1:l0)//'vpcf-x3y1z2.dat')
else
 pes_x3y1z2_vpc = cx_null ; allocate (pes_x3y1z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2z2')) then
 write (*,*) ' reading vpcf-x2y2z2.dat'
 call pes_x2y2z2_vread (iun, pes_dir(1:l0)//'vpcf-x2y2z2.dat')
else
 pes_x2y2z2_vpc = cx_null ; allocate (pes_x2y2z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y3z2')) then
 write (*,*) ' reading vpcf-x1y3z2.dat'
 call pes_x1y3z2_vread (iun, pes_dir(1:l0)//'vpcf-x1y3z2.dat')
else
 pes_x1y3z2_vpc = cx_null ; allocate (pes_x1y3z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3z3')) then
 write (*,*) ' reading vpcf-x3z3.dat'
 call pes_x3z3_vread (iun, pes_dir(1:l0)//'vpcf-x3z3.dat')
else
 pes_x3z3_vpc = cx_null ; allocate (pes_x3z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y1z3')) then
 write (*,*) ' reading vpcf-x2y1z3.dat'
 call pes_x2y1z3_vread (iun, pes_dir(1:l0)//'vpcf-x2y1z3.dat')
else
 pes_x2y1z3_vpc = cx_null ; allocate (pes_x2y1z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y2z3')) then
 write (*,*) ' reading vpcf-x1y2z3.dat'
 call pes_x1y2z3_vread (iun, pes_dir(1:l0)//'vpcf-x1y2z3.dat')
else
 pes_x1y2z3_vpc = cx_null ; allocate (pes_x1y2z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'y3z3')) then
 write (*,*) ' reading vpcf-y3z3.dat'
 call pes_y3z3_vread (iun, pes_dir(1:l0)//'vpcf-y3z3.dat')
else
 pes_y3z3_vpc = cx_null ; allocate (pes_y3z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5u1')) then
 write (*,*) ' reading vpcf-x5u1.dat'
 call pes_x5u1_vread (iun, pes_dir(1:l0)//'vpcf-x5u1.dat')
else
 pes_x5u1_vpc = cx_null ; allocate (pes_x5u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1u1')) then
 write (*,*) ' reading vpcf-x4y1u1.dat'
 call pes_x4y1u1_vread (iun, pes_dir(1:l0)//'vpcf-x4y1u1.dat')
else
 pes_x4y1u1_vpc = cx_null ; allocate (pes_x4y1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2u1')) then
 write (*,*) ' reading vpcf-x3y2u1.dat'
 call pes_x3y2u1_vread (iun, pes_dir(1:l0)//'vpcf-x3y2u1.dat')
else
 pes_x3y2u1_vpc = cx_null ; allocate (pes_x3y2u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4z1u1')) then
 write (*,*) ' reading vpcf-x4z1u1.dat'
 call pes_x4z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x4z1u1.dat')
else
 pes_x4z1u1_vpc = cx_null ; allocate (pes_x4z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1z1u1')) then
 write (*,*) ' reading vpcf-x3y1z1u1.dat'
 call pes_x3y1z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x3y1z1u1.dat')
else
 pes_x3y1z1u1_vpc = cx_null ; allocate (pes_x3y1z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2z1u1')) then
 write (*,*) ' reading vpcf-x2y2z1u1.dat'
 call pes_x2y2z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x2y2z1u1.dat')
else
 pes_x2y2z1u1_vpc = cx_null ; allocate (pes_x2y2z1u1_vcf(0:-1))
endif
! Read vpcf data for seven-body systems.
if (cx_substr(sys(1:l1),'x7')) then
 write (*,*) ' reading vpcf-x7.dat'
 call pes_x7_vread (iun, pes_dir(1:l0)//'vpcf-x7.dat')
else
 pes_x7_vpc = cx_null ; allocate (pes_x7_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6y1')) then
 write (*,*) ' reading vpcf-x6y1.dat'
 call pes_x6y1_vread (iun, pes_dir(1:l0)//'vpcf-x6y1.dat')
else
 pes_x6y1_vpc = cx_null ; allocate (pes_x6y1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y2')) then
 write (*,*) ' reading vpcf-x5y2.dat'
 call pes_x5y2_vread (iun, pes_dir(1:l0)//'vpcf-x5y2.dat')
else
 pes_x5y2_vpc = cx_null ; allocate (pes_x5y2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y3')) then
 write (*,*) ' reading vpcf-x4y3.dat'
 call pes_x4y3_vread (iun, pes_dir(1:l0)//'vpcf-x4y3.dat')
else
 pes_x4y3_vpc = cx_null ; allocate (pes_x4y3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y1z1')) then
 write (*,*) ' reading vpcf-x5y1z1.dat'
 call pes_x5y1z1_vread (iun, pes_dir(1:l0)//'vpcf-x5y1z1.dat')
else
 pes_x5y1z1_vpc = cx_null ; allocate (pes_x5y1z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2z1')) then
 write (*,*) ' reading vpcf-x4y2z1.dat'
 call pes_x4y2z1_vread (iun, pes_dir(1:l0)//'vpcf-x4y2z1.dat')
else
 pes_x4y2z1_vpc = cx_null ; allocate (pes_x4y2z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y3z1')) then
 write (*,*) ' reading vpcf-x3y3z1.dat'
 call pes_x3y3z1_vread (iun, pes_dir(1:l0)//'vpcf-x3y3z1.dat')
else
 pes_x3y3z1_vpc = cx_null ; allocate (pes_x3y3z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2z2')) then
 write (*,*) ' reading vpcf-x3y2z2.dat'
 call pes_x3y2z2_vread (iun, pes_dir(1:l0)//'vpcf-x3y2z2.dat')
else
 pes_x3y2z2_vpc = cx_null ; allocate (pes_x3y2z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y3z2')) then
 write (*,*) ' reading vpcf-x2y3z2.dat'
 call pes_x2y3z2_vread (iun, pes_dir(1:l0)//'vpcf-x2y3z2.dat')
else
 pes_x2y3z2_vpc = cx_null ; allocate (pes_x2y3z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y1z3')) then
 write (*,*) ' reading vpcf-x3y1z3.dat'
 call pes_x3y1z3_vread (iun, pes_dir(1:l0)//'vpcf-x3y1z3.dat')
else
 pes_x3y1z3_vpc = cx_null ; allocate (pes_x3y1z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y2z3')) then
 write (*,*) ' reading vpcf-x2y2z3.dat'
 call pes_x2y2z3_vread (iun, pes_dir(1:l0)//'vpcf-x2y2z3.dat')
else
 pes_x2y2z3_vpc = cx_null ; allocate (pes_x2y2z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x1y3z3')) then
 write (*,*) ' reading vpcf-x1y3z3.dat'
 call pes_x1y3z3_vread (iun, pes_dir(1:l0)//'vpcf-x1y3z3.dat')
else
 pes_x1y3z3_vpc = cx_null ; allocate (pes_x1y3z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6u1')) then
 write (*,*) ' reading vpcf-x6u1.dat'
 call pes_x6u1_vread (iun, pes_dir(1:l0)//'vpcf-x6u1.dat')
else
 pes_x6u1_vpc = cx_null ; allocate (pes_x6u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y1u1')) then
 write (*,*) ' reading vpcf-x5y1u1.dat'
 call pes_x5y1u1_vread (iun, pes_dir(1:l0)//'vpcf-x5y1u1.dat')
else
 pes_x5y1u1_vpc = cx_null ; allocate (pes_x5y1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2u1')) then
 write (*,*) ' reading vpcf-x4y2u1.dat'
 call pes_x4y2u1_vread (iun, pes_dir(1:l0)//'vpcf-x4y2u1.dat')
else
 pes_x4y2u1_vpc = cx_null ; allocate (pes_x4y2u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5z1u1')) then
 write (*,*) ' reading vpcf-x5z1u1.dat'
 call pes_x5z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x5z1u1.dat')
else
 pes_x5z1u1_vpc = cx_null ; allocate (pes_x5z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y1z1u1')) then
 write (*,*) ' reading vpcf-x4y1z1u1.dat'
 call pes_x4y1z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x4y1z1u1.dat')
else
 pes_x4y1z1u1_vpc = cx_null ; allocate (pes_x4y1z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2z1u1')) then
 write (*,*) ' reading vpcf-x3y2z1u1.dat'
 call pes_x3y2z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x3y2z1u1.dat')
else
 pes_x3y2z1u1_vpc = cx_null ; allocate (pes_x3y2z1u1_vcf(0:-1))
endif
! Read vpcf data for eight-body systems.
if (cx_substr(sys(1:l1),'x7y1')) then
 write (*,*) ' reading vpcf-x7y1.dat'
 call pes_x7y1_vread (iun, pes_dir(1:l0)//'vpcf-x7y1.dat')
else
 pes_x7y1_vpc = cx_null ; allocate (pes_x7y1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6y2')) then
 write (*,*) ' reading vpcf-x6y2.dat'
 call pes_x6y2_vread (iun, pes_dir(1:l0)//'vpcf-x6y2.dat')
else
 pes_x6y2_vpc = cx_null ; allocate (pes_x6y2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y3')) then
 write (*,*) ' reading vpcf-x5y3.dat'
 call pes_x5y3_vread (iun, pes_dir(1:l0)//'vpcf-x5y3.dat')
else
 pes_x5y3_vpc = cx_null ; allocate (pes_x5y3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y4')) then
 write (*,*) ' reading vpcf-x4y4.dat'
 call pes_x4y4_vread (iun, pes_dir(1:l0)//'vpcf-x4y4.dat')
else
 pes_x4y4_vpc = cx_null ; allocate (pes_x4y4_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y2z1')) then
 write (*,*) ' reading vpcf-x5y2z1.dat'
 call pes_x5y2z1_vread (iun, pes_dir(1:l0)//'vpcf-x5y2z1.dat')
else
 pes_x5y2z1_vpc = cx_null ; allocate (pes_x5y2z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y3z1')) then
 write (*,*) ' reading vpcf-x4y3z1.dat'
 call pes_x4y3z1_vread (iun, pes_dir(1:l0)//'vpcf-x4y3z1.dat')
else
 pes_x4y3z1_vpc = cx_null ; allocate (pes_x4y3z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2z2')) then
 write (*,*) ' reading vpcf-x4y2z2.dat'
 call pes_x4y2z2_vread (iun, pes_dir(1:l0)//'vpcf-x4y2z2.dat')
else
 pes_x4y2z2_vpc = cx_null ; allocate (pes_x4y2z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y3z2')) then
 write (*,*) ' reading vpcf-x3y3z2.dat'
 call pes_x3y3z2_vread (iun, pes_dir(1:l0)//'vpcf-x3y3z2.dat')
else
 pes_x3y3z2_vpc = cx_null ; allocate (pes_x3y3z2_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y2z3')) then
 write (*,*) ' reading vpcf-x3y2z3.dat'
 call pes_x3y2z3_vread (iun, pes_dir(1:l0)//'vpcf-x3y2z3.dat')
else
 pes_x3y2z3_vpc = cx_null ; allocate (pes_x3y2z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x2y3z3')) then
 write (*,*) ' reading vpcf-x2y3z3.dat'
 call pes_x2y3z3_vread (iun, pes_dir(1:l0)//'vpcf-x2y3z3.dat')
else
 pes_x2y3z3_vpc = cx_null ; allocate (pes_x2y3z3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6y1u1')) then
 write (*,*) ' reading vpcf-x6y1u1.dat'
 call pes_x6y1u1_vread (iun, pes_dir(1:l0)//'vpcf-x6y1u1.dat')
else
 pes_x6y1u1_vpc = cx_null ; allocate (pes_x6y1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6z1u1')) then
 write (*,*) ' reading vpcf-x6z1u1.dat'
 call pes_x6z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x6z1u1.dat')
else
 pes_x6z1u1_vpc = cx_null ; allocate (pes_x6z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y1z1u1')) then
 write (*,*) ' reading vpcf-x5y1z1u1.dat'
 call pes_x5y1z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x5y1z1u1.dat')
else
 pes_x5y1z1u1_vpc = cx_null ; allocate (pes_x5y1z1u1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y2z1u1')) then
 write (*,*) ' reading vpcf-x4y2z1u1.dat'
 call pes_x4y2z1u1_vread (iun, pes_dir(1:l0)//'vpcf-x4y2z1u1.dat')
else
 pes_x4y2z1u1_vpc = cx_null ; allocate (pes_x4y2z1u1_vcf(0:-1))
endif
! Read vpcf data for nine-body systems.
if (cx_substr(sys(1:l1),'x6y3')) then
 write (*,*) ' reading vpcf-x6y3.dat'
 call pes_x6y3_vread (iun, pes_dir(1:l0)//'vpcf-x6y3.dat')
else
 pes_x6y3_vpc = cx_null ; allocate (pes_x6y3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y4')) then
 write (*,*) ' reading vpcf-x5y4.dat'
 call pes_x5y4_vread (iun, pes_dir(1:l0)//'vpcf-x5y4.dat')
else
 pes_x5y4_vpc = cx_null ; allocate (pes_x5y4_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x5y3z1')) then
 write (*,*) ' reading vpcf-x5y3z1.dat'
 call pes_x5y3z1_vread (iun, pes_dir(1:l0)//'vpcf-x5y3z1.dat')
else
 pes_x5y3z1_vpc = cx_null ; allocate (pes_x5y3z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y4z1')) then
 write (*,*) ' reading vpcf-x4y4z1.dat'
 call pes_x4y4z1_vread (iun, pes_dir(1:l0)//'vpcf-x4y4z1.dat')
else
 pes_x4y4z1_vpc = cx_null ; allocate (pes_x4y4z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x3y3z3')) then
 write (*,*) ' reading vpcf-x3y3z3.dat'
 call pes_x3y3z3_vread (iun, pes_dir(1:l0)//'vpcf-x3y3z3.dat')
else
 pes_x3y3z3_vpc = cx_null ; allocate (pes_x3y3z3_vcf(0:-1))
endif
! Read vpcf data for ten-body systems.
if (cx_substr(sys(1:l1),'x7y3')) then
 write (*,*) ' reading vpcf-x7y3.dat'
 call pes_x7y3_vread (iun, pes_dir(1:l0)//'vpcf-x7y3.dat')
else
 pes_x7y3_vpc = cx_null ; allocate (pes_x7y3_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x6y3z1')) then
 write (*,*) ' reading vpcf-x6y3z1.dat'
 call pes_x6y3z1_vread (iun, pes_dir(1:l0)//'vpcf-x6y3z1.dat')
else
 pes_x6y3z1_vpc = cx_null ; allocate (pes_x6y3z1_vcf(0:-1))
endif
if (cx_substr(sys(1:l1),'x4y4z2')) then
 write (*,*) ' reading vpcf-x4y4z2.dat'
 call pes_x4y4z2_vread (iun, pes_dir(1:l0)//'vpcf-x4y4z2.dat')
else
 pes_x4y4z2_vpc = cx_null ; allocate (pes_x4y4z2_vcf(0:-1))
endif
return
END SUBROUTINE pes1_vinit_xyz
