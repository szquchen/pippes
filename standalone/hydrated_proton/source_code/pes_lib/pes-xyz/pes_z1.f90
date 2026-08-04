MODULE pes_z1
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_z1_read, pes_z1_pot, pes_z1_add, &
  pes_z1_vread, pes_z1_vfun, pes_z1_vadd
!..data
save
integer, parameter, public :: &
  pes_z1_nki(0:0)=(/1/), pes_z1_nk=1, &
  pes_z1_nb=1, pes_z1_nvb=1
character (len=*), parameter, public :: &
  pes_z1_sysall='z1'
real (kind=wp), public :: pes_z1_cf, pes_z1_vcf(0:0)
!..procedures
CONTAINS
include 'pes_z1_read.f90'
include 'pes_z1_pot.f90'
include 'pes_z1_add.f90'
include 'pes_z1_vread.f90'
include 'pes_z1_vfun.f90'
include 'pes_z1_vadd.f90'
END MODULE pes_z1
