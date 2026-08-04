MODULE pes_u1
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_u1_read, pes_u1_pot, pes_u1_add, &
  pes_u1_vread, pes_u1_vfun, pes_u1_vadd
!..data
save
integer, parameter, public :: &
  pes_u1_nki(0:0)=(/1/), pes_u1_nk=1, &
  pes_u1_nb=1, pes_u1_nvb=1
character (len=*), parameter, public :: &
  pes_u1_sysall='u1'
real (kind=wp), public :: pes_u1_cf, pes_u1_vcf(0:0)
!..procedures
CONTAINS
include 'pes_u1_read.f90'
include 'pes_u1_pot.f90'
include 'pes_u1_add.f90'
include 'pes_u1_vread.f90'
include 'pes_u1_vfun.f90'
include 'pes_u1_vadd.f90'
END MODULE pes_u1
