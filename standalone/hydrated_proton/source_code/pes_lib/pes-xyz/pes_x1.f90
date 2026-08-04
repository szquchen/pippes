MODULE pes_x1
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_x1_read, pes_x1_pot, pes_x1_add, &
  pes_x1_vread, pes_x1_vfun, pes_x1_vadd
!..data
save
integer, parameter, public :: &
  pes_x1_nki(0:0)=(/1/), pes_x1_nk=1, &
  pes_x1_nb=1, pes_x1_nvb=1
character (len=*), parameter, public :: &
  pes_x1_sysall='x1'
real (kind=wp), public :: pes_x1_cf, pes_x1_vcf(0:0)
!..procedures
CONTAINS
include 'pes_x1_read.f90'
include 'pes_x1_pot.f90'
include 'pes_x1_add.f90'
include 'pes_x1_vread.f90'
include 'pes_x1_vfun.f90'
include 'pes_x1_vadd.f90'
END MODULE pes_x1
