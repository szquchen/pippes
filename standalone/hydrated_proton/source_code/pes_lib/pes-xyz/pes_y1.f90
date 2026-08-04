MODULE pes_y1
!..use and access
use inv, wp=>inv_wp
use pes0
private
public :: pes_y1_read, pes_y1_pot, pes_y1_add, &
  pes_y1_vread, pes_y1_vfun, pes_y1_vadd
!..data
save
integer, parameter, public :: &
  pes_y1_nki(0:0)=(/1/), pes_y1_nk=1, &
  pes_y1_nb=1, pes_y1_nvb=1
character (len=*), parameter, public :: &
  pes_y1_sysall='y1'
real (kind=wp), public :: pes_y1_cf, pes_y1_vcf(0:0)
!..procedures
CONTAINS
include 'pes_y1_read.f90'
include 'pes_y1_pot.f90'
include 'pes_y1_add.f90'
include 'pes_y1_vread.f90'
include 'pes_y1_vfun.f90'
include 'pes_y1_vadd.f90'
END MODULE pes_y1
