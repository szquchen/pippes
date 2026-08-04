MODULE pes_x3
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_x2
private
public :: pes_x3_read, pes_x3_pot, pes_x3_add, pes_x3_getcf, &
  pes_x3_vread, pes_x3_vfun, pes_x3_vadd, pes_x3_getvcf
!..data
save
integer, parameter, public :: &
  pes_x3_nki(0:0)=(/3/), pes_x3_nk=3, &
  pes_x3_nb(-1:size(cx_nb3(-1:))-2)=cx_nb3(-1:), &
  pes_x3_nvb(-1:size(cxv_nb3(-1:))-2)=cxv_nb3(-1:)
character (len=*), parameter, public :: &
  pes_x3_sysall='x1 x2 x3'
type (cx_t), public :: &
  pes_x3_pc = cx_null, &
  pes_x3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x3_cf(:), &
  pes_x3_vcf(:)
!..procedures
CONTAINS
include 'pes_x3_read.f90'
include 'pes_x3_pot.f90'
include 'pes_x3_add.f90'
include 'pes_x3_getcf.f90'
include 'pes_x3_vread.f90'
include 'pes_x3_vfun.f90'
include 'pes_x3_vadd.f90'
include 'pes_x3_getvcf.f90'
END MODULE pes_x3
