MODULE pes_y3
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_y1
use pes_y2
private
public :: pes_y3_read, pes_y3_pot, pes_y3_add, pes_y3_getcf, &
  pes_y3_vread, pes_y3_vfun, pes_y3_vadd, pes_y3_getvcf
!..data
save
integer, parameter, public :: &
  pes_y3_nki(0:0)=(/3/), pes_y3_nk=3, &
  pes_y3_nb(-1:size(cx_nb3(-1:))-2)=cx_nb3(-1:), &
  pes_y3_nvb(-1:size(cxv_nb3(-1:))-2)=cxv_nb3(-1:)
character (len=*), parameter, public :: &
  pes_y3_sysall='y1 y2 y3'
type (cx_t), public :: &
  pes_y3_pc = cx_null, &
  pes_y3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_y3_cf(:), &
  pes_y3_vcf(:)
!..procedures
CONTAINS
include 'pes_y3_read.f90'
include 'pes_y3_pot.f90'
include 'pes_y3_add.f90'
include 'pes_y3_getcf.f90'
include 'pes_y3_vread.f90'
include 'pes_y3_vfun.f90'
include 'pes_y3_vadd.f90'
include 'pes_y3_getvcf.f90'
END MODULE pes_y3
