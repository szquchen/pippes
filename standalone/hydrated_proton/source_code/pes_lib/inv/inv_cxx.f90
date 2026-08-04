MODULE inv_cxx
!..use and access
use inv_core
implicit none
private
public :: cx_cut, cx_var, cx_write, operator(.eq.), cx_sort, cx_dim, &
  cx_substr, cx_charid
!..types
type, public :: cx_t
 integer :: dg, kx, lx
 real (kind=wp) :: ax, bx
end type
!..data
type (cx_t), parameter, public :: cx_null=cx_t(-1,0,0,0.0_wp,0.0_wp)
!..procedures
interface operator (.eq.)
 module procedure cx_equals
end interface
CONTAINS
include 'cx_cut.f90'
include 'cx_var.f90'
include 'cx_write.f90'
include 'cx_equals.f90'
include 'cx_sort.f90'
include 'cx_dim.f90'
include 'cx_substr.f90'
include 'cx_charid.f90'
END MODULE inv_cxx
