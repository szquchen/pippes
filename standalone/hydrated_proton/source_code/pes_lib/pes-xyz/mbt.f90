MODULE mbt
!..use and access
use inv, wp=>inv_wp
implicit none
private
public :: mbt_read
!..types
type, public :: mbt_t
 integer :: mode, nd, nb, gpi, mxd
 integer, allocatable :: nkj(:)
 real (kind=wp), allocatable :: gpr(:), cf(:)
end type mbt_t
!..data
!..procedures
CONTAINS
END MODULE mbt
