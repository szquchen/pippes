MODULE mbe
!..use and access
use inv, wp=>inv_wp
implicit none
private
public :: mbe_read
!..types
type, public :: mbe_t
 integer :: nkk, mode, nb, vti
 integer, allocatable :: id(:)
 real (kind=wp), allocatable :: scal(:,:), vtr(:)
 character (len=255) :: dnam
 type (mbt_t), allocatable :: p(:)
end type mbe_t
!..data
!..procedures
CONTAINS
END MODULE mbe
