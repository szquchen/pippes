MODULE inv_mg7111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg7111_prims, mg7111_prib, mg7111_isecs, mg7111_secs, &
  mg7111_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/7,1,1,1/)
integer, parameter, public :: &
  mg7111_id=111, &
  mg7111_nkk=nkk, mg7111_nk=nk, mg7111_nr=nr, mg7111_ngrp=5040, &
  mg7111_nkj(0:nkk-1)=nkj, &
  mg7111_dnpr(0:9) = (/ 0, 7, 5, 8, 6, 6, 5, 6, 0, 0 /), &
  mg7111_npr(0:9) = (/ 0, 7, 12, 20, 26, 32, 37, 43, 43, 43 /), &
  mg7111_dnpb(0:9) = (/ 1, 7, 33, 127, 427, 1299, 3662, 9704, &
    24423, 58829 /), &
  mg7111_npb(0:9) = (/ 1, 8, 41, 168, 595, 1894, 5556, 15260, &
    39683, 98512 /), &
  mg7111_dnsc(0:9) = (/ 1, 0, 6, 28, 135, 613, 2876, 13005, &
    57109, 240304 /), &
  mg7111_nsc(0:9) = (/ 1, 1, 7, 35, 170, 783, 3659, 16664, &
    73773, 314077 /), &
  mg7111_dnb(0:9) = (/ 1, 7, 39, 197, 956, 4543, 21402, 99965, &
    461315, 2091189 /), &
  mg7111_nb(0:9) = (/ 1, 8, 47, 244, 1200, 5743, 27145, 127110, &
    588425, 2679614 /)
!..procedures
CONTAINS
include 'mg7111_prims.f90'
include 'mg7111_prib.f90'
include 'mg7111_isecs.f90'
include 'mg7111_secs.f90'
include 'mg7111_base.f90'
END MODULE inv_mg7111
