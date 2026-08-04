MODULE inv_mg22
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg22_prims, mg22_prib, mg22_isecs, mg22_secs, &
  mg22_base
!..data
integer, parameter, private :: nkk=2, nk=4, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2/)
integer, parameter, public :: &
  mg22_id=9, &
  mg22_nkk=nkk, mg22_nk=nk, mg22_nr=nr, mg22_ngrp=4, &
  mg22_nkj(0:nkk-1)=nkj, &
  mg22_dnpr(0:19) = (/ 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg22_npr(0:19) = (/ 0, 3, 6, 6, 6, 6, 6, 6, 6, 6, &
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6 /), &
  mg22_dnpb(0:19) = (/ 1, 3, 9, 19, 39, 69, 119, 189, 294, 434, &
    630, 882, 1218, 1638, 2178, 2838, 3663, 4653, 5863, 7293 /), &
  mg22_npb(0:19) = (/ 1, 4, 13, 32, 71, 140, 259, 448, 742, 1176, &
    1806, 2688, 3906, 5544, 7722, 10560, 14223, 18876, 24739, 32032 /), &
  mg22_dnsc(0:19) = (/ 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg22_nsc(0:19) = (/ 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, &
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2 /), &
  mg22_dnb(0:19) = (/ 1, 3, 9, 20, 42, 78, 138, 228, 363, 553, &
    819, 1176, 1652, 2268, 3060, 4056, 5301, 6831, 8701, 10956 /), &
  mg22_nb(0:19) = (/ 1, 4, 13, 33, 75, 153, 291, 519, 882, 1435, &
    2254, 3430, 5082, 7350, 10410, 14466, 19767, 26598, 35299, 46255 /)
!..procedures
CONTAINS
include 'mg22_prims.f90'
include 'mg22_prib.f90'
include 'mg22_isecs.f90'
include 'mg22_secs.f90'
include 'mg22_base.f90'
END MODULE inv_mg22
