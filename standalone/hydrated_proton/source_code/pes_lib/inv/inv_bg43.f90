MODULE inv_bg43
!..use and access
use inv_wp
implicit none
private
public :: bg43_gens, bg43_prims, bg43_secs, bg43_base
!..data
integer, parameter, public :: &
  bg43_nr=12, bg43_ngrp=24*6, bg43_ngen=4, &
  bg43_dnpr(0:9) = (/ 0, 1, 3, 3, 3, 0, 1, 0, 0, 0 /), &
  bg43_npr(0:9) = (/ 0, 1, 4, 7, 10, 10, 11, 11, 11, 11 /), &
  bg43_dnpb(0:9) = (/ 1, 1, 4, 7, 16, 25, 51, 78, 138, 208 /), &
  bg43_npb(0:9) = (/ 1, 2, 6, 13, 29, 54, 105, 183, 321, 529 /), &
  bg43_dnsc(0:9) = (/ 1, 0, 0, 3, 9, 17, 34, 52, 94, 148 /), &
  bg43_nsc(0:9) = (/ 1, 1, 1, 4, 13, 30, 64, 116, 210, 358 /), &
  bg43_dnb(0:9) = (/ 1, 1, 4, 10, 28, 63, 159, 343, 758, 1546 /), &
  bg43_nb(0:9) = (/ 1, 2, 6, 16, 44, 107, 266, 609, 1367, 2913 /)
! Note: there is also a primary at degree 12.
!..procedures
CONTAINS
include 'bg43_gens.f90'
include 'bg43_prims.f90'
include 'bg43_secs.f90'
include 'bg43_base.f90'
END MODULE inv_bg43
