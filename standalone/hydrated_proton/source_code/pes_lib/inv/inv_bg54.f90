MODULE inv_bg54
!..use and access
use inv_wp
implicit none
private
public :: bg54_gens, bg54_prims, bg54_secs, bg54_base
!..data
integer, parameter, public :: &
  bg54_nr=5*4, bg54_ngrp=120*24, bg54_ngen=4, &
  bg54_dnpr(0:9) = (/ 0, 1, 3, 5, 5, 1, 1, 0, 0, 0 /), &
  bg54_npr(0:9) = (/ 0, 1, 4, 9, 14, 15, 16, 16, 16, 16 /), &
  bg54_dnpb(0:9) = (/ 1, 1, 4, 9, 20, 36, 77, 135, 248, 424 /), &
  bg54_npb(0:9) = (/ 1, 2, 6, 15, 35, 71, 148, 283, 531, 955 /), &
  bg54_dnsc(0:9) = (/ 1, 0, 0, 1, 12, 32, 83, 185, 480, 1121 /), &
  bg54_nsc(0:9) = (/ 1, 1, 1, 2, 14, 46, 129, 314, 794, 1915 /), &
  bg54_dnb(0:9) = (/ 1, 1, 4, 10, 33, 84, 249, 659, 1809, 4661 /), &
  bg54_nb(0:9) = (/ 1, 2, 6, 16, 49, 133, 382, 1041, 2850, 7511 /)
! Note: there are also primaries at degrees 10, 12, 15 and 20.
!..procedures
CONTAINS
include 'bg54_gens.f90'
include 'bg54_prims.f90'
include 'bg54_secs.f90'
include 'bg54_base.f90'
END MODULE inv_bg54
