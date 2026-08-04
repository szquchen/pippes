MODULE inv_bg72
!..use and access
use inv_wp
implicit none
private
public :: bg72_gens, bg72_prims, bg72_secs, bg72_base
!..data
integer, parameter, public :: &
  bg72_nr=14, bg72_ngrp=10080, bg72_ngen=3, &
  bg72_dnpr(0:9) = (/ 0, 1, 3, 2, 2, 1, 2, 1, 0, 0 /), &
  bg72_npr(0:9) = (/ 0, 1, 4, 6, 8, 9, 11, 12, 12, 12 /), &
  bg72_dnsc(0:9) = (/ 1, 0, 0, 1, 4, 6, 10, 15, 25, 34 /), &
  bg72_nsc(0:9) = (/ 1, 1, 1, 2, 6, 12, 22, 37, 62, 96 /), &
  bg72_dnb(0:9) = (/ 1, 1, 4, 7, 19, 35, 80, 149, 302, 549 /), &
  bg72_nb(0:9) = (/ 1, 2, 6, 13, 32, 67, 147, 296, 598, 1147 /)
!..procedures
CONTAINS
include 'bg72_gens.f90'
include 'bg72_prims.f90'
include 'bg72_secs.f90'
include 'bg72_base.f90'
END MODULE inv_bg72
