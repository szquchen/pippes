MODULE inv_ms22
!..use and access
use inv_wp
implicit none
private
public :: ms22_gens, ms22_prims, ms22_secs, ms22_base
!..data
integer, parameter, public :: &
  ms22_nr=4, ms22_ngrp=2, ms22_ngen=1, &
  ms22_dnpr(0:9) = (/ 0, 2, 2, 0, 0, 0, 0, 0, 0, 0 /), &
  ms22_npr(0:9) = (/ 0, 2, 4, 4, 4, 4, 4, 4, 4, 4 /), &
  ms22_dnsc(0:9) = (/ 1, 0, 1, 0, 0, 0, 0, 0, 0, 0 /), &
  ms22_nsc(0:9) = (/ 1, 1, 2, 2, 2, 2, 2, 2, 2, 2 /), &
  ms22_dnb(0:9) = (/ 1, 2, 6, 10, 19, 28, 44, 60, 85, 110 /), &
  ms22_nb(0:9) = (/ 1, 3, 9, 19, 38, 66, 110, 170, 255, 365 /)
!..procedures
CONTAINS
include 'ms22_gens.f90'
include 'ms22_prims.f90'
include 'ms22_secs.f90'
include 'ms22_base.f90'
END MODULE inv_ms22
