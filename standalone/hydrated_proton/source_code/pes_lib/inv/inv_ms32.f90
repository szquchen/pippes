MODULE inv_ms32
!..use and access
use inv_wp
implicit none
private
public :: ms32_gens, ms32_prims, ms32_secs, ms32_base
!..data
integer, parameter, public :: &
  ms32_nr=6, ms32_ngrp=6, ms32_ngen=2, &
  ms32_dnpr(0:9) = (/ 0, 2, 2, 2, 0, 0, 0, 0, 0, 0 /), &
  ms32_npr(0:9) = (/ 0, 2, 4, 6, 6, 6, 6, 6, 6, 6 /), &
  ms32_dnsc(0:9) = (/ 1, 0, 1, 2, 1, 0, 1, 0, 0, 0 /), &
  ms32_nsc(0:9) = (/ 1, 1, 2, 4, 5, 5, 6, 6, 6, 6 /), &
  ms32_dnb(0:9) = (/ 1, 2, 6, 14, 28, 52, 93, 152, 242, 370 /), &
  ms32_nb(0:9) = (/ 1, 3, 9, 23, 51, 103, 196, 348, 590, 960 /)
!..procedures
CONTAINS
include 'ms32_gens.f90'
include 'ms32_prims.f90'
include 'ms32_secs.f90'
include 'ms32_base.f90'
END MODULE inv_ms32
