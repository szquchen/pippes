MODULE inv_t3
!..use and access
use inv_wp
implicit none
private
public :: t3_prims, t3_secs, t3_base
!..data
integer, parameter, public :: &
  t3_nr=3, t3_ngrp=1, t3_ngen=0, &
  t3_dnpr(0:9) = (/ 0, 3, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  t3_npr(0:9) = (/ 0, 3, 3, 3, 3, 3, 3, 3, 3, 3 /), &
  t3_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  t3_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  t3_dnb(0:9) = (/ 1, 3, 6, 10, 15, 21, 28, 36, 45, 55 /), &
  t3_nb(0:9) = (/ 1, 4, 10, 20, 35, 56, 84, 120, 165, 220 /)
!..procedures
CONTAINS
include 't3_prims.f90'
include 't3_secs.f90'
include 't3_base.f90'
END MODULE inv_t3
