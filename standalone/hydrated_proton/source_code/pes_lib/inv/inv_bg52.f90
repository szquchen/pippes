MODULE inv_bg52
!..use and access
use inv_wp
implicit none
private
public :: bg52_gens, bg52_prims, bg52_secs, bg52_base
!..data
integer, parameter, public :: &
  bg52_nr=10, bg52_ngrp=240, bg52_ngen=3, &
  bg52_dnpr(0:9) = (/ 0, 1, 3, 1, 2, 1, 1, 0, 0, 0 /), &
  bg52_npr(0:9) = (/ 0, 1, 4, 5, 7, 8, 9, 9, 9, 9 /), &
  bg52_dnsc(0:9) = (/ 1, 0, 0, 2, 4, 6, 9, 11, 17, 21 /), &
  bg52_nsc(0:9) = (/ 1, 1, 1, 3, 7, 13, 22, 33, 50, 71 /), &
  bg52_dnb(0:9) = (/ 1, 1, 4, 7, 19, 35, 76, 136, 263, 452 /), &
  bg52_nb(0:9) = (/ 1, 2, 6, 13, 32, 67, 143, 279, 542, 994 /)
!..procedures
CONTAINS
include 'bg52_gens.f90'
include 'bg52_prims.f90'
include 'bg52_secs.f90'
include 'bg52_base.f90'
END MODULE inv_bg52
