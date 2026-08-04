MODULE inv_bg32
!..use and access
use inv_wp
implicit none
private
public :: bg32_gens, bg32_prims, bg32_secs, bg32_base
!..data
integer, parameter, public :: &
  bg32_nr=6, bg32_ngrp=12, bg32_ngen=3, &
  bg32_dnpr(0:9) = (/ 0, 1, 3, 1, 0, 0, 1, 0, 0, 0 /), &
  bg32_npr(0:9) = (/ 0, 1, 4, 5, 5, 5, 6, 6, 6, 6 /), &
  bg32_dnsc(0:9) = (/ 1, 0, 0, 2, 3, 1, 1, 2, 1, 1 /), &
  bg32_nsc(0:9) = (/ 1, 1, 1, 3, 6, 7, 8, 10, 11, 12 /), &
  bg32_dnb(0:9) = (/ 1, 1, 4, 7, 16, 26, 50, 76, 126, 185 /), &
  bg32_nb(0:9) = (/ 1, 2, 6, 13, 29, 55, 105, 181, 307, 492 /)
!..procedures
CONTAINS
include 'bg32_gens.f90'
include 'bg32_prims.f90'
include 'bg32_secs.f90'
include 'bg32_base.f90'
END MODULE inv_bg32
