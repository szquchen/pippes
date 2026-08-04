MODULE inv_bg33
!..use and access
use inv_wp
implicit none
private
public :: bg33_gens, bg33_prims, bg33_secs, bg33_base
!..data
integer, parameter, public :: &
  bg33_nr=9, bg33_ngrp=36, bg33_ngen=4, &
  bg33_dnpr(0:9) = (/ 0, 1, 3, 3, 1, 0, 1, 0, 0, 0 /), &
  bg33_npr(0:9) = (/ 0, 1, 4, 7, 8, 8, 9, 9, 9, 9 /), &
  bg33_dnsc(0:9) = (/ 1, 0, 0, 3, 7, 9, 13, 15, 20, 22 /), &
  bg33_nsc(0:9) = (/ 1, 1, 1, 4, 11, 20, 33, 48, 68, 90 /), &
  bg33_dnb(0:9) = (/ 1, 1, 4, 10, 24, 51, 114, 219, 424, 768 /), &
  bg33_nb(0:9) = (/ 1, 2, 6, 16, 40, 91, 205, 424, 848, 1616 /)
!..procedures
CONTAINS
include 'bg33_gens.f90'
include 'bg33_prims.f90'
include 'bg33_secs.f90'
include 'bg33_base.f90'
END MODULE inv_bg33
