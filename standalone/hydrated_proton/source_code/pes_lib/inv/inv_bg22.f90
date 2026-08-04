MODULE inv_bg22
!..use and access
use inv_wp
implicit none
private
public :: bg22_gens, bg22_prims, bg22_secs, bg22_base
!..data
integer, parameter, public :: &
  bg22_nr=4, bg22_ngrp=4, bg22_ngen=2, &
  bg22_dnpr(0:9) = (/ 0, 1, 3, 0, 0, 0, 0, 0, 0, 0 /), &
  bg22_npr(0:9) = (/ 0, 1, 4, 4, 4, 4, 4, 4, 4, 4 /), &
  bg22_dnsc(0:9) = (/ 1, 0, 0, 1, 0, 0, 0, 0, 0, 0 /), &
  bg22_nsc(0:9) = (/ 1, 1, 1, 2, 2, 2, 2, 2, 2, 2 /), &
  bg22_dnb(0:9) = (/ 1, 1, 4, 5, 11, 14, 24, 30, 45, 55 /), &
  bg22_nb(0:9) = (/ 1, 2, 6, 11, 22, 36, 60, 90, 135, 190 /)
!..procedures
CONTAINS
include 'bg22_gens.f90'
include 'bg22_prims.f90'
include 'bg22_secs.f90'
include 'bg22_base.f90'
END MODULE inv_bg22
