MODULE inv_bg73
!..use and access
use inv_wp
implicit none
private
public :: bg73_gens, bg73_prims, bg73_secs, bg73_base
!..data
integer, parameter, public :: &
  bg73_nr=21, bg73_ngrp=5040*6, bg73_ngen=4, &
  bg73_dnpr(0:9) = (/ 0, 1, 3, 3, 4, 2, 2, 1, 0, 0 /), &
  bg73_npr(0:9) = (/ 0, 1, 4, 7, 11, 13, 15, 16, 16, 16 /), &
  bg73_dnsc(0:9) = (/ 1, 0, 0, 3, 8, 20, 53, 108, 244, 531 /), &
  bg73_nsc(0:9) = (/ 1, 1, 1, 4, 12, 32, 85, 193, 437, 968 /), &
  bg73_dnb(0:9) = (/ 1, 1, 4, 10, 28, 68, 184, 443, 1098, 2595 /), &
  bg73_nb(0:9) = (/ 1, 2, 6, 16, 44, 112, 296, 739, 1837, 4432 /)
!..procedures
CONTAINS
include 'bg73_gens.f90'
include 'bg73_prims.f90'
include 'bg73_secs.f90'
include 'bg73_base.f90'
END MODULE inv_bg73
