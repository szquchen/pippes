MODULE inv_bg42
!..use and access
use inv_wp
implicit none
private
public :: bg42_gens, bg42_prims, bg42_secs, bg42_base
!..data
integer, parameter, public :: &
  bg42_nr=8, bg42_ngrp=48, bg42_ngen=3, &
  bg42_dnpr(0:9) = (/ 0, 1, 3, 1, 2, 0, 1, 0, 0, 0 /), &
  bg42_npr(0:9) = (/ 0, 1, 4, 5, 7, 7, 8, 8, 8, 8 /), &
  bg42_dnsc(0:9) = (/ 1, 0, 0, 2, 4, 4, 4, 6, 6, 6 /), &
  bg42_nsc(0:9) = (/ 1, 1, 1, 3, 7, 11, 15, 21, 27, 33 /), &
  bg42_dnb(0:9) = (/ 1, 1, 4, 7, 19, 32, 68, 114, 210, 336 /), &
  bg42_nb(0:9) = (/ 1, 2, 6, 13, 32, 64, 132, 246, 456, 792 /)
!..procedures
CONTAINS
include 'bg42_gens.f90'
include 'bg42_prims.f90'
include 'bg42_secs.f90'
include 'bg42_base.f90'
END MODULE inv_bg42
