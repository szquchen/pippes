MODULE inv_bg63
!..use and access
use inv_wp
implicit none
private
public :: bg63_gens, bg63_prims, bg63_secs, bg63_base
!..data
integer, parameter, public :: &
  bg63_nr=6*3, bg63_ngrp=720*6, bg63_ngen=4, &
  bg63_dnpr(0:9) = (/ 0, 1, 3, 3, 4, 2, 2, 0, 0, 0 /), &
  bg63_npr(0:9) = (/ 0, 1, 4, 7, 11, 13, 15, 15, 15, 15 /), &
  bg63_dnpb(0:9) = (/ 1, 1, 4, 7, 17, 28, 58, 94, 173, 275 /), &
  bg63_npb(0:9) = (/ 1, 2, 6, 13, 30, 58, 116, 210, 383, 658 /), &
  bg63_dnsc(0:9) = (/ 1, 0, 0, 3, 8, 20, 53, 101, 220, 455 /), &
  bg63_nsc(0:9) = (/ 1, 1, 1, 4, 12, 32, 85, 186, 406, 861 /), &
  bg63_dnb(0:9) = (/ 1, 1, 4, 10, 28, 68, 184, 435, 1066, 2463 /), &
  bg63_nb(0:9) = (/ 1, 2, 6, 16, 44, 112, 296, 731, 1797, 4260 /)
! Note: there are also primaries at degrees 10, 12, and 15.
!..procedures
CONTAINS
include 'bg63_gens.f90'
include 'bg63_prims.f90'
include 'bg63_secs.f90'
include 'bg63_base.f90'
END MODULE inv_bg63
