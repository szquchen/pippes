MODULE inv_bg53
!..use and access
use inv_wp
implicit none
private
public :: bg53_gens, bg53_prims, bg53_secs, bg53_base
!..data
integer, parameter, public :: &
  bg53_nr=5*3, bg53_ngrp=120*6, bg53_ngen=4, &
  bg53_dnpr(0:9) = (/ 0, 1, 3, 3, 3, 1, 1, 0, 0, 0 /), &
  bg53_npr(0:9) = (/ 0, 1, 4, 7, 10, 11, 12, 12, 12, 12 /), &
  bg53_dnpb(0:9) = (/ 1, 1, 4, 7, 16, 26, 52, 82, 145, 224 /), &
  bg53_npb(0:9) = (/ 1, 2, 6, 13, 29, 55, 107, 189, 334, 558 /), &
  bg53_dnsc(0:9) = (/ 1, 0, 0, 3, 9, 21, 47, 87, 179, 342 /), &
  bg53_nsc(0:9) = (/ 1, 1, 1, 4, 13, 34, 81, 168, 347, 689 /), &
  bg53_dnb(0:9) = (/ 1, 1, 4, 10, 28, 68, 177, 411, 968, 2148 /), &
  bg53_nb(0:9) = (/ 1, 2, 6, 16, 44, 112, 289, 700, 1668, 3816 /)
! Note: there are also primaries at degrees 10, 12, and 15.
!..procedures
CONTAINS
include 'bg53_gens.f90'
include 'bg53_prims.f90'
include 'bg53_secs.f90'
include 'bg53_base.f90'
END MODULE inv_bg53
