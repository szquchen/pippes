MODULE inv_cg6
!..use and access
use inv_wp
implicit none
private
public :: cg6_gens, cg6_prims, cg6_prib, cg6_secs, cg6_base
!..data
integer, parameter, private :: nk=6, nr=nk*(nk-1)/2
integer, parameter, public :: &
  cg6_nk=nk, cg6_nr=nr, cg6_ngrp=720, cg6_ngen=2, &
  cg6_dnpr(0:9) = (/ 0, 1, 2, 4, 3, 3, 2, 0, 0, 0 /), &
  cg6_npr(0:9) = (/ 0, 1, 3, 7, 10, 13, 15, 15, 15, 15 /), &
  cg6_dnpb(0:9) = (/ 1, 1, 3, 7, 13, 24, 46, 76, 132, 218 /), &
  cg6_npb(0:9) = (/ 1, 2, 5, 12, 25, 49, 95, 171, 303, 521 /), &
  cg6_dnsc(0:9) = (/ 1, 0, 0, 1, 7, 18, 40, 81, 167, 328 /), &
  cg6_nsc(0:9) = (/ 1, 1, 1, 2, 9, 27, 67, 148, 315, 643 /), &
  cg6_dnb(0:9) = (/ 1, 1, 3, 8, 21, 52, 132, 313, 741, 1684 /), &
  cg6_nb(0:9) = (/ 1, 2, 5, 13, 34, 86, 218, 531, 1272, 2956 /)
!..procedures
CONTAINS
include 'cg6_gens.f90'
include 'cg6_prims.f90'
include 'cg6_prib.f90'
include 'cg6_secs.f90'
include 'cg6_base.f90'
END MODULE inv_cg6
