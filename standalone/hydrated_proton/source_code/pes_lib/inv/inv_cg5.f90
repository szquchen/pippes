MODULE inv_cg5
!..use and access
use inv_wp
implicit none
private
public :: cg5_gens, cg5_prims, cg5_prib, cg5_secs, cg5_base
!..data
integer, parameter, private :: nk=5, nr=nk*(nk-1)/2
integer, parameter, public :: &
  cg5_nk=nk, cg5_nr=nr, cg5_ngrp=120, cg5_ngen=2, &
  cg5_dnpr(0:9) = (/ 0, 1, 2, 2, 2, 2, 1, 0, 0, 0 /), &
  cg5_npr(0:9) = (/ 0, 1, 3, 5, 7, 9, 10, 10, 10, 10 /), &
  cg5_dnpb(0:9) = (/ 1, 1, 3, 5, 10, 16, 28, 42, 68, 100 /), &
  cg5_npb(0:9) = (/ 1, 2, 5, 10, 20, 36, 64, 106, 174, 274 /), &
  cg5_dnsc(0:9) = (/ 1, 0, 0, 2, 5, 8, 15, 23, 33, 46 /), &
  cg5_nsc(0:9) = (/ 1, 1, 1, 3, 8, 16, 31, 54, 87, 133 /), &
  cg5_dnb(0:9) = (/ 1, 1, 3, 7, 17, 35, 76, 149, 291, 539 /), &
  cg5_nb(0:9) = (/ 1, 2, 5, 12, 29, 64, 140, 289, 580, 1119 /)
!..procedures
CONTAINS
include 'cg5_gens.f90'
include 'cg5_prims.f90'
include 'cg5_prib.f90'
include 'cg5_secs.f90'
include 'cg5_base.f90'
END MODULE inv_cg5
