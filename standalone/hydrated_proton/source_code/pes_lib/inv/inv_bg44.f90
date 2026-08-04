MODULE inv_bg44
!..use and access
use inv_wp
implicit none
private
public :: bg44_gens, bg44_prims, bg44_secs, bg44_base
!..data
integer, parameter, public :: &
  bg44_nr=4*4, bg44_ngrp=24*24, bg44_ngen=4, &
  bg44_dnpr(0:9) = (/ 0, 1, 3, 5, 5, 0, 1, 0, 0, 0 /), &
  bg44_npr(0:9) = (/ 0, 1, 4, 9, 14, 14, 15, 15, 15, 15 /), &
  bg44_dnpb(0:9) = (/ 1, 1, 4, 9, 20, 35, 76, 131, 239, 404 /), &
  bg44_npb(0:9) = (/ 1, 2, 6, 15, 35, 70, 146, 277, 516, 920 /), &
  bg44_dnsc(0:9) = (/ 1, 0, 0, 1, 12, 27, 64, 118, 272, 532 /), &
  bg44_nsc(0:9) = (/ 1, 1, 1, 2, 14, 41, 105, 223, 495, 1027 /), &
  bg44_dnb(0:9) = (/ 1, 1, 4, 10, 33, 78, 224, 549, 1403, 3292 /), &
  bg44_nb(0:9) = (/ 1, 2, 6, 16, 49, 127, 351, 900, 2303, 5595 /)
! Note: there is also a primary at degree 12.
!..procedures
CONTAINS
include 'bg44_gens.f90'
include 'bg44_prims.f90'
include 'bg44_secs.f90'
include 'bg44_base.f90'
END MODULE inv_bg44
