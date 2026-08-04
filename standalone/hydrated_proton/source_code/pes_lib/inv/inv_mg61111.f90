MODULE inv_mg61111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg61111_prims, mg61111_prib, mg61111_isecs, mg61111_secs, &
  mg61111_base
!..data
integer, parameter, private :: nkk=5, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,1,1,1,1/)
integer, parameter, public :: &
  mg61111_id=120, &
  mg61111_nkk=nkk, mg61111_nk=nk, mg61111_nr=nr, mg61111_ngrp=720, &
  mg61111_nkj(0:nkk-1)=nkj, &
  mg61111_dnpr(0:9) = (/ 0, 11, 6, 8, 7, 7, 6, 0, 0, 0 /), &
  mg61111_npr(0:9) = (/ 0, 11, 17, 25, 32, 39, 45, 45, 45, 45 /), &
  mg61111_dnpb(0:9) = (/ 1, 11, 72, 360, 1513, 5610, 18895, 58918, &
    172363, 477712 /), &
  mg61111_npb(0:9) = (/ 1, 12, 84, 444, 1957, 7567, 26462, 85380, &
    257743, 735455 /), &
  mg61111_dnsc(0:9) = (/ 1, 0, 10, 49, 251, 1268, 6219, 28351, &
    122743, 495419 /), &
  mg61111_nsc(0:9) = (/ 1, 1, 11, 60, 311, 1579, 7798, 36149, &
    158892, 654311 /), &
  mg61111_dnb(0:9) = (/ 1, 11, 82, 519, 3023, 16767, 89904, 467571, &
    2354818, 11445045 /), &
  mg61111_nb(0:9) = (/ 1, 12, 94, 613, 3636, 20403, 110307, 577878, &
    2932696, 14377741 /)
!..procedures
CONTAINS
include 'mg61111_prims.f90'
include 'mg61111_prib.f90'
include 'mg61111_isecs.f90'
include 'mg61111_secs.f90'
include 'mg61111_base.f90'
END MODULE inv_mg61111
