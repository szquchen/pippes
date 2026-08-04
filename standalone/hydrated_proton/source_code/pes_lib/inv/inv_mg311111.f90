MODULE inv_mg311111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg311111_prims, mg311111_prib, mg311111_isecs, mg311111_secs, &
  mg311111_base
!..data
integer, parameter, private :: nkk=6, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,1,1,1,1,1/)
integer, parameter, public :: &
  mg311111_id=63, &
  mg311111_nkk=nkk, mg311111_nk=nk, mg311111_nr=nr, mg311111_ngrp=6, &
  mg311111_nkj(0:nkk-1)=nkj, &
  mg311111_dnpr(0:9) = (/ 0, 16, 6, 6, 0, 0, 0, 0, 0, 0 /), &
  mg311111_npr(0:9) = (/ 0, 16, 22, 28, 28, 28, 28, 28, 28, 28 /), &
  mg311111_dnpb(0:9) = (/ 1, 16, 142, 918, 4809, 21588, 85925, 310214, &
    1032434, 3205864 /), &
  mg311111_npb(0:9) = (/ 1, 17, 159, 1077, 5886, 27474, 113399, 423613, &
    1456047, 4661911 /), &
  mg311111_dnsc(0:9) = (/ 1, 0, 15, 50, 120, 330, 646, 876, 1170, 1360 /), &
  mg311111_nsc(0:9) = (/ 1, 1, 16, 66, 186, 516, 1162, 2038, 3208, 4568 /), &
  mg311111_dnb(0:9) = (/ 1, 16, 157, 1208, 7859, 44708, 226926, 1042716, &
    4387647, 17070354 /), &
  mg311111_nb(0:9) = (/ 1, 17, 174, 1382, 9241, 53949, 280875, 1323591, &
    5711238, 22781592 /)
!..procedures
CONTAINS
include 'mg311111_prims.f90'
include 'mg311111_prib.f90'
include 'mg311111_isecs.f90'
include 'mg311111_secs.f90'
include 'mg311111_base.f90'
END MODULE inv_mg311111
