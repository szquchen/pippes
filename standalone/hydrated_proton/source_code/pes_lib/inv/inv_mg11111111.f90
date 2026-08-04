MODULE inv_mg11111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg11111111_prims, mg11111111_prib, &
  mg11111111_secs, mg11111111_base
!..data
integer, parameter, private :: nkk=8, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/1,1,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg11111111_id=66, &
  mg11111111_nkk=nkk, mg11111111_nk=nk, mg11111111_nr=nr, mg11111111_ngrp=1, &
  mg11111111_nkj(0:nkk-1)=nkj, &
  mg11111111_dnpr(0:9) = (/ 0, 28, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg11111111_npr(0:9) = (/ 0, 28, 28, 28, 28, 28, 28, 28, 28, 28 /), &
  mg11111111_dnpb(0:9) = (/ 1, 28, 406, 4060, 31465, 201376, 1107568, &
    5379616, 23535820, 94143280 /), &
  mg11111111_npb(0:9) = (/ 1, 29, 435, 4495, 35960, 237336, 1344904, &
    6724520, 30260340, 124403620 /), &
  mg11111111_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg11111111_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  mg11111111_dnb(0:9) = (/ 1, 28, 406, 4060, 31465, 201376, 1107568, &
    5379616, 23535820, 94143280 /), &
  mg11111111_nb(0:9) = (/ 1, 29, 435, 4495, 35960, 237336, 1344904, &
    6724520, 30260340, 124403620 /)
!..procedures
CONTAINS
include 'mg11111111_prims.f90'
include 'mg11111111_prib.f90'
include 'mg11111111_secs.f90'
include 'mg11111111_base.f90'
END MODULE inv_mg11111111
