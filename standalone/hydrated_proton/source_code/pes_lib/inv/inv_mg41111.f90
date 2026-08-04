MODULE inv_mg41111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg41111_prims, mg41111_prib, mg41111_isecs, mg41111_secs, &
  mg41111_base
!..data
integer, parameter, private :: nkk=5, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,1,1,1,1/)
integer, parameter, public :: &
  mg41111_id=60, &
  mg41111_nkk=nkk, mg41111_nk=nk, mg41111_nr=nr, mg41111_ngrp=24, &
  mg41111_nkj(0:nkk-1)=nkj, &
  mg41111_dnpr(0:9) = (/ 0, 11, 6, 6, 5, 0, 0, 0, 0, 0 /), &
  mg41111_npr(0:9) = (/ 0, 11, 17, 23, 28, 28, 28, 28, 28, 28 /), &
  mg41111_dnpb(0:9) = (/ 1, 11, 72, 358, 1489, 5437, 17949, 54617, &
    155296, 416790 /), &
  mg41111_npb(0:9) = (/ 1, 12, 84, 442, 1931, 7368, 25317, 79934, &
    235230, 652020 /), &
  mg41111_dnsc(0:9) = (/ 1, 0, 10, 45, 175, 560, 1745, 4525, 10450, &
    21360 /), &
  mg41111_nsc(0:9) = (/ 1, 1, 11, 56, 231, 791, 2536, 7061, 17511, &
    38871 /), &
  mg41111_dnb(0:9) = (/ 1, 11, 82, 513, 2879, 14742, 69454, 302682, &
    1226371, 4642800 /), &
  mg41111_nb(0:9) = (/ 1, 12, 94, 607, 3486, 18228, 87682, 390364, &
    1616735, 6259535 /)
!..procedures
CONTAINS
include 'mg41111_prims.f90'
include 'mg41111_prib.f90'
include 'mg41111_isecs.f90'
include 'mg41111_secs.f90'
include 'mg41111_base.f90'
END MODULE inv_mg41111
