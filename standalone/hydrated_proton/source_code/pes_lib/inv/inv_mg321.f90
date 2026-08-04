MODULE inv_mg321
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg321_prims, mg321_prib, mg321_isecs, mg321_secs, &
  mg321_base
!..data
integer, parameter, private :: nkk=3, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,1/)
integer, parameter, public :: &
  mg321_id=24, &
  mg321_nkk=nkk, mg321_nk=nk, mg321_nr=nr, mg321_ngrp=12, &
  mg321_nkj(0:nkk-1)=nkj, &
  mg321_dnpr(0:9) = (/ 0, 5, 6, 3, 0, 0, 1, 0, 0, 0 /), &
  mg321_npr(0:9) = (/ 0, 5, 11, 14, 14, 14, 15, 15, 15, 15 /), &
  mg321_dnpb(0:9) = (/ 1, 5, 21, 68, 196, 504, 1203, 2679, 5661, 11402 /), &
  mg321_npb(0:9) = (/ 1, 6, 27, 95, 291, 795, 1998, 4677, 10338, 21740 /), &
  mg321_dnsc(0:9) = (/ 1, 0, 4, 16, 29, 46, 94, 124, 118, 118 /), &
  mg321_nsc(0:9) = (/ 1, 1, 5, 21, 50, 96, 190, 314, 432, 550 /), &
  mg321_dnb(0:9) = (/ 1, 5, 25, 104, 389, 1303, 4008, 11363, 30061, &
    74702 /), &
  mg321_nb(0:9) = (/ 1, 6, 31, 135, 524, 1827, 5835, 17198, 47259, &
    121961 /)
!..procedures
CONTAINS
include 'mg321_prims.f90'
include 'mg321_prib.f90'
include 'mg321_isecs.f90'
include 'mg321_secs.f90'
include 'mg321_base.f90'
END MODULE inv_mg321
