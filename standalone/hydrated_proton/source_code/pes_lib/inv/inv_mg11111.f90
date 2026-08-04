MODULE inv_mg11111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg11111_prims, mg11111_prib, mg11111_secs, &
  mg11111_base
!..data
integer, parameter, private :: nkk=5, nk=5, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/1,1,1,1,1/)
integer, parameter, public :: &
  mg11111_id=18, &
  mg11111_nkk=nkk, mg11111_nk=nk, mg11111_nr=nr, mg11111_ngrp=1, &
  mg11111_nkj(0:nkk-1)=nkj, &
  mg11111_dnpr(0:9) = (/ 0, 10, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg11111_npr(0:9) = (/ 0, 10, 10, 10, 10, 10, 10, 10, 10, 10 /), &
  mg11111_dnpb(0:9) = (/ 1, 10, 55, 220, 715, 2002, 5005, 11440, 24310, &
    48620 /), &
  mg11111_npb(0:9) = (/ 1, 11, 66, 286, 1001, 3003, 8008, 19448, 43758, &
    92378 /), &
  mg11111_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg11111_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  mg11111_dnb(0:9) = (/ 1, 10, 55, 220, 715, 2002, 5005, 11440, 24310, &
    48620 /), &
  mg11111_nb(0:9) = (/ 1, 11, 66, 286, 1001, 3003, 8008, 19448, 43758, &
    92378 /)
!..procedures
CONTAINS
include 'mg11111_prims.f90'
include 'mg11111_prib.f90'
include 'mg11111_secs.f90'
include 'mg11111_base.f90'
END MODULE inv_mg11111
