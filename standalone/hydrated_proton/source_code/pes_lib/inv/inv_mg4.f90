MODULE inv_mg4
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4_prims, mg4_prib, mg4_isecs, mg4_secs, mg4_base
!..data
integer, parameter, private :: nkk=1, nk=4, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4/)
integer, parameter, public :: &
  mg4_id=7, &
  mg4_nkk=nkk, mg4_nk=nk, mg4_nr=nr, mg4_ngrp=24, &
  mg4_nkj(0:nkk-1)=nkj, &
  mg4_dnpr(0:19) = (/ 0, 1, 2, 2, 1, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg4_npr(0:19) = (/ 0, 1, 3, 5, 6, 6, 6, 6, 6, 6, &
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6 /), &
  mg4_dnpb(0:19) = (/ 1, 1, 3, 5, 9, 13, 22, 30, 45, 61, &
    85, 111, 150, 190, 247, 309, 390, 478, 593, 715 /), &
  mg4_npb(0:19) = (/ 1, 2, 5, 10, 19, 32, 54, 84, 129, 190, &
    275, 386, 536, 726, 973, 1282, 1672, 2150, 2743, 3458 /), &
  mg4_dnsc(0:19) = (/ 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg4_nsc(0:19) = (/ 1, 1, 1, 2, 3, 4, 5, 5, 5, 6, &
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6 /), &
  mg4_dnb(0:19) = (/ 1, 1, 3, 6, 11, 18, 32, 48, 75, 111, &
    160, 224, 313, 420, 562, 738, 956, 1221, 1550, 1936 /), &
  mg4_nb(0:19) = (/ 1, 2, 5, 11, 22, 40, 72, 120, 195, 306, &
    466, 690, 1003, 1423, 1985, 2723, 3679, 4900, 6450, 8386 /)
!..procedures
CONTAINS
include 'mg4_prims.f90'
include 'mg4_prib.f90'
include 'mg4_isecs.f90'
include 'mg4_secs.f90'
include 'mg4_base.f90'
END MODULE inv_mg4
