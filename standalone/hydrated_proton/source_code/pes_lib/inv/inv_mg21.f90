MODULE inv_mg21
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg21_prims, mg21_prib, mg21_secs, mg21_base
!..data
integer, parameter, private :: nkk=2, nk=3, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,1/)
integer, parameter, public :: &
  mg21_id=5, &
  mg21_nkk=nkk, mg21_nk=nk, mg21_nr=nr, mg21_ngrp=2, &
  mg21_nkj(0:nkk-1)=nkj, &
  mg21_dnpr(0:19) = (/ 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg21_npr(0:19) = (/ 0, 2, 3, 3, 3, 3, 3, 3, 3, 3, &
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3 /), &
  mg21_dnpb(0:19) = (/ 1, 2, 4, 6, 9, 12, 16, 20, 25, 30, &
    36, 42, 49, 56, 64, 72, 81, 90, 100, 110 /), &
  mg21_npb(0:19) = (/ 1, 3, 7, 13, 22, 34, 50, 70, 95, 125, &
    161, 203, 252, 308, 372, 444, 525, 615, 715, 825 /), &
  mg21_dnsc(0:19) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg21_nsc(0:19) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  mg21_dnb(0:19) = (/ 1, 2, 4, 6, 9, 12, 16, 20, 25, 30, &
    36, 42, 49, 56, 64, 72, 81, 90, 100, 110 /), &
  mg21_nb(0:19) = (/ 1, 3, 7, 13, 22, 34, 50, 70, 95, 125, &
    161, 203, 252, 308, 372, 444, 525, 615, 715, 825 /)
!..procedures
CONTAINS
include 'mg21_prims.f90'
include 'mg21_prib.f90'
include 'mg21_secs.f90'
include 'mg21_base.f90'
END MODULE inv_mg21
