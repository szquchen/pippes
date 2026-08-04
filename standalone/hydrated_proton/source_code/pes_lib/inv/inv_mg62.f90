MODULE inv_mg62
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg62_prims, mg62_prib, mg62_isecs, mg62_secs, &
  mg62_base
!..data
integer, parameter, private :: nkk=2, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,2/)
integer, parameter, public :: &
  mg62_id=47, &
  mg62_nkk=nkk, mg62_nk=nk, mg62_nr=nr, mg62_ngrp=1440, &
  mg62_nkj(0:nkk-1)=nkj, &
  mg62_dnpr(0:9) = (/ 0, 3, 5, 6, 5, 4, 4, 0, 0, 0 /), &
  mg62_npr(0:9) = (/ 0, 3, 8, 14, 19, 23, 27, 27, 27, 27 /), &
  mg62_dnpb(0:9) = (/ 1, 3, 11, 31, 83, 201, 470, 1030, 2190, 4470 /), &
  mg62_npb(0:9) = (/ 1, 4, 15, 46, 129, 330, 800, 1830, 4020, 8490 /), &
  mg62_dnsc(0:9) = (/ 1, 0, 1, 10, 44, 153, 541, 1819, 5938, 18147 /), &
  mg62_nsc(0:9) = (/ 1, 1, 2, 12, 56, 209, 750, 2569, 8507, 26654 /), &
  mg62_dnb(0:9) = (/ 1, 3, 12, 44, 168, 627, 2347, 8550, 30411, &
    104484 /), &
  mg62_nb(0:9) = (/ 1, 4, 16, 60, 228, 855, 3202, 11752, 42163, &
    146647 /)
! Note: there is also a primary at degree 10
!..procedures
CONTAINS
include 'mg62_prims.f90'
include 'mg62_prib.f90'
include 'mg62_isecs.f90'
include 'mg62_secs.f90'
include 'mg62_base.f90'
END MODULE inv_mg62
