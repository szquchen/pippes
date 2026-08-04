MODULE inv_mg8
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg8_prims, mg8_prib, mg8_isecs, mg8_secs, &
  mg8_base
!..data
integer, parameter, private :: nkk=1, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/7,1/)
integer, parameter, public :: &
  mg8_id=45, &
  mg8_nkk=nkk, mg8_nk=nk, mg8_nr=nr, mg8_ngrp=40320, &
  mg8_nkj(0:nkk-1)=nkj, &
  mg8_dnpr(0:9) = (/ 0, 1, 2, 6, 3, 3, 2, 3, 0, 0 /), &
  mg8_npr(0:9) = (/ 0, 1, 3, 9, 12, 15, 17, 20, 20, 20 /), &
  mg8_dnpb(0:9) = (/ 1, 1, 3, 9, 15, 30, 63, 108, 192, 344 /), &
  mg8_npb(0:9) = (/ 1, 2, 5, 14, 29, 59, 122, 230, 422, 766 /), &
  mg8_dnsc(0:9) = (/ 1, 0, 0, -1, 9, 28, 88, 242, 751, 2269 /), &
  mg8_nsc(0:9) = (/ 1, 1, 1, 0, 9, 37, 125, 367, 1118, 3387 /), &
  mg8_dnb(0:9) = (/ 1, 1, 3, 8, 23, 64, 197, 588, 1806, 5509 /), &
  mg8_nb(0:9) = (/ 1, 2, 5, 13, 36, 100, 297, 885, 2691, 8200 /)
! Note: there is also a primary at degree ..
!..procedures
CONTAINS
include 'mg8_prims.f90'
include 'mg8_prib.f90'
include 'mg8_isecs.f90'
include 'mg8_secs.f90'
include 'mg8_base.f90'
END MODULE inv_mg8
