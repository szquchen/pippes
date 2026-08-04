MODULE inv_mg43
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg43_prims, mg43_prib, mg43_isecs, mg43_secs, &
  mg43_base
!..data
integer, parameter, private :: nkk=2, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,3/)
integer, parameter, public :: &
  mg43_id=33, &
  mg43_nkk=nkk, mg43_nk=nk, mg43_nr=nr, mg43_ngrp=144, &
  mg43_nkj(0:nkk-1)=nkj, &
  mg43_dnpr(0:9) = (/ 0, 3, 6, 6, 4, 0, 1, 0, 0, 0 /), &
  mg43_npr(0:9) = (/ 0, 3, 9, 15, 19, 19, 20, 20, 20, 20 /), &
  mg43_dnpb(0:9) = (/ 1, 3, 12, 34, 94, 228, 538, 1174, 2488, 5022 /), &
  mg43_npb(0:9) = (/ 1, 4, 16, 50, 144, 372, 910, 2084, 4572, 9594 /), &
  mg43_dnsc(0:9) = (/ 1, 0, 2, 16, 51, 154, 457, 1205, 2971, 6629 /), &
  mg43_nsc(0:9) = (/ 1, 1, 3, 19, 70, 224, 681, 1886, 4857, 11486 /), &
  mg43_dnb(0:9) = (/ 1, 3, 14, 56, 217, 795, 2801, 9292, 29312, 87622 /), &
  mg43_nb(0:9) = (/ 1, 4, 18, 74, 291, 1086, 3887, 13179, 42491, 130113 /)
! Note: there is also a primary at degree 12
!..procedures
CONTAINS
include 'mg43_prims.f90'
include 'mg43_prib.f90'
include 'mg43_isecs.f90'
include 'mg43_secs.f90'
include 'mg43_base.f90'
END MODULE inv_mg43
