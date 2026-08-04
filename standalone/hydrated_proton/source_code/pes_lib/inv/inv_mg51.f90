MODULE inv_mg51
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg51_prims, mg51_prib, mg51_isecs, mg51_secs, &
  mg51_base
!..data
integer, parameter, private :: nkk=2, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,1/)
integer, parameter, public :: &
  mg51_id=20, &
  mg51_nkk=nkk, mg51_nk=nk, mg51_nr=nr, mg51_ngrp=120, &
  mg51_nkj(0:nkk-1)=nkj, &
  mg51_dnpr(0:9) = (/ 0, 2, 3, 3, 3, 3, 1, 0, 0, 0 /), &
  mg51_npr(0:9) = (/ 0, 2, 5, 8, 11, 14, 15, 15, 15, 15 /), &
  mg51_dnpb(0:9) = (/ 1, 2, 6, 13, 29, 57, 111, 201, 360, 616 /), &
  mg51_npb(0:9) = (/ 1, 3, 9, 22, 51, 108, 219, 420, 780, 1396 /), &
  mg51_dnsc(0:9) = (/ 1, 0, 1, 7, 20, 46, 107, 230, 461, 847 /), &
  mg51_nsc(0:9) = (/ 1, 1, 2, 9, 29, 75, 182, 412, 873, 1720 /), &
  mg51_dnb(0:9) = (/ 1, 2, 7, 22, 69, 198, 550, 1441, 3611, 8608 /), &
  mg51_nb(0:9) = (/ 1, 3, 10, 32, 101, 299, 849, 2290, 5901, 14509 /)
!..procedures
CONTAINS
include 'mg51_prims.f90'
include 'mg51_prib.f90'
include 'mg51_isecs.f90'
include 'mg51_secs.f90'
include 'mg51_base.f90'
END MODULE inv_mg51
