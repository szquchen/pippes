MODULE inv_mg52
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg52_prims, mg52_prib, mg52_isecs, mg52_secs, &
  mg52_base
!..data
integer, parameter, private :: nkk=2, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,2/)
integer, parameter, public :: &
  mg52_id=32, &
  mg52_nkk=nkk, mg52_nk=nk, mg52_nr=nr, mg52_ngrp=240, &
  mg52_nkj(0:nkk-1)=nkj, &
  mg52_dnpr(0:9) = (/ 0, 3, 5, 3, 4, 3, 2, 0, 0, 0 /), &
  mg52_npr(0:9) = (/ 0, 3, 8, 11, 15, 18, 20, 20, 20, 20 /), &
  mg52_dnpb(0:9) = (/ 1, 3, 11, 28, 73, 164, 364, 745, 1496, 2855 /), &
  mg52_npb(0:9) = (/ 1, 4, 15, 43, 116, 280, 644, 1389, 2885, 5740 /), &
  mg52_dnsc(0:9) = (/ 1, 0, 1, 12, 39, 113, 338, 932, 2402, 5678 /), &
  mg52_nsc(0:9) = (/ 1, 1, 2, 14, 53, 166, 504, 1436, 3838, 9516 /), &
  mg52_dnb(0:9) = (/ 1, 3, 12, 43, 159, 554, 1879, 6066, 18755, &
    55213 /), &
  mg52_nb(0:9) = (/ 1, 4, 16, 59, 218, 772, 2651, 8717, 27472, &
    82685 /)
! Note: there is also a primary at degree 10
!..procedures
CONTAINS
include 'mg52_prims.f90'
include 'mg52_prib.f90'
include 'mg52_isecs.f90'
include 'mg52_secs.f90'
include 'mg52_base.f90'
END MODULE inv_mg52
