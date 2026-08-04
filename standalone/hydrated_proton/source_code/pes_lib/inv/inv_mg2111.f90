MODULE inv_mg2111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg2111_prims, mg2111_prib, mg2111_isecs, mg2111_secs, &
  mg2111_base
!..data
integer, parameter, private :: nkk=4, nk=5, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,1,1,1/)
integer, parameter, public :: &
  mg2111_id=17, &
  mg2111_nkk=nkk, mg2111_nk=nk, mg2111_nr=nr, mg2111_ngrp=2, &
  mg2111_nkj(0:nkk-1)=nkj, &
  mg2111_dnpr(0:9) = (/ 0, 7, 3, 0, 0, 0, 0, 0, 0, 0 /), &
  mg2111_npr(0:9) = (/ 0, 7, 10, 10, 10, 10, 10, 10, 10, 10 /), &
  mg2111_dnpb(0:9) = (/ 1, 7, 31, 105, 300, 756, 1732, 3676, 7330, &
    13870 /), &
  mg2111_npb(0:9) = (/ 1, 8, 39, 144, 444, 1200, 2932, 6608, 13938, &
    27808 /), &
  mg2111_dnsc(0:9) = (/ 1, 0, 3, 0, 0, 0, 0, 0, 0, 0 /), &
  mg2111_nsc(0:9) = (/ 1, 1, 4, 4, 4, 4, 4, 4, 4, 4 /), &
  mg2111_dnb(0:9) = (/ 1, 7, 34, 126, 393, 1071, 2632, 5944, 12526, &
    24898 /), &
  mg2111_nb(0:9) = (/ 1, 8, 42, 168, 561, 1632, 4264, 10208, 22734, &
    47632 /)
!..procedures
CONTAINS
include 'mg2111_prims.f90'
include 'mg2111_prib.f90'
include 'mg2111_isecs.f90'
include 'mg2111_secs.f90'
include 'mg2111_base.f90'
END MODULE inv_mg2111
