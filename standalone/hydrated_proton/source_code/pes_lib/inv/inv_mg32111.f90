MODULE inv_mg32111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg32111_prims, mg32111_prib, mg32111_isecs, mg32111_secs, &
  mg32111_base
!..data
integer, parameter, private :: nkk=5, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,1,1,1/)
integer, parameter, public :: &
  mg32111_id=61, &
  mg32111_nkk=nkk, mg32111_nk=nk, mg32111_nr=nr, mg32111_ngrp=12, &
  mg32111_nkj(0:nkk-1)=nkj, &
  mg32111_dnpr(0:9) = (/ 0, 12, 10, 5, 0, 0, 1, 0, 0, 0 /), &
  mg32111_npr(0:9) = (/ 0, 12, 22, 27, 27, 27, 28, 28, 28, 28 /), &
  mg32111_dnpb(0:9) = (/ 1, 12, 88, 489, 2260, 9108, 32972, 109356, &
    337040, 975564 /), &
  mg32111_npb(0:9) = (/ 1, 13, 101, 590, 2850, 11958, 44930, 154286, &
    491326, 1466890 /), &
  mg32111_dnsc(0:9) = (/ 1, 0, 16, 55, 195, 610, 1529, 3115, 5625, 8626 /), &
  mg32111_nsc(0:9) = (/ 1, 1, 17, 72, 267, 877, 2406, 5521, 11146, 19772 /), &
  mg32111_dnb(0:9) = (/ 1, 12, 104, 736, 4523, 24722, 122036, 549882, &
    2282079, 8791307 /), &
  mg32111_nb(0:9) = (/ 1, 13, 117, 853, 5376, 30098, 152134, 702016, &
    2984095, 11775402 /)
!..procedures
CONTAINS
include 'mg32111_prims.f90'
include 'mg32111_prib.f90'
include 'mg32111_isecs.f90'
include 'mg32111_secs.f90'
include 'mg32111_base.f90'
END MODULE inv_mg32111
