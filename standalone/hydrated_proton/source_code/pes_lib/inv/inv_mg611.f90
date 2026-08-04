MODULE inv_mg611
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg611_prims, mg611_prib, mg611_isecs, mg611_secs, &
  mg611_base
!..data
integer, parameter, private :: nkk=3, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,1,1/)
integer, parameter, public :: &
  mg611_id=50, &
  mg611_nkk=nkk, mg611_nk=nk, mg611_nr=nr, mg611_ngrp=720, &
  mg611_nkj(0:nkk-1)=nkj, &
  mg611_dnpr(0:9) = (/ 0, 4, 4, 6, 5, 5, 4, 0, 0, 0 /), &
  mg611_npr(0:9) = (/ 0, 4, 8, 14, 19, 24, 28, 28, 28, 28 /), &
  mg611_dnpb(0:9) = (/ 1, 4, 14, 42, 114, 285, 675, 1514, 3262, 6774 /), &
  mg611_npb(0:9) = (/ 1, 5, 19, 61, 175, 460, 1135, 2649, 5911, 12685 /), &
  mg611_dnsc(0:9) = (/ 1, 0, 3, 15, 61, 227, 822, 2767, 8957, 27013 /), &
  mg611_nsc(0:9) = (/ 1, 1, 4, 19, 80, 307, 1129, 3896, 12853, 39866 /), &
  mg611_dnb(0:9) = (/ 1, 4, 17, 69, 277, 1092, 4231, 15874, 57583, &
    200807 /), &
  mg611_nb(0:9) = (/ 1, 5, 22, 91, 368, 1460, 5691, 21565, 79148, &
    279955 /)
!..procedures
CONTAINS
include 'mg611_prims.f90'
include 'mg611_prib.f90'
include 'mg611_isecs.f90'
include 'mg611_secs.f90'
include 'mg611_base.f90'
END MODULE inv_mg611
