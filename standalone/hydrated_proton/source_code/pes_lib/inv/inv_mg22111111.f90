MODULE inv_mg22111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg22111111_prims, mg22111111_prib, mg22111111_isecs, &
  mg22111111_secs, mg22111111_base
!..data
integer, parameter, private :: nkk=8, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg22111111_id=136, &
  mg22111111_nkk=nkk, mg22111111_nk=nk, mg22111111_nr=nr, &
  mg22111111_ngrp=2*2, &
  mg22111111_nkj(0:nkk-1)=nkj, &
  mg22111111_dnpr(0:8) = (/ 0, 30, 15, 0, 0, 0, 0, 0, 0 /), &
  mg22111111_npr(0:8) = (/ 0, 30, 45, 45, 45, 45, 45, 45, 45 /), &
  mg22111111_dnpb(0:8) = (/ 1, 30, 480, 5410, 48015, 356256, &
    2293440, 13137120, 68185080 /), &
  mg22111111_npb(0:8) = (/ 1, 31, 511, 5921, 53936, 410192, &
    2703632, 15840752, 84025832 /), &
  mg22111111_dnsc(0:8) = (/ 1, 0, 42, 49, 511, 490, &
    1484, 1519, 1519 /), &
  mg22111111_nsc(0:8) = (/ 1, 1, 43, 92, 603, 1093, &
    2577, 4096, 5615 /), &
  mg22111111_dnb(0:8) = (/ 1, 30, 522, 6719, 70156, 622816, &
    4836624, 33498356, 209912078 /), &
  mg22111111_nb(0:8) = (/ 1, 31, 553, 7272, 77428, 700244, &
    5536868, 39035224, 248947302 /)
!..procedures
CONTAINS
include 'mg22111111_prims.f90'
include 'mg22111111_prib.f90'
include 'mg22111111_isecs.f90'
include 'mg22111111_secs.f90'
include 'mg22111111_base.f90'
END MODULE inv_mg22111111
