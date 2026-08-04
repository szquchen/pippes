MODULE inv_mg4111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4111_prims, mg4111_prib, mg4111_isecs, mg4111_secs, &
  mg4111_base
!..data
integer, parameter, private :: nkk=4, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,1,1,1/)
integer, parameter, public :: &
  mg4111_id=38, &
  mg4111_nkk=nkk, mg4111_nk=nk, mg4111_nr=nr, mg4111_ngrp=24, &
  mg4111_nkj(0:nkk-1)=nkj, &
  mg4111_dnpr(0:9) = (/ 0, 7, 5, 5, 4, 0, 0, 0, 0, 0 /), &
  mg4111_npr(0:9) = (/ 0, 7, 12, 17, 21, 21, 21, 21, 21, 21 /), &
  mg4111_dnpb(0:9) = (/ 1, 7, 33, 124, 404, 1180, 3171, 7957, 18863, &
    42588 /), &
  mg4111_npb(0:9) = (/ 1, 8, 41, 165, 569, 1749, 4920, 12877, 31740, &
    74328 /), &
  mg4111_dnsc(0:9) = (/ 1, 0, 6, 26, 82, 211, 551, 1191, 2277, 3826 /), &
  mg4111_nsc(0:9) = (/ 1, 1, 7, 33, 115, 326, 877, 2068, 4345, 8171 /), &
  mg4111_dnb(0:9) = (/ 1, 7, 39, 192, 866, 3567, 13553, 47720, 156658, &
    482172 /), &
  mg4111_nb(0:9) = (/ 1, 8, 47, 239, 1105, 4672, 18225, 65945, 222603, &
    704775 /)
!..procedures
CONTAINS
include 'mg4111_prims.f90'
include 'mg4111_prib.f90'
include 'mg4111_isecs.f90'
include 'mg4111_secs.f90'
include 'mg4111_base.f90'
END MODULE inv_mg4111
