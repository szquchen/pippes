MODULE inv_mg211111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg211111_prims, mg211111_prib, mg211111_isecs, mg211111_secs, &
  mg211111_base
!..data
integer, parameter, private :: nkk=6, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,1,1,1,1,1/)
integer, parameter, public :: &
  mg211111_id=43, &
  mg211111_nkk=nkk, mg211111_nk=nk, mg211111_nr=nr, mg211111_ngrp=2, &
  mg211111_nkj(0:nkk-1)=nkj, &
  mg211111_dnpr(0:9) = (/ 0, 16, 5, 0, 0, 0, 0, 0, 0, 0 /), &
  mg211111_npr(0:9) = (/ 0, 16, 21, 21, 21, 21, 21, 21, 21, 21 /), &
  mg211111_dnpb(0:9) = (/ 1, 16, 141, 896, 4571, 19824, 75719, 260864, &
    824604, 2422464 /), &
  mg211111_npb(0:9) = (/ 1, 17, 158, 1054, 5625, 25449, 101168, 362032, &
    1186636, 3609100 /), &
  mg211111_dnsc(0:9) = (/ 1, 0, 10, 0, 5, 0, 0, 0, 0, 0 /), &
  mg211111_nsc(0:9) = (/ 1, 1, 11, 11, 16, 16, 16, 16, 16, 16 /), &
  mg211111_dnb(0:9) = (/ 1, 16, 151, 1056, 5986, 28864, 122134, 463584, &
    1604649, 5130224 /), &
  mg211111_nb(0:9) = (/ 1, 17, 168, 1224, 7210, 36074, 158208, 621792, &
    2226441, 7356665 /)
!..procedures
CONTAINS
include 'mg211111_prims.f90'
include 'mg211111_prib.f90'
include 'mg211111_isecs.f90'
include 'mg211111_secs.f90'
include 'mg211111_base.f90'
END MODULE inv_mg211111
