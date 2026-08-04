MODULE inv_mg3211111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3211111_prims, mg3211111_prib, mg3211111_isecs, mg3211111_secs, &
  mg3211111_base
!..data
integer, parameter, private :: nkk=7, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,1,1,1,1,1/)
integer, parameter, public :: &
  mg3211111_id=133, &
  mg3211111_nkk=nkk, mg3211111_nk=nk, mg3211111_nr=nr, mg3211111_ngrp=12, &
  mg3211111_nkj(0:nkk-1)=nkj, &
  mg3211111_dnpr(0:9) = (/ 0, 23, 14, 7, 0, 0, 1, 0, 0, 0 /), &
  mg3211111_npr(0:9) = (/ 0, 23, 37, 44, 44, 44, 45, 45, 45, 45 /), &
  mg3211111_dnpb(0:9) = (/ 1, 23, 290, 2629, 19080, 117375, 633963, &
    3078480, 13669800, 56215574 /), &
  mg3211111_npb(0:9) = (/ 1, 24, 314, 2943, 22023, 139398, 773361, &
    3851841, 17521641, 73737215 /), &
  mg3211111_dnsc(0:9) = (/ 1, 0, 36, 126, 763, 3171, 10570, &
    31276, 78945, 169498 /), &
  mg3211111_nsc(0:9) = (/ 1, 1, 37, 163, 926, 4097, 14667, &
    45943, 124888, 294386 /), &
  mg3211111_dnb(0:9) = (/ 1, 23, 326, 3583, 33181, 269279, 1956870, &
    12907963, 78039910, 435823800 /), &
  mg3211111_nb(0:9) = (/ 1, 24, 350, 3933, 37114, 306393, 2263263, &
    15171226, 93211136, 529034936 /)
!..procedures
CONTAINS
include 'mg3211111_prims.f90'
include 'mg3211111_prib.f90'
include 'mg3211111_isecs.f90'
include 'mg3211111_secs.f90'
include 'mg3211111_base.f90'
END MODULE inv_mg3211111
