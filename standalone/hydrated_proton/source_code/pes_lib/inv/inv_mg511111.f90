MODULE inv_mg511111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg511111_prims, mg511111_prib, mg511111_isecs, mg511111_secs, &
  mg511111_base
!..data
integer, parameter, private :: nkk=6, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,1,1,1,1,1/)
integer, parameter, public :: &
  mg511111_id=127, &
  mg511111_nkk=nkk, mg511111_nk=nk, mg511111_nr=nr, mg511111_ngrp=120, &
  mg511111_nkj(0:nkk-1)=nkj, &
  mg511111_dnpr(0:9) = (/ 0, 16, 7, 7, 7, 7, 1, 0, 0, 0 /), &
  mg511111_npr(0:9) = (/ 0, 16, 23, 30, 37, 44, 45, 45, 45, 45 /), &
  mg511111_dnpb(0:9) = (/ 1, 16, 143, 935, 4975, 22784, 92926, 345266, &
    1187468, 3825275 /), &
  mg511111_npb(0:9) = (/ 1, 17, 160, 1095, 6070, 28854, 121780, 467046, &
    1654514, 5479789 /), &
  mg511111_dnsc(0:9) = (/ 1, 0, 15, 77, 420, 2184, 10255, 44079, &
    172955, 620802 /), &
  mg511111_nsc(0:9) = (/ 1, 1, 16, 93, 513, 2697, 12952, 57031, &
    229986, 850788 /), &
  mg511111_dnb(0:9) = (/ 1, 16, 158, 1252, 8772, 56724, 344805, 1983272, &
    10811950, 55874051 /), &
  mg511111_nb(0:9) = (/ 1, 17, 175, 1427, 10199, 66923, 411728, 2395000, &
    13206950, 69081001 /)
!..procedures
CONTAINS
include 'mg511111_prims.f90'
include 'mg511111_prib.f90'
include 'mg511111_isecs.f90'
include 'mg511111_secs.f90'
include 'mg511111_base.f90'
END MODULE inv_mg511111
