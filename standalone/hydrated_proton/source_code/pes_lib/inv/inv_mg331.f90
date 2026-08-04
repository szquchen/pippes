MODULE inv_mg331
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg331_prims, mg331_prib, mg331_isecs, mg331_secs, &
  mg331_base
!..data
integer, parameter, private :: nkk=3, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,1/)
integer, parameter, public :: &
  mg331_id=36, &
  mg331_nkk=nkk, mg331_nk=nk, mg331_nr=nr, mg331_ngrp=36, &
  mg331_nkj(0:nkk-1)=nkj, &
  mg331_dnpr(0:9) = (/ 0, 5, 7, 7, 1, 0, 1, 0, 0, 0 /), &
  mg331_npr(0:9) = (/ 0, 5, 12, 19, 20, 20, 21, 21, 21, 21 /), &
  mg331_dnpb(0:9) = (/ 1, 5, 22, 77, 239, 670, 1745, 4255, 9849, 21762 /), &
  mg331_npb(0:9) = (/ 1, 6, 28, 105, 344, 1014, 2759, 7014, 16863, 38625 /), &
  mg331_dnsc(0:9) = (/ 1, 0, 6, 29, 84, 250, 690, 1566, 3191, 5735 /), &
  mg331_nsc(0:9) = (/ 1, 1, 7, 36, 120, 370, 1060, 2626, 5817, 11552 /), &
  mg331_dnb(0:9) = (/ 1, 5, 28, 136, 600, 2440, 9200, 32190, 105276, &
    323199 /), &
  mg331_nb(0:9) = (/ 1, 6, 34, 170, 770, 3210, 12410, 44600, 149876, &
    473075 /)
!..procedures
CONTAINS
include 'mg331_prims.f90'
include 'mg331_prib.f90'
include 'mg331_isecs.f90'
include 'mg331_secs.f90'
include 'mg331_base.f90'
END MODULE inv_mg331
