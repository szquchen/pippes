MODULE inv_mg21111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg21111111_prims, mg21111111_prib, mg21111111_isecs, &
  mg21111111_secs, mg21111111_base
!..data
integer, parameter, private :: nkk=8, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,1,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg21111111_id=95, &
  mg21111111_nkk=nkk, mg21111111_nk=nk, mg21111111_nr=nr, mg21111111_ngrp=2, &
  mg21111111_nkj(0:nkk-1)=nkj, &
  mg21111111_dnpr(0:9) = (/ 0, 29, 7, 0, 0, 0, 0, 0, 0, 0 /), &
  mg21111111_npr(0:9) = (/ 0, 29, 36, 36, 36, 36, 36, 36, 36, 36 /), &
  mg21111111_dnpb(0:9) = (/ 1, 29, 442, 4698, 39033, 269613, 1608888, &
    8514168, 40718298, 178504338 /), &
  mg21111111_npb(0:9) = (/ 1, 30, 472, 5170, 44203, 313816, 1922704, &
    10436872, 51155170, 229659508 /), &
  mg21111111_dnsc(0:9) = (/ 1, 0, 21, 0, 35, 0, 7, 0, 0, 0 /), &
  mg21111111_nsc(0:9) = (/ 1, 1, 22, 22, 57, 57, 64, 64, 64, 64 /), &
  mg21111111_dnb(0:9) = (/ 1, 29, 463, 5307, 48350, 369286, 2444058, &
    14340674, 75874195, 366771207 /), &
  mg21111111_nb(0:9) = (/ 1, 30, 493, 5800, 54150, 423436, 2867494, &
    17208168, 93082363, 459853570 /)
!..procedures
CONTAINS
include 'mg21111111_prims.f90'
include 'mg21111111_prib.f90'
include 'mg21111111_isecs.f90'
include 'mg21111111_secs.f90'
include 'mg21111111_base.f90'
END MODULE inv_mg21111111
