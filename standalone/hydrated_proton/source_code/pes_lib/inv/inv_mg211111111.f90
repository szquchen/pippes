MODULE inv_mg211111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg211111111_prims, mg211111111_prib, mg211111111_isecs, &
  mg211111111_secs, mg211111111_base
!..data
integer, parameter, private :: nkk=9, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,1,1,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg211111111_id=137, &
  mg211111111_nkk=nkk, mg211111111_nk=nk, mg211111111_nr=nr, &
  mg211111111_ngrp=2, &
  mg211111111_nkj(0:nkk-1)=nkj, &
  mg211111111_dnpr(0:8) = (/ 0, 37, 8, 0, 0, 0, 0, 0, 0 /), &
  mg211111111_npr(0:8) = (/ 0, 37, 45, 45, 45, 45, 45, 45, 45 /), &
  mg211111111_dnpb(0:8) = (/ 1, 37, 711, 9435, 97050, 823842, &
    6002334, 38552742, 222573645 /), &
  mg211111111_npb(0:8) = (/ 1, 38, 749, 10184, 107234, 931076, &
    6933410, 45486152, 268059797 /), &
  mg211111111_dnsc(0:8) = (/ 1, 0, 28, 0, 70, 0, 28, 0, 1 /), &
  mg211111111_nsc(0:8) = (/ 1, 1, 29, 29, 99, 99, 127, 127, 128 /), &
  mg211111111_dnb(0:8) = (/ 1, 37, 739, 10471, 117028, 1090612, &
    8769532, 62281804, 397452406 /), &
  mg211111111_nb(0:8) = (/ 1, 38, 777, 11248, 128276, 1218888, &
    9988420, 72270224, 469722630 /)
!..procedures
CONTAINS
include 'mg211111111_prims.f90'
include 'mg211111111_prib.f90'
include 'mg211111111_isecs.f90'
include 'mg211111111_secs.f90'
include 'mg211111111_base.f90'
END MODULE inv_mg211111111
