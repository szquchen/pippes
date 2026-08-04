MODULE inv_mg2111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg2111111_prims, mg2111111_prib, mg2111111_secs, &
  mg2111111_base
!..data
integer, parameter, private :: nkk=7, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg2111111_id=65, &
  mg2111111_nkk=nkk, mg2111111_nk=nk, mg2111111_nr=nr, mg2111111_ngrp=2, &
  mg2111111_nkj(0:nkk-1)=nkj, &
  mg2111111_dnpr(0:9) = (/ 0, 22, 6, 0, 0, 0, 0, 0, 0, 0 /), &
  mg2111111_npr(0:9) = (/ 0, 22, 28, 28, 28, 28, 28, 28, 28, 28 /), &
  mg2111111_dnpb(0:9) = (/ 1, 22, 259, 2156, 14189, 78386, 377279, &
    1622456, 6348149, 22908886 /), &
  mg2111111_npb(0:9) = (/ 1, 23, 282, 2438, 16627, 95013, 472292, &
    2094748, 8442897, 31351783 /), &
  mg2111111_dnsc(0:9) = (/ 1, 0, 15, 0, 15, 0, 1, 0, 0, 0 /), &
  mg2111111_nsc(0:9) = (/ 1, 1, 16, 16, 31, 31, 32, 32, 32, 32 /), &
  mg2111111_dnb(0:9) = (/ 1, 22, 274, 2486, 18089, 111056, 594000, &
    2830608, 12220428, 48423672 /), &
  mg2111111_nb(0:9) = (/ 1, 23, 297, 2783, 20872, 131928, 725928, &
    3556536, 15776964, 64200636 /)
!..procedures
CONTAINS
include 'mg2111111_prims.f90'
include 'mg2111111_prib.f90'
include 'mg2111111_isecs.f90'
include 'mg2111111_secs.f90'
include 'mg2111111_base.f90'
END MODULE inv_mg2111111
