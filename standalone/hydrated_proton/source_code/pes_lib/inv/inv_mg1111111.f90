MODULE inv_mg1111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg1111111_prims, mg1111111_prib, mg1111111_secs, &
  mg1111111_base
!..data
integer, parameter, private :: nkk=7, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/1,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg1111111_id=44, &
  mg1111111_nkk=nkk, mg1111111_nk=nk, mg1111111_nr=nr, mg1111111_ngrp=1, &
  mg1111111_nkj(0:nkk-1)=nkj, &
  mg1111111_dnpr(0:9) = (/ 0, 21, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg1111111_npr(0:9) = (/ 0, 21, 21, 21, 21, 21, 21, 21, 21, 21 /), &
  mg1111111_dnpb(0:9) = (/ 1, 21, 231, 1771, 10626, 53130, 230230, &
    888030, 3108105, 10015005 /), &
  mg1111111_npb(0:9) = (/ 1, 22, 253, 2024, 12650, 65780, 296010, &
    1184040, 4292145, 14307150 /), &
  mg1111111_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg1111111_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  mg1111111_dnb(0:9) = (/ 1, 21, 231, 1771, 10626, 53130, 230230, &
    888030, 3108105, 10015005 /), &
  mg1111111_nb(0:9) = (/ 1, 22, 253, 2024, 12650, 65780, 296010, &
    1184040, 4292145, 14307150 /)
!..procedures
CONTAINS
include 'mg1111111_prims.f90'
include 'mg1111111_prib.f90'
include 'mg1111111_secs.f90'
include 'mg1111111_base.f90'
END MODULE inv_mg1111111
