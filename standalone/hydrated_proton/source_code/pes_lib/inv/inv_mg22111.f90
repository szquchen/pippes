MODULE inv_mg22111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg22111_prims, mg22111_prib, mg22111_isecs, mg22111_secs, &
  mg22111_base
!..data
integer, parameter, private :: nkk=5, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,1,1,1/)
integer, parameter, public :: &
  mg22111_id=42, &
  mg22111_nkk=nkk, mg22111_nk=nk, mg22111_nr=nr, mg22111_ngrp=4, &
  mg22111_nkj(0:nkk-1)=nkj, &
  mg22111_dnpr(0:9) = (/ 0, 12, 9, 0, 0, 0, 0, 0, 0, 0 /), &
  mg22111_npr(0:9) = (/ 0, 12, 21, 21, 21, 21, 21, 21, 21, 21 /), &
  mg22111_dnpb(0:9) = (/ 1, 12, 87, 472, 2112, 8184, 28336, 89496, &
    261756, 716936 /), &
  mg22111_npb(0:9) = (/ 1, 13, 100, 572, 2684, 10868, 39204, 128700, &
    390456, 1107392 /), &
  mg22111_dnsc(0:9) = (/ 1, 0, 12, 16, 38, 32, 12, 16, 1, 0 /), &
  mg22111_nsc(0:9) = (/ 1, 1, 13, 29, 67, 99, 111, 127, 128, 128 /), &
  mg22111_dnb(0:9) = (/ 1, 12, 99, 632, 3386, 15728, 64934, 242376, &
    829329, 2629908 /), &
  mg22111_nb(0:9) = (/ 1, 13, 112, 744, 4130, 19858, 84792, 327168, &
    1156497, 3786405 /)
!..procedures
CONTAINS
include 'mg22111_prims.f90'
include 'mg22111_prib.f90'
include 'mg22111_isecs.f90'
include 'mg22111_secs.f90'
include 'mg22111_base.f90'
END MODULE inv_mg22111
