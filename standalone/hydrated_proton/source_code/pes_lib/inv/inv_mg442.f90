MODULE inv_mg442
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg442_prims, mg442_prib, mg442_isecs, mg442_secs, &
  mg442_base
!..data
integer, parameter, private :: nkk=3, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,4,2/)
integer, parameter, public :: &
  mg442_id=109, &
  mg442_nkk=nkk, mg442_nk=nk, mg442_nr=nr, mg442_ngrp=24*24*2, &
  mg442_nkj(0:nkk-1)=nkj, &
  mg442_dnpr(0:9) = (/ 0, 6, 13, 11, 11, 0, 3, 0, 0, 0 /), &
  mg442_npr(0:9) = (/ 0, 6, 19, 30, 41, 41, 44, 44, 44, 44 /), &
  mg442_dnpb(0:9) = (/ 1, 6, 34, 145, 567, 1966, 6383, 19293, &
    55448, 151617 /), &
  mg442_npb(0:9) = (/ 1, 7, 41, 186, 753, 2719, 9102, 28395, &
    83843, 235460 /), &
  mg442_dnsc(0:9) = (/ 1, 0, 7, 62, 310, 1575, 8222, 40040, &
    182254, 775064 /), &
  mg442_nsc(0:9) = (/ 1, 1, 8, 70, 380, 1955, 10177, 50217, &
    232471, 1007535 /), &
  mg442_dnb(0:9) = (/ 1, 6, 41, 249, 1487, 8524, 47554, 256081, &
    1328208, 6607037 /), &
  mg442_nb(0:9) = (/ 1, 7, 48, 297, 1784, 10308, 57862, 313943, &
    1642151, 8249188 /)
!..procedures
CONTAINS
include 'mg442_prims.f90'
include 'mg442_prib.f90'
include 'mg442_isecs.f90'
include 'mg442_secs.f90'
include 'mg442_base.f90'
END MODULE inv_mg442
