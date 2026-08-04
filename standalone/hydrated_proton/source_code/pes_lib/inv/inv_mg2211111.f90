MODULE inv_mg2211111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg2211111_prims, mg2211111_prib, mg2211111_isecs, mg2211111_secs, &
  mg2211111_base
!..data
integer, parameter, private :: nkk=7, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,1,1,1,1,1/)
integer, parameter, public :: &
  mg2211111_id=94, &
  mg2211111_nkk=nkk, mg2211111_nk=nk, mg2211111_nr=nr, mg2211111_ngrp=4, &
  mg2211111_nkj(0:nkk-1)=nkj, &
  mg2211111_dnpr(0:9) = (/ 0, 23, 13, 0, 0, 0, 0, 0, 0, 0 /), &
  mg2211111_npr(0:9) = (/ 0, 23, 36, 36, 36, 36, 36, 36, 36, 36 /), &
  mg2211111_dnpb(0:9) = (/ 1, 23, 289, 2599, 18629, 112723, 596661, &
    2830035, 12238395, 48885005 /), &
  mg2211111_npb(0:9) = (/ 1, 24, 313, 2912, 21541, 134264, 730925, &
    3560960, 15799355, 64684360 /), &
  mg2211111_dnsc(0:9) = (/ 1, 0, 30, 36, 255, 240, 452, &
    472, 255, 240 /), &
  mg2211111_nsc(0:9) = (/ 1, 1, 31, 67, 322, 562, 1014, &
    1486, 1741, 1981 /), &
  mg2211111_dnb(0:9) = (/ 1, 23, 319, 3325, 28382, 207202, 1328762, &
    7625342, 39712147, 189798437 /), &
  mg2211111_nb(0:9) = (/ 1, 24, 343, 3668, 32050, 239252, 1568014, &
    9193356, 48905503, 238703940 /)
!..procedures
CONTAINS
include 'mg2211111_prims.f90'
include 'mg2211111_prib.f90'
include 'mg2211111_isecs.f90'
include 'mg2211111_secs.f90'
include 'mg2211111_base.f90'
END MODULE inv_mg2211111
