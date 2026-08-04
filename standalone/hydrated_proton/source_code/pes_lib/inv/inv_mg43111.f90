MODULE inv_mg43111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg43111_prims, mg43111_prib, mg43111_isecs, mg43111_secs, &
  mg43111_base
!..data
integer, parameter, private :: nkk=5, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,3,1,1,1/)
integer, parameter, public :: &
  mg43111_id=122, &
  mg43111_nkk=nkk, mg43111_nk=nk, mg43111_nr=nr, mg43111_ngrp=24*6, &
  mg43111_nkj(0:nkk-1)=nkj, &
  mg43111_dnpr(0:9) = (/ 0, 12, 12, 12, 7, 0, 1, 0, 0, 0 /), &
  mg43111_npr(0:9) = (/ 0, 12, 24, 36, 43, 43, 44, 44, 44, 44 /), &
  mg43111_dnpb(0:9) = (/ 1, 12, 90, 520, 2530, 10836, 42009, 150136, &
    501184, 1577984 /), &
  mg43111_npb(0:9) = (/ 1, 13, 103, 623, 3153, 13989, 55998, 206134, &
    707318, 2285302 /), &
  mg43111_dnsc(0:9) = (/ 1, 0, 20, 111, 601, 3333, 16797, 75685, &
    313223, 1181687 /), &
  mg43111_nsc(0:9) = (/ 1, 1, 21, 132, 733, 4066, 20863, 96548, &
    409771, 1591458 /), &
  mg43111_dnb(0:9) = (/ 1, 12, 110, 871, 6263, 41771, 261212, 1537425, &
    8531023, 44675082 /), &
  mg43111_nb(0:9) = (/ 1, 13, 123, 994, 7257, 49028, 310240, 1847665, &
    10378688, 55053770 /)
!..procedures
CONTAINS
include 'mg43111_prims.f90'
include 'mg43111_prib.f90'
include 'mg43111_isecs.f90'
include 'mg43111_secs.f90'
include 'mg43111_base.f90'
END MODULE inv_mg43111
