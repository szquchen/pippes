MODULE inv_mg321111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg321111_prims, mg321111_prib, mg321111_isecs, mg321111_secs, &
  mg321111_base
!..data
integer, parameter, private :: nkk=6, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,1,1,1,1/)
integer, parameter, public :: &
  mg321111_id=91, &
  mg321111_nkk=nkk, mg321111_nk=nk, mg321111_nr=nr, mg321111_ngrp=12, &
  mg321111_nkj(0:nkk-1)=nkj, &
  mg321111_dnpr(0:9) = (/ 0, 17, 12, 6, 0, 0, 1, 0, 0, 0 /), &
  mg321111_npr(0:9) = (/ 0, 17, 29, 35, 35, 35, 36, 36, 36, 36 /), &
  mg321111_dnpb(0:9) = (/ 1, 17, 165, 1179, 6861, 34293, 152111, 612043, &
    2269242, 7844176 /), &
  mg321111_npb(0:9) = (/ 1, 18, 183, 1362, 8223, 42516, 194627, 806670, &
    3075912, 10920088 /), &
  mg321111_dnsc(0:9) = (/ 1, 0, 25, 86, 406, 1496, 4322, 10788, &
    23453, 42898 /), &
  mg321111_nsc(0:9) = (/ 1, 1, 26, 112, 518, 2014, 6336, 17124, &
    40577, 83475 /), &
  mg321111_dnb(0:9) = (/ 1, 17, 190, 1690, 12854, 86356, 521774, 2869190, &
    14490544, 67731068 /), &
  mg321111_nb(0:9) = (/ 1, 18, 208, 1898, 14752, 101108, 622882, 3492072, &
    17982616, 85713684 /)
!..procedures
CONTAINS
include 'mg321111_prims.f90'
include 'mg321111_prib.f90'
include 'mg321111_isecs.f90'
include 'mg321111_secs.f90'
include 'mg321111_base.f90'
END MODULE inv_mg321111
