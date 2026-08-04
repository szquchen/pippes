MODULE inv_mg81
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg81_prims, mg81_prib, mg81_isecs, mg81_secs, &
  mg81_base
!..data
integer, parameter, private :: nkk=2, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/8,1/)
integer, parameter, public :: &
  mg81_id=68, &
  mg81_nkk=nkk, mg81_nk=nk, mg81_nr=nr, mg81_ngrp=, &
  mg81_nkj(0:nkk-1)=nkj, &
  mg81_dnpr(0:9) = (/ 0, 2, 3, 7, 4, 4, 3, 4, 1, 0 /), &
  mg81_npr(0:9) = (/ 0, 2, 5, 12, 16, 20, 23, 27, 28, 28 /), &
  mg81_dnpb(0:9) = (/ 1, 2, 6, 17, 38, 84, 183, 368, 724, 1391 /), &
  mg81_npb(0:9) = (/ 1, 3, 9, 26, 64, 148, 331, 699, 1423, 2814 /), &
  mg81_dnsc(0:9) = (/ 1, 0, 1, 4, 27, 92, 338, 1217, 4494, 16162 /), &
  mg81_nsc(0:9) = (/ 1, 1, 2, 6, 33, 125, 463, 1680, 6174, 22336 /), &
  mg81_dnb(0:9) = (/ 1, 2, 7, 23, 79, 271, 973, 3508, 12789, 46453 /), &
  mg81_nb(0:9) = (/ 1, 3, 10, 33, 112, 383, 1356, 4864, 17653, 64106 /)
END MODULE inv_mg81
