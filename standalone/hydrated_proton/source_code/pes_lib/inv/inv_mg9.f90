MODULE inv_mg9
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg9_prims, mg9_prib, mg9_isecs, mg9_secs, &
  mg9_base
!..data
integer, parameter, private :: nkk=1, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/9/)
integer, parameter, public :: &
  mg9_id=67, &
  mg9_nkk=nkk, mg9_nk=nk, mg9_nr=nr, mg9_ngrp=, &
  mg9_nkj(0:nkk-1)=nkj, &
  mg9_dnpr(0:9) = (/ 
  mg9_npr(0:9) = (/ 
  mg9_dnpb(0:9) = (/ 
  mg9_npb(0:9) = (/ 
  mg9_dnsc(0:9) = (/ 
  mg9_nsc(0:9) = (/ 
  mg9_dnb(0:9) = (/ 
  mg9_nb(0:9) = (/ 
END MODULE inv_mg9
