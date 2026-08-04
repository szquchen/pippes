MODULE inv_cg2
!..use and access
use inv_wp
implicit none
private
public :: cg2_prims, cg2_prib, cg2_secs, cg2_base
!..data
! Note: all rather trivial.  We keep it this way to be consistent
! with other routines in the inv package.
integer, private :: i_local
integer, parameter, private :: nk=2, nr=nk*(nk-1)/2
integer, parameter, public :: &
  cg2_nk=nk, cg2_nr=nr, cg2_ngrp=2, cg2_ngen=1, &
  cg2_dnpr(0:19) = (/ 0, 1, (0,i_local=2,19) /), &
  cg2_npr(0:19) = (/ 0, (1,i_local=1,19) /), &
  cg2_dnpb(0:19) = (/ (1,i_local=0,19) /), &
  cg2_npb(0:19) = (/ (i_local+1,i_local=0,19) /), &
  cg2_dnsc(0:19) = (/ 1, (0,i_local=1,19) /), &
  cg2_nsc(0:19) = (/ (1,i_local=0,19) /), &
  cg2_dnb(0:19) = (/ (1,i_local=0,19) /), &
  cg2_nb(0:19) = (/ (i_local+1,i_local=0,19) /)
!..procedures
interface cg2_base
 module procedure cg2_base_vec, cg2_base_scal
end interface
CONTAINS
include 'cg2_prims.f90'
include 'cg2_prib.f90'
include 'cg2_secs.f90'
include 'cg2_base_vec.f90'
include 'cg2_base_scal.f90'
END MODULE inv_cg2
