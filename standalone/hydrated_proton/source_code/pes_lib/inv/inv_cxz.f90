MODULE inv_cxz
!..use and access
use inv_core
use inv_mg
use inv_mgv
use inv_cxx
use inv_cxy
implicit none
private
public :: cx_nbx, cx_bx, cx_fx, cxv_nbx, cxv_bx, cxv_fx, &
  cx_nbase, cx_base, cx_getcf, cxv_nbase, cxv_base, cxv_getcf
!..procedures
CONTAINS
include 'cx_nbx.f90'
include 'cx_bx.f90'
include 'cx_fx.f90'
include 'cxv_nbx.f90'
include 'cxv_bx.f90'
include 'cxv_fx.f90'
include 'cx_nbase.f90'
include 'cx_base.f90'
include 'cx_getcf.f90'
include 'cxv_nbase.f90'
include 'cxv_base.f90'
include 'cxv_getcf.f90'
END MODULE inv_cxz
