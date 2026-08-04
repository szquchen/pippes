FUNCTION pes_x8_pot (xn) RESULT (f)
! Potential for generic X8
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x8_nk-1,0:pes_x8_nk-1)
integer, parameter :: nki(0:0)=pes_x8_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)+ &
  cx_f4(nki,r,pes_x4_pc,pes_x4_cf)+ &
  cx_f5(nki,r,pes_x5_pc,pes_x5_cf)+ &
  cx_f6(nki,r,pes_x6_pc,pes_x6_cf)+ &
  cx_f7(nki,r,pes_x7_pc,pes_x7_cf)+ &
  cx_f8(nki,r,pes_x8_pc,pes_x8_cf)
return
END FUNCTION pes_x8_pot
