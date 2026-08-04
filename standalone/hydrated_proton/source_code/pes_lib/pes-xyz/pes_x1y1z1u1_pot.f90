FUNCTION pes_x1y1z1u1_pot (xn) RESULT (f)
! Potential for generic X1Y1Z1U1
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x1y1z1u1_nk-1,0:pes_x1y1z1u1_nk-1)
integer, parameter :: nki(0:3)=pes_x1y1z1u1_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+pes_y1_cf*nki(1)+pes_z1_cf*nki(2)+ &
  pes_u1_cf*nki(3)+ &
  cx_f11(nki,r,pes_x1y1_pc,pes_x1y1_cf)+ &
  cx_f101(nki,r,pes_x1z1_pc,pes_x1z1_cf)+ &
  cx_f011(nki,r,pes_y1z1_pc,pes_y1z1_cf)+ &
  cx_f1001(nki,r,pes_x1u1_pc,pes_x1u1_cf)+ &
  cx_f0101(nki,r,pes_y1u1_pc,pes_y1u1_cf)+ &
  cx_f0011(nki,r,pes_z1u1_pc,pes_z1u1_cf)+ &
  cx_f111(nki,r,pes_x1y1z1_pc,pes_x1y1z1_cf)+ &
  cx_f1101(nki,r,pes_x1y1u1_pc,pes_x1y1u1_cf)+ &
  cx_f1011(nki,r,pes_x1z1u1_pc,pes_x1z1u1_cf)+ &
  cx_f0111(nki,r,pes_y1z1u1_pc,pes_y1z1u1_cf)+ &
  cx_f1111(nki,r,pes_x1y1z1u1_pc,pes_x1y1z1u1_cf)
return
END FUNCTION pes_x1y1z1u1_pot
