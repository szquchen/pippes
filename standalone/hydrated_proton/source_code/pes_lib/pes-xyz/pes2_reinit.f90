SUBROUTINE pes2_reinit (fun, vfun)
! Recompute pes2_df (, pes2_dgf, pes2_dg2f, pes2_ddip)
optional :: vfun
interface
 function fun (xn0) result (f0)
 use pes0, wp=>pes0_wp
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp) :: f0
 end function fun
 function vfun (xn0) result (vf0)
 use pes0, wp=>pes0_wp
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp) :: vf0(0:size(xn0,2)-1)
 end function vfun
end interface
!----------------------------------------------------------------------
integer :: ip
do ip = 0, size(pes2_f)-1
 pes2_df(ip) = pes2_f(ip)-fun(pes2_xn(:,:,ip))
 if (pes2_havegf) then
  pes2_dgf(:,:,ip) = pes2_gf(:,:,ip)- &
    pes2_grad(fun,pes2_dfdis,pes2_xn(:,:,ip))
 endif
 if (pes2_haveg2f) then
  pes2_dg2f(:,:,ip) = pes2_g2f(:,:,ip)- &
    pes2_grad2(fun,pes2_df2dis,pes2_xn(:,:,ip))
 endif
 if (pes2_havedip) then
  pes2_ddip(:,ip) = pes2_dip(:,ip)- &
    matmul(pes2_xn(:,:,ip),vfun(pes2_xn(:,:,ip)))
 endif
enddo
return
END SUBROUTINE pes2_reinit
