PURE FUNCTION cx_dim (nb, dg) RESULT (n)
integer, intent (in) :: nb(0:), dg
integer :: n
!-----------------------------------------------------------------------
if (0.le.dg) then
 n = nb(dg)
else
 n = 0
endif
return
END FUNCTION cx_dim
