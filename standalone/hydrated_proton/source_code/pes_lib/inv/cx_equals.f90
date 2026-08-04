PURE FUNCTION cx_equals (pc0, pc1) RESULT (b)
type (cx_t), intent (in) :: pc0, pc1
logical :: b
!-----------------------------------------------------------------------
b = pc0%dg.eq.pc1%dg.and. &
  pc0%kx.eq.pc1%kx.and. &
  pc0%lx.eq.pc1%lx.and. &
  pc0%ax.eq.pc1%ax.and. &
  pc0%bx.eq.pc1%bx
return
END FUNCTION cx_equals
