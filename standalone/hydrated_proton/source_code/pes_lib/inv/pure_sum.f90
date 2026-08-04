PURE FUNCTION pure_sum (ni) RESULT (n)
integer, intent (in) :: ni(0:)
integer :: n
!-----------------------------------------------------------------------
n = sum(ni)
return
END FUNCTION pure_sum
