PURE FUNCTION cx_substr (s0, s1) RESULT (b)
! Test if a trimmed version of s1 occurs as a unit in s0.
character (len=*), intent (in) :: s0, s1
logical :: b
!-----------------------------------------------------------------------
integer :: l0, l2
character, parameter :: sep=' '
character (len=len(s1)) :: s2
s2 = adjustl(s1)
if (s0.eq.'*') then
! Treat as wildcard
 b = .true.
else
 l0 = len(s0) ; l2 = len_trim(s2)
 if (s0.eq.s2(1:l2)) then
  b = .true.
 else if (l0.le.l2) then
  b = .false.
 else if (s2(1:l2)//sep.eq.s0(1:l2+1).or. &
   sep//s2(1:l2).eq.s0(l0-l2:l0)) then
  b = .true.
 else
  b = index(s0,sep//s2(1:l2)//sep).ne.0
 endif
endif
return
END FUNCTION cx_substr
