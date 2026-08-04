FUNCTION pes_nmbrec (iun) RESULT (n)
integer, intent (in) :: iun
integer :: n
!-----------------------------------------------------------------------
integer :: k
k = 0
do while (.true.)
 read (iun,*,end=1)
 k = k+1
enddo
1 continue
n = k
rewind (iun)
END FUNCTION pes_nmbrec
