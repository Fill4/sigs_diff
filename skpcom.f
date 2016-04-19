!***************************************************************************
	subroutine skpcom (nunit)
! This subroutine checks if there is a line starting by "#" and
! if there is it skips it.
!
	implicit integer (i-n)
	character ain*1
!
 1	read (nunit,1000,end=2) ain
	if (ain.eq.'#') goto 1
	backspace (nunit)
 2	return
!
 1000	format (a1)
	end
!***************************************************************************
