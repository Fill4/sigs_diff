!--------------------------------------------------------------------
!	joao faria: 20/08/2012 
!		remove INTYPE as an argument, it is contained in the module
!		COMMONVAR
!--------------------------------------------------------------------
	subroutine openfiles (afile)
!	 open the files necessary for the OUTPUT

	use commonvar
	
	implicit none
	
	character(len=80), intent(in)  :: afile
	integer             :: nunit
	logical             :: res_exists


	! RES file (unit = 9) -
	inquire( file="res", exist=res_exists)

	if (res_exists) then
		open (9, file='res', status='old', position='append')
	else 
		open (9, file='res', status='unknown')
		write (9,*) ' '
		close (9)

		open (9,file='res',status='old')
		! write to terminal that RES was created
		write (6,*) "  In file RES   [filename,C1,C2,...] (all final values)"

		! header -
		write (9,'(x, a, i1, x, a)') "# SIG_GENETIC results (", nconst, "parameters)"
		write (9,*) ''

		write (9,'(x, a, a23, 7a10)') &
		  "#", "file", "tau_bcz", "phi_bcz", "amp_bcz", "tau_he", "phi_he", "amp_he", "beta"
		write (9,*) &
		  "#---------------------------------------------------------------------------------------------"
		write (9,*) ''
	endif

	return
	
	end subroutine openfiles
