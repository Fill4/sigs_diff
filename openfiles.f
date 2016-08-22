!--------------------------------------------------------------------
! Joao Faria: 20/08/2012	|	Revised: Filipe Pereira - Abr 2016
!--------------------------------------------------------------------
subroutine openfiles
! This subroutine opens or creates the Results file.

	use commonvar
	implicit none
	
	logical			:: bool_Results

	! Results_diff file (unit = 9)
	inquire( file="Results_diff", exist=bool_Results)
	if (bool_Results) then
		open (9, file='Results_diff', status='old', position='append')
	else 
		open (9, file='Results_diff', status='unknown')
		if (verbose) write (9,*) ' '
		close (9)

		open (9,file='Results_diff',status='old')

		! Write header
		if (verbose) then
			write (9,'(a, i1, x, a)') "# SIG_GENETIC results (", nconst, "parameters)"

			write (9,'(a, a6, a29, 7a10)') &
				"#", "file", "|", "tau_bcz", "phi_bcz", "amp_bcz", "tau_he", "phi_he", "amp_he", "beta"
			write (9,'(a)') &
				"#---------------------------------------------------------------------------------------------"
		end if
	endif

	return
	
end subroutine openfiles