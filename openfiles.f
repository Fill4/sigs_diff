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


	! COF file (unit = 3) -
	if (iprint.ge.1 .and. iprint.le.4) then
		close (3)
		open (3, file='cof', status='unknown')
		write (3,*) ' '
		close (3)
		open (3, file='cof', status='unknown')
		! write to terminal that COF was created -	
		write (6,*) "  File COF   [C1,C2,...] (each iteration)"
		
		write (3,*) "# tau_d     phi        A_d   "
		write (3,*) '#-----------------------------------'
		call flush (3)
	endif
	

	! SIG file (unit = 10) -
	if (iprint.ge.3.and.iprint.le.4) then
		close (10)
		open (10, file='sig', status='unknown')
		write (10,*) ' '
		close (10)
		open (10, file='sig', status='unknown')
		! write to terminal that SIG was created -	
		write (6,*) "  File SIG   [v, v-smooth, l, n, sigma, fit] "
		
		write (10,*) "# Final signal isolated by SIG_BCZ!"
		write (10,'(x,a,a20)') "# in frequencies from: ", afile
	endif


	! QFT file (unit = 7) -
	if (iprint.eq.5) then
		close (7)
		open (7, file='qft', status='unknown')
		write (7,*) ' '
		close (7)
		open (7, file='qft', status='unknown')
		! write to terminal that QFT was created -	
		write (6,*) "  File QFT   [v, v-smooth, l, n, sigma, fit] (1st iteration only) "
		
		write (7,*) "# Signal isolated in the 1st iteration"
		write (7,'(x,a,a20)') "# in frequencies from: ", afile
	endif


	return
	
	end subroutine openfiles
