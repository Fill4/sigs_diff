!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
	subroutine output (frequency_file, resd)
!	 write the results to the terminal

        use types_and_interfaces
		use commonvar
		use commonarray, only: c, n
		
		character(len=80), intent(in) :: frequency_file
		real(dp), intent(in)          :: resd
	
		real(dp)    :: tauBCZ, tauHe, a, a_bcz, a_he
		real(dp)    :: chi2, chi2norm
		
		integer     :: nfile


		tauBCZ = c(3)
		tauHe = c(7)
		
		a_bcz = c(2)!*1.0d12
		a_he = c(5)!*2.0d3
		a = tauHe / (sqrt(2.0_dp*pi)*w0ref*c(6))
		
		
		write (6,*) "  Frequencies from file: ", frequency_file
	
		write (6,1010) 'Results:', &
		                   'tau_BCZ = ', tauBCZ, &
		                   'phi_bcz = ', c(4), 'A_bcz = ', a_bcz, &
                           'tau_He = ', tauHe, &
                           'phi_he = ', c(8), 'A_he = ', a_he, c(6), &
                           'A_II / sqrt(2pi)*nu0*Delta_II = ', a, &
                           'chi2 = ', resd, 'chi2norm = ', resd/(n-nconst)
                           
 1010	format (3x, a, //, &
                6x, a, f9.4, //, &
                6x, a, f9.2, 6x, a, f10.4, //, &
                6x, a, f9.4, //, &
                6x, a, f9.2, 6x, a, f10.4, 2x, f15.3, //, &
                6x, a, f15.8, //, &
                6x, a, f15.10, 5x, a, f15.10, /)

        
!		nfile=length(afile)

!		! output to "res" file -
!		if (intype.eq.0) then
!			write (9,9003) afile(nfile-6:nfile-4), taud, c(2), c(3), amess
! 9003		format (3x, a, x, f9.4, 2x, f7.5, 2x, f10.8, 2x, a1)
!		endif

        return
        
	end subroutine output
