!****************************************************************************
! Joao Faria: Jan 2013
!****************************************************************************
subroutine output (frequency_file, chi2)
!	 write the results to the terminal

	use types_and_interfaces
	use commonvar
	use commonarray, only: c, n
	
	character(len=80), intent(in) :: frequency_file
	real(dp), intent(in)          :: chi2

	real(dp)    :: tauBCZ, tauHe, a_bcz, a_he, beta
	real(dp)    :: chi2, chi2norm

	a_bcz = c(1)*w0ref
	tauBCZ = c(2)
	a_he = c(4)*w0ref
	beta = c(5)
	tauHe = c(6)
	
	write (6,*) "  Frequencies from file: ", frequency_file

	write (6,1010)	'Results:', &
					'tau_BCZ = ', tauBCZ, &
					'phi_bcz = ', c(3), 'A_bcz = ', a_bcz, &
					'tau_He = ', tauHe, &
					'phi_he = ', c(7), 'A_he = ', a_he, beta, &
					'chi2 = ', chi2, 'chi2norm = ', chi2/(n-nconst)
						   
1010	format (3x, a, //, &
				6x, a, f9.4, //, &
				6x, a, f9.2, 6x, a, f10.8, //, &
				6x, a, f9.4, //, &
				6x, a, f9.2, 6x, a, f10.6, 2x, f15.3, //, &
				6x, a, f15.10, 5x, a, f15.10 /)

		

	write (9,9003) frequency_file, tauBCZ, c(3), a_bcz, tauHe, c(7), a_he, beta
9003	format (x, a24, 7f10.4)
	close(9)

	return
		
end subroutine output
