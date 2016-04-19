!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
! Outputs the plots and writes the results to a file.sss
subroutine output (afile, chi2)

	use types_and_interfaces
	use commonvar
	use commonarray, only: c, n, nd2
	implicit none

	character(len=80), intent(in) :: afile
	real(dp), intent(inout)          :: chi2
	real(dp)    :: tauBCZ, tauHe, a_bcz, a_he, beta

	! Get the parameters from the results array C.
	a_bcz = c(1)*w0ref
	tauBCZ = c(2)
	a_he = c(4)*w0ref
	beta = c(5)
	tauHe = c(6)
	chi2 = chi2*w0ref**2

	! Write the results to the shell during execution
	write (6,1010)	'Results:', &
					'tau_BCZ = ', tauBCZ, 'A_bcz = ', a_bcz,&
					'phi_bcz = ', c(3),  &
					'tau_He = ', tauHe, 'A_he = ', a_he,&
					'phi_he = ', c(7),  'Beta = ', beta,&
					'chi2 = ', chi2, 'chi2norm = ', chi2/nd2
						   
1010 format (3x, a, //, &
			&3x, a, f9.4, 6x, a, f10.6, //,&
			&3x, a, f9.2, //,&
			&3x, a, f9.4, 6x, a, f10.6, //,&
			&3x, a, f9.2, 6x, a, f10.4, //,&
			&3x, a, f12.5, 3x, a, f10.5 //)

	! Write results to the Results file
	write (9,9003) afile, tauBCZ, c(3), a_bcz, tauHe, c(7), a_he, beta
9003 format (x, a24, 7f10.4)
	close(9)

	return
		
end subroutine output
