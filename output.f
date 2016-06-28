!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine output (afile, chi2)
! Outputs the plots and writes the results to a file.sss

	use types_and_interfaces
	use commonvar
	use commonarray
	use lib_array
	use lib_plot
	use lib_io
	implicit none

	character(len=80), intent(in)	:: afile
	real(dp), intent(inout)			:: chi2
	real(dp)						:: tauBCZ, tauHe, a_bcz, a_he, beta
	real(dp), dimension(150)		:: xx, result_fun, he_fun, bcz_fun, smooth_fun
	real(dp)						:: min_xx, max_xx


	! Create arrays with all the results obtained.		
	min_xx = minval(w_d2(1:nd2))
	max_xx = maxval(w_d2(1:nd2))
	call linspace(min_xx, max_xx, xx)
	result_fun = fun(xx)
	he_fun = he_comp(xx)
	bcz_fun = bcz_comp(xx)
	smooth_fun = smooth_comp(xx)

	if (show_plots) then
		! Plot initial second differences and smooth function fitted to them
		call plot(w_d2(1:nd2)*w0ref, pre_d2(1:nd2)*w0ref, &
		xx*w0ref, smooth_fun*w0ref, ' 5.00-', color1='black', color2='red')

		! Plot second differences without the smooth component
		!call plot(w_d2(1:nd2)*w0ref, d2(1:nd2)*w0ref, ' 5.', color1='black')
		
		! Plot the fit of fun to the second differences
		call plot(w_d2(1:nd2)*w0ref, d2(1:nd2)*w0ref, &
		xx*w0ref, result_fun*w0ref, ' 5.00-', color1='black', color2='red', errors=sigd2(1:nd2))

		! Plot the HeII and the Bcz components
		call plot(xx*w0ref, he_fun*w0ref, &
		xx*w0ref, bcz_fun*w0ref, '00-00-', color1='blue', color2='red')
	end if

	! Get the parameters from the results array C.
	a_bcz = c(1)*w0ref
	tauBCZ = c(2)
	a_he = c(4)*w0ref
	beta = c(5)
	tauHe = c(6)
	chi2 = chi2*w0ref**2

	! Write the results to the shell during execution if verbose is TRUE
	if (verbose) write (6,1010)	'Results:', &
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
9003 format (a24, 7f10.4)
	close(9)

	return
		
end subroutine output
