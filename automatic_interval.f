subroutine automatic_interval

	use types_and_interfaces
	use commonvar
	use commonarray

	implicit none

	real 	:: bcz_coef(6), he2_coef(6), bcz_est, he2_est

	bcz_coef = (/ 3.41483132E3, 2.93091912, -8.85539759E1, 5.36225977E-3, -3.78731978E-4, 1.32955170E-1/)
	he2_coef = (/ 1.04042545E4, -1.72370361, -5.41870744E1, 4.64983481E-3, 7.96606252E-5, 7.15437626E-2/)

	bcz_est = bcz_coef(1) + (bcz_coef(2)*teff) + (bcz_coef(3)*large_sep) + &
	(bcz_coef(4)*teff*large_sep) + (bcz_coef(5)*teff**2) + (bcz_coef(6)*large_sep**2)

	he2_est = he2_coef(1) + (he2_coef(2)*teff) + (he2_coef(3)*large_sep) + &
	(he2_coef(4)*teff*large_sep) + (he2_coef(5)*teff**2) + (he2_coef(6)*large_sep**2)

	upper_tau_bcz = bcz_est + bcz_interval/2
	lower_tau_bcz = bcz_est - bcz_interval/2
	upper_tau_he2 = he2_est + he2_interval/2
	lower_tau_he2 = he2_est - he2_interval/2

	if (verbose) then
		write (*,*) ' Determining range for tau_bcz and tau_he2 automatically'
		write (*,*) ' '
		write (*,1099) 'Range in tau_bcz  : ', lower_tau_bcz, upper_tau_bcz
		write (*,1099) 'Range in tau_he2  : ', lower_tau_he2, upper_tau_he2
	endif

1099 format (2x, a, f10.5, x, '-', x, f10.5)

	return

end subroutine automatic_interval