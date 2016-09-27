module commonvar
! Module that has all the single valued variables that need to be shared between subroutines
! This module also initializes ad sets default values to all variables that can be passed 
! through options_file which is parsed in file set_inputs.f

	use types_and_interfaces, only: dp
	
	implicit none

	! Input variables
	!--------------------------------------------------------
	! Passable Arguments
	logical		:: automatic = .FALSE.
	logical		:: show_plots = .FALSE.
	logical		:: verbose = .FALSE.
	! Range in degree
	integer		:: lmin = 0, lmax = 3
	! Minimum number of modes with same degree
	integer		:: nlmin = 5
	! Range in radial order
	integer		:: nmin = 8, nmax = 28
	! Whether it should use the errors or not
	logical		:: use_error_chi2 = .TRUE.
	! Upper limit for error
	real(dp)	:: ssmax = 0.100
	! Value to work with sigs_diff
	integer		:: degree = 2
	! Fitting control parameter
	real(dp)	:: ftol
	! Smoothing control parameter
	real(dp)	:: lambda
	! Fitting procedure values
	integer		:: smooth_iter_max
	! Fitting procedure values
	integer		:: pikaia_pop = 80, pikaia_gen = 3000
	! Reference frequency
	real(dp)	:: w0ref = -1
	! Star parameters
	real		:: large_sep, teff
	! Initial values for parameters
	real		:: upper_tau_bcz = 4500, lower_tau_bcz = 1500
	real		:: upper_tau_he2 = 1400, lower_tau_he2 = 300
	integer		:: bcz_interval = 800, he2_interval = 400
	!--------------------------------------------------------

	! Other variables used during execution
	!--------------------------------------------------------
	! Time variables to measure code execution time
	real		:: start, finish
	! Number of parameters to fit
	integer		:: nconst
	! Value of chi squared for the fitted parameters in c
	real(dp)	:: chi2			
	! Constants
	real(dp)	:: pi = 4.0d0*atan(1.0d0)
	!--------------------------------------------------------					

end module commonvar