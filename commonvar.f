!--------------------------------------------------------------------
!	Joao Faria: 20/08/2012	|	Revised: Filipe Pereira - Abr 2016
!--------------------------------------------------------------------
module commonvar
!	Module that contains the common variables that all subroutines
!	need to share. 
	
	use types_and_interfaces, only: dp
	implicit none
	
	logical, public		:: use_error_chi2 		! Use errors from frequencies
	logical, public		:: show_plots = .FALSE.
	logical, public		:: verbose = .FALSE.	! Toggle print information during execution
	
	integer, public		:: nconst  				! Number of parameters to fit
	real(dp), public	:: pi 					! Constants
	real(dp), public	:: w0ref                ! Reference frequency

	integer, public		:: pikaia_pop			! Controls the initial population size of pikaia
	integer, public		:: pikaia_gen			! Controls the number of generations for pikaia
	integer, public		:: iterIRLS, maxIter	! Control the IRLS procedure (not implemented)

	integer, public		:: degree				! Degree of polynomial fitted to 2nd differences

	integer, public		:: lmin, lmax 			! Min and max degree l to consider
	integer, public		:: nlmin				! Minimum number of l points
	integer, public		:: nmin, nmax			! Min and max radial order n to consider
	real(dp), public	:: vleft,vright			! Percentage of left and right bounds of frequencies to consider
	real(dp), public	:: ssmax				! Max error allowed in frequencies

	!Variables to use in rescale function
	real(dp), public	:: upper_tau_bcz, lower_tau_bcz
	real(dp), public	:: upper_tau_he2, lower_tau_he2
	real(dp), public	:: upper_beta, lower_beta
	
	reaL, public		:: large_sep, teff, lum

end module commonvar
