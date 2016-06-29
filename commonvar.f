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
	
	integer, public		:: nconst				! number of parameters to fit
	integer, public		:: lmin, lmax, nlmin	! lmin - min degree l to consider
												! lmax - max degree l to consider
												! nlmin - min # of l points
												
	integer, public		:: pikaia_pop, pikaia_gen, iterIRLS, maxIter
	integer, public		:: isel
	integer, public		:: nleft,nright
	integer, public		:: degree
	
	real(dp), public	:: fac, pi
	
	real(dp), public	:: vleft,vright			!
	real(dp), public	:: ssmax                ! Max error allowed in frequencies
	real(dp), public	:: w0ref                ! Reference values

	!Variables to use in rescale function
	real(dp), public		:: upper_tau_bcz, lower_tau_bcz
	real(dp), public		:: upper_tau_he2, lower_tau_he2
	real(dp), public		:: upper_beta, lower_beta
	
	reaL, public		:: large_sep, teff, lum

end module commonvar
