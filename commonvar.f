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
	
	real, public		:: vleft,vright
	real, public		:: ssmax				! max error allowed in frequencies
	real, public		:: w0ref				! Reference frequency

	real, public		:: star_mass			
	real, public		:: star_rad
	real, public		:: star_lum				!Various star parameters
	real, public		:: star_teff
	real, public		:: star_age

end module commonvar
