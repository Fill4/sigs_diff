!--------------------------------------------------------------------
!	Joao Faria: 20/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common variables that all subroutines
!	 need to share. 

module commonvar
	
	use types_and_interfaces, only: dp
	
	implicit none
	
	logical, public 	 :: use_error_chi2 	!Use errors from frequencies
	
	integer, public      :: nconst			! number of parameters to fit
	integer, public      :: iprint
	integer, public      :: intype			! type of frequency input
	integer, public      :: lmin, lmax, nlmin	! lmin - min degree l to consider
												! lmax - max degree l to consider
												! nlmin - min # of l points
	integer, public      :: iterinit, iterfit, iterIRLS
	integer, public      :: itermod
	integer, public      :: isel
	integer, public      :: nleft,nright
	
	logical, public      :: write_final
	
	real(dp), public     :: fac, pi, pi_sq, twopi
	real(dp), public     :: tau0_houdek, tau0_houdek_sq  
	
	real, public         :: xinit
	real, public         :: ftol,tolfit,dc
	real, public         :: valtype
	real, public         :: vleft,vright
	real, public         :: xmass,xrad			! mass & radius of star
	real, public         :: ssmax				! max error allowed in frequencies
	                        ! initial values:   
	real, public         :: amp_bcz, tau_bcz, phi_bcz, &
		                    amp1_he, amp2_he, tau_he, phi_he, &
		                    poly0
	real, public         :: nu0, w0, tau0, w0ref,xamp0ref,tau0ref,phi0ref	! reference values
	

end module commonvar
