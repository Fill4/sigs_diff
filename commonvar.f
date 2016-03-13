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
	integer, public      :: lmin, lmax, nlmin	! lmin - min degree l to consider
												! lmax - max degree l to consider
												! nlmin - min # of l points
	integer, public      :: pikaia_pop, pikaia_gen, iterIRLS
	integer, public      :: isel
	integer, public      :: nleft,nright
	
	real(dp), public     :: fac, pi
	
	real, public         :: vleft,vright
	real, public         :: ssmax				! max error allowed in frequencies
	real, public         :: w0ref

end module commonvar
